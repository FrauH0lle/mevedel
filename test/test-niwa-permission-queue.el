;;; test-niwa-permission-queue.el -- Tests for niwa-permission-queue -*- lexical-binding: t -*-

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

(require 'niwa-tool-exec-registration)

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-request)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-agent-control)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-lobby)
(require 'niwa-execution-target)
(require 'niwa-structs)
(require 'niwa-permissions)
(require 'niwa-permission-log)
(require 'niwa-permission-prompt)
(require 'niwa-permission-queue)
(require 'niwa-collaboration-transport)
(require 'niwa-session-persistence)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-tool-exec)
(require 'niwa-tool-ui)
(require 'niwa-tools)
(require 'niwa-view)
(require 'niwa-mentions)
(require 'niwa-skills-ui)
(require 'niwa-permission-rules)
(require 'niwa-permission-mode)
(require 'niwa-sandbox)

(defun test-pq--make-session (&optional rules target)
  "Create a fresh queue-test session with optional RULES and TARGET."
  (niwa-session--create
   :name "test"
   :workspace nil
   :execution-target target
   :permission-rules rules
   :permission-mode 'ask
   :permission-queue nil
   :pending-plan-approval nil))

(defun test-pq--read-permission-log (session)
  "Read SESSION's permission log entries."
  (let ((file (niwa-permission-log-path session))
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

(niwa-deftest niwa-permission-queue-resource-needs-tree-p
  (:doc "only unrepresentable confined directory scopes need a tree")
  (let* ((root (make-temp-file "niwa-scope-" t))
         (file (file-name-concat root "file"))
         (missing (file-name-concat root "missing"))
         (session (niwa-session--create
                   :permission-mode 'edits :sandbox-mode 'required))
         (entry (list :kind 'sandbox :tool-name "Bash" :session session :mode "fresh"))
         (niwa-protected-paths nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "fixture"))
          (dolist (kind '(sandbox bash eval generic))
            (plist-put entry :kind kind)
            (dolist (path (list root file missing))
              (dolist (access '(read write))
                (should (eq (not (null
                                  (niwa-permission-queue-resource-needs-tree-p
                                   entry (list :path path :access access))))
                            (and (not (eq kind 'generic))
                                 (equal path root) (eq access 'write)))))))
          (plist-put entry :kind 'eval)
          (plist-put entry :mode "live")
          (should-not (niwa-permission-queue-resource-needs-tree-p
                       entry (list :path root :access 'write)))
          (plist-put entry :kind 'sandbox)
          (let ((niwa-protected-paths (list (cons root 'inaccessible))))
            (should (niwa-permission-queue-resource-needs-tree-p
                     entry (list :path root :access 'read))))
          (setf (niwa-session-permission-mode session) 'ask
                (niwa-session-sandbox-mode session) 'off)
          (should-not (niwa-permission-queue-resource-needs-tree-p
                       entry (list :path root :access 'write))))
      (delete-directory root t))))

(niwa-deftest niwa-permission-queue-select-resource
  (:doc "changing extent preserves command, independent and omitted remembering")
  (let* ((original '(:path "/directory" :access write))
         (selected '(:path "/directory" :access write :recursive t)))
    (dolist (key '(:file-system :resource-grants nil))
      (let ((entry (list :resource-selection-cell (list (list original))
                         :remember-authority-cell
                         (list (append '(:operation t :network t)
                                       (and key (list key (list original))))))))
        (niwa-permission-queue-select-resource entry original selected)
        (should (equal (list selected) (car (plist-get entry :resource-selection-cell))))
        (should (equal (append '(:operation t :network t)
                               (and key (list key (list selected))))
                       (car (plist-get entry :remember-authority-cell))))))))

(niwa-deftest niwa-permission-queue-prepare-resources
  (:doc "fallback admission prepares the same tree idempotently and creates no authority")
  (let* ((root (make-temp-file "niwa-human-scope-" t))
         (session (niwa-session--create
                   :permission-mode 'edits :sandbox-mode 'required))
         (original (list :path root :access 'write))
         (entry (list :kind 'sandbox :tool-name "Bash" :origin "/root"
                      :resource-originals (list original)
                      :resource-selection-cell (list (list original))
                      :remember-authority-cell (list (list :file-system (list original)))
                      :callback #'ignore))
         (niwa-permission-reviewer 'auto)
         fallback)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-permission-queue--render-entry) #'ignore)
                  ((symbol-function 'niwa-permission-review-start)
                   (lambda (reviewed cont)
                     (niwa-permission-queue-prepare-resources reviewed)
                     (niwa-permission-queue-validate-approval reviewed 'allow-once)
                     (setq fallback cont))))
          (niwa-permission-request entry session)
          (should-not (niwa-session-permission-queue session))
          (funcall fallback)
          (let ((selected (car (car (plist-get entry :resource-selection-cell)))))
            (should (plist-get selected :recursive))
            (should (equal root (plist-get selected :path)))
            (should (equal (list original) (plist-get entry :resource-originals)))
            (should (equal (list selected)
                           (plist-get (car (plist-get entry :remember-authority-cell))
                                      :file-system)))
            (should-not (niwa-session-resource-grants session))
            (should-not (niwa-session-permission-rules session))
            (niwa-permission-queue--render-head session)
            (should (equal selected (car (car (plist-get entry :resource-selection-cell)))))))
      (niwa-permission-queue-abort-all session)
      (delete-directory root t))))

(niwa-deftest niwa-permission-queue-justification
  (:doc "reads a trimmed reason from the entry or its arguments, never blank")
  (progn
    (should (equal "Inspect" (niwa-permission-queue-justification
                              '(:args (:justification "  Inspect ")))))
    (should (equal "Entry" (niwa-permission-queue-justification
                            '(:justification "Entry" :args (:justification "Args")))))
    (dolist (entry '((:args (:justification "  ")) (:justification 7) nil))
      (should-not (niwa-permission-queue-justification entry)))))

(niwa-deftest niwa-permission-queue--admit
  (:doc "cancellation during directory inspection never leaves a pending card")
  (let* ((root (make-temp-file "niwa-admit-cancel-" t))
         (session (niwa-session--create :permission-mode 'edits))
         (request (niwa-request--create))
         (directory-p (symbol-function 'file-directory-p))
         (niwa-permission-reviewer 'user)
         outcomes rendered)
    (unwind-protect
        (cl-letf (((symbol-function 'file-directory-p)
                   (lambda (path)
                     (when (equal path root)
                       (setf (niwa-request-cancelled-p request) t))
                     (funcall directory-p path)))
                  ((symbol-function 'niwa-permission-queue--render-entry)
                   (lambda (_) (setq rendered t))))
          (niwa-permission-request
           (list :kind 'sandbox :tool-name "Bash" :origin "/root"
                 :request request :resource-selection-cell
                 (list (list (list :path root :access 'write)))
                 :callback (lambda (outcome) (push outcome outcomes))) session)
          (should (equal '(aborted) outcomes))
          (should-not rendered)
          (should-not (niwa-session-permission-queue session)))
      (niwa-permission-queue-abort-all session)
      (delete-directory root t))))

(niwa-deftest niwa-permission-queue-validate-approval ()
  ,test
  (test)
  :doc "exact directory writes cannot settle or leave the queue as approved"
  (let* ((root (make-temp-file "niwa-grant-approval-" t))
         (session (niwa-session--create
                   :permission-mode 'ask :sandbox-mode 'required))
         (selection (list (list (list :path root :access 'write))))
         received)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                   #'ignore))
          (niwa-permission-request
           (list :kind 'sandbox :tool-name "Bash" :origin "/root"
                 :resource-selection-cell selection
                 :callback (lambda (outcome) (push outcome received)))
           session)
          (let ((entry (car (niwa-session-permission-queue session))))
            ;; Simulate a stale or externally supplied unusable selection.
            (setcar selection (list (list :path root :access 'write)))
            (dolist (outcome '(allow-once allow-session always-allow))
              (should-error
               (niwa-permission-queue-validate-approval entry outcome)
               :type 'user-error)
              (should-error
               (niwa-permission-queue--on-head-outcome entry outcome)
               :type 'user-error)
              (should (eq entry (car (niwa-session-permission-queue session))))
             (should-not received))
            ;; A recursive ancestor cannot validate an exact protected child.
            (let* ((child (file-name-concat root ".git"))
                   (niwa-protected-paths '(("**/.git/**" . read-only))))
              (make-directory child)
              (setcar selection (list (list :path root :access 'write :recursive t)
                                      (list :path child :access 'write)))
              (should-error (niwa-permission-queue-validate-approval entry 'allow-once)
                            :type 'user-error))
            (setcar selection (list (list :path root :access 'write :recursive t)))
            (niwa-permission-queue-validate-approval entry 'allow-once)
            (niwa-permission-queue--on-head-outcome entry 'allow-once)
            (should (equal '(allow-once) received))
            (should-not (niwa-session-permission-queue session))))
      (delete-directory root t)))
  :doc "unconfined Bash approves exact directory writes without widening them"
  (dolist (sandbox-mode '(off best-effort))
    (let* ((root (make-temp-file "niwa-unconfined-approval-" t))
           (niwa-permission-rules nil)
           (niwa-permission-reviewer 'user)
           (session (niwa-session--create
                     :permission-mode 'ask :sandbox-mode sandbox-mode
                     :working-directory root))
           entry allowed)
      (unwind-protect
          (with-temp-buffer
            (setq-local niwa--session session)
            (cl-letf (((symbol-function 'niwa-hooks-run-event)
                       (lambda (_event _payload callback &rest _) (funcall callback nil)))
                      ((symbol-function 'niwa-sandbox-probe)
                       (lambda (&rest _) '(:available nil :reason "Test unavailable")))
                      ((symbol-function 'niwa-permission-queue--render-entry)
                       (lambda (item) (setq entry item))))
              (niwa-tool-permission-step
               (list :tool (niwa-tool-ensure "Bash")
                     :args (list :command "touch example"
                                 :sandbox_permissions "with_additional_permissions"
                                 :justification "Write the requested example directory"
                                 :additional_permissions
                                 (list :file_system (list :write (vector root))))
                     :session session :buffer (current-buffer))
               (lambda (_) (setq allowed t))
               (lambda (&rest args) (ert-fail args)))
              (should entry)
              (niwa-permission-queue--on-head-outcome entry 'allow-once)
              (should allowed)
              (should-not (plist-get
                           (car (car (plist-get entry :resource-selection-cell)))
                           :recursive))))
        (niwa-permission-queue-abort-all session)
        (delete-directory root t))))
  :doc "denial, native and live directory access need no recursive extent"
  (let* ((root (make-temp-file "niwa-grant-native-" t))
         (entry (list :kind 'generic :resource-selection-cell
                      (list (list (list :path root :access 'write))))))
    (unwind-protect
        (progn
          (niwa-permission-queue-validate-approval entry 'allow-once)
          (plist-put entry :kind 'eval)
          (niwa-permission-queue-validate-approval entry 'allow-once)
          (plist-put entry :kind 'sandbox)
          (niwa-permission-queue-validate-approval entry 'deny-once))
      (delete-directory root t))))

(niwa-deftest niwa-permission-queue--pop ()
  ,test
  (test)
  :doc "yielding validation preserves new siblings and respects a replaced head"
  (dolist (cancel-head '(nil t))
    (let* ((session (test-pq--make-session))
           (root (make-temp-file "niwa-queue-validation-" t))
           (niwa-permission-reviewer 'user)
           (directory-p (symbol-function 'file-directory-p))
           first second outcomes yielded)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-permission-queue--render-entry) #'ignore))
            (setf (niwa-session-sandbox-mode session) 'off)
            (niwa-permission-request
             (list :kind 'sandbox :tool-name "Bash" :origin "/root"
                   :resource-selection-cell (list (list (list :path root :access 'write)))
                   :callback (lambda (value) (push (cons 'first value) outcomes))) session)
            (setq first (car (niwa-session-permission-queue session)))
            (cl-letf (((symbol-function 'file-directory-p)
                       (lambda (path)
                         (when (and (equal path root) (not yielded))
                           (setq yielded t)
                           (when cancel-head
                             (niwa-permission-queue--on-head-outcome first 'aborted))
                           (niwa-permission-request
                            (list :kind 'generic :tool-name "Read" :origin "/root"
                                  :callback (lambda (value) (push (cons 'second value) outcomes)))
                            session)
                           (setq second (car (last (niwa-session-permission-queue session)))))
                         (funcall directory-p path))))
              ;; No mount restriction, but checking the actual directory still yields.
              (setf (niwa-session-sandbox-mode session) 'off)
              (niwa-permission-queue--on-head-outcome first 'allow-once))
            (should yielded)
            (should (equal (list second) (niwa-session-permission-queue session)))
            (should (equal (list (cons 'first (if cancel-head 'aborted 'allow-once))) outcomes))
            (niwa-permission-queue--on-head-outcome second 'deny-once)
            (should (= 2 (length outcomes))))
        (niwa-permission-queue-abort-all session)
        (delete-directory root t)))))

(niwa-deftest niwa-permission-request
  (:quiet t :doc "FIFO permission queue contract")
  ,test
  (test)

  :doc "enqueue keeps the visible head rendered while siblings wait"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (rendered nil))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               (lambda (entry) (push entry rendered))))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :origin "/root" :callback #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Edit"
             :origin "/root" :callback #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Write"
             :origin "/root" :callback #'ignore)))
    ;; The visible head is re-rendered as siblings arrive so pending
    ;; counts stay current, but FIFO order is unchanged.
    (should (= 3 (length rendered)))
    (should (cl-every (lambda (entry)
                        (equal "Read" (plist-get entry :tool-name)))
                      rendered))
    ;; All three entries are on the queue in FIFO order.
    (let ((q (niwa-session-permission-queue session)))
      (should (= 3 (length q)))
      (should (equal "Read" (plist-get (nth 0 q) :tool-name)))
      (should (equal "Edit" (plist-get (nth 1 q) :tool-name)))
      (should (equal "Write" (plist-get (nth 2 q) :tool-name)))))

  :doc "entry captures :session at enqueue so settlement is buffer-independent"
  (let* ((session (test-pq--make-session))
         (niwa--session session))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :origin "/root" :callback #'ignore)))
    (let* ((q (niwa-session-permission-queue session))
           (entry (car q)))
      (should (eq session (plist-get entry :session)))))

  :doc "no-session enqueue aborts without direct rendering"
  (let ((niwa--session nil)
        (rendered nil)
        outcome)
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               (lambda (entry) (push entry rendered))))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :origin "/root"
             :callback (lambda (o) (setq outcome o)))))
    (should (null rendered))
    (should (eq 'aborted outcome)))

  :doc "the notify function fires once per admitted card with its entry"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (notified nil)
         (kinds nil)
         (niwa-notify-function
          (lambda (kind entry) (push kind kinds) (push entry notified))))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-key :path :specifier-value "one.el"
             :origin "/root" :callback #'ignore))
      ;; A sibling re-renders the head but notifies only for itself.
      (niwa-permission-request
       (list :kind 'bash :command "git add ." :origin "/root"
             :callback #'ignore)))
    (should (equal '(permission permission) kinds))
    (should (equal "Read" (plist-get (cadr notified) :tool-name)))
    (should (eq :path (plist-get (cadr notified) :specifier-key)))
    (should (equal "one.el"
                   (plist-get (cadr notified) :specifier-value)))
    (should (equal "git add ." (plist-get (car notified) :command)))
    (should (eq session (plist-get (car notified) :session)))
    (should (cl-every (lambda (entry)
                        (not (plist-member entry :callback)))
                      notified)))

  :doc "the notify function never fires for a no-session abort"
  (let ((niwa--session nil)
        (notified 0)
        (niwa-notify-function
         (lambda (_kind _entry) (cl-incf notified))))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :origin "/root" :callback #'ignore)))
    (should (= 0 notified)))

  :doc "a signaling notify function does not break admission"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (niwa-notify-function
          (lambda (_kind _entry) (error "Notifier broke")))
         captured)
    (niwa-test--with-captured-diagnostics captured
      (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                 #'ignore))
        (niwa-permission-request
         (list :kind 'generic :tool-name "Read"
               :origin "/root" :callback #'ignore))))
    (should (= 1 (length (niwa-session-permission-queue session))))
    (should (string-match-p "permission notify failed" captured)))

  :doc "unrenderable cards abort once without a display or user response"
  (let ((session (test-pq--make-session)) outcomes)
    (niwa-permission-request
     (list :kind 'unknown :origin "/root"
           :callback (lambda (outcome) (push outcome outcomes)))
     session)
    (niwa-permission-queue-abort-all session)
    (should (equal '(aborted) outcomes))
    (should-not (niwa-session-permission-queue session))
    (let ((events (niwa-session-permission-log-pending session)))
      (should (equal '(permission-enqueued permission-aborted)
                     (mapcar (lambda (event) (plist-get event :event)) events)))
      (should (eq 'render-failed (plist-get (cadr events) :settlement-source)))
      (should (equal (plist-get (car events) :permission-id)
                     (plist-get (cadr events) :permission-id)))))

  :doc "enqueue rejects a missing origin"
  (let ((session (test-pq--make-session)))
    (should-error
     (niwa-permission-request
      (list :kind 'generic :tool-name "Read" :callback #'ignore)
      session)
     :type 'error))

  :doc "enqueue rejects malformed canonical paths"
  (let ((session (test-pq--make-session)))
    (dolist (origin '(nil "" "root" "/root/" "/root/Bad"
                          "/root/explorer.agent" "explorer--abc"))
      (should-error
       (niwa-permission-request
        (list :kind 'generic :tool-name "Read"
              :origin origin :callback #'ignore)
        session)
       :type 'error)))

  :doc "marks retained requesters blocked until their final queued permission settles"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (invocation
          (niwa-agent-invocation--create :path "/root/worker"))
         (record
          (niwa-agent-record--create
           :path "/root/worker" :activity 'running
           :invocation invocation)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (cl-letf (((symbol-function 'niwa-permission--prompt-async-attributed)
               (lambda (&rest _args) nil)))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :origin "/root/worker" :callback #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Edit"
             :origin "/root/worker" :callback #'ignore))
      (should (eq 'permission-blocked
                  (niwa-agent-record-activity record)))
      (niwa-permission-queue--on-head-outcome
       (car (niwa-session-permission-queue session)) 'allow-once)
      (should (eq 'permission-blocked
                  (niwa-agent-record-activity record)))
      (niwa-permission-queue--on-head-outcome
       (car (niwa-session-permission-queue session)) 'allow-once)
      (should (eq 'running (niwa-agent-record-activity record))))))


;;
;;; Permission diagnostics

(niwa-deftest niwa-permission-log
  ()
  ,test
  (test)
  :doc "buffers diagnostics until the session is materialized"
  (let ((session (test-pq--make-session))
        (niwa-permission-log-enabled t))
    (niwa-permission-log session 'permission-decision :tool-name "Read")
    (should (= 1 (length (niwa-session-permission-log-pending session))))
    (should-not (niwa-permission-log-path session)))

  :doc "retains a materialized diagnostic when persistence fails"
  (let* ((root (make-temp-file "niwa-permission-log-retry-" t))
         (blocked (file-name-concat root "blocked"))
         (session (test-pq--make-session))
         (niwa-permission-log-enabled t))
    (unwind-protect
        (progn
          (write-region "not a directory" nil blocked nil 'silent)
          (setf (niwa-session-save-path session) blocked)
          (let (diagnostics)
            (niwa-test--with-captured-diagnostics diagnostics
              (niwa-permission-log
               session 'permission-decision :tool-name "Read"))
            (should (string-match-p "persistence failed" diagnostics)))
          (should (= 1
                     (length
                      (niwa-session-permission-log-pending session)))))
      (delete-directory root t)))

  :doc "defers remote diagnostics and combines one serialized append"
  (let* ((root (make-temp-file "niwa-permission-log-remote-" t))
         (target
          (niwa-execution-target-create
           "/ssh:permission-host:/workspace/"))
         (session (test-pq--make-session nil target))
         (niwa-permission-log-enabled t)
         calls)
    (setf (niwa-session-save-path session) root)
    (unwind-protect
        (cl-letf
            (((symbol-function
               'niwa-session-publication-append-diagnostic)
              (lambda (_session path content)
                (push (list path content) calls)
                t))
             ((symbol-function 'niwa-telemetry-record) #'ignore))
          (niwa-permission-log
           session 'permission-enqueued :tool-name "Read")
          (niwa-permission-log
           session 'permission-resolved :tool-name "Read")
          (should (= 2 (length
                        (niwa-session-permission-log-pending session))))
          (should-not calls)
          (niwa-permission-log-flush session)
          (should-not (niwa-session-permission-log-pending session))
          (pcase-let ((`((,path ,content)) calls))
            (should
             (equal (file-name-concat root "permission-log.el") path))
            (with-temp-buffer
              (insert content)
              (goto-char (point-min))
              (should (eq 'permission-enqueued
                          (plist-get (read (current-buffer)) :event)))
              (should (eq 'permission-resolved
                          (plist-get (read (current-buffer)) :event))))))
      (delete-directory root t)))

  :doc "side queue lifecycle forwards only whitelisted durable audit fields"
  (let* ((parent (test-pq--make-session))
         (side (test-pq--make-session))
         calls)
    (setf (niwa-session-audit-session side) parent)
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore)
              ((symbol-function 'niwa-telemetry-record)
               (lambda (session event &rest props)
                 (push (list session event props) calls))))
      (niwa-permission-request
       (list :kind 'sandbox :tool-name "Bash"
             :specifier-key :path :specifier-value "/private/path"
             :resource-path "/private/resource"
             :resource-access 'write :protected-path t
             :additional-permissions '(:network t)
             :justification "private justification"
             :origin "/root" :callback #'ignore)
       side))
    (let ((call
           (cl-find-if
            (lambda (entry)
              (and (eq parent (car entry))
                   (eq 'permission-enqueued (cadr entry))))
            calls)))
      (should call)
      (let ((props (nth 2 call)))
        (should (eq 'btw (plist-get props :conversation-scope)))
        (should (eq 'sandbox (plist-get props :kind)))
        (should (eq :path (plist-get props :specifier-key)))
        (should (eq 'write (plist-get props :resource-access)))
        (dolist (key '(:specifier-value :resource-path
                       :additional-permissions :justification))
          (should-not (plist-member props key)))
        (should-not
         (string-match-p
          (regexp-opt '("/private/path" "/private/resource"
                        "private justification"))
          (prin1-to-string props))))))

  :doc "forwarded terminal telemetry preserves categorical outcome and lifetime"
  (dolist (outcome '(allow-session (feedback . "private feedback text")))
    (let* ((parent (test-pq--make-session))
           (side (test-pq--make-session))
           (niwa-permission-log-enabled t)
           (niwa-telemetry-enabled t))
      (setf (niwa-session-audit-session side) parent)
      (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                 #'ignore))
        (niwa-permission-request
         (list :kind 'eval :mode "fresh" :origin "/root"
               :permission-mode-effective 'edits :permission-via 'mode
               :resource-originals '((:path "/private/request" :access read))
               :resource-selection-cell '(((:path "/private/tree" :access read :recursive t)))
               :callback #'ignore)
         side)
        (niwa-permission-queue--on-head-outcome
         (car (niwa-session-permission-queue side)) outcome))
      (let ((events (reverse (niwa-session-telemetry-pending parent))))
        (should (= 2 (length events)))
        (let ((admitted (car events)) (terminal (cadr events)))
          (should (equal (plist-get admitted :permission-id)
                         (plist-get terminal :permission-id)))
          (should (eq 'edits (plist-get terminal :permission-mode-effective)))
          (should (equal "fresh" (plist-get terminal :eval-mode)))
          (should (eq (if (consp outcome) 'deny outcome)
                      (plist-get terminal :outcome)))
          (when (eq outcome 'allow-session)
            (should (eq 'session (plist-get terminal :approval-lifetime)))))
        (should-not (string-match-p "/private/\\|private feedback"
                                    (prin1-to-string events))))))

  :doc "permission queue writes enqueue and resolve diagnostics"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-permission-log-" t)))
         (session (test-pq--make-session))
         (niwa--session session)
         (niwa-permission-log-enabled t)
         outcome)
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) dir)
          (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                     #'ignore))
            (niwa-permission-request
             (list :kind 'generic
                   :tool-name "Read"
                   :specifier-key :path
                   :specifier-value "/tmp/a.el"
                   :origin "/root/verifier"
                   :callback (lambda (o) (setq outcome o)))
             session))
          (let ((entry (car (niwa-session-permission-queue session))))
            (niwa-permission-queue--on-head-outcome entry 'deny-once))
          (should (eq 'deny-once outcome))
          (let ((entries (test-pq--read-permission-log session)))
            (should (= 2 (length entries)))
            (should (eq 'permission-enqueued
                        (plist-get (nth 0 entries) :event)))
            (should (eq 'permission-resolved
                        (plist-get (nth 1 entries) :event)))
            (should (equal "Read" (plist-get (nth 0 entries) :tool-name)))
            (should (equal "/root/verifier"
                           (plist-get (nth 0 entries) :origin)))
            (should (eq 'deny-once
                        (plist-get (nth 1 entries) :outcome)))))
      (when (file-directory-p dir)
        (delete-directory dir t))))
  :doc "Bash lifecycle diagnostics omit raw command payload"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-permission-log-" t)))
         (session (test-pq--make-session))
         (niwa--session session)
         (niwa-permission-log-enabled t))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) dir)
          (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                     #'ignore))
            (niwa-permission-request
             (list :kind 'bash
                   :command "printf SECRET_TOKEN"
                   :commands-summary "printf"
                   :command-class 'unknown
                   :origin "/root"
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
               (make-temp-file "niwa-permission-log-" t)))
         (session (test-pq--make-session))
         (niwa--session session)
         (niwa-permission-log-enabled t))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) dir)
          (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                     #'ignore))
            (niwa-permission-request
             (list :kind 'eval
                   :expression "(message \"SECRET_TOKEN\")"
                   :mode "live"
                   :origin "/root"
                   :callback #'ignore)
             session))
          (let ((entry (car (test-pq--read-permission-log session))))
            (should (eq 'permission-enqueued (plist-get entry :event)))
            (should (equal "live" (plist-get entry :eval-mode)))
            (should-not (plist-member entry :expression))))
      (when (file-directory-p dir)
        (delete-directory dir t))))
  :doc "network escalation records authority without the command payload"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-permission-log-" t)))
         (session (test-pq--make-session))
         (niwa--session session)
         (niwa-permission-log-enabled t))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) dir)
          (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                     #'ignore))
            (niwa-permission-request
             (list :kind 'sandbox
                   :tool-name "Bash"
                   :detail "curl https://secret.example"
                   :sandbox-permissions 'additive
                   :additional-permissions '(:network t)
                   :justification "Download the requested page?"
                   :origin "/root"
                   :callback #'ignore)
             session))
          (let ((entry (car (test-pq--read-permission-log session))))
            (should (eq 'permission-enqueued (plist-get entry :event)))
            (should (eq 'additive
                        (plist-get entry :sandbox-permissions)))
            (should (equal '(:network t)
                           (plist-get entry :additional-permissions)))
            (should (equal "Download the requested page?"
                           (plist-get entry :justification)))
            (should-not (plist-member entry :detail))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(niwa-deftest niwa-permission-log-flush
  (:quiet t :doc "retains failed queued diagnostics and clears them after retry")
  (let* ((root (make-temp-file "niwa-permission-flush-" t))
         (blocked (file-name-concat root "blocked"))
         (restored (file-name-concat root "restored"))
         (session (test-pq--make-session))
         (entry '(:event permission-decision :time "now"))
         later-entry
         (niwa-permission-log-enabled t))
    (unwind-protect
        (progn
          (write-region "not a directory" nil blocked nil 'silent)
          (setf (niwa-session-save-path session) blocked
                (niwa-session-permission-log-pending session)
                (list entry))
          (niwa-permission-log-flush session)
          (should (equal (list entry)
                         (niwa-session-permission-log-pending session)))
          (setf (niwa-session-save-path session) restored)
          (cl-letf (((symbol-function 'niwa-telemetry-record) #'ignore))
            (niwa-permission-log session 'permission-resolved))
          (setq later-entry
                (cadr (niwa-session-permission-log-pending session)))
          (should (= 2 (length
                        (niwa-session-permission-log-pending session))))
          (niwa-permission-log-flush session)
          (should-not (niwa-session-permission-log-pending session))
          (should (equal (list entry later-entry)
                         (test-pq--read-permission-log session))))
      (delete-directory root t))))


;;
;;; Coalesce: rule-creating outcomes

(niwa-deftest niwa-permission-queue--coalesce
  (:doc "coalesce re-evaluates queued siblings against the just-created rule")
  ,test
  (test)

  :doc "allow-session coalesces queued path siblings via a resource grant"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (notified 0)
         (niwa-notify-function
          (lambda (_kind _entry) (cl-incf notified)))
         (resolved-outcomes nil))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      ;; Enqueue two identical Read requests.
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo/bar.el"
             :resource-access 'read
             :origin "/root"
             :callback (lambda (o) (push (cons "Read1" o) resolved-outcomes))))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo/bar.el"
             :resource-access 'read
             :origin "/root"
             :callback (lambda (o) (push (cons "Read2" o) resolved-outcomes))))
      ;; User answers allow-session for the head.  Simulate the exact
      ;; resource grant written by the normal permission prompt.
      (niwa-permission-add-session-resource-grant
       session "/foo/bar.el" 'read)
      ;; Coalesce should resolve the queued sibling as 'allow.
      (niwa-permission-queue--coalesce 'allow-session session))
    ;; Read2 was coalesced; Read1 (the head) wasn't touched by
    ;; --coalesce (the head's callback already fired before
    ;; --coalesce was called).
    (should (assoc "Read2" resolved-outcomes))
    (should (eq 'allow (cdr (assoc "Read2" resolved-outcomes))))
    (should (= 2 notified))
    ;; Queue is now empty (Read1 was already dropped before
    ;; --coalesce; Read2 was just resolved).
    (should (null (niwa-session-permission-queue session))))

  :doc "deny-session coalesces queued siblings to deny"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/x.el"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/x.el"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (push '("Read" :path "/x.el" :action deny)
            (niwa-session-permission-rules session))
      (niwa-permission-queue--coalesce 'deny-session session))
    (should (memq 'deny outcomes)))

  :doc "removes coalesced entries before reentrant teardown settles callbacks"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         outcomes)
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (dolist (id '(first second))
        (let ((entry-id id))
          (niwa-permission-request
           (list :kind 'generic :tool-name "Read"
                 :specifier-value "/reentrant.el"
                 :resource-access 'read
                 :origin "/root"
                 :callback
                 (lambda (outcome)
                   (push (cons entry-id outcome) outcomes)
                   (when (eq entry-id 'first)
                     (niwa-permission-queue-abort-all session)))))))
      (niwa-permission-add-session-resource-grant
       session "/reentrant.el" 'read)
      (niwa-permission-queue--coalesce 'allow-session session))
    (should (equal '((second . allow) (first . allow)) outcomes))
    (should-not (niwa-session-permission-queue session)))

  :doc "queued sibling whose rule does not cover it stays queued"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      ;; Two queued entries with different paths.
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo.el"
             :resource-access 'read
             :origin "/root"
             :callback (lambda (o) (push (cons "foo" o) outcomes))))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/bar.el"
             :resource-access 'read
             :origin "/root"
             :callback (lambda (o) (push (cons "bar" o) outcomes))))
      ;; Resource authority covers /foo.el only.
      (niwa-permission-add-session-resource-grant
       session "/foo.el" 'read)
      (niwa-permission-queue--coalesce 'allow-session session))
    ;; /foo.el's queued sibling resolved; /bar.el stayed.
    (let ((q (niwa-session-permission-queue session)))
      (should (= 1 (length q)))
      (should (equal "/bar.el" (plist-get (car q) :specifier-value)))))

  :doc "coalesce honors non-path generic specifier keys"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "WebFetch"
             :specifier-key :domain
             :specifier-value "example.com"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (push '("WebFetch" :domain "example.com" :action allow)
            (niwa-session-permission-rules session))
      (niwa-permission-queue--coalesce 'allow-session session))
    (should (memq 'allow outcomes))
    (should (null (niwa-session-permission-queue session))))

  :doc "protected paths do not coalesce allow rules but do coalesce deny rules"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (niwa-protected-paths '(("**/.git/**" . read-only)))
         (path "/repo/.git/config")
         (outcomes nil))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value path
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value path
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (push `("Read" :path ,path :action allow)
            (niwa-session-permission-rules session))
      (niwa-permission-queue--coalesce 'allow-session session)
      (should-not outcomes)
      (should (= 2 (length (niwa-session-permission-queue session))))
      (setf (niwa-session-permission-rules session)
            (list `("Read" :path ,path :action deny)))
      (niwa-permission-queue--coalesce 'deny-session session))
    (should (equal '(deny deny) outcomes))
    (should (null (niwa-session-permission-queue session))))

  :doc "rule write failure leaves queued siblings pending"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/uncovered.el"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/uncovered.el"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (niwa-permission-queue--coalesce 'allow-session session))
    (should-not outcomes)
    (should (= 2 (length (niwa-session-permission-queue session)))))

  :doc "filesystem sandbox siblings coalesce through exact resource grants"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         outcomes)
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (niwa-permission-request
         (list :kind 'sandbox :tool-name "Bash"
               :detail "pwd" :sandbox-permissions 'additive
               :requested-additional-permissions
               '(:file-system ((:path "/tmp/secret" :access read)))
               :resource-path "/tmp/secret"
               :resource-access 'read
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (niwa-permission-add-session-resource-grant
       session "/tmp/secret" 'read)
      (niwa-permission-queue--coalesce 'allow-session session))
    (should (equal '(allow-once allow-once) outcomes))
    (should-not (niwa-session-permission-queue session)))

  :doc "combined filesystem siblings wait until every exact grant exists"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         outcomes
         (missing
          '(:file-system
            ((:path "/tmp/input" :access read)
             (:path "/tmp/output" :access write)))))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (niwa-permission-request
         (list :kind 'sandbox :tool-name "Bash"
               :detail "pwd" :sandbox-permissions 'additive
               :requested-additional-permissions missing
               :resource-path "/tmp/input"
               :resource-access 'read
               :missing-additional-permissions missing
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (niwa-permission-add-session-resource-grant
       session "/tmp/input" 'read)
      (niwa-permission-queue--coalesce 'allow-session session)
      (should-not outcomes)
      (should (= 2 (length (niwa-session-permission-queue session))))
      (niwa-permission-add-session-resource-grant
       session "/tmp/output" 'write)
      (niwa-permission-queue--coalesce 'allow-session session))
    (should (equal '(allow-once allow-once) outcomes))
    (should-not (niwa-session-permission-queue session)))

  :doc "filesystem sandbox siblings coalesce through exact path denies"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         outcomes)
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (niwa-permission-request
         (list :kind 'sandbox :tool-name "Bash"
               :detail "pwd" :sandbox-permissions 'additive
               :requested-additional-permissions
               '(:file-system ((:path "/tmp/secret" :access read)))
               :resource-path "/tmp/secret"
               :resource-access 'read
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (push '("Bash" :path "/tmp/secret" :action deny)
            (niwa-session-permission-rules session))
      (niwa-permission-queue--coalesce 'deny-session session))
    (should (equal '(deny-once deny-once) outcomes))
    (should-not (niwa-session-permission-queue session)))

  :doc "full escalation siblings coalesce through a qualified session allow"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         outcomes)
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (niwa-permission-request
         (list :kind 'sandbox :tool-name "Bash"
               :detail "emacs --batch -Q"
               :sandbox-permissions 'require-escalated
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (push '("Bash" :pattern "emacs --batch -Q"
                     :sandbox-permissions require-escalated
                     :action allow)
            (niwa-session-permission-rules session))
      (niwa-permission-queue--coalesce 'allow-session session))
    (should (equal '(allow-once allow-once) outcomes))
    (should-not (niwa-session-permission-queue session)))

  :doc "full escalation siblings coalesce through a qualified session deny"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         outcomes)
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (niwa-permission-request
         (list :kind 'sandbox :tool-name "Eval"
               :detail "(message \"hello\")"
               :sandbox-permissions 'require-escalated
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (push '("Eval" :sandbox-permissions require-escalated :action deny)
            (niwa-session-permission-rules session))
      (niwa-permission-queue--coalesce 'deny-session session))
    (should (equal '(deny-once deny-once) outcomes))
    (should-not (niwa-session-permission-queue session))))


;;
;;; Render dispatch

(niwa-deftest niwa-permission-queue--reevaluate
  (:doc "rechecks complete execution authority, including captured delegation")
  (let* ((root (make-temp-file "niwa-pq-recheck-" t))
         (source (file-name-concat root "source"))
         (other (file-name-concat root "other"))
         (grant (list :path source :access 'read))
         (missing (list :path other :access 'read))
         (allow '(("Bash" :pattern "make *" :action allow)))
         (niwa-permission-rules nil)
         (niwa-permission-reviewer 'user))
    (unwind-protect
        (progn
          (dolist (path (list source other))
            (with-temp-file path (insert "recheck contents")))
          (dolist
              (scenario
               `(("Bash operation does not cover missing network"
                  ,allow (:kind bash :command "make test"
                          :requested-additional-permissions (:network t)) ask)
                 ("Bash operation does not cover another resource"
                  ,allow (:kind bash :command "make test"
                          :requested-additional-permissions
                          (:file-system (,missing))) ask)
                 ("confined edits authorizes an operation with covered resources"
                  nil (:kind sandbox :tool-name "Bash" :detail "make test"
                       :sandbox-permissions additive
                       :requested-additional-permissions (:file-system (,grant))
                       :missing-additional-permissions (:file-system (,grant))) allow)
                 ("recheck includes a previously granted but now missing resource"
                  ,allow (:kind sandbox :tool-name "Bash" :detail "make test"
                          :sandbox-permissions additive
                          :requested-additional-permissions (:file-system (,grant ,missing))
                          :missing-additional-permissions (:file-system (,grant))) ask)
                 ("matching Eval authority can clear covered resources"
                  (("Eval" :pattern "(+ 1 2)" :action allow))
                  (:kind eval :expression "(+ 1 2)" :mode "fresh"
                   :requested-additional-permissions (:file-system (,grant))) allow)
                 ("captured request deny remains authoritative"
                  ,allow (:kind bash :command "make test"
                          :request ,(niwa-request--create
                                     :skill-permission-rules
                                     '(("Bash" :pattern "make *" :action deny)))) deny)
                 ("captured Plan request still prohibits mutation"
                  ,allow (:kind bash :command "make test"
                          :request ,(niwa-request--create :plan-read-only t)) deny)))
            (ert-info ((car scenario))
              (let* ((session (test-pq--make-session (nth 1 scenario)))
                     (entry (append (list :session session :origin "/root")
                                    (nth 2 scenario))))
                (setf (niwa-session-permission-mode session) 'edits)
                (niwa-permission-add-session-resource-grant session source 'read)
                (should (eq (nth 3 scenario)
                            (niwa-permission-queue--reevaluate entry)))))))
      (delete-directory root t))))

(niwa-deftest niwa-permission-queue--attribution-origin
  (:doc "attributes prompts to non-root canonical agent paths")
  ,test
  (test)

  :doc "the root path needs no attribution line"
  (should-not
   (niwa-permission-queue--attribution-origin '(:origin "/root")))

  :doc "nested canonical paths retain attribution"
  (let* ((origin "/root/worker/verifier")
         (entry (list :origin origin)))
    (should (equal origin
                   (niwa-permission-queue--attribution-origin entry))))

  :doc "agent ToolCall calls retain both the requester and nested call"
  (let ((attribution
         (niwa-permission-queue--attribution-origin
          '(:origin "/root/worker" :call-source ptc
            :tool-use-id "ptc-1/2" :parent-tool-use-id "ptc-1"))))
    (should (string-match-p "/root/worker" attribution))
    (should (string-match-p "ToolCall ptc-1" attribution))
    (should (string-match-p "child ptc-1/2" attribution)))

  :doc "root ToolCall calls identify the envelope and child"
  (should
   (equal "ToolCall ptc-1 (child ptc-1/2)"
          (niwa-permission-queue--attribution-origin
           '(:origin "/root" :call-source ptc
             :tool-use-id "ptc-1/2" :parent-tool-use-id "ptc-1")))))

(niwa-deftest niwa-permission-queue--render-entry
  (:quiet t :doc "renders and settles every kind through its captured queue")
  ,test
  (test)

  :doc "every card preserves attribution, depth and exactly-once settlement across buffers"
  (dolist (kind '(generic bash eval sandbox))
    (let* ((session (test-pq--make-session))
           (other-session (test-pq--make-session))
           outcomes
           (entry (list :kind kind :session session :origin "/root/worker"
                        :tool-name "Read" :specifier-value "/tmp/input"
                        :command "git status" :command-class 'read-only
                        :expression "(+ 1 2)" :mode "fresh"
                        :detail "cat /tmp/input" :justification "Read input"
                        :callback (lambda (outcome) (push outcome outcomes))))
           (sibling (list :kind 'generic :session session :tool-name "Read"
                          :callback #'ignore))
           rendered)
      (setf (niwa-session-permission-queue session) (list entry sibling))
      (cl-letf (((symbol-function 'niwa-permission--prompt-async-with-content)
                 (lambda (body _always callback count card &rest _)
                   (push (list body callback count card) rendered))))
        (niwa-permission-queue--render-entry entry)
        (let ((card (car rendered)))
          (should (eq entry (nth 3 card)))
          (should (= 2 (nth 2 card)))
          (should (string-match-p "from /root/worker" (car card)))
          (should (string-match-p
                   (regexp-quote
                    (pcase kind
                      ('generic "Path: /tmp/input")
                      ('bash "Command: git status")
                      ('eval "Mode: fresh")
                      ('sandbox "Model's reason: Read input")))
                   (car card)))
          (with-temp-buffer
            (setq-local niwa--session other-session)
            (funcall (nth 1 card) 'deny-once)
            (funcall (nth 1 card) 'deny-once)))
        (should (equal '(deny-once) outcomes))
        (should (equal (list sibling)
                       (niwa-session-permission-queue session)))
        (should (= 1 (nth 2 (car rendered))))
        (should-not (niwa-session-permission-queue other-session)))
      (niwa-permission-queue-abort-all session)))

  :doc "ToolCall prompts show the envelope and child identity"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'generic :tool-name "Read"
                      :origin "/root" :call-source 'ptc
                      :tool-use-id "ptc-1/2"
                      :parent-tool-use-id "ptc-1"
                      :session session :callback #'ignore))
         attribution)
    (setf (niwa-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'niwa-permission--prompt-async-attributed)
               (lambda (_tool _path _always origin _cont _count _entry)
                 (setq attribution origin))))
      (niwa-permission-queue--render-entry entry))
    (should (equal "ToolCall ptc-1 (child ptc-1/2)" attribution)))

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
    (setf (niwa-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'niwa-permission--prompt-async-attributed)
               (lambda (tool path include-always _origin cont count rendered-entry)
                 (setq captured
                       (list tool path include-always cont count
                             rendered-entry)))))
      (niwa-permission-queue--render-entry entry))
    (should (equal "Read" (nth 0 captured)))
    (should (null (nth 1 captured)))
    (should (= 1 (nth 4 captured)))
    (should (eq entry (nth 5 captured))))

  :doc "missing live view aborts the visible head"
  (with-temp-buffer
    (let* ((session (test-pq--make-session))
           (niwa--session session)
           (outcome nil)
           (entry (list :kind 'generic
                        :tool-name "Read"
                        :specifier-value "/tmp/file.txt"
                        :include-always nil
                        :session session
                        :callback (lambda (o) (setq outcome o)))))
      (setf (niwa-session-permission-queue session) (list entry))
      (niwa-permission-queue--render-head session)
      (should (eq 'aborted outcome))
      (should (null (niwa-session-permission-queue session)))))

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
            (setq-local niwa--session session))
          (niwa-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'niwa--prompt-block-face)
                     (lambda () 'ask))
                    ((symbol-function 'display-warning)
                     (lambda (_type message &optional _level _buffer-name)
                       (when (string-match-p "permission-queue: render error"
                                             message)
                         (setq render-error message)))))
            (with-current-buffer data-buf
              (niwa-permission-request
               (list :kind 'generic
                     :tool-name "Grep"
                     :args (list :pattern "niwa-check-permission"
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
                     :origin "/root"
                     :callback #'ignore)
               session)
              (setq entry (car (niwa-session-permission-queue session)))
              (setq interaction-id
                    (niwa-queue--entry-metadata-get
                     entry :interaction-id))
              (should-not render-error)
              (should interaction-id)
              (should-not (plist-member entry :interaction-id))
              (should-not (plist-member entry :view-buffer))
              (with-current-buffer view-buf
                (should (gethash interaction-id
                                 niwa-view--interaction-overlays))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "missing Bash UI helper produces the pinned denial outcome"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (entry (list :kind 'bash
                      :command "sudo ls"
                      :command-class 'dangerous
                      :include-always nil
                      :session session))
         (outcome nil)
         (saved (and (fboundp 'niwa-permission--prompt-async-bash)
                     (symbol-function
                      'niwa-permission--prompt-async-bash))))
    (setq entry (plist-put entry :callback (lambda (o) (setq outcome o))))
    (unwind-protect
        (progn
          (when saved
            (fmakunbound 'niwa-permission--prompt-async-bash))
          (setf (niwa-session-permission-queue session) (list entry))
          (niwa-permission-queue--render-head session)
          (should (equal '(deny . "Bash permission UI unavailable")
                         outcome))
          (should (null (niwa-session-permission-queue session))))
          (when saved
            (fset 'niwa-permission--prompt-async-bash saved))))

  :doc "Bash approval resumes exactly once"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'bash
                      :command "git status"
                      :command-class 'read-only
                      :include-always nil
                      :session session))
         (outcomes nil)
         rendered)
    (setq entry (plist-put entry :callback
                           (lambda (o) (push o outcomes))))
    (setf (niwa-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               (lambda (next-entry) (push next-entry rendered))))
      (niwa-permission-queue--on-head-outcome entry 'allow-once)
      (niwa-permission-queue--on-head-outcome entry 'allow-once))
    (should (equal '(allow-once) outcomes))
    (should (null rendered))
    (should (null (niwa-session-permission-queue session))))

  :doc "missing live view aborts the visible head"
  (with-temp-buffer
    (let* ((session (test-pq--make-session))
           (niwa--session session)
           (outcome nil)
           (entry (list :kind 'eval
                        :expression "(message \"hi\")"
                        :session session
                        :callback (lambda (o) (setq outcome o)))))
      (setf (niwa-session-permission-queue session) (list entry))
      (niwa-permission-queue--render-head session)
      (should (eq 'aborted outcome))
      (should (null (niwa-session-permission-queue session)))))

  :doc "agent Eval permission renders in the parent interaction view"
  (let ((parent-data (generate-new-buffer " *test-pq-parent-data*"))
        (parent-view (generate-new-buffer " *test-pq-parent-view*"))
        (agent-data (generate-new-buffer " *test-pq-agent-data*"))
        (session (test-pq--make-session)))
    (setf (niwa-session-agent-transcripts session)
          '(("/root/verifier"
             :status running)))
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local niwa--session session)
            (setq-local niwa--view-buffer parent-view)
            (setq-local niwa--agent-invocation
                        (niwa-agent-invocation--create
                         :agent-id
                         "/root/verifier")))
          (cl-letf (((symbol-function 'niwa--prompt-block-face)
                     (lambda () 'ask)))
            (with-current-buffer agent-data
              (niwa-permission-request
               (list :kind 'eval
                     :expression "(message \"hi\")"
                     :mode "fresh"
                     :origin "/root/verifier"
                     :callback #'ignore)
               session)))
          (with-current-buffer parent-view
            (should (string-match-p "Elisp Evaluation Request"
                                    (buffer-string)))
            (should (string-match-p "from /root/verifier"
                                    (buffer-string)))
            (should (string-match-p "Mode: fresh"
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
    (setf (niwa-session-agent-transcripts session)
          '(("/root/verifier"
             :status running)))
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local niwa--session session)
            (setq-local niwa--view-buffer parent-view)
            (setq-local niwa--agent-invocation
                        (niwa-agent-invocation--create
                         :agent-id
                         "/root/verifier")))
          (cl-letf (((symbol-function 'niwa--prompt-block-face)
                     (lambda () 'ask))
                    ((symbol-function 'niwa-view--agent-status-collect)
                     (lambda ()
                       (list (list :path
                                   "/root/verifier"
                                   :status 'blocked
                                   :role "verifier"
                                   :description "Verify tracked diff"
                                   :calls 18)))))
            (with-current-buffer parent-view
              (niwa-view--render-agent-status))
            (with-current-buffer agent-data
              (dotimes (i 3)
                (niwa-permission-request
                 (list :kind 'eval
                       :expression (format "(+ %d 1)" i)
                       :mode "fresh"
                       :origin
                       "/root/verifier"
                       :callback (lambda (outcome)
                                   (push outcome outcomes)))
                 session)))
            (with-current-buffer parent-view
              (niwa-view--render-agent-status)
              (niwa-view--interaction-rebuild)
              (should-not outcomes)
              (should (= 3 (length (niwa-session-permission-queue session))))
              (let* ((text (buffer-substring-no-properties
                            (point-min) (niwa-view-transcript-tail)))
                     (agent-pos (string-search
                                 "Blocked   agent    /root/verifier" text))
                     (prompt-pos (string-search
                                  "Elisp Evaluation Request"
                                  text)))
                (should agent-pos)
                (should prompt-pos)
                (should (< agent-pos prompt-pos))
                (should (equal "3 permissions pending"
                               (niwa-view--interaction-count-label)))
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "3 permissions pending"
                                  (niwa-view-transcript-tail))
                  (should (eq 'interaction
                              (get-text-property
                               (match-beginning 0)
                               'niwa-view-zone-namespace)))
                  (should (eq :separator
                              (get-text-property
                               (match-beginning 0)
                               'niwa-view-zone-id))))
                (should (string-search "from /root/verifier" text))
                (should (string-search "Mode: fresh" text))))))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))))

  :doc "network request uses the once-only sandbox prompt adapter"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'sandbox
                      :tool-name "Bash"
                      :detail "curl https://example.test"
                      :justification "Download the requested page?"
                      :additional-permissions '(:network t)
                      :origin "/root"
                      :session session
                      :callback #'ignore))
         captured)
    (setf (niwa-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'niwa-permission--prompt-async-sandbox)
               (lambda (&rest args) (setq captured args))))
      (niwa-permission-queue--render-entry entry))
    (should (equal "Bash" (nth 0 captured)))
    (should (equal "curl https://example.test" (nth 1 captured)))
    (should (equal "Download the requested page?" (nth 2 captured)))
    (should (= 1 (nth 5 captured)))
    (should (eq entry (nth 6 captured))))

  :doc "filesystem request preserves its exact resource metadata"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'sandbox
                      :tool-name "Bash"
                      :detail "cat /tmp/secret"
                      :justification "Read the requested file?"
                      :resource-path "/tmp/secret"
                      :resource-access 'read
                      :origin "/root"
                      :session session
                      :callback #'ignore))
         captured)
    (setf (niwa-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'niwa-permission--prompt-async-sandbox)
               (lambda (&rest args) (setq captured args))))
      (niwa-permission-queue--render-entry entry))
    (should (equal "cat /tmp/secret" (nth 1 captured)))
    (should (= 1 (nth 5 captured)))
    (should (eq entry (nth 6 captured)))))

(niwa-deftest niwa-permission-queue--lobby-wake ()
  ,test
  (test)
  :doc "admission wakes the lobby; later display wakes only remaining room devices"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (let* ((session (niwa-skills-test--make-session "wake" niwa-user-dir))
             (niwa-collaboration--rooms (make-hash-table :test #'eq))
             (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
             (niwa-interaction-prompt-created-hook '(niwa-collaboration--on-prompt-created))
             (niwa-collaboration-remote-interactions t)
             (lobby-subs (make-hash-table :test #'equal))
             (room-subs (make-hash-table :test #'equal)) controls)
        (setq-local niwa--session session)
        (puthash "phone" '(:writable t :endpoint "lobby-phone") lobby-subs)
        (puthash niwa-user-dir (list :push-guests lobby-subs :transport 'lobby
                                        :guests (make-hash-table)) niwa-collaboration-lobby--lobbies)
        (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
                   (lambda (transport frame) (push (cons transport frame) controls))))
          (dotimes (_ 2)
            (niwa-permission-request
             (list :kind 'generic :tool-name "Read" :origin "/root" :callback #'ignore) session))
          (should (equal controls (make-list 2 '(lobby :t "push" :guestIds ["phone"]))))
          ;; Share the session after admission; the old lobby is no longer available.
          (clrhash niwa-collaboration-lobby--lobbies)
          (puthash "phone" '(:writable t :endpoint "room-phone") room-subs)
          (puthash "tablet" '(:writable t :endpoint "room-tablet") room-subs)
          (puthash data-buf (list :data-buffer data-buf :session session :push-guests room-subs
                                  :guests (make-hash-table) :ui-requests (make-hash-table)
                                  :transport 'room) niwa-collaboration--rooms)
          (setq controls nil)
          (with-current-buffer view-buf
            (niwa--prompt--settle (car (hash-table-values niwa-view--interaction-overlays))
                                     'deny-once))
          (should (equal controls '((room :t "push" :guestIds ["tablet"])))))))))

(niwa-deftest niwa-permission-queue--allow-once-advance
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
            (setq-local niwa--session session))
          (niwa-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'niwa--prompt-block-face)
                     (lambda () 'ask)))
            (with-current-buffer data-buf
              (niwa-permission-request
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-key :path
                     :specifier-value "/tmp/one.el"
                     :include-always t
                     :origin "/root"
                     :callback
                     (lambda (outcome)
                       (push (cons "one" outcome) outcomes)))
               session)
              (niwa-permission-request
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-key :path
                     :specifier-value "/tmp/two.el"
                     :include-always t
                     :origin "/root"
                     :callback
                     (lambda (outcome)
                       (push (cons "two" outcome) outcomes)))
               session))
            (with-current-buffer view-buf
              (let ((first-id (niwa-queue--entry-metadata-get
                               (car (niwa-session-permission-queue session))
                               :interaction-id)))
                (should first-id)
                (should (gethash first-id niwa-view--interaction-overlays))
                (niwa--prompt--settle
                 (gethash first-id niwa-view--interaction-overlays)
                 'allow-once)
                (should (equal '(("one" . allow-once)) outcomes))
                (should (= 1 (length (niwa-session-permission-queue session))))
                (let ((next-id (niwa-queue--entry-metadata-get
                                (car (niwa-session-permission-queue session))
                                :interaction-id)))
                  (should next-id)
                  (should (gethash next-id niwa-view--interaction-overlays)))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(niwa-deftest niwa-permission-queue--gptel-batch-dispatch
  (:doc "queues every concurrently dispatched outside-workspace Read")
  (let* ((workspace-root (file-name-as-directory
                          (make-temp-file "niwa-pq-workspace-" t)))
         (outside-root (file-name-as-directory
                        (make-temp-file "niwa-pq-outside-" t)))
         (data-buf (generate-new-buffer " *test-pq-batch-data*"))
         (view-buf (generate-new-buffer " *test-pq-batch-view*"))
         (workspace (niwa-workspace-get-or-create
                     'project workspace-root workspace-root "workspace"))
         (session (niwa-session-create "main" workspace))
         (_register (niwa-tool-fs--register))
         (read-tool (niwa-tool-get "Read"))
         (gptel-tool (niwa-tool-gptel-tool read-tool))
         (paths (list (file-name-concat outside-root "outside-a.el")
                      (file-name-concat outside-root "outside-b.el")
                      (file-name-concat outside-root "outside-c.el")
                      (file-name-concat outside-root "outside-d.el")))
         processed-results)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session)
            (setq-local temporary-file-directory workspace-root))
          (niwa-view--setup view-buf data-buf)
          (let ((niwa-permission-rules nil)
                (niwa-permission-mode 'ask))
            (cl-letf (((symbol-function 'niwa--prompt-block-face)
                       (lambda () 'ask)))
              (with-current-buffer data-buf
                (dolist (path paths)
                  (funcall (gptel-tool-function gptel-tool)
                           (lambda (result)
                             (push result processed-results))
                           path)))
              (should-not processed-results)
              (should (= 4 (length (niwa-session-permission-queue session))))
              (with-current-buffer view-buf
                (should (string-match-p
                         "4 permissions pending"
                         (niwa-view--interaction-count-label)))
                (let* ((first-id
                        (niwa-queue--entry-metadata-get
                         (car (niwa-session-permission-queue session))
                         :interaction-id))
                       (ov (and first-id
                                (gethash first-id
                                         niwa-view--interaction-overlays))))
                  (should first-id)
                  (should ov)
                  (should (overlay-get ov 'niwa--callback))
                  (niwa--prompt--settle ov 'allow-once))
                (should (= 3 (length (niwa-session-permission-queue
                                      session))))
                (should (string-match-p
                         "3 permissions pending"
                         (niwa-view--interaction-count-label))))
              (should (= 1 (length processed-results)))
              (niwa-permission-queue-abort-all session))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (file-directory-p workspace-root)
        (delete-directory workspace-root t))
      (when (file-directory-p outside-root)
        (delete-directory outside-root t))
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-permission--prompt-select-resource
  (:quiet t)
  (let* ((tree-p ,tree-p)
         (approval ,approval)
         (covered-p ,covered-p)
         (hook-event ,hook-event)
         (workspace-root (make-temp-file "niwa-pq-tree-workspace-" t))
         (outside-root (make-temp-file "niwa-pq-tree-outside-" t))
         (tree (file-name-concat outside-root "design"))
         (nested (file-name-concat tree "notes"))
         (first (file-name-concat nested "first.txt"))
         (second (file-name-concat tree "second.txt"))
         (future (file-name-concat nested "future.txt"))
         (unrelated (file-name-concat outside-root "unrelated.txt"))
         (data-buf (generate-new-buffer " *test-pq-tree-data*"))
         (view-buf (generate-new-buffer " *test-pq-tree-view*"))
         (agent-buf (generate-new-buffer " *test-pq-tree-agent*"))
         (workspace (niwa-workspace-get-or-create
                     'project workspace-root workspace-root "workspace"))
         (session (niwa-session-create "main" workspace))
         (_register (niwa-tool-fs--register))
         (read-fn (gptel-tool-function
                   (niwa-tool-gptel-tool (niwa-tool-get "Read"))))
         (draft "> please keep this draft\nsecond line")
         results)
    (unwind-protect
        (progn
          (make-directory nested t)
          (dolist (file (list first second unrelated))
            (with-temp-file file (insert "directory fixture contents")))
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session)
            (setq-local temporary-file-directory workspace-root))
          (niwa-view--setup view-buf data-buf)
          (with-current-buffer agent-buf
            (org-mode)
            (setq-local niwa--session session)
            (setq-local temporary-file-directory workspace-root)
            (setq-local niwa--view-buffer view-buf)
            (setq-local niwa--agent-invocation
                        (niwa-agent-invocation--create
                         :path "/root/reader" :agent-id "/root/reader"
                         :parent-session session :parent-data-buffer data-buf
                         :buffer agent-buf)))
          (setf (niwa-session-permission-mode session) 'ask)
          (with-current-buffer view-buf
            (niwa-view-test--insert-composer-draft draft))
          (let ((niwa-permission-rules nil)
                (niwa-permission-mode 'ask)
                (hook
                 (lambda (event)
                   (when (equal second
                                (plist-get (plist-get event :tool-input)
                                           :file_path))
                     '(:permission-decision ask)))))
            (when hook-event
              (with-current-buffer data-buf
                (add-hook hook-event hook nil t)))
            (cl-letf (((symbol-function 'niwa--prompt-block-face)
                       (lambda () 'ask)))
              (with-current-buffer data-buf
                (dolist (file (list first second unrelated))
                  (funcall read-fn (lambda (result) (push result results)) file)))
              (should (= 3 (length (niwa-session-permission-queue session))))
              (setf (niwa-session-permission-mode session) 'ask)
              (with-current-buffer view-buf
                (should (equal draft (niwa-view-composer-draft-text nil t)))
                (let* ((entry (car (niwa-session-permission-queue session)))
                       (id (niwa-queue--entry-metadata-get entry :interaction-id))
                       (ov (gethash id niwa-view--interaction-overlays))
                       (stale-callback (overlay-get ov 'niwa--callback))
                       (choice (if tree-p
                                   (format "Read %s (recursive)" tree)
                                 (format "Read %s (exact)" first))))
                  (goto-char (overlay-start ov))
                  (let ((before (copy-tree (car (plist-get entry :resource-selection-cell))))
                        cancelled)
                    (cl-letf (((symbol-function 'completing-read)
                               (lambda (&rest _) (signal 'quit nil))))
                      (condition-case nil
                          (call-interactively (lookup-key (overlay-get ov 'keymap) "g"))
                        (quit (setq cancelled t))))
                    (should cancelled)
                    (should (equal before (car (plist-get entry :resource-selection-cell))))
                    (should-not (niwa-session-resource-grants session))
                    (should-not results)
                    (should (equal draft (niwa-view-composer-draft-text nil t))))
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (_prompt collection &rest _)
                               (should (assoc choice collection))
                               choice)))
                    (call-interactively
                     (lookup-key (overlay-get ov 'keymap) "g")))
                  (should-not results)
                  (should (equal draft (niwa-view-composer-draft-text nil t)))
                  (setq ov (gethash id niwa-view--interaction-overlays))
                  (should (string-match-p
                           (regexp-quote choice)
                           (buffer-substring-no-properties
                            (overlay-start ov) (overlay-end ov))))
                  (let ((body (buffer-substring-no-properties
                               (overlay-start ov) (overlay-end ov))))
                    ;; The folded card keeps the actual scope; TAB reveals why.
                    (should-not (string-match-p "admission: ask" body)))
                  (goto-char (overlay-start ov))
                  (call-interactively (lookup-key (overlay-get ov 'keymap) (kbd "TAB")))
                  (setq ov (gethash id niwa-view--interaction-overlays))
                  (let ((body (buffer-substring-no-properties
                               (overlay-start ov) (overlay-end ov))))
                    (should (string-match-p (regexp-quote choice) body))
                    (should (string-match-p "admission: ask" body))
                    (should (string-match-p "outside the allowed roots" body)))
                  (should (equal draft (niwa-view-composer-draft-text nil t)))
                  (goto-char (overlay-start ov))
                  (call-interactively
                   (lookup-key (overlay-get ov 'keymap) approval))
                  (funcall stale-callback 'allow-once))
                (should (equal draft (niwa-view-composer-draft-text nil t))))
              (should (= (if (and covered-p (not hook-event)) 2 1)
                         (length results)))
              (should (cl-every (lambda (result)
                                  (string-match-p "directory fixture contents" result))
                                results))
              (should (= (if (and covered-p (not hook-event)) 1 2)
                         (length (niwa-session-permission-queue session))))
              (with-temp-file future (insert "future descendant contents"))
              (with-current-buffer data-buf
                (funcall read-fn (lambda (result) (push result results)) future))
              (should (= (if covered-p (if hook-event 2 3) 1)
                         (length results)))
              (when covered-p
                (should (string-match-p "future descendant contents" (car results))))
              (should (= (if covered-p (if hook-event 2 1) 3)
                         (length (niwa-session-permission-queue session))))
              (when (and covered-p (not hook-event))
                (dotimes (index 10)
                  (let ((file (file-name-concat tree (format "extra-%d.txt" index))))
                    (with-temp-file file (insert (format "design note %d" index)))
                    (with-current-buffer (if (cl-evenp index) data-buf agent-buf)
                      (funcall read-fn
                               (lambda (result) (push result results)) file))
                    (should (string-match-p
                             (format "design note %d" index) (car results)))))
                (should (= 13 (length results)))
                (with-current-buffer agent-buf
                  (funcall (gptel-tool-function
                            (niwa-tool-gptel-tool (niwa-tool-get "Glob")))
                           (lambda (result) (push result results)) "*.txt" tree))
                (with-current-buffer data-buf
                  (funcall (gptel-tool-function
                            (niwa-tool-gptel-tool (niwa-tool-get "Grep")))
                           (lambda (result) (push result results)) "design note" tree))
                (let ((deadline (+ (float-time) 10)))
                  (while (and (< (length results) 15) (< (float-time) deadline))
                    (accept-process-output nil 0.01)))
                (should (= 15 (length results)))
                (should (string-match-p "extra-0.txt" (car results)))
                (should (string-match-p "extra-0.txt" (cadr results)))
                (when (equal approval "s")
                  ;; The same documentation tree covers an explicitly requested
                  ;; confined decompressor after native reading and searching.
                  (let ((compressed (file-name-concat tree "manual.gz"))
                        (niwa-permission-reviewer 'user)
                        decompressed)
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (let ((coding-system-for-read 'no-conversion)
                            (coding-system-for-write 'no-conversion))
                        (should (zerop (process-file "gzip" nil t nil "-c" first)))
                        (write-region (point-min) (point-max) compressed nil 'silent)))
                    (niwa-tool-exec--register)
                    (setf (niwa-session-permission-mode session) 'edits
                          (niwa-session-sandbox-mode session) 'required)
                    (with-current-buffer data-buf
                      (niwa-pipeline-run-tool
                       (niwa-tool-get "Bash") (lambda (value) (setq decompressed value))
                       (list :command (concat "gzip -cd -- " (shell-quote-argument compressed))
                             :description "Read compressed manual content"
                             :sandbox_permissions "with_additional_permissions"
                             :additional_permissions (list :file_system (list :read (vector compressed)))
                             :justification "Read the compressed manual in the approved documentation tree")))
                    (let ((deadline (+ (float-time) 10)))
                      (while (and (not decompressed) (< (float-time) deadline))
                        (accept-process-output nil 0.01)))
                    (should decompressed)
                    (should (string-match-p "directory fixture contents" decompressed))
                    (should (string-match-p "sandbox: bubblewrap" decompressed))
                    (should (string-match-p "exit_code=\"0\"" decompressed))))
                (should (= 1 (length (niwa-session-permission-queue session)))))
              (niwa-permission-queue-abort-all session)
              (niwa-permission-queue-abort-all session)
              (let* ((events (append (test-pq--read-permission-log session)
                                     (niwa-session-permission-log-pending session)))
                     (admissions (seq-filter
                                  (lambda (event)
                                    (eq (plist-get event :event) 'permission-enqueued))
                                  events))
                     (first-id (plist-get (car admissions) :permission-id)))
                (should (stringp first-id))
                (should (= (length admissions)
                           (length (delete-dups
                                    (mapcar (lambda (event)
                                              (plist-get event :permission-id))
                                            admissions)))))
                (dolist (admission admissions)
                  (let* ((id (plist-get admission :permission-id))
                         (related (seq-filter (lambda (event)
                                                (equal id (plist-get event :permission-id)))
                                              events))
                         (terminal (seq-filter
                                    (lambda (event)
                                      (memq (plist-get event :event)
                                            '(permission-resolved permission-coalesced
                                              permission-aborted permission-swept)))
                                    related)))
                    (should (= 1 (length terminal)))
                    (should (<= (cl-count 'permission-displayed related
                                         :key (lambda (event) (plist-get event :event))) 1))
                    (dolist (event related)
                      (should (eq (plist-get admission :permission-mode-effective)
                                  (plist-get event :permission-mode-effective))))))
                (let* ((resolved (seq-find
                                  (lambda (event)
                                    (and (equal first-id (plist-get event :permission-id))
                                         (eq (plist-get event :event) 'permission-resolved)))
                                  events))
                       (scope (car (plist-get resolved :selected-resources))))
                  (should (eq 'ask (plist-get resolved :permission-mode-base)))
                  (should (eq 'ask (plist-get resolved :permission-mode-effective)))
                  (should (equal (if tree-p tree first) (plist-get scope :path)))
                  (should (eq tree-p (plist-get scope :recursive)))
                  (should (eq (pcase approval
                                ("a" 'invocation) ("s" 'session) ("A" 'workspace))
                              (plist-get resolved :approval-lifetime)))
                  (should (= 1 (cl-count-if
                                (lambda (event)
                                  (and (equal first-id (plist-get event :permission-id))
                                       (eq (plist-get event :event) 'permission-displayed)))
                                events)))))
              (with-current-buffer view-buf
                (should (equal draft (niwa-view-composer-draft-text nil t)))))))
      (niwa-permission-queue-abort-all session)
      (niwa-execution-teardown-session session)
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (delete-directory workspace-root t)
      (delete-directory outside-root t)
      (niwa-workspace-clear-registry)))

  (tree-p approval covered-p hook-event)
  :doc "directory selection approves queued and future real reads"
  t "s" t nil
  :doc "exact approval leaves sibling and future descendant reads pending"
  nil "s" nil nil
  :doc "invocation directory approval leaves all other reads pending"
  t "a" nil nil
  :doc "workspace directory approval covers queued and future reads"
  t "A" t nil
  :doc "directory approval preserves a sibling PreToolUse ask"
  t "s" t 'niwa-pre-tool-use-functions
  :doc "directory approval preserves a sibling PermissionRequest ask"
  t "s" t 'niwa-permission-request-functions)

(niwa-deftest niwa-permission-queue--execution-directory-profile
                 (:quiet t)
                 (progn
                   (let ((availability (niwa-sandbox-probe)))
                     (unless (plist-get availability :available)
                       (ert-skip (plist-get availability :reason))))
                   (let* ((approval ,approval)
                          (remember-p (not (equal approval "RET")))
                          (git-p ,git-p)
                          (root (make-temp-file "niwa-pq-exec-root-" t))
                          (external (make-temp-file "niwa-pq-exec-external-" t))
                          (cache (if git-p (file-name-concat root ".git")
                                   (file-name-concat external "cache")))
                          (script (file-name-concat root "validate"))
                          (counter (file-name-concat root "counter"))
                          (sibling (file-name-concat root "sibling" ".git"))
                          (nested (file-name-concat cache "private"))
                          (niwa-protected-paths
                           (append niwa-protected-paths
                                   (when git-p (list (cons (concat nested "/**") 'inaccessible)))))
                          (data-buf (generate-new-buffer " *test-pq-exec-data*"))
                          (view-buf (generate-new-buffer " *test-pq-exec-view*"))
                          (workspace (niwa-workspace-get-or-create
                                      'project root root "workspace"))
                          (session (niwa-session-create "main" workspace))
                          (_register (niwa-tool-exec--register))
                          result)
                     (unwind-protect
                         (progn
                           (make-directory (file-name-concat root "tmp"))
                           (if git-p
                               (let ((default-directory root))
                                 (should (zerop (process-file "git" nil nil nil "init" "-q")))
                                 (make-directory (file-name-directory sibling))
                                 (let ((default-directory (file-name-directory sibling)))
                                   (should (zerop (process-file "git" nil nil nil "init" "-q"))))
                                 (make-directory nested)
                                 (with-temp-file (file-name-concat nested "value")
                                   (insert "protected nested content"))
                                 (make-directory (file-name-concat root ".scratch"))
                                 (with-temp-file (file-name-concat root ".scratch" "note")
                                   (insert "keep locally")))
                             (make-directory cache))
                           (with-temp-file script
                             (insert "#!/bin/sh\nset -eu\n"
                                     "n=0\n"
                                     (format "if test -f %s; then n=$(cat %s); fi\n"
                                             (shell-quote-argument counter) (shell-quote-argument counter))
                                     "n=$((n + 1))\n"
                                     (format "printf '%%s' \"$n\" > %s\n" (shell-quote-argument counter))
                                     (format "mkdir -p %s/run-\"$n\"\n" (shell-quote-argument cache))
                                     (format "printf 'cache value' > %s/run-\"$n\"/value\n"
                                             (shell-quote-argument cache))
                                     (format "if touch %s 2>/dev/null; then exit 55; fi\n"
                                             (shell-quote-argument (file-name-concat external "unapproved")))
                                     (if git-p
                                         (concat
                                          (format "if touch %s 2>/dev/null; then exit 56; fi\n"
                                                  (shell-quote-argument (file-name-concat sibling "unapproved")))
                                          (format "if cat %s 2>/dev/null; then exit 57; fi\n"
                                                  (shell-quote-argument (file-name-concat nested "value")))
                                          (format "if touch %s 2>/dev/null; then exit 58; fi\n"
                                                  (shell-quote-argument (file-name-concat nested "unapproved")))
                                          "git rm --cached -r -- .scratch/\n")
                                       "")
                                     "printf 'confined validation complete'\n"))
                           (set-file-modes script #o700)
                           (setf (niwa-session-permission-mode session) 'edits
                                 (niwa-session-sandbox-mode session) 'required)
                           (when git-p
                             ;; Reproduce remembered parent profile + exact child.
                             (setf (niwa-session-permission-rules session)
                                   `(("Bash" :pattern ,script :action allow
                                      :file-system ((:path ,root :access write :recursive t))))))
                           (with-current-buffer data-buf
                             (org-mode)
                             (setq-local niwa--session session)
                             (setq-local temporary-file-directory (file-name-concat root "tmp")))
                           (niwa-view--setup view-buf data-buf)
                           (with-current-buffer view-buf
                             (niwa-view-test--insert-composer-draft "> keep this draft\nand its second line"))
                           (let ((niwa-permission-rules nil)
                                 (niwa-permission-reviewer 'user))
                             (cl-letf (((symbol-function 'niwa--prompt-block-face)
                                        (lambda () 'ask)))
                                      (dotimes (index (if remember-p 4 1))
                                        (setq result nil)
                                        (when git-p
                                          (let ((default-directory root))
                                            (should (zerop (process-file "git" nil nil nil
                                                                         "add" "--" ".scratch/")))))
                                        (with-current-buffer data-buf
                                          (niwa-pipeline-run-tool
                                           (niwa-tool-get "Bash")
                                           (lambda (value) (setq result value))
                                           (append
                                            (list :command script :description "Probe execution directory authority")
                                            (when (or (zerop index) (and git-p (= index 1)))
                                              (list :sandbox_permissions "with_additional_permissions"
                                                    :additional_permissions
                                                    (list :file_system (list :write (vector cache)))
                                                    :justification "Write validation cache")))))
                                        (when (zerop index)
                                          (should (= 1 (length (niwa-session-permission-queue session))))
                                          (with-current-buffer view-buf
                                            (let* ((entry (car (niwa-session-permission-queue session)))
                                                   (id (niwa-queue--entry-metadata-get entry :interaction-id))
                                                   (ov (gethash id niwa-view--interaction-overlays))
                                                   (choice (format "Write %s (recursive)" cache)))
                                              (goto-char (overlay-start ov))
                                              (should (plist-get
                                                       (car (car (plist-get entry :resource-selection-cell)))
                                                       :recursive))
                                              (should (string-match-p
                                                       (regexp-quote choice)
                                                       (buffer-substring-no-properties
                                                        (overlay-start ov) (overlay-end ov))))
                                              (when (and git-p (equal approval "s"))
                                                (cl-letf (((symbol-function 'completing-read)
                                                           (lambda (prompt choices &rest _)
                                                             (if (equal prompt "Resource to change: ")
                                                                 (format "Write %s (exact)" cache)
                                                               (should-not (assoc (format "Write %s (exact)" cache)
                                                                                  choices))
                                                               choice))))
                                                  (call-interactively (lookup-key (overlay-get ov 'keymap) "g")))
                                                (setq ov (gethash id niwa-view--interaction-overlays)))
                                              (should (equal "> keep this draft\nand its second line"
                                                             (niwa-view-composer-draft-text nil t)))
                                              (goto-char (overlay-start ov))
                                              (call-interactively
                                               (lookup-key (overlay-get ov 'keymap) (kbd approval))))))
                                        (let ((deadline (+ (float-time) 15)))
                                          (while (and (not result) (< (float-time) deadline))
                                            (accept-process-output nil 0.01)))
                                        (should result)
                                        (should (string-match-p "confined validation complete" result))
                                        (should (string-match-p "sandbox: bubblewrap" result))
                                        (should (file-exists-p
                                                 (file-name-concat cache (format "run-%d" (1+ index)) "value")))
                                        (when git-p
                                          (should (file-exists-p (file-name-concat root ".scratch" "note")))
                                          (with-temp-buffer
                                            (let ((default-directory root))
                                              (should (zerop (process-file "git" nil t nil "ls-files"))))
                                            (should (string-empty-p (buffer-string)))))
                                        (should-not (file-exists-p (file-name-concat external "unapproved")))
                                        (should-not (file-exists-p (file-name-concat sibling "unapproved")))
                                        (should-not (file-exists-p (file-name-concat nested "unapproved")))
                                        (should-not (niwa-session-permission-queue session)))
                                      (unless remember-p
                                        (setq result nil)
                                        (with-current-buffer data-buf
                                          (niwa-pipeline-run-tool
                                           (niwa-tool-get "Bash")
                                           (lambda (value) (setq result value))
                                           (list :command script :description "Probe execution directory authority")))
                                        (let ((deadline (+ (float-time) 15)))
                                          (while (and (not result) (< (float-time) deadline))
                                            (accept-process-output nil 0.01)))
                                        (should result)
                                        (should-not (string-match-p "confined validation complete" result))
                                        (should-not (niwa-session-permission-queue session))
                                        (should-not (file-exists-p (file-name-concat cache "run-2" "value")))))))
                       (niwa-permission-queue-abort-all session)
                       (niwa-execution-teardown-session session)
                       (when (buffer-live-p view-buf) (kill-buffer view-buf))
                       (when (buffer-live-p data-buf) (kill-buffer data-buf))
                       (delete-directory root t)
                       (delete-directory external t)
                       (niwa-workspace-clear-registry))))
                 (approval git-p)
                 :doc "real confined cache writes reuse the default directory profile"
                 "s" nil
                 :doc "RET grants the default tree only for the invocation"
                 "RET" nil
                 :doc "RET permits Git index changes while preserving local scratch files"
                 "RET" t
                 :doc "session approval remembers the displayed Git tree"
                 "s" t
                 :doc "workspace approval remembers the displayed Git tree"
                 "A" t)

(niwa-deftest niwa-permission-queue--protected-child-lifecycle
  (:quiet t :doc "real Bash preserves child denial, cancellation, reusable authority and revocation")
  (let* ((root (make-temp-file "niwa-child-lifecycle-" t))
         (git (file-name-concat root ".git"))
         (witness (file-name-concat git "child-witness"))
         (script (file-name-concat root "validate-child"))
         (buffer (generate-new-buffer " *test-protected-child-lifecycle*"))
         (workspace (niwa-workspace-get-or-create 'project root root "workspace"))
         (session (niwa-session-create "main" workspace))
         (parent-rule `("Bash" :pattern ,script :action allow
                        :file-system ((:path ,root :access write :recursive t))))
         (child `(:path ,git :access write :recursive t))
         (args `(:command ,script :description "Probe confined execution authority" :sandbox_permissions "with_additional_permissions"
                 :additional_permissions (:file_system (:write [,git]))
                 :justification "Write disposable child marker"))
         (niwa-protected-paths '(("**/.git/**" . read-only)))
         (niwa-permission-rules nil)
         (niwa-permission-reviewer 'user)
         (_register (niwa-tool-exec--register))
         entry result)
    (unwind-protect
        (progn
          (let ((availability (niwa-sandbox-probe)))
            (unless (plist-get availability :available)
              (ert-skip (plist-get availability :reason))))
          (let ((default-directory root))
            (should (zerop (process-file "git" nil nil nil "init" "-q"))))
          (make-directory (file-name-concat root "tmp"))
          (with-temp-file script
            (insert "#!/bin/sh\nset -eu\n"
                    (format "printf 'witness' > %s\n" (shell-quote-argument witness))
                    "printf 'protected child ran'\n"))
          (set-file-modes script #o700)
          (setf (niwa-session-permission-mode session) 'edits
                (niwa-session-sandbox-mode session) 'required
                (niwa-session-permission-rules session) (list parent-rule))
          (with-current-buffer buffer
            (setq-local niwa--session session
                        temporary-file-directory (file-name-concat root "tmp"))
            (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                       (lambda (item) (setq entry item))))
              (dolist (outcome '(deny-once aborted))
                (setq entry nil result nil)
                (niwa-pipeline-run-tool (niwa-tool-get "Bash")
                                          (lambda (value) (setq result value)) args)
                (should entry)
                (should (eq 'ask (niwa-permission-queue--reevaluate entry)))
                (if (eq outcome 'aborted)
                    (niwa-permission-queue-abort-all session)
                  (niwa-permission-queue--on-head-outcome entry outcome))
                (should result)
                (should-not (file-exists-p witness))
                (should-not (niwa-session-permission-queue session))
                (should-not (niwa-session-resource-grants session)))
              ;; Independent sufficient child authority reuses the exact request
              ;; without selecting or exposing a broader checkout exception.
              (setf (niwa-session-resource-grants session) (list child))
              (setq entry nil result nil)
              (niwa-pipeline-run-tool (niwa-tool-get "Bash")
                                        (lambda (value) (setq result value)) args)
              (let ((deadline (+ (float-time) 15)))
                (while (and (not result) (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should-not entry)
              (should (string-match-p "protected child ran" result))
              (should (string-match-p "sandbox: bubblewrap" result))
              (should (file-exists-p witness))
              (delete-file witness)
              (setf (niwa-session-resource-grants session) nil)
              (setq entry nil result nil)
              (niwa-pipeline-run-tool (niwa-tool-get "Bash")
                                        (lambda (value) (setq result value)) args)
              (should entry)
              (should (eq 'ask (niwa-permission-queue--reevaluate entry)))
              (setf (niwa-session-resource-grants session) (list child))
              (should (eq 'allow (niwa-permission-queue--reevaluate entry)))
              (setf (niwa-session-resource-grants session) nil)
              (should (eq 'ask (niwa-permission-queue--reevaluate entry)))
              (niwa-permission-queue-abort-all session)
              (should-not (file-exists-p witness)))))
      (niwa-permission-queue-abort-all session)
      (niwa-execution-teardown-session session)
      (kill-buffer buffer)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-permission-queue--masked-directory-read
  (:quiet t :doc "human approval mounts a masked read tree without exposing its sibling")
  (let* ((root (make-temp-file "niwa-read-tree-" t))
         (mask (file-name-concat root "hidden"))
         (tree (file-name-concat mask "selected"))
         (sibling (file-name-concat mask "sibling"))
         (buffer (generate-new-buffer " *test-masked-directory-read*"))
         (workspace (niwa-workspace-get-or-create 'project root root "workspace"))
         (session (niwa-session-create "main" workspace))
         (niwa-protected-paths (list (cons mask 'inaccessible)))
         (_register (niwa-tool-exec--register))
         (niwa-permission-reviewer 'user)
         (niwa-permission-rules nil)
         entry result)
    (unwind-protect
        (progn
          (let ((availability (niwa-sandbox-probe)))
            (unless (plist-get availability :available)
              (ert-skip (plist-get availability :reason))))
          (make-directory tree t)
          (make-directory (file-name-concat root "tmp"))
          (with-temp-file (file-name-concat tree "value") (insert "approved read"))
          (with-temp-file sibling (insert "unapproved read"))
          (setf (niwa-session-permission-mode session) 'edits)
          (with-current-buffer buffer
            (setq-local niwa--session session
                        temporary-file-directory (file-name-concat root "tmp"))
            (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                       (lambda (item) (setq entry item))))
              (niwa-pipeline-run-tool
               (niwa-tool-get "Bash") (lambda (value) (setq result value))
               (list :description "Probe masked directory read" :command
                     (format "cat %s; if cat %s 2>/dev/null; then exit 55; fi"
                             (shell-quote-argument (file-name-concat tree "value"))
                             (shell-quote-argument sibling))
                     :sandbox_permissions "with_additional_permissions"
                     :additional_permissions (list :file_system (list :read (vector tree)))
                     :justification "Read the selected directory"))
              (should entry)
              (should (equal (list (list :path tree :access 'read :recursive t))
                             (car (plist-get entry :resource-selection-cell))))
              (niwa-permission-queue--on-head-outcome entry 'allow-once)))
          (let ((deadline (+ (float-time) 15)))
            (while (and (not result) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should (string-match-p "approved read" result))
          (should-not (string-match-p "unapproved read" result))
          (should (string-match-p "sandbox: bubblewrap" result))
          (should-not (niwa-session-resource-grants session)))
      (niwa-permission-queue-abort-all session)
      (niwa-execution-teardown-session session)
      (kill-buffer buffer)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-permission-queue--gptel-batch-deny-once
  (:doc "deny-once settles one queued gptel-dispatched permission")
  (let* ((workspace-root (file-name-as-directory
                          (make-temp-file "niwa-pq-deny-workspace-" t)))
         (outside-root (file-name-as-directory
                        (make-temp-file "niwa-pq-deny-outside-" t)))
         (data-buf (generate-new-buffer " *test-pq-deny-data*"))
         (view-buf (generate-new-buffer " *test-pq-deny-view*"))
         (workspace (niwa-workspace-get-or-create
                     'project workspace-root workspace-root "workspace"))
         (session (niwa-session-create "main" workspace))
         (_register (niwa-tool-fs--register))
         (read-tool (niwa-tool-get "Read"))
         (gptel-tool (niwa-tool-gptel-tool read-tool))
         (paths (list (file-name-concat outside-root "outside-a.el")
                      (file-name-concat outside-root "outside-b.el")
                      (file-name-concat outside-root "outside-c.el")
                      (file-name-concat outside-root "outside-d.el")))
         processed-results)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session)
            (setq-local temporary-file-directory workspace-root))
          (niwa-view--setup view-buf data-buf)
          (let ((niwa-permission-rules nil)
                (niwa-permission-mode 'ask))
            (cl-letf (((symbol-function 'niwa--prompt-block-face)
                       (lambda () 'ask)))
              (with-current-buffer data-buf
                (dolist (path paths)
                  (funcall (gptel-tool-function gptel-tool)
                           (lambda (result)
                             (push result processed-results))
                           path)))
              (should-not processed-results)
              (should (= 4 (length (niwa-session-permission-queue session))))
              (with-current-buffer view-buf
                (should (string-match-p
                         "4 permissions pending"
                         (niwa-view--interaction-count-label)))
                (let* ((first-id
                        (niwa-queue--entry-metadata-get
                         (car (niwa-session-permission-queue session))
                         :interaction-id))
                       (ov (and first-id
                                (gethash first-id
                                         niwa-view--interaction-overlays))))
                  (should first-id)
                  (should ov)
                  (niwa--prompt--settle ov 'deny-once))
                (should (= 3 (length (niwa-session-permission-queue
                                      session))))
                (should (string-match-p
                         "3 permissions pending"
                         (niwa-view--interaction-count-label))))
              (should (= 1 (length processed-results)))
              (should (string-match-p
                       "Error: Permission denied"
                       (car processed-results)))
              (niwa-permission-queue-abort-all session))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (file-directory-p workspace-root)
        (delete-directory workspace-root t))
      (when (file-directory-p outside-root)
        (delete-directory outside-root t))
      (niwa-workspace-clear-registry))))






;;
;;; Coalesce vocabulary translation

(niwa-deftest niwa-permission-queue--translate-coalesce-outcome
  (:doc "translates 'allow / 'deny to the kind's expected callback vocabulary")
  ,test
  (test)

  :doc "generic kind passes 'allow / 'deny through unchanged"
  (should (eq 'allow
              (niwa-permission-queue--translate-coalesce-outcome
               'generic 'allow)))
  (should (eq 'deny
              (niwa-permission-queue--translate-coalesce-outcome
               'generic 'deny)))

  :doc "bash kind passes 'allow through"
  (should (eq 'allow
              (niwa-permission-queue--translate-coalesce-outcome
               'bash 'allow)))
  (should (eq 'deny
              (niwa-permission-queue--translate-coalesce-outcome
               'bash 'deny)))

  :doc "eval kind translates 'allow to authoritative allow-once"
  (should (eq 'allow-once
              (niwa-permission-queue--translate-coalesce-outcome
               'eval 'allow)))

  :doc "sandbox kind translates both outcomes to once-only vocabulary"
  (should (eq 'allow-once
              (niwa-permission-queue--translate-coalesce-outcome
               'sandbox 'allow)))
  (should (eq 'deny-once
              (niwa-permission-queue--translate-coalesce-outcome
               'sandbox 'deny))))


;;
;;; Abort flush

(niwa-deftest niwa-permission-queue-abort-all
  (:doc "abort flushes queue and fires 'aborted on every callback")
  ,test
  (test)

  :doc "every queued callback fires 'aborted; queue is empty after"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :origin "/root"
             :callback (lambda (o) (push (cons "Read" o) outcomes))))
      (niwa-permission-request
       (list :kind 'bash :command "rm /tmp/x"
             :origin "/root"
             :callback (lambda (o) (push (cons "Bash" o) outcomes))))
      (niwa-permission-queue-abort-all session))
    (should (= 2 (length outcomes)))
    (should (cl-every (lambda (o) (eq 'aborted (cdr o))) outcomes))
    (should (null (niwa-session-permission-queue session))))

  :doc "abort on empty queue is a no-op"
  (let ((session (test-pq--make-session)))
    (should-not (niwa-permission-queue-abort-all session))
    (should (null (niwa-session-permission-queue session)))))


;;
;;; Per-request sweep

(niwa-deftest niwa-permission-queue-sweep-request
  (:doc "sweep fires 'aborted on entries owned by REQUEST-ID; others stay")
  ,test
  (test)

  :doc "same-origin entries from other requests remain queued"
  (let* ((session (test-pq--make-session))
         (niwa--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
               #'ignore))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :origin "/root/explorer"
             :request-id "request-1"
             :callback (lambda (o) (push (cons "explorer" o) outcomes))))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :origin "/root/explorer"
             :request-id "request-2"
             :callback (lambda (o) (push (cons "main" o) outcomes))))
      (niwa-permission-request
       (list :kind 'generic :tool-name "Read"
             :origin "/root/explorer"
             :request-id "request-1"
             :callback (lambda (o) (push (cons "explore2" o) outcomes))))
      (niwa-permission-queue-sweep-request "request-1" session))
    (should (eq 'aborted (cdr (assoc "explorer" outcomes))))
    (should (eq 'aborted (cdr (assoc "explore2" outcomes))))
    (should-not (assoc "main" outcomes))
    (let ((q (niwa-session-permission-queue session)))
      (should (= 1 (length q)))
      (should (equal "request-2" (plist-get (car q) :request-id)))))

  :doc "sweeping the visible head removes its interaction overlay"
  (let ((data-buf (generate-new-buffer " *test-pq-sweep-data*"))
        (view-buf (generate-new-buffer " *test-pq-sweep-view*"))
        (session (test-pq--make-session))
        (draft "> preserve during cancellation\nsecond line")
        outcomes
        swept-id)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (niwa-view-test--insert-composer-draft draft))
          (cl-letf (((symbol-function 'niwa--prompt-block-face)
                     (lambda () 'ask)))
            (with-current-buffer data-buf
              (niwa-permission-request
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-value "/tmp/agent.txt"
                     :include-always nil
                     :origin "/root/explorer"
                     :request-id "request-1"
                     :callback (lambda (o) (push (cons "agent" o) outcomes))))
              (setq swept-id
                    (niwa-queue--entry-metadata-get
                     (car (niwa-session-permission-queue session))
                     :interaction-id))
              (niwa-permission-request
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-value "/tmp/main.txt"
                     :include-always nil
                     :origin "/root/explorer"
                     :request-id "request-2"
                     :callback (lambda (o) (push (cons "main" o) outcomes))))
              (should swept-id)
              (with-current-buffer view-buf
                (should (gethash swept-id niwa-view--interaction-overlays)))
              (niwa-permission-queue-sweep-request "request-1" session)))
          (should (eq 'aborted (cdr (assoc "agent" outcomes))))
          (should-not (assoc "main" outcomes))
          (with-current-buffer view-buf
            (should-not (gethash swept-id niwa-view--interaction-overlays))
            (should (= 1 (hash-table-count
                          niwa-view--interaction-overlays))))
          (let ((q (niwa-session-permission-queue session)))
            (should (= 1 (length q)))
            (should (equal "request-2"
                           (plist-get (car q) :request-id))))
          (with-current-buffer view-buf
            (should (equal draft (niwa-view-composer-draft-text nil t)))
            (let* ((entry (car (niwa-session-permission-queue session)))
                   (id (niwa-queue--entry-metadata-get entry :interaction-id))
                   (overlay (gethash id niwa-view--interaction-overlays)))
              (niwa--prompt--settle overlay 'aborted)
              (niwa--prompt--settle overlay 'aborted))
            (should-not (niwa-session-permission-queue session))
            (should (equal draft (niwa-view-composer-draft-text nil t))))
          (niwa-permission-queue-sweep-request "request-1" session)
          (niwa-permission-queue-abort-all session)
          (should (= 2 (length outcomes)))
          (should (eq 'aborted (cdr (assoc "main" outcomes))))
          (should-not (niwa-session-permission-queue session))
          (let* ((events (niwa-session-permission-log-pending session))
                 (terminal
                  (seq-filter
                   (lambda (event)
                     (memq (plist-get event :event)
                           '(permission-resolved permission-swept permission-aborted)))
                   events)))
            (should (equal '(permission-swept permission-aborted)
                           (mapcar (lambda (event) (plist-get event :event)) terminal)))
            (should (eq 'cancelled (plist-get (cadr terminal) :settlement-source)))
            (dolist (end terminal)
              (let ((related
                     (seq-filter
                      (lambda (event)
                        (equal (plist-get end :permission-id)
                               (plist-get event :permission-id)))
                      events)))
                (should (= 3 (length related)))
                (should (= 1 (seq-count
                              (lambda (event)
                                (eq 'permission-displayed (plist-get event :event)))
                              related)))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))


(provide 'test-niwa-permission-queue)
;;; test-niwa-permission-queue.el ends here
