;;; test-niwa-structs.el --- Tests for niwa-structs.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-structs)
(require 'niwa-execution-target)
(require 'niwa-permissions)
(require 'niwa-permission-queue)
(require 'niwa-plan-mode)
(require 'niwa-goal)
(require 'niwa-agents)
(require 'niwa-reminders)
(require 'niwa-sandbox)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-session-persistence)
(require 'niwa-telemetry)
(require 'niwa-turn)
(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;; `niwa-agents'
(defvar niwa--agent-invocation)


;;
;;; Session transient state

(niwa-deftest niwa-conversation-working-directory
  (:doc "projects only the selected conversation directory, preserving root and sibling defaults")
  (let* ((workspace (niwa-workspace--create :root "/tmp/workspace/"))
         (session (niwa-session--create :workspace workspace
                                           :working-directory "/tmp/workspace/session/")))
    (with-temp-buffer
      (setq-local niwa--session session)
      (let ((parent (current-buffer)))
        (should (equal "/tmp/workspace/session/"
                       (niwa-conversation-working-directory)))
        (with-temp-buffer
          (setq-local niwa--session session
                      niwa--conversation-working-directory "/tmp/workspace/child/")
          (should (equal "/tmp/workspace/child/"
                         (niwa-conversation-working-directory)))
          (should (equal "/tmp/other-session/"
                         (niwa-conversation-working-directory
                          (niwa-session--create
                           :working-directory "/tmp/other-session/"))))
          (should (equal "/tmp/workspace/session/"
                         (niwa-conversation-working-directory session parent))))
        (should (equal "/tmp/workspace/session/"
                       (niwa-session-working-directory session)))))))

(niwa-deftest niwa-session-authority-mode-for-session
  (:doc "normalizes durable workspaces, permits workspace-free state, and rejects obsolete modes")
  (let* ((workspace
          (niwa-workspace--create
           :type 'project :id "project" :root "/tmp/project/"
           :name "project"))
         (session (niwa-session--create :workspace workspace)))
    (should (eq 'portable
                (niwa-session-authority-mode-for-session session)))
    (should (eq 'portable (niwa-session-authority-mode session)))
    (setf (niwa-session-authority-mode session) 'pid-lock)
    (should-error
     (niwa-session-authority-mode-for-session session)
     :type 'error)
    (should-not
     (niwa-session-authority-mode-for-session
      (niwa-session--create)))
    (setf (niwa-session-authority-mode session) 'unknown)
    (should-error
     (niwa-session-authority-mode-for-session session)
     :type 'error)))

(niwa-deftest niwa-session-authority-mode-for-workspace ()
  ,test
  (test)
  :doc "all workspace types share portable authority"
  (dolist (type '(project file custom))
    (should (eq 'portable
                (niwa-session-authority-mode-for-workspace
                 (niwa-workspace--create :type type)))))
  :doc "workspace-free sessions claim no durable authority"
  (let ((session (niwa-session--create)))
    (should-not (niwa-session-authority-mode-for-workspace nil))
    (should-not (niwa-session-durability--portable-session-p session))
    (with-temp-buffer
      (setq-local niwa--session session)
      (should (niwa-session-artifacts-assert-mutation-authority session))
      (should (niwa-session-artifacts-assert-new-mutation-authority session)))
    (should-not (niwa-session-save-path session))))

(niwa-deftest niwa-session-audit-target ()
  ,test
  (test)
  :doc "defaults to the runtime session and honors a transient audit target"
  (let* ((parent (niwa-session--create :name "parent"))
         (side (niwa-session--create :name "side")))
    (should (eq side (niwa-session-audit-target side)))
    (setf (niwa-session-audit-session side) parent)
    (should (eq parent (niwa-session-audit-target side)))))

(niwa-deftest niwa-session-pending-inputs ()
  ,test
  (test)
  :doc "returns each pending input category and rejects unknown categories"
  (let ((session (niwa-session--create
                  :pending-steering '((:input "steer"))
                  :pending-follow-ups '((:input "later")))))
    (should (equal '((:input "steer"))
                   (niwa-session-pending-inputs session 'steering)))
    (should (equal '((:input "later"))
                   (niwa-session-pending-inputs session 'follow-up)))
    (should-error
     (niwa-session-pending-inputs session 'unknown))))

(niwa-deftest niwa-session-set-pending-inputs ()
  ,test
  (test)
  :doc "replaces one pending category without changing the other"
  (let ((session (niwa-session--create
                  :pending-steering '((:input "steer")))))
    (should (equal '((:input "later"))
                   (niwa-session-set-pending-inputs
                    session 'follow-up '((:input "later")))))
    (should (equal '((:input "steer"))
                   (niwa-session-pending-steering session)))
    (should-error
     (niwa-session-set-pending-inputs session 'unknown nil))))

(niwa-deftest niwa-session-pending-input-p ()
  ,test
  (test)
  :doc "combines the two pending-input categories"
  (let ((session (niwa-session--create)))
    (should-not (niwa-session-pending-input-p session))
    (setf (niwa-session-pending-steering session) '((:input "now")))
    (should (niwa-session-pending-input-p session))
    (setf (niwa-session-pending-steering session) nil
          (niwa-session-pending-follow-ups session) '((:input "later")))
    (should (niwa-session-pending-input-p session))))

(niwa-deftest niwa-session-enqueue-pending-input ()
  ,test
  (test)
  :doc "assigns stable session-local IDs and appends within each category"
  (let* ((session (niwa-session--create))
         (first (niwa-session-enqueue-pending-input
                 session 'follow-up '(:input "first")))
         (steering (niwa-session-enqueue-pending-input
                    session 'steering '(:input "steer")))
         (second (niwa-session-enqueue-pending-input
                  session 'follow-up '(:input "second"))))
    (should (equal '(1 3)
                   (mapcar (lambda (entry) (plist-get entry :id))
                           (niwa-session-pending-follow-ups session))))
    (should (= 2 (plist-get steering :id)))
    (should (eq 'follow-up (plist-get first :category)))
    (should (eq 'follow-up (plist-get second :category)))
    (should (eq 'steering (plist-get steering :category)))))

(niwa-deftest niwa-session-set-pending-input-paused ()
  ,test
  (test)
  :doc "normalizes the transient delivery pause to a boolean"
  (let ((session (niwa-session--create)))
    (should
     (niwa-session-set-pending-input-paused session 'cockpit))
    (should (eq t (niwa-session-pending-input-paused session)))
    (should-not
     (niwa-session-set-pending-input-paused session nil))
    (should-not (niwa-session-pending-input-paused session))))

(niwa-deftest niwa-session-set-pending-input-failure-paused ()
  ,test
  (test)
  :doc "normalizes failure recovery pause to a boolean"
  (let ((session (niwa-session--create)))
    (should
     (niwa-session-set-pending-input-failure-paused session 'failure))
    (should (eq t
                (niwa-session-pending-input-failure-paused session)))
    (should-not
     (niwa-session-set-pending-input-failure-paused session nil))))

(niwa-deftest niwa-session-pending-input-delivery-paused-p ()
  ,test
  (test)
  :doc "combines cockpit pause and failure pause"
  (let ((session (niwa-session--create)))
    (should-not (niwa-session-pending-input-delivery-paused-p session))
    (niwa-session-set-pending-input-paused session t)
    (should (niwa-session-pending-input-delivery-paused-p session))
    (niwa-session-set-pending-input-paused session nil)
    (niwa-session-set-pending-input-failure-paused session t)
    (should (niwa-session-pending-input-delivery-paused-p session))))

(niwa-deftest niwa-session-set-hook-context-pending ()
  ,test
  (test)
  :doc "replaces the session's pending hook context entries"
  (let* ((session (niwa-session--create))
         (entries '((:event SessionStart :body "context"))))
    (should (equal entries
                   (niwa-session-set-hook-context-pending
                    session entries)))
    (should (equal entries
                   (niwa-session-hook-context-pending session)))))


;;
;;; Agent tree identity

(niwa-deftest niwa-agent-path-p ()
  ,test
  (test)
  :doc "accepts root and lowercase ASCII descendant segments only"
  (dolist (path '("/root" "/root/alpha" "/root/alpha/child_2"
                  "/root/switch-buffer-error" "/root/task-2/child_3-next"
                  "/root/-"))
    (should (niwa-agent-path-p path)))
  (dolist (path '(nil "" "root" "/root/" "/root/Upper"
                  "/root/../peer" "/root/alpha//child"
                  "/root/." "/root/two words" "/root/task.name"
                  "/root/task\\name" "/root/caf\u00e9"
                  "default--opaque"))
    (should-not (niwa-agent-path-p path))))


;;
;;; Task invariants

(niwa-deftest niwa-task-normalize-owner ()
  ,test
  (test)

  :doc "accepts registered canonical paths and ordinary buckets"
  (let ((registry '(("/root/worker" . retained))))
    (should
     (equal "/root/worker"
            (niwa-task-normalize-owner "/root/worker" registry)))
    (should (equal "backend"
                   (niwa-task-normalize-owner "backend" registry)))
    (should-not (niwa-task-normalize-owner "" registry))
    (should-not (niwa-task-normalize-owner "/root" registry)))

  :doc "rejects opaque IDs, malformed paths, and unknown agent paths"
  (let ((registry '(("/root/worker" . retained))))
    (should-error
     (niwa-task-normalize-owner
      "explorer--0123456789abcdef0123456789abcdef" registry))
    (should-error
     (niwa-task-normalize-owner "/root/Upper" registry))
    (should-error
     (niwa-task-normalize-owner "/root/ghost" registry)))

  :doc "rejects non-string owner values"
  (should-error (niwa-task-normalize-owner 42 nil)))

(niwa-deftest niwa-task-prune-resolved-dependencies ()
  ,test
  (test)
  :doc "removes missing IDs from task dependencies"
  (let* ((first
          (niwa-task--create
           :id 1 :blocked-by '(2 3)))
         (third
          (niwa-task--create
           :id 3 :blocked-by '(1 2)))
         (tasks (list first third)))
    (should (eq tasks (niwa-task-prune-resolved-dependencies tasks)))
    (should (equal '(3) (niwa-task-blocked-by first)))
    (should (equal '(1) (niwa-task-blocked-by third))))

  :doc "removes completed blockers, which nothing else would ever clear"
  (let* ((done
          (niwa-task--create
           :id 1 :status 'completed))
         (running
          (niwa-task--create
           :id 2 :status 'in-progress))
         (waiting
          (niwa-task--create
           :id 3 :status 'pending :blocked-by '(1 2)))
         (tasks (list done running waiting)))
    (niwa-task-prune-resolved-dependencies tasks)
    (should (equal '(2) (niwa-task-blocked-by waiting)))))


;;
;;; Workspace struct

(niwa-deftest niwa-workspace--create
  (:doc "`niwa-workspace--create' creates workspace with all slots")
  (let ((ws (niwa-workspace--create
             :type 'project
             :id "/tmp/test-project/"
             :root "/tmp/test-project/"
             :name "test-project")))
    (should (eq 'project (niwa-workspace-type ws)))
    (should (equal "/tmp/test-project/" (niwa-workspace-id ws)))
    (should (equal "/tmp/test-project/" (niwa-workspace-root ws)))
    (should (equal "test-project" (niwa-workspace-name ws)))
    (should (null (niwa-workspace-file-cache ws)))))


;;
;;; Session struct

(niwa-deftest niwa-session-create
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "creates session with correct defaults"
  (let ((saved-permission-mode
         (default-toplevel-value 'niwa-permission-mode))
        (saved-sandbox-mode
         (default-toplevel-value 'niwa-sandbox-mode)))
    (unwind-protect
        (progn
          (set-default-toplevel-value 'niwa-permission-mode 'ask)
          (set-default-toplevel-value 'niwa-sandbox-mode 'best-effort)
          (let* ((ws (niwa-workspace-get-or-create
                      'project "/tmp/p1/" "/tmp/p1/" "p1"))
                 (session (niwa-session-create "main" ws)))
            (should (equal "main" (niwa-session-name session)))
            (should (eq ws (niwa-session-workspace session)))
            (should (hash-table-p (niwa-session-touched-files session)))
            (should (= 0 (niwa-session-turn-count session)))
            (should (null (niwa-session-agent-registry session)))
            (should (null (niwa-session-agent-reservations session)))
            (should (eq 'idle (niwa-session-agent-root-activity session)))
            (should (= 10 (niwa-session-agent-turn-capacity session)))
            (should (null (niwa-session-tasks session)))
            (should (null (niwa-session-reminders session)))
            (should (eq 'ask (niwa-session-permission-mode session)))
            (should (eq 'best-effort
                        (niwa-session-sandbox-mode session)))
            (should-not
             (niwa-execution-target-remote-p
              (niwa-session-execution-target session)))))
      (set-default-toplevel-value
       'niwa-permission-mode saved-permission-mode)
      (set-default-toplevel-value 'niwa-sandbox-mode saved-sandbox-mode)))

  :doc "binds one immutable target and qualifies its working directory"
  (let* ((ws (niwa-workspace--create
              :type 'project
              :id "/ssh:user@host:/srv/project/"
              :root "/ssh:user@host:/srv/project/"
              :name "remote"))
         (session (niwa-session-create
                   "main" ws "/srv/project/lib/"))
         (target (niwa-session-execution-target session)))
    (should (equal "/ssh:user@host:/srv/project/lib/"
                   (niwa-session-working-directory session)))
    (should (equal "/ssh:user@host:"
                   (niwa-execution-target-prefix target)))
    (should-error
     (setf (niwa-session-execution-target session)
           (niwa-execution-target-create "/tmp/"))))

  :doc "rejects a working directory on another target"
  (let ((ws (niwa-workspace--create
             :type 'project
             :id "/ssh:user@host:/srv/project/"
             :root "/ssh:user@host:/srv/project/"
             :name "remote")))
    (should-error
     (niwa-session-create
      "main" ws "/ssh:user@other:/srv/project/")
     :type 'niwa-execution-target-error))

  :doc "two sessions share same workspace by reference"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
         (s1 (niwa-session-create "main" ws))
         (s2 (niwa-session-create "refactor" ws)))
    (should (eq (niwa-session-workspace s1)
                (niwa-session-workspace s2)))
    (setf (niwa-session-permission-rules s1)
          '(("Bash" :pattern "npx test*" :action allow))
          (niwa-session-resource-grants s1)
          '((:path "/tmp/input" :access read)))
    (should-not (niwa-session-permission-rules s2))
    (should-not (niwa-session-resource-grants s2)))

  :doc "snapshots the global sandbox default independently per session"
  (let ((saved-mode (default-toplevel-value 'niwa-sandbox-mode))
        (ws (niwa-workspace-get-or-create
             'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (unwind-protect
        (progn
          (set-default-toplevel-value 'niwa-sandbox-mode 'required)
          (let ((required (niwa-session-create "required" ws)))
            (set-default-toplevel-value 'niwa-sandbox-mode 'best-effort)
            (let ((best-effort (niwa-session-create "best-effort" ws)))
              (should (eq 'required
                          (niwa-session-sandbox-mode required)))
              (should (eq 'best-effort
                          (niwa-session-sandbox-mode best-effort)))
              (setf (niwa-session-sandbox-mode required) 'off)
              (should (eq 'best-effort
                          (niwa-session-sandbox-mode best-effort))))))
      (set-default-toplevel-value 'niwa-sandbox-mode saved-mode)))

  :doc "snapshots the global permission default independently per session"
  (let ((saved-mode (default-toplevel-value 'niwa-permission-mode))
        (ws (niwa-workspace-get-or-create
             'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (unwind-protect
        (progn
          (set-default-toplevel-value 'niwa-permission-mode 'edits)
          (let ((edits (niwa-session-create "edits" ws)))
            (set-default-toplevel-value 'niwa-permission-mode 'ask)
            (let ((ask (niwa-session-create "ask" ws)))
              (should (eq 'edits
                          (niwa-session-permission-mode edits)))
              (should (eq 'ask
                          (niwa-session-permission-mode ask)))
              (setf (niwa-session-permission-mode edits) 'full-auto)
              (should (eq 'ask
                          (niwa-session-permission-mode ask))))))
      (set-default-toplevel-value 'niwa-permission-mode saved-mode))))

(niwa-deftest niwa-session--set-agent-registry ()
  ,test
  (test)
  :doc "stores the session-owned retained agent registry by identity"
  (let* ((session (niwa-session--create))
         (registry (list (cons "/root/worker" (make-symbol "record")))))
    (niwa-session--set-agent-registry session registry)
    (should (eq registry (niwa-session-agent-registry session)))))

(niwa-deftest niwa-session--set-agent-reservations ()
  ,test
  (test)
  :doc "stores transient unpublished agent reservations by identity"
  (let* ((session (niwa-session--create))
         (reservations (list (cons "/root/worker" (make-symbol "record")))))
    (niwa-session--set-agent-reservations session reservations)
    (should (eq reservations
                (niwa-session-agent-reservations session)))))

(niwa-deftest niwa-session--set-execution-state ()
  ,test
  (test)
  :doc "stores opaque transient execution state by identity"
  (let* ((session (niwa-session--create))
         (state (make-symbol "execution-state")))
    (niwa-session--set-execution-state session state)
    (should (eq state (niwa-session-execution-state session)))))

(niwa-deftest niwa-session--set-dropped-file-grants ()
  ,test
  (test)
  :doc "replaces pending dropped-file grants"
  (let ((session (niwa-session--create))
        (paths '("/tmp/one" "/tmp/two")))
    (niwa-session--set-dropped-file-grants session paths)
    (should (eq paths (niwa-session-dropped-file-grants session)))))

(niwa-deftest niwa-session--set-active-dropped-file-grants ()
  ,test
  (test)
  :doc "replaces active dropped-file grants"
  (let ((session (niwa-session--create))
        (paths '("/tmp/one" "/tmp/two")))
    (niwa-session--set-active-dropped-file-grants session paths)
    (should (eq paths
                (niwa-session-active-dropped-file-grants session)))))


;;
;;; Session buffer name

(niwa-deftest niwa-session-buffer-name
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry)
   :doc "`niwa-session-buffer-name' formats correctly")
  (should (equal ,expected (niwa-session-buffer-name
                            ,session-name
                            (niwa-workspace--create :name ,ws-name))))
  (session-name ws-name expected)
  "main"     "myproject" "*niwa:main@myproject*"
  "refactor" "myproject" "*niwa:refactor@myproject*"
  "review"   "myproject" "*niwa:review@myproject*")

;;
;;; Request timing

(niwa-deftest niwa-request-set-active-work-paused ()
  ,test
  (test)
  :doc "starts idempotently and accumulates completed active-work pauses"
  (let* ((start (seconds-to-time 100))
         (request (niwa-request--create :started-at start)))
    (niwa-request-set-active-work-paused request t (seconds-to-time 110))
    (niwa-request-set-active-work-paused request t (seconds-to-time 115))
    (should (equal (seconds-to-time 110)
                   (niwa-request-active-work-pause-started-at request)))
    (niwa-request-set-active-work-paused request nil (seconds-to-time 125))
    (should-not (niwa-request-active-work-pause-started-at request))
    (should (= 15 (niwa-request-active-work-pause-duration request)))))

(niwa-deftest niwa-request-active-elapsed-seconds ()
  ,test
  (test)
  :doc "excludes completed and current pauses from active work"
  (let ((request
         (niwa-request--create
          :started-at (seconds-to-time 100)
          :active-work-pause-started-at (seconds-to-time 125)
          :active-work-pause-duration 5)))
    (should (= 20
               (niwa-request-active-elapsed-seconds
                request (seconds-to-time 130))))))

(niwa-deftest niwa-session-activate-dropped-file-grants ()
  ,test
  (test)
  :doc "adds exact session-scoped Read grants without duplicates"
  (let* ((ws (niwa-workspace--create
              :type 'project :id "drop" :root "/tmp/drop/"
              :name "drop"))
         (session (niwa-session-create "main" ws))
         (path "/tmp/drop-file.txt")
         (expanded (expand-file-name path)))
    (niwa-session-activate-dropped-file-grants session
                                                  (list path path))
    (should (equal (list expanded)
                   (niwa-session-active-dropped-file-grants session)))
    (should-not (niwa-session-permission-rules session))
    (niwa-session-activate-dropped-file-grants session (list path))
    (should (equal (list expanded)
                   (niwa-session-active-dropped-file-grants session))))

  :doc "request end keeps session-scoped grants"
  (with-temp-buffer
    (let* ((root (make-temp-file "niwa-dropped-grants-" t))
           (ws (niwa-workspace-get-or-create 'file root root "p1"))
           (session (niwa-session-create "main" ws))
           (path "/tmp/dropped.txt")
           (expanded (expand-file-name path)))
      (unwind-protect
          (progn
            (setq-local niwa--session session)
            (niwa-session-activate-dropped-file-grants session (list path))
            (niwa-request-begin session)
            (niwa-request-end)
            (should (equal (list expanded)
                           (niwa-session-active-dropped-file-grants session))))
        (niwa-session-persistence-lock-release
         (niwa-session-save-path session) session)
        (delete-directory root t)))))

(niwa-deftest niwa-session-generate-id ()
  ,test
  (test)
  :doc "generates id matching <timestamp>-<random-suffix>"
  (let ((id (niwa-session-generate-id)))
    (should (string-match-p
             "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9a-f]\\{12\\}\\'"
             id))))


(provide 'test-niwa-structs)
;;; test-niwa-structs.el ends here
