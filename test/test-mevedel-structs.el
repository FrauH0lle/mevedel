;;; test-mevedel-structs.el --- Tests for mevedel-structs.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel)
(require 'mevedel-structs)
(require 'mevedel-execution-target)
(require 'mevedel-permissions)
(require 'mevedel-permission-queue)
(require 'mevedel-plan-mode)
(require 'mevedel-goal)
(require 'mevedel-agents)
(require 'mevedel-reminders)
(require 'mevedel-sandbox)
(require 'mevedel-session-durability)
(require 'mevedel-session-publication)
(require 'mevedel-session-persistence)
(require 'mevedel-telemetry)
(require 'mevedel-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;; `mevedel-agents'
(defvar mevedel--agent-invocation)


;;
;;; Session transient state

(mevedel-deftest mevedel-session-authority-mode-for-session
  (:doc "normalizes workspace authority once and rejects unset or mismatched state")
  (let* ((workspace
          (mevedel-workspace--create
           :type 'project :id "project" :root "/tmp/project/"
           :name "project"))
         (session (mevedel-session--create :workspace workspace)))
    (should (eq 'portable
                (mevedel-session-authority-mode-for-session session)))
    (should (eq 'portable (mevedel-session-authority-mode session)))
    (setf (mevedel-session-authority-mode session) 'pid-lock)
    (should-error
     (mevedel-session-authority-mode-for-session session)
     :type 'error))
  (should-error
   (mevedel-session-authority-mode-for-session
    (mevedel-session--create))
   :type 'error))

(mevedel-deftest mevedel-session-audit-target ()
  ,test
  (test)
  :doc "defaults to the runtime session and honors a transient audit target"
  (let* ((parent (mevedel-session--create :name "parent"))
         (side (mevedel-session--create :name "side")))
    (should (eq side (mevedel-session-audit-target side)))
    (setf (mevedel-session-audit-session side) parent)
    (should (eq parent (mevedel-session-audit-target side)))))

(mevedel-deftest mevedel-session-pending-inputs ()
  ,test
  (test)
  :doc "returns each pending input category and rejects unknown categories"
  (let ((session (mevedel-session--create
                  :pending-steering '((:input "steer"))
                  :pending-follow-ups '((:input "later")))))
    (should (equal '((:input "steer"))
                   (mevedel-session-pending-inputs session 'steering)))
    (should (equal '((:input "later"))
                   (mevedel-session-pending-inputs session 'follow-up)))
    (should-error
     (mevedel-session-pending-inputs session 'unknown))))

(mevedel-deftest mevedel-session-set-pending-inputs ()
  ,test
  (test)
  :doc "replaces one pending category without changing the other"
  (let ((session (mevedel-session--create
                  :pending-steering '((:input "steer")))))
    (should (equal '((:input "later"))
                   (mevedel-session-set-pending-inputs
                    session 'follow-up '((:input "later")))))
    (should (equal '((:input "steer"))
                   (mevedel-session-pending-steering session)))
    (should-error
     (mevedel-session-set-pending-inputs session 'unknown nil))))

(mevedel-deftest mevedel-session-pending-input-p ()
  ,test
  (test)
  :doc "combines the two pending-input categories"
  (let ((session (mevedel-session--create)))
    (should-not (mevedel-session-pending-input-p session))
    (setf (mevedel-session-pending-steering session) '((:input "now")))
    (should (mevedel-session-pending-input-p session))
    (setf (mevedel-session-pending-steering session) nil
          (mevedel-session-pending-follow-ups session) '((:input "later")))
    (should (mevedel-session-pending-input-p session))))

(mevedel-deftest mevedel-session-enqueue-pending-input ()
  ,test
  (test)
  :doc "assigns stable session-local IDs and appends within each category"
  (let* ((session (mevedel-session--create))
         (first (mevedel-session-enqueue-pending-input
                 session 'follow-up '(:input "first")))
         (steering (mevedel-session-enqueue-pending-input
                    session 'steering '(:input "steer")))
         (second (mevedel-session-enqueue-pending-input
                  session 'follow-up '(:input "second"))))
    (should (equal '(1 3)
                   (mapcar (lambda (entry) (plist-get entry :id))
                           (mevedel-session-pending-follow-ups session))))
    (should (= 2 (plist-get steering :id)))
    (should (eq 'follow-up (plist-get first :category)))
    (should (eq 'follow-up (plist-get second :category)))
    (should (eq 'steering (plist-get steering :category)))))

(mevedel-deftest mevedel-session-set-pending-input-paused ()
  ,test
  (test)
  :doc "normalizes the transient delivery pause to a boolean"
  (let ((session (mevedel-session--create)))
    (should
     (mevedel-session-set-pending-input-paused session 'cockpit))
    (should (eq t (mevedel-session-pending-input-paused session)))
    (should-not
     (mevedel-session-set-pending-input-paused session nil))
    (should-not (mevedel-session-pending-input-paused session))))

(mevedel-deftest mevedel-session-set-pending-input-failure-paused ()
  ,test
  (test)
  :doc "normalizes failure recovery pause to a boolean"
  (let ((session (mevedel-session--create)))
    (should
     (mevedel-session-set-pending-input-failure-paused session 'failure))
    (should (eq t
                (mevedel-session-pending-input-failure-paused session)))
    (should-not
     (mevedel-session-set-pending-input-failure-paused session nil))))

(mevedel-deftest mevedel-session-pending-input-delivery-paused-p ()
  ,test
  (test)
  :doc "combines cockpit pause and failure pause"
  (let ((session (mevedel-session--create)))
    (should-not (mevedel-session-pending-input-delivery-paused-p session))
    (mevedel-session-set-pending-input-paused session t)
    (should (mevedel-session-pending-input-delivery-paused-p session))
    (mevedel-session-set-pending-input-paused session nil)
    (mevedel-session-set-pending-input-failure-paused session t)
    (should (mevedel-session-pending-input-delivery-paused-p session))))

(mevedel-deftest mevedel-session-set-hook-context-pending ()
  ,test
  (test)
  :doc "replaces the session's pending hook context entries"
  (let* ((session (mevedel-session--create))
         (entries '((:event SessionStart :body "context"))))
    (should (equal entries
                   (mevedel-session-set-hook-context-pending
                    session entries)))
    (should (equal entries
                   (mevedel-session-hook-context-pending session)))))


;;
;;; Agent tree identity

(mevedel-deftest mevedel-agent-path-p ()
  ,test
  (test)
  :doc "accepts root and lowercase ASCII descendant segments only"
  (dolist (path '("/root" "/root/alpha" "/root/alpha/child_2"))
    (should (mevedel-agent-path-p path)))
  (dolist (path '(nil "" "root" "/root/" "/root/Upper"
                  "/root/../peer" "/root/alpha//child"
                  "default--opaque"))
    (should-not (mevedel-agent-path-p path))))


;;
;;; Task invariants

(mevedel-deftest mevedel-task-normalize-owner ()
  ,test
  (test)

  :doc "accepts registered canonical paths and ordinary buckets"
  (let ((registry '(("/root/worker" . retained))))
    (should
     (equal "/root/worker"
            (mevedel-task-normalize-owner "/root/worker" registry)))
    (should (equal "backend"
                   (mevedel-task-normalize-owner "backend" registry)))
    (should-not (mevedel-task-normalize-owner "" registry))
    (should-not (mevedel-task-normalize-owner "/root" registry)))

  :doc "rejects opaque IDs, malformed paths, and unknown agent paths"
  (let ((registry '(("/root/worker" . retained))))
    (should-error
     (mevedel-task-normalize-owner
      "explorer--0123456789abcdef0123456789abcdef" registry))
    (should-error
     (mevedel-task-normalize-owner "/root/Upper" registry))
    (should-error
     (mevedel-task-normalize-owner "/root/ghost" registry)))

  :doc "rejects non-string owner values"
  (should-error (mevedel-task-normalize-owner 42 nil)))

(mevedel-deftest mevedel-task-prune-dangling-dependencies ()
  ,test
  (test)
  :doc "removes missing IDs from both task dependency directions"
  (let* ((first
          (mevedel-task--create
           :id 1 :blocks '(2 3) :blocked-by '(2 3)))
         (third
          (mevedel-task--create
           :id 3 :blocks '(1 2) :blocked-by '(1 2)))
         (tasks (list first third)))
    (should (eq tasks (mevedel-task-prune-dangling-dependencies tasks)))
    (should (equal '(3) (mevedel-task-blocks first)))
    (should (equal '(3) (mevedel-task-blocked-by first)))
    (should (equal '(1) (mevedel-task-blocks third)))
    (should (equal '(1) (mevedel-task-blocked-by third)))))


;;
;;; Workspace struct

(mevedel-deftest mevedel-workspace--create
  (:doc "`mevedel-workspace--create' creates workspace with all slots")
  (let ((ws (mevedel-workspace--create
             :type 'project
             :id "/tmp/test-project/"
             :root "/tmp/test-project/"
             :name "test-project")))
    (should (eq 'project (mevedel-workspace-type ws)))
    (should (equal "/tmp/test-project/" (mevedel-workspace-id ws)))
    (should (equal "/tmp/test-project/" (mevedel-workspace-root ws)))
    (should (equal "test-project" (mevedel-workspace-name ws)))
    (should (null (mevedel-workspace-file-cache ws)))))


;;
;;; Session struct

(mevedel-deftest mevedel-session-create
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "creates session with correct defaults"
  (let ((saved-permission-mode
         (default-toplevel-value 'mevedel-permission-mode))
        (saved-sandbox-mode
         (default-toplevel-value 'mevedel-sandbox-mode)))
    (unwind-protect
        (progn
          (set-default-toplevel-value 'mevedel-permission-mode 'ask)
          (set-default-toplevel-value 'mevedel-sandbox-mode 'best-effort)
          (let* ((ws (mevedel-workspace-get-or-create
                      'project "/tmp/p1/" "/tmp/p1/" "p1"))
                 (session (mevedel-session-create "main" ws)))
            (should (equal "main" (mevedel-session-name session)))
            (should (eq ws (mevedel-session-workspace session)))
            (should (hash-table-p (mevedel-session-touched-files session)))
            (should (= 0 (mevedel-session-turn-count session)))
            (should (null (mevedel-session-agent-registry session)))
            (should (null (mevedel-session-agent-reservations session)))
            (should (eq 'idle (mevedel-session-agent-root-activity session)))
            (should (= 3 (mevedel-session-agent-turn-capacity session)))
            (should (null (mevedel-session-tasks session)))
            (should (null (mevedel-session-reminders session)))
            (should (null (mevedel-session-deferred-pending session)))
            (should (null (mevedel-session-deferred-injected session)))
            (should (eq 'ask (mevedel-session-permission-mode session)))
            (should (eq 'best-effort
                        (mevedel-session-sandbox-mode session)))
            (should-not
             (mevedel-execution-target-remote-p
              (mevedel-session-execution-target session)))))
      (set-default-toplevel-value
       'mevedel-permission-mode saved-permission-mode)
      (set-default-toplevel-value 'mevedel-sandbox-mode saved-sandbox-mode)))

  :doc "binds one immutable target and qualifies its working directory"
  (let* ((ws (mevedel-workspace--create
              :type 'project
              :id "/ssh:user@host:/srv/project/"
              :root "/ssh:user@host:/srv/project/"
              :name "remote"))
         (session (mevedel-session-create
                   "main" ws "/srv/project/lib/"))
         (target (mevedel-session-execution-target session)))
    (should (equal "/ssh:user@host:/srv/project/lib/"
                   (mevedel-session-working-directory session)))
    (should (equal "/ssh:user@host:"
                   (mevedel-execution-target-prefix target)))
    (should-error
     (setf (mevedel-session-execution-target session)
           (mevedel-execution-target-create "/tmp/"))))

  :doc "rejects a working directory on another target"
  (let ((ws (mevedel-workspace--create
             :type 'project
             :id "/ssh:user@host:/srv/project/"
             :root "/ssh:user@host:/srv/project/"
             :name "remote")))
    (should-error
     (mevedel-session-create
      "main" ws "/ssh:user@other:/srv/project/")
     :type 'mevedel-execution-target-error))

  :doc "two sessions share same workspace by reference"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
         (s1 (mevedel-session-create "main" ws))
         (s2 (mevedel-session-create "refactor" ws)))
    (should (eq (mevedel-session-workspace s1)
                (mevedel-session-workspace s2)))
    (setf (mevedel-session-permission-rules s1)
          '(("Bash" :pattern "npx test*" :action allow))
          (mevedel-session-resource-grants s1)
          '((:path "/tmp/input" :access read)))
    (should-not (mevedel-session-permission-rules s2))
    (should-not (mevedel-session-resource-grants s2)))

  :doc "snapshots the global sandbox default independently per session"
  (let ((saved-mode (default-toplevel-value 'mevedel-sandbox-mode))
        (ws (mevedel-workspace-get-or-create
             'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (unwind-protect
        (progn
          (set-default-toplevel-value 'mevedel-sandbox-mode 'required)
          (let ((required (mevedel-session-create "required" ws)))
            (set-default-toplevel-value 'mevedel-sandbox-mode 'best-effort)
            (let ((best-effort (mevedel-session-create "best-effort" ws)))
              (should (eq 'required
                          (mevedel-session-sandbox-mode required)))
              (should (eq 'best-effort
                          (mevedel-session-sandbox-mode best-effort)))
              (setf (mevedel-session-sandbox-mode required) 'off)
              (should (eq 'best-effort
                          (mevedel-session-sandbox-mode best-effort))))))
      (set-default-toplevel-value 'mevedel-sandbox-mode saved-mode)))

  :doc "snapshots the global permission default independently per session"
  (let ((saved-mode (default-toplevel-value 'mevedel-permission-mode))
        (ws (mevedel-workspace-get-or-create
             'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (unwind-protect
        (progn
          (set-default-toplevel-value 'mevedel-permission-mode 'edits)
          (let ((edits (mevedel-session-create "edits" ws)))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (let ((ask (mevedel-session-create "ask" ws)))
              (should (eq 'edits
                          (mevedel-session-permission-mode edits)))
              (should (eq 'ask
                          (mevedel-session-permission-mode ask)))
              (setf (mevedel-session-permission-mode edits) 'full-auto)
              (should (eq 'ask
                          (mevedel-session-permission-mode ask))))))
      (set-default-toplevel-value 'mevedel-permission-mode saved-mode))))

(mevedel-deftest mevedel-session--set-agent-registry ()
  ,test
  (test)
  :doc "stores the session-owned retained agent registry by identity"
  (let* ((session (mevedel-session--create))
         (registry (list (cons "/root/worker" (make-symbol "record")))))
    (mevedel-session--set-agent-registry session registry)
    (should (eq registry (mevedel-session-agent-registry session)))))

(mevedel-deftest mevedel-session--set-agent-reservations ()
  ,test
  (test)
  :doc "stores transient unpublished agent reservations by identity"
  (let* ((session (mevedel-session--create))
         (reservations (list (cons "/root/worker" (make-symbol "record")))))
    (mevedel-session--set-agent-reservations session reservations)
    (should (eq reservations
                (mevedel-session-agent-reservations session)))))

(mevedel-deftest mevedel-session--set-execution-state ()
  ,test
  (test)
  :doc "stores opaque transient execution state by identity"
  (let* ((session (mevedel-session--create))
         (state (make-symbol "execution-state")))
    (mevedel-session--set-execution-state session state)
    (should (eq state (mevedel-session-execution-state session)))))

(mevedel-deftest mevedel-session--set-dropped-file-grants ()
  ,test
  (test)
  :doc "replaces pending dropped-file grants"
  (let ((session (mevedel-session--create))
        (paths '("/tmp/one" "/tmp/two")))
    (mevedel-session--set-dropped-file-grants session paths)
    (should (eq paths (mevedel-session-dropped-file-grants session)))))

(mevedel-deftest mevedel-session--set-active-dropped-file-grants ()
  ,test
  (test)
  :doc "replaces active dropped-file grants"
  (let ((session (mevedel-session--create))
        (paths '("/tmp/one" "/tmp/two")))
    (mevedel-session--set-active-dropped-file-grants session paths)
    (should (eq paths
                (mevedel-session-active-dropped-file-grants session)))))


;;
;;; Session buffer name

(mevedel-deftest mevedel-session-buffer-name
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry)
   :doc "`mevedel-session-buffer-name' formats correctly")
  (should (equal ,expected (mevedel-session-buffer-name
                            ,session-name
                            (mevedel-workspace--create :name ,ws-name))))
  (session-name ws-name expected)
  "main"     "myproject" "*mevedel:main@myproject*"
  "refactor" "myproject" "*mevedel:refactor@myproject*"
  "review"   "myproject" "*mevedel:review@myproject*")

;;
;;; Request timing

(mevedel-deftest mevedel-request-set-active-work-paused ()
  ,test
  (test)
  :doc "starts idempotently and accumulates completed active-work pauses"
  (let* ((start (seconds-to-time 100))
         (request (mevedel-request--create :started-at start)))
    (mevedel-request-set-active-work-paused request t (seconds-to-time 110))
    (mevedel-request-set-active-work-paused request t (seconds-to-time 115))
    (should (equal (seconds-to-time 110)
                   (mevedel-request-active-work-pause-started-at request)))
    (mevedel-request-set-active-work-paused request nil (seconds-to-time 125))
    (should-not (mevedel-request-active-work-pause-started-at request))
    (should (= 15 (mevedel-request-active-work-pause-duration request)))))

(mevedel-deftest mevedel-request-active-elapsed-seconds ()
  ,test
  (test)
  :doc "excludes completed and current pauses from active work"
  (let ((request
         (mevedel-request--create
          :started-at (seconds-to-time 100)
          :active-work-pause-started-at (seconds-to-time 125)
          :active-work-pause-duration 5)))
    (should (= 20
               (mevedel-request-active-elapsed-seconds
                request (seconds-to-time 130))))))

(mevedel-deftest mevedel-session-activate-dropped-file-grants ()
  ,test
  (test)
  :doc "adds exact session-scoped Read grants without duplicates"
  (let* ((ws (mevedel-workspace--create
              :type 'project :id "drop" :root "/tmp/drop/"
              :name "drop"))
         (session (mevedel-session-create "main" ws))
         (path "/tmp/drop-file.txt")
         (expanded (expand-file-name path)))
    (mevedel-session-activate-dropped-file-grants session
                                                  (list path path))
    (should (equal (list expanded)
                   (mevedel-session-active-dropped-file-grants session)))
    (should-not (mevedel-session-permission-rules session))
    (mevedel-session-activate-dropped-file-grants session (list path))
    (should (equal (list expanded)
                   (mevedel-session-active-dropped-file-grants session))))

  :doc "request end keeps session-scoped grants"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (path "/tmp/dropped.txt")
           (expanded (expand-file-name path)))
      (setq-local mevedel--session session)
      (mevedel-session-activate-dropped-file-grants session (list path))
      (mevedel-request-begin session)
      (mevedel-request-end)
      (should (equal (list expanded)
                     (mevedel-session-active-dropped-file-grants session))))))

(provide 'test-mevedel-structs)
;;; test-mevedel-structs.el ends here
