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

(mevedel-deftest mevedel-request-note-untracked-effect
  (:doc "deduplicates capture gaps by their mutation source")
  (let ((request (mevedel-request--create)))
    (mevedel-request-note-untracked-effect request "Bash" "first")
    (mevedel-request-note-untracked-effect request "Bash" "second")
    (mevedel-request-note-untracked-effect request "Eval" "third")
    (should (equal '(("Eval" . "third") ("Bash" . "first"))
                   (mevedel-request-untracked-effects request)))))

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
;;; Workspace registry

(mevedel-deftest mevedel-workspace-get-or-create
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "creates workspace on first call"
  (let ((ws (mevedel-workspace-get-or-create
             'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (should (mevedel-workspace-p ws))
    (should (eq 'project (mevedel-workspace-type ws)))
    (should (equal "p1" (mevedel-workspace-name ws)))
    (should (mevedel-file-cache-p (mevedel-workspace-file-cache ws))))

  :doc "returns same struct on second call"
  (let ((ws1 (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
        (ws2 (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1-renamed")))
    (should (eq ws1 ws2))
    (should (equal "p1" (mevedel-workspace-name ws2))))

  :doc "different IDs create different workspaces"
  (let ((ws1 (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
        (ws2 (mevedel-workspace-get-or-create
              'project "/tmp/p2/" "/tmp/p2/" "p2")))
    (should-not (eq ws1 ws2))
    (should (equal "p1" (mevedel-workspace-name ws1)))
    (should (equal "p2" (mevedel-workspace-name ws2))))

  :doc "different types with same ID create different workspaces"
  (let ((ws1 (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1-project"))
        (ws2 (mevedel-workspace-get-or-create
              'file "/tmp/p1/" "/tmp/p1/" "p1-file")))
    (should-not (eq ws1 ws2)))

  :doc "normalizes tilde project roots"
  (let* ((root "~/mevedel-workspace-root/")
         (expected (expand-file-name root))
         (ws (mevedel-workspace-get-or-create
              'project root root "home-root")))
    (should (equal expected (mevedel-workspace-id ws)))
    (should (equal expected (mevedel-workspace-root ws))))

  :doc "deduplicates project root aliases after expansion"
  (let* ((root "~/mevedel-workspace-root/")
         (expanded (expand-file-name root))
         (ws1 (mevedel-workspace-get-or-create
               'project root root "home-root"))
         (ws2 (mevedel-workspace-get-or-create
               'project expanded expanded "expanded-root")))
    (should (eq ws1 ws2)))

  :doc "keeps non-project identifiers opaque"
  (let* ((root "~/mevedel-file-root/")
         (ws (mevedel-workspace-get-or-create
              'file "relative-id" root "file-root")))
    (should (equal "relative-id" (mevedel-workspace-id ws)))
    (should (equal (expand-file-name root)
                   (mevedel-workspace-root ws)))))

(mevedel-deftest mevedel-workspace-clear-registry
  (:doc "`mevedel-workspace-clear-registry' removes all entries")
  (let ((ws (mevedel-workspace-get-or-create 'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (mevedel-workspace-clear-registry)
    (should-not
     (eq ws (mevedel-workspace-get-or-create 'project "/tmp/p1/" "/tmp/p1/" "p1")))))


;;
;;; Workspace helpers

(mevedel-deftest mevedel-workspace-state-dir
  (:doc "`mevedel-workspace-state-dir' returns .mevedel/ under root")
  ,test
  (test)
  :doc "returns .mevedel under root"
  (let ((ws (mevedel-workspace--create :root "/tmp/project/")))
    (should (equal (file-name-concat (expand-file-name "/tmp/project/")
                                     ".mevedel/")
                   (mevedel-workspace-state-dir ws))))

  :doc "expands tilde roots"
  (let* ((root "~/mevedel-test-root/")
         (ws (mevedel-workspace--create :root root)))
    (should (equal (file-name-concat (expand-file-name root) ".mevedel/")
                   (mevedel-workspace-state-dir ws)))))

(mevedel-deftest mevedel-workspace-find-state-file
  (:doc "`mevedel-workspace-find-state-file' checks project then global")
  ,test
  (test)
  :doc "returns project path when project file exists"
  (let* ((dir (make-temp-file "mevedel-test-" t))
         (mevedel-dir (file-name-concat dir ".mevedel/"))
         (ws (mevedel-workspace--create :root (file-name-as-directory dir))))
    (unwind-protect
        (progn
          (make-directory mevedel-dir t)
          (write-region "" nil (file-name-concat mevedel-dir "config.el"))
          (should (equal (file-name-concat mevedel-dir "config.el")
                         (mevedel-workspace-find-state-file ws "config.el"))))
      (delete-directory dir t)))

  :doc "falls back to global path when project file missing"
  (let* ((dir (make-temp-file "mevedel-test-" t))
         (global-dir (make-temp-file "mevedel-global-" t))
         (mevedel-user-dir (file-name-as-directory global-dir))
         (ws (mevedel-workspace--create :root (file-name-as-directory dir))))
    (unwind-protect
        (progn
          (write-region "" nil (file-name-concat global-dir "config.el"))
          (should (equal (file-name-concat global-dir "config.el")
                         (mevedel-workspace-find-state-file ws "config.el"))))
      (delete-directory dir t)
      (delete-directory global-dir t)))

  :doc "returns project path when neither exists"
  (let* ((ws (mevedel-workspace--create :root "/tmp/nonexistent-project/"))
         (result (mevedel-workspace-find-state-file ws "config.el")))
    (should (equal (file-name-concat
                    (expand-file-name "/tmp/nonexistent-project/")
                    ".mevedel/config.el")
                   result))))


;;
;;; Session struct

(mevedel-deftest mevedel-session-create
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "creates session with correct defaults"
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
    (should (eq 'best-effort (mevedel-session-sandbox-mode session)))
    (should-not
     (mevedel-execution-target-remote-p
      (mevedel-session-execution-target session))))

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
  "tutor"    "myproject" "*mevedel:tutor@myproject*")


;;
;;; Request lifecycle

(mevedel-deftest mevedel-current-origin ()
  ,test
  (test)
  :doc "prefers request ownership, then agent path, then root"
  (let ((mevedel--current-request
         (mevedel-request--create :origin "/root/request"))
        (mevedel--agent-invocation
         (mevedel-agent-invocation--create :agent-id "agent-owner" :path "/root/agent_owner")))
    (should (equal "/root/request" (mevedel-current-origin))))
  (with-temp-buffer
    (let ((mevedel--current-request nil))
      (setq-local
       mevedel--agent-invocation
       (mevedel-agent-invocation--create
        :agent-id "agent-owner" :path "/root/agent_owner"))
      (should (equal "/root/agent_owner" (mevedel-current-origin)))))
  (with-temp-buffer
    (let ((mevedel--current-request nil))
      (should (equal "/root" (mevedel-current-origin))))))

(mevedel-deftest mevedel-current-turn ()
  ,test
  (test)
  :doc "uses the active request reservation and otherwise the next turn"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :turn-count 4))
           (mevedel--current-request nil))
      (should (= 5 (mevedel-current-turn session)))
      (setq mevedel--current-request
            (mevedel-request--create :session session :turn 7))
      (should (= 7 (mevedel-current-turn session))))))

(mevedel-deftest mevedel-request-active-p ()
  ,test
  (test)
  :doc "reports whether a buffer has an active request"
  (with-temp-buffer
    (should-not (mevedel-request-active-p))
    (setq-local mevedel--current-request t)
    (should (mevedel-request-active-p))))

(mevedel-deftest mevedel-request-state-label ()
  ,test
  (test)
  :doc "reports idle without an active request and running with one"
  (with-temp-buffer
    (should (equal "idle" (mevedel-request-state-label)))
    (setq-local mevedel--current-request t)
    (should (equal "running" (mevedel-request-state-label)))))

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

(mevedel-deftest mevedel-request-begin
  (:before-each (mevedel-workspace-clear-registry)
   :after-each
   (mevedel-workspace-clear-registry)
   (setq mevedel--current-request nil))
  ,test
  (test)
  :doc "creates request and sets buffer-local"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (req (mevedel-request-begin session)))
      (should (mevedel-request-p req))
      (should (eq req mevedel--current-request))
      (should (eq session (mevedel-request-session req)))
      (should (eq 'running
                  (mevedel-session-agent-root-activity session)))
      (should (hash-table-p (mevedel-request-file-snapshots req)))
      (should (null (mevedel-request-directive-uuid req)))))

  :doc "reserves the next turn without committing it"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws)))
      (setf (mevedel-session-turn-count session) 4)
      (let ((req (mevedel-request-begin session)))
        (should (= 5 (mevedel-request-turn req)))
        (should (= 4 (mevedel-session-turn-count session))))))

  :doc "sets directive-uuid when provided"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (req (mevedel-request-begin session "test-uuid")))
      (should (equal "test-uuid" (mevedel-request-directive-uuid req)))))

  :doc "stamps directive planning authority onto the request"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws)))
      (setf (mevedel-session-directive-planning session)
            '(:directive-id "d1" :phase planning))
      (should (mevedel-request-plan-read-only
               (mevedel-request-begin session)))))

  :doc "records agent origin when request begins in a sub-agent buffer"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (agent (mevedel-agent--create :name "verifier"))
           (inv (mevedel-agent-invocation-create agent)))
      (setf (mevedel-agent-invocation-agent-id inv) "verifier--abc")
      (setf (mevedel-agent-invocation-path inv) "/root/verifier")
      (setq-local mevedel--agent-invocation inv)
      (let ((req (mevedel-request-begin session)))
        (should (equal "/root/verifier"
                       (mevedel-request-origin req)))
        (should (eq 'idle
                    (mevedel-session-agent-root-activity session))))))

  :doc "replaces stale request with warning"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (req1 (mevedel-request-begin session))
           diagnostics
           (req2 (mevedel-test--with-captured-messages diagnostics
                   (mevedel-request-begin session))))
      (should (string-match-p "stale request found" diagnostics))
      (should (eq req2 mevedel--current-request))
      (should-not (eq req1 req2))))

  :doc "replacing stale request drains queued interactions"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (outcomes nil)
           (request (mevedel-request-begin session)))
      (setf (mevedel-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :origin "/root"
                        :request-id (mevedel-request-id request)
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'permission outcome) outcomes)))))
      (setf (mevedel-session-pending-plan-approval session)
            (list :body "# Plan"
                        :chat-buffer (current-buffer)
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'plan outcome) outcomes))))
      (mevedel-test--with-captured-messages nil
        (mevedel-request-begin session))
      (should (null (mevedel-session-permission-queue session)))
      (should (null (mevedel-session-pending-plan-approval session)))
      (should (equal '((plan . aborted) (permission . aborted))
                     outcomes))))

  :doc "probes an unprobed remote target before admitting the request"
  (with-temp-buffer
    (let* ((target (mevedel-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (workspace (mevedel-workspace--create
                       :type 'project :id "remote"
                       :root "/ssh:user@host:/srv/project/" :name "remote"))
           (session (mevedel-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/ssh:user@host:/srv/project/"))
           (probe-count 0))
      (cl-letf (((symbol-function 'mevedel-execution-target-probe)
                 (lambda (probed &optional _refresh _sandbox-mode)
                   (cl-incf probe-count)
                   (setf (mevedel-execution-target-readiness probed)
                         '(:status ready))))
                ((symbol-function
                  'mevedel-session-persistence-assert-mutation-authority)
                 (lambda (&rest _) t)))
        (should (mevedel-request-p (mevedel-request-begin session))))
      (should (= 1 probe-count))))

  :doc "rechecks a cached remote target and revokes grants after replacement"
  (with-temp-buffer
    (let* ((target (mevedel-execution-target-create
                    "/docker:dev:/workspace/"))
           (workspace (mevedel-workspace--create
                       :type 'project :id "remote"
                       :root "/docker:dev:/workspace/" :name "remote"))
           (session (mevedel-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/docker:dev:/workspace/"))
           calls)
      (setf (mevedel-execution-target-readiness target) '(:status ready)
            (mevedel-execution-target-incarnation target) "old-incarnation"
            (mevedel-execution-target-observed-incarnation target)
            "new-incarnation"
            (mevedel-execution-target-incarnation-changed-p target) t)
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-execution-target-probe)
                 (lambda (&rest _args)
                   (push 'probe calls)
                   '(:status ready)))
                ((symbol-function
                  'mevedel-session-persistence-assert-mutation-authority)
                 (lambda (checked &optional buffer)
                   (push 'lease calls)
                   (mevedel-session-persistence--check-target-incarnation
                    checked (or buffer (current-buffer)))
                   t))
                ((symbol-function
                  'mevedel-permission-invalidate-target-grants)
                 (lambda (_session) (push 'invalidate calls) t))
                ((symbol-function
                  'mevedel-session-persistence-publish-sidecar-state)
                 (lambda (_session _root-buffer) (push 'publish calls) t)))
        (should (mevedel-request-p (mevedel-request-begin session))))
      (should (equal '(probe lease invalidate)
                     (nreverse calls)))
      (should (equal "new-incarnation"
                     (mevedel-execution-target-incarnation target)))
      (should-not
       (mevedel-execution-target-observed-incarnation target))
      (should-not
       (mevedel-execution-target-incarnation-changed-p target))))

  :doc "commits a replacement incarnation only with revoked exact grants"
  (let* ((local-root
          (file-name-as-directory
           (make-temp-file "mevedel-incarnation-ack-" t)))
         (host "incarnation-ack")
         (remote-root
          (format "/mevedelmock:%s:%s" host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?9))
         (mevedel-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         session
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let* ((workspace
                  (mevedel-workspace-get-or-create
                   'project remote-root remote-root "incarnation-ack"))
                 (_identity
                  (mevedel-workspace-identity-ensure remote-root))
                 (grant-path (file-name-concat remote-root "granted.el"))
                 (session-id "main-incarnation-ack")
                 (session-dir
                  (file-name-as-directory
                   (file-name-concat
                    remote-root ".mevedel" "sessions" session-id)))
                 (segment
                  (file-name-concat session-dir "segment-0001.chat.org")))
            (setq session
                  (mevedel-session-create "main" workspace remote-root)
                  buffer
                  (generate-new-buffer " *incarnation-ack-root*"))
            (let ((target (mevedel-session-execution-target session))
                  (replacement-incarnation
                   (secure-hash
                    'sha256
                    (mevedel-execution-target--incarnation-payload
                     "fixture" "fixture" "1" "fixture"))))
              (mevedel-execution-target-seed-incarnation
               target "old-incarnation")
              (setf (mevedel-execution-target-support-tier target) 'supported
                    (mevedel-session-sandbox-mode session) 'off
                    (mevedel-session-resource-grants session)
                    (list (list :path grant-path :access 'read))
                    (mevedel-session-session-id session) session-id
                    (mevedel-session-save-path session) session-dir
                    (mevedel-session-created-at session) "created"
                    (mevedel-session-updated-at session) "updated"
                    (mevedel-session-current-segment session) 1)
              (puthash
               (mevedel-execution-target-identity target)
               t mevedel-session-durability--disclosed-targets)
              (with-current-buffer buffer
                (org-mode)
                (setq-local mevedel--session session)
                (setq buffer-file-name segment)
                (insert "Published transcript\n")
                (set-buffer-modified-p nil))
              (make-directory session-dir t)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir (buffer-name buffer) session))
              (should
               (mevedel-session-publication-publish
                session
                (list
                 (list :path segment :content "Published transcript\n")
                 (list
                  :path
                  (file-name-concat session-dir "session.meta.el")
                  :content
                  (let ((print-length nil)
                        (print-level nil)
                        (print-circle t)
                        (print-quoted t))
                    (prin1-to-string
                     (mevedel-session-persistence-serialize session)))
                  :commit-marker t))))
              (cl-letf (((symbol-function 'executable-find)
                         (lambda (name &optional _remote)
                           (concat "/usr/bin/" name)))
                        ((symbol-function 'process-file)
                         (lambda (program _in destination _display
                                          &rest args)
                           (with-current-buffer destination
                             (insert
                              (cond
                               ((equal program "env")
                                (concat "HOME=" local-root "\0"))
                               ((string-suffix-p "bash" program)
                                "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                               ((equal args '("-r")) "6.8.0-target\n")
                               (t "Linux\n"))))
                           0)))
                (let ((readiness
                       (mevedel-execution-target-probe target t 'off)))
                  (should (eq 'ready (plist-get readiness :status)))))
              ;; Anything persisting reentrantly after the probe still sees
              ;; the old baseline paired with the old exact grant.
              (let ((pre-ack
                     (mevedel-session-persistence-serialize session)))
                (should
                 (equal "old-incarnation"
                        (plist-get pre-ack :target-incarnation)))
                (should
                 (equal
                  (list
                   (list :path (file-name-concat local-root "granted.el")
                         :access 'read))
                  (plist-get pre-ack :resource-grants))))
              (should (equal "old-incarnation"
                             (mevedel-execution-target-incarnation target)))
              (should (equal replacement-incarnation
                             (mevedel-execution-target-observed-incarnation
                              target)))
              (with-current-buffer buffer
                (should (mevedel-request-p
                         (mevedel-request-begin session))))
              (let* ((sidecar-text
                      (mevedel-session-persistence-read-artifact
                       session "session.meta.el" t))
                     (sidecar
                      (with-temp-buffer
                        (insert sidecar-text)
                        (goto-char (point-min))
                        (read (current-buffer))))
                     (reloaded
                      (plist-get
                       (mevedel-session-persistence-deserialize
                        sidecar workspace)
                       :session)))
                (should
                 (equal replacement-incarnation
                        (plist-get sidecar :target-incarnation)))
                (should-not (plist-get sidecar :resource-grants))
                (should
                 (equal replacement-incarnation
                        (mevedel-execution-target-incarnation
                         (mevedel-session-execution-target reloaded))))
                (should-not
                 (mevedel-session-resource-grants reloaded)))
              (should-not
               (mevedel-execution-target-incarnation-changed-p target))
              (should-not
               (mevedel-execution-target-observed-incarnation target))
              ;; A replacement after request admission is fenced again at
              ;; the next mutating-tool boundary, not deferred to a new turn.
              (setf (mevedel-session-resource-grants session)
                    (list (list :path grant-path :access 'read)))
              (mevedel-execution-target--record-incarnation
               target "second-incarnation")
              (with-current-buffer buffer
                (mevedel-session-persistence-assert-new-mutation-authority
                 session))
              (should-not (mevedel-session-resource-grants session))
              (should (equal "second-incarnation"
                             (mevedel-execution-target-incarnation target)))
              (should-not
               (mevedel-execution-target-incarnation-changed-p target)))))
      (when (and session (mevedel-session-save-path session))
        ;; Releasing reaches the target, so it needs the mock method that
        ;; only resolves inside this helper.
        (ignore-errors
          (mevedel-test--with-local-shell-tramp (list host)
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "keeps a failed replacement marker pending and retryable"
  (with-temp-buffer
    (let* ((target (mevedel-execution-target-create
                    "/docker:dev:/workspace/"))
           (workspace (mevedel-workspace--create
                       :type 'project :id "remote"
                       :root "/docker:dev:/workspace/" :name "remote"))
           (session (mevedel-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/docker:dev:/workspace/"
                     :session-id "failed-replacement"
                     :save-path "/tmp/mevedel-failed-replacement/"
                     :resource-grants
                     '((:path "/docker:dev:/outside.el" :access read))))
           (fail-publication t))
      (setq-local mevedel--session session)
      (setf (mevedel-execution-target-readiness target) '(:status ready)
            (mevedel-execution-target-incarnation target) "old-incarnation"
            (mevedel-execution-target-observed-incarnation target)
            "new-incarnation"
            (mevedel-execution-target-incarnation-changed-p target) t)
      (cl-letf
          (((symbol-function 'mevedel-execution-target-probe) #'ignore)
           ((symbol-function
             'mevedel-session-persistence-artifact-present-p)
            (lambda (&rest _) t))
           ((symbol-function
             'mevedel-session-persistence-assert-mutation-authority)
            (lambda (checked &optional _buffer)
              (when (mevedel-session-pending-publication checked)
                (user-error "Session has pending publication"))
              (mevedel-session-persistence--check-target-incarnation
               checked (current-buffer))
              t))
           ((symbol-function 'mevedel-permission-invalidate-target-grants)
            (lambda (checked)
              (setf (mevedel-session-resource-grants checked) nil)
              t))
           ((symbol-function
             'mevedel-session-persistence-publish-sidecar-state)
            (lambda (checked _root-buffer)
              (if fail-publication
                  (progn
                    (setf (mevedel-session-pending-publication checked)
                          '(:batches (staged-incarnation-marker)))
                    (user-error "Injected publication failure"))
                t))))
        (should-error (mevedel-request-begin session) :type 'user-error)
        (should
         (mevedel-execution-target-incarnation-changed-p target))
        (should (equal "new-incarnation"
                       (mevedel-execution-target-incarnation target)))
        (should (equal "new-incarnation"
                       (mevedel-execution-target-observed-incarnation target)))
        (should-not (mevedel-session-resource-grants session))
        (should (mevedel-session-pending-publication session))
        (should-error (mevedel-request-begin session) :type 'user-error)
        (setf (mevedel-session-pending-publication session) nil
              fail-publication nil)
        (should (mevedel-request-p (mevedel-request-begin session)))
        (should-not
         (mevedel-execution-target-incarnation-changed-p target))
        (should-not
         (mevedel-execution-target-observed-incarnation target)))))

  :doc "keeps replacement unacknowledged when grant invalidation fails"
  (with-temp-buffer
    (let* ((target (mevedel-execution-target-create
                    "/docker:dev:/workspace/"))
           (workspace (mevedel-workspace--create
                       :type 'project :id "remote"
                       :root "/docker:dev:/workspace/" :name "remote"))
           (session (mevedel-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/docker:dev:/workspace/")))
      (setf (mevedel-execution-target-readiness target) '(:status ready)
            (mevedel-execution-target-incarnation target) "old-incarnation"
            (mevedel-execution-target-observed-incarnation target)
            "new-incarnation"
            (mevedel-execution-target-incarnation-changed-p target) t)
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-execution-target-probe)
                 #'ignore)
                ((symbol-function
                  'mevedel-session-persistence-assert-mutation-authority)
                 (lambda (checked &optional buffer)
                   (mevedel-session-persistence--check-target-incarnation
                    checked (or buffer (current-buffer)))
                   t))
                ((symbol-function
                  'mevedel-permission-invalidate-target-grants)
                 (lambda (_session) (user-error "Publication failed"))))
        (should-error (mevedel-request-begin session) :type 'user-error))
      (should
       (mevedel-execution-target-incarnation-changed-p target))
      (should (equal "old-incarnation"
                     (mevedel-execution-target-incarnation target)))
      (should (equal "new-incarnation"
                     (mevedel-execution-target-observed-incarnation target)))
      (should-not mevedel--current-request)))

  :doc "blocks a remote request with one aggregated readiness error"
  (with-temp-buffer
    (let* ((target (mevedel-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (workspace (mevedel-workspace--create
                       :type 'project :id "remote"
                       :root "/ssh:user@host:/srv/project/" :name "remote"))
           (session (mevedel-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/ssh:user@host:/srv/project/")))
      (setf (mevedel-execution-target-readiness target)
            '(:status blocked :reason missing-dependencies
              :missing-dependencies (rg bash)))
      (let ((message
             (condition-case err
                 (progn (mevedel-request-begin session) nil)
               (user-error (error-message-string err)))))
        (should (string-match-p "rg, bash" message))
        (should-not mevedel--current-request))))

  :doc "does not impose remote readiness probing on a local request"
  (with-temp-buffer
    (let* ((workspace (mevedel-workspace-get-or-create
                       'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" workspace)))
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-execution-target-probe)
                 (lambda (&rest _)
                   (ert-fail "local request unexpectedly probed"))))
        (should (mevedel-request-p (mevedel-request-begin session))))))

  :doc "invalidates local project grants, publishes the replacement, and admits later requests"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-local-incarnation-" t)))
         (workspace (mevedel-workspace-get-or-create
                     'project "local-incarnation" root "local-incarnation"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *local-incarnation-root*"))
         (grant-path (file-name-concat root "granted.el"))
         save-path)
    (unwind-protect
        (progn
          (with-temp-file grant-path
            (insert "granted"))
          (setf (mevedel-execution-target-incarnation
                 (mevedel-session-execution-target session))
                "old-incarnation"
                (mevedel-session-resource-grants session)
                (list (list :path grant-path :access 'read)))
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "Initial local request\n")
            (cl-letf (((symbol-function
                        'mevedel-execution-target--local-incarnation)
                       (lambda () "old-incarnation")))
              (mevedel-session-persistence-save session buffer))
            (setq save-path (mevedel-session-save-path session))
            (let ((sidecar
                   (with-temp-buffer
                     (insert
                      (mevedel-session-persistence-read-artifact
                       session "session.meta.el" t))
                     (goto-char (point-min))
                     (read (current-buffer)))))
              (should (equal "old-incarnation"
                             (plist-get sidecar :target-incarnation)))
              (should (plist-get sidecar :resource-grants)))
            (cl-letf (((symbol-function
                        'mevedel-execution-target--local-incarnation)
                       (lambda () "new-incarnation")))
              (should (mevedel-request-p (mevedel-request-begin session)))
              (should-not (mevedel-session-resource-grants session))
              (should-not
               (mevedel-execution-target-incarnation-changed-p
                (mevedel-session-execution-target session)))
              (let ((sidecar
                     (with-temp-buffer
                       (insert
                        (mevedel-session-persistence-read-artifact
                         session "session.meta.el" t))
                       (goto-char (point-min))
                       (read (current-buffer)))))
                (should (equal "new-incarnation"
                               (plist-get sidecar :target-incarnation)))
                (should-not (plist-get sidecar :resource-grants)))
              ;; A later request must see the acknowledged replacement as
              ;; the current baseline and remain admissible.
              (mevedel-request-end)
              (should (mevedel-request-p (mevedel-request-begin session)))
              (mevedel-request-end))))
      (when mevedel--current-request
        (mevedel-request-end))
      (when (and save-path (mevedel-session-save-path session))
        (ignore-errors
          (mevedel-session-persistence-lock-release save-path session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-request-end
  (:before-each (mevedel-workspace-clear-registry)
   :after-each
   (mevedel-workspace-clear-registry)
   (setq mevedel--current-request nil))
  ,test
  (test)
  :doc "clears buffer-local"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws)))
      (mevedel-request-begin session)
      (should mevedel--current-request)
      (mevedel-request-end)
      (should (null mevedel--current-request))
      (should (eq 'idle
                  (mevedel-session-agent-root-activity session)))))

  :doc "drains every registered canceller on end"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws)))
      (let* ((fired nil)
             (req (mevedel-request-begin session)))
        (mevedel-request-push-canceller req (lambda () (push 'a fired)))
        (mevedel-request-push-canceller req (lambda () (push 'b fired)))
        (mevedel-request-end)
        (should (equal (sort (copy-sequence fired) (lambda (a b)
                                                     (string< (symbol-name a)
                                                              (symbol-name b))))
                       '(a b)))
        (should (null mevedel--current-request)))))

  :doc "tolerates canceller errors"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws)))
      (let* ((survivor-fired nil)
             (req (mevedel-request-begin session)))
        (mevedel-request-push-canceller req (lambda () (error "Boom")))
        (mevedel-request-push-canceller
         req (lambda () (setq survivor-fired t)))
        (mevedel-request-end)
        (should survivor-fired)
        (should (null mevedel--current-request)))))

  :doc "does not re-invoke cancellers on second end"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws)))
      (let* ((count 0)
             (req (mevedel-request-begin session)))
        (mevedel-request-push-canceller req (lambda () (cl-incf count)))
        ;; Drain once manually, then end; canceller already fired and
        ;; the list is empty, so end must not re-fire it.
        (mevedel-request-drain-cancellers req)
        (should (= count 1))
        (mevedel-request-end)
        (should (= count 1))
        (should (null mevedel--current-request)))))

  :doc "no-op when no active request"
  (with-temp-buffer
    (should (null mevedel--current-request))
    (mevedel-request-end)
    (should (null mevedel--current-request))))

(mevedel-deftest mevedel-request-cancel ()
  ,test
  (test)
  :doc "cancels only an explicit request's scoped permission entries"
  (let* ((session (mevedel-session--create))
         (request
          (mevedel-request--create
           :id "request-1"
           :session session
           :origin "/root/worker"))
         swept)
    (cl-letf (((symbol-function 'mevedel-permission-queue-sweep-request)
               (lambda (request-id actual-session &optional _no-render)
                 (setq swept (list request-id actual-session)))))
      (mevedel-request-cancel request))
    (should
     (equal
      "request-1"
      (car swept)))
    (should (eq session (cadr swept))))
  :doc "drains registered cancellers without changing the ambient request"
  (let* ((ambient (mevedel-request--create))
         (request (mevedel-request--create))
         fired)
    (let ((mevedel--current-request ambient))
      (mevedel-request-push-canceller
       request (lambda () (setq fired t)))
      (cl-letf (((symbol-function 'mevedel-permission-queue-sweep-request)
                 #'ignore))
        (mevedel-request-cancel request))
      (should fired)
      (should (eq ambient mevedel--current-request)))))

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

(mevedel-deftest mevedel-request-end/queues
  (:before-each (mevedel-workspace-clear-registry)
   :after-each
   (mevedel-workspace-clear-registry)
   (setq mevedel--current-request nil))
  ,test
  (test)

  :doc "request end sweeps only request-owned permissions and keeps plan approvals"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws)))
      (let* ((outcomes nil)
             (request (mevedel-request-begin session)))
        (setf (mevedel-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root"
                        :request-id (mevedel-request-id request)
                        :callback
                        (lambda (outcome)
                          (push (cons 'main-permission outcome)
                                outcomes)))
                  (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root"
                        :request-id "other-request"
                        :callback
                        (lambda (outcome)
                          (push (cons 'agent-permission outcome)
                                outcomes)))))
        (setf (mevedel-session-pending-plan-approval session)
              (list :body "# Plan"
                          :chat-buffer (current-buffer)
                          :session session
                          :callback
                          (lambda (outcome)
                            (push (cons 'plan outcome) outcomes))))
        (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                   #'ignore))
          (mevedel-request-end))
        (should (= 1 (length (mevedel-session-permission-queue session))))
        (should (equal "other-request"
                       (plist-get (car (mevedel-session-permission-queue session))
                                  :request-id)))
        (should (mevedel-session-pending-plan-approval session))
        (should (equal '((main-permission . aborted))
                       outcomes))
        (mevedel-request-end)
        (should (equal '((main-permission . aborted))
                       outcomes)))))

  :doc "request end renders a surviving permission when the swept entry was visible"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws)))
      (let* ((outcomes nil)
             (rendered nil)
             (request (mevedel-request-begin session)))
        (setf (mevedel-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root"
                        :request-id (mevedel-request-id request)
                        :callback
                        (lambda (outcome)
                          (push (cons 'main-permission outcome)
                                outcomes)))
                  (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root/verifier"
                        :request-id "other-request"
                        :callback
                        (lambda (outcome)
                          (push (cons 'agent-permission outcome)
                                outcomes)))))
        (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                   (lambda (entry)
                     (push (plist-get entry :origin) rendered))))
          (mevedel-request-end))
        (should (equal '("/root/verifier") rendered))
        (should (equal '((main-permission . aborted))
                       outcomes)))))

  :doc "agent request end sweeps only that agent's permission entries"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (agent (mevedel-agent--create :name "verifier"))
           (inv (mevedel-agent-invocation-create agent))
           (outcomes nil)
           request)
      (setf (mevedel-agent-invocation-agent-id inv) "verifier--abc")
      (setf (mevedel-agent-invocation-path inv) "/root/verifier")
      (setq-local mevedel--agent-invocation inv)
      (setq request (mevedel-request-begin session))
      (setf (mevedel-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root"
                        :request-id "root-request"
                        :callback
                        (lambda (outcome)
                          (push (cons 'main-permission outcome)
                                outcomes)))
                  (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root/verifier"
                        :request-id (mevedel-request-id request)
                        :callback
                        (lambda (outcome)
                          (push (cons 'agent-permission outcome)
                                outcomes)))))
      (mevedel-request-end)
      (should (= 1 (length (mevedel-session-permission-queue session))))
      (should (equal "/root"
                     (plist-get (car (mevedel-session-permission-queue session))
                                :origin)))
      (should (equal '((agent-permission . aborted))
                     outcomes)))))

(provide 'test-mevedel-structs)
;;; test-mevedel-structs.el ends here
