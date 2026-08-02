;;; test-mevedel-structs.el --- Tests for mevedel-structs.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-permission-queue)
(require 'mevedel-plan-mode)
(require 'mevedel-goal)
(require 'mevedel-agents)
(require 'mevedel-reminders)
(require 'mevedel-sandbox)
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
;;; Directive activity

(mevedel-deftest mevedel-directive-request-changed-p
  (:doc "compares the authored request with the latest attempt snapshot")
  (let ((directive
         (mevedel-directive--create
          :id "directive" :request "Current" :anchor '(:state attached)
          :attempts
          (list
           (mevedel-directive-attempt--create
            :directive-request "Older")
           (mevedel-directive-attempt--create
            :directive-request "Current")))))
    (should-not (mevedel-directive-request-changed-p directive))
    (mevedel-directive-set-request directive "Edited")
    (should (mevedel-directive-request-changed-p directive))
    (should-not (mevedel-directive-state directive))))

(mevedel-deftest mevedel-directive-recompute-state
  (:doc "derives lifecycle state from the latest surviving model activity")
  (let* ((success
         (mevedel-directive-attempt--create
           :sequence 1 :directive-request "Current" :outcome 'success
           :checkpoint '(:session-id "session" :turn 1)))
         (failure
         (mevedel-directive-attempt--create
           :sequence 3 :directive-request "Current" :outcome 'error
           :checkpoint '(:session-id "session" :turn 3)))
         (discussion
         (mevedel-directive-discussion-turn--create
           :sequence 2 :directive-request "Current" :outcome 'success
           :checkpoint '(:session-id "session" :turn 2)))
         (directive
          (mevedel-directive--create
           :id "directive" :request "Current" :anchor '(:state attached))))
    (should-not (mevedel-directive-recompute-state directive))
    (setf (mevedel-directive-attempts directive) (list success))
    (should (eq 'implemented
                (mevedel-directive-recompute-state directive)))
    (setf (mevedel-directive-discussion directive) (list discussion))
    (should (eq 'implemented
                (mevedel-directive-recompute-state directive)))
    (setf (mevedel-directive-attempts directive) (list success failure))
    (should (eq 'failed
                (mevedel-directive-recompute-state directive)))
    (setf (mevedel-directive-request directive) "Edited")
    (should-not (mevedel-directive-recompute-state directive))
    (setf (mevedel-directive-discussion directive)
          (append
           (mevedel-directive-discussion directive)
           (list
            (mevedel-directive-discussion-turn--create
             :sequence 4 :directive-request "Edited" :outcome 'success
             :checkpoint '(:session-id "session" :turn 4)))))
    (should (eq 'discussed
                (mevedel-directive-recompute-state directive)))))

(mevedel-deftest mevedel-directive-next-activity-sequence
  (:doc "allocates after the greatest surviving activity sequence")
  (let ((directive
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :attempts
          (list (mevedel-directive-attempt--create :sequence 2))
          :discussion
          (list (mevedel-directive-discussion-turn--create :sequence 5)))))
    (should (= 6 (mevedel-directive-next-activity-sequence directive)))))

(mevedel-deftest mevedel-workspace-rewind-directives
  ()
  ,test
  (test)
  :doc "prunes one execution-session suffix while retaining authored records"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'test :id "rewind" :root "/tmp" :name "rewind"))
         (earlier
          (mevedel-directive--create
           :id "earlier" :request "Earlier" :anchor '(:state attached)
           :attempts
           (list
            (mevedel-directive-attempt--create
             :sequence 1 :directive-request "Earlier" :outcome 'success
             :checkpoint '(:session-id "session" :turn 1))
            (mevedel-directive-attempt--create
             :sequence 3 :directive-request "Earlier" :outcome 'error
             :checkpoint '(:session-id "session" :turn 4)))
           :discussion
           (list
            (mevedel-directive-discussion-turn--create
             :sequence 2 :outcome 'success
             :checkpoint '(:session-id "session" :turn 2)))))
         (edited
          (mevedel-directive--create
           :id "edited" :request "Edited request" :anchor '(:state attached)
           :attempts
           (list
            (mevedel-directive-attempt--create
             :sequence 1 :directive-request "Original request" :outcome 'success
             :checkpoint '(:session-id "session" :turn 2)))
           :discussion
           (list
            (mevedel-directive-discussion-turn--create
             :sequence 2 :directive-request "Edited request" :outcome 'success
             :checkpoint '(:session-id "session" :turn 3)))))
         (later
          (mevedel-directive--create
           :id "later" :request "Later" :anchor '(:state source-missing)
           :attempts
           (list
            (mevedel-directive-attempt--create
             :directive-request "Later" :outcome 'aborted
             :checkpoint '(:session-id "session" :turn 5))))))
    (mevedel-workspace-set-directives workspace (list earlier edited later))
    (mevedel-workspace-rewind-directives workspace "session" 4)
    (should (equal (list earlier edited later)
                   (mevedel-workspace-directives workspace)))
    (should (= 1 (length (mevedel-directive-attempts earlier))))
    (should (eq 'implemented (mevedel-directive-state earlier)))
    (should (= 1 (length (mevedel-directive-attempts edited))))
    (should (eq 'discussed (mevedel-directive-state edited)))
    (should-not (mevedel-directive-attempts later))
    (should-not (mevedel-directive-state later)))

  :doc "restores consumed children while retaining later authored children"
  (let* ((consumed
          (mevedel-subdirective--create
           :id "consumed" :request "Original detail"
           :anchor '(:state attached :file "/tmp/source" :start 3 :end 7)))
         (later
          (mevedel-subdirective--create
           :id "later" :request "Later correction"
           :anchor '(:state attached :file "/tmp/source" :start 9 :end 12)))
         (directive
          (mevedel-directive--create
           :id "parent" :request "Parent" :anchor '(:state attached)
           :state 'implemented :subdirectives (list later)
           :attempts
           (list
            (mevedel-directive-attempt--create
             :directive-request "Parent" :outcome 'success
             :consumed-subdirectives (list consumed)
             :checkpoint '(:session-id "session" :turn 2)))))
         (workspace
          (mevedel-workspace--create
           :type 'test :id "children" :root "/tmp" :name "children"
           :directives (list directive))))
    (mevedel-workspace-rewind-directives workspace "session" 2)
    (should-not (mevedel-directive-attempts directive))
    (should-not (mevedel-directive-state directive))
    (should
     (equal '("consumed" "later")
            (mapcar #'mevedel-subdirective-id
                    (mevedel-directive-subdirectives directive))))))


;;
;;; Session transient state

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
    (should (eq 'best-effort (mevedel-session-sandbox-mode session))))

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
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (req (mevedel-request-begin session)))
      (should (mevedel-request-p req))
      (should (eq req mevedel--current-request))
      (should (eq session (mevedel-request-session req)))
      (should (eq 'running
                  (mevedel-session-agent-root-activity session)))
      (should (hash-table-p (mevedel-request-file-snapshots req)))
      (should (null (mevedel-request-directive-uuid req)))))

  :doc "sets directive-uuid when provided"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (req (mevedel-request-begin session "test-uuid")))
      (should (equal "test-uuid" (mevedel-request-directive-uuid req)))))

  :doc "records agent origin when request begins in a sub-agent buffer"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
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
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (req1 (mevedel-request-begin session))
           (req2 (mevedel-request-begin session)))
      (should (eq req2 mevedel--current-request))
      (should-not (eq req1 req2))))

  :doc "replacing stale request drains queued interactions"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
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
      (mevedel-request-begin session)
      (should (null (mevedel-session-permission-queue session)))
      (should (null (mevedel-session-pending-plan-approval session)))
      (should (equal '((plan . aborted) (permission . aborted))
                     outcomes)))))

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
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
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
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (fired nil)
           (req (mevedel-request-begin session)))
      (mevedel-request-push-canceller req (lambda () (push 'a fired)))
      (mevedel-request-push-canceller req (lambda () (push 'b fired)))
      (mevedel-request-end)
      (should (equal (sort (copy-sequence fired) (lambda (a b)
                                                   (string< (symbol-name a)
                                                            (symbol-name b))))
                     '(a b)))
      (should (null mevedel--current-request))))

  :doc "tolerates canceller errors"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (survivor-fired nil)
           (req (mevedel-request-begin session)))
      (mevedel-request-push-canceller req (lambda () (error "Boom")))
      (mevedel-request-push-canceller
       req (lambda () (setq survivor-fired t)))
      (mevedel-request-end)
      (should survivor-fired)
      (should (null mevedel--current-request))))

  :doc "does not re-invoke cancellers on second end"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (count 0)
           (req (mevedel-request-begin session)))
      (mevedel-request-push-canceller req (lambda () (cl-incf count)))
      ;; Drain once manually, then end; canceller already fired and
      ;; the list is empty, so end must not re-fire it.
      (mevedel-request-drain-cancellers req)
      (should (= count 1))
      (mevedel-request-end)
      (should (= count 1))
      (should (null mevedel--current-request))))

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
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (path "/tmp/dropped.txt")
           (expanded (expand-file-name path)))
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
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (outcomes nil)
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
                     outcomes))))

  :doc "request end renders a surviving permission when the swept entry was visible"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (outcomes nil)
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
                     outcomes))))

  :doc "agent request end sweeps only that agent's permission entries"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
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
