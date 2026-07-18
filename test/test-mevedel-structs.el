;;; test-mevedel-structs.el --- Tests for mevedel-structs.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-permission-queue)
(require 'mevedel-goal)
(require 'mevedel-agents)
(require 'mevedel-reminders)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


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
    (should (null (mevedel-session-agents session)))
    (should (null (mevedel-session-tasks session)))
    (should (null (mevedel-session-reminders session)))
    (should (null (mevedel-session-deferred-pending session)))
    (should (null (mevedel-session-deferred-injected session))))

  :doc "two sessions share same workspace by reference"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
         (s1 (mevedel-session-create "main" ws))
         (s2 (mevedel-session-create "refactor" ws)))
    (should (eq (mevedel-session-workspace s1)
                (mevedel-session-workspace s2)))))

(mevedel-deftest mevedel-session--set-execution-state ()
  ,test
  (test)
  :doc "stores opaque transient execution state by identity"
  (let* ((session (mevedel-session--create))
         (state (make-symbol "execution-state")))
    (mevedel-session--set-execution-state session state)
    (should (eq state (mevedel-session-execution-state session)))))


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
  :doc "prefers request ownership, then agent ownership, then main"
  (let ((mevedel--current-request
         (mevedel-request--create :origin "request-owner"))
        (mevedel--agent-invocation
         (mevedel-agent-invocation--create :agent-id "agent-owner")))
    (should (equal "request-owner" (mevedel-current-origin))))
  (let ((mevedel--current-request nil)
        (mevedel--agent-invocation
         (mevedel-agent-invocation--create :agent-id "agent-owner")))
    (should (equal "agent-owner" (mevedel-current-origin))))
  (let ((mevedel--current-request nil)
        (mevedel--agent-invocation nil))
    (should (equal "main" (mevedel-current-origin)))))

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
      (setq-local mevedel--agent-invocation inv)
      (let ((req (mevedel-request-begin session)))
        (should (equal "verifier--abc"
                       (mevedel-request-origin req))))))

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
           (outcomes nil))
      (mevedel-request-begin session)
      (setf (mevedel-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :origin "main"
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'permission outcome) outcomes)))))
      (setf (mevedel-session-plan-queue session)
            (list (list :body "# Plan"
                        :chat-buffer (current-buffer)
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'plan outcome) outcomes)))))
      (mevedel-request-begin session)
      (should (null (mevedel-session-permission-queue session)))
      (should (null (mevedel-session-plan-queue session)))
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
      (should (null mevedel--current-request))))

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
  :doc "cancels only an explicit request's scoped permission owner"
  (let* ((session (mevedel-session--create))
         (request
          (mevedel-request--create
           :session session
           :origin "goal-plan-revision--aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
         swept)
    (cl-letf (((symbol-function 'mevedel-permission-queue-sweep-origin)
               (lambda (origin actual-session &optional _no-render)
                 (setq swept (list origin actual-session)))))
      (mevedel-request-cancel request))
    (should
     (equal
      "goal-plan-revision--aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      (car swept)))
    (should (eq session (cadr swept))))
  :doc "drains registered cancellers without changing the ambient request"
  (let* ((ambient (mevedel-request--create))
         (request (mevedel-request--create))
         fired)
    (let ((mevedel--current-request ambient))
      (mevedel-request-push-canceller
       request (lambda () (setq fired t)))
      (cl-letf (((symbol-function 'mevedel-permission-queue-sweep-origin)
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

  :doc "request end sweeps only main-owned permissions and keeps plan approvals"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (outcomes nil))
      (mevedel-request-begin session)
      (setf (mevedel-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "main"
                        :callback
                        (lambda (outcome)
                          (push (cons 'main-permission outcome)
                                outcomes)))
                  (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "verifier--abc"
                        :callback
                        (lambda (outcome)
                          (push (cons 'agent-permission outcome)
                                outcomes)))))
      (setf (mevedel-session-plan-queue session)
            (list (list :body "# Plan"
                        :chat-buffer (current-buffer)
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'plan outcome) outcomes)))))
      (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                 #'ignore))
        (mevedel-request-end))
      (should (= 1 (length (mevedel-session-permission-queue session))))
      (should (equal "verifier--abc"
                     (plist-get (car (mevedel-session-permission-queue session))
                                :origin)))
      (should (mevedel-session-plan-queue session))
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
           (rendered nil))
      (mevedel-request-begin session)
      (setf (mevedel-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "main"
                        :callback
                        (lambda (outcome)
                          (push (cons 'main-permission outcome)
                                outcomes)))
                  (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "verifier--abc"
                        :callback
                        (lambda (outcome)
                          (push (cons 'agent-permission outcome)
                                outcomes)))))
      (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                 (lambda (entry)
                   (push (plist-get entry :origin) rendered))))
        (mevedel-request-end))
      (should (equal '("verifier--abc") rendered))
      (should (equal '((main-permission . aborted))
                     outcomes))))

  :doc "agent request end sweeps only that agent's permission entries"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (agent (mevedel-agent--create :name "verifier"))
           (inv (mevedel-agent-invocation-create agent))
           (outcomes nil))
      (setf (mevedel-agent-invocation-agent-id inv) "verifier--abc")
      (setq-local mevedel--agent-invocation inv)
      (mevedel-request-begin session)
      (setf (mevedel-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "main"
                        :callback
                        (lambda (outcome)
                          (push (cons 'main-permission outcome)
                                outcomes)))
                  (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "verifier--abc"
                        :callback
                        (lambda (outcome)
                          (push (cons 'agent-permission outcome)
                                outcomes)))))
      (mevedel-request-end)
      (should (= 1 (length (mevedel-session-permission-queue session))))
      (should (equal "main"
                     (plist-get (car (mevedel-session-permission-queue session))
                                :origin)))
      (should (equal '((agent-permission . aborted))
                     outcomes)))))

(provide 'test-mevedel-structs)
;;; test-mevedel-structs.el ends here
