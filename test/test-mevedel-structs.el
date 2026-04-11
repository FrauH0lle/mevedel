;;; test-mevedel-structs.el --- Tests for mevedel-structs.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
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
    (should (null (mevedel-workspace-additional-roots ws)))
    (should (null (mevedel-workspace-file-cache ws)))
    (should (null (mevedel-workspace-hints ws)))))


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
    (should-not (eq ws1 ws2))))

(mevedel-deftest mevedel-workspace-get
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "returns nil for unregistered workspace"
  (should (null (mevedel-workspace-get 'project "/tmp/nonexistent/")))

  :doc "returns workspace after registration"
  (let ((ws (mevedel-workspace-get-or-create
             'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (should (eq ws (mevedel-workspace-get 'project "/tmp/p1/")))))

(mevedel-deftest mevedel-workspace-all
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "returns empty list when registry is empty"
  (should (null (mevedel-workspace-all)))

  :doc "returns all registered workspaces"
  (progn
    (mevedel-workspace-get-or-create 'project "/tmp/p1/" "/tmp/p1/" "p1")
    (mevedel-workspace-get-or-create 'project "/tmp/p2/" "/tmp/p2/" "p2")
    (should (= 2 (length (mevedel-workspace-all))))))

(mevedel-deftest mevedel-workspace-clear-registry
  (:doc "`mevedel-workspace-clear-registry' removes all entries")
  (mevedel-workspace-get-or-create 'project "/tmp/p1/" "/tmp/p1/" "p1")
  (mevedel-workspace-clear-registry)
  (should (null (mevedel-workspace-all))))


;;
;;; Workspace helpers

(mevedel-deftest mevedel-workspace-state-dir
  (:doc "`mevedel-workspace-state-dir' returns .mevedel/ under root")
  (let ((ws (mevedel-workspace--create :root "/tmp/project/")))
    (should (equal "/tmp/project/.mevedel/"
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
    (should (equal "/tmp/nonexistent-project/.mevedel/config.el" result))))


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

  :doc "replaces stale request with warning"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (req1 (mevedel-request-begin session))
           (req2 (mevedel-request-begin session)))
      (should (eq req2 mevedel--current-request))
      (should-not (eq req1 req2)))))

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

  :doc "calls cancel-fn when set"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (cancelled nil)
           (req (mevedel-request-begin session)))
      (setf (mevedel-request-cancel-fn req) (lambda () (setq cancelled t)))
      (mevedel-request-end)
      (should cancelled)
      (should (null mevedel--current-request))))

  :doc "tolerates cancel-fn errors"
  (with-temp-buffer
    (let* ((ws (mevedel-workspace-get-or-create
                'project "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (mevedel-session-create "main" ws))
           (req (mevedel-request-begin session)))
      (setf (mevedel-request-cancel-fn req) (lambda () (error "Boom")))
      (mevedel-request-end)
      (should (null mevedel--current-request))))

  :doc "no-op when no active request"
  (with-temp-buffer
    (should (null mevedel--current-request))
    (mevedel-request-end)
    (should (null mevedel--current-request))))

(provide 'test-mevedel-structs)
;;; test-mevedel-structs.el ends here
