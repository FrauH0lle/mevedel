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
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-structs)
(require 'mevedel-permissions)
(require 'mevedel-permission-queue)

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

  :doc "first entry renders immediately; subsequent entries wait"
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
    ;; Only the first entry was rendered (the others wait their turn).
    (should (= 1 (length rendered)))
    (should (equal "Read" (plist-get (car rendered) :tool-name)))
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
    (should (null (mevedel-session-permission-queue session)))))


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
             :origin "explore--abc"
             :callback (lambda (o) (push (cons "explore" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "main"
             :callback (lambda (o) (push (cons "main" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "explore--abc"
             :callback (lambda (o) (push (cons "explore2" o) outcomes))))
      (mevedel-permission-queue-sweep-agent "explore--abc" session))
    ;; explore-owned entries fired 'aborted.
    (should (eq 'aborted (cdr (assoc "explore" outcomes))))
    (should (eq 'aborted (cdr (assoc "explore2" outcomes))))
    ;; main-owned entry did NOT fire — still queued.
    (should-not (assoc "main" outcomes))
    (let ((q (mevedel-session-permission-queue session)))
      (should (= 1 (length q)))
      (should (equal "main" (plist-get (car q) :origin))))))


(provide 'test-mevedel-permission-queue)
;;; test-mevedel-permission-queue.el ends here
