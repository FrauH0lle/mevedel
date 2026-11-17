;;; test-mevedel-claude-code-fork.el --- Native answer forks -*- lexical-binding: t -*-

;;; Commentary:
;; Check exact fork coordinates, admission exclusion and owned cleanup.

;;; Code:

(require 'mevedel-claude-code-fork)
(require 'mevedel-claude-code)
(require 'mevedel-acp)
(require 'mevedel-turn)
(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(ert-deftest mevedel-claude-code-fork/coordinates-and-owned-cleanup ()
  (let* ((mevedel-claude-code-fork--sessions nil)
         (session (mevedel-session--create))
         (record (list :engine 'claude-code :id "current" :host (system-name)
                       :directory (expand-file-name mevedel-claude-code-directory)
                       :state 'uncertain :boundaries
                       `((:turn 1 :id "original" :host ,(system-name)
                                :directory ,(expand-file-name mevedel-claude-code-directory)
                                :message-id "answer-1"
                                :input-boundary (1 . 20)))))
         callbacks calls result cleanup)
    (cl-letf (((symbol-function 'mevedel-claude-code-launch)
               (lambda (&rest _) '(:meta ((claudeCode . ((options . nil)))))))
              ((symbol-function 'mevedel-acp-session-operation)
               (lambda (launch operation id complete)
                 (push (list launch operation id) calls)
                 (if (eq operation 'delete) (funcall complete '(:status success))
                   (push complete callbacks)))))
      (should (eq :pending
                  (mevedel-claude-code-fork-histories
                   (list (cons "root" record))
                   (lambda (histories error dispose)
                     (should-not error)
                     (should (memq session mevedel-claude-code-fork--sessions))
                     (setq result histories cleanup dispose)) session)))
      (should (memq session mevedel-claude-code-fork--sessions))
      (should (equal "original" (nth 2 (car calls))))
      (should (equal '((version . 1) (messageId . "answer-1"))
                     (alist-get 'fork (alist-get 'air (alist-get 'jetbrains
                      (plist-get (caar calls) :meta))))))
      (funcall (pop callbacks) '(:status success :session-id "new"))
      (should-not mevedel-claude-code-fork--sessions)
      (should (equal "new" (plist-get (cdar result) :id)))
      (should (eq 'uncertain (plist-get (cdar result) :state)))
      (should (equal '(1 . 20) (plist-get (cdar result) :input-boundary)))
      (should (equal "current" (plist-get record :id)))
      (funcall cleanup)
      (funcall cleanup)
      (should (equal '(delete "new") (cdr (car calls))))
      (should (= 2 (length calls))))))

(ert-deftest mevedel-claude-code-fork/scope-failure-deletes-only-created-forks ()
  (let* ((mevedel-claude-code-fork--sessions nil)
         (session (mevedel-session--create))
         (base (list :engine 'claude-code :host (system-name)
                     :directory (expand-file-name mevedel-claude-code-directory) :state 'ready))
         (root (append base (list :id "root-old" :boundaries
                                  (list (append base '(:turn 1 :id "root-old" :message-id "root-a"))))))
         (child (append base (list :id "child-old" :boundaries
                                   (list (append base '(:turn 1 :id "child-old" :message-id "child-a"))))))
         callbacks deleted diagnosis)
    (cl-letf (((symbol-function 'mevedel-claude-code-launch) (lambda (&rest _) nil))
              ((symbol-function 'mevedel-acp-session-operation)
               (lambda (_launch operation id complete)
                 (if (eq operation 'delete)
                     (progn (push id deleted) (funcall complete '(:status success)))
                   (push complete callbacks)))))
      (mevedel-claude-code-fork-histories
       (list (cons "root" root) (cons "/root/child" child))
       (lambda (histories error _cleanup)
         (should-not histories) (setq diagnosis error)) session)
      (funcall (pop callbacks) '(:status success :session-id "root-new"))
      (funcall (pop callbacks) '(:status error :message "Native session was deleted"))
      (should (string-search "/root/child" diagnosis))
      (should (string-search "Native session was deleted" diagnosis))
      (should (equal '("root-new") deleted))
      (should-not mevedel-claude-code-fork--sessions))))

(ert-deftest mevedel-claude-code-fork/refuses-missing-answer-before-native-operation ()
  (let ((session (mevedel-session--create)) (mevedel-claude-code-fork--sessions nil) diagnosis)
    (cl-letf (((symbol-function 'mevedel-acp-session-operation)
               (lambda (&rest _) (ert-fail "No native operation without a recorded boundary"))))
      (mevedel-claude-code-fork-histories
       (list (list "root" :engine 'claude-code :id "native" :host (system-name)
                   :directory (expand-file-name mevedel-claude-code-directory) :state 'ready))
       (lambda (_histories error _cleanup) (setq diagnosis error)) session)
      (should (string-search "No recorded native answer boundary for root" diagnosis))
      (should-not mevedel-claude-code-fork--sessions))))

(ert-deftest mevedel-claude-code-fork/excludes-root-and-child-admission ()
  (let* ((session (mevedel-session--create))
         (mevedel-claude-code-fork--sessions (list session)))
    (dolist (child '(nil t))
      (let ((mevedel--agent-invocation child))
        (should-error (mevedel-request-begin session) :type 'user-error)))))

(ert-deftest mevedel-claude-code-fork/rejects-source-and-created-id-collisions ()
  (dolist (collision '("child-old" "child-source" "root-new"))
    (let* ((base (list :engine 'claude-code :host (system-name)
                       :directory (expand-file-name mevedel-claude-code-directory) :state 'ready))
           (root (append base (list :id "root-old" :boundaries
                                    (list (append base '(:turn 1 :id "root-source" :message-id "root-a"))))))
           (child (append base (list :id "child-old" :boundaries
                                     (list (append base '(:turn 1 :id "child-source" :message-id "child-a"))))))
           callbacks deleted diagnosis)
      (cl-letf (((symbol-function 'mevedel-claude-code-launch) (lambda (&rest _) nil))
                ((symbol-function 'mevedel-acp-session-operation)
                 (lambda (_launch operation id complete)
                   (if (eq operation 'delete)
                       (progn (push id deleted) (funcall complete '(:status success)))
                     (push complete callbacks)))))
        (mevedel-claude-code-fork-histories
         (list (cons "root" root) (cons "/root/child" child))
         (lambda (histories error _cleanup)
           (should-not histories) (setq diagnosis error)))
        (funcall (pop callbacks) '(:status success :session-id "root-new"))
        (funcall (pop callbacks) (list :status 'success :session-id collision))
        (should (string-search "existing session ID" diagnosis))
        (should (equal '("root-new") deleted))))))

(ert-deftest mevedel-claude-code-fork/rejects-foreign-answer-on-local-record ()
  (let* ((record (list :engine 'claude-code :id "local" :host (system-name)
                       :directory (expand-file-name mevedel-claude-code-directory) :state 'ready
                       :boundaries '((:turn 1 :id "foreign" :message-id "foreign-a"
                                           :host "other-installation" :directory "/other/"))))
         diagnosis)
    (cl-letf (((symbol-function 'mevedel-acp-session-operation)
               (lambda (&rest _) (ert-fail "Must reject foreign source before native operation"))))
      (mevedel-claude-code-fork-histories
       (list (cons "root" record))
       (lambda (_histories error _cleanup) (setq diagnosis error)))
      (should (string-search "another installation" diagnosis)))))

(provide 'test-mevedel-claude-code-fork)
;;; test-mevedel-claude-code-fork.el ends here
