;;; test-mevedel-executions-list.el --- Live execution cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests all-owner execution projection, controls, refresh, and slash commands.

;;; Code:

(require 'mevedel-cockpit)
(require 'mevedel-execution)
(require 'mevedel-executions-list)
(require 'mevedel-structs)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-executions-list-test--session ()
  "Return a minimal execution cockpit session."
  (mevedel-session--create :name "main"))

(defun mevedel-executions-list-test--context (session view data)
  "Return a cockpit context for SESSION, VIEW, and DATA."
  (list :session session
        :view-buffer view
        :data-buffer data
        :origin-buffer view))

(defun mevedel-executions-list-test--item (artifact)
  "Return one live execution snapshot backed by ARTIFACT."
  (list :execution-id "exec-000007"
        :owner "explorer--one"
        :command "python -i"
        :state 'running
        :tty t
        :wall-time-seconds 65.2
        :output-bytes 42
        :output-lines 3
        :output-tail ">>> ready"
        :artifact-path artifact
        :sandbox-state 'bubblewrap
        :sandbox-facts
        '(:sandbox bubblewrap :filesystem workspace-write :network isolated)))

(mevedel-deftest mevedel-executions-list-open ()
  ,test
  (test)
  :doc "opens an all-owner row with live facts and details"
  (let* ((session (mevedel-executions-list-test--session))
         (view (generate-new-buffer " *execution-list-view*"))
         (data (generate-new-buffer " *execution-list-data*"))
         (artifact (make-temp-file "mevedel-execution-list-artifact-"))
         (item (mevedel-executions-list-test--item artifact)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-execution-list-user)
                   (lambda (seen-session)
                     (should (eq session seen-session))
                     (list item))))
          (let ((buffer
                 (mevedel-executions-list-open
                  (mevedel-executions-list-test--context
                   session view data))))
            (with-current-buffer buffer
              (should (eq major-mode 'mevedel-executions-list-mode))
              (should (= 1 (length tabulated-list-entries)))
              (should (equal "exec-000007" (tabulated-list-get-id)))
              (should (string-match-p "1 live execution"
                                      (mevedel-cockpit-surface-header-line)))
              (should (string-match-p
                       ">>> ready"
                       (mevedel-executions-list--details item nil)))))
      (when-let* ((buffer (get-buffer mevedel-executions-list-buffer-name)))
        (kill-buffer buffer))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (when (file-exists-p artifact) (delete-file artifact))))))

(mevedel-deftest mevedel-executions-list-send-input ()
  ,test
  (test)
  :doc "sends one newline-terminated line only to a selected PTY"
  (let ((item '(:tty t)) call)
    (cl-letf (((symbol-function 'mevedel-executions-list--selected)
               (lambda () item))
              ((symbol-function 'mevedel-executions-list--call-control)
               (lambda (&rest args) (setq call args)))
              ((symbol-function 'read-string) (lambda (&rest _) "hello")))
      (mevedel-executions-list-send-input))
    (should (equal (list #'mevedel-execution-write-user "hello\n") call))
    (setq item '(:tty nil))
    (should-error (mevedel-executions-list-send-input) :type 'user-error)))

(mevedel-deftest mevedel-executions-list-interrupt ()
  ,test
  (test)
  :doc "routes interrupt through session-wide user authority"
  (let (call)
    (cl-letf (((symbol-function 'mevedel-executions-list--call-control)
               (lambda (&rest args) (setq call args))))
      (mevedel-executions-list-interrupt))
    (should (equal (list #'mevedel-execution-interrupt-user) call))))

(mevedel-deftest mevedel-executions-list-stop ()
  ,test
  (test)
  :doc "routes stop through session-wide user authority"
  (let (call)
    (cl-letf (((symbol-function 'mevedel-executions-list--call-control)
               (lambda (&rest args) (setq call args))))
      (mevedel-executions-list-stop))
    (should (equal (list #'mevedel-execution-stop-user) call))))

(mevedel-deftest mevedel-executions-list-open-artifact ()
  ,test
  (test)
  :doc "opens a readable selected artifact and rejects a stale path"
  (let ((artifact (make-temp-file "mevedel-execution-open-artifact-"))
        opened item)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-executions-list--selected)
                   (lambda () item))
                  ((symbol-function 'find-file)
                   (lambda (path) (setq opened path))))
          (setq item (list :artifact-path artifact))
          (mevedel-executions-list-open-artifact)
          (should (equal artifact opened))
          (setq item '(:artifact-path "/missing/mevedel-artifact"))
          (should-error (mevedel-executions-list-open-artifact)
                        :type 'user-error))
      (delete-file artifact))))

(mevedel-deftest mevedel-executions-list-quit ()
  ,test
  (test)
  :doc "returns to the owning session cockpit"
  (let (label)
    (cl-letf (((symbol-function 'mevedel-cockpit-quit)
               (lambda (&optional seen) (setq label seen))))
      (mevedel-executions-list-quit))
    (should (equal "execution cockpit" label))))

(mevedel-deftest mevedel-executions-list--refresh-session ()
  ,test
  (test)
  :doc "refreshes live output and removes a completed row"
  (let* ((session (mevedel-executions-list-test--session))
         (view (generate-new-buffer " *execution-stale-view*"))
         (data (generate-new-buffer " *execution-stale-data*"))
         (artifact (make-temp-file "mevedel-execution-stale-artifact-"))
         (item (mevedel-executions-list-test--item artifact))
         (live-p t))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-execution-list-user)
                   (lambda (_session) (and live-p (list item)))))
          (let ((buffer
                 (mevedel-executions-list-open
                  (mevedel-executions-list-test--context
                   session view data))))
            (setq item (plist-put item :output-tail ">>> updated"))
            (mevedel-executions-list--refresh-session session)
            (with-current-buffer buffer
              (should (equal ">>> updated"
                             (plist-get
                              (mevedel-cockpit-surface-selected)
                              :output-tail))))
            (setq live-p nil)
            (mevedel-executions-list--refresh-session session)
            (with-current-buffer buffer
              (should-not tabulated-list-entries)
              (should-error (mevedel-executions-list-stop)
                            :type 'user-error))))
      (when-let* ((buffer (get-buffer mevedel-executions-list-buffer-name)))
        (kill-buffer buffer))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (when (file-exists-p artifact) (delete-file artifact)))))

(mevedel-deftest mevedel-executions-list-handle-event ()
  ,test
  (test)
  :doc "refreshes progress and yield without duplicating terminal settlement"
  (let (types)
    (cl-letf (((symbol-function 'mevedel-executions-list--refresh-session)
               (lambda (session) (push session types))))
      (dolist (type '(progress yield terminal))
        (mevedel-executions-list-handle-event
         (list :type type :session type))))
    (should (equal '(yield progress) types))))

(mevedel-deftest mevedel-executions-list--call-control ()
  ,test
  (test)
  :doc "refreshes and reports a selected-row race rejected by the backend"
  (let* ((session (mevedel-executions-list-test--session))
         (view (generate-new-buffer " *execution-race-view*"))
         (data (generate-new-buffer " *execution-race-data*"))
         (artifact (make-temp-file "mevedel-execution-race-artifact-"))
         (item (mevedel-executions-list-test--item artifact))
         (live-p t))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-execution-list-user)
                   (lambda (_session) (and live-p (list item))))
                  ((symbol-function 'mevedel-execution-stop-user)
                   (lambda (&rest _)
                     (setq live-p nil)
                     (signal 'mevedel-execution-not-found '("gone")))))
          (let ((buffer
                 (mevedel-executions-list-open
                  (mevedel-executions-list-test--context
                   session view data))))
            (with-current-buffer buffer
              (should-error (mevedel-executions-list-stop)
                            :type 'user-error)
              (should-not tabulated-list-entries))))
      (when-let* ((buffer (get-buffer mevedel-executions-list-buffer-name)))
        (kill-buffer buffer))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (when (file-exists-p artifact) (delete-file artifact)))))

(provide 'test-mevedel-executions-list)
;;; test-mevedel-executions-list.el ends here
