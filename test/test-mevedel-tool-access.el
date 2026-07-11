;;; test-mevedel-tool-access.el --- Tests for directory access tool -*- lexical-binding: t -*-

;;; Commentary:

;; RequestAccess deduplication, diagnostics, result formatting, and schema.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'mevedel-permission-log)
(require 'mevedel-pipeline)
(require 'mevedel-structs)
(require 'mevedel-tool-access)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-tool-access--read-permission-log (session)
  "Read SESSION's permission log entries."
  (let ((file (mevedel-permission-log-path session))
        entries)
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (condition-case nil
            (while t
              (push (read (current-buffer)) entries))
          (end-of-file nil))))
    (nreverse entries)))


;;
;;; Registration

(mevedel-deftest mevedel-tool-access-register
  (:vars ((saved-registry (copy-hash-table mevedel-tool--registry)))
   :before-each (mevedel-tool-clear-registry)
   :after-each (setq mevedel-tool--registry saved-registry))
  ,test
  (test)
  :doc "declares RequestAccess.directory as a semantic path"
  (progn
    (mevedel-tool-access-register)
    (should
     (eq 'path
         (cadr (assq 'directory
                     (mevedel-tool-args
                      (mevedel-tool-get "RequestAccess"))))))))


;;
;;; Request deduplication

(mevedel-deftest mevedel-tools--request-access ()
  ,test
  (test)
  :doc "later concurrent requests join the first prompt"
  (with-temp-buffer
    (let ((calls 0))
      (cl-letf (((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (_root _reason _callback) (cl-incf calls)))
                ((symbol-function 'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (&rest _args) nil)))
        (mevedel-tools--request-access "/tmp/foo" "" #'ignore)
        (mevedel-tools--request-access "/tmp/foo" "" #'ignore)
        (mevedel-tools--request-access "/tmp/foo" "" #'ignore))
      (should (= 1 calls))
      (should (= 2 (length (cddr
                            (assoc "/tmp/foo"
                                   mevedel--pending-access-requests
                                   #'string=)))))))
  :doc "one approval fans out and later calls hit the cache"
  (with-temp-buffer
    (let (received prompt-callback)
      (cl-letf (((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (_root _reason callback)
                   (setq prompt-callback callback)))
                ((symbol-function 'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'mevedel-add-project-root) #'ignore))
        (dotimes (index 3)
          (mevedel-tools--request-access
           "/tmp/bar" "" (lambda (outcome)
                            (push (cons index outcome) received))))
        (funcall prompt-callback 'approve)
        (should (= 3 (length received)))
        (should (cl-every (lambda (entry) (eq (cdr entry) 'approve))
                          received))
        (setq received nil)
        (mevedel-tools--request-access
         "/tmp/bar" "" (lambda (outcome) (push outcome received)))
        (should (equal '(approve) received)))))
  :doc "feedback reaches waiters but caches as denial"
  (with-temp-buffer
    (let (received prompt-callback)
      (cl-letf (((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (_root _reason callback)
                   (setq prompt-callback callback)))
                ((symbol-function 'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (&rest _args) nil)))
        (mevedel-tools--request-access
         "/tmp/feed" "" (lambda (outcome) (push outcome received)))
        (mevedel-tools--request-access
         "/tmp/feed" "" (lambda (outcome) (push outcome received)))
        (funcall prompt-callback '(feedback . "use git instead"))
        (should (= 2 (length received)))
        (setq received nil)
        (mevedel-tools--request-access
         "/tmp/feed" "" (lambda (outcome) (push outcome received)))
        (should (equal '(deny) received)))))
  :doc "abort and denial outcomes fan out unchanged"
  (dolist (outcome '(aborted deny))
    (with-temp-buffer
      (let (received prompt-callback)
        (cl-letf (((symbol-function 'mevedel--prompt-user-for-access)
                   (lambda (_root _reason callback)
                     (setq prompt-callback callback)))
                  ((symbol-function
                    'mevedel-workspace--file-in-allowed-roots-p)
                   (lambda (&rest _args) nil)))
          (mevedel-tools--request-access
           "/tmp/outcome" "" (lambda (value) (push value received)))
          (mevedel-tools--request-access
           "/tmp/outcome" "" (lambda (value) (push value received)))
          (funcall prompt-callback outcome)
          (should (equal (list outcome outcome) received))))))
  :doc "already allowed roots bypass the prompt"
  (with-temp-buffer
    (let ((calls 0)
          received)
      (cl-letf (((symbol-function
                  'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (_root _buffer) "/tmp/allowed/"))
                ((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (&rest _args) (cl-incf calls))))
        (mevedel-tools--request-access
         "/tmp/allowed" "" (lambda (outcome) (push outcome received))))
      (should (= 0 calls))
      (should (equal '(approve) received)))))


;;
;;; Diagnostics and pipeline

(mevedel-deftest mevedel-tools--request-access--log ()
  ,test
  (test)
  :doc "records prompt creation and resolution"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-access-log-" t)))
         (session (mevedel-session--create :name "main" :save-path dir))
         (buffer (generate-new-buffer " *mevedel-access-data*"))
         callback)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session)
            (cl-letf (((symbol-function
                        'mevedel-workspace--file-in-allowed-roots-p)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'mevedel--prompt-user-for-access)
                       (lambda (_root _reason value-callback)
                         (setq callback value-callback)
                         (let ((overlay (make-overlay (point-min) (point-min))))
                           (overlay-put overlay 'mevedel-view-interaction-id
                                        'request-id)
                           overlay))))
              (mevedel-tools--request-access "/tmp/outside" "inspect" #'ignore)))
          (funcall callback 'deny)
          (let ((entries (test-tool-access--read-permission-log session)))
            (should (equal '(request-access-created request-access-resolved)
                           (mapcar (lambda (entry) (plist-get entry :event))
                                   entries)))
            (should (eq 'request-id
                        (plist-get (car entries) :interaction-id)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory dir t))))

(mevedel-deftest mevedel-tool-access--request ()
  ,test
  (test)
  :doc "pipeline returns an access grant after approval"
  (let* ((outside (make-temp-file "mevedel-access-outside-" t))
         callback
         result)
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-workspace--file-in-allowed-roots-p)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'mevedel--prompt-user-for-access)
                   (lambda (_root _reason value-callback)
                     (setq callback value-callback)))
                  ((symbol-function 'mevedel-add-project-root) #'ignore))
          (mevedel-tool-access--request
           (lambda (value) (setq result value))
           (list :directory outside :reason "inspect"))
          (funcall callback 'approve)
          (should (equal
                   (list :result
                         (format "Access granted to %s. You can now read and write files in this directory."
                                 (expand-file-name outside)))
                   result)))
      (delete-directory outside t))))


;;
;;; Result formatting

(mevedel-deftest mevedel-tools--request-access--format-result ()
  ,test
  (test)
  :doc "formats approval, denial, feedback, and abort outcomes"
  (progn
    (should (string-match-p
             "Access granted to /tmp/x\\."
             (mevedel-tools--request-access--format-result "/tmp/x" 'approve)))
    (should (string-match-p
             "Access denied to /tmp/x\\."
             (mevedel-tools--request-access--format-result "/tmp/x" 'deny)))
    (should (string-match-p
             "Feedback: use git"
             (mevedel-tools--request-access--format-result
              "/tmp/x" '(feedback . "use git"))))
    (should (equal "Error: aborted"
                   (mevedel-tools--request-access--format-result
                    "/tmp/x" 'aborted)))))

(provide 'test-mevedel-tool-access)
;;; test-mevedel-tool-access.el ends here
