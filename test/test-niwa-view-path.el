;;; test-niwa-view-path.el --- Deferred view path tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-structs)
(require 'niwa-view-path)
(require 'niwa-workspace)

(niwa-deftest niwa-view--path-known-p ()
  ,test
  (test)

  :doc "the workspace file cache answers without target I/O"
  (let* ((workspace (niwa-workspace--create
                     :type 'project :id "/niwamock:host:/srv/project/"
                     :root "/niwamock:host:/srv/project/" :name "fc"
                     :file-cache (niwa-test-file-cache-create)))
         (session (niwa-session-create "main" workspace))
         (path "/niwamock:host:/srv/project/cached.py"))
    (with-temp-buffer
      (setq-local niwa--session session)
      (should-not (niwa-view--path-known-p path))
      (cl-letf (((symbol-function 'niwa-file-cache-get)
                 (lambda (_cache probed) (equal probed path))))
        (should (niwa-view--path-known-p path))))))

(niwa-deftest niwa-view--verify-paths ()
  ,test
  (test)

  :doc "remote paths on one target share one existence command"
  (let* ((host "view-path-batch-host")
         (root (file-name-as-directory (make-temp-file "niwa-view-batch-" t)))
         (process (symbol-function 'process-file))
         (calls 0))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (write-region "" nil (file-name-concat root "a.el") nil 'silent)
          (write-region "" nil (file-name-concat root "b c.el") nil 'silent)
          (let ((remote (lambda (name) (format "/niwamock:%s:%s%s" host root name))))
            (cl-letf (((symbol-function 'process-file)
                       (lambda (&rest args) (cl-incf calls) (apply process args))))
              (should (equal '(t nil t nil)
                             (niwa-view--path-existences
                              (list (funcall remote "a.el")
                                    (funcall remote "missing.el")
                                    (funcall remote "b c.el")
                                    (file-name-concat root "local-missing.el"))))))
            (should (= 1 calls))))
      (delete-directory root t)))

  :doc "verification defers and memoizes both outcomes"
  (let* ((root (make-temp-file "niwa-view-paths-" t))
         (there (file-name-concat root "there.py"))
         (gone (file-name-concat root "gone.py")))
    (unwind-protect
        (with-temp-buffer
          (write-region "" nil there nil 'silent)
          (let ((buffer (current-buffer)) deferred-thunk rerendered)
            (setq niwa-view--path-pending (list gone there))
            (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                       (lambda (_key _path thunk &optional _on-cancel)
                         (setq deferred-thunk thunk)
                         t))
                      ((symbol-function 'niwa-view-rerender)
                       (lambda (&optional view) (setq rerendered view))))
              (niwa-view--verify-paths buffer)
              (should (functionp deferred-thunk))
              (should-not niwa-view--path-existence)
              (should (= 2 (length niwa-view--path-pending)))
              (funcall deferred-thunk)
              (should-not niwa-view--path-pending)
              (should (eq buffer rerendered))
              (should (car (gethash there niwa-view--path-existence)))
              (should-not (car (gethash gone
                                        niwa-view--path-existence))))))
      (delete-directory root t)))

  :doc "coalesced verification drains paths discovered while waiting"
  (let* ((root (make-temp-file "niwa-view-paths-" t))
         (first (file-name-concat root "first.py"))
         (second (file-name-concat root "second.py")))
    (unwind-protect
        (with-temp-buffer
          (write-region "" nil first nil 'silent)
          (write-region "" nil second nil 'silent)
          (let ((buffer (current-buffer)) deferred-thunk)
            (setq niwa-view--path-pending (list first))
            (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                       (lambda (_key _path thunk &optional _on-cancel)
                         (unless deferred-thunk (setq deferred-thunk thunk))
                         t))
                      ((symbol-function 'niwa-view-rerender) #'ignore))
              (niwa-view--verify-paths buffer)
              (push second niwa-view--path-pending)
              (niwa-view--verify-paths buffer)
              (should (= 2 (length niwa-view--path-pending)))
              (funcall deferred-thunk)
              (should-not niwa-view--path-pending)
              (should (car (gethash first niwa-view--path-existence)))
              (should (car (gethash second
                                    niwa-view--path-existence))))))
      (delete-directory root t))))

(niwa-deftest niwa-view-path-teardown ()
  ,test
  (test)

  :doc "cancels the buffer timer and keyed deferred transport work"
  (with-temp-buffer
    (let ((timer (run-at-time 60 nil #'ignore)) cancelled)
      (unwind-protect
          (progn
            (setq niwa-view--path-verify-timer timer)
            (cl-letf (((symbol-function 'niwa-transport-cancel-pending)
                       (lambda (key) (setq cancelled key))))
              (niwa-view-path-teardown))
            (should niwa-view--path-torn-down-p)
            (should-not niwa-view--path-verify-timer)
            (should-not (memq timer timer-list))
            (should (equal cancelled
                           (list 'view-path-verify (current-buffer))))
            (niwa-view--schedule-path-verification)
            (should-not niwa-view--path-verify-timer))
        (cancel-timer timer)))))

(provide 'test-niwa-view-path)
;;; test-niwa-view-path.el ends here
