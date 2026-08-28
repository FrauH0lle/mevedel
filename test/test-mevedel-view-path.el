;;; test-mevedel-view-path.el --- Deferred view path tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-structs)
(require 'mevedel-view-path)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-view--path-known-p ()
  ,test
  (test)

  :doc "the workspace file cache answers without target I/O"
  (let* ((workspace (mevedel-workspace--create
                     :type 'project :id "/mevedelmock:host:/srv/project/"
                     :root "/mevedelmock:host:/srv/project/" :name "fc"
                     :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session-create "main" workspace))
         (path "/mevedelmock:host:/srv/project/cached.py"))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (should-not (mevedel-view--path-known-p path))
      (cl-letf (((symbol-function 'mevedel-file-cache-get)
                 (lambda (_cache probed) (equal probed path))))
        (should (mevedel-view--path-known-p path))))))

(mevedel-deftest mevedel-view--verify-paths ()
  ,test
  (test)

  :doc "verification defers and memoizes both outcomes"
  (let* ((root (make-temp-file "mevedel-view-paths-" t))
         (there (file-name-concat root "there.py"))
         (gone (file-name-concat root "gone.py")))
    (unwind-protect
        (with-temp-buffer
          (write-region "" nil there nil 'silent)
          (let ((buffer (current-buffer)) deferred-thunk rerendered)
            (setq mevedel-view--path-pending (list gone there))
            (cl-letf (((symbol-function 'mevedel-transport-run-when-idle)
                       (lambda (_key _path thunk &optional _on-cancel)
                         (setq deferred-thunk thunk)
                         t))
                      ((symbol-function 'mevedel-view-rerender)
                       (lambda (&optional view) (setq rerendered view))))
              (mevedel-view--verify-paths buffer)
              (should (functionp deferred-thunk))
              (should-not mevedel-view--path-existence)
              (should (= 2 (length mevedel-view--path-pending)))
              (funcall deferred-thunk)
              (should-not mevedel-view--path-pending)
              (should (eq buffer rerendered))
              (should (car (gethash there mevedel-view--path-existence)))
              (should-not (car (gethash gone
                                        mevedel-view--path-existence))))))
      (delete-directory root t)))

  :doc "coalesced verification drains paths discovered while waiting"
  (let* ((root (make-temp-file "mevedel-view-paths-" t))
         (first (file-name-concat root "first.py"))
         (second (file-name-concat root "second.py")))
    (unwind-protect
        (with-temp-buffer
          (write-region "" nil first nil 'silent)
          (write-region "" nil second nil 'silent)
          (let ((buffer (current-buffer)) deferred-thunk)
            (setq mevedel-view--path-pending (list first))
            (cl-letf (((symbol-function 'mevedel-transport-run-when-idle)
                       (lambda (_key _path thunk &optional _on-cancel)
                         (unless deferred-thunk (setq deferred-thunk thunk))
                         t))
                      ((symbol-function 'mevedel-view-rerender) #'ignore))
              (mevedel-view--verify-paths buffer)
              (push second mevedel-view--path-pending)
              (mevedel-view--verify-paths buffer)
              (should (= 2 (length mevedel-view--path-pending)))
              (funcall deferred-thunk)
              (should-not mevedel-view--path-pending)
              (should (car (gethash first mevedel-view--path-existence)))
              (should (car (gethash second
                                    mevedel-view--path-existence))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-view-path-teardown ()
  ,test
  (test)

  :doc "cancels the buffer timer and keyed deferred transport work"
  (with-temp-buffer
    (let ((timer (run-at-time 60 nil #'ignore)) cancelled)
      (unwind-protect
          (progn
            (setq mevedel-view--path-verify-timer timer)
            (cl-letf (((symbol-function 'mevedel-transport-cancel-pending)
                       (lambda (key) (setq cancelled key))))
              (mevedel-view-path-teardown))
            (should mevedel-view--path-torn-down-p)
            (should-not mevedel-view--path-verify-timer)
            (should-not (memq timer timer-list))
            (should (equal cancelled
                           (list 'view-path-verify (current-buffer))))
            (mevedel-view--schedule-path-verification)
            (should-not mevedel-view--path-verify-timer))
        (cancel-timer timer)))))

(provide 'test-mevedel-view-path)
;;; test-mevedel-view-path.el ends here
