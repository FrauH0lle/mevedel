;;; test-mevedel-view-markdown-path.el --- Markdown path target tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-structs)
(require 'mevedel-view-markdown)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-view--path-target ()
  ,test
  (test)

  :doc "an unverified remote path is queued, not decorated, and never stated"
  (with-temp-buffer
    (add-hook 'kill-buffer-hook #'mevedel-view-path-teardown nil t)
    (let ((stats 0))
      (mevedel-test--with-captured-diagnostics nil
        (cl-letf (((symbol-function 'file-exists-p)
                   (lambda (&rest _) (setq stats (1+ stats)) t)))
          (should-not (mevedel-view--path-target
                       "/mevedelmock:host:/srv/project/main.py"))
          (should (= 0 stats))))
      (should (member "/mevedelmock:host:/srv/project/main.py"
                      mevedel-view--path-pending))))

  :doc "a remote path verified present is decorated on the next redraw"
  (with-temp-buffer
    (setq mevedel-view--path-existence (make-hash-table :test #'equal))
    (puthash "/mevedelmock:host:/srv/project/main.py"
             (cons t (float-time))
             mevedel-view--path-existence)
    (should (eq t (mevedel-view--path-target
                   "/mevedelmock:host:/srv/project/main.py")))
    (should-not mevedel-view--path-pending))

  :doc "a workspace-cached remote path is decorated without waiting"
  (let* ((workspace (mevedel-workspace--create
                     :type 'project :id "/mevedelmock:host:/srv/project/"
                     :root "/mevedelmock:host:/srv/project/" :name "fc"
                     :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session-create "main" workspace))
         (path "/mevedelmock:host:/srv/project/cached.py"))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-file-cache-get)
                 (lambda (_cache probed) (equal probed path))))
        (should (eq t (mevedel-view--path-target path))))
      (should-not mevedel-view--path-pending)))

  :doc "a local path is still decided exactly, since it costs no connection"
  (let* ((root (make-temp-file "mevedel-pt-" t))
         (present (file-name-concat root "there.el"))
         (absent (file-name-concat root "gone.el")))
    (unwind-protect
        (with-temp-buffer
          (write-region "" nil present nil 'silent)
          (should (eq t (mevedel-view--path-target present)))
          (should-not (mevedel-view--path-target absent))
          (should-not mevedel-view--path-pending))
      (delete-directory root t)))

  :doc "a memoized absent remote path stays undecorated and is not requeued"
  (with-temp-buffer
    (setq mevedel-view--path-existence (make-hash-table :test #'equal))
    (puthash "/mevedelmock:host:/srv/project/gone.py"
             (cons nil (float-time))
             mevedel-view--path-existence)
    (should-not (mevedel-view--path-target
                 "/mevedelmock:host:/srv/project/gone.py"))
    (should-not mevedel-view--path-pending))

  :doc "an absence memo expires so a file written later is re-verified"
  (with-temp-buffer
    (add-hook 'kill-buffer-hook #'mevedel-view-path-teardown nil t)
    (setq mevedel-view--path-existence (make-hash-table :test #'equal))
    (puthash "/mevedelmock:host:/srv/project/late.py"
             (cons nil (- (float-time) (1+ mevedel-view--path-absence-ttl)))
             mevedel-view--path-existence)
    (should-not (mevedel-view--path-target
                 "/mevedelmock:host:/srv/project/late.py"))
    (should (member "/mevedelmock:host:/srv/project/late.py"
                    mevedel-view--path-pending))))

(provide 'test-mevedel-view-markdown-path)
;;; test-mevedel-view-markdown-path.el ends here
