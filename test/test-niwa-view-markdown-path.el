;;; test-niwa-view-markdown-path.el --- Markdown path target tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-structs)
(require 'niwa-view-markdown)
(require 'niwa-workspace)

(niwa-deftest niwa-view--path-target ()
  ,test
  (test)

  :doc "an unverified remote path is queued, not decorated, and never stated"
  (with-temp-buffer
    (add-hook 'kill-buffer-hook #'niwa-view-path-teardown nil t)
    (let ((stats 0))
      (niwa-test--with-captured-diagnostics nil
        (cl-letf (((symbol-function 'file-exists-p)
                   (lambda (&rest _) (setq stats (1+ stats)) t)))
          (should-not (niwa-view--path-target
                       "/niwamock:host:/srv/project/main.py"))
          (should (= 0 stats))))
      (should (member "/niwamock:host:/srv/project/main.py"
                      niwa-view--path-pending))))

  :doc "a remote path verified present is decorated on the next redraw"
  (with-temp-buffer
    (setq niwa-view--path-existence (make-hash-table :test #'equal))
    (puthash "/niwamock:host:/srv/project/main.py"
             (cons t (float-time))
             niwa-view--path-existence)
    (should (eq t (niwa-view--path-target
                   "/niwamock:host:/srv/project/main.py")))
    (should-not niwa-view--path-pending))

  :doc "a workspace-cached remote path is decorated without waiting"
  (let* ((workspace (niwa-workspace--create
                     :type 'project :id "/niwamock:host:/srv/project/"
                     :root "/niwamock:host:/srv/project/" :name "fc"
                     :file-cache (niwa-test-file-cache-create)))
         (session (niwa-session-create "main" workspace))
         (path "/niwamock:host:/srv/project/cached.py"))
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-file-cache-get)
                 (lambda (_cache probed) (equal probed path))))
        (should (eq t (niwa-view--path-target path))))
      (should-not niwa-view--path-pending)))

  :doc "a local path is still decided exactly, since it costs no connection"
  (let* ((root (make-temp-file "niwa-pt-" t))
         (present (file-name-concat root "there.el"))
         (absent (file-name-concat root "gone.el")))
    (unwind-protect
        (with-temp-buffer
          (write-region "" nil present nil 'silent)
          (should (eq t (niwa-view--path-target present)))
          (should-not (niwa-view--path-target absent))
          (should-not niwa-view--path-pending))
      (delete-directory root t)))

  :doc "a memoized absent remote path stays undecorated and is not requeued"
  (with-temp-buffer
    (setq niwa-view--path-existence (make-hash-table :test #'equal))
    (puthash "/niwamock:host:/srv/project/gone.py"
             (cons nil (float-time))
             niwa-view--path-existence)
    (should-not (niwa-view--path-target
                 "/niwamock:host:/srv/project/gone.py"))
    (should-not niwa-view--path-pending))

  :doc "an absence memo expires so a file written later is re-verified"
  (with-temp-buffer
    (add-hook 'kill-buffer-hook #'niwa-view-path-teardown nil t)
    (setq niwa-view--path-existence (make-hash-table :test #'equal))
    (puthash "/niwamock:host:/srv/project/late.py"
             (cons nil (- (float-time) (1+ niwa-view--path-absence-ttl)))
             niwa-view--path-existence)
    (should-not (niwa-view--path-target
                 "/niwamock:host:/srv/project/late.py"))
    (should (member "/niwamock:host:/srv/project/late.py"
                    niwa-view--path-pending))))

(provide 'test-niwa-view-markdown-path)
;;; test-niwa-view-markdown-path.el ends here
