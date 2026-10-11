;;; test-mevedel-pipeline-read-access.el --- Read-access arguments through the tool pipeline -*- lexical-binding: t; -*-

;;; Commentary:

;; A `path-or-resource' argument declared `:access read' reaches the handler
;; as an authorized read: a resource address carries a prepared `read'
;; attempt even for a mutating tool.  An undeclared argument of the same
;; tool is not prepared.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-pipeline)
(require 'mevedel-resource)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)

(defun mevedel-test--run-read-access-tool (args spec session)
  "Run a mutating tool declaring ARG SPEC with ARGS in SESSION.
Return a plist with the `:attempt' its handler saw for the source address."
  (let (seen result)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (_event _payload callback &rest _) (funcall callback nil)))
              ((symbol-function 'mevedel-check-permission-async-with-metadata)
               (lambda (_name callback &rest _)
                 (funcall callback '(:outcome allow :raw-outcome allow)))))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-pipeline-run-tool
         (mevedel-tool--create
          :name "ReadsOne" :read-only-p nil :groups '(edit) :async-p nil
          :args (list spec)
          :handler (lambda (handler-args)
                     (let ((source (plist-get handler-args :source)))
                       (setq seen (list :attempt (mevedel-resource-current-attempt source))))
                     '(:result "done")))
         (lambda (value) (setq result value))
         args)))
    (should (string-prefix-p "done" result))
    seen))

(mevedel-deftest mevedel-pipeline--step-prepare-resources
  (:doc "A read-access argument's resource address is prepared for reading")
  ,test
  (test)
  :doc "prepares a resource address for reading, but only when the argument declares it"
  (let* ((save-path (make-temp-file "mevedel-read-access-resource-" t))
         (session (mevedel-session--create :name "read-access" :authority-mode 'pid-lock :save-path save-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat save-path "local") t)
          (should (plist-get
                   (mevedel-test--run-read-access-tool
                    '(:source "work://plot.png")
                    '(source path-or-resource :required "S" :access read) session)
                   :attempt))
          (should-not (plist-get
                       (mevedel-test--run-read-access-tool
                        '(:source "work://plot.png")
                        '(source path-or-resource :required "S") session)
                       :attempt)))
      (delete-directory save-path t)))
  :doc "rejects an address read cannot serve before permission or the handler"
  (let* ((session (mevedel-session--create :name "read-access" :authority-mode 'pid-lock))
         result handled)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (_event _payload callback &rest _) (funcall callback nil))))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-pipeline-run-tool
         (mevedel-tool--create
          :name "ReadsOne" :read-only-p nil :groups '(edit) :async-p nil
          :args '((source path-or-resource :required "S" :access read))
          :handler (lambda (_) (setq handled t) '(:result "done")))
         (lambda (value) (setq result value))
         '(:source "work://notes/../secret"))))
    (should (string-match-p "Error:" result))
    (should-not handled)))

(provide 'test-mevedel-pipeline-read-access)
;;; test-mevedel-pipeline-read-access.el ends here
