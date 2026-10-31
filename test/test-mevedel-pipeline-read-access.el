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
(require 'mevedel-tool-fs)
(require 'mevedel-workspace)
(require 'mevedel-hooks)
(require 'mevedel-permission-queue)

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

(defun mevedel-test--read-through-pipeline (session directory path)
  "Run the real Read tool for PATH in SESSION from DIRECTORY.
Return (:queued QUEUED :result RESULT); a queued prompt is left unanswered."
  (let (queued result)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (_event _payload callback &rest _) (funcall callback nil)))
              ((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session) (setq queued entry))))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (setq default-directory directory)
        (mevedel-pipeline-run-tool
         (mevedel-tool-get "Read") (lambda (value) (setq result value))
         (list :file_path path))
        (let ((deadline (+ (float-time) 5)))
          (while (and (not (or queued result)) (< (float-time) deadline))
            (accept-process-output nil 0.02)))))
    (list :queued (and queued t) :result result)))

(mevedel-deftest mevedel-tool-permission-paths ()
  ,test
  (test)
  :doc "Read through a workspace symlink into a protected tree asks, never allows"
  (let* ((top (file-truename (make-temp-file "mevedel-read-symlink-" t)))
         (outside (file-name-concat top "outside"))
         (ws (file-name-as-directory (file-name-concat top "ws")))
         (workspace nil)
         (mevedel-permission-rules nil)
         ;; A decoy protected tree; no real key material is involved.
         (mevedel-protected-paths `((,(concat outside "/.ssh/**") . inaccessible))))
    (unwind-protect
        (progn
          (mevedel-tool-fs--register)
          (make-directory (file-name-concat outside ".ssh") t)
          (make-directory ws t)
          (setq workspace (mevedel-workspace-get-or-create 'project ws ws "ws"))
          (write-region "DECOY-KEY" nil (file-name-concat outside ".ssh/key") nil 'silent)
          (write-region "PLAIN" nil (file-name-concat ws "plain.txt") nil 'silent)
          (make-symbolic-link (file-name-concat outside ".ssh") (file-name-concat ws "s"))
          (dolist (mode '(edits ask))
            (let* ((mevedel-permission-mode mode)
                   (session (mevedel-session-create "main" workspace)))
              (setf (mevedel-session-permission-mode session) mode)
              (dolist (path (list (file-name-concat ws "s/key")
                                  (concat "/:" (file-name-concat ws "s/key"))
                                  (file-name-concat outside ".ssh/key")))
                (let ((run (mevedel-test--read-through-pipeline session ws path)))
                  ;; Asked, or refused outright for a quoted `/:' name.
                  (should (or (plist-get run :queued)
                              (string-prefix-p "Error:" (plist-get run :result))))
                  (should-not (string-match-p "DECOY-KEY"
                                              (format "%s" (plist-get run :result))))))
              ;; Negative control: an ordinary workspace file is read unasked.
              (let ((run (mevedel-test--read-through-pipeline
                          session ws (file-name-concat ws "plain.txt"))))
                (should-not (plist-get run :queued))
                (should (string-match-p "PLAIN" (format "%s" (plist-get run :result))))))))
      (delete-directory top t))))

(provide 'test-mevedel-pipeline-read-access)
;;; test-mevedel-pipeline-read-access.el ends here
