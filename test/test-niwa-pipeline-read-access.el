;;; test-niwa-pipeline-read-access.el --- Read-access arguments through the tool pipeline -*- lexical-binding: t; -*-

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
(require 'niwa-pipeline)
(require 'niwa-resource)
(require 'niwa-structs)
(require 'niwa-tool-registry)
(require 'niwa-tool-fs)
(require 'niwa-workspace)
(require 'niwa-hooks)
(require 'niwa-permission-queue)
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))

(defun niwa-test--run-read-access-tool (args spec session)
  "Run a mutating tool declaring ARG SPEC with ARGS in SESSION.
Return a plist with the `:attempt' its handler saw for the source address."
  (let (seen result)
    (cl-letf (((symbol-function 'niwa-hooks-run-event)
               (lambda (_event _payload callback &rest _) (funcall callback nil)))
              ((symbol-function 'niwa-check-permission-async-with-metadata)
               (lambda (_name callback &rest _)
                 (funcall callback '(:outcome allow :raw-outcome allow)))))
      (with-temp-buffer
        (setq-local niwa--session session)
        (niwa-pipeline-run-tool
         (niwa-tool--create
          :name "ReadsOne" :read-only-p nil :groups '(edit) :async-p nil
          :args (list spec)
          :handler (lambda (handler-args)
                     (let ((source (plist-get handler-args :source)))
                       (setq seen (list :attempt (niwa-resource-current-attempt source))))
                     '(:result "done")))
         (lambda (value) (setq result value))
         args)))
    (should (string-prefix-p "done" result))
    seen))

(niwa-deftest niwa-pipeline--step-prepare-resources
  (:doc "A read-access argument's resource address is prepared for reading")
  ,test
  (test)
  :doc "prepares a resource address for reading, but only when the argument declares it"
  (let* ((save-path (make-temp-file "niwa-read-access-resource-" t))
         (session (niwa-session--create :name "read-access" :save-path save-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat save-path "local") t)
          (should (plist-get
                   (niwa-test--run-read-access-tool
                    '(:source "work://plot.png")
                    '(source path-or-resource :required "S" :access read) session)
                   :attempt))
          (should-not (plist-get
                       (niwa-test--run-read-access-tool
                        '(:source "work://plot.png")
                        '(source path-or-resource :required "S") session)
                       :attempt)))
      (delete-directory save-path t)))
  :doc "rejects an address read cannot serve before permission or the handler"
  (let* ((session (niwa-session--create :name "read-access" ))
         result handled)
    (cl-letf (((symbol-function 'niwa-hooks-run-event)
               (lambda (_event _payload callback &rest _) (funcall callback nil))))
      (with-temp-buffer
        (setq-local niwa--session session)
        (niwa-pipeline-run-tool
         (niwa-tool--create
          :name "ReadsOne" :read-only-p nil :groups '(edit) :async-p nil
          :args '((source path-or-resource :required "S" :access read))
          :handler (lambda (_) (setq handled t) '(:result "done")))
         (lambda (value) (setq result value))
         '(:source "work://notes/../secret"))))
    (should (string-match-p "Error:" result))
    (should-not handled)))

(defun niwa-test--read-through-pipeline (session directory path)
  "Run the real Read tool for PATH in SESSION from DIRECTORY.
Return (:queued QUEUED :result RESULT); a queued prompt is left unanswered."
  (let (queued result)
    (cl-letf (((symbol-function 'niwa-hooks-run-event)
               (lambda (_event _payload callback &rest _) (funcall callback nil))))
      (with-temp-buffer
        (setq-local niwa--session session)
        (setq default-directory directory)
        (niwa-pq-test--with-view-here view
          (niwa-pipeline-run-tool
           (niwa-tool-get "Read") (lambda (value) (setq result value))
           (list :file_path path))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not (or (niwa-pq-test--cards view) result))
                        (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (setq queued (and (niwa-pq-test--cards view) t))
          ;; Leave the prompt unanswered, and keep nothing on the session.
          (let ((settled result))
            (niwa-permission-queue-abort-all session)
            (setq result settled)))))
    (list :queued queued :result result)))

(niwa-deftest niwa-tool-permission-paths ()
  ,test
  (test)
  :doc "Read through a workspace symlink into a protected tree asks, never allows"
  (let* ((top (file-truename (make-temp-file "niwa-read-symlink-" t)))
         (outside (file-name-concat top "outside"))
         (ws (file-name-as-directory (file-name-concat top "ws")))
         (workspace nil)
         (niwa-permission-rules nil)
         ;; A decoy protected tree; no real key material is involved.
         (niwa-protected-paths `((,(concat outside "/.ssh/**") . inaccessible))))
    (unwind-protect
        (progn
          (niwa-tool-fs--register)
          (make-directory (file-name-concat outside ".ssh") t)
          (make-directory ws t)
          (setq workspace (niwa-workspace-get-or-create 'project ws ws "ws"))
          (write-region "DECOY-KEY" nil (file-name-concat outside ".ssh/key") nil 'silent)
          (write-region "PLAIN" nil (file-name-concat ws "plain.txt") nil 'silent)
          (make-symbolic-link (file-name-concat outside ".ssh") (file-name-concat ws "s"))
          (dolist (mode '(edits ask))
            (let* ((niwa-permission-mode mode)
                   (session (niwa-session-create "main" workspace)))
              (setf (niwa-session-permission-mode session) mode)
              (dolist (path (list (file-name-concat ws "s/key")
                                  (concat "/:" (file-name-concat ws "s/key"))
                                  (file-name-concat outside ".ssh/key")))
                (let ((run (niwa-test--read-through-pipeline session ws path)))
                  ;; Asked, or refused outright for a quoted `/:' name.
                  (should (or (plist-get run :queued)
                              (string-prefix-p "Error:" (plist-get run :result))))
                  (should-not (string-match-p "DECOY-KEY"
                                              (format "%s" (plist-get run :result))))))
              ;; Negative control: an ordinary workspace file is read unasked.
              (let ((run (niwa-test--read-through-pipeline
                          session ws (file-name-concat ws "plain.txt"))))
                (should-not (plist-get run :queued))
                (should (string-match-p "PLAIN" (format "%s" (plist-get run :result))))))))
      (delete-directory top t))))

(provide 'test-niwa-pipeline-read-access)
;;; test-niwa-pipeline-read-access.el ends here
