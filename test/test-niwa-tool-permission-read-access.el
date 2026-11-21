;;; test-niwa-tool-permission-read-access.el --- Read-only path arguments of mutating tools -*- lexical-binding: t; -*-

;;; Commentary:

;; A `path-or-resource' argument declared `:access read' is authorized as a
;; read, however mutating the tool is, while paths the tool touches in its
;; own right keep the access their mutability implies.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-permission-persistence)
(require 'niwa-permission-queue)
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))
(require 'niwa-permissions)
(require 'niwa-structs)
(require 'niwa-tool-permission)
(require 'niwa-tool-registry)

(defun niwa-test--read-access-tool (&optional get-path)
  "Return a mutating edit tool whose `source' argument it only reads.
GET-PATH, when non-nil, names a path the tool also writes."
  (niwa-tool--create
   :name "ReadsOne" :read-only-p nil :groups '(edit)
   :args '((source path-or-resource :required "Source" :access read))
   :get-path get-path))

(niwa-deftest niwa-tool-permission-read-paths
  ()
  ,test
  (test)
  :doc "returns only plain-path values of read-access arguments"
  (let ((tool (niwa-tool--create
               :name "Mixed"
               :args '((source path-or-resource :optional "S" :access read)
                       (other path-or-resource :optional "O")
                       (label string :optional "L" :access read)))))
    (should (equal '("/tmp/a.png")
                   (niwa-tool-permission-read-paths
                    tool '(:source "/tmp/a.png" :other "/tmp/b.png" :label "x"))))
    (should-not (niwa-tool-permission-read-paths tool '(:source "work://plot.png")))
    (should-not (niwa-tool-permission-read-paths tool '(:source "")))
    (should-not (niwa-tool-permission-read-paths tool nil))
    (should-not (niwa-tool-permission-read-paths tool '(:other "/tmp/b.png")))))

(niwa-deftest niwa-tool-permission--entries
  (:doc "Read-access paths are authorized as reads, tool-wide paths as the tool's own access"
   :before-each
   (advice-add 'display-warning :around #'niwa-test--drop-sessionless-permission-warning)
   :after-each
   (advice-remove 'display-warning #'niwa-test--drop-sessionless-permission-warning))
  ,test
  (test)
  :doc "allows an unprotected path outside the workspace in edits mode, but asks for the same path as a write"
  (let* ((dir (make-temp-file "niwa-read-access-" t))
         (niwa-user-dir (file-name-concat dir "global"))
         (workspace (niwa-workspace--create :type 'project :root dir))
         (session (niwa-session--create :workspace workspace :permission-mode 'edits))
         (source "/srv/niwa-outside/plot.png")
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         allowed)
    (unwind-protect
        (niwa-pq-test--with-session-view session view
          (niwa-tool-permission-step
           (list :tool (niwa-test--read-access-tool)
                 :session session :workspace workspace :args (list :source source))
           (lambda (_) (setq allowed t)) #'ignore)
          (should allowed)
          (should-not (niwa-pq-test--requested-p session))
          ;; The same path as a tool-wide write asks.
          (setq allowed nil)
          (niwa-tool-permission-step
           (list :tool (niwa-test--read-access-tool
                        (lambda (args) (plist-get args :source)))
                 :session session :workspace workspace
                 :args (list :source source))
           (lambda (_) (setq allowed t)) #'ignore)
          (should (niwa-pq-test--card view))
          (should-not allowed))
      (delete-directory dir t)))
  :doc "asks for a protected path until a read grant covers it, which a write path would not accept"
  (let* ((dir (make-temp-file "niwa-read-protected-" t))
         (niwa-user-dir (file-name-concat dir "global"))
         (workspace (niwa-workspace--create :type 'project :root dir))
         (source "/srv/niwa-outside/secret.png")
         (session (niwa-session--create :workspace workspace :permission-mode 'edits))
         (niwa-permission-rules nil)
         (niwa-protected-paths (list (cons source 'inaccessible)))
         allowed)
    (unwind-protect
        (niwa-pq-test--with-session-view session view
          (let ((context (list :tool (niwa-test--read-access-tool)
                               :session session :workspace workspace
                               :args (list :source source))))
            (niwa-tool-permission-step
             context (lambda (_) (setq allowed t)) #'ignore)
            (should (niwa-pq-test--card view))
            (should-not allowed)
            (setf (niwa-session-resource-grants session)
                  (list (list :path source :access 'read)))
            (niwa-permission-queue-abort-all session)
            (niwa-pq-test--forget-requests session)
            (niwa-tool-permission-step
             context (lambda (_) (setq allowed t)) #'ignore)
            (should allowed)
            (should-not (niwa-pq-test--requested-p session))
            ;; A read grant is not authority to write the path.
            (setq allowed nil)
            (niwa-tool-permission-step
             (plist-put (copy-sequence context) :tool
                        (niwa-test--read-access-tool
                         (lambda (args) (plist-get args :source))))
             (lambda (_) (setq allowed t)) #'ignore)
            (should (niwa-pq-test--card view))
            (should-not allowed)))
      (delete-directory dir t)))
  :doc "in ask mode the prompt offers a read grant for a read-access path and a write grant for a written one"
  (let* ((dir (make-temp-file "niwa-read-prompt-" t))
         (niwa-user-dir (file-name-concat dir "global"))
         (workspace (niwa-workspace--create :type 'project :root dir))
         (session (niwa-session--create :workspace workspace :permission-mode 'ask))
         (source "/srv/niwa-outside/plot.png")
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         )
    (unwind-protect
        (niwa-pq-test--with-session-view session view
          (dolist (get-path (list nil (lambda (args) (plist-get args :source))))
            (niwa-tool-permission-step
             (list :tool (niwa-test--read-access-tool get-path)
                   :session session :workspace workspace :args (list :source source))
             #'ignore #'ignore))
          ;; Both requests are queued behind the one visible card.
          (should (= 1 (length (niwa-pq-test--cards view))))
          (should (= 2 (length (niwa-session-permission-queue session))))
          (pcase-let ((`(,written ,read)
                       (reverse (niwa-session-permission-queue session))))
            (should (eq 'read (plist-get read :resource-access)))
            (should (eq 'read (plist-get (caar (plist-get read :resource-selection-cell)) :access)))
            (should (eq 'write (plist-get written :resource-access)))
            (should (eq 'write (plist-get (caar (plist-get written :resource-selection-cell)) :access)))))
      (delete-directory dir t)))
  :doc "a path named as both a write path and a read path is authorized once, as a write"
  (let* ((tool (niwa-test--read-access-tool (lambda (args) (plist-get args :source))))
         (context (list :tool tool :args '(:source "/tmp/both.png"))))
    (should (equal '(("/tmp/both.png"))
                   (niwa-tool-permission--entries tool context)))
    (should (equal '(("/tmp/only.png" . read))
                   (niwa-tool-permission--entries
                    (niwa-test--read-access-tool)
                    (list :args '(:source "/tmp/only.png")))))
    (should-not (niwa-tool-permission--entries
                 (niwa-test--read-access-tool)
                 (list :args '(:source "work://plot.png"))))))

(provide 'test-niwa-tool-permission-read-access)
;;; test-niwa-tool-permission-read-access.el ends here
