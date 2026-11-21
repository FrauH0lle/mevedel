;;; test-niwa-plugins.el -- Plugin facade tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests active-workspace resolution and session-facing plugin projections.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'niwa-plugin-registry)
(require 'niwa-cockpit)
(require 'niwa-plugins)
(require 'niwa-structs)
(require 'niwa-skills-core)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-plugin-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-plugin-test-support"))

(defvar niwa--session)
(defvar niwa--workspace)

(niwa-deftest niwa-plugins-current-workspace ()
  ,test
  (test)
  :doc "resolves the active session workspace before the buffer workspace"
  (let* ((session-root (make-temp-file "niwa-plugin-session-" t))
         (buffer-root (make-temp-file "niwa-plugin-buffer-" t))
         (session-workspace (niwa-plugins-test--workspace session-root))
         (buffer-workspace (niwa-plugins-test--workspace buffer-root))
         (niwa--session
          (niwa-session-create "main" session-workspace session-root))
         (niwa--workspace buffer-workspace))
    (unwind-protect
        (should (eq session-workspace
                    (niwa-plugins-current-workspace)))
      (delete-directory session-root t)
      (delete-directory buffer-root t))))

(niwa-deftest niwa-plugins-refresh-session ()
  ,test
  (test)
  :doc "rescans the current session when the skill owner is loaded"
  (let (rescanned)
    (cl-letf (((symbol-function 'niwa-skills-rescan)
               (lambda () (setq rescanned t))))
      (should (eq t (niwa-plugins-refresh-session)))
      (should rescanned)))

  :doc "attributes refresh failures to the context, preserving the raw result"
  (let* ((workspace (niwa-workspace--create :root "/workspace/origin/"))
         (context (list :workspace workspace))
         (niwa--warn-once-table (make-hash-table :test #'equal))
         warnings)
    (cl-letf (((symbol-function 'niwa-cockpit-call-in-data)
               (lambda (&rest _) (error "Broken refresh")))
              ((symbol-function 'display-warning)
               (lambda (type text &optional level _buffer)
                 (push (list type text level) warnings))))
      (with-temp-buffer
        (setq-local niwa--workspace
                    (niwa-workspace--create :root "/workspace/other/"))
        (should (equal "Broken refresh"
                       (niwa-plugins-refresh-session context)))))
    (should (equal '((niwa
                     "niwa [/workspace/origin/]: Plugin registry refresh failed: Broken refresh"
                     :warning))
                   warnings))))

(niwa-deftest niwa-plugins-skill-dirs
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "niwa-plugin-skills-" t)))
           (niwa-user-dir root)
           (niwa-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (niwa-plugin-extra-roots nil)
           (workspace (niwa-plugins-test--workspace root))
           (plugin-root (niwa-plugins-test--plugin-root root "demo"))
           (skills-dir (file-name-concat plugin-root "skills")))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "projects enabled plugin skill directories with their source identity"
  (progn
    (make-directory skills-dir t)
    (niwa-plugins-test--write-manifest
     plugin-root "{\"name\":\"demo\",\"skills\":\"skills\"}")
    (should (niwa-plugins-enable "demo" workspace))
    (should (equal (list (cons (file-name-as-directory skills-dir)
                               '(plugin . "demo")))
                   (niwa-plugins-skill-dirs workspace)))))

(provide 'test-niwa-plugins)
;;; test-niwa-plugins.el ends here
