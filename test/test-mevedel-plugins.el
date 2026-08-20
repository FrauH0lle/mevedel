;;; test-mevedel-plugins.el -- Plugin facade tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests active-workspace resolution and session-facing plugin projections.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-plugin-registry)
(require 'mevedel-plugins)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-plugin-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-plugin-test-support"))

(defvar mevedel--session)
(defvar mevedel--workspace)

(mevedel-deftest mevedel-plugins-current-workspace ()
  ,test
  (test)
  :doc "resolves the active session workspace before the buffer workspace"
  (let* ((session-root (make-temp-file "mevedel-plugin-session-" t))
         (buffer-root (make-temp-file "mevedel-plugin-buffer-" t))
         (session-workspace (mevedel-plugins-test--workspace session-root))
         (buffer-workspace (mevedel-plugins-test--workspace buffer-root))
         (mevedel--session
          (mevedel-session-create "main" session-workspace session-root))
         (mevedel--workspace buffer-workspace))
    (unwind-protect
        (should (eq session-workspace
                    (mevedel-plugins-current-workspace)))
      (delete-directory session-root t)
      (delete-directory buffer-root t))))

(mevedel-deftest mevedel-plugins-refresh-session ()
  ,test
  (test)
  :doc "rescans the current session when the skill owner is loaded"
  (let (rescanned)
    (cl-letf (((symbol-function 'mevedel-skills-rescan)
               (lambda () (setq rescanned t))))
      (should (eq t (mevedel-plugins-refresh-session)))
      (should rescanned))))

(mevedel-deftest mevedel-plugins-skill-dirs
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-plugin-skills-" t)))
           (mevedel-user-dir root)
           (mevedel-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (mevedel-plugin-extra-roots nil)
           (workspace (mevedel-plugins-test--workspace root))
           (plugin-root (mevedel-plugins-test--plugin-root root "demo"))
           (skills-dir (file-name-concat plugin-root "skills")))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "projects enabled plugin skill directories with their source identity"
  (progn
    (make-directory skills-dir t)
    (mevedel-plugins-test--write-manifest
     plugin-root "{\"name\":\"demo\",\"skills\":\"skills\"}")
    (should (mevedel-plugins-enable "demo" workspace))
    (should (equal (list (cons (file-name-as-directory skills-dir)
                               '(plugin . "demo")))
                   (mevedel-plugins-skill-dirs workspace)))))

(provide 'test-mevedel-plugins)
;;; test-mevedel-plugins.el ends here
