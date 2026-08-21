;;; test-mevedel-plugin-lifecycle.el -- Plugin lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests plugin install, update, and removal ownership with real directories.

;;; Code:

(require 'mevedel-plugin-lifecycle)
(require 'mevedel-plugin-registry)
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

(mevedel-deftest mevedel-plugins-install
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-plugin-install-" t)))
           (mevedel-user-dir root)
           (mevedel-plugin-install-directory
            (file-name-concat root ".agents" "plugins")))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "refuses to overwrite an installed plugin destination"
  (let ((plugin-root
         (mevedel-plugins-test--github-install-root "owner" "repo")))
    (mevedel-plugins-test--write-manifest
     plugin-root "{\"name\":\"demo\"}")
    (should (equal
             "Plugin demo is already installed; use /plugin update demo."
             (mevedel-plugins-install "owner/repo"))))

  :doc "a clone with no manifest leaves nothing at the destination"
  (let ((dest (mevedel-plugins-test--github-install-root "owner" "repo"))
        (mevedel-plugins-git-executor
         (lambda (_dir args)
           ;; git writes the tree it was asked for, without a manifest.
           (make-directory (car (last args)) t)
           (list 0 ""))))
    (should (equal
             (concat "Failed to install plugin owner/repo: no Codex plugin "
                     "manifest found.")
             (mevedel-plugins-install "owner/repo")))
    (should-not (file-exists-p dest))
    (should-not (mevedel-plugins-test--staging-leftovers dest)))

  :doc "a clone with a malformed manifest leaves nothing behind"
  (let ((dest (mevedel-plugins-test--github-install-root "owner" "repo"))
        (mevedel-plugins-git-executor
         (lambda (_dir args)
           (mevedel-plugins-test--write-manifest
            (car (last args)) "{not json")
           (list 0 ""))))
    (should (string-prefix-p "Failed to install plugin owner/repo"
                             (mevedel-plugins-install "owner/repo")))
    (should-not (file-exists-p dest))
    (should-not (mevedel-plugins-test--staging-leftovers dest)))

  :doc "a clone with an unsafe manifest name leaves nothing behind"
  (let ((dest (mevedel-plugins-test--github-install-root "owner" "repo"))
        (mevedel-plugins-git-executor
         (lambda (_dir args)
           (mevedel-plugins-test--write-manifest
            (car (last args)) "{\"name\":\"../evil\"}")
           (list 0 ""))))
    (should (string-prefix-p "Failed to install plugin owner/repo"
                             (mevedel-plugins-install "owner/repo")))
    (should-not (file-exists-p dest))
    (should-not (mevedel-plugins-test--staging-leftovers dest)))

  :doc "a failed clone that already wrote a valid manifest is not adopted"
  (let ((dest (mevedel-plugins-test--github-install-root "owner" "repo"))
        (mevedel-plugins-git-executor
         (lambda (_dir args)
           (mevedel-plugins-test--write-manifest
            (car (last args)) "{\"name\":\"demo\"}")
           (list 1 "fatal: early EOF"))))
    (should (equal "Failed to install plugin owner/repo: fatal: early EOF"
                   (mevedel-plugins-install "owner/repo")))
    ;; Left in place it would be discoverable, enableable, and unremovable.
    (should-not (file-exists-p dest))
    (should-not (mevedel-plugins-test--staging-leftovers dest)))

  :doc "a non-directory at the destination is reported, not signalled"
  (let ((dest (mevedel-plugins-test--github-install-root "owner" "repo"))
        (mevedel-plugins-git-executor
         (lambda (_dir args)
           (mevedel-plugins-test--write-manifest
            (car (last args)) "{\"name\":\"demo\"}")
           (list 0 ""))))
    ;; The already-installed guard tests for a directory, so a plain file here
    ;; reaches the publish step; every other failure path returns a string.
    (make-directory (file-name-directory dest) t)
    (with-temp-file dest (insert "not a plugin"))
    (should (string-match-p "already exists"
                            (mevedel-plugins-install "owner/repo")))
    (should-not (mevedel-plugins-test--staging-leftovers dest)))

  :doc "a failed install can be retried once its cause is fixed"
  (let ((mevedel-plugins-git-executor
         (lambda (_dir args)
           (make-directory (car (last args)) t)
           (list 0 ""))))
    (should (string-prefix-p "Failed to install plugin owner/repo"
                             (mevedel-plugins-install "owner/repo")))
    (let ((mevedel-plugins-git-executor
           (lambda (_dir args)
             (mevedel-plugins-test--write-manifest
              (car (last args)) "{\"name\":\"demo\"}")
             (list 0 ""))))
      (should (equal "Installed plugin demo."
                     (mevedel-plugins-install "owner/repo"))))))

(mevedel-deftest mevedel-plugins-update
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-plugin-update-" t)))
           (plugin-root (file-name-concat root "external"))
           (mevedel-user-dir root)
           (mevedel-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (mevedel-plugin-extra-roots (list plugin-root)))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "leaves an unmanaged plugin for its external owner to update"
  (progn
    (mevedel-plugins-test--write-manifest
     plugin-root "{\"name\":\"demo\"}")
    (should (equal
             (format
              "Plugin demo is not managed by mevedel; update %s manually."
              (file-name-as-directory plugin-root))
             (mevedel-plugins-update "demo")))))

(mevedel-deftest mevedel-plugins-remove
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-plugin-remove-" t)))
           (mevedel-user-dir root)
           (mevedel-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (plugin-root (mevedel-plugins-test--plugin-root root "demo")))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "deletes an approved plugin under the managed install root"
  (progn
    (mevedel-plugins-test--write-manifest
     plugin-root "{\"name\":\"demo\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (should (equal "Removed plugin demo."
                     (mevedel-plugins-remove "demo"))))
    (should-not (file-exists-p plugin-root))))

(provide 'test-mevedel-plugin-lifecycle)
;;; test-mevedel-plugin-lifecycle.el ends here
