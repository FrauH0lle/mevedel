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
             (mevedel-plugins-install "owner/repo")))))

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
