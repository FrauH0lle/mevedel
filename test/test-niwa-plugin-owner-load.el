;;; test-niwa-plugin-owner-load.el -- Cold plugin owner test -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies compiled plugin owners without loading the niwa umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-plugins-list/cold-load ()
  ,test
  (test)
  :doc "loads compiled facade, registry, lifecycle, and UI owners directly"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "niwa"))))
         (compiled-root (make-temp-file "niwa-plugin-owner-" t))
         (plugin-home (file-name-concat compiled-root "home"))
         (extra-root (file-name-concat compiled-root "external"))
         (plugin-root (file-name-concat extra-root "demo"))
         (manifest-dir (file-name-concat plugin-root ".codex-plugin"))
         (manifest-file (file-name-concat manifest-dir "plugin.json"))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("niwa-plugin-lifecycle.el"
                   "niwa-plugin-registry.el"
                   "niwa-plugin-ui.el"
                   "niwa-plugins.el")))
    (unwind-protect
        (progn
          (make-directory manifest-dir t)
          (with-temp-file manifest-file
            (insert "{\"name\":\"demo\"}"))
          (dolist (owner owners)
            (copy-file (file-name-concat root owner)
                       (file-name-concat compiled-root owner))
            (let ((byte-compile-verbose nil))
              (byte-compile-file (file-name-concat compiled-root owner))))
          (with-temp-buffer
            (let ((status
                   (call-process
                    emacs nil t nil
                    "--batch" "-Q" "-L" compiled-root "-L" root
                    "--eval"
                    (prin1-to-string
                     `(progn
                        (require 'niwa-structs)
                        (require 'niwa-plugin-registry)
                        (require 'niwa-plugin-lifecycle)
                        (require 'niwa-plugins)
                        (require 'niwa-plugin-ui)
                        (let* ((niwa-user-dir ,plugin-home)
                               (niwa-plugin-install-directory
                                ,(file-name-concat
                                  plugin-home ".agents" "plugins"))
                               (niwa-plugin-extra-roots
                                (list ,extra-root))
                               (workspace
                                (niwa-workspace--create
                                 :type 'file
                                 :id ,plugin-home
                                 :root ,plugin-home
                                 :name "test")))
                          (unless (equal '("demo")
                                         (mapcar
                                          #'niwa-plugin-name
                                          (niwa-plugins-list workspace)))
                           (error "Registry owner did not discover the plugin"))
                          (unless (string-prefix-p
                                   "Plugin demo is not managed by niwa;"
                                   (niwa-plugins-update "demo" workspace))
                            (error "Lifecycle owner did not use the registry"))
                          (unless (null (niwa-plugins-skill-dirs
                                         workspace))
                            (error "Facade returned unexpected skill roots"))
                          (with-temp-buffer
                            (setq-local niwa--workspace workspace)
                            (let ((result (niwa-plugins-slash-command
                                           "update demo")))
                              (unless (string-prefix-p
                                       "Plugin demo is not managed by niwa;"
                                       result)
                                (error
                                 "UI owner did not use the lifecycle: %S"
                                 result))))
                          (let ((view (generate-new-buffer " *plugin-view*"))
                                (data (generate-new-buffer " *plugin-data*")))
                            (unwind-protect
                                (let ((buffer
                                       (niwa-plugins-list-open
                                        (list :view-buffer view
                                              :data-buffer data
                                              :origin-buffer view
                                              :workspace workspace))))
                                  (unless (with-current-buffer buffer
                                            (derived-mode-p
                                             'niwa-plugins-list-mode))
                                    (error "UI owner did not open its cockpit"))
                                  (kill-buffer buffer))
                              (kill-buffer view)
                              (kill-buffer data)))
                          (when (featurep 'niwa)
                            (error "Plugin owners loaded the niwa umbrella"))))))))
              (unless (zerop status)
                (error "Cold plugin owner failed: %s" (buffer-string))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-niwa-plugin-owner-load)
;;; test-niwa-plugin-owner-load.el ends here
