;;; test-mevedel-plugin-owner-load.el -- Cold plugin owner test -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies compiled plugin owners without loading the mevedel umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-plugins-list/cold-load ()
  ,test
  (test)
  :doc "loads compiled facade, registry, lifecycle, and UI owners directly"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-plugin-owner-" t))
         (plugin-home (file-name-concat compiled-root "home"))
         (extra-root (file-name-concat compiled-root "external"))
         (plugin-root (file-name-concat extra-root "demo"))
         (manifest-dir (file-name-concat plugin-root ".codex-plugin"))
         (manifest-file (file-name-concat manifest-dir "plugin.json"))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("mevedel-plugin-lifecycle.el"
                   "mevedel-plugin-registry.el"
                   "mevedel-plugin-ui.el"
                   "mevedel-plugins.el")))
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
                        (require 'mevedel-structs)
                        (require 'mevedel-plugin-registry)
                        (require 'mevedel-plugin-lifecycle)
                        (require 'mevedel-plugins)
                        (require 'mevedel-plugin-ui)
                        (let* ((mevedel-user-dir ,plugin-home)
                               (mevedel-plugin-install-directory
                                ,(file-name-concat
                                  plugin-home ".agents" "plugins"))
                               (mevedel-plugin-extra-roots
                                (list ,extra-root))
                               (workspace
                                (mevedel-workspace--create
                                 :type 'file
                                 :id ,plugin-home
                                 :root ,plugin-home
                                 :name "test")))
                          (unless (equal '("demo")
                                         (mapcar
                                          #'mevedel-plugin-name
                                          (mevedel-plugins-list workspace)))
                           (error "Registry owner did not discover the plugin"))
                          (unless (string-prefix-p
                                   "Plugin demo is not managed by mevedel;"
                                   (mevedel-plugins-update "demo" workspace))
                            (error "Lifecycle owner did not use the registry"))
                          (unless (null (mevedel-plugins-skill-dirs
                                         workspace))
                            (error "Facade returned unexpected skill roots"))
                          (with-temp-buffer
                            (setq-local mevedel--workspace workspace)
                            (let ((result (mevedel-plugins-slash-command
                                           "update demo")))
                              (unless (string-prefix-p
                                       "Plugin demo is not managed by mevedel;"
                                       result)
                                (error
                                 "UI owner did not use the lifecycle: %S"
                                 result))))
                          (let ((view (generate-new-buffer " *plugin-view*"))
                                (data (generate-new-buffer " *plugin-data*")))
                            (unwind-protect
                                (let ((buffer
                                       (mevedel-plugins-list-open
                                        (list :view-buffer view
                                              :data-buffer data
                                              :origin-buffer view
                                              :workspace workspace))))
                                  (unless (with-current-buffer buffer
                                            (derived-mode-p
                                             'mevedel-plugins-list-mode))
                                    (error "UI owner did not open its cockpit"))
                                  (kill-buffer buffer))
                              (kill-buffer view)
                              (kill-buffer data)))
                          (when (featurep 'mevedel)
                            (error "Plugin owners loaded the mevedel umbrella"))))))))
              (unless (zerop status)
                (error "Cold plugin owner failed: %s" (buffer-string))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-plugin-owner-load)
;;; test-mevedel-plugin-owner-load.el ends here
