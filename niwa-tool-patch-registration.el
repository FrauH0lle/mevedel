;;; niwa-tool-patch-registration.el --- Patch tool catalog -*- lexical-binding: t -*-

;;; Commentary:

;; Complete discovery metadata; implementation loads at first use.

;;; Code:

(require 'niwa-tool-registry)

(autoload 'niwa-tool-patch--get-paths "niwa-tool-patch")
(autoload 'niwa-tool-patch--render "niwa-tool-patch")
(autoload 'niwa-tool-patch-handler "niwa-tool-patch")
(autoload 'niwa-tool-patch-parse "niwa-tool-patch")

(defun niwa-tool-patch-register ()
  "Register the ApplyPatch tool."
  (niwa-define-tool
    :name "ApplyPatch"
    :description "Apply one patch that may add, update, delete, or move files."
    :summary "Apply a multi-file patch."
    :prompt-file "prompts/tools/applypatch.md"
    :handler #'niwa-tool-patch-handler
    :args ((patch string :required
                  "A complete *** Begin Patch / *** End Patch patch."))
    :async-p t
    :groups (edit reviewed-edit)
    :snapshot-p t
    :get-paths #'niwa-tool-patch--get-paths
    :display-arg (lambda (args)
                   (let ((proposal
                          (ignore-errors
                            (niwa-tool-patch-parse
                             (plist-get args :patch)))))
                     (when proposal
                       (let ((count (length (plist-get proposal :operations))))
                         (format "%d %s" count
                                 (if (= count 1) "file" "files"))))))
    :renderer '((success . niwa-tool-patch--render))))

(provide 'niwa-tool-patch-registration)
;;; niwa-tool-patch-registration.el ends here
