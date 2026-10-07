;;; mevedel-tool-patch-registration.el --- Patch tool catalog -*- lexical-binding: t -*-

;;; Commentary:

;; Complete discovery metadata; implementation loads at first use.

;;; Code:

(require 'mevedel-tool-registry)

(autoload 'mevedel-tool-patch--get-paths "mevedel-tool-patch")
(autoload 'mevedel-tool-patch--render "mevedel-tool-patch")
(autoload 'mevedel-tool-patch-handler "mevedel-tool-patch")
(autoload 'mevedel-tool-patch-parse "mevedel-tool-patch")

(defun mevedel-tool-patch-register ()
  "Register the ApplyPatch tool."
  (mevedel-define-tool
    :name "ApplyPatch"
    :description "Apply one patch that may add, update, delete, or move files."
    :summary "Apply a multi-file patch."
    :prompt-file "prompts/tools/applypatch.md"
    :handler #'mevedel-tool-patch-handler
    :args ((patch string :required
                  "A complete *** Begin Patch / *** End Patch patch."))
    :async-p t
    :groups (edit reviewed-edit)
    :snapshot-p t
    :get-paths #'mevedel-tool-patch--get-paths
    :display-arg (lambda (args)
                   (let ((proposal
                          (ignore-errors
                            (mevedel-tool-patch-parse
                             (plist-get args :patch)))))
                     (when proposal
                       (let ((count (length (plist-get proposal :operations))))
                         (format "%d %s" count
                                 (if (= count 1) "file" "files"))))))
    :renderer '((success . mevedel-tool-patch--render))))

(provide 'mevedel-tool-patch-registration)
;;; mevedel-tool-patch-registration.el ends here
