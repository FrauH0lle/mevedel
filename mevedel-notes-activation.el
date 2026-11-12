;;; mevedel-notes-activation.el --- Cheap note activation on file visits -*- lexical-binding: t -*-

;;; Commentary:
;; The always-available visit hook has no package dependencies.  Presentation
;; loads only for remembered instructions or a nonempty local workspace store.

;;; Code:

;; `mevedel-instruction-registry'
(defvar mevedel--instruction-states)

;; `mevedel-persistence'
(declare-function mevedel-instruction-store-visit "mevedel-persistence"
                  (file &optional key root))
(defvar mevedel--inhibit-file-patching)
(autoload 'mevedel-instruction-store-visit "mevedel-persistence")

;; `mevedel-structs'
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-workspace'
(defvar mevedel-workspace--registry)

(defun mevedel-notes-activation--cached-key (file &optional root)
  "Return FILE's remembered instruction key, or the key for exact ROOT.
All comparisons are lexical; a registered ancestor does not own a file."
  (catch 'found
    (when (and (not root) (boundp 'mevedel--instruction-states))
      (maphash
       (lambda (key state)
         (when (assoc file (plist-get state :instructions))
           (throw 'found key)))
       mevedel--instruction-states))
    (when (boundp 'mevedel-workspace--registry)
      (maphash
       (lambda (key workspace)
         (if root
             (let ((cached-root (mevedel-workspace-root workspace)))
               (when (and (stringp cached-root)
                          (equal root (file-name-as-directory cached-root)))
                 (throw 'found key)))
           (dolist (directive (mevedel-workspace-directives workspace))
             (when (equal file (plist-get (mevedel-directive-anchor directive) :file))
               (throw 'found key)))))
       mevedel-workspace--registry))
    nil))

;;;###autoload
(defun mevedel-notes-file-visit ()
  "Restore notes from memory or a local workspace store on file visit.
Never discover or observe a remote workspace.  Remote files use only
instructions already remembered by this Emacs."
  (when (and buffer-file-name
             (not (bound-and-true-p mevedel--inhibit-file-patching)))
    (let* ((file buffer-file-name)
           (key (mevedel-notes-activation--cached-key file)))
      (cond
       (key (mevedel-instruction-store-visit file key))
       ((not (file-remote-p file))
        (when-let* ((located (locate-dominating-file
                             (file-name-directory file) ".mevedel/instructions.el"))
                    ;; `locate-dominating-file' may abbreviate a HOME root.
                    (root (expand-file-name located)))
          (if-let* ((key (mevedel-notes-activation--cached-key file root)))
              (mevedel-instruction-store-visit file key)
            (when-let* ((data
                         ;; A load hint only; persistence validates the record.
                         (condition-case nil
                             (with-temp-buffer
                               (insert-file-contents
                                (file-name-concat root ".mevedel/instructions.el"))
                               (read (current-buffer)))
                           (error nil)))
                        ((or (plist-get data :files)
                             (plist-get data :directives))))
              (mevedel-instruction-store-visit file nil root)))))))))

;;;###autoload
(add-hook 'find-file-hook #'mevedel-notes-file-visit)

(provide 'mevedel-notes-activation)
;;; mevedel-notes-activation.el ends here
