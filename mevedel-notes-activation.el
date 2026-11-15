;;; mevedel-notes-activation.el --- Cheap note activation on file visits -*- lexical-binding: t -*-

;;; Commentary:
;; `mevedel-notes-global-mode' installs a visit hook with no package
;; dependencies.  Presentation loads `mevedel', so the user's configuration of
;; it applies first, for remembered instructions or a nonempty local workspace
;; store.  A local roots-only index queues cold remote restoration outside the
;; hook.

;;; Code:

;; `mevedel-instruction-registry'
(defvar mevedel--instruction-states)

;; `mevedel-persistence'
(declare-function mevedel-instruction-store-read-connected "mevedel-persistence"
                  (root))
(declare-function mevedel-instruction-store-visit "mevedel-persistence"
                  (file &optional key root))
(defvar mevedel--inhibit-file-patching)
(autoload 'mevedel-instruction-store-read-connected "mevedel-persistence")
(autoload 'mevedel-instruction-store-visit "mevedel-persistence")

;; `mevedel-structs'
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-workspace'
(defvar mevedel-workspace--registry)

(defvar mevedel-notes-activation--remote-index nil
  "Cached local index as (FILE . REMOTE-ROOTS), with no note content.")

(defvar mevedel-notes-activation--remote-attempted nil
  "Remote workspace roots already scheduled for this Emacs run.")

(defun mevedel-notes-activation--remote-roots ()
  "Return the locally remembered remote roots, reading the index once."
  (let ((file (expand-file-name
               "mevedel/remote-notes.el"
               (or (getenv "XDG_STATE_HOME") "~/.local/state/"))))
    (unless (equal file (car mevedel-notes-activation--remote-index))
      (setq mevedel-notes-activation--remote-index
            (cons file
                  (condition-case nil
                      (with-temp-buffer
                        (insert-file-contents file)
                        (let ((roots (read (current-buffer))))
                          (and (proper-list-p roots)
                               (seq-every-p
                                (lambda (root)
                                  (and (stringp root) (file-remote-p root nil 'never)
                                       (string-suffix-p "/" root)))
                                roots)
                               roots)))
                    (error nil)))))
    (cdr mevedel-notes-activation--remote-index)))

(defun mevedel-notes-remember-remote-root (root has-notes)
  "Remember remote ROOT locally when HAS-NOTES; otherwise forget it."
  (when (and (stringp root) (file-remote-p root nil 'never))
    (setq root (file-name-as-directory root))
    ;; shortcut: simultaneous writers can still race; lock if that recurs.
    (setq mevedel-notes-activation--remote-index nil)
    (let* ((roots (mevedel-notes-activation--remote-roots))
           (updated (if has-notes
                        (if (member root roots) roots (cons root roots))
                      (remove root roots))))
      (unless (equal roots updated)
        (let ((file (car mevedel-notes-activation--remote-index)))
          (make-directory (file-name-directory file) t)
          (let ((temporary (make-temp-file file)))
            (unwind-protect
                (progn
                  (with-temp-file temporary (prin1 updated (current-buffer)))
                  (rename-file temporary file t))
              (when (file-exists-p temporary) (delete-file temporary)))))
        (setcdr mevedel-notes-activation--remote-index updated)))))

(defun mevedel-notes-activation--remote-visit (file)
  "Schedule one background store read for FILE's closest indexed root."
  (let (closest)
    (dolist (root (mevedel-notes-activation--remote-roots))
      (when (and (string-prefix-p root file)
                 (> (length root) (length closest)))
        (setq closest root)))
    (when (and closest
               (not (member closest mevedel-notes-activation--remote-attempted)))
      (push closest mevedel-notes-activation--remote-attempted)
      (run-at-time
       0 nil (lambda ()
               (require 'mevedel)
               (mevedel-instruction-store-read-connected closest))))))

(defun mevedel-notes-activation--present (file &optional key root)
  "Present FILE's notes from KEY or ROOT after loading `mevedel'.
Loading it first runs the user's configuration of `mevedel', such as note
keybindings, before any note overlay exists."
  (require 'mevedel)
  (mevedel-instruction-store-visit file key root))

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
                 (throw 'found key)))))
       mevedel-workspace--registry))
    nil))

;;;###autoload
(defun mevedel-notes-file-visit ()
  "Restore remembered notes, discover local stores, or queue a remote read.
Indexed remote roots are read once in the background, never in this hook."
  (when (and buffer-file-name
             (not (bound-and-true-p mevedel--inhibit-file-patching)))
    (let* ((file buffer-file-name)
           (key (mevedel-notes-activation--cached-key file)))
      (cond
       (key (mevedel-notes-activation--present file key))
       ((file-remote-p file nil 'never) (mevedel-notes-activation--remote-visit file))
       (t
        (when-let* ((located (locate-dominating-file
                             (file-name-directory file) ".mevedel/instructions.el"))
                    ;; `locate-dominating-file' may abbreviate a HOME root.
                    (root (expand-file-name located)))
          (if-let* ((key (mevedel-notes-activation--cached-key file root)))
              (mevedel-notes-activation--present file key)
            (when-let* ((data
                         ;; A load hint only; persistence validates the record.
                         (condition-case nil
                             (with-temp-buffer
                               (insert-file-contents
                                (file-name-concat root ".mevedel/instructions.el"))
                               (read (current-buffer)))
                           (error nil)))
                        ((plist-get data :files)))
              (mevedel-notes-activation--present file nil root)))))))))

;;;###autoload
(define-minor-mode mevedel-notes-global-mode
  "Show notes in files as they are visited, without starting a chat.
Enabling it also shows notes in buffers already visiting files."
  :global t
  :group 'mevedel
  (if mevedel-notes-global-mode
      (progn
        (add-hook 'find-file-hook #'mevedel-notes-file-visit)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer (mevedel-notes-file-visit))))
    (remove-hook 'find-file-hook #'mevedel-notes-file-visit)))

(provide 'mevedel-notes-activation)
;;; mevedel-notes-activation.el ends here
