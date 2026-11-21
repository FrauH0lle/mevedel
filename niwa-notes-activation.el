;;; niwa-notes-activation.el --- Cheap note activation on file visits -*- lexical-binding: t -*-

;;; Commentary:
;; `niwa-notes-global-mode' installs a visit hook with no package
;; dependencies.  Presentation loads `niwa', so the user's configuration of
;; it applies first, for remembered instructions or a nonempty local workspace
;; store.  A local roots-only index queues cold remote restoration outside the
;; hook.

;;; Code:

;; `niwa-instruction-registry'
(defvar niwa--instruction-states)

;; `niwa-persistence'
(declare-function niwa-instruction-store-read-connected "niwa-persistence"
                  (root))
(declare-function niwa-instruction-store-visit "niwa-persistence"
                  (file &optional key root))
(defvar niwa--inhibit-file-patching)
(autoload 'niwa-instruction-store-read-connected "niwa-persistence")
(autoload 'niwa-instruction-store-visit "niwa-persistence")

;; `niwa-structs'
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)

;; `niwa-workspace'
(defvar niwa-workspace--registry)

(defvar niwa-notes-activation--remote-index nil
  "Cached local index as (FILE . REMOTE-ROOTS), with no note content.")

(defvar niwa-notes-activation--remote-attempted nil
  "Remote workspace roots already scheduled for this Emacs run.")

(defun niwa-notes-activation--remote-roots ()
  "Return the locally remembered remote roots, reading the index once."
  (let ((file (expand-file-name
               "niwa/remote-notes.el"
               (or (getenv "XDG_STATE_HOME") "~/.local/state/"))))
    (unless (equal file (car niwa-notes-activation--remote-index))
      (setq niwa-notes-activation--remote-index
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
    (cdr niwa-notes-activation--remote-index)))

(defun niwa-notes-remember-remote-root (root has-notes)
  "Remember remote ROOT locally when HAS-NOTES; otherwise forget it."
  (when (and (stringp root) (file-remote-p root nil 'never))
    (setq root (file-name-as-directory root))
    ;; shortcut: simultaneous writers can still race; lock if that recurs.
    (setq niwa-notes-activation--remote-index nil)
    (let* ((roots (niwa-notes-activation--remote-roots))
           (updated (if has-notes
                        (if (member root roots) roots (cons root roots))
                      (remove root roots))))
      (unless (equal roots updated)
        (let ((file (car niwa-notes-activation--remote-index)))
          (make-directory (file-name-directory file) t)
          (let ((temporary (make-temp-file file)))
            (unwind-protect
                (progn
                  (with-temp-file temporary (prin1 updated (current-buffer)))
                  (rename-file temporary file t))
              (when (file-exists-p temporary) (delete-file temporary)))))
        (setcdr niwa-notes-activation--remote-index updated)))))

(defun niwa-notes-activation--remote-visit (file)
  "Schedule one background store read for FILE's closest indexed root."
  (let (closest)
    (dolist (root (niwa-notes-activation--remote-roots))
      (when (and (string-prefix-p root file)
                 (> (length root) (length closest)))
        (setq closest root)))
    (when (and closest
               (not (member closest niwa-notes-activation--remote-attempted)))
      (push closest niwa-notes-activation--remote-attempted)
      (run-at-time
       0 nil (lambda ()
               (require 'niwa)
               (niwa-instruction-store-read-connected closest))))))

(defun niwa-notes-activation--present (file &optional key root)
  "Present FILE's notes from KEY or ROOT after loading `niwa'.
Loading it first runs the user's configuration of `niwa', such as note
keybindings, before any note overlay exists."
  (require 'niwa)
  (niwa-instruction-store-visit file key root))

(defun niwa-notes-activation--cached-key (file &optional root)
  "Return FILE's remembered instruction key, or the key for exact ROOT.
All comparisons are lexical; a registered ancestor does not own a file."
  (catch 'found
    (when (and (not root) (boundp 'niwa--instruction-states))
      (maphash
       (lambda (key state)
         (when (assoc file (plist-get state :instructions))
           (throw 'found key)))
       niwa--instruction-states))
    (when (boundp 'niwa-workspace--registry)
      (maphash
       (lambda (key workspace)
         (if root
             (let ((cached-root (niwa-workspace-root workspace)))
               (when (and (stringp cached-root)
                          (equal root (file-name-as-directory cached-root)))
                 (throw 'found key)))))
       niwa-workspace--registry))
    nil))

;;;###autoload
(defun niwa-notes-file-visit ()
  "Restore remembered notes, discover local stores, or queue a remote read.
Indexed remote roots are read once in the background, never in this hook."
  (when (and buffer-file-name
             (not (bound-and-true-p niwa--inhibit-file-patching)))
    (let* ((file buffer-file-name)
           (key (niwa-notes-activation--cached-key file)))
      (cond
       (key (niwa-notes-activation--present file key))
       ((file-remote-p file nil 'never) (niwa-notes-activation--remote-visit file))
       (t
        (when-let* ((located (locate-dominating-file
                             (file-name-directory file) ".niwa/instructions.el"))
                    ;; `locate-dominating-file' may abbreviate a HOME root.
                    (root (expand-file-name located)))
          (unless (niwa-notes-activation--cached-key file root)
            (when-let* ((data
                         ;; A load hint only; persistence validates the record.
                         (condition-case nil
                             (with-temp-buffer
                               (insert-file-contents
                                (file-name-concat root ".niwa/instructions.el"))
                               (read (current-buffer)))
                           (error nil)))
                        ((plist-get data :files)))
              (niwa-notes-activation--present file nil root)))))))))

;;;###autoload
(define-minor-mode niwa-notes-global-mode
  "Show notes in files as they are visited, without starting a chat.
Enabling it also shows notes in buffers already visiting files."
  :global t
  :group 'niwa
  (if niwa-notes-global-mode
      (progn
        (add-hook 'find-file-hook #'niwa-notes-file-visit)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer (niwa-notes-file-visit))))
    (remove-hook 'find-file-hook #'niwa-notes-file-visit)))

(provide 'niwa-notes-activation)
;;; niwa-notes-activation.el ends here
