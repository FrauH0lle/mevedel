;;; niwa-state-cleanup.el --- Referenced workspace artifact retention -*- lexical-binding: t -*-

;;; Commentary:

;; Collect old generated media and review packages only after a complete search
;; of retained workspace state and this client's buffers.  Historical snapshots
;; count as references too.  Live foreign session owners postpone collection
;; because their unsaved input is invisible here; legacy pre-lease control
;; state also refuses collection.  Plugin data belongs to its plugin;
;; journal recovery and rotating diagnostics have their own retention owners.
;; The hourly cleanup opportunity, driven by turns and the idle maintenance
;; timer in `niwa-journal-idle', runs this collection.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ring)
(require 'niwa-session-control-fs)
(require 'niwa-session-durability)
(require 'niwa-structs)

;; `gptel'
(defvar gptel-context)

;; `niwa-utilities'
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")

;; `niwa-view-history'
(defvar niwa-view-history--ring)
(defvar niwa-view-history--stored-incomplete)

(defun niwa-state-cleanup--candidates (root now)
  "Return old generated files and their attributes below ROOT.
NOW is target time.  Keep symlinks, unknown names, and files over 32 MiB."
  (let (candidates)
    (dolist (spec '(("media" . "\\`\\(?:clipboard\\|guest\\)-[0-9]\\{8\\}-[0-9]\\{6\\}\\(?:-[0-9]+\\)?\\.png\\'")
                    ("review-packages" . "\\`review-[[:alnum:]]+\\.md\\'")))
      (dolist (path (niwa-session-control-fs-list-directory
                     (file-name-concat root "state" (car spec)) (cdr spec)))
        (let ((attrs (file-attributes path)))
          (when (and attrs (null (file-attribute-type attrs))
                     (<= (file-attribute-size attrs) (* 32 1024 1024))
                     (< (float-time (file-attribute-modification-time attrs))
                        (- now (* 7 86400))))
            (push (cons path attrs) candidates)))))
    (nreverse candidates)))

(defun niwa-state-cleanup--check-owners (root)
  "Refuse cleanup when a foreign session under ROOT can hold unsaved input."
  (dolist (directory (niwa-session-control-fs-list-directory
                      (file-name-concat root "sessions") "\\`[^.]"))
    (when (file-directory-p directory)
      (niwa-session-durability--assert-no-pid-lock directory)
      (when (file-directory-p (file-name-concat directory ".lease"))
        (let ((records (niwa-session-durability--read-records
                        (niwa-session-durability--generation-paths
                         (file-name-concat directory ".lease")))))
          (unless (cl-every #'niwa-session-durability--valid-lease-p records)
            (error "Unreadable session ownership prevents artifact cleanup: %s" directory))
          (unless (memq (niwa-session-durability--lease-state-of
                         directory (niwa-session-durability--head-of-records records))
                        '(owned available expired))
            (error "Foreign session owner prevents artifact cleanup: %s" directory)))))))

(defun niwa-state-cleanup--references (root names)
  "Return referenced NAMES after a complete target-side search under ROOT.
Signal on unavailable search or unreadable state.  Search hidden, ignored,
binary and linked files, including old publication generations."
  (let ((default-directory (file-name-as-directory root)))
    (with-temp-buffer
      (let ((status
             (apply #'process-file "rg" nil (list t nil) nil
                    (append '("--no-config" "--text" "--hidden" "--no-ignore"
                              "--follow" "--fixed-strings" "--only-matching"
                              "--no-filename" "--no-line-number" "--color=never"
                              "--glob=!state/media/**"
                              "--glob=!state/review-packages/**"
                              "--glob=!state/diagnostics/**"
                              "--glob=!media/**" "--glob=!review-packages/**"
                              "--glob=!diagnostics/**")
                            (cl-mapcan (lambda (name) (list "-e" name)) names)
                            '("--" ".")))))
        (unless (memq status '(0 1))
          (error "Workspace artifact reference search failed (%s)" status))
        (delete-dups (split-string (buffer-string) "\n" t))))))

(defun niwa-state-cleanup--live-reference-p (name)
  "Return non-nil when a live buffer or gptel context references NAME."
  (cl-some
   (lambda (buffer)
     (with-current-buffer buffer
       (or (and buffer-file-name (string-search name buffer-file-name))
           (and (boundp 'niwa-view-history--ring)
                (ring-p niwa-view-history--ring)
                (cl-some (lambda (text) (string-search name text))
                         (ring-elements niwa-view-history--ring)))
           (and (boundp 'niwa-view-history--stored-incomplete)
                (stringp niwa-view-history--stored-incomplete)
                (string-search name niwa-view-history--stored-incomplete))
           (and (boundp 'gptel-context)
                (cl-some (lambda (entry)
                           (and (stringp (car entry))
                                (string-search name (car entry))))
                         gptel-context))
           (save-restriction
             (widen)
             (save-excursion
               (goto-char (point-min))
               (search-forward name nil t))))))
   (buffer-list)))

;;;###autoload
(defun niwa-state-cleanup (workspace)
  "Collect one batch of unreferenced generated artifacts for WORKSPACE.
Files must be at least seven days old.  Saved workspace state, historical
snapshots, input history, live buffers and gptel context retain referenced
files.  Foreign session ownership or an incomplete scan retains the batch.
Return a deletion count, or nil on failure with a diagnostic message.
Called by the hourly-throttled cleanup opportunity, which completed root
turns and the idle maintenance timer schedule."
  (condition-case err
      (let* ((root (file-name-concat (niwa-workspace-root workspace) ".niwa"))
             (candidates
              (and (file-directory-p root)
                   (niwa-state-cleanup--candidates
                    root (niwa-session-control-fs-target-time root))))
             (deleted 0))
        (when candidates
          (let ((referenced (niwa-state-cleanup--references
                             root (mapcar (lambda (entry)
                                            (file-name-nondirectory (car entry)))
                                          candidates))))
            (setq candidates
                  (cl-remove-if
                   (lambda (candidate)
                     (let ((name (file-name-nondirectory (car candidate))))
                       (or (member name referenced)
                           (niwa-state-cleanup--live-reference-p name))))
                   candidates))
            (when candidates (niwa-state-cleanup--check-owners root))
            (dolist (candidate candidates)
              (let* ((path (car candidate))
                     (name (file-name-nondirectory path)))
                (unless (or (>= deleted 100) (member name referenced)
                            (niwa-state-cleanup--live-reference-p name))
                  (let ((bytes (niwa-session-control-fs-read-file
                                path 'no-conversion (1+ (* 32 1024 1024)))))
                    ;; Reading can update atime; compare mtime, ctime, size,
                    ;; modes and identity instead.
                    (when (equal (nthcdr 5 (cdr candidate))
                                 (nthcdr 5 (file-attributes path)))
                      (niwa-session-control-fs-program-value
                       (car (last (niwa-session-control-fs-run-program
                                   (list (list :op 'verify :path path :content bytes)
                                         (list :op 'delete-file :path path))
                                   (directory-file-name (file-name-directory path))))))
                      (cl-incf deleted))))))))
        deleted)
    (error
     (niwa-diagnostic-message (niwa-diagnostic-scope workspace) "artifact cleanup postponed: %s" (error-message-string err))
     nil)))

(provide 'niwa-state-cleanup)
;;; niwa-state-cleanup.el ends here
