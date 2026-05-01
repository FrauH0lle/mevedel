;;; mevedel-view-history.el --- Input history for mevedel view buffers -*- lexical-binding: t -*-

;;; Commentary:

;; Comint-style input history for `mevedel-view-mode', implemented on
;; top of `ring.el' without inheriting from `comint-mode'.  The view
;; buffer remains an ephemeral projection of the data buffer; this
;; module owns only the editable input region's history ring and its
;; optional per-session persistence sidecar.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ring)
(require 'subr-x)

;; `mevedel-structs'
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)

;; `mevedel-view'
(declare-function mevedel-view--clear-input "mevedel-view" ())
(declare-function mevedel-view--input-start "mevedel-view" ())
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-read
                  "mevedel-session-persistence" (path))
(declare-function mevedel-session-persistence-write
                  "mevedel-session-persistence" (path plist))
(defvar mevedel-session-persistence)
(defvar mevedel-session--read-only-mode)


;;
;;; Customization

(defcustom mevedel-view-input-history-size 500
  "Maximum number of entries kept in the view buffer's input ring."
  :type 'integer
  :group 'mevedel)


;;
;;; State

(defvar-local mevedel-view-history--ring nil
  "Input history ring for the current view buffer.")

(defvar-local mevedel-view-history--index nil
  "Current index while navigating `mevedel-view-history--ring'.")

(defvar-local mevedel-view-history--stored-incomplete nil
  "Input text saved before history navigation began.")

(defvar-local mevedel-view-history--save-failed nil
  "Non-nil after an input-history save failure in this view buffer.")

(defvar-local mevedel-view-history--load-warned nil
  "Non-nil after warning about a corrupt input-history file.")


;;
;;; Ring helpers

(defun mevedel-view-history--ensure-ring ()
  "Return the current view buffer's input history ring."
  (unless (ring-p mevedel-view-history--ring)
    (setq mevedel-view-history--ring
          (make-ring (max 1 mevedel-view-input-history-size))))
  mevedel-view-history--ring)

(defun mevedel-view-history--entries ()
  "Return history entries newest first."
  (let ((ring (mevedel-view-history--ensure-ring))
        entries)
    (dotimes (i (ring-length ring))
      (push (ring-ref ring i) entries))
    (nreverse entries)))

(defun mevedel-view-history--input-active-p ()
  "Return non-nil when point is in the editable view input region."
  (and (derived-mode-p 'mevedel-view-mode)
       (not (bound-and-true-p mevedel-view--agent-transcript-p))
       (boundp 'mevedel--data-buffer)
       mevedel--data-buffer
       (ignore-errors
         (>= (point) (mevedel-view--input-start)))))

(defun mevedel-view-history--input-text ()
  "Return current view input text, untrimmed."
  (buffer-substring-no-properties (mevedel-view--input-start) (point-max)))

(defun mevedel-view-history--replace-input (text)
  "Replace the current view input with TEXT."
  (let ((inhibit-read-only t))
    (mevedel-view--clear-input)
    (goto-char (mevedel-view--input-start))
    (insert text)
    (goto-char (point-max))))

(defun mevedel-view-history-add (input)
  "Add INPUT to the current view buffer's input history.
INPUT is trimmed before insertion.  Empty entries and consecutive
duplicates are skipped.  History navigation state is reset."
  (let ((entry (string-trim (or input "")))
        (ring (mevedel-view-history--ensure-ring)))
    (setq mevedel-view-history--index nil
          mevedel-view-history--stored-incomplete nil)
    (unless (or (string-empty-p entry)
                (and (> (ring-length ring) 0)
                     (string= entry (ring-ref ring 0))))
      (ring-insert ring entry))))


;;
;;; Persistence

(defun mevedel-view-history--session ()
  "Return the session associated with the current view buffer."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-view-history--read-only-p ()
  "Return non-nil when the associated data buffer is read-only."
  (and (boundp 'mevedel--data-buffer)
       (buffer-live-p mevedel--data-buffer)
       (buffer-local-value 'mevedel-session--read-only-mode
                           mevedel--data-buffer)))

(defun mevedel-view-history--path (&optional session)
  "Return SESSION's input-history sidecar path, or nil."
  (when-let* ((sess (or session (mevedel-view-history--session)))
              (save-path (mevedel-session-save-path sess)))
    (file-name-concat save-path "input-history.el")))

(defun mevedel-view-history--rename-bad-file (path)
  "Rename corrupt input history PATH to PATH.bad when possible."
  (when (file-exists-p path)
    (let ((bad (concat path ".bad")))
      (condition-case err
          (progn
            (when (file-exists-p bad)
              (delete-file bad))
            (rename-file path bad))
        (error
         (display-warning
          'mevedel
          (format "Could not rename corrupt input history %s: %s"
                  path (error-message-string err))
          :warning))))))

(defun mevedel-view-history-load (&optional session)
  "Load SESSION's input history into the current view buffer.
Missing history files are normal and produce an empty ring.  Corrupt
files are renamed to `.bad', warned about once, and ignored."
  (setq mevedel-view-history--ring
        (make-ring (max 1 mevedel-view-input-history-size))
        mevedel-view-history--index nil
        mevedel-view-history--stored-incomplete nil)
  (when-let* ((path (and (bound-and-true-p mevedel-session-persistence)
                         (mevedel-view-history--path session))))
    (when (file-exists-p path)
      (condition-case err
          (let* ((plist (mevedel-session-persistence-read path))
                 (entries (plist-get plist :entries)))
            (unless (and (equal 1 (plist-get plist :version))
                         (listp entries)
                         (cl-every #'stringp entries))
              (error "Malformed input-history sidecar"))
            ;; File format is newest-first; insert oldest-to-newest so
            ;; ring-ref 0 remains the newest entry.
            (dolist (entry (reverse entries))
              (ring-insert mevedel-view-history--ring entry)))
        (error
         (unless mevedel-view-history--load-warned
           (setq mevedel-view-history--load-warned t)
           (display-warning
            'mevedel
            (format "Input history unreadable at %s: %s; starting empty"
                    path (error-message-string err))
            :warning))
         (mevedel-view-history--rename-bad-file path))))))

(defun mevedel-view-history-save (&optional view-buffer)
  "Persist VIEW-BUFFER's input history when session persistence allows it."
  (let ((buf (or view-buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when-let* (((bound-and-true-p mevedel-session-persistence))
                    ((not mevedel-view-history--save-failed))
                    ((not (mevedel-view-history--read-only-p)))
                    (path (mevedel-view-history--path))
                    ((file-directory-p (file-name-directory path))))
          (condition-case err
              (mevedel-session-persistence-write
               path
               (list :version 1
                     :entries (mevedel-view-history--entries)))
            (error
             (setq mevedel-view-history--save-failed t)
             (display-warning
              'mevedel
              (format "Input history save failed at %s: %s"
                      path (error-message-string err))
              :warning))))))))

(defun mevedel-view-history-copy-file (parent-save-path new-save-path)
  "Copy input history from PARENT-SAVE-PATH to NEW-SAVE-PATH if present."
  (let ((src (and parent-save-path
                  (file-name-concat parent-save-path "input-history.el")))
        (dst (and new-save-path
                  (file-name-concat new-save-path "input-history.el"))))
    (when (and src dst (file-exists-p src))
      (condition-case err
          (copy-file src dst t)
        (error
         (display-warning
          'mevedel
          (format "Fork: failed to copy input history: %s"
                  (error-message-string err))
          :warning))))))


;;
;;; Commands

(defun mevedel-view-history-previous ()
  "Cycle backward through view input history when in the input area."
  (interactive)
  (if (not (mevedel-view-history--input-active-p))
      (call-interactively #'backward-sentence)
    (let ((ring (mevedel-view-history--ensure-ring)))
      (when (ring-empty-p ring)
        (user-error "No input history"))
      (unless mevedel-view-history--index
        (setq mevedel-view-history--stored-incomplete
              (mevedel-view-history--input-text)))
      (setq mevedel-view-history--index
            (if mevedel-view-history--index
                (min (1- (ring-length ring))
                     (1+ mevedel-view-history--index))
              0))
      (mevedel-view-history--replace-input
       (ring-ref ring mevedel-view-history--index)))))

(defun mevedel-view-history-next ()
  "Cycle forward through view input history when in the input area."
  (interactive)
  (if (not (mevedel-view-history--input-active-p))
      (call-interactively #'forward-sentence)
    (cond
     ((null mevedel-view-history--index)
      (user-error "Not browsing input history"))
     ((> mevedel-view-history--index 0)
      (setq mevedel-view-history--index
            (1- mevedel-view-history--index))
      (mevedel-view-history--replace-input
       (ring-ref (mevedel-view-history--ensure-ring)
                 mevedel-view-history--index)))
     (t
      (setq mevedel-view-history--index nil)
      (mevedel-view-history--replace-input
       (or mevedel-view-history--stored-incomplete ""))
      (setq mevedel-view-history--stored-incomplete nil)))))

(defun mevedel-view-history-search ()
  "Search backward through input history and replace the input with a match."
  (interactive)
  (if (not (mevedel-view-history--input-active-p))
      (call-interactively #'move-to-window-line-top-bottom)
    (let* ((stored (mevedel-view-history--input-text))
           (regexp (read-regexp "History search: "))
           (match (cl-find-if (lambda (entry)
                                (string-match-p regexp entry))
                              (mevedel-view-history--entries))))
      (if match
          (progn
            (setq mevedel-view-history--stored-incomplete stored
                  mevedel-view-history--index nil)
            (mevedel-view-history--replace-input match))
        (user-error "No matching input history")))))

(defun mevedel-view-history-browse ()
  "Browse input history and insert the selected entry into the input area."
  (interactive)
  (let ((entries (mevedel-view-history--entries)))
    (when (null entries)
      (user-error "No input history"))
    (let ((choice (completing-read "Input history: " entries nil t)))
      (goto-char (mevedel-view--input-start))
      (mevedel-view-history--replace-input choice))))

(defun mevedel-view-history-clear-input ()
  "Clear current input when point is in the input region."
  (interactive)
  (when (mevedel-view-history--input-active-p)
    (mevedel-view--clear-input)
    (goto-char (mevedel-view--input-start))))

(defun mevedel-view-history-beginning-of-line (&optional arg)
  "Move to input start on the prompt line, otherwise run normal BOL.
ARG is forwarded to `move-beginning-of-line' on the normal path."
  (interactive "^p")
  (if (and (mevedel-view-history--input-active-p)
           (save-excursion
             (let ((input-start (mevedel-view--input-start)))
               (and (> (point) input-start)
                    (<= (line-beginning-position) input-start)
                    (<= input-start (line-end-position))))))
      (goto-char (mevedel-view--input-start))
    (move-beginning-of-line arg)))

(provide 'mevedel-view-history)
;;; mevedel-view-history.el ends here
