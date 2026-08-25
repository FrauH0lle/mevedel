;;; mevedel-view-history.el --- Input history for mevedel view buffers -*- lexical-binding: t -*-

;;; Commentary:

;; Comint-style input history for `mevedel-view-mode', implemented on
;; top of `ring.el' without inheriting from `comint-mode'.  The view
;; buffer remains an ephemeral projection of the data buffer; this
;; module owns only the editable composer's history ring and its
;; optional workspace-level persistence sidecar.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'subr-x)
(require 'mevedel-mention-bindings)
(require 'mevedel-utilities)
(require 'mevedel-transport)

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-copy-text
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-valid-p
                  "mevedel-mention-bindings" (text))

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-read
                  "mevedel-session-codec" (path))
(declare-function mevedel-session-codec-write
                  "mevedel-session-codec" (path plist))
(autoload 'mevedel-session-codec-read "mevedel-session-codec")
(autoload 'mevedel-session-codec-write "mevedel-session-codec")

;; `mevedel-session-control-fs'
(declare-function mevedel-session-control-fs-physical-path
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-read-file
                  "mevedel-session-control-fs" (path &optional coding-system))
(autoload 'mevedel-session-control-fs-physical-path
  "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-read-file "mevedel-session-control-fs")

;; `mevedel-session-persistence'
(defvar mevedel-session--read-only-mode)

;; `mevedel-structs'
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)

;; `mevedel-view'
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-view-agent'
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--clear-input "mevedel-view-composer" ())
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(declare-function mevedel-view-abort "mevedel-view-composer" ())

;; `mevedel-workspace'
(declare-function mevedel-workspace-ensure-generated-state-ignored
                  "mevedel-workspace" (workspace))
(declare-function mevedel-workspace-state-dir "mevedel-workspace" (workspace))
(autoload 'mevedel-workspace-state-dir "mevedel-workspace")


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

(defvar-local mevedel-view-history--loaded-entries nil
  "Newest-first input history entries last loaded or saved in this view.")


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
  (ring-elements (mevedel-view-history--ensure-ring)))

(defun mevedel-view-history--input-active-p ()
  "Return non-nil when point is in the editable composer."
  (and (derived-mode-p 'mevedel-view-mode)
       (not (bound-and-true-p mevedel-view--agent-transcript-p))
       (boundp 'mevedel--data-buffer)
       mevedel--data-buffer
       (ignore-errors
         (>= (point) (mevedel-view--input-start)))))

(defun mevedel-view-history--input-text ()
  "Return current view input text, untrimmed."
  (mevedel-mention-bindings-copy-text
   (buffer-substring (mevedel-view--input-start) (point-max))))

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
  (let ((entry (string-trim
                (mevedel-mention-bindings-copy-text
                 (mevedel--normalize-message-text (or input "")))))
        (ring (mevedel-view-history--ensure-ring)))
    (setq mevedel-view-history--index nil
          mevedel-view-history--stored-incomplete nil)
    (unless (string-empty-p entry)
      (when (and (> (ring-length ring) 0)
                 (string= entry (ring-ref ring 0)))
        (ring-remove ring 0))
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
  "Return SESSION's workspace input-history path, or nil."
  (when-let* ((sess (or session (mevedel-view-history--session)))
              (workspace (mevedel-session-workspace sess)))
    (file-name-concat (mevedel-workspace-state-dir workspace)
                      "input-history.el")))

(defun mevedel-view-history--call-global-key (key)
  "Invoke KEY's global binding, or report it as undefined.
Used by history keys outside the editable composer so the view mode
does not substitute its own navigation behavior there."
  (let ((cmd (lookup-key (current-global-map) key)))
    (if (commandp cmd)
        (call-interactively cmd)
      (user-error "%s is undefined" (key-description key)))))

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

(defun mevedel-view-history--validate-entries (plist)
  "Return newest-first history entries from PLIST.
Signal an error when PLIST is not valid input-history data."
  (let ((entries (plist-get plist :entries)))
    (unless (and (equal 2 (plist-get plist :version))
                 (listp entries)
                 (cl-every
                  (lambda (entry)
                    (and (stringp entry)
                         (mevedel-mention-bindings-valid-p entry)))
                  entries))
      (error "Malformed input history"))
    (mapcar (lambda (entry)
              (mevedel-mention-bindings-copy-text
               (mevedel--normalize-message-text entry)))
            entries)))

(defun mevedel-view-history--new-since-load (current loaded)
  "Return CURRENT entries added since LOADED was captured.
CURRENT and LOADED are newest-first.  History rings drop older entries,
so the unchanged part of CURRENT is the longest suffix that matches a
prefix of LOADED."
  (let ((count 0)
        found)
    (while (and (not found)
                (<= count (length current)))
      (let ((tail (nthcdr count current)))
        (when (and (<= (length tail) (length loaded))
                   (equal-including-properties
                    tail (take (length tail) loaded)))
          (setq found t)))
      (unless found
        (setq count (1+ count))))
    (if found
        (take count current)
      current)))

(defun mevedel-view-history--merge-entries (current existing &optional loaded)
  "Merge newest-first CURRENT and EXISTING history entries.
LOADED is the newest-first history previously loaded into this view.
Entries added to CURRENT since LOADED win recency ties, followed by
EXISTING, then CURRENT's older entries.  Duplicate strings are kept at
their first occurrence and the result is capped to
`mevedel-view-input-history-size'."
  (let ((seen (make-hash-table :test #'equal))
        entries)
    (dolist (entry (append (mevedel-view-history--new-since-load
                            current loaded)
                           existing
                           current))
      (when (and (stringp entry)
                 (not (string-empty-p entry))
                 (not (gethash entry seen)))
        (puthash entry t seen)
        (push entry entries)))
    (take mevedel-view-input-history-size (nreverse entries))))

(defun mevedel-view-history--set-entries (entries)
  "Replace the current buffer's history ring with newest-first ENTRIES."
  (setq mevedel-view-history--ring
        (make-ring (max 1 mevedel-view-input-history-size))
        mevedel-view-history--loaded-entries entries)
  (dolist (entry (reverse entries))
    (ring-insert mevedel-view-history--ring entry)))

(defun mevedel-view-history-load (&optional session)
  "Load SESSION's workspace input history into the current view buffer.
Missing history files are normal and produce an empty ring.  Read failures
warn once and leave the file in place for retry.  Malformed files are renamed
to `.bad' and ignored."
  (setq mevedel-view-history--ring
        (make-ring (max 1 mevedel-view-input-history-size))
        mevedel-view-history--index nil
        mevedel-view-history--stored-incomplete nil
        mevedel-view-history--loaded-entries nil)
  (when-let* ((path (mevedel-view-history--path session)))
    (let (contents entries problem malformed-p)
      (condition-case err
          (when (file-exists-p path)
            (setq contents
                  (mevedel-session-control-fs-read-file
                   (mevedel-session-control-fs-physical-path path))))
        (error (setq problem err)))
      (when contents
        (condition-case err
            (setq entries
                  (mevedel-view-history--validate-entries
                   (car (read-from-string contents))))
          (error
           (setq problem err
                 malformed-p t)))
        (unless problem
          (mevedel-view-history--set-entries entries)))
      (when problem
        (mevedel--warn-once
         (list 'view-history-load path)
         "Input history unreadable at %s: %s; starting empty"
         path (error-message-string problem))
        (when malformed-p
          (mevedel-view-history--rename-bad-file path))))))

(defun mevedel-view-history-save (&optional view-buffer)
  "Persist VIEW-BUFFER input history."
  (let ((buf (or view-buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when-let* (((not (bound-and-true-p
                           mevedel-view--agent-transcript-p)))
                    ((not (mevedel-view-history--read-only-p)))
                    ;; This buffer contributed nothing since it last read
                    ;; or wrote the file, and no earlier save is owed a
                    ;; retry: the read-merge-write would reproduce what is
                    ;; already stored, at a held exclusive connection.
                    ((not (and (not mevedel-view-history--save-failed)
                               (equal-including-properties
                                (mevedel-view-history--entries)
                                mevedel-view-history--loaded-entries))))
                    (session (mevedel-view-history--session))
                    (path (mevedel-view-history--path session)))
          (condition-case err
              ;; Directory, ignore entry, read-merge-write: one transaction
              ;; of target commands, held so a foreign timer cannot send its
              ;; own in between and take the reply.
              (mevedel-transport-with-exclusive-connection
                (make-directory (file-name-directory path) t)
                (mevedel-workspace-ensure-generated-state-ignored
                 (mevedel-session-workspace session))
                (let* ((current (mevedel-view-history--entries))
                       (existing (and (file-exists-p path)
                                      (mevedel-view-history--validate-entries
                                       (mevedel-session-codec-read path))))
                       (merged (mevedel-view-history--merge-entries
                                current existing
                                mevedel-view-history--loaded-entries)))
                  (mevedel-session-codec-write
                   path
                   (list :version 2 :entries merged))
                  (mevedel-view-history--set-entries merged)
                  (setq mevedel-view-history--save-failed nil)))
            (error
             (setq mevedel-view-history--save-failed t)
             (mevedel--warn-once
              (list 'view-history-save path)
              "Input history save failed at %s: %s"
              path (error-message-string err)))))))))


;;
;;; Commands

(defun mevedel-view-history-previous ()
  "Cycle backward through view input history when in the composer."
  (interactive)
  (if (not (mevedel-view-history--input-active-p))
      (mevedel-view-history--call-global-key (kbd "M-p"))
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
  "Cycle forward through view input history when in the composer."
  (interactive)
  (if (not (mevedel-view-history--input-active-p))
      (mevedel-view-history--call-global-key (kbd "M-n"))
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

(defun mevedel-view-history--search-matches (query entries)
  "Return ENTRIES matching QUERY for incremental history search."
  (if (string-empty-p query)
      nil
    (let ((case-fold-search search-default-mode))
      (cl-remove-if-not
       (lambda (entry)
         (string-match-p (regexp-quote query) entry))
       entries))))

(defun mevedel-view-history--search-show (query matches index)
  "Show QUERY search state for MATCHES at INDEX in the echo area."
  (message "History search: %s%s"
           query
           (cond
            ((string-empty-p query) "")
            ((null matches) " [no match]")
            (t (format " [%d/%d]"
                       (1+ index) (length matches))))))

(defun mevedel-view-history--search-apply (matches index)
  "Replace input with MATCHES item at INDEX when present."
  (when-let* ((match (nth index matches)))
    (mevedel-view-history--replace-input match)))

(defun mevedel-view-history--search-restore (stored)
  "Restore STORED input and reset transient history navigation state."
  (mevedel-view-history--replace-input stored)
  (setq mevedel-view-history--stored-incomplete nil
        mevedel-view-history--index nil))

(defun mevedel-view-history-search ()
  "Incrementally search backward through input history.
Typing narrows matches as in a small history isearch.  `M-r' during
the search cycles through matching entries.  `RET' accepts the current
match.  `C-g' restores the input that was present before search started
and quits; `\\[mevedel-abort]' restores that input and aborts the active
request."
  (interactive)
  (if (not (mevedel-view-history--input-active-p))
      (mevedel-view-history--call-global-key (kbd "M-r"))
    (let* ((entries (mevedel-view-history--entries))
           (stored (mevedel-view-history--input-text))
           (query "")
           (matches nil)
           (index 0)
           (done nil))
      (when (null entries)
        (user-error "No input history"))
      (setq mevedel-view-history--stored-incomplete stored
            mevedel-view-history--index nil)
      (unwind-protect
          (condition-case nil
              (while (not done)
                (mevedel-view-history--search-show query matches index)
                (let ((key (read-key)))
                  (cond
                   ((memq key '(?\C-g ?\e escape))
                    (mevedel-view-history--search-restore stored)
                    (keyboard-quit))
                   ((eq key ?\C-c)
                    (let ((next (read-key)))
                      (if (eq next ?\C-k)
                          (progn
                            (mevedel-view-history--search-restore stored)
                            (setq done t)
                            (call-interactively #'mevedel-view-abort))
                        (setq unread-command-events
                              (append (list key next)
                                      unread-command-events)
                              done t))))
                   ((or (eq key 'return) (eq key ?\r) (eq key ?\n))
                    (setq done t))
                   ((or (eq key 'backspace) (eq key ?\d) (eq key 127))
                    (unless (string-empty-p query)
                      (setq query (substring query 0 -1)
                            matches (mevedel-view-history--search-matches
                                     query entries)
                            index 0)
                      (if matches
                          (mevedel-view-history--search-apply matches index)
                        (mevedel-view-history--replace-input stored))))
                   ((equal key ?\M-r)
                    (when matches
                      (setq index (mod (1+ index) (length matches)))
                      (mevedel-view-history--search-apply matches index)))
                   ((characterp key)
                    (setq query (concat query (string key))
                          matches (mevedel-view-history--search-matches
                                   query entries)
                          index 0)
                    (if matches
                        (mevedel-view-history--search-apply matches index)
                      (mevedel-view-history--replace-input stored)))
                   (t
                    (setq unread-command-events
                          (append (list key) unread-command-events)
                          done t)))))
            (quit
             (mevedel-view-history--search-restore stored)
             (signal 'quit nil)))
        (message nil)))))

(defun mevedel-view-history-browse ()
  "Browse input history and insert the selected entry into the composer."
  (interactive)
  (let ((entries (mevedel-view-history--entries)))
    (when (null entries)
      (user-error "No input history"))
    (let* ((choice (completing-read "Input history: " entries nil t))
           (entry (cl-find choice entries :test #'string=)))
      (goto-char (mevedel-view--input-start))
      (mevedel-view-history--replace-input (or entry choice)))))

(defun mevedel-view-history-clear-input ()
  "Clear current input when point is in the composer."
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
