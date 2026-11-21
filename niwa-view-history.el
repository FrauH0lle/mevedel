;;; niwa-view-history.el --- Input history for niwa view buffers -*- lexical-binding: t -*-

;;; Commentary:

;; Comint-style input history for `niwa-view-mode', implemented on
;; top of `ring.el' without inheriting from `comint-mode'.  The view
;; buffer remains an ephemeral projection of the data buffer; this
;; module owns only the editable composer's history ring and its
;; optional workspace-level persistence sidecar.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'subr-x)
(require 'niwa-mention-bindings)
(require 'niwa-structs)
(require 'niwa-utilities)
(require 'niwa-transport)

;; `niwa-mention-bindings'
(declare-function niwa-mention-bindings-copy-text
                  "niwa-mention-bindings" (text))
(declare-function niwa-mention-bindings-valid-p
                  "niwa-mention-bindings" (text))

;; `niwa-session-codec'
(declare-function niwa-session-codec-read
                  "niwa-session-codec" (path))
(declare-function niwa-session-codec-write
                  "niwa-session-codec" (path plist))
(autoload 'niwa-session-codec-read "niwa-session-codec")
(autoload 'niwa-session-codec-write "niwa-session-codec")

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-physical-path
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-read-file
                  "niwa-session-control-fs" (path &optional coding-system max-bytes))
(autoload 'niwa-session-control-fs-physical-path
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-read-file "niwa-session-control-fs")

;; `niwa-session-persistence'
(defvar niwa-session--read-only-mode)

;; `niwa-structs'
(declare-function niwa-session-clear-dropped-file-grants
                  "niwa-structs" (session))
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)

;; `niwa-view'
(defvar niwa--data-buffer)
(defvar niwa--session)

;; `niwa-view-composer'
(declare-function niwa-view--clear-input "niwa-view-composer" ())
(declare-function niwa-view--ensure-interactive-chat-view
                  "niwa-view-composer" ())
(declare-function niwa-view--session "niwa-view-composer" ())
(declare-function niwa-view-abort "niwa-view-composer" ())
(declare-function niwa-view-composer-draft-start "niwa-view-composer" (&optional view))
(declare-function niwa-view-composer-draft-text "niwa-view-composer" (&optional view without-properties))
(declare-function niwa-view-composer-in-draft-p "niwa-view-composer" (position &optional view))
(declare-function niwa-view-composer-replace "niwa-view-composer" (text &optional view))
(defvar niwa-composer--view)

;; `niwa-workspace'
(declare-function niwa-workspace-ensure-generated-state-ignored
                  "niwa-workspace" (workspace))
(declare-function niwa-workspace-state-dir "niwa-workspace" (workspace))
(autoload 'niwa-workspace-state-dir "niwa-workspace")


;;
;;; Customization

(defcustom niwa-view-input-history-size 500
  "Maximum number of entries kept in the view buffer's input ring."
  :type 'integer
  :group 'niwa)


;;
;;; State

(defvar-local niwa-view-history--ring nil
  "Input history ring for the current view buffer.")

(defvar-local niwa-view-history--index nil
  "Current index while navigating `niwa-view-history--ring'.")

(defvar-local niwa-view-history--stored-incomplete nil
  "Input text saved before history navigation began.")

(defvar-local niwa-view-history--save-failed nil
  "Non-nil after an input-history save failure in this view buffer.")

(defvar-local niwa-view-history--loaded-entries nil
  "Newest-first input history entries last loaded or saved in this view.")


;;
;;; Ring helpers

(defun niwa-view-history--ensure-ring ()
  "Return the current view buffer's input history ring."
  (unless (ring-p niwa-view-history--ring)
    (setq niwa-view-history--ring
          (make-ring (max 1 niwa-view-input-history-size))))
  niwa-view-history--ring)

(defun niwa-view-history--entries ()
  "Return history entries newest first."
  (ring-elements (niwa-view-history--ensure-ring)))

(defun niwa-view-history--input-active-p ()
  "Return non-nil when point is in the composer draft."
  (and (derived-mode-p 'niwa-composer-mode)
       (buffer-live-p niwa-composer--view)
       (niwa-view-composer-in-draft-p (point))))

(defun niwa-view-history--input-text ()
  "Return current view input text, untrimmed."
  (niwa-mention-bindings-copy-text
   (niwa-view-composer-draft-text)))

(defun niwa-view-history--replace-input (text)
  "Replace the current view input with TEXT."
  (niwa-view--ensure-interactive-chat-view)
  (niwa-view-composer-replace text)
  (when-let* ((session (niwa-view--session)))
    (niwa-session-clear-dropped-file-grants session)))

(defun niwa-view-history-add (input)
  "Add INPUT to the current view buffer's input history.
INPUT is trimmed before insertion.  Empty entries and consecutive
duplicates are skipped.  History navigation state is reset."
  (let ((entry (string-trim
                (niwa-mention-bindings-copy-text
                 (niwa--normalize-message-text (or input "")))))
        (ring (niwa-view-history--ensure-ring)))
    (setq niwa-view-history--index nil
          niwa-view-history--stored-incomplete nil)
    (unless (string-empty-p entry)
      (when (and (> (ring-length ring) 0)
                 (string= entry (ring-ref ring 0)))
        (ring-remove ring 0))
      (ring-insert ring entry))))


;;
;;; Persistence

(defun niwa-view-history--session ()
  "Return the session associated with the current view buffer."
  (or (and (boundp 'niwa--session) niwa--session)
      (and (boundp 'niwa--data-buffer)
           (buffer-live-p niwa--data-buffer)
           (buffer-local-value 'niwa--session niwa--data-buffer))))

(defun niwa-view-history--read-only-p ()
  "Return non-nil when the associated data buffer is read-only."
  (and (boundp 'niwa--data-buffer)
       (buffer-live-p niwa--data-buffer)
       (buffer-local-value 'niwa-session--read-only-mode
                           niwa--data-buffer)))

(defun niwa-view-history--path (&optional session)
  "Return SESSION's workspace input-history path, or nil."
  (when-let* ((sess (or session (niwa-view-history--session)))
              (workspace (niwa-session-workspace sess)))
    (file-name-concat (niwa-workspace-state-dir workspace)
                      "input-history.el")))

(defun niwa-view-history--call-global-key (key)
  "Invoke KEY's global binding, or report it as undefined.
Used by history keys outside the editable composer so the view mode
does not substitute its own navigation behavior there."
  (let ((cmd (lookup-key (current-global-map) key)))
    (if (commandp cmd)
        (call-interactively cmd)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "%s is undefined" (key-description key)))))

(defun niwa-view-history--rename-bad-file (path scope)
  "Rename corrupt input history PATH to PATH.bad, reporting with SCOPE."
  (when (file-exists-p path)
    (let ((bad (concat path ".bad")))
      (condition-case err
          (progn
            (when (file-exists-p bad)
              (delete-file bad))
            (rename-file path bad))
        (error
         (niwa-diagnostic-warning
          scope
          'niwa
          (format "Could not rename corrupt input history %s: %s"
                  path (error-message-string err))
          :warning))))))

(defun niwa-view-history--validate-entries (plist)
  "Return newest-first history entries from PLIST.
Signal an error when PLIST is not valid input-history data."
  (let ((entries (plist-get plist :entries)))
    (unless (and (equal 2 (plist-get plist :version))
                 (listp entries)
                 (cl-every
                  (lambda (entry)
                    (and (stringp entry)
                         (niwa-mention-bindings-valid-p entry)))
                  entries))
      (error "Malformed input history"))
    (mapcar (lambda (entry)
              (niwa-mention-bindings-copy-text
               (niwa--normalize-message-text entry)))
            entries)))

(defun niwa-view-history--new-since-load (current loaded)
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

(defun niwa-view-history--merge-entries (current existing &optional loaded)
  "Merge newest-first CURRENT and EXISTING history entries.
LOADED is the newest-first history previously loaded into this view.
Entries added to CURRENT since LOADED win recency ties, followed by
EXISTING, then CURRENT's older entries.  Duplicate strings are kept at
their first occurrence and the result is capped to
`niwa-view-input-history-size'."
  (let ((seen (make-hash-table :test #'equal))
        entries)
    (dolist (entry (append (niwa-view-history--new-since-load
                            current loaded)
                           existing
                           current))
      (when (and (stringp entry)
                 (not (string-empty-p entry))
                 (not (gethash entry seen)))
        (puthash entry t seen)
        (push entry entries)))
    (take niwa-view-input-history-size (nreverse entries))))

(defun niwa-view-history--set-entries (entries)
  "Replace the current buffer's history ring with newest-first ENTRIES."
  (setq niwa-view-history--ring
        (make-ring (max 1 niwa-view-input-history-size))
        niwa-view-history--loaded-entries entries)
  (dolist (entry (reverse entries))
    (ring-insert niwa-view-history--ring entry)))

(defun niwa-view-history-load (&optional session)
  "Load SESSION's workspace input history into the current view buffer.
Missing history files are normal and produce an empty ring.  Read failures
warn once and leave the file in place for retry.  Malformed files are renamed
to `.bad' and ignored."
  (setq niwa-view-history--ring
        (make-ring (max 1 niwa-view-input-history-size))
        niwa-view-history--index nil
        niwa-view-history--stored-incomplete nil
        niwa-view-history--loaded-entries nil)
  (when-let* ((path (niwa-view-history--path session)))
    (let (contents entries problem malformed-p)
      (condition-case err
          (when (file-exists-p path)
            (setq contents
                  (niwa-session-control-fs-read-file
                   (niwa-session-control-fs-physical-path path))))
        (error (setq problem err)))
      (when contents
        (condition-case err
            (setq entries
                  (niwa-view-history--validate-entries
                   (car (read-from-string contents))))
          (error
           (setq problem err
                 malformed-p t)))
        (unless problem
          (niwa-view-history--set-entries entries)))
      (when problem
        (niwa--warn-once
         (niwa-diagnostic-scope (or session (niwa-view-history--session)))
         (list 'view-history-load path)
         "Input history unreadable at %s: %s; starting empty"
         path (error-message-string problem))
        (when malformed-p
          (niwa-view-history--rename-bad-file
           path (niwa-diagnostic-scope (or session (niwa-view-history--session)))))))))

(defun niwa-view-history-save (&optional view-buffer)
  "Persist VIEW-BUFFER input history."
  (let ((buf (or view-buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when-let* (((not (bound-and-true-p
                           niwa-view--agent-transcript-p)))
                    ((not (niwa-view-history--read-only-p)))
                    ;; This buffer contributed nothing since it last read
                    ;; or wrote the file, and no earlier save is owed a
                    ;; retry: the read-merge-write would reproduce what is
                    ;; already stored, at a held exclusive connection.
                    ((not (and (not niwa-view-history--save-failed)
                               (equal-including-properties
                                (niwa-view-history--entries)
                                niwa-view-history--loaded-entries))))
                    (session (niwa-view-history--session))
                    (path (niwa-view-history--path session)))
          (condition-case err
              ;; Directory, ignore entry, read-merge-write: one transaction
              ;; of target commands, held so a foreign timer cannot send its
              ;; own in between and take the reply.
              (niwa-transport-with-exclusive-connection
                (make-directory (file-name-directory path) t)
                (niwa-workspace-ensure-generated-state-ignored
                 (niwa-session-workspace session))
                (let* ((current (niwa-view-history--entries))
                       (existing (and (file-exists-p path)
                                      (niwa-view-history--validate-entries
                                       (niwa-session-codec-read path))))
                       (merged (niwa-view-history--merge-entries
                                current existing
                                niwa-view-history--loaded-entries)))
                  (niwa-session-codec-write
                   path
                   (list :version 2 :entries merged))
                  (niwa-view-history--set-entries merged)
                  (setq niwa-view-history--save-failed nil)))
            (error
             (setq niwa-view-history--save-failed t)
             (niwa--warn-once
              (niwa-diagnostic-scope session)
              (list 'view-history-save path)
              "Input history save failed at %s: %s"
              path (error-message-string err)))))))))


;;
;;; Commands

(defun niwa-view-history-previous ()
  "Cycle backward through view input history when in the composer."
  (interactive)
  (if (not (niwa-view-history--input-active-p))
      (niwa-view-history--call-global-key (kbd "M-p"))
    (with-current-buffer niwa-composer--view
      (let ((ring (niwa-view-history--ensure-ring)))
        (when (ring-empty-p ring)
          (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No input history"))
        (unless niwa-view-history--index
          (setq niwa-view-history--stored-incomplete
                (niwa-view-history--input-text)))
        (setq niwa-view-history--index
              (if niwa-view-history--index
                  (min (1- (ring-length ring))
                       (1+ niwa-view-history--index))
                0))
        (niwa-view-history--replace-input
         (ring-ref ring niwa-view-history--index))))))

(defun niwa-view-history-next ()
  "Cycle forward through view input history when in the composer."
  (interactive)
  (if (not (niwa-view-history--input-active-p))
      (niwa-view-history--call-global-key (kbd "M-n"))
    (with-current-buffer niwa-composer--view
      (cond
       ((null niwa-view-history--index)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Not browsing input history"))
       ((> niwa-view-history--index 0)
        (setq niwa-view-history--index
              (1- niwa-view-history--index))
        (niwa-view-history--replace-input
         (ring-ref (niwa-view-history--ensure-ring)
                   niwa-view-history--index)))
       (t
        (setq niwa-view-history--index nil)
        (niwa-view-history--replace-input
         (or niwa-view-history--stored-incomplete ""))
        (setq niwa-view-history--stored-incomplete nil))))))

(defun niwa-view-history--search-matches (query entries)
  "Return ENTRIES matching QUERY for incremental history search."
  (if (string-empty-p query)
      nil
    (let ((case-fold-search search-default-mode))
      (cl-remove-if-not
       (lambda (entry)
         (string-match-p (regexp-quote query) entry))
       entries))))

(defun niwa-view-history--search-show (query matches index)
  "Show QUERY search state for MATCHES at INDEX in the echo area."
  (message "History search: %s%s"
           query
           (cond
            ((string-empty-p query) "")
            ((null matches) " [no match]")
            (t (format " [%d/%d]"
                       (1+ index) (length matches))))))

(defun niwa-view-history--search-apply (matches index)
  "Replace input with MATCHES item at INDEX when present."
  (when-let* ((match (nth index matches)))
    (niwa-view-history--replace-input match)))

(defun niwa-view-history--search-restore (stored)
  "Restore STORED input and reset transient history navigation state."
  (niwa-view-history--replace-input stored)
  (with-current-buffer niwa-composer--view
    (setq niwa-view-history--stored-incomplete nil
          niwa-view-history--index nil)))

(defun niwa-view-history-search ()
  "Incrementally search backward through input history.
Typing narrows matches as in a small history isearch.  `M-r' during
the search cycles through matching entries.  `RET' accepts the current
match.  `C-g' restores the input that was present before search started
and quits; `\\[niwa-abort]' restores that input and aborts the active
request."
  (interactive)
  (if (not (niwa-view-history--input-active-p))
      (niwa-view-history--call-global-key (kbd "M-r"))
    (let* ((entries (with-current-buffer niwa-composer--view
                      (niwa-view-history--entries)))
           (stored (niwa-view-history--input-text))
           (query "")
           (matches nil)
           (index 0)
           (done nil))
      (when (null entries)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No input history"))
      (with-current-buffer niwa-composer--view
        (setq niwa-view-history--stored-incomplete stored
              niwa-view-history--index nil))
      (unwind-protect
          (condition-case nil
              (while (not done)
                (niwa-view-history--search-show query matches index)
                (let ((key (read-key)))
                  (cond
                   ((memq key '(?\C-g ?\e escape))
                    (niwa-view-history--search-restore stored)
                    (keyboard-quit))
                   ((eq key ?\C-c)
                    (let ((next (read-key)))
                      (if (eq next ?\C-k)
                          (progn
                            (niwa-view-history--search-restore stored)
                            (setq done t)
                            (call-interactively #'niwa-view-abort))
                        (setq unread-command-events
                              (append (list key next)
                                      unread-command-events)
                              done t))))
                   ((or (eq key 'return) (eq key ?\r) (eq key ?\n))
                    (setq done t))
                   ((or (eq key 'backspace) (eq key ?\d) (eq key 127))
                    (unless (string-empty-p query)
                      (setq query (substring query 0 -1)
                            matches (niwa-view-history--search-matches
                                     query entries)
                            index 0)
                      (if matches
                          (niwa-view-history--search-apply matches index)
                        (niwa-view-history--replace-input stored))))
                   ((equal key ?\M-r)
                    (when matches
                      (setq index (mod (1+ index) (length matches)))
                      (niwa-view-history--search-apply matches index)))
                   ((characterp key)
                    (setq query (concat query (string key))
                          matches (niwa-view-history--search-matches
                                   query entries)
                          index 0)
                    (if matches
                        (niwa-view-history--search-apply matches index)
                      (niwa-view-history--replace-input stored)))
                   (t
                    (setq unread-command-events
                          (append (list key) unread-command-events)
                          done t)))))
            (quit
             (niwa-view-history--search-restore stored)
             (signal 'quit nil)))
        (message nil)))))

(defun niwa-view-history-browse ()
  "Browse input history and insert the selected entry into the composer."
  (interactive)
  (let ((entries (with-current-buffer (or niwa-composer--view (current-buffer))
                   (niwa-view-history--entries))))
    (when (null entries)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No input history"))
    (let* ((choice (completing-read "Input history: " entries nil t))
           (entry (cl-find choice entries :test #'string=)))
      (niwa-view-history--replace-input (or entry choice)))))

(defun niwa-view-history-clear-input ()
  "Clear current input when point is in the composer."
  (interactive)
  (when (niwa-view-history--input-active-p)
    (niwa-view--clear-input)))

(defun niwa-view-history-beginning-of-line (&optional arg)
  "Move to input start on the prompt line, otherwise run normal BOL.
ARG is forwarded to `move-beginning-of-line' on the normal path."
  (interactive "^p")
  (if-let* (((niwa-view-history--input-active-p))
            (start (niwa-view-composer-draft-start))
            ((> (point) start))
            ((<= (line-beginning-position) start (line-end-position))))
      (goto-char start)
    (move-beginning-of-line arg)))

(provide 'niwa-view-history)
;;; niwa-view-history.el ends here
