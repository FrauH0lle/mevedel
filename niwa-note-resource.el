;;; niwa-note-resource.el -- Read-only workspace note documents -*- lexical-binding: t -*-

;;; Commentary:

;; Authorized resource operations project workspace notes, never conversations.
;; Source code is read only when the caller has ordinary Read authority for it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-permissions)

;; `niwa-structs'
(defvar niwa--agent-invocation)
(defvar niwa--current-request)

(defvar niwa-note-resource--source-buffers nil
  "Operation-owned temporary source buffers, keyed by file name.")

(defun niwa-note-resource--source-buffer (file)
  "Return FILE's live or operation-owned source buffer, reading it at most once."
  (or (gethash file niwa-note-resource--source-buffers)
      (find-buffer-visiting file)
      (let ((buffer (generate-new-buffer " *note source*")))
        (puthash file buffer niwa-note-resource--source-buffers)
        (with-current-buffer buffer (insert-file-contents file))
        buffer)))

(defun niwa-note-resource--header (entry workspace)
  "Return Markdown metadata for note ENTRY in WORKSPACE."
  (let ((properties (plist-get entry :properties)))
    (format "# Note #%s\n\nAddress: notes://%s.md\nLocation: %s:%d\nAuthor: %s\nState: %s%s\nTags: %s\n\n%s\n"
            (plist-get properties 'niwa-id)
            (plist-get entry :uuid)
            (if-let* ((file (plist-get entry :file)))
                (file-relative-name file (niwa-workspace-root workspace))
              "(unsaved buffer)")
            (plist-get entry :line)
            (plist-get properties 'niwa-note-author)
            (plist-get properties 'niwa-note-state)
            (if-let* ((reason (plist-get entry :closed-reason)))
                (format " (%s)" reason) "")
            (mapconcat #'symbol-name (plist-get properties 'niwa-note-tags) ", ")
            (or (plist-get properties 'niwa-note-text) ""))))

(defun niwa-note-resource--code (entry)
  "Return current code and line for ENTRY without visiting a source file."
  (let* ((source (plist-get entry :source))
         (read-code
          (lambda ()
            (save-restriction
              (widen)
              (when-let* ((range (if (overlayp source)
                                    (cons (overlay-start source) (overlay-end source))
                                  (niwa-persistence-resolve-instruction-anchor
                                   (plist-get source :overlay-start)
                                   (plist-get source :overlay-end)
                                   (plist-get source :anchor) nil))))
                (let* ((text (buffer-substring-no-properties (car range) (cdr range)))
                       (fence (niwa--delimiting-markdown-backticks text)))
                  (list :line (line-number-at-pos (car range))
                        :code (concat fence "\n" text "\n" fence))))))))
    (condition-case nil
        (with-current-buffer (if (overlayp source) (overlay-buffer source)
                               (niwa-note-resource--source-buffer (plist-get entry :file)))
          (funcall read-code))
      (file-error nil))))

(defun niwa-note-resource--document (entry session)
  "Return ENTRY's text and current code for SESSION, respecting Read authority."
  (let* ((workspace (niwa-session-workspace session))
         (file (plist-get entry :file))
         (permission (and file (niwa-permission--invocation-context
                      :tool-name "Read" :session session :workspace workspace
                      :request niwa--current-request
                      :invocation niwa--agent-invocation
                      :buffer (current-buffer) :path file
                      :workspace-root (niwa-workspace-root workspace))))
         (observation
          (cond
           ((not file) (list :code "Code unavailable: no source file."))
           ((not (eq 'allow (niwa-check-permission "Read" :normalized-context permission)))
            (list :code "Code unavailable: Read permission denied."))
           (t (or (niwa-note-resource--code entry)
                  (list :code "Code unavailable: source or anchor missing."))))))
    (when (plist-get observation :line)
      (setq entry (plist-put (copy-sequence entry) :line (plist-get observation :line))))
    (concat (niwa-note-resource--header entry workspace) "\n" (plist-get observation :code))))

;;;###autoload
(defun niwa-note-resource-execute (operation components session)
  "Execute authorized note OPERATION for COMPONENTS in SESSION's workspace."
  (let ((workspace (niwa-session-workspace session))
        (niwa-note-resource--source-buffers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (niwa-instruction-store-observe workspace)
          (let* ((entries
                  (append
                   (niwa-notes-open workspace)
                   (mapcar
                    (lambda (closed)
                      (let* ((source (plist-get closed :instruction))
                             (properties (plist-get source :properties)))
                        (list :file (plist-get closed :file) :source source
                              :line (or (car (plist-get source :closed-position)) 1)
                              :uuid (plist-get properties 'niwa-uuid)
                              :properties properties
                              :closed-reason (plist-get source :closed-reason))))
                    (niwa-notes-closed workspace t))))
                 (selected (and components
                                (cl-find (file-name-base (car components)) entries
                                         :key (lambda (entry) (plist-get entry :uuid))
                                         :test #'equal))))
            (when (and components (not selected))
              (signal 'niwa-resource-unavailable
                      '("Note not found in this workspace; Read notes:// to discover notes")))
            (cond
             ((memq operation '(glob grep))
              (list :resource-search-documents
                    (mapcar (lambda (entry)
                              (cons (concat (plist-get entry :uuid) ".md")
                                    (if (eq operation 'glob) ""
                                      (niwa-note-resource--document entry session))))
                            (if selected (list selected) entries))))
             (components (niwa-note-resource--document selected session))
             (t
              (if entries
                  (mapconcat
                   (lambda (entry)
                     (let ((properties (plist-get entry :properties)))
                       (format "notes://%s.md\t#%s %s %s%s"
                               (plist-get entry :uuid) (plist-get properties 'niwa-id)
                               (plist-get properties 'niwa-note-author)
                               (plist-get properties 'niwa-note-state)
                               (if-let* ((reason (plist-get entry :closed-reason)))
                                   (format " (%s)" reason) ""))))
                   entries "\n")
                "No notes in this workspace.")))))
      (maphash (lambda (_file buffer) (kill-buffer buffer))
               niwa-note-resource--source-buffers))))

(provide 'niwa-note-resource)
;;; niwa-note-resource.el ends here
