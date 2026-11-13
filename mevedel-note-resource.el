;;; mevedel-note-resource.el -- Read-only workspace note documents -*- lexical-binding: t -*-

;;; Commentary:

;; Authorized resource operations project workspace notes, never conversations.
;; Source code is read only when the caller has ordinary Read authority for it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-permissions)

;; `mevedel-structs'
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)

(defvar mevedel-note-resource--source-buffers nil
  "Operation-owned temporary source buffers, keyed by file name.")

(defun mevedel-note-resource--source-buffer (file)
  "Return FILE's live or operation-owned source buffer, reading it at most once."
  (or (gethash file mevedel-note-resource--source-buffers)
      (find-buffer-visiting file)
      (let ((buffer (generate-new-buffer " *note source*")))
        (puthash file buffer mevedel-note-resource--source-buffers)
        (with-current-buffer buffer (insert-file-contents file))
        buffer)))

(defun mevedel-note-resource--header (entry workspace)
  "Return Markdown metadata for note ENTRY in WORKSPACE."
  (let ((properties (plist-get entry :properties)))
    (format "# Note #%s\n\nAddress: notes://%s.md\nLocation: %s:%d\nAuthor: %s\nState: %s%s\nTags: %s\n\n%s\n"
            (plist-get properties 'mevedel-id)
            (plist-get entry :uuid)
            (if-let* ((file (plist-get entry :file)))
                (file-relative-name file (mevedel-workspace-root workspace))
              "(unsaved buffer)")
            (plist-get entry :line)
            (plist-get properties 'mevedel-note-author)
            (plist-get properties 'mevedel-note-state)
            (if-let* ((reason (plist-get entry :closed-reason)))
                (format " (%s)" reason) "")
            (mapconcat #'symbol-name (plist-get properties 'mevedel-note-tags) ", ")
            (or (plist-get properties 'mevedel-note-text) ""))))

(defun mevedel-note-resource--code (entry)
  "Return current code and line for ENTRY without visiting a source file."
  (let* ((source (plist-get entry :source))
         (read-code
          (lambda ()
            (save-restriction
              (widen)
              (when-let* ((range (if (overlayp source)
                                    (cons (overlay-start source) (overlay-end source))
                                  (mevedel-persistence-resolve-instruction-anchor
                                   (plist-get source :overlay-start)
                                   (plist-get source :overlay-end)
                                   (plist-get source :anchor) nil))))
                (let* ((text (buffer-substring-no-properties (car range) (cdr range)))
                       (fence (mevedel--delimiting-markdown-backticks text)))
                  (list :line (line-number-at-pos (car range))
                        :code (concat fence "\n" text "\n" fence))))))))
    (condition-case nil
        (with-current-buffer (if (overlayp source) (overlay-buffer source)
                               (mevedel-note-resource--source-buffer (plist-get entry :file)))
          (funcall read-code))
      (file-error nil))))

(defun mevedel-note-resource--document (entry session)
  "Return ENTRY's text and current code for SESSION, respecting Read authority."
  (let* ((workspace (mevedel-session-workspace session))
         (file (plist-get entry :file))
         (permission (and file (mevedel-permission--invocation-context
                      :tool-name "Read" :session session :workspace workspace
                      :request mevedel--current-request
                      :invocation mevedel--agent-invocation
                      :buffer (current-buffer) :path file
                      :workspace-root (mevedel-workspace-root workspace))))
         (observation
          (cond
           ((not file) (list :code "Code unavailable: no source file."))
           ((not (eq 'allow (mevedel-check-permission "Read" :normalized-context permission)))
            (list :code "Code unavailable: Read permission denied."))
           (t (or (mevedel-note-resource--code entry)
                  (list :code "Code unavailable: source or anchor missing."))))))
    (when (plist-get observation :line)
      (setq entry (plist-put (copy-sequence entry) :line (plist-get observation :line))))
    (concat (mevedel-note-resource--header entry workspace) "\n" (plist-get observation :code))))

;;;###autoload
(defun mevedel-note-resource-execute (operation components session)
  "Execute authorized note OPERATION for COMPONENTS in SESSION's workspace."
  (let ((workspace (mevedel-session-workspace session))
        (mevedel-note-resource--source-buffers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (mevedel-instruction-store-observe workspace)
          (let* ((entries
                  (append
                   (mevedel-notes-open workspace)
                   (mapcar
                    (lambda (closed)
                      (let* ((source (plist-get closed :instruction))
                             (properties (plist-get source :properties)))
                        (list :file (plist-get closed :file) :source source
                              :line (or (car (plist-get source :closed-position)) 1)
                              :uuid (plist-get properties 'mevedel-uuid)
                              :properties properties
                              :closed-reason (plist-get source :closed-reason))))
                    (mevedel-notes-closed workspace t))))
                 (selected (and components
                                (cl-find (file-name-base (car components)) entries
                                         :key (lambda (entry) (plist-get entry :uuid))
                                         :test #'equal))))
            (when (and components (not selected))
              (signal 'mevedel-resource-unavailable
                      '("Note not found in this workspace; Read notes:// to discover notes")))
            (cond
             ((memq operation '(glob grep))
              (list :resource-search-documents
                    (mapcar (lambda (entry)
                              (cons (concat (plist-get entry :uuid) ".md")
                                    (if (eq operation 'glob) ""
                                      (mevedel-note-resource--document entry session))))
                            (if selected (list selected) entries))))
             (components (mevedel-note-resource--document selected session))
             (t
              (if entries
                  (mapconcat
                   (lambda (entry)
                     (let ((properties (plist-get entry :properties)))
                       (format "notes://%s.md\t#%s %s %s%s"
                               (plist-get entry :uuid) (plist-get properties 'mevedel-id)
                               (plist-get properties 'mevedel-note-author)
                               (plist-get properties 'mevedel-note-state)
                               (if-let* ((reason (plist-get entry :closed-reason)))
                                   (format " (%s)" reason) ""))))
                   entries "\n")
                "No notes in this workspace.")))))
      (maphash (lambda (_file buffer) (kill-buffer buffer))
               mevedel-note-resource--source-buffers))))

(provide 'mevedel-note-resource)
;;; mevedel-note-resource.el ends here
