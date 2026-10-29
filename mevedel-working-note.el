;;; mevedel-working-note.el --- Pure working-note metadata codec -*- lexical-binding: t -*-

;;; Commentary:

;; Classify Markdown working notes and normalize their lifecycle frontmatter.
;; This module owns only strings: callers supply attribution and target time,
;; and retain filesystem, permission, preview and transaction ownership.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defun mevedel-working-note-markdown-p (path)
  "Return non-nil when PATH names a Markdown note, ignoring case."
  (and (stringp path) (string-suffix-p ".md" (downcase path))))

(defun mevedel-working-note--timestamp-p (value)
  "Return non-nil when VALUE is a real UTC timestamp at second precision."
  (and (stringp value)
       (string-match
        "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)Z\\'"
        value)
       (condition-case nil
           (let ((parts (mapcar (lambda (n) (string-to-number (match-string n value)))
                                '(6 5 4 3 2 1))))
             (equal value (format-time-string
                           "%Y-%m-%dT%H:%M:%SZ"
                           (encode-time (append parts '(nil -1 t))) t)))
         (error nil))))

(defun mevedel-working-note--scalar (text)
  "Decode one single-line metadata scalar TEXT, rejecting YAML structures."
  (setq text (string-trim text))
  (cond
   ((string-prefix-p "\"" text)
    (let ((value (json-parse-string text)))
      (unless (stringp value) (error "Metadata value must be a string"))
      value))
   ((string-prefix-p "'" text)
    (unless (string-match-p "\\`'\\(?:[^']\\|''\\)*'\\'" text)
      (error "Invalid quoted metadata value"))
    (replace-regexp-in-string "''" "'" (substring text 1 -1) t t))
   ((or (string-empty-p text)
        (memq (aref text 0) '(?> ?| ?! ?& ?* ?{ ?\[))
        (string-match-p "[\n\r\0]\\|[[:space:]]#" text))
    (error "Invalid metadata scalar"))
   (t text)))

(defun mevedel-working-note--parse (content)
  "Parse CONTENT into fields, body and header, or signal on invalid metadata.
Missing headers and missing fields are allowed here; classification requires
all four fields, while normalization supplies absent fields."
  (when (string-match-p "[\0]" content)
    (error "Markdown note contains binary data"))
  ;; A lifecycle key signals metadata even without a closing delimiter.
  ;; Other field-shaped blocks need a closing delimiter: an ordinary Markdown
  ;; rule followed by "Note: prose" is not an unterminated YAML header.
  (if (not (string-match "\\`---\r?\n" content))
      (list :body content)
    (let* ((start (match-end 0))
           (closed (string-match "^---\r?$" content start))
           (end (and closed (match-end 0)))
           (region (substring content start closed))
           (intent (or (string-match-p "^[ \t]*\\(?:kind\\|status\\|owner\\|created\\)[ \t]*:" region)
                       (and closed (string-match-p "^[ \t]*[A-Za-z][A-Za-z_-]*[ \t]*:" region))))
           fields)
      (cond
       ((not intent) (list :body content))
       ((not closed) (error "Unterminated working-note frontmatter"))
       (t
        (dolist (line (split-string region "\n" t))
          (unless (or (string-empty-p (string-trim line))
                      (string-match-p "\\`[ \t]*#" line))
            (unless (string-match "\\`\\(kind\\|status\\|owner\\|created\\):[ \t]*\\(.*\\)\\r?\\'" line)
              (error "Unknown or malformed working-note field"))
            (let* ((key (intern (concat ":" (match-string 1 line))))
                   (value (mevedel-working-note--scalar (match-string 2 line))))
              (when (plist-member fields key)
                (error "Duplicate working-note field: %s" key))
              (pcase key
                (:kind (unless (member value '("handoff" "report" "scratch"))
                         (error "Invalid working-note kind"))
                       (setq value (intern value)))
                (:status (unless (member value '("open" "done"))
                           (error "Invalid working-note status"))
                         (setq value (intern value)))
                (:owner (when (or (string-empty-p (string-trim value))
                                  (string-match-p "[[:cntrl:]]" value))
                          (error "Working-note owner must be nonempty single-line attribution")))
                (:created (unless (mevedel-working-note--timestamp-p value)
                            (error "Working-note created must be a UTC timestamp"))))
              (setq fields (plist-put fields key value)))))
        (append (list :header (substring content 0 end)
                      :body (substring content
                                       (if (and (< end (length content))
                                                (eq (aref content end) ?\n))
                                           (1+ end) end)))
                fields))))))

(defun mevedel-working-note-classify (path content)
  "Classify PATH and CONTENT as managed, unmanaged or attachment.
Return a plist with `:classification', `:body', and managed `:kind',
`:status', `:owner', `:created'.  Unmanaged Markdown carries a bounded
`:diagnostic' and defaults to open handoff for lifecycle purposes.
Non-Markdown attachments are not parsed, even when CONTENT is binary."
  (if (not (mevedel-working-note-markdown-p path))
      (list :classification 'attachment)
    (condition-case err
        (let ((parsed (mevedel-working-note--parse content)))
          (unless (cl-every (lambda (key) (plist-member parsed key))
                            '(:kind :status :owner :created))
            (error "Missing working-note metadata"))
          (append (list :classification 'managed) parsed))
      (error (list :classification 'unmanaged :kind 'handoff :status 'open
                   :body content :diagnostic (truncate-string-to-width
                                             (error-message-string err) 160))))))

(defun mevedel-working-note-normalize (content owner created &optional previous-content)
  "Normalize CONTENT with default attribution OWNER and UTC CREATED.
Preserve valid PREVIOUS-CONTENT metadata for fields not explicitly authored.
Malformed authored headers fail.  An unchanged malformed external header is
replaced on first edit, not treated as newly authored metadata."
  (let* ((previous (and previous-content
                        (mevedel-working-note-classify "note.md" previous-content)))
         (parsed
          (condition-case err
              (mevedel-working-note--parse content)
            (error
             (if (and previous-content
                      (string-match "\\`---\r?\n\\(?:.\\|\n\\)*?\n---\r?\\(?:\n\\|\\'\\)" content)
                      (string-prefix-p (match-string 0 content) previous-content))
                 (list :body (substring content (match-end 0)))
               (signal (car err) (cdr err))))))
         (defaults (if (eq (plist-get previous :classification) 'managed)
                       previous
                     (list :kind 'scratch :status 'open :owner owner :created created)))
         (kind (or (plist-get parsed :kind) (plist-get defaults :kind)))
         (status (or (plist-get parsed :status) (plist-get defaults :status)))
         (attribution (or (plist-get parsed :owner) (plist-get defaults :owner)))
         (creation (or (plist-get parsed :created) (plist-get defaults :created)))
         (result (format "---\nkind: %s\nstatus: %s\nowner: %s\ncreated: %s\n---\n%s"
                         kind status (json-serialize attribution)
                         (json-serialize creation) (plist-get parsed :body))))
    ;; Validate invocation defaults as strictly as authored metadata.
    (mevedel-working-note--parse result)
    result))

(provide 'mevedel-working-note)
;;; mevedel-working-note.el ends here
