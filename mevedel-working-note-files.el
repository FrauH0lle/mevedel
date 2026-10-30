;;; mevedel-working-note-files.el --- Bounded working-note observations -*- lexical-binding: t -*-

;;; Commentary:

;; Shared filesystem observations for discovery and note capture.  This module
;; owns no memory decisions or write authority.  Age readiness is advisory;
;; capture and application independently check use, dependencies and freshness.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'url-util)
(require 'mevedel-session-control-fs)
(require 'mevedel-utilities)

(defvar remote-file-name-inhibit-cache)

(defcustom mevedel-working-note-max-age-days 14
  "Untouched age in days before a shared note or attachment can be deleted.
Age comes only from file modification time; reading does not reset it.
Active local use and unsaved buffer changes still prevent deletion.
Set to nil to disable automatic shared-file disposal."
  :type '(choice (const :tag "Never delete" nil) natnum)
  :group 'mevedel)

(defun mevedel-working-note-files-markdown-p (path)
  "Return non-nil when PATH names a Markdown note, ignoring case."
  (and (stringp path) (string-suffix-p ".md" (downcase path))))

(defun mevedel-working-note-files-eligibility (modified now)
  "Return modification-age readiness at target time NOW for MODIFIED.
Both times are seconds on the same target clock.  Eligibility describes age
only, not capture or application safety.  Exactly the configured age qualifies."
  (let* ((age (and (numberp modified) (numberp now) (- now modified)))
         (reason (cond ((null mevedel-working-note-max-age-days) 'disabled)
                       ((null age) 'unknown-mtime)
                       ((< age 0) 'future-mtime)
                       ((>= age (* mevedel-working-note-max-age-days 86400)) 'age-ready)
                       (t 'age-wait))))
    (list :age-seconds age :reason reason :eligible (eq reason 'age-ready))))

(defun mevedel-working-note-files-heading (content)
  "Return the bounded first Markdown heading in CONTENT, or an empty string."
  (if (string-match "^#+[ \t]+\\(.+\\)$" content)
      (truncate-string-to-width
       (string-trim (replace-regexp-in-string "[[:space:][:cntrl:]]+" " " (match-string 1 content)))
       160 nil nil t)
    ""))

(defconst mevedel-working-note-files-max-entries 4096
  "Maximum shared entries inventoried in one observation.")

(defconst mevedel-working-note-files-prefix-bytes (* 32 1024)
  "Maximum prefix bytes inspected per shared file.")

(defun mevedel-working-note-files-inventory (directory)
  "Walk DIRECTORY without following links, bounded by the entry limit.
Return :files and :directories as (RELATIVE . ATTRIBUTES) rows, :complete,
and :problems.  Remote attribute caches are bypassed.
No missing directory or coordination storage is created."
  (let ((remote-file-name-inhibit-cache t)
        (pending (list "")) (remaining mevedel-working-note-files-max-entries)
        (complete t) files directories problems)
    (while pending
      (let* ((relative (pop pending))
             (path (if (string-empty-p relative) directory (file-name-concat directory relative)))
             (children (condition-case err
                           (directory-files-and-attributes path nil directory-files-no-dot-files-regexp
                                                           t nil (1+ remaining))
                         (error (setq complete nil)
                                (push (cons relative (error-message-string err)) problems)
                                nil))))
        (unless (string-empty-p relative)
          (push (cons relative (list :empty (null children)
                                     :modified (file-attribute-modification-time (file-attributes path))))
                directories))
        (dolist (child children)
          (if (<= remaining 0)
              (setq complete nil)
            (cl-decf remaining)
            (let* ((name (car child)) (attributes (cdr child))
                   (type (file-attribute-type attributes))
                   (child-relative (if (string-empty-p relative) name (concat relative "/" name))))
              (cond
               ((stringp type)
                (setq complete nil)
                (push (cons child-relative "Linked shared entries are not followed") problems))
               ((eq type t) (push child-relative pending))
               (t (push (cons child-relative
                              (list :size (file-attribute-size attributes)
                                    :modified (file-attribute-modification-time attributes))) files))))))))
    (list :files (sort files (lambda (a b) (string< (car a) (car b))))
          :directories (sort directories (lambda (a b) (string< (car a) (car b))))
          :complete complete :problems (nreverse problems))))

(defun mevedel-working-note-files-text (bytes &optional truncated)
  "Decode UTF-8 BYTES, or return nil for binary or invalid text.
TRUNCATED allows an incomplete final character in a bounded prefix."
  (let ((text (decode-coding-string bytes 'utf-8-unix)))
    (when truncated
      (while (and (> (length text) 0)
                  (eq (char-charset (aref text (1- (length text)))) 'eight-bit))
        (setq text (substring text 0 -1))))
    (unless (or (string-search "\0" bytes)
                (cl-some #'mevedel--invalid-message-char-p text))
      text)))

(defun mevedel-working-note-files-current-p (directory row)
  "Return non-nil if ROW still has its observed size and mtime in DIRECTORY.
Links and missing entries never match; remote attribute caches are bypassed.
This is an observation, not write proof."
  (let* ((remote-file-name-inhibit-cache t)
         (path (file-name-concat directory (car row)))
         (attributes (file-attributes path)))
    (and attributes (null (file-attribute-type attributes))
         (equal (file-attribute-size attributes) (plist-get (cdr row) :size))
         (equal (file-attribute-modification-time attributes) (plist-get (cdr row) :modified)))))

(defun mevedel-working-note-files-read (directory rows)
  "Read bounded prefixes of file ROWS in DIRECTORY without changing mtimes.
Rows carry :kind `text', `attachment' or `unknown'.
Only readable Markdown is a text note.  Readable attachments
still carry :text for conservative reference scans; :bytes is complete
only without :truncated.  Failed or changed observations carry :error."
  (let ((pending rows) results)
    (while pending
      (let* ((batch (seq-take pending 8))
             (reads (condition-case err
                        (mevedel-session-control-fs-run-program
                         (mapcar (lambda (row)
                                   (list :op 'read :optional t :coding 'no-conversion
                                         :path (file-name-concat directory (car row))
                                         :max-bytes (1+ mevedel-working-note-files-prefix-bytes))) batch))
                      (error (mapcar (lambda (_) (list :status 'failed :diagnostic (error-message-string err))) batch)))))
        (setq pending (nthcdr 8 pending))
        (unless (= (length reads) (length batch))
          (setq reads (mapcar (lambda (_) (list :status 'failed)) batch)))
        (cl-mapc
         (lambda (row result)
           (push
            (cond
             ((not (eq (plist-get result :status) 'ok))
              (append row (list :kind 'unknown :error "Unreadable shared file")))
             ((not (mevedel-working-note-files-current-p directory row))
              (append row (list :kind 'unknown :error "Shared file changed during observation")))
             (t
              (let* ((bytes (plist-get result :value))
                     (truncated (> (length bytes) mevedel-working-note-files-prefix-bytes))
                     (text (mevedel-working-note-files-text
                            (if truncated (substring bytes 0 mevedel-working-note-files-prefix-bytes) bytes) truncated)))
                (append row (list :kind (cond ((and (not text) (mevedel-working-note-files-markdown-p (car row))) 'unknown)
                                              ((not text) 'attachment)
                                              ((mevedel-working-note-files-markdown-p (car row)) 'text)
                                              (t 'attachment))
                                  :text text :bytes (unless truncated bytes)
                                  :error (and (not text) (mevedel-working-note-files-markdown-p (car row))
                                              "Markdown note is not readable UTF-8")
                                  :truncated truncated))))) results))
         batch reads)))
    (sort results (lambda (a b) (string< (car a) (car b))))))

(defun mevedel-working-note-files-listing (directory address)
  "Generate bounded discovery for DIRECTORY beneath canonical ADDRESS.
Completeness describes observation only.  Age readiness never claims that
capture/application safety, use or reference dependencies have been checked."
  (let* ((inventory (mevedel-working-note-files-inventory directory))
         (rows (mevedel-working-note-files-read directory (plist-get inventory :files)))
         (now (condition-case nil (mevedel-session-control-fs-observation-time directory) (error nil)))
         (complete (and now (plist-get inventory :complete)
                        (not (seq-some (lambda (row) (plist-get (cdr row) :error)) rows))))
         (lines
          (mapcar
           (lambda (row)
             (let* ((data (cdr row))
                    (readiness (mevedel-working-note-files-eligibility
                                (and (plist-get data :modified)
                                     (float-time (plist-get data :modified))) now))
                    (age (plist-get readiness :age-seconds))
                    (reason (if (plist-get data :error) "observation-incomplete"
                              (if (plist-get readiness :eligible)
                                  "age-ready; capture/application safety checks pending"
                                (symbol-name (plist-get readiness :reason))))))
               (format "%s/%s\tage=%s readiness=%s heading=%s%s"
                       (string-remove-suffix "/" address)
                       (mapconcat (lambda (part) (url-hexify-string part)) (split-string (car row) "/") "/")
                       (if age (format "%.2fd" (/ age 86400.0)) "unknown")
                       reason
                       (if (eq (plist-get data :kind) 'text)
                           (mevedel-working-note-files-heading (plist-get data :text)) "")
                       (if-let* ((diagnostic (plist-get data :error)))
                           (concat " diagnostic=" diagnostic)
                         (if (plist-get data :truncated) " observation=prefix-only" ""))))) rows)))
    (string-join
     (append lines
             (mapcar (lambda (problem) (format "Observation incomplete: %s (%s)" (car problem) (cdr problem)))
                     (plist-get inventory :problems))
             (unless complete (list "Observation incomplete; narrow the directory or retry unavailable reads."))
             (unless (or lines (not complete)) (list (format "No files found under %s" address)))) "\n")))

(provide 'mevedel-working-note-files)
;;; mevedel-working-note-files.el ends here
