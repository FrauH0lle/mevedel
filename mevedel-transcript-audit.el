;;; mevedel-transcript-audit.el --- Hidden transcript audit records -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the encoding and structural parsing of hidden transcript audit
;; records.  Producers write records; views consume parsed records and spans.

;;; Code:

;; `mevedel-utilities'
(defvar mevedel--hook-audit-close)
(defvar mevedel--hook-audit-open)

;; `subr'
(defvar read-eval)

(defun mevedel--hook-audit-regexp ()
  "Return a regexp matching one hidden hook audit block."
  (concat "\n?"
          (regexp-quote mevedel--hook-audit-open)
          "\\(?:.\\|\n\\)*?"
          (regexp-quote mevedel--hook-audit-close)
          "\n?"))

(defun mevedel--strip-hook-audit-blocks (text)
  "Return TEXT without generated hook audit blocks."
  (replace-regexp-in-string
   (mevedel--hook-audit-regexp) "" (or text "") t t))

(defun mevedel--plain-hook-audit-data (value)
  "Return VALUE stripped of text properties in contained strings."
  (cond
   ((stringp value) (substring-no-properties value))
   ((consp value)
    (cons (mevedel--plain-hook-audit-data (car value))
          (mevedel--plain-hook-audit-data (cdr value))))
   ((vectorp value)
    (apply #'vector (mapcar #'mevedel--plain-hook-audit-data value)))
   (t value)))

(defun mevedel--hook-prompt-rewrite-audit-record
    (event original submitted &optional reason)
  "Return a prompt rewrite audit record for EVENT, or nil if unchanged."
  (when (and (stringp submitted) (not (equal submitted original)))
    (append
     (list :type 'prompt-rewrite
           :event (format "%s" event)
           :original (or original "")
           :submitted submitted)
     (when reason (list :reason reason)))))

(defun mevedel--hook-audit-record-payload (record)
  "Return encoded payload text for hook audit RECORD."
  (base64-encode-string
   (encode-coding-string
    (let ((print-level nil)
          (print-length nil)
          (print-circle t))
      (prin1-to-string (mevedel--plain-hook-audit-data record)))
    'utf-8 t)
   t))

(defun mevedel--read-hook-audit-record (text)
  "Read one encoded hook audit record from TEXT, or nil."
  (condition-case nil
      (let ((read-eval nil))
        (with-temp-buffer
          (insert
           (decode-coding-string
            (base64-decode-string (string-trim (or text "")))
            'utf-8 t))
          (goto-char (point-min))
          (let ((record (read (current-buffer))))
            (and (listp record) (keywordp (car-safe record)) record))))
    (error nil)))

(defun mevedel--format-hook-audit-record (record)
  "Return a hidden transcript side-channel block for hook audit RECORD."
  (propertize
   (concat "\n" mevedel--hook-audit-open "\n"
           (mevedel--hook-audit-record-payload record)
           "\n" mevedel--hook-audit-close "\n")
   'invisible t
   'gptel 'ignore
   'mevedel-hook-audit t))

(defun mevedel-transcript-audit-spans (text &optional type)
  "Return parsed audit spans from TEXT, optionally restricted to TYPE.

Each span is a plist containing `:record', `:start', and `:end'."
  (when (stringp text)
    (let (spans)
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (search-forward mevedel--hook-audit-open nil t)
          (let ((start (- (match-beginning 0) (point-min)))
                (record-start (point)))
            (when (search-forward mevedel--hook-audit-close nil t)
              (when-let* ((record
                           (mevedel--read-hook-audit-record
                            (buffer-substring-no-properties
                             record-start (match-beginning 0))))
                          ((or (null type)
                               (eq (plist-get record :type) type))))
                (push (list :record record
                            :start start
                            :end (- (point) (point-min)))
                      spans))))))
      (nreverse spans))))

(defun mevedel-transcript-audit-records (text &optional type)
  "Return audit records parsed from TEXT, optionally restricted to TYPE."
  (mapcar (lambda (span) (plist-get span :record))
          (mevedel-transcript-audit-spans text type)))

(defun mevedel-transcript-audit-only-p (text)
  "Return non-nil when non-whitespace TEXT consists only of audit blocks."
  (and (stringp text)
       (not (string-empty-p (string-trim text)))
       (mevedel-transcript-audit-spans text)
       (string-empty-p
        (string-trim (mevedel--strip-hook-audit-blocks text)))))

(provide 'mevedel-transcript-audit)

;;; mevedel-transcript-audit.el ends here
