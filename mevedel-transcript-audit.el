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

(defun mevedel-transcript-audit-trusted-range-p (start end &optional object)
  "Return non-nil when START..END is trusted audit data in OBJECT.
OBJECT is a string or buffer and defaults to the current buffer."
  (and (< start end)
       (or
        (and (eq t (get-text-property start 'mevedel-hook-audit object))
             (= end (next-single-property-change
                     start 'mevedel-hook-audit object end)))
        (and (eq 'mevedel-hook-audit
                 (get-text-property start 'gptel object))
             (= end (next-single-property-change start 'gptel object end))))))

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

(defvar mevedel-transcript-audit--decode-cache nil
  "Pure decoded records shared within one projection, or nil outside it.")

(defun mevedel--read-hook-audit-record (text)
  "Read one encoded hook audit record from TEXT, or nil.
Reuse pure decoding within a projection.  Trust checks belong to callers."
  (if (not mevedel-transcript-audit--decode-cache)
      (mevedel-transcript-audit--decode text)
    (if-let* ((entry (gethash text mevedel-transcript-audit--decode-cache)))
        (cdr entry)
      (let ((record (mevedel-transcript-audit--decode text)))
        (puthash text (cons t record) mevedel-transcript-audit--decode-cache)
        record))))

(defun mevedel-transcript-audit--decode (text)
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
   'gptel 'mevedel-hook-audit
   'mevedel-hook-audit t))

(defun mevedel-transcript-audit-guest-prompts ()
  "Return guest attribution positions in the current buffer.
Each element is (POSITION . RECORD) where RECORD is the attribution plist
and POSITION is the buffer position
of the record's audit block, in ascending order.  The prompt a record
attributes is the nearest user turn ending at or before POSITION."
  (save-excursion
    (save-restriction
      (widen)
      (let (result)
        (goto-char (point-min))
        (while (search-forward mevedel--hook-audit-open nil t)
          (let ((start (match-beginning 0))
                (record-start (point)))
            (when (and (mevedel-transcript-audit-trusted-range-p start record-start)
                       (search-forward mevedel--hook-audit-close nil t))
              (when-let* (((mevedel-transcript-audit-trusted-range-p
                            start (point)))
                          (record (mevedel--read-hook-audit-record
                                   (buffer-substring-no-properties
                                    record-start (match-beginning 0))))
                          ((eq (plist-get record :type) 'guest-prompt))
                          ((stringp (plist-get record :name))))
                (push (cons start record) result)))))
        (nreverse result)))))

(defun mevedel-transcript-audit-shared-context (text shared)
  "Return display parts for TEXT with trusted SHARED question attribution.
Only the exact generated suffix after the attributed question is folded.
Edited or mismatched prompts remain fully visible.  This never changes TEXT."
  (let ((question (plist-get shared :text)))
    (when (and (stringp text) (stringp question)
               (not (plist-get shared :edited))
               (string-prefix-p
                (concat question "\n\nShared content snapshot (user-provided data):\n")
                text))
      (list :text question
            :context (string-trim-right (substring text (+ (length question) 2)))
            :label (format "Shared context · %s · %s · revision %s"
                           (replace-regexp-in-string
                            "[\n\r]" " " (or (plist-get shared :title) "Shared item"))
                           (if (equal (plist-get shared :scope) "selection")
                               "Selection" "Whole item")
                           (or (plist-get shared :revision) "?"))))))

(defun mevedel-transcript-audit-spans (text &optional type)
  "Return parsed audit spans from TEXT, optionally restricted to TYPE.

Each span is a plist containing `:record', `:start', and `:end'.

Scans TEXT directly: this runs per transcript segment during live
rendering, and copying TEXT into a temporary buffer first dominated
render allocation."
  (when (stringp text)
    (let ((open-length (length mevedel--hook-audit-open))
          (close-length (length mevedel--hook-audit-close))
          (search 0)
          open spans)
      (while (setq open (string-search mevedel--hook-audit-open text search))
        (let* ((record-start (+ open open-length))
               ;; A quoted opener must not consume a later real record's close.
               (close (and (mevedel-transcript-audit-trusted-range-p
                            open record-start text)
                           (string-search mevedel--hook-audit-close
                                          text record-start))))
          (if (not close)
              (setq search record-start)
            (let ((end (+ close close-length)))
              (when-let* (((mevedel-transcript-audit-trusted-range-p
                            open end text))
                          (record
                           (mevedel--read-hook-audit-record
                            (substring-no-properties
                             text record-start close)))
                          ((or (null type)
                               (eq (plist-get record :type) type))))
                (push (list :record record
                            :start open
                            :end end)
                      spans))
              (setq search end)))))
      (nreverse spans))))

(defvar-local mevedel-transcript-audit--buffer-records nil
  "Bounded pure decoding memo for this buffer; never a provenance cache.")

(defvar-local mevedel-transcript-audit--buffer-record-bytes 0
  "Encoded bytes retained in the buffer decoding memo.")

(defun mevedel-transcript-audit-buffer-spans (&optional type)
  "Return current buffer audit spans, optionally restricted to TYPE.
Positions are absolute.  Check current provenance and exact payload bytes on
every scan, reusing only pure decoding.  Never copy the whole transcript.
Keep at most 128 payloads totaling 4 MiB, with a 1 MiB per-payload limit."
  (unless mevedel-transcript-audit--buffer-records
    (setq mevedel-transcript-audit--buffer-records (make-hash-table :test #'equal)))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (spans)
        (while (search-forward mevedel--hook-audit-open nil t)
          (let ((start (match-beginning 0)) (body (point)))
            (when (and (mevedel-transcript-audit-trusted-range-p start body)
                       (search-forward mevedel--hook-audit-close nil t))
              (let ((end (point)) (close (match-beginning 0)))
                (when (mevedel-transcript-audit-trusted-range-p start end)
                  (let* ((text (buffer-substring-no-properties body close))
                         (memo mevedel-transcript-audit--buffer-records)
                         (entry (gethash text memo)))
                    (unless entry
                      (setq entry (cons t (mevedel--read-hook-audit-record text)))
                      (when (<= (string-bytes text) (* 1024 1024))
                        (when (or (>= (hash-table-count memo) 128)
                                  (> (+ mevedel-transcript-audit--buffer-record-bytes
                                        (string-bytes text)) (* 4 1024 1024)))
                          (clrhash memo)
                          (setq mevedel-transcript-audit--buffer-record-bytes 0))
                        (setq mevedel-transcript-audit--buffer-record-bytes
                              (+ mevedel-transcript-audit--buffer-record-bytes (string-bytes text)))
                        (puthash text entry memo)))
                    (when (and (cdr entry)
                               (or (null type) (eq (plist-get (cdr entry) :type) type)))
                      (push (list :record (cdr entry) :start start :end end) spans))))))))
        (nreverse spans)))))

(defun mevedel-transcript-audit-records (text &optional type)
  "Return audit records parsed from TEXT, optionally restricted to TYPE."
  (mapcar (lambda (span) (plist-get span :record))
          (mevedel-transcript-audit-spans text type)))

(defun mevedel-transcript--audit-block-start (text start)
  "Return START including TEXT's generated leading newline when present.
TEXT may also be the current buffer."
  (if (if (bufferp text)
          (eq (char-before start) ?\n)
        (and (> start 0) (eq (aref text (1- start)) ?\n)))
      (1- start)
    start))

(defun mevedel-transcript--audit-block-end (text end)
  "Return END including TEXT's generated trailing newline when present.
TEXT may also be the current buffer."
  (if (if (bufferp text)
          (eq (char-after end) ?\n)
        (and (< end (length text)) (eq (aref text end) ?\n)))
      (1+ end)
    end))

(defun mevedel--strip-hook-audit-blocks (text)
  "Return TEXT without trusted hook audit blocks."
  (let ((text (or text ""))
        (cursor 0)
        parts)
    (dolist (span (mevedel-transcript-audit-spans text))
      (let ((start (mevedel-transcript--audit-block-start
                    text (plist-get span :start)))
            (end (mevedel-transcript--audit-block-end
                  text (plist-get span :end))))
        (push (substring text cursor start) parts)
        (setq cursor end)))
    (if (null parts)
        text
      (push (substring text cursor) parts)
      (apply #'concat (nreverse parts)))))

(defun mevedel-transcript-directive-ranges (text &optional allow-open)
  "Return directive turn ranges parsed from TEXT or the current buffer.
Signal when directive boundaries are unmatched, nested, or disagree on
directive identity or reserved turn.  When ALLOW-OPEN is non-nil, include
one unmatched final start as a running range through the end of TEXT."
  (let ((spans (if (bufferp text)
                   (mevedel-transcript-audit-buffer-spans 'directive-turn-boundary)
                 (mevedel-transcript-audit-spans text 'directive-turn-boundary)))
        open
        ranges)
    (dolist (span spans)
      (let* ((record (plist-get span :record))
             (edge (plist-get record :edge)))
        (pcase edge
          ('start
           (when open
             (error "Nested directive turn boundaries"))
           (setq open span))
          ('end
           (unless open
             (error "Directive turn end has no matching start"))
           (let ((start-record (plist-get open :record)))
             (unless (and
                      (equal (plist-get start-record :directive-id)
                             (plist-get record :directive-id))
                      (equal (plist-get start-record :turn)
                             (plist-get record :turn)))
               (error "Directive turn boundaries do not match"))
             (push
              (list
               :start (mevedel-transcript--audit-block-start
                       text (plist-get open :start))
               :body-start (mevedel-transcript--audit-block-end
                            text (plist-get open :end))
               :body-end (mevedel-transcript--audit-block-start
                          text (plist-get span :start))
               :end (mevedel-transcript--audit-block-end
                     text (plist-get span :end))
               :directive-id (plist-get record :directive-id)
               :action (plist-get start-record :action)
               :turn (plist-get record :turn)
               :outcome (plist-get record :outcome)
               :activity-kind (plist-get record :activity-kind)
               :sequence (plist-get record :sequence)
               :start-record start-record
               :end-record record)
              ranges)
             (setq open nil)))
          (_ (error "Unknown directive turn boundary edge: %S" edge)))))
    (when open
      (if allow-open
          (let ((record (plist-get open :record)))
            (push
             (list
              :start (mevedel-transcript--audit-block-start
                      text (plist-get open :start))
              :body-start (mevedel-transcript--audit-block-end
                           text (plist-get open :end))
              :body-end (if (bufferp text) (point-max) (length text))
              :end (if (bufferp text) (point-max) (length text))
              :directive-id (plist-get record :directive-id)
              :action (plist-get record :action)
              :turn (plist-get record :turn)
              :outcome 'running
              :start-record record)
             ranges))
        (error "Directive turn start has no matching end")))
    (nreverse ranges)))

(defun mevedel-transcript-buffer-directive-ranges (&optional allow-open)
  "Return the current buffer's directive ranges as buffer positions.
ALLOW-OPEN is forwarded to `mevedel-transcript-directive-ranges'."
  (save-restriction
    (widen)
    (mevedel-transcript-directive-ranges (current-buffer) allow-open)))

(defun mevedel-transcript-exclude-directive-turns (&optional _fsm)
  "Mark directive bodies ignored in the current request-copy buffer."
  (let ((text (buffer-substring (point-min) (point-max))))
    (dolist (range (mevedel-transcript-directive-ranges text))
      (add-text-properties
       (+ (point-min) (plist-get range :body-start))
       (+ (point-min) (plist-get range :body-end))
       '(gptel ignore)))))

(defun mevedel-transcript-audit-only-p (text)
  "Return non-nil when non-whitespace TEXT consists only of audit blocks."
  (when (stringp text)
    (let ((spans (mevedel-transcript-audit-spans text))
          (cursor 0))
      (and spans
           (catch 'visible
             ;; Inspect the gaps without copying TEXT or decoding the same
             ;; audit bodies a second time merely to remove them.
             (dolist (span spans)
               (when-let* ((nonblank (string-match-p "[^ \t\r\n]" text cursor))
                           ((< nonblank (plist-get span :start))))
                 (throw 'visible nil))
               (setq cursor (plist-get span :end)))
             (not (string-match-p "[^ \t\r\n]" text cursor)))))))

(provide 'mevedel-transcript-audit)

;;; mevedel-transcript-audit.el ends here
