;;; niwa-transcript-audit.el --- Hidden transcript audit records -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the encoding and structural parsing of hidden transcript audit
;; records.  Producers write records; views consume parsed records and spans.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `niwa-utilities'
(defvar niwa--hook-audit-close)
(defvar niwa--hook-audit-open)

;; `subr'
(defvar read-eval)

(defun niwa-transcript-audit-trusted-range-p (start end &optional object)
  "Return non-nil when START..END is trusted audit data in OBJECT.
OBJECT is a string or buffer and defaults to the current buffer."
  (and (< start end)
       (or
        (and (eq t (get-text-property start 'niwa-hook-audit object))
             (= end (next-single-property-change
                     start 'niwa-hook-audit object end)))
        (and (eq 'niwa-hook-audit
                 (get-text-property start 'gptel object))
             (= end (next-single-property-change start 'gptel object end))))))

(defun niwa--plain-hook-audit-data (value)
  "Return VALUE stripped of text properties in contained strings."
  (cond
   ((stringp value) (substring-no-properties value))
   ((consp value)
    (cons (niwa--plain-hook-audit-data (car value))
          (niwa--plain-hook-audit-data (cdr value))))
   ((vectorp value)
    (apply #'vector (mapcar #'niwa--plain-hook-audit-data value)))
   (t value)))

(defun niwa--hook-prompt-rewrite-audit-record
    (event original submitted &optional reason)
  "Return a prompt rewrite audit record for EVENT, or nil if unchanged."
  (when (and (stringp submitted) (not (equal submitted original)))
    (append
     (list :type 'prompt-rewrite
           :event (format "%s" event)
           :original (or original "")
           :submitted submitted)
     (when reason (list :reason reason)))))

(defun niwa--hook-audit-record-payload (record)
  "Return encoded payload text for hook audit RECORD."
  (base64-encode-string
   (encode-coding-string
    (let ((print-level nil)
          (print-length nil)
          (print-circle t))
      (prin1-to-string (niwa--plain-hook-audit-data record)))
    'utf-8 t)
   t))

(defvar niwa-transcript-audit--decode-cache nil
  "Pure decoded records shared within one projection, or nil outside it.")

(defvar-local niwa-transcript-audit--buffer-records nil
  "Bounded pure decoding memo for this buffer; never a provenance cache.")

(defvar-local niwa-transcript-audit--buffer-record-bytes 0
  "Encoded bytes retained in the buffer decoding memo.")

(defun niwa--read-hook-audit-record (text)
  "Read one encoded hook audit record from TEXT, or nil.
Reuse pure decoding within a projection and across calls in this buffer.
Keep at most 1024 payloads totaling 4 MiB, with a 1 MiB per-payload limit.
Trust checks always belong to callers; cached records convey no authority."
  (unless niwa-transcript-audit--buffer-records
    (setq niwa-transcript-audit--buffer-records (make-hash-table :test #'equal)))
  (let* ((memo niwa-transcript-audit--buffer-records)
         (entry (or (and niwa-transcript-audit--decode-cache
                         (gethash text niwa-transcript-audit--decode-cache))
                    (gethash text memo))))
    (unless entry
      (setq entry (cons t (niwa-transcript-audit--decode text)))
      (let ((bytes (string-bytes (or text ""))))
        (when (<= bytes (* 1024 1024))
          (when (or (>= (hash-table-count memo) 1024)
                    (> (+ niwa-transcript-audit--buffer-record-bytes bytes)
                       (* 4 1024 1024)))
            (clrhash memo)
            (setq niwa-transcript-audit--buffer-record-bytes 0))
          (cl-incf niwa-transcript-audit--buffer-record-bytes bytes)
          (puthash text entry memo))))
    (when niwa-transcript-audit--decode-cache
      (puthash text entry niwa-transcript-audit--decode-cache))
    (cdr entry)))

(defun niwa-transcript-audit--payload-type (object start end)
  "Return (t . TYPE) when the payload START..END in OBJECT names its TYPE.
OBJECT is a string, or nil for the current buffer.  Only the first base64
block is decoded: a printed record that begins with its `:type' settles what
`plist-get' would answer for the whole record, so a scan for another type
can skip the payload without copying or decoding it.  Return nil when the
head does not settle the type; the caller then reads the whole payload."
  (let* ((start (if object
                    (or (string-match-p "[^ \t\r\n]" object start) end)
                  (save-excursion
                    (goto-char start)
                    (skip-chars-forward " \t\r\n" end)
                    (point))))
         (count (* 4 (/ (min 64 (max 0 (- end start))) 4)))
         (head (and (> count 0)
                    (ignore-errors
                      (base64-decode-string
                       (if object
                           (substring-no-properties object start (+ start count))
                         (buffer-substring-no-properties
                          start (+ start count))))))))
    (when (and head (string-match "\\`(:type \\([-a-z0-9]+\\)[ )]" head))
      (cons t (intern (match-string 1 head))))))

(defvar-local niwa-transcript-audit--position-records nil
  "(TICK . TABLE) mapping payload starts to (END . RECORD) at TICK.
Pure decoding only, like `niwa-transcript-audit--buffer-records'.  At an
unchanged modification tick a payload's text is unchanged, so its record
is found without copying the payload to look it up by content.")

(defun niwa-transcript-audit--buffer-record (start end)
  "Return the record decoded from current buffer payload START..END."
  (let ((tick (buffer-modified-tick))
        (memo niwa-transcript-audit--position-records))
    (unless (eql tick (car memo))
      (setq memo (cons tick (if memo
                                (progn (clrhash (cdr memo)) (cdr memo))
                              (make-hash-table :test #'eql)))
            niwa-transcript-audit--position-records memo))
    (let ((entry (gethash start (cdr memo))))
      (if (eql end (car entry))
          (cdr entry)
        (let ((record (niwa--read-hook-audit-record
                       (buffer-substring-no-properties start end))))
          (puthash start (cons end record) (cdr memo))
          record)))))

(defun niwa-transcript-audit--typed-record (object start end type)
  "Return the record in payload START..END of OBJECT when it has TYPE.
OBJECT is a string, or nil for the current buffer.  With TYPE nil return
any valid record."
  (unless (when-let* ((type type)
                      (named (niwa-transcript-audit--payload-type
                              object start end)))
            (not (eq (cdr named) type)))
    (when-let* ((record (if object
                            (niwa--read-hook-audit-record
                             (substring-no-properties object start end))
                          (niwa-transcript-audit--buffer-record start end)))
                ((or (null type) (eq (plist-get record :type) type))))
      record)))

(defun niwa-transcript-audit--decode (text)
  "Read one encoded hook audit record from TEXT, or nil."
  (condition-case nil
      (let* ((read-eval nil)
             (record (car (read-from-string
                           (decode-coding-string
                            (base64-decode-string (string-trim (or text "")))
                            'utf-8 t)))))
        (and (listp record) (keywordp (car-safe record)) record))
    (error nil)))

(defun niwa--format-hook-audit-record (record)
  "Return a hidden transcript side-channel block for hook audit RECORD."
  (propertize
   (concat "\n" niwa--hook-audit-open "\n"
           (niwa--hook-audit-record-payload record)
           "\n" niwa--hook-audit-close "\n")
   'invisible t
   'gptel 'niwa-hook-audit
   'niwa-hook-audit t))

(defvar-local niwa-transcript-audit--guest-prompts-memo nil
  "(TICK ATTRIBUTIONS . GUESTS) from the last whole-buffer attribution scan.")

(defun niwa-transcript-audit--attribution-memo ()
  "Return the current (ATTRIBUTIONS . GUESTS) turn attribution scan."
  (let ((tick (buffer-modified-tick)))
    (if (eql tick (car niwa-transcript-audit--guest-prompts-memo))
        (cdr niwa-transcript-audit--guest-prompts-memo)
      (let* ((all (save-match-data
                    (niwa-transcript-audit--scan-guest-prompts)))
             (result
              (cons all
                    (cl-remove-if-not
                     (lambda (entry)
                       (eq (plist-get (cdr entry) :type) 'guest-prompt))
                     all))))
        (setq niwa-transcript-audit--guest-prompts-memo (cons tick result))
        result))))

(defun niwa-transcript-audit-guest-prompts ()
  "Return guest attribution positions in the current buffer.
Each element is (POSITION . RECORD) where RECORD is the attribution plist
and POSITION is the buffer position
of the record's audit block, in ascending order.  The prompt a record
attributes is the nearest user turn ending at or before POSITION.
The result is shared until the buffer's text or properties change;
callers must not modify it.  A full projection asks once per prompt, and
rescanning every audit payload each time dominated its allocation."
  (cdr (niwa-transcript-audit--attribution-memo)))

(defun niwa-transcript-audit-turn-attributions ()
  "Return every user turn attribution position in the current buffer.
Like `niwa-transcript-audit-guest-prompts', but also includes
background-event attributions, whose turns the harness started for
background work rather than for a person."
  (car (niwa-transcript-audit--attribution-memo)))

(defun niwa-transcript-audit--scan-guest-prompts ()
  "Return guest and background-event attributions in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (let (result)
        (goto-char (point-min))
        (while (search-forward niwa--hook-audit-open nil t)
          (let ((start (match-beginning 0))
                (record-start (point)))
            (when (and (niwa-transcript-audit-trusted-range-p start record-start)
                       (search-forward niwa--hook-audit-close nil t))
              (when-let* ((record-end (match-beginning 0))
                          ((niwa-transcript-audit-trusted-range-p
                            start (point)))
                          (record
                           (let ((named (niwa-transcript-audit--payload-type
                                         nil record-start record-end)))
                             (if named
                                 (and (memq (cdr named)
                                            '(guest-prompt background-event))
                                      (niwa-transcript-audit--typed-record
                                       nil record-start record-end (cdr named)))
                               (let ((any (niwa-transcript-audit--typed-record
                                           nil record-start record-end nil)))
                                 (and (memq (plist-get any :type)
                                            '(guest-prompt background-event))
                                      any)))))
                          ((or (eq (plist-get record :type) 'background-event)
                               (stringp (plist-get record :name)))))
                (push (cons start record) result)))))
        (nreverse result)))))

(defun niwa-transcript-audit-shared-context (text shared)
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
            :label (let ((flat (lambda (value fallback)
                                 (replace-regexp-in-string
                                  "[\n\r]" " " (or value fallback)))))
                     (format "Discussion · %s · %s"
                             (funcall flat (plist-get shared :title) "Shared item")
                             (funcall flat (plist-get shared :targetLabel)
                                      (if (equal (plist-get shared :scope) "selection")
                                          "Selection" "Whole item"))))))))

(defun niwa-transcript-audit--string-ranges (text)
  "Return trusted audit block ranges in TEXT without decoding them.
Each range is (OPEN RECORD-START CLOSE END)."
  (let ((open-length (length niwa--hook-audit-open))
        (close-length (length niwa--hook-audit-close))
        (search 0)
        open ranges)
    (while (setq open (string-search niwa--hook-audit-open text search))
      (let* ((record-start (+ open open-length))
             ;; A quoted opener must not consume a later real record's close.
             (close (and (niwa-transcript-audit-trusted-range-p
                          open record-start text)
                         (string-search niwa--hook-audit-close
                                        text record-start))))
        (if (not close)
            (setq search record-start)
          (let ((end (+ close close-length)))
            (when (niwa-transcript-audit-trusted-range-p open end text)
              (push (list open record-start close end) ranges))
            (setq search end)))))
    (nreverse ranges)))

(defun niwa-transcript-audit--string-record (text range &optional type)
  "Return the record TEXT encodes within trusted RANGE, or nil.
With TYPE, return only a record of that type."
  (niwa-transcript-audit--typed-record
   text (nth 1 range) (nth 2 range) type))

(defun niwa-transcript-audit-spans (text &optional type)
  "Return parsed audit spans from TEXT, optionally restricted to TYPE.

Each span is a plist containing `:record', `:start', and `:end'.

Scans TEXT directly: this runs per transcript segment during live
rendering, and copying TEXT into a temporary buffer first dominated
render allocation."
  (when (stringp text)
    (let (spans)
      (dolist (range (niwa-transcript-audit--string-ranges text))
        (when-let* ((record (niwa-transcript-audit--string-record
                             text range type)))
          (push (list :record record :start (nth 0 range) :end (nth 3 range))
                spans)))
      (nreverse spans))))

(defun niwa-transcript-audit-buffer-spans (&optional type start end)
  "Return current buffer audit spans, optionally restricted to TYPE.
Positions are absolute.  START and END bound the scan, defaulting to the
whole widened buffer.  Partial records at either bound are excluded.
Check current provenance and exact payload bytes on every scan, reusing
only pure decoding.  Never copy the whole transcript."
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region (or start (point-min)) (or end (point-max)))
      (goto-char (point-min))
      (let (spans)
        (while (search-forward niwa--hook-audit-open nil t)
          (let ((open (match-beginning 0)) (body (point)))
            (when (and (niwa-transcript-audit-trusted-range-p open body)
                       (search-forward niwa--hook-audit-close nil t))
              (let ((close (match-beginning 0)) (finish (point)))
                (when-let* (((niwa-transcript-audit-trusted-range-p open finish))
                            (record (niwa-transcript-audit--typed-record
                                     nil body close type)))
                  (push (list :record record :start open :end finish) spans))))))
        (nreverse spans)))))

(defun niwa-transcript-audit-records (text &optional type)
  "Return audit records parsed from TEXT, optionally restricted to TYPE."
  (mapcar (lambda (span) (plist-get span :record))
          (niwa-transcript-audit-spans text type)))

(defun niwa-transcript--audit-block-start (text start)
  "Return START including TEXT's generated leading newline when present.
TEXT may also be the current buffer."
  (if (if (bufferp text)
          (eq (char-before start) ?\n)
        (and (> start 0) (eq (aref text (1- start)) ?\n)))
      (1- start)
    start))

(defun niwa-transcript--audit-block-end (text end)
  "Return END including TEXT's generated trailing newline when present.
TEXT may also be the current buffer."
  (if (if (bufferp text)
          (eq (char-after end) ?\n)
        (and (< end (length text)) (eq (aref text end) ?\n)))
      (1+ end)
    end))

(defun niwa--strip-hook-audit-blocks (text)
  "Return TEXT without trusted hook audit blocks."
  (let ((text (or text ""))
        (cursor 0)
        parts)
    (dolist (span (niwa-transcript-audit-spans text))
      (let ((start (niwa-transcript--audit-block-start
                    text (plist-get span :start)))
            (end (niwa-transcript--audit-block-end
                  text (plist-get span :end))))
        (push (substring text cursor start) parts)
        (setq cursor end)))
    (if (null parts)
        text
      (push (substring text cursor) parts)
      (apply #'concat (nreverse parts)))))

(defun niwa-transcript-audit-only-p (text)
  "Return non-nil when non-whitespace TEXT consists only of audit blocks."
  (when (stringp text)
    (let ((ranges (niwa-transcript-audit--string-ranges text))
          (cursor 0))
      (and ranges
           ;; Visible text in any gap settles the answer, and an undecodable
           ;; block only adds visible text, so decode once the gaps are blank.
           (catch 'visible
             (dolist (range ranges)
               (when-let* ((nonblank (string-match-p "[^ \t\r\n]" text cursor))
                           ((< nonblank (nth 0 range))))
                 (throw 'visible nil))
               (setq cursor (nth 3 range)))
             (not (string-match-p "[^ \t\r\n]" text cursor)))
           (cl-every (lambda (range)
                       (niwa-transcript-audit--string-record text range))
                     ranges)))))

(defun niwa-transcript-audit-buffer-only-p (start end)
  "Return non-nil when non-whitespace START..END holds only audit blocks.
This is `niwa-transcript-audit-only-p' for a region of the current buffer,
without copying it."
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((open-length (length niwa--hook-audit-open))
            found)
        (catch 'visible
          (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
            (let* ((open (point))
                   (body (+ open open-length)))
              (unless (and (<= body (point-max))
                           (search-forward niwa--hook-audit-open body t)
                           (niwa-transcript-audit-trusted-range-p open body)
                           (search-forward niwa--hook-audit-close nil t)
                           (niwa-transcript-audit-trusted-range-p
                            open (point))
                           (niwa-transcript-audit--typed-record
                            nil body (match-beginning 0) nil))
                (throw 'visible nil))
              (setq found t)))
          found)))))

(provide 'niwa-transcript-audit)

;;; niwa-transcript-audit.el ends here
