;;; mevedel-collaboration-projection.el --- canonical collaboration projection -*- lexical-binding: t; -*-

;;; Commentary:

;; Reconstructs the allowlisted collaboration records from the authoritative
;; data buffer and tracks live tool records until canonical settlement.

;;; Code:

;; `json'
(declare-function json-encode "json" (object))

;; `mevedel-collaboration'
(defvar mevedel-collaboration--room)

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (start end))

;; `mevedel-view-render'
(declare-function mevedel-view--tool-call-parse
                  "mevedel-view-render" (data-buf start end &optional raw))
(declare-function mevedel-view--user-turn-text
                  "mevedel-view-render" (segments data-buf))
(declare-function mevedel-view--visible-response-text
                  "mevedel-view-render" (text))

(defconst mevedel-collaboration--protocol-version 1)
(defconst mevedel-collaboration--max-message-bytes (* 1 1024 1024))
(defconst mevedel-collaboration--max-record-text-bytes
  (/ (* 1 1024 1024) 6)
  "Raw text bytes one projected record carries.
The wire bound applies to encoded JSON, where escaping expands one byte at
most six-fold, so bounding the raw text keeps every record sendable without
measuring an encoding that would only be rejected later.")
(defconst mevedel-collaboration--max-tool-result-bytes 50000)


;;
;;; Canonical records


(defun mevedel-collaboration--truncate-bytes (string limit)
  "Return STRING no longer than LIMIT bytes, with a truncation marker."
  (if (<= (string-bytes string) limit)
      string
    (let ((marker "\n[truncated]")
          (max (max 0 (- limit (string-bytes "\n[truncated]"))))
          (low 0)
          (high (length string)))
      ;; Search by character index so a multibyte character is never split.
      ;; Each candidate is measured in encoded bytes, keeping large rejected
      ;; HTTP and transcript payloads linearithmic instead of quadratic.
      (while (< low high)
        (let ((mid (ceiling (+ low high) 2)))
          (if (<= (string-bytes (substring string 0 mid)) max)
              (setq low mid)
            (setq high (1- mid)))))
      (concat (substring string 0 low) marker))))

(defun mevedel-collaboration--clean-response (text)
  "Return the visible, text-only form of assistant TEXT."
  (require 'mevedel-view-render)
  (let ((visible (mevedel-view--visible-response-text text)))
    (unless (stringp visible)
      (error "Canonical response projection failed"))
    (string-trim visible)))

(defun mevedel-collaboration--clean-user (segment data-buffer)
  "Return visible user text for SEGMENT from DATA-BUFFER."
  (require 'mevedel-view-render)
  (let ((visible (mevedel-view--user-turn-text (list segment) data-buffer)))
    (unless (stringp visible)
      (error "Canonical user projection failed"))
    (string-trim visible)))

(defun mevedel-collaboration--record (id kind &rest fields)
  "Build a projection record with ID, KIND, and FIELDS."
  (append (list :id id :kind kind) fields))

(defun mevedel-collaboration--stable-record-id (kind text &optional occurrence)
  "Return a content identity for KIND and TEXT.

Transcript segments do not carry durable positions for user or response
turns.  A digest gives newly opened rooms an ID independent of buffer
positions; OCCURRENCE distinguishes repeated identical records.  Publication
reconciliation below keeps the identity stable while an assistant response
grows in the existing room."
  (format "%s-%s" kind
          (substring (secure-hash
                      'sha256
                      (format "%s\0%s\0%d" kind text (or occurrence 0)))
                     0 24)))

(defun mevedel-collaboration--reuse-record-ids (old new)
  "Reuse room-local IDs from OLD for the ordered records in NEW.

The canonical transcript parser exposes no stable hook identity for a growing
response.  Matching the ordered role streams lets a replacement retain its
room-local ID without using a numeric buffer position or a guessed tool-call
key."
  (let ((by-kind (make-hash-table :test #'eq)))
    (dolist (record old)
      (let ((kind (plist-get record :kind)))
        (puthash kind
                 (append (gethash kind by-kind)
                         (unless (plist-get record :pending)
                           (list record)))
                 by-kind)))
    (mapcar
     (lambda (record)
       (if (plist-get record :identity-fixed)
           record
         (let* ((kind (plist-get record :kind))
                (candidates (gethash kind by-kind))
                (previous (car candidates)))
           (when previous
             (puthash kind (cdr candidates) by-kind)
             (setq record (plist-put record :id
                                     (plist-get previous :id))))
           record)))
     new)))

(defun mevedel-collaboration--record-without-revision (record)
  "Return RECORD without its transport-local revision."
  (let ((copy (copy-sequence record)))
    (setq copy (plist-put copy :revision nil))
    copy))

(defun mevedel-collaboration--json-record (record)
  "Return JSON-safe alist representation of RECORD."
  (let (out)
    (dolist (key '(:id :kind :revision :text :name :status :summary :result
                   :truncated))
      (when (plist-member record key)
        (push (cons (substring (symbol-name key) 1)
                    (plist-get record key))
              out)))
    (nreverse out)))

(defun mevedel-collaboration--json-string (object)
  "Encode OBJECT as compact JSON text."
  (require 'json)
  (json-encode object))


;;
;;; Canonical projection

(defun mevedel-collaboration--tool-record (data-buffer segment &optional occurrence)
  "Return an allowlisted tool record for SEGMENT in DATA-BUFFER."
  (with-current-buffer data-buffer
    (require 'mevedel-view-render)
    (let* ((start (cadr segment))
           (end (caddr segment))
           (parsed (mevedel-view--tool-call-parse data-buffer start end))
           (name (plist-get parsed :name))
           (result (or (plist-get parsed :result) ""))
           (tool-use-id (plist-get parsed :tool-use-id))
           (raw (buffer-substring-no-properties start end)))
      (unless (stringp name)
        (error "Canonical tool projection failed"))
      (let* ((id (if tool-use-id
                     (format "tool-%s" tool-use-id)
                   (mevedel-collaboration--stable-record-id
                    "tool" raw occurrence)))
           (result (string-trim (if (stringp result) result "")))
           (status (if (string-match-p
                        "\\(?:Error:\\|blocked by\\|<tool_call_error>\\)"
                        result)
                       "failed"
                     "completed"))
           (result (mevedel-collaboration--truncate-bytes
                    result mevedel-collaboration--max-tool-result-bytes))
           (truncated (string-suffix-p "\n[truncated]" result)))
      (mevedel-collaboration--record
       id "tool"
       :revision 0
       :name (format "%s" name)
       :status status
       :summary (format "%s" name)
       :result result
       :truncated (and truncated t)
       :identity-fixed (and tool-use-id t))))))

(defun mevedel-collaboration--canonical-records (data-buffer)
  "Return allowlisted records reconstructed from DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (require 'mevedel-transcript)
      (let (records (occurrences (make-hash-table :test #'equal)))
        (dolist (segment (mevedel-transcript-segments
                          (point-min) (point-max)))
          (cond
           ((eq (car segment) 'user)
            (let ((text (mevedel-collaboration--clean-user
                         segment data-buffer)))
              (unless (string-empty-p text)
                (let* ((key (list "user" text))
                       (occurrence (gethash key occurrences 0)))
                  (puthash key (1+ occurrence) occurrences)
                  (push (mevedel-collaboration--record
                         (mevedel-collaboration--stable-record-id
                          "user" text occurrence) "user"
                         :revision 0
                         :text (mevedel-collaboration--truncate-bytes
                                text
                                mevedel-collaboration--max-record-text-bytes))
                        records)))))
           ((eq (car segment) 'response)
            (let ((text (mevedel-collaboration--clean-response
                         (buffer-substring-no-properties
                          (cadr segment) (caddr segment)))))
              (unless (string-empty-p text)
                (let* ((key (list "assistant" text))
                       (occurrence (gethash key occurrences 0)))
                  (puthash key (1+ occurrence) occurrences)
                  (push (mevedel-collaboration--record
                         (mevedel-collaboration--stable-record-id
                          "assistant" text occurrence) "assistant"
                         :revision 0
                         :text (mevedel-collaboration--truncate-bytes
                                text
                                mevedel-collaboration--max-record-text-bytes))
                        records)))))
           ((eq (car segment) 'tool)
            (let* ((start (cadr segment))
                   (end (caddr segment))
                   (raw (buffer-substring-no-properties start end))
                   (key (list "tool" raw))
                   (occurrence (gethash key occurrences 0)))
              (puthash key (1+ occurrence) occurrences)
              (push (mevedel-collaboration--tool-record
                     data-buffer segment occurrence)
                    records)))))
        (nreverse records)))))

(defun mevedel-collaboration--tool-records (records)
  "Return the tool records in RECORDS, preserving their order."
  (let (tools)
    (dolist (record records)
      (when (equal (plist-get record :kind) "tool")
        (push record tools)))
    (nreverse tools)))

(defun mevedel-collaboration--tool-call-fingerprint (info)
  "Return a bounded stable fingerprint for tool-call INFO."
  (let ((print-level 4)
        (print-length 32)
        (print-circle t))
    (prin1-to-string (plist-get info :args))))

(defun mevedel-collaboration--tool-call-key (info)
  "Return the matching key for a gptel tool-call INFO plist."
  (or (and (plist-get info :id)
           (format "id:%s" (plist-get info :id)))
      (and (plist-get info :call-id)
           (format "id:%s" (plist-get info :call-id)))
      (and (plist-get info :tool-call-id)
           (format "id:%s" (plist-get info :tool-call-id)))
      (and (plist-get info :tool_call_id)
           (format "id:%s" (plist-get info :tool_call_id)))
      (format "call:%s\0%s"
              (plist-get info :name)
              (mevedel-collaboration--tool-call-fingerprint info))))

(defun mevedel-collaboration--tool-result-fields (result)
  "Return status, bounded RESULT, and truncation for tool RESULT."
  (let* ((result (if (stringp result) result (format "%s" (or result ""))))
         (result (string-trim result))
         (status (if (string-match-p
                      "\\(?:Error:\\|blocked by\\|<tool_call_error>\\)"
                      result)
                     "failed"
                   "completed"))
         (bounded (mevedel-collaboration--truncate-bytes
                   result mevedel-collaboration--max-tool-result-bytes)))
    (list :status status
          :result bounded
          :truncated (and (string-suffix-p "\n[truncated]" bounded) t))))

(defun mevedel-collaboration--pending-tool-match (info pending)
  "Return non-nil when INFO matches pending tool record PENDING."
  (and (equal (plist-get pending :call-key)
              (mevedel-collaboration--tool-call-key info))
       (equal (plist-get pending :name)
              (format "%s" (plist-get info :name)))))

(defun mevedel-collaboration--project-records (room)
  "Return the current semantic projection for ROOM.

Pending tool records are a live-only projection until their settled
canonical transcript record appears.  A pending record's identity is copied
onto that canonical record, so a viewer updates one card from running through
completion instead of seeing a duplicate tool card."
  (let* ((canonical (mevedel-collaboration--canonical-records
                    (plist-get room :data-buffer)))
         (pending (plist-get room :pending-tools))
         (canonical-tools (mevedel-collaboration--tool-records canonical))
         (remaining nil))
    (dolist (entry pending)
      (let* ((status (plist-get entry :status))
             (baseline (min (length canonical-tools)
                            (max 0 (or (plist-get entry :baseline-tool-count)
                                       0))))
             (candidates (nthcdr baseline canonical-tools))
             candidate)
        (unless (equal status "running")
          (dolist (record candidates)
            (when (and (null candidate)
                       (equal (plist-get record :name)
                              (plist-get entry :name))
                       (equal (plist-get record :status) status)
                       (equal (plist-get record :result)
                              (plist-get entry :result)))
              (setq candidate record))))
        (if candidate
            (let ((index 0)
                  found)
              (dolist (record canonical)
                (when (and (null found) (eq record candidate))
                  (setq found index))
                (setq index (1+ index)))
              (setq index found)
              (setf (nth index canonical)
                    (plist-put (plist-put candidate :id
                                           (plist-get entry :id))
                               :identity-fixed t)))
          (push entry remaining))))
    (setq remaining (nreverse remaining))
    (when (eq room mevedel-collaboration--room)
      (setq mevedel-collaboration--room
            (plist-put mevedel-collaboration--room
                       :pending-tools remaining)))
    (let ((pending-at nil)
          (length (length canonical))
          output)
      (dolist (entry remaining)
        (let* ((baseline (min length
                               (max 0 (or (plist-get entry
                                                   :baseline-record-count)
                                          length))))
               (cell (assq baseline pending-at)))
          (if cell
              (setcdr cell (append (cdr cell) (list entry)))
            (push (cons baseline (list entry)) pending-at))))
      (dotimes (index (1+ length))
        (dolist (entry (cdr (assq index pending-at)))
          (push entry output))
        (when (< index length)
          (push (nth index canonical) output)))
      (nreverse output))))


(provide 'mevedel-collaboration-projection)
;;; mevedel-collaboration-projection.el ends here
