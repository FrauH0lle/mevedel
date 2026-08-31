;;; mevedel-collaboration-projection.el --- canonical collaboration projection -*- lexical-binding: t; -*-

;;; Commentary:

;; Reconstructs the allowlisted collaboration records from the authoritative
;; data buffer and tracks live tool records until canonical settlement.

;;; Code:

;; `json'
(declare-function json-encode "json" (object))

;; `mevedel-collaboration-artifact-projection'
(declare-function mevedel-collaboration--artifact-fields
                  "mevedel-collaboration-artifact-projection" (render-data))

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (start end))

;; `mevedel-transcript-audit'
(declare-function mevedel--strip-hook-audit-blocks
                  "mevedel-transcript-audit" (text))

;; `mevedel-utilities'
(declare-function mevedel--trim-tool-result "mevedel-utilities" (text))
(declare-function mevedel-transcript-audit-guest-prompts
                  "mevedel-transcript-audit" ())
(declare-function mevedel-transcript-buffer-directive-ranges
                  "mevedel-transcript-audit" (&optional allow-open))

;; `mevedel-view-render'
(declare-function mevedel-view--tool-call-parse
                  "mevedel-view-render" (data-buf start end &optional raw))
(declare-function mevedel-view--user-turn-text
                  "mevedel-view-render" (segments data-buf))
(declare-function mevedel-view--visible-response-text
                  "mevedel-view-render" (text))

(require 'json)
(require 'mevedel-collaboration-artifact-projection)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-utilities)
(require 'mevedel-view-render)

(defconst mevedel-collaboration--protocol-version 2)
(defconst mevedel-collaboration--max-record-text-bytes
  (/ (- (* 1 1024 1024) 4096) 6)
  "Raw text bytes one projected record carries.
The wire bound applies to the encoded frame, where escaping expands one byte
at most six-fold, so bounding the raw text keeps every record sendable
without measuring an encoding that would only be rejected later.  The
reserve covers the record's own keys and the frame around it, which the
six-fold worst case alone does not.")
(defconst mevedel-collaboration--max-tool-result-bytes 50000)
(defconst mevedel-collaboration--tool-error-regexp
  "\\(?:Error:\\|blocked by\\|<tool_call_error>\\)"
  "Result text marking a settled tool call as failed.")


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
  "Return the visible, text-only form of assistant TEXT.
Audit blocks are stripped defensively: a hidden record swallowed into a
response span must never reach the wire as visible text."
  (let ((visible (mevedel-view--visible-response-text
                  (mevedel--strip-hook-audit-blocks text))))
    (unless (stringp visible)
      (error "Canonical response projection failed"))
    (string-trim visible)))

(defun mevedel-collaboration--clean-user (segment data-buffer)
  "Return visible user text for SEGMENT from DATA-BUFFER."
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
  "Return JSON-safe alist representation of RECORD.
An artifact record's `:artifact-path' stays host-side: guests address
an artifact only by its record id, never by a filesystem path."
  (let (out)
    (dolist (key '(:id :kind :revision :text :name :status :summary :result
                       :truncated :guest :directive :detail :diff
                       :artifact :size :missing))
      (when (plist-member record key)
        (push (cons (substring (symbol-name key) 1)
                    (plist-get record key))
              out)))
    (nreverse out)))

(defun mevedel-collaboration--json-string (object)
  "Encode OBJECT as compact JSON text."
  (json-encode object))


;;
;;; Canonical projection

(defun mevedel-collaboration--tool-detail (args)
  "Return a bounded one-line viewer summary of tool ARGS, or nil.
The detail is the tool's primary operand -- a command, path, pattern, or
query -- so a collapsed tool row says what the call did."
  (let ((value (and (listp args)
                    (cl-loop for key in '(:command :file_path :pattern
                                          :query :url :path :name)
                             for found = (plist-get args key)
                             when (stringp found) return found))))
    (when value
      (mevedel-collaboration--truncate-bytes
       (car (split-string value "\n")) 200))))

(defun mevedel-collaboration--tool-extras (name args)
  "Return the optional :detail and :diff record fields for NAME with ARGS.
Shared by the canonical tool record and the live pending record so both
carry the same operand summary and, for ApplyPatch, the authored patch."
  (append
   (when-let* ((detail (mevedel-collaboration--tool-detail args)))
     (list :detail detail))
   (when-let* (((equal (format "%s" name) "ApplyPatch"))
              (patch (plist-get args :patch))
              ((stringp patch)))
     (list :diff (mevedel-collaboration--truncate-bytes
                  patch mevedel-collaboration--max-tool-result-bytes)))))


(defun mevedel-collaboration--tool-record (parsed raw &optional occurrence)
  "Return an allowlisted tool record from PARSED and transcript RAW text."
  (let* ((name (plist-get parsed :name))
         (result (or (plist-get parsed :result) ""))
         (tool-use-id (plist-get parsed :tool-use-id)))
    (unless (stringp name)
      (error "Canonical tool projection failed"))
    (let* ((id (if tool-use-id
                   (format "tool-%s" tool-use-id)
                 (mevedel-collaboration--stable-record-id
                  "tool" raw occurrence)))
           (result (mevedel--trim-tool-result
                    (if (stringp result) result "")))
           (status (if (string-match-p
                        mevedel-collaboration--tool-error-regexp result)
                       "failed"
                     "completed"))
           (result (mevedel-collaboration--truncate-bytes
                    result mevedel-collaboration--max-tool-result-bytes))
           (truncated (string-suffix-p "\n[truncated]" result)))
      (apply #'mevedel-collaboration--record
             id "tool"
             :revision 0
             :name (format "%s" name)
             :status status
             :summary (format "%s" name)
             :result result
             :truncated (and truncated t)
             :identity-fixed (and tool-use-id t)
             (mevedel-collaboration--tool-extras
              name (plist-get parsed :args))))))

(defun mevedel-collaboration--tool-segment-records
    (data-buffer segment &optional occurrence)
  "Return canonical records for tool SEGMENT in DATA-BUFFER.
A settled ApplyPatch may produce several artifact cards.  A patch touching
only artifact destinations reuses its ordinary tool record as the first card;
a mixed patch retains the ordinary row and adds child cards."
  (with-current-buffer data-buffer
    (let* ((start (cadr segment))
           (end (caddr segment))
           (parsed (mevedel-view--tool-call-parse data-buffer start end))
           (base (mevedel-collaboration--tool-record
                  parsed (buffer-substring-no-properties start end)
                  occurrence))
           (files (and (equal (plist-get base :name) "ApplyPatch")
                       (equal (plist-get base :status) "completed")
                       (mevedel-collaboration--artifact-fields
                        (plist-get parsed :render-data))))
           (all-files (and (eq (plist-get (plist-get parsed :render-data)
                                          :kind)
                               'patch)
                           (plist-get (plist-get parsed :render-data) :files)))
           (pure (and files (= (length files) (length all-files))))
           cards)
      (dolist (fields files)
        (let* ((relative (plist-get fields :artifact))
               (first (null cards))
               (record (if (and pure first)
                           (copy-sequence base)
                         (list :id (format "%s-artifact-%s"
                                           (plist-get base :id)
                                           (substring
                                            (secure-hash 'sha256 relative)
                                            0 12))
                               :kind "tool"
                               :revision 0
                               :name "Artifact"
                               :status "completed"
                               :summary "Artifact"
                               :result ""
                               :truncated nil
                               :identity-fixed t
                               :artifact-child t))))
          (setq record (append record fields))
          (push record cards)))
      (setq cards (nreverse cards))
      (if pure cards (cons base cards)))))

(defun mevedel-collaboration--directive-at (ranges position)
  "Return the directive id owning POSITION per directive RANGES, or nil."
  (cl-loop for range in ranges
           when (and (<= (plist-get range :start) position)
                     (< position (plist-get range :end)))
           return (plist-get range :directive-id)))

(defun mevedel-collaboration--directive-ranges ()
  "Return the current buffer's directive turn ranges, or nil.
A malformed audit grammar degrades to untagged records instead of
failing the whole projection."
  (ignore-errors (mevedel-transcript-buffer-directive-ranges t)))

(defun mevedel-collaboration--attribute-guest-prompts (user-starts)
  "Attach guest names to user records per attribution positions.
USER-STARTS is an ordered list of (SEGMENT-START . RECORD).  Each guest
attribution record names the last user turn starting at or before it.
Starts are the stable anchor: segment repair can grow a user turn's end
over the audit blocks that follow it, or reclassify trailing text, but
the turn always begins before its own attribution block."
  (dolist (attribution (mevedel-transcript-audit-guest-prompts))
    (let (owner)
      (dolist (entry user-starts)
        (when (<= (car entry) (car attribution))
          (setq owner (cdr entry))))
      (when owner
        (plist-put owner :guest (cdr attribution))))))

(defun mevedel-collaboration--canonical-records (data-buffer)
  "Return allowlisted records reconstructed from DATA-BUFFER.
Records inside a directive turn carry that directive's id so a viewer
can filter the transcript to one directive client-side; user records
attributed to a collaboration guest carry that guest's name."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (let ((ranges (mevedel-collaboration--directive-ranges))
            records user-starts (occurrences (make-hash-table :test #'equal)))
        (dolist (segment (mevedel-transcript-segments
                          (point-min) (point-max)))
          (let ((directive (mevedel-collaboration--directive-at
                            ranges (cadr segment))))
            (cond
             ((eq (car segment) 'user)
              (let ((text (mevedel-collaboration--clean-user
                           segment data-buffer)))
                (unless (string-empty-p text)
                  (let* ((key (list "user" text))
                         (occurrence (gethash key occurrences 0)))
                    (puthash key (1+ occurrence) occurrences)
                    (push (apply
                           #'mevedel-collaboration--record
                           (mevedel-collaboration--stable-record-id
                            "user" text occurrence) "user"
                           :revision 0
                           :text (mevedel-collaboration--truncate-bytes
                                  text
                                  mevedel-collaboration--max-record-text-bytes)
                           (when directive (list :directive directive)))
                          records)
                    (push (cons (cadr segment) (car records))
                          user-starts)))))
             ((eq (car segment) 'response)
              (let ((text (mevedel-collaboration--clean-response
                           (buffer-substring
                            (cadr segment) (caddr segment)))))
                (unless (string-empty-p text)
                  (let* ((key (list "assistant" text))
                         (occurrence (gethash key occurrences 0)))
                    (puthash key (1+ occurrence) occurrences)
                    (push (apply
                           #'mevedel-collaboration--record
                           (mevedel-collaboration--stable-record-id
                            "assistant" text occurrence) "assistant"
                           :revision 0
                           :text (mevedel-collaboration--truncate-bytes
                                  text
                                  mevedel-collaboration--max-record-text-bytes)
                           (when directive (list :directive directive)))
                          records)))))
             ((eq (car segment) 'tool)
              (let* ((start (cadr segment))
                     (end (caddr segment))
                     (raw (buffer-substring-no-properties start end))
                     (key (list "tool" raw))
                     (occurrence (gethash key occurrences 0)))
                (puthash key (1+ occurrence) occurrences)
                (dolist (record (mevedel-collaboration--tool-segment-records
                                 data-buffer segment occurrence))
                  (when directive
                    (setq record (plist-put record :directive directive)))
                  (push record records)))))))
        (mevedel-collaboration--attribute-guest-prompts (nreverse user-starts))
        (nreverse records)))))

(defun mevedel-collaboration--tool-records (records)
  "Return the tool records in RECORDS, preserving their order."
  (let (tools)
    (dolist (record records)
      (when (and (equal (plist-get record :kind) "tool")
                 (not (plist-get record :artifact-child)))
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
         (result (mevedel--trim-tool-result result))
         (status (if (string-match-p
                      mevedel-collaboration--tool-error-regexp result)
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
         (claimed nil)
         (remaining nil))
    (dolist (entry pending)
      (let* ((status (plist-get entry :status))
             (baseline (min (length canonical-tools)
                            (max 0 (or (plist-get entry :baseline-tool-count)
                                       0))))
             (candidates (nthcdr baseline canonical-tools))
             exact candidate)
        ;; Prefer the exact settled twin, but fall back to the first
        ;; unclaimed same-name record at or after this entry's baseline:
        ;; the canonical transcript is authoritative once a record lands
        ;; there, and an unmatched pending would otherwise duplicate it
        ;; forever -- as a stuck "running" card when the settlement info
        ;; missed the pending entry, or as a completed twin when the
        ;; transcript-formatted result text diverges from the raw result.
        (dolist (record candidates)
          (unless (memq record claimed)
            (when (equal (plist-get record :name) (plist-get entry :name))
              (unless candidate (setq candidate record))
              (when (and (null exact)
                         (equal (plist-get record :status) status)
                         (equal (plist-get record :result)
                                (plist-get entry :result)))
                (setq exact record)))))
        (setq candidate (or exact candidate))
        (if candidate
            (let ((index 0)
                  found)
              (push candidate claimed)
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
    ;; A room plist is never empty, so this mutates in place and the
    ;; room registry keeps pointing at the same object.
    (setq room (plist-put room :pending-tools remaining))
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
