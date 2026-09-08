;;; mevedel-journal-store.el -- Immutable workspace journal -*- lexical-binding: t -*-

;;; Commentary:

;; Publishes and reads structured digests and completed reviews below the workspace's native
;; .mevedel/journal directory.  Capture owners supply frozen metadata; this
;; module owns the closed public format and exclusive publication.  Private
;; state is never an entry.  All filesystem access uses the target's pinned
;; control operations, including on TRAMP targets.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-context-summary)
(require 'mevedel-session-control-fs)

(define-error 'mevedel-journal-store-invalid "Invalid journal entry")

(defconst mevedel-journal-store--entry-max-bytes (* 8 1024 1024)
  "Maximum public entry bytes, including complete turn coverage and body.")

(defconst mevedel-journal-store-hash-regexp "[0-9a-f]\\{64\\}"
  "Collision-resistant journal identity spelling, for composing file patterns.")

(defconst mevedel-journal-store-id-regexp
  (concat "\\`" mevedel-journal-store-hash-regexp "\\'")
  "Exact journal identity spelling.")

(defun mevedel-journal-store-id-p (id)
  "Return non-nil when ID is an exact lowercase journal identity string."
  (and (stringp id)
       (let ((case-fold-search nil)) (string-match-p mevedel-journal-store-id-regexp id))))

(defconst mevedel-journal-store--time-regexp
  "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z"
  "Canonical UTC journal timestamp spelling.")

(defconst mevedel-journal-store-file-regexp
  (concat "\\`" mevedel-journal-store--time-regexp "-"
          mevedel-journal-store-hash-regexp "\\.md\\'")
  "Exact public journal filename spelling; no subdirectories are allowed.")

;;;###autoload
(defun mevedel-journal-store-file-name-p (name)
  "Return non-nil when NAME spells one public journal entry file."
  (and (stringp name)
       (let ((case-fold-search nil)) (string-match-p mevedel-journal-store-file-regexp name))))

(defconst mevedel-journal-store--fields
  '(("capture-id" . :capture-id) ("session" . :session)
    ("session-name" . :session-name) ("workspace" . :workspace)
    ("trigger" . :trigger) ("segment" . :segment)
    ("source-revision" . :source-revision) ("turns" . :turns)
    ("turn-ids" . :turn-ids)
    ("created" . :created) ("model" . :model))
  "Closed digest capture metadata, in publication order.")

(defconst mevedel-journal-store--review-fields
  '(("pass-id" . :pass-id) ("workspace" . :workspace) ("created" . :created)
    ("model" . :model) ("focus" . :focus) ("digests" . :digests)
    ("proposals" . :proposals) ("references" . :references))
  "Closed completed-review metadata, in publication order.")

(defconst mevedel-journal-store--reference-fields
  '(("topic" . :topic) ("token" . :token) ("scope" . :scope)
    ("result" . :result) ("checked" . :checked))
  "Closed reference-check evidence fields.")

(defconst mevedel-journal-store--decision-fields
  '(("decision-id" . :decision-id) ("pass-id" . :pass-id)
    ("proposal-id" . :proposal-id) ("workspace" . :workspace)
    ("created" . :created) ("status" . :status) ("reason" . :reason)
    ("state-hash" . :state-hash))
  "Closed decision metadata; private before/after state is referenced by hash.")

(defun mevedel-journal-store--fields-for-kind (kind)
  "Return the closed metadata fields for journal KIND."
  (pcase kind
    ('digest mevedel-journal-store--fields)
    ('consolidation mevedel-journal-store--review-fields)
    ('decision mevedel-journal-store--decision-fields)
    (_ (signal 'mevedel-journal-store-invalid '("Unknown journal kind")))))

(defun mevedel-journal-store--decision-body (metadata)
  "Render decision METADATA without copying private proposal evidence."
  (decode-coding-string
   (format "## Decision\n- Proposal: %s\n- Status: %s\n- Reason: %s"
           (plist-get metadata :proposal-id) (plist-get metadata :status)
           (json-serialize (plist-get metadata :reason))) 'utf-8-unix))

(defun mevedel-journal-store-timestamp (seconds)
  "Return the canonical UTC journal timestamp for target clock SECONDS."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" seconds t))

(defun mevedel-journal-store-claim-directory (directory scope)
  "Return the private claim directory for work SCOPE below journal DIRECTORY.
SCOPE is `digest-run' for one digest request at a time, `mutation' for
journal state changes, or `consolidation' for one memory pass at a time."
  (file-name-concat directory "state" (symbol-name scope)))

(defun mevedel-journal-store-entry-for-capture (entries id)
  "Return the entry among ENTRIES published from capture ID, or nil."
  (cl-find id entries :key (lambda (entry) (plist-get entry :capture-id)) :test #'equal))

(defun mevedel-journal-store-directory (root)
  "Return the target journal directory for absolute workspace ROOT.
This computes a path; it does not create any state."
  (file-name-concat
   (mevedel-session-control-fs-physical-path root) ".mevedel" "journal"))

(defun mevedel-journal-store--utc-time-p (value)
  "Return non-nil when VALUE is a canonical UTC timestamp that round-trips."
  (and (stringp value)
       (string-match-p (concat "\\`" mevedel-journal-store--time-regexp "\\'") value)
       (condition-case nil
           ;; Validation spells the format itself so a faked producer clock
           ;; can never make its own output look canonical.
           (equal value (format-time-string "%Y-%m-%dT%H:%M:%SZ" (date-to-time value) t))
         (error nil))))

(defun mevedel-journal-store--reference-p (reference)
  "Return non-nil for closed, bounded reference-check REFERENCE evidence."
  (and (proper-list-p reference) (= (length reference) 10)
       (let (seen)
         (cl-loop for (key value) on reference by #'cddr
                  always (and (rassq key mevedel-journal-store--reference-fields)
                              (not (memq key seen)) (push key seen)
                              (stringp value) (not (string-empty-p value))
                              (<= (string-bytes value) 4096))))
       (member (plist-get reference :result) '("found" "not found" "unknown"))
       (mevedel-journal-store--utc-time-p (plist-get reference :checked))))

(defun mevedel-journal-store--decode-references (encoded)
  "Decode ENCODED reference checks without interning untrusted object keys."
  (let ((rows (json-parse-string encoded)))
    (unless (vectorp rows) (error "Reference checks must be an array"))
    (mapcar
     (lambda (row)
       (unless (and (hash-table-p row) (= (hash-table-count row) 5))
         (error "Invalid reference-check object"))
       (cl-loop for (name . key) in mevedel-journal-store--reference-fields
                append (list key (gethash name row))))
     rows)))

(defun mevedel-journal-store--review-body (metadata)
  "Render the public review summary from validated METADATA.
Private proposal replacement bodies and before-state never enter this text."
  (decode-coding-string
   (concat
   (format "## Review\n- Fully examined digests: %d\n- Proposals: %d\n\n## Focus\n- %s\n\n## Reference checks\n"
           (length (plist-get metadata :digests)) (length (plist-get metadata :proposals))
           (if (string-empty-p (plist-get metadata :focus)) "none"
             (json-serialize (plist-get metadata :focus))))
   (if-let* ((references (plist-get metadata :references)))
       (mapconcat
        (lambda (reference)
          (format "- %s in %s: %s; scope %s; checked %s"
                  (json-serialize (plist-get reference :token))
                  (json-serialize (plist-get reference :topic))
                  (plist-get reference :result)
                  (json-serialize (plist-get reference :scope))
                  (plist-get reference :checked))) references "\n")
     "- none")) 'utf-8-unix))

(cl-defun mevedel-journal-store--validate-metadata (metadata &optional (kind 'digest))
  "Validate frozen METADATA, returning it or signaling an error.
KIND selects the digest, consolidation, or decision schema."
  (unless (and (proper-list-p metadata)
               (= (length metadata) (* 2 (length (mevedel-journal-store--fields-for-kind kind)))))
    (signal 'mevedel-journal-store-invalid '("Invalid metadata fields")))
  (let (keys)
    (cl-loop for (key value) on metadata by #'cddr do
             (unless (and (rassq key (mevedel-journal-store--fields-for-kind kind))
                          (not (memq key keys)))
               (signal 'mevedel-journal-store-invalid '("Unknown or repeated field")))
             (push key keys)
             (unless
                 (pcase key
                   ((or :capture-id :pass-id :decision-id :proposal-id :state-hash :workspace :source-revision)
                    (mevedel-journal-store-id-p value))
                   (:trigger (memq value '(compaction session-end)))
                   ((or :focus :reason) (and (stringp value) (<= (string-bytes value) 4096)))
                   (:status (memq value '(applied rejected stale unavailable recovery-required reversed)))
                   ((or :digests :proposals)
                    (and (proper-list-p value)
                         (or (eq key :proposals) (<= (length value) 20))
                         (= (length value) (length (delete-dups (copy-sequence value))))
                         (cl-every #'mevedel-journal-store-id-p value)))
                   (:references
                    (and (proper-list-p value) (<= (length value) 200)
                         (cl-every #'mevedel-journal-store--reference-p value)))
                   (:segment (natnump value))
                   (:turns
                    (and (consp value) (proper-list-p value)
                         (let ((previous 0))
                           (cl-every (lambda (turn)
                                       (when (and (integerp turn) (> turn previous))
                                         (setq previous turn)))
                                     value))))
                   (:turn-ids
                    (and (proper-list-p value)
                         (proper-list-p (plist-get metadata :turns))
                         (= (length value) (length (plist-get metadata :turns)))
                         (= (length value) (length (delete-dups (copy-sequence value))))
                         (cl-every #'mevedel-journal-store-id-p value)))
                   (:created (mevedel-journal-store--utc-time-p value))
                   (_ (and (stringp value) (not (string-empty-p value))
                           (<= (string-bytes value) 4096))))
               (signal 'mevedel-journal-store-invalid (list "Invalid field" key)))))
  (when (> (string-bytes (mevedel-journal-store--header metadata kind))
           (- mevedel-journal-store--entry-max-bytes
              mevedel-context-summary--digest-max-bytes 1))
    (signal 'mevedel-journal-store-invalid
            '("Metadata leaves insufficient space for a complete digest")))
  metadata)

(cl-defun mevedel-journal-store--record-id (metadata &optional (kind 'digest))
  "Return the stable journal identity for frozen METADATA of KIND."
  (secure-hash 'sha256
               (concat (symbol-name kind) "\0"
                       (plist-get metadata (pcase kind
                                             ('digest :capture-id)
                                             ('consolidation :pass-id)
                                             ('decision :decision-id)
                                             (_ (error "Unknown journal kind")))))))

(cl-defun mevedel-journal-store--filename (metadata &optional (kind 'digest))
  "Return the immutable filename belonging to validated METADATA of KIND."
  (concat (plist-get metadata :created) "-"
          (mevedel-journal-store--record-id metadata kind) ".md"))

(cl-defun mevedel-journal-store--header (metadata &optional (kind 'digest))
  "Encode the public header for structurally validated METADATA of KIND."
  (decode-coding-string
   (concat
   "---\nkind: " (symbol-name kind) "\nid: "
   (json-serialize (mevedel-journal-store--record-id metadata kind)) "\n"
   (mapconcat
    (lambda (field)
      (let ((value (plist-get metadata (cdr field))))
        (concat (car field) ": "
                (pcase (cdr field)
                  ((or :trigger :status) (symbol-name value))
                  ((or :turns :turn-ids :digests :proposals :references)
                   (json-serialize (vconcat value)))
                  (_ (json-serialize value))))))
    (mevedel-journal-store--fields-for-kind kind) "\n")
   "\n---\n\n") 'utf-8-unix))

(cl-defun mevedel-journal-store--encode (metadata body &optional (kind 'digest))
  "Encode METADATA and BODY as public Markdown of KIND.
Consolidation and decision bodies are derived exclusively from metadata."
  (mevedel-journal-store--validate-metadata metadata kind)
  (let ((text (concat (mevedel-journal-store--header metadata kind)
                      (pcase kind
                        ('digest (mevedel-context-summary--validate-output body 'digest))
                        ('consolidation (mevedel-journal-store--review-body metadata))
                        ('decision (mevedel-journal-store--decision-body metadata))) "\n")))
    (when (> (string-bytes text) mevedel-journal-store--entry-max-bytes)
      (signal 'mevedel-journal-store-invalid '("Journal entry exceeds its byte limit")))
    text))

(defun mevedel-journal-store--decode (text file)
  "Decode public TEXT from FILE, rejecting unknown or malformed records."
  (condition-case err
      (progn
        (unless (and (<= (string-bytes text) mevedel-journal-store--entry-max-bytes)
                     (string-prefix-p "---\n" text)
                     (string-match "\n---\n" text 4))
          (error "Invalid frontmatter"))
        (let ((header (substring text 4 (match-beginning 0)))
              (body (substring text (match-end 0)))
              metadata id kind seen)
          (unless (string-match "^kind: \\(digest\\|consolidation\\|decision\\)$" header)
            (error "Unknown journal record kind"))
          (setq kind (intern (match-string 1 header)))
          (dolist (line (split-string header "\n"))
            (unless (string-match "\\`\\([a-z-]+\\): \\(.*\\)\\'" line)
              (error "Invalid frontmatter line"))
            (let* ((name (match-string 1 line))
                   (encoded (match-string 2 line))
                   (field (assoc name (mevedel-journal-store--fields-for-kind kind))))
              (when (member name seen) (error "Repeated frontmatter field"))
              (push name seen)
              (cond
               ((equal name "kind") nil)
               ((equal name "id") (setq id (json-parse-string encoded)))
               (field
                (setq metadata
                      (plist-put
                       metadata (cdr field)
                       (pcase (cdr field)
                         ;; Validation below rejects any symbol outside the closed sets.
                         ((or :trigger :status) (intern-soft encoded))
                         (:references (mevedel-journal-store--decode-references encoded))
                         ((or :turns :turn-ids :digests :proposals)
                          (let ((values (json-parse-string encoded)))
                            (unless (vectorp values) (error "Expected journal metadata array"))
                            (append values nil)))
                         (_ (json-parse-string encoded))))))
               (t (error "Unknown frontmatter field")))))
          (mevedel-journal-store--validate-metadata metadata kind)
          (unless (and (equal id (mevedel-journal-store--record-id metadata kind))
                       (equal file (mevedel-journal-store--filename metadata kind)))
            (error "Record identity does not match its filename"))
          (let ((expected (pcase kind
                            ('consolidation (mevedel-journal-store--review-body metadata))
                            ('decision (mevedel-journal-store--decision-body metadata)))))
            (when (and expected (not (equal (string-trim body) expected)))
              (error "Record body differs from its metadata"))
            (append (list :kind kind :id id :file file) metadata
                    (list :body (or expected
                                    (mevedel-context-summary--validate-output (string-trim body) 'digest))
                          :text text)))))
    (error (signal 'mevedel-journal-store-invalid (list file (error-message-string err))))))

(defun mevedel-journal-store-expired-marker (directory file)
  "Return the private marker that records public entry FILE's expiry.
DIRECTORY is the journal directory."
  (file-name-concat directory "state" "expired" (concat file ".json")))

(defun mevedel-journal-store--assert-current (root file)
  "Signal when public entry FILE in workspace ROOT has been durably expired."
  (when (mevedel-session-control-fs-path-exists-p
         (mevedel-journal-store-expired-marker (mevedel-journal-store-directory root) file))
    (signal 'mevedel-journal-store-invalid '("Journal entry has expired"))))

(defun mevedel-journal-store-read (root file)
  "Return the validated public entry FILE in workspace ROOT.
Reject private state, traversal, symbolic links, and malformed entries."
  (unless (mevedel-journal-store-file-name-p file)
    (signal 'mevedel-journal-store-invalid '("Invalid public entry name")))
  (mevedel-journal-store--assert-current root file)
  (mevedel-journal-store--decode
   (mevedel-session-control-fs-read-file
    (file-name-concat (mevedel-journal-store-directory root) file)
    'utf-8-unix (1+ mevedel-journal-store--entry-max-bytes))
   file))

(defun mevedel-journal-store-entries (root)
  "Return valid published entries in workspace ROOT, newest first.
Ignore incomplete or malformed public records.  Private state is never
read.  Storage errors, including symlink substitution, remain errors."
  (let ((paths (sort (mevedel-session-control-fs-list-directory
                      (mevedel-journal-store-directory root)
                      mevedel-journal-store-file-regexp)
                     #'string>))
        entries)
    (dolist (path paths (nreverse entries))
      (condition-case nil
          (push (mevedel-journal-store-read root (file-name-nondirectory path))
                entries)
        (mevedel-session-control-fs-absent nil)
        (mevedel-journal-store-invalid nil)))))

(defun mevedel-journal-store--read-coverage (path)
  "Return the published turn identities recorded at private coverage PATH.
Malformed records fail closed so completed work cannot become eligible again."
  (let* ((object (json-parse-string
                  (mevedel-session-control-fs-read-file
                   path 'utf-8-unix (1+ mevedel-journal-store--entry-max-bytes))))
         (id (and (hash-table-p object) (gethash "capture-id" object)))
         (turns (and (hash-table-p object) (gethash "turn-ids" object))))
    (unless (and (hash-table-p object) (= 2 (hash-table-count object))
                 (stringp id)
                 (mevedel-journal-store-id-p id)
                 (equal (concat id ".json") (file-name-nondirectory path))
                 (vectorp turns) (> (length turns) 0)
                 (= (length turns) (length (delete-dups (append turns nil))))
                 (cl-every (lambda (turn)
                             (and (stringp turn)
                                  (mevedel-journal-store-id-p turn)))
                           turns))
      (signal 'mevedel-journal-store-invalid '("Invalid capture coverage")))
    (append turns nil)))

(defun mevedel-journal-store--record-coverage (root entry)
  "Retain ENTRY's published turn IDs in ROOT before its capture can retire."
  (let* ((id (plist-get entry :capture-id))
         (directory (file-name-concat (mevedel-journal-store-directory root) "state" "coverage"))
         (path (file-name-concat directory (concat id ".json")))
         (turns (plist-get entry :turn-ids)))
    (mevedel-session-control-fs-make-directory directory t)
    (unless (mevedel-session-control-fs-create-or-verify
             path (json-serialize (list :capture-id id :turn-ids (vconcat turns))))
      (signal 'mevedel-journal-store-invalid '("Conflicting capture coverage")))))

(defun mevedel-journal-store-covered-turns (root)
  "Return IDs of all turns already published in workspace ROOT.
These private identity records outlive public entry expiry.  They contain no
evidence text and are independent of consolidation review coverage."
  (delete-dups
   (mapcan #'mevedel-journal-store--read-coverage
           (mevedel-session-control-fs-list-directory
            (file-name-concat (mevedel-journal-store-directory root) "state" "coverage")
            (concat "\\`" mevedel-journal-store-hash-regexp "\\.json\\'")))))

(defun mevedel-journal-store-publish-digest (root metadata body)
  "Publish a digest from frozen capture METADATA and BODY in workspace ROOT.
Return the published entry.  METADATA's capture identity and UTC creation
time must come from the durable capture descriptor and stay fixed on retry.
An existing result wins over a later response for that same capture.  This
does not acquire capture ownership; the caller must hold its claim."
  (let ((entry (mevedel-journal-store--publish-record
                root metadata 'digest body
                (lambda (existing)
                  (cl-every (lambda (field)
                              (equal (plist-get existing (cdr field))
                                     (plist-get metadata (cdr field))))
                            mevedel-journal-store--fields)))))
    (mevedel-journal-store--record-coverage root entry)
    entry))

(defun mevedel-journal-store-publish-review (root metadata)
  "Publish immutable completed-review METADATA in workspace ROOT.
The caller must already have accepted ownership settlement and durably stored
the proposal batch. METADATA carries that pass's frozen identity and time.
Publication is the review-coverage commit point; this function neither claims
ownership nor invents coverage from a scheduling timestamp."
  (mevedel-journal-store--publish-record root metadata 'consolidation))

(defun mevedel-journal-store-publish-decision (root metadata)
  "Publish immutable proposal decision METADATA in workspace ROOT.
The caller must have durably accepted this exact decision and retained any
private before/after evidence referenced by its state hash."
  (mevedel-journal-store--publish-record root metadata 'decision))

(defun mevedel-journal-store--publish-record (root metadata kind &optional body same-p)
  "Publish exactly one immutable record of KIND from METADATA and BODY in ROOT.
Return the entry.  When the file already exists, accept it only if SAME-P
returns non-nil for the existing entry; without SAME-P the texts must match."
  (let* ((text (mevedel-journal-store--encode metadata body kind))
         (directory (mevedel-journal-store-directory root))
         (file (mevedel-journal-store--filename metadata kind)))
    (mevedel-journal-store--assert-current root file)
    (mevedel-session-control-fs-make-directory directory t)
    (if (mevedel-session-control-fs-create-file (file-name-concat directory file) text)
        (mevedel-journal-store--decode text file)
      (let ((existing (mevedel-journal-store-read root file)))
        (unless (if same-p (funcall same-p existing) (equal text (plist-get existing :text)))
          (signal 'mevedel-journal-store-invalid '("Conflicting journal publication")))
        existing))))

(provide 'mevedel-journal-store)
;;; mevedel-journal-store.el ends here
