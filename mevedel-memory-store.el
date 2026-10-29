;;; mevedel-memory-store.el -- Durable consolidation evidence -*- lexical-binding: t -*-

;;; Commentary:

;; Private immutable pass records retain the original scope and whole evidence.
;; Journal mutation ownership serializes evidence pinning with accepted expiry.
;; Preparing a pass does not publish review coverage or grant mutation authority
;; over its memory roots.

;;; Code:

(require 'cl-lib)
(require 'mevedel-journal-cleanup)
(require 'mevedel-memory-scope)

(defvar read-circle)
(defvar read-eval)

;; `mevedel-memory-notes'
(declare-function mevedel-memory-notes-housekeeping "mevedel-memory-notes" (scope))
(declare-function mevedel-memory-notes-index "mevedel-memory-notes" (scope))
(declare-function mevedel-memory-notes-root "mevedel-memory-notes" (scope))
(autoload 'mevedel-memory-notes-housekeeping "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-index "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-root "mevedel-memory-notes")

(defconst mevedel-memory-store--max-bytes (* 16 1024 1024)
  "Maximum private immutable pass or write record bytes.
A preserved 512 KiB note can appear as two before-state and three after-state
byte strings, with up to fourfold escaping, plus decoded content and metadata.")

(defun mevedel-memory-store--directory (workspace id)
  "Return WORKSPACE's private pass directory for ID."
  (unless (mevedel-journal-store-id-p id) (error "Invalid memory pass identity"))
  (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "passes" id))

(defun mevedel-memory-store--claim-directory (workspace)
  "Return WORKSPACE's consolidation ownership directory."
  (mevedel-journal-store-claim-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))
                                         'consolidation))

(defun mevedel-memory-store--assert-owned (claim)
  "Reject CLAIM unless it is the current unexpired, unsettled owner."
  (unless (mevedel-journal-claim-owned-p claim)
    (error "Memory pass ownership is unavailable")))

(defun mevedel-memory-store--mutation (workspace function)
  "Call FUNCTION with WORKSPACE's journal mutation claim.
FUNCTION rechecks the claim before each mutation; accepted immutable outcomes
remain recoverable if an operation fails after committing its publication."
  (let ((claim (mevedel-journal-claim-acquire
                (mevedel-journal-store-claim-directory
                 (mevedel-journal-store-directory (mevedel-workspace-root workspace)) 'mutation)
                120)))
    (unless claim (error "Journal mutation is busy"))
    (unwind-protect
        (funcall function claim)
      (mevedel-journal-claim-settle claim 'completed ""))))

(defun mevedel-memory-store--read-lisp (path &optional observation)
  "Read the bounded private Lisp record at PATH as (DATA . TEXT).
DATA is nil when TEXT is oversized, unreadable, or followed by anything but
whitespace; callers validate DATA's shape and signal their own errors.
OBSERVATION may supply PATH's pinned read from this operation's bounded batch."
  (when (and observation (not (equal path (plist-get observation :path))))
    (error "Memory record observation addresses another path"))
  (let* ((text (if observation (mevedel-session-control-fs-program-value observation)
                 (mevedel-session-control-fs-read-file path 'utf-8-unix (1+ mevedel-memory-store--max-bytes))))
         (read-eval nil) (read-circle nil)
         (parsed (and (<= (string-bytes text) mevedel-memory-store--max-bytes)
                      (ignore-errors (read-from-string text)))))
    (cons (and parsed (string-blank-p (substring text (cdr parsed))) (car parsed)) text)))

(defun mevedel-memory-store--print (value)
  "Encode VALUE as bounded plain durable Lisp, preserving literal byte strings."
  (let* ((print-length nil) (print-level nil) (print-circle nil)
         (print-escape-nonascii t) (print-escape-multibyte t)
         (text (prin1-to-string value)))
    (when (> (string-bytes text) mevedel-memory-store--max-bytes)
      (error "Memory pass record exceeds its byte limit"))
    text))

(defun mevedel-memory-store--entry (entry)
  "Freeze the complete body and source fingerprint of validated journal ENTRY."
  (append (cl-loop for key in '(:id :file :session :session-name :created :body)
                   append (list key (plist-get entry key)))
          (list :sha256 (secure-hash 'sha256 (plist-get entry :text)))))

(defun mevedel-memory-store--pin (workspace id digest)
  "Return WORKSPACE's pin path for pass ID and DIGEST identity."
  (unless (and (mevedel-journal-store-id-p id) (mevedel-journal-store-id-p digest))
    (error "Invalid memory evidence identity"))
  (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "evidence-pins" digest (concat id ".pin")))

(defun mevedel-memory-store-read (workspace id)
  "Read WORKSPACE's immutable prepared pass ID, including its private scope.
This internal storage interface grants no right to display original memory
bodies; inspection must use the original authority checks on the returned scope."
  (when (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) id)
    (error "Memory pass has expired"))
  (let* ((record (mevedel-memory-store--read-lisp
                  (file-name-concat (mevedel-memory-store--directory workspace id) "prepared.el")))
         (data (car record)) (text (cdr record))
         (claim (and (proper-list-p data) (plist-get data :claim)))
         (entries (plist-get data :entries)))
    (unless (and (proper-list-p data) (= (length data) 12)
                 (cl-every (lambda (key) (plist-member data key)) '(:id :claim :scope :entries :focus :created))
                 (equal id (plist-get data :id))
                 (equal id (plist-get claim :owner))
                 (equal (plist-get (plist-get data :scope) :workspace)
                        (mevedel-workspace-identity-read (mevedel-workspace-root workspace)))
                 (stringp (plist-get data :focus)) (<= (string-bytes (plist-get data :focus)) 4096)
                 (proper-list-p entries) (<= (length entries) 20)
                 (cl-every (lambda (entry)
                             (and (proper-list-p entry) (= (length entry) 14)
                                  (cl-every (lambda (key) (plist-member entry key))
                                            '(:id :file :session :session-name :created :body :sha256))
                                  (mevedel-journal-store-id-p (plist-get entry :id))
                                  (mevedel-journal-store-id-p (plist-get entry :sha256))
                                  (stringp (plist-get entry :file))
                                  (mevedel-journal-store-file-name-p (plist-get entry :file))
                                  (stringp (plist-get entry :body))
                                  (<= (string-bytes (plist-get entry :body)) mevedel-context-summary--digest-max-bytes))) entries))
      (error "Invalid prepared memory pass"))
    (let ((token (append (list :directory (mevedel-memory-store--claim-directory workspace)) claim)))
      (unless (equal claim (mevedel-journal-claim--read (mevedel-journal-claim--path token nil) nil))
        (error "Prepared pass does not match its ownership record"))
      (plist-put data :claim token))
    (append data (list :hash (secure-hash 'sha256 text)))))

(defun mevedel-memory-store-prepare (workspace claim scope entries focus)
  "Persist SCOPE and complete ENTRIES for WORKSPACE's current pass CLAIM.
FOCUS must fit the public review record before inference starts. Recover
accepted expiry under journal mutation ownership, verify current evidence,
then retain its pins. Return the restart-readable prepared record.
No coverage is consumed."
  (unless (and (equal (plist-get claim :directory) (mevedel-memory-store--claim-directory workspace))
               (stringp focus) (<= (string-bytes focus) 4096)
               (proper-list-p entries) (<= (length entries) 20)
               (mevedel-journal-store-id-p (plist-get scope :workspace))
               (equal (plist-get scope :workspace) (mevedel-workspace-identity-read (mevedel-workspace-root workspace))))
    (error "Invalid memory pass preparation"))
  (mevedel-memory-store--assert-owned claim)
  (mevedel-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((root (mevedel-workspace-root workspace))
            (id (plist-get claim :owner))
            (directory (mevedel-memory-store--directory workspace id)))
       (mevedel-journal-cleanup-recover (mevedel-journal-store-directory root))
       (dolist (entry entries)
         (unless (and (eq (plist-get entry :kind) 'digest)
                      (equal entry (mevedel-journal-store-read root (plist-get entry :file))))
           (error "Selected journal evidence changed")))
       (let* ((data (list :id id :claim (mevedel-journal-claim--record claim) :scope scope
                          :entries (mapcar #'mevedel-memory-store--entry entries) :focus focus
                          :created (mevedel-journal-store-timestamp (- (plist-get claim :expires-at) 180))))
              (text (mevedel-memory-store--print data))
              (hash (secure-hash 'sha256 text))
              (path (file-name-concat directory "prepared.el")))
         (mevedel-memory-store--assert-owned claim)
         (mevedel-memory-store--assert-owned mutation)
         (mevedel-session-control-fs-make-directory directory t)
         (unless (mevedel-session-control-fs-create-or-verify path text 'utf-8-unix)
           (error "Conflicting prepared memory pass"))
         (dolist (entry entries)
           (mevedel-memory-store--assert-owned claim)
           (mevedel-memory-store--assert-owned mutation)
           (let ((pin (mevedel-memory-store--pin workspace id (plist-get entry :id))))
             (mevedel-session-control-fs-make-directory (file-name-directory pin) t)
             (unless (mevedel-session-control-fs-create-or-verify pin hash 'utf-8-unix)
               (error "Conflicting memory evidence pin"))))
         (mevedel-memory-store-read workspace id))))))

(defun mevedel-memory-store-release (workspace id)
  "Release only WORKSPACE pass ID's pins after a fenced unsuccessful outcome.
Repeated release is harmless. Live and accepted passes retain their evidence.
Return the number of pins released."
  (mevedel-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((prepared (mevedel-memory-store-read workspace id))
            (outcome (mevedel-journal-claim-outcome (plist-get prepared :claim))))
       (unless (memq (plist-get outcome :status) '(failed cancelled expired))
         (error "Memory pass still needs its evidence"))
       (mevedel-memory-store--release-pins workspace prepared nil mutation)))))

(defun mevedel-memory-store--release-pins (workspace prepared keep mutation)
  "Release PREPARED's pins in WORKSPACE except digest IDs in KEEP.
MUTATION holds journal ownership; the caller has proved release eligibility.
Return the number of pins released; already absent pins are not counted."
  (let ((released 0))
    (dolist (entry (plist-get prepared :entries) released)
      (unless (member (plist-get entry :id) keep)
        (let ((pin (mevedel-memory-store--pin workspace (plist-get prepared :id) (plist-get entry :id))))
          (condition-case nil
              (progn
                (unless (equal (plist-get prepared :hash) (mevedel-session-control-fs-read-file pin 'utf-8-unix 65))
                  (error "Memory evidence pin changed"))
                (mevedel-memory-store--assert-owned mutation)
                (mevedel-session-control-fs-delete-file pin)
                (cl-incf released))
            (mevedel-session-control-fs-absent nil)))))))

(defun mevedel-memory-store--proposal (scope id proposal)
  "Bind PROPOSAL to pass ID, original SCOPE targets, and complete before-state.
A condensed note also records the hash of its resulting version."
  (let* ((root (cdr (assoc (plist-get proposal :root) (plist-get scope :roots))))
         (action (plist-get proposal :action))
         (files (delete-dups
                 (append (list (plist-get proposal :file)) (plist-get proposal :merged-files)
                         (unless (or (eq action 'instructions) (mevedel-memory-proposal-note-action-p action))
                           (list "MEMORY.md")))))
         (before (mapcar (lambda (file) (cons file (mevedel-memory-scope-before scope (plist-get proposal :root) file)))
                         (sort files #'string<)))
         (proposal (if (eq action 'note-condense)
                       (append proposal
                               (list :result (secure-hash
                                              'sha256 (mevedel-memory-proposal-condensed-bytes
                                                       (plist-get proposal :body)))))
                     proposal))
         (identity (list id (plist-get root :dir) (plist-get root :client) (plist-get root :target)
                         proposal before)))
    (append proposal (list :id (secure-hash 'sha256 (mevedel-memory-store--print identity)) :before before))))

(defun mevedel-memory-store-accepted (workspace id)
  "Return the fenced accepted bundle for WORKSPACE pass ID, or nil.
Verify its exact bytes against the claim outcome before reading its private
proposal data. Return the immutable prepared record under :prepared and the
authenticated accepted bytes' hash under :hash."
  (let* ((prepared (mevedel-memory-store-read workspace id))
         (outcome (mevedel-journal-claim-outcome (plist-get prepared :claim))))
    (when (eq (plist-get outcome :status) 'completed)
      (let* ((record (mevedel-memory-store--read-lisp
                      (file-name-concat (mevedel-memory-store--directory workspace id) "accepted.el")))
             (text (cdr record)))
        (unless (equal (secure-hash 'sha256 text) (plist-get outcome :payload))
          (error "Accepted memory bundle changed"))
        (let* ((data (car record))
               (review (plist-get data :review)))
          (unless (and (proper-list-p data) (= (length data) 8)
                       (cl-every (lambda (key) (plist-member data key)) '(:prepared-hash :review :proposals :no-action))
                       (equal (plist-get prepared :hash) (plist-get data :prepared-hash))
                       (equal id (plist-get review :pass-id))
                       (equal (plist-get review :workspace) (plist-get (plist-get prepared :scope) :workspace))
                       (equal (plist-get review :proposals)
                              (mapcar (lambda (proposal) (plist-get proposal :id)) (plist-get data :proposals))))
            (error "Invalid accepted memory bundle"))
          (mevedel-journal-store--validate-metadata review 'consolidation)
          (append data (list :prepared prepared :hash (plist-get outcome :payload))))))))

(defun mevedel-memory-store-accept (workspace prepared reply entries model references
                                              &optional notes inspected)
  "Accept validated REPLY for WORKSPACE's PREPARED pass before public coverage.
ENTRIES are the complete admitted subset; MODEL is its exact provider label and
REFERENCES are bounded dated observations. Retain original target before-state.
NOTES lists the admitted shared working note IDs.  A nil REPLY accepts a
housekeeping-only pass, which admitted no digests or notes for inference.
INSPECTED lists the normalized workspace paths the review actually read.
The immutable claim outcome accepts the bundle hash, allowing later recovery."
  (let ((scope (plist-get prepared :scope)))
    (when (and (null reply) (or entries notes))
      (error "Admitted evidence requires a validated review reply"))
    (mevedel-memory-store-accept-proposals
     workspace prepared
     (if reply
         (append
          (mevedel-memory-proposal-parse
           reply (plist-get scope :roots) (mapcar (lambda (entry) (plist-get entry :id)) entries)
           (seq-filter (lambda (row) (member (car row) notes)) (mevedel-memory-notes-index scope)))
          (list :inspected inspected))
       (list :proposals nil :no-action "- Shared-folder housekeeping only."))
     entries model references)))

(defun mevedel-memory-store--note-p (proposal)
  "Return non-nil when PROPOSAL disposes of a shared working file."
  (mevedel-memory-proposal-note-action-p (plist-get proposal :action)))

(defun mevedel-memory-store--kept (proposal why)
  "Return note PROPOSAL accepted as keep, explaining WHY before its reason."
  (plist-put
   (plist-put (plist-put (copy-sequence proposal) :action 'note-keep) :guard-converted t)
   :reason (concat "Kept: " why " Reviewer: " (plist-get proposal :reason))))

(defun mevedel-memory-store--preserved (proposal paragraphs)
  "Return note PROPOSAL condensed with lost measurement PARAGRAPHS verbatim.
PARAGRAPHS retain their original order and conditions from the captured note."
  (let ((copy (copy-sequence proposal)))
    (plist-put copy :body
               (concat (if (eq (plist-get proposal :action) 'note-delete)
                           (format "Measurements retained from %s.\n\n" (plist-get proposal :note))
                         (concat (string-trim-right (plist-get proposal :body) "\n+")
                                 "\n\n## Preserved measurements\n\n"))
                       (string-join paragraphs "\n\n")))
    (plist-put copy :action 'note-condense)
    (setq copy (plist-put copy :guard-preserved t))
    (plist-put copy :reason
               (concat "Preserved: measurement paragraphs retained verbatim. Reviewer: "
                       (plist-get proposal :reason)))))

(defun mevedel-memory-store--unsupported-delete-p (proposal inspected)
  "Return non-nil when delete PROPOSAL leaves an open item unsupported.
A delete that lists open items must quote, at least once per item, workspace
text establishing their resolution or durable record, and each quote must
occur in what the review read from that file.  INSPECTED maps paths to that
delivered text.  Each item's positional resolution anchor must occur in both
the item and its authenticated quote.  Explicit unresolved items prevent delete.
Absence of a visible record is not resolution."
  (and (eq (plist-get proposal :action) 'note-delete)
       (or (let ((unresolved (plist-get proposal :unresolved-items)))
             (and unresolved (not (equal unresolved []))))
           (and (plist-get proposal :open-items)
                (let ((resolved (plist-get proposal :resolved-by))
                      (anchors (plist-get proposal :resolution-anchors))
                      (items (plist-get proposal :open-items)))
                  (or (/= (length anchors) (length items))
                      (< (length resolved) (length items))
                      (cl-some (lambda (entry)
                                 (not (mevedel-memory-proposal-quoted-p
                                       (cdr entry) (or (cdr (assoc (car entry) inspected)) ""))))
                               resolved)
                      (cl-loop for item in items for entry in resolved for anchor in anchors
                               thereis (not (and (mevedel-memory-store--anchor-p anchor item)
                                                 (mevedel-memory-store--resolution-anchor-p anchor (cdr entry))
                                                 (mevedel-memory-store--anchor-p
                                                  anchor (or (cdr (assoc (car entry) inspected)) "") t))))))))))

(defun mevedel-memory-store--plain (text &optional delivered)
  "Normalize TEXT without erasing raw tab-separated measurement counts.
Only DELIVERED Read text may contain numeric line-number prefixes to strip."
  (mevedel-memory-proposal--plain
   (if delivered text (replace-regexp-in-string "\t" " " text))))

(defun mevedel-memory-store--anchor-p (anchor text &optional delivered)
  "Return non-nil when nonempty ANCHOR occurs as whole words in TEXT.
Case and whitespace are normalized; numeric and word substrings do not match.
DELIVERED marks Read text whose numeric line prefixes are presentation only."
  (and (stringp anchor) (not (string-blank-p anchor))
       (let ((case-fold-search t)
             (plain (mevedel-memory-store--plain anchor)))
         (and (string-match-p "[[:alnum:]]" plain)
              (string-match-p
               (concat "\\(?:\\`\\|[^[:alnum:]_]\\)"
                       (regexp-quote plain)
                       "\\(?:\\'\\|[^[:alnum:]_]\\)")
               (mevedel-memory-store--plain text delivered))))))

(defun mevedel-memory-store--resolution-anchor-p (anchor quote)
  "Return non-nil when ANCHOR occurs in QUOTE's authenticated segments.
When quotation marks delimit segments, surrounding citation text is not proof."
  (let ((start 0) segments)
    (while (string-match "[\"\u201c]\\([^\"\u201d]+\\)[\"\u201d]" quote start)
      (push (match-string 1 quote) segments)
      (setq start (match-end 0)))
    (cl-some (lambda (segment) (mevedel-memory-store--anchor-p anchor segment))
             (or segments (list quote)))))

(defun mevedel-memory-store--unaccounted-handoff-p (proposal scope)
  "Return non-nil if delete PROPOSAL omits SCOPE's explicit handoff markers.
An empty open-item inventory cannot account for a source's labelled open work.
This conservative lexical guard may retain already-completed handoff history."
  (when (and (plist-get proposal :note)
             (eq (plist-get proposal :action) 'note-delete)
             (null (plist-get proposal :open-items)))
    (let* ((before (mevedel-memory-scope-before scope (plist-get proposal :root) (plist-get proposal :file)))
           (text (mevedel-memory-store--plain (decode-coding-string (plist-get before :bytes) 'utf-8-unix)))
           (case-fold-search t))
      (string-match-p
       (concat "\\_<\\(?:next steps?\\|remaining\\|open\\|handoff\\|pending\\|unverified\\|"
               "unresolved\\|TODO\\|\\(?:owner\\|root\\) \\(?:to\\|owns\\)\\)\\_>\\|\\[ \\]")
       text))))

(defun mevedel-memory-store--measurements (text)
  "Return conservative measurement paragraphs from TEXT, with their conditions.
Protect whole paragraphs, not bare numbers: a value without its operation,
baseline or sample conditions is not the same evidence.  Integers of at least
two digits after leading zeros, decimals, percentages and durations qualify.
Mask explicit dates, clock times, hashes, paths, line references, versions and
list ordinals first.  Ambiguous bare decimals remain protected, not erased."
  (with-syntax-table (standard-syntax-table)
    (let ((case-fold-search t))
      (seq-filter
       (lambda (paragraph)
	 (let ((data paragraph) (start 0) found
               (duration "\\(?:ns\\|us\\|ms\\|s\\|seconds?\\|minutes?\\|hours?\\)"))
           ;; Mask spans, not whole paragraphs: metadata beside a measurement
           ;; must not conceal it.  Ordinals need the original line boundaries.
           (dolist (pattern
                    '("^[ \t]*[0-9]+[.)][ \t]+"
                      "[0-9]\\{4\\}[-/][0-9]\\{2\\}[-/][0-9]\\{2\\}"
                      "[0-9]\\{2\\}[./-][0-9]\\{2\\}[./-][0-9]\\{4\\}"
                      "\\_<T?[0-2]?[0-9]:[0-5][0-9]\\(?::[0-5][0-9]\\(?:\\.[0-9]+\\)?\\)?\\(?:Z\\|[+-][0-9][0-9]:?[0-9][0-9]\\|[ \t]*[ap]m\\)?"
                      "\\_<\\(?:commit\\|hash\\|sha1\\|sha256\\)[: \t]+[[:xdigit:]]\\{7,64\\}\\_>"
                      "\\_<\\(?:version\\|release\\|ver\\)[: \t]+v?[0-9]+\\(?:\\.[0-9]+\\)*\\(?:[-+][[:alnum:].-]+\\)?"
                      "\\_<v[0-9]+\\(?:\\.[0-9]+\\)*\\(?:[-+][[:alnum:].-]+\\)?"
                      "\\(?:\\`\\|[^[:alnum:]_.]\\)[0-9]+\\(?:\\.[0-9]+\\)\\{2,\\}\\(?:[-+][[:alnum:].-]+\\)?"
                      "\\_<[[:alpha:]_][[:alnum:]_.-]*:[0-9]+\\(?:[-:][0-9]+\\)?"
                      "\\_<\\(?:lines? +\\|L\\)[0-9]+\\(?:[-:][0-9]+\\)?\\_>"))
             (setq data (replace-regexp-in-string pattern " " data t t)))
           (cl-labels ((mask-path (token)
                         (save-match-data
			   (if (or (and (not (string-match-p
                                              (concat "\\`[+-]?\\(?:[0-9][0-9,.]*\\|\\.[0-9]+\\)" duration "/") token))
                                        (string-match-p "[/\\\\]" token)
                                        (or (string-match-p "[[:alpha:]~.]" token)
                                            (string-match-p "\\`[\"'(]*[/~]" token)))
				   (and (string-search "." token)
                                        (string-match-p
                                         "\\`[[:alnum:]_-]+\\.[[:alpha:]][[:alnum:]]*\\_>"
                                         (string-trim-left token "[^[:alnum:]_-]+"))))
                               " " token))))
             ;; Quoted paths can contain spaces; mask them before tokenizing.
             (dolist (pattern '("\"[^\"\n]+\"" "`[^`\n]+`" "'[^'\n]+'"))
               (setq data
                     (replace-regexp-in-string
                      pattern (lambda (quoted)
                                (save-match-data
				  (let ((first (car (split-string (substring quoted 1 -1) "[ \t]+" t))))
                                    (if (and first (equal " " (mask-path first))) " " quoted))))
                      data t t)))
             (setq data (replace-regexp-in-string "[^ \t\n]+" #'mask-path data t t)))
           (setq data (mevedel-memory-store--plain data))
           (while (and (not found)
                       (string-match "\\(?:[0-9][0-9,]*\\(?:\\.[0-9]+\\)?\\|\\.[0-9]+\\)" data start))
             (let* ((begin (match-beginning 0)) (end (match-end 0))
                    (number (match-string 0 data))
                    (left (and (> begin 0) (substring data (1- begin) begin)))
                    (right (and (< end (length data)) (substring data end (1+ end))))
                    (suffix (string-match-p
                             (concat "\\`[ \t]*\\(?:%\\|" duration "\\(?:\\'\\|[^[:alnum:]_]\\)\\)")
                             (substring data end))))
               (setq start end
                     found (and (not (and left (string-match-p "[[:alnum:]_.]" left)))
				(not (and (member left '("-" "+")) (> begin 1)
                                          (string-match-p "[[:alpha:]_]" (substring data (- begin 2) (1- begin)))))
				(or suffix (not (and right (string-match-p "[[:alnum:]_]" right))))
				(or suffix (string-match-p "\\." number)
                                    (string-match-p
                                     "[1-9][0-9]+"
                                     (replace-regexp-in-string "," "" number)))))))
           found))
       (split-string text "\n[ \t]*\n" t)))))

(defun mevedel-memory-store--contains-paragraph-p (paragraph text &optional delivered)
  "Return non-nil when normalized PARAGRAPH occurs intact within TEXT.
Require whitespace, quote or text boundaries: 512 must not match 1512 reads.
DELIVERED marks Read text whose numeric line prefixes are presentation only."
  (let* ((needle (mevedel-memory-store--plain paragraph))
         (haystack (mevedel-memory-store--plain text delivered))
         (offset 0) found)
    ;; Literal search also supports paragraphs beyond the regexp size limit.
    (when case-fold-search
      (setq needle (downcase needle) haystack (downcase haystack)))
    (unless (string-empty-p needle)
      (while (and (not found) (<= offset (length haystack))
                  (setq offset (string-search needle haystack offset)))
        (let ((end (+ offset (length needle))))
          (setq found (and (or (= offset 0) (memq (aref haystack (1- offset)) '(?\s ?\" #x201c)))
                           (or (= end (length haystack)) (memq (aref haystack end) '(?\s ?\" #x201d))))))
        (setq offset (1+ offset))))
    found))

(defun mevedel-memory-store--lost-measurements (proposal scope extractions inspected)
  "Return measurements a delete or condense PROPOSAL drops from SCOPE's preimage.
An intact paragraph in the replacement or EXTRACTIONS citing this note
preserves it.  A resolution quote may account for it only when the quote
contains that paragraph and occurs in INSPECTED's delivered workspace text.
This is a conservative content guard, not semantic proof of resolution.
Completed measurement paragraphs without such evidence require preservation,
even when a sequential summary omitted them to fit its size limit."
  (when (and (plist-get proposal :note)
             (memq (plist-get proposal :action) '(note-delete note-condense)))
    (let* ((before (mevedel-memory-scope-before scope (plist-get proposal :root) (plist-get proposal :file)))
           (paragraphs (mevedel-memory-store--measurements
                        (decode-coding-string (plist-get before :bytes) 'utf-8-unix)))
           (preserved (cons (plist-get proposal :body)
                            (mapcar (lambda (item) (plist-get item :body))
                                    (seq-filter
                                     (lambda (item)
                                       (member (plist-get proposal :note) (plist-get item :evidence)))
                                     extractions))))
           (quotes (seq-filter
                    (lambda (entry)
                      (mevedel-memory-proposal-quoted-p
                       (cdr entry) (or (cdr (assoc (car entry) inspected)) "")))
                    (plist-get proposal :resolved-by))))
      (seq-remove
       (lambda (paragraph)
         (or (cl-some (lambda (body) (mevedel-memory-store--contains-paragraph-p paragraph body)) preserved)
             (cl-some (lambda (entry)
                        (and (mevedel-memory-store--contains-paragraph-p paragraph (cdr entry))
                             (mevedel-memory-store--contains-paragraph-p
                              paragraph (cdr (assoc (car entry) inspected)) t)))
                      quotes)))
       paragraphs))))

(defun mevedel-memory-store-accept-proposals (workspace prepared parsed entries producer references)
  "Accept trusted PARSED proposals for WORKSPACE's PREPARED pass.
ENTRIES and REFERENCES retain the same evidence contract as model replies.
PRODUCER is the provider label, or `user' for an explicit cockpit action.
Callers validate model text before this boundary; direct user actions construct
structured changes from fresh captured snapshots without synthetic model text.
Housekeeping for the scope's captured attachments and empty folders is added
here, after every extraction.  Each note disposal records the extractions
citing its note as :requires, so it cannot apply before them.  Deleting a
note that memory or instructions cite, without any extraction citing it, is
accepted as keep instead, so a citation never silently breaks.  A delete
listing open items is likewise accepted as keep unless PARSED's :inspected
reads include every file it cites as their resolution or record."
  (let* ((id (plist-get prepared :id))
         (claim (plist-get prepared :claim))
         (scope (plist-get prepared :scope))
         (ids (mapcar (lambda (entry) (plist-get entry :id)) entries))
         (inspected (plist-get parsed :inspected)))
    (mevedel-memory-store--assert-owned claim)
    (unless (equal prepared (mevedel-memory-store-read workspace id))
      (error "Prepared memory evidence changed"))
    (unless (or entries (null (plist-get prepared :entries)))
      (error "A selected digest batch must admit a complete digest"))
    (dolist (entry entries)
      (let ((original (cl-find (plist-get entry :id) (plist-get prepared :entries)
                               :key (lambda (candidate) (plist-get candidate :id)) :test #'equal)))
        (unless (and original
                     (cl-every (lambda (key) (equal (plist-get entry key) (plist-get original key)))
                               '(:id :file :session :session-name :created :body)))
          (error "Accepted digest was not fully admitted"))
        (unless (equal (plist-get prepared :hash)
                       (mevedel-session-control-fs-read-file
                        (mevedel-memory-store--pin workspace id (plist-get entry :id)) 'utf-8-unix 65))
          (error "Accepted digest evidence is not retained"))))
    (let* ((proposals
            (let* ((parsed (append (plist-get parsed :proposals) (mevedel-memory-notes-housekeeping scope)))
                   (extractions (mapcar (lambda (proposal) (mevedel-memory-store--proposal scope id proposal))
                                        (seq-remove #'mevedel-memory-store--note-p parsed))))
              ;; Note disposals depend on every extraction that cites them.
              (append extractions
                      (mapcar (lambda (proposal)
                                (let* ((requires
                                        (when (plist-get proposal :note)
                                          (mapcar (lambda (extraction) (plist-get extraction :id))
                                                  (seq-filter
                                                   (lambda (extraction)
                                                     (member (plist-get proposal :note)
                                                             (plist-get extraction :evidence)))
                                                   extractions))))
                                       (handoff (mevedel-memory-store--unaccounted-handoff-p proposal scope))
                                       (lost (mevedel-memory-store--lost-measurements
                                              proposal scope extractions inspected))
                                       (cited (plist-get (cdr (assoc (plist-get proposal :file)
                                                                     (plist-get (cdr (mevedel-memory-notes-root scope))
                                                                                :notes)))
                                                         :cited)))
                                  ;; The citation is the note's continuing use until the
                                  ;; citing memory changes in the same review.
                                  (cond
                                   (handoff
                                    (setq proposal
                                          (mevedel-memory-store--kept
                                           proposal "the source contains explicit handoff markers but the review lists no open items.")))
                                   ((and cited (null requires) (eq (plist-get proposal :action) 'note-delete))
                                    (setq proposal
                                          (mevedel-memory-store--kept
                                           proposal (concat "a memory topic or instruction file cites "
                                                            "this note, and no update in this review "
                                                            "replaces that citation."))))
                                   ((mevedel-memory-store--unsupported-delete-p
                                     proposal inspected)
                                    (setq proposal
                                          (mevedel-memory-store--kept
                                           proposal (format "the review did not quote read workspace evidence with a matching resolution anchor for each item, or still lists unresolved items: %s."
                                                            (string-join (plist-get proposal :open-items) "; ")))))
                                   (lost
                                    (setq proposal (mevedel-memory-store--preserved proposal lost))))
                                  (mevedel-memory-store--proposal scope id (append proposal (list :requires requires)))))
                              (seq-filter #'mevedel-memory-store--note-p parsed)))))
           (review (list :pass-id id :workspace (plist-get scope :workspace)
                         :created (mevedel-journal-store-timestamp
                                   (mevedel-session-control-fs-target-time (plist-get claim :directory)))
                         :model producer :focus (plist-get prepared :focus) :digests ids
                         :proposals (mapcar (lambda (proposal) (plist-get proposal :id)) proposals)
                         :references references))
           (_ (mevedel-journal-store--validate-metadata review 'consolidation))
           (data (list :prepared-hash (plist-get prepared :hash) :review review
                       :proposals proposals :no-action (plist-get parsed :no-action)))
           (text (mevedel-memory-store--print data))
           (path (file-name-concat (mevedel-memory-store--directory workspace id) "accepted.el")))
      (mevedel-memory-store--assert-owned claim)
      (unless (mevedel-session-control-fs-create-file path text)
        (error "Memory result bundle already exists"))
      (when (mevedel-journal-claim-settle claim 'completed (secure-hash 'sha256 text))
        (mevedel-memory-store-accepted workspace id)))))

(defun mevedel-memory-store-publish (workspace id)
  "Recoverably publish WORKSPACE pass ID's accepted review and release spare pins.
The accepted private proposal bundle must already exist. Pending proposals keep
their admitted evidence; a no-action pass releases it after public publication."
  (mevedel-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((accepted (or (mevedel-memory-store-accepted workspace id)
                          (error "Memory pass has no accepted result")))
            (prepared (plist-get accepted :prepared))
            (review (plist-get accepted :review))
            (root (mevedel-workspace-root workspace))
            (journal (mevedel-journal-store-directory root))
            (path (file-name-concat journal (mevedel-journal-store--filename review 'consolidation))))
       (mevedel-journal-cleanup-recover journal)
       (unless (mevedel-session-control-fs-path-exists-p path)
         (dolist (entry (plist-get prepared :entries))
           (when (member (plist-get entry :id) (plist-get review :digests))
             (unless (and (equal (plist-get prepared :hash)
                                 (mevedel-session-control-fs-read-file
                                  (mevedel-memory-store--pin workspace id (plist-get entry :id)) 'utf-8-unix 65))
                          (equal (plist-get entry :sha256)
                                 (secure-hash 'sha256
                                              (plist-get (mevedel-journal-store-read root (plist-get entry :file)) :text))))
               (error "Accepted review evidence is unavailable")))))
       (mevedel-memory-store--assert-owned mutation)
       (let ((published (mevedel-journal-store-publish-review root review)))
         (setf (mevedel-workspace-journal-observation workspace) nil)
         (mevedel-memory-store--release-pins workspace prepared
                                            (and (plist-get accepted :proposals) (plist-get review :digests)) mutation)
         (mevedel-journal-cleanup-schedule workspace t)
         published)))))

(defun mevedel-memory-store-recover (workspace)
  "Recover settled pass publications and evidence releases in WORKSPACE.
Call after acquiring a successor pass claim, so all older attempts are fenced.
Live preparations are left alone. Unreadable accepted state fails closed rather
than letting a new selection silently ignore its unpublished review coverage.
Authenticate every completed bundle, but avoid a publication transaction when
the public bytes already match and no spare evidence pins remain.
Return non-nil when recovery changed committed journal state: finished an
expiry, published, or released evidence pins."
  (let ((published (make-hash-table :test #'equal)) changed)
    ;; Publication used to do this once per completed pass.  Recover accepted
    ;; expiry before observing retirement or taking the no-op publication path.
    (when (> (mevedel-memory-store--mutation
              workspace
              (lambda (mutation)
                (mevedel-memory-store--assert-owned mutation)
                (mevedel-journal-cleanup-recover
                 (mevedel-journal-store-directory (mevedel-workspace-root workspace)))))
             0)
      (setq changed t))
    (dolist (entry (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
      (puthash (plist-get entry :file) (plist-get entry :text) published))
    (dolist (directory
             (mevedel-session-control-fs-list-directory
              (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "passes") mevedel-journal-store-id-regexp))
      ;; A crash after creating the directory but before preparation retains no
      ;; evidence pins: the immutable prepared file always precedes pin writes.
      (when (and (not (mevedel-journal-cleanup-pass-retired-p
                       (mevedel-workspace-root workspace) (file-name-nondirectory (directory-file-name directory))))
                 (mevedel-session-control-fs-path-exists-p (file-name-concat directory "prepared.el")))
        (let* ((id (file-name-nondirectory (directory-file-name directory)))
               (prepared (mevedel-memory-store-read workspace id))
               (outcome (mevedel-journal-claim-outcome (plist-get prepared :claim))))
          (pcase (plist-get outcome :status)
            ('completed
             (let* ((accepted (mevedel-memory-store-accepted workspace id))
                    (prepared (plist-get accepted :prepared))
                    (review (plist-get accepted :review))
                    (file (mevedel-journal-store--filename review 'consolidation))
                    (text (mevedel-journal-store--encode review nil 'consolidation))
                    (keep (and (plist-get accepted :proposals) (plist-get review :digests))))
               (when (or (not (equal text (gethash file published)))
                         (seq-some (lambda (entry)
                                     (and (not (member (plist-get entry :id) keep))
                                          (mevedel-session-control-fs-path-exists-p
                                           (mevedel-memory-store--pin workspace id (plist-get entry :id)))))
                                   (plist-get prepared :entries)))
                 (mevedel-memory-store-publish workspace id)
                 (setq changed t))))
            ((or 'failed 'cancelled 'expired)
             (when (> (mevedel-memory-store-release workspace id) 0) (setq changed t)))))))
    changed))

(provide 'mevedel-memory-store)
;;; mevedel-memory-store.el ends here
