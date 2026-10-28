;;; mevedel-memory-proposal.el -- Validated consolidation proposals -*- lexical-binding: t -*-

;;; Commentary:

;; Decodes one bounded consolidation reply against its captured file and
;; evidence scope. No proposal is returned unless the entire reply is valid.
;; This module performs no filesystem access or writes. Scope capture owns
;; canonical roots and before-state; application must recheck their authority
;; and freshness before acting on these untrusted suggestions.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst mevedel-memory-proposal--max-bytes (* 32 1024)
  "Maximum encoded consolidation reply size.")

(defconst mevedel-memory-proposal--sections
  '(("Promote" . promote) ("Update" . update) ("Merge" . merge)
    ("Remove" . remove) ("Instructions" . instructions) ("Notes" . notes)
    ("No action" . nil))
  "Closed ordered consolidation sections and their actions.
Notes is required exactly when shared working notes were admitted.")

(defconst mevedel-memory-proposal--fields
  '(("root" . :root) ("file" . :file) ("type" . :type)
    ("title" . :title) ("hook" . :hook) ("reason" . :reason)
    ("evidence" . :evidence) ("merged-files" . :merged-files))
  "Closed proposal header fields; values use JSON syntax.")

(defconst mevedel-memory-proposal--note-fields
  '(("note" . :note) ("disposition" . :disposition) ("reason" . :reason)
    ("open-items" . :open-items) ("resolved-by" . :resolved-by))
  "Closed shared-note disposition header fields; values use JSON syntax.
Only `delete' carries `open-items' and `resolved-by'.")

(defconst mevedel-memory-proposal--dispositions
  '(("delete" . note-delete) ("keep" . note-keep) ("condense" . note-condense))
  "Model-selectable shared-note dispositions and their actions.")

(defconst mevedel-memory-proposal-note-actions '(note-delete note-keep note-condense note-prune)
  "Proposal actions disposing of shared working files, not memory topics.")

(defun mevedel-memory-proposal-note-action-p (action)
  "Return non-nil when ACTION disposes of a shared working file."
  (memq action mevedel-memory-proposal-note-actions))

(defun mevedel-memory-proposal-condensed-bytes (body)
  "Return the exact UTF-8 bytes a condense disposition writes for BODY.
Acceptance hashes and application writes these same bytes."
  (encode-coding-string (concat (string-trim-right body "\n+") "\n") 'utf-8-unix))

(defun mevedel-memory-proposal-relative-path-p (path)
  "Return non-nil when PATH is a plain relative path without traversal.
Absolute, home-relative, control-character, colon, and `..' spellings fail."
  (and (stringp path) (not (string-empty-p path))
       (not (file-name-absolute-p path))
       (not (string-prefix-p "~" path))
       (not (string-match-p "[\\\\[:cntrl:]:]" path))
       (not (member ".." (split-string path "/")))))

(defun mevedel-memory-proposal-normalize-path (path)
  "Return relative PATH in canonical spelling, without filesystem access.
PATH must satisfy `mevedel-memory-proposal-relative-path-p'."
  (let ((root "/mevedel-relative-root/"))
    (directory-file-name (file-relative-name (expand-file-name path root) root))))

(defconst mevedel-memory-proposal--min-quote 12
  "Minimum characters of a delete's resolution quote, after normalization.")

(defun mevedel-memory-proposal--plain (text)
  "Return TEXT without Read line numbers or Markdown marks, spaces collapsed.
Quotes of Markdown prose commonly omit `*' and backquote formatting."
  (string-trim (replace-regexp-in-string
                "[ \t\n]+" " "
                (replace-regexp-in-string
                 "[*`]" "" (replace-regexp-in-string "^ *[0-9]+\t" "" text)))))

(defun mevedel-memory-proposal-quoted-p (quote text)
  "Return non-nil when QUOTE occurs in delivered Read TEXT.
When QUOTE wraps words in double quotation marks, as in
\"\\\"exact words\\\" (lines 7-11)\", every quoted segment must occur;
otherwise the whole QUOTE must.  Line-number prefixes, emphasis and code
marks, and whitespace differences are ignored."
  (let ((plain (mevedel-memory-proposal--plain text))
        (start 0) segments)
    (while (string-match "[\"“]\\([^\"”]+\\)[\"”]" quote start)
      (push (match-string 1 quote) segments)
      (setq start (match-end 0)))
    (and (>= (length (mevedel-memory-proposal--plain (apply #'concat (or segments (list quote)))))
             mevedel-memory-proposal--min-quote)
         (cl-every (lambda (segment)
                     (string-search (mevedel-memory-proposal--plain segment) plain))
                   (or segments (list quote))))))

(defun mevedel-memory-proposal--resolution (entry)
  "Return resolution ENTRY \"PATH: QUOTE\" as (NORMALIZED-PATH . QUOTE), or nil.
Plain relative paths contain no colon, so the first one separates them."
  (when (string-match "\\`\\([^:]+\\): \\(.+\\)\\'" entry)
    (let ((path (match-string 1 entry)) (quote (match-string 2 entry)))
      (when (and (mevedel-memory-proposal-relative-path-p path)
                 (>= (length (mevedel-memory-proposal--plain quote)) mevedel-memory-proposal--min-quote))
        (cons (mevedel-memory-proposal-normalize-path path) quote)))))

(defun mevedel-memory-proposal--strings (value predicate)
  "Return VALUE as a list when it is a duplicate-free array of PREDICATE strings."
  (unless (and (vectorp value)
               (cl-every (lambda (item)
                           (and (stringp item) (not (string-blank-p item))
                                (not (string-match-p "[[:cntrl:]]" item))
                                (funcall predicate item)))
                         value)
               (= (length value) (length (delete-dups (append value nil)))))
    (error "Note disposition open-items and resolved-by must be arrays of distinct strings"))
  (append value nil))

(defun mevedel-memory-proposal--file-p (file)
  "Return non-nil for an ordinary relative Markdown FILE below a root."
  (and (mevedel-memory-proposal-relative-path-p file)
       (string-suffix-p ".md" file)
       (cl-every (lambda (part) (not (member part '("" "." ".mevedel"))))
                 (split-string file "/"))))

(defun mevedel-memory-proposal--fenced (text names &optional repeats)
  "Split fenced proposal TEXT into its decoded header plist and body.
NAMES maps the closed header field names to keywords.  Without a `---'
separator the body is empty and every line must be a header line; the
action's own body rule then decides whether an empty body is valid.
With REPEATS, an exactly repeated header line is accepted."
  (let* ((separated (string-match "^-\\{3,\\}\\(?:\n\\|\\'\\)" text))
         (header (if separated (substring text 0 (match-beginning 0)) text))
         (body (if separated (string-remove-suffix "\n" (substring text (match-end 0))) ""))
        fields)
    (when (string-blank-p header)
      (error "Proposal lacks its header/body separator"))
    (dolist (line (split-string (string-remove-suffix "\n" header) "\n"))
      (unless (string-match "\\`\\([a-z-]+\\): \\(.+\\)\\'" line)
        (error "Invalid proposal header line"))
      (let ((key (cdr (assoc (match-string 1 line) names)))
            (encoded (match-string 2 line)))
        (let ((value (and key (json-parse-string encoded :null-object :null
                                                 :false-object :false))))
          ;; An exactly repeated line is unambiguous; a differing repeat is not.
          (unless (and key (or (not (plist-member fields key))
                               (and repeats (equal value (plist-get fields key)))))
            (error "Unknown or repeated proposal field"))
          (setq fields (plist-put fields key value)))))
    (cons fields body)))

(defun mevedel-memory-proposal--disposition (text notes)
  "Decode one shared-note disposition TEXT for admitted NOTES.
NOTES maps each admitted note ID to (ROOT-ID . RELATIVE-PATH).
A delete also lists the note's OPEN-ITEMS (unresolved findings,
hypotheses, limits or next steps) and RESOLVED-BY entries \"PATH: QUOTE\",
exact words from a workspace file establishing their resolution or durable
record, returned as (PATH . QUOTE).  Acceptance checks the quotes against the
review's reads; this parser only validates shape.  Keep and condense may
carry the same fields; they are validated and recorded but never required."
  (let* ((decoded (mevedel-memory-proposal--fenced text mevedel-memory-proposal--note-fields t))
         (fields (car decoded)) (body (cdr decoded))
         (note (assoc (plist-get fields :note) notes))
         (action (cdr (assoc (plist-get fields :disposition) mevedel-memory-proposal--dispositions)))
         (reason (plist-get fields :reason)))
    (unless (and (cl-every (lambda (key) (plist-member fields key)) '(:note :disposition :reason))
                 ;; A delete must account for its open items.  Missing
                 ;; resolution evidence is simply none: acceptance then keeps
                 ;; any delete that lists an open item.
                 (or (not (eq action 'note-delete)) (plist-member fields :open-items)))
      (error "Note disposition fields are incomplete"))
    (unless (and note action (stringp reason) (not (string-blank-p reason))
                 (not (string-match-p "[[:cntrl:]]" reason)))
      (error "Note disposition names an unadmitted note or unknown disposition"))
    (unless (if (eq action 'note-condense) (not (string-blank-p body)) (string-empty-p body))
      (error "Note disposition body does not match its disposition"))
    (append
     (list :action action :root (cadr note) :file (cddr note) :note (car note)
           :type "note" :title (cddr note) :hook "" :reason reason
           :evidence (list (car note)) :body body)
     (when (plist-member fields :open-items)
       (list :open-items (mevedel-memory-proposal--strings (plist-get fields :open-items) #'always)))
     (cond
      ((eq action 'note-delete)
       ;; An entry that is not "PATH: QUOTE" (an address, a placeholder such
       ;; as "none") is no evidence; acceptance then keeps a delete whose
       ;; open items lack enough valid quotes.  It never rejects the reply.
       (list :resolved-by (delq nil (mapcar #'mevedel-memory-proposal--resolution
                                            (mevedel-memory-proposal--strings
                                             (if (plist-member fields :resolved-by) (plist-get fields :resolved-by) [])
                                             #'always)))))
      ;; Resolution evidence only matters for a delete; elsewhere check shape.
      ((plist-member fields :resolved-by)
       (mevedel-memory-proposal--strings (plist-get fields :resolved-by) #'always)
       nil)))))

(defun mevedel-memory-proposal--item (text action roots evidence)
  "Decode proposal TEXT for ACTION against captured ROOTS and EVIDENCE."
  (let* ((decoded (mevedel-memory-proposal--fenced text mevedel-memory-proposal--fields))
         (fields (car decoded)) (body (cdr decoded)))
    (unless (and (= (length fields) (if (eq action 'merge) 16 14))
                 (eq (not (null (plist-member fields :merged-files))) (eq action 'merge)))
      (error "Proposal fields do not match its action"))
    (dolist (key '(:root :file :type :title :hook :reason))
      (let ((value (plist-get fields key)))
        (unless (and (stringp value) (not (string-blank-p value))
                     (not (string-match-p "[[:cntrl:]]" value)))
          (error "Invalid proposal field: %s" key))))
    (unless (member (plist-get fields :type) '("user" "feedback" "project" "reference"))
      (error "Unknown memory proposal type"))
    (let* ((root (cdr (assoc (plist-get fields :root) roots)))
           (file (plist-get fields :file))
           (ids (plist-get fields :evidence))
           (merged (plist-get fields :merged-files))
           (admitted (plist-get root :files))
           (new-file (and (plist-get root :complete)
                          (not (member file (plist-get root :existing))))))
      (unless root
        (error "Unknown proposal root"))
      (unless (mevedel-memory-proposal--file-p file)
        (error "Unsafe proposal file path"))
      (unless (vectorp ids)
        (error "Proposal evidence must be an array"))
      (unless (= (length ids) (length (delete-dups (append ids nil))))
        (error "Proposal evidence contains duplicate entries"))
      (unless (cl-every (lambda (id) (and (stringp id) (member id evidence))) ids)
        (error "Proposal evidence contains an invalid or unadmitted entry"))
      (if (eq action 'instructions)
          (unless (and (eq (plist-get root :kind) 'instructions)
                       (member file admitted))
            (error "Instruction proposal is not an applicable captured file"))
        (unless (and (eq (plist-get root :kind) 'memory)
                     (not (equal (file-name-nondirectory file) "MEMORY.md"))
                     (pcase action
                       ('promote new-file)
                       ((or 'update 'remove) (member file admitted))
                       ('merge
                        (and (or (member file admitted) new-file)
                             (vectorp merged) (>= (length merged) 2)
                             (= (length merged)
                                (length (delete-dups (append merged nil))))
                             (cl-every
                              (lambda (source)
                                (and (mevedel-memory-proposal--file-p source)
                                     (member source admitted)
                                     (not (equal (file-name-nondirectory source) "MEMORY.md"))))
                              merged)))))
          (error "Memory proposal action conflicts with its captured files")))
      (unless (if (eq action 'remove) (string-empty-p body)
                (not (string-blank-p body)))
        (error "Proposal replacement body does not match its action"))
      (setq fields (plist-put fields :evidence (append ids nil)))
      (when merged
        (setq fields (plist-put fields :merged-files (append merged nil))))
      (append (list :action action) fields (list :body body)))))

(defun mevedel-memory-proposal--block-end (fence)
  "Find the end of the proposal block opened by FENCE, starting at point.
Return the end of its content and leave point after the block.  The body
of a block ends only at a line of exactly FENCE, so Markdown code blocks
inside replacement bodies stay intact.  These unambiguous variants of real
replies are accepted too: a separator of three or more dashes, and a
backtick-only line, section heading or next proposal opener closing a block
whose body is still empty.  An unterminated non-empty body is an error."
  (let ((body nil) (empty t) end)
    (while (not end)
      (when (eobp) (error "Unclosed proposal fence"))
      (cond
       ((and empty (looking-at "`\\{3,\\}proposal$"))
        (setq end (point)))
       ((looking-at "`\\{3,\\}proposal$")
        (error "Unclosed proposal fence"))
       ((and empty (or (looking-at "`\\{3,\\}$") (looking-at "## ")))
        (setq end (point))
        (unless (looking-at "## ") (forward-line 1)))
       ((and body (looking-at (concat (regexp-quote fence) "$")))
        (setq end (point))
        (forward-line 1))
       ((and (not body) (looking-at "-\\{3,\\}$"))
        (setq body t)
        (forward-line 1))
       (t
        (when (and body (not (looking-at "[ \t]*$"))) (setq empty nil))
        (forward-line 1))))
    end))

(defun mevedel-memory-proposal-parse (text roots evidence &optional notes)
  "Return proposals and no-action text from a fully validated reply TEXT.
ROOTS maps opaque root IDs to captured scope plists. Each root has :kind
`memory' or `instructions', and :files naming admitted before-state files.
Memory roots additionally have :existing, the captured directory names,
and :complete, non-nil only when that observation proves new-file absence.
Instruction :files may include explicitly captured absent applicable files.
EVIDENCE lists the digest IDs admitted to this pass. This interface checks
names only; the scope owner retains canonical locators and content snapshots.
NOTES maps each admitted shared working note ID to (ROOT-ID . RELATIVE).
Memory proposals may cite those IDs as evidence.  When NOTES is non-nil, the
Notes section must dispose of every admitted note exactly once; otherwise
it may be omitted or contain `- none'.

Each ordered section contains `- none' or fenced proposal blocks. A block
has JSON-valued header lines, a `---' separator, and the replacement body;
a block with an empty body may omit the separator.
Its closing backtick fence must match its opener, allowing shorter fences
inside Markdown bodies. Remove proposals have empty bodies. No action has
plain bullets and, being commentary, may be omitted at the very end of the
reply.  Sections may appear in any order but each at most once; every
required section must be present, and proposals are returned in canonical
section order.  Conflicting file operations make the entire reply invalid."
  (unless (and (stringp text) (<= (string-bytes text) mevedel-memory-proposal--max-bytes)
               (not (string-blank-p text)))
    (error "Consolidation reply is empty or exceeds its byte limit"))
  (with-temp-buffer
    (setq-local case-fold-search nil)
    (insert (string-trim text) "\n")
    (goto-char (point-min))
    (let ((evidence (append evidence (mapcar #'car notes)))
          proposals touched no-action disposed seen)
      (cl-labels ((blank-lines ()
                    (while (looking-at "[ \t]*\n") (forward-line 1))))
        (blank-lines)
        (while (not (eobp))
          (unless (looking-at "## \\(.+\\)\n")
            (error "Text outside consolidation sections"))
          (let ((section (assoc (match-string-no-properties 1) mevedel-memory-proposal--sections)))
            (unless section (error "Unknown consolidation heading: %s" (match-string-no-properties 1)))
            (when (member (car section) seen)
              (error "Repeated consolidation heading: %s" (car section)))
            (push (car section) seen)
            (forward-line 1)
            (blank-lines)
            (cond
             ((null (cdr section))
              (let ((start (point)))
                (while (looking-at "- \\S-.*\n") (forward-line 1))
                (when (= start (point)) (error "No action section requires a bullet"))
                (setq no-action (string-trim (buffer-substring-no-properties start (point))))))
             ((looking-at "- none\n")
              (when (and (eq (cdr section) 'notes) notes)
                (error "Every admitted note requires a disposition"))
              (forward-line 1))
             (t
              (let ((count 0))
                (while (looking-at "\\(`\\{3,\\}\\)proposal\n")
                  (let ((fence (match-string-no-properties 1))
                        start end)
                    (forward-line 1)
                    (setq start (point))
                    (setq end (mevedel-memory-proposal--block-end fence))
                    (let* ((item (if (eq (cdr section) 'notes)
                                     (mevedel-memory-proposal--disposition
                                      (buffer-substring-no-properties start end) notes)
                                   (mevedel-memory-proposal--item
                                    (buffer-substring-no-properties start end)
                                    (cdr section) roots evidence)))
                           (files (delete-dups
                                   (cons (plist-get item :file)
                                         (copy-sequence (plist-get item :merged-files))))))
                      (dolist (file files)
                        (let ((target (cons (plist-get item :root) file)))
                          (when (member target touched)
                            (error "Conflicting proposals for the same file"))
                          (push target touched)))
                      (when (plist-get item :note) (push (plist-get item :note) disposed))
                      (push item proposals))
                    (cl-incf count)
                    (blank-lines)))
                (when (= count 0) (error "Action section requires proposals or none"))))))
          (blank-lines))
        ;; Every action section is required, so a reply truncated after an
        ;; early section cannot pass as complete; Notes only with admitted
        ;; notes, and No action is commentary.
        (dolist (section mevedel-memory-proposal--sections)
          (unless (or (member (car section) seen) (null (cdr section))
                      (and (eq (cdr section) 'notes) (null notes)))
            (error "Expected consolidation heading: %s" (car section))))
        (unless no-action (setq no-action "- none"))
        (unless (= (length disposed) (length notes))
          (error "Every admitted note requires exactly one disposition")))
      (let ((rank (lambda (item)
                    (cl-position (if (mevedel-memory-proposal-note-action-p (plist-get item :action))
                                     'notes (plist-get item :action))
                                 mevedel-memory-proposal--sections :key #'cdr))))
        (list :proposals (cl-stable-sort (nreverse proposals) #'< :key rank)
              :no-action no-action)))))

(provide 'mevedel-memory-proposal)
;;; mevedel-memory-proposal.el ends here
