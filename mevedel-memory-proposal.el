;;; mevedel-memory-proposal.el -- Validated consolidation proposals -*- lexical-binding: t -*-

;;; Commentary:

;; Decodes one bounded consolidation reply against its captured file and
;; evidence scope. No proposal is returned unless the entire reply is valid.
;; This module performs no filesystem access or writes. Scope capture owns
;; canonical roots and before-state; application must recheck their authority
;; and freshness before acting on these untrusted suggestions.

;;; Code:

(eval-when-compile (require 'cl-lib))
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
  '(("note" . :note) ("disposition" . :disposition) ("reason" . :reason))
  "Closed shared-note disposition header fields; values use JSON syntax.")

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

(defun mevedel-memory-proposal--file-p (file)
  "Return non-nil for an ordinary relative Markdown FILE below a root."
  (and (mevedel-memory-proposal-relative-path-p file)
       (string-suffix-p ".md" file)
       (cl-every (lambda (part) (not (member part '("" "." ".mevedel"))))
                 (split-string file "/"))))

(defun mevedel-memory-proposal--fenced (text names)
  "Split fenced proposal TEXT into its decoded header plist and body.
NAMES maps the closed header field names to keywords."
  (unless (string-match "^---\n" text)
    (error "Proposal lacks its header/body separator"))
  (let ((header (substring text 0 (match-beginning 0)))
        (body (string-remove-suffix "\n" (substring text (match-end 0))))
        fields)
    (dolist (line (split-string (string-remove-suffix "\n" header) "\n"))
      (unless (string-match "\\`\\([a-z-]+\\): \\(.+\\)\\'" line)
        (error "Invalid proposal header line"))
      (let ((key (cdr (assoc (match-string 1 line) names)))
            (encoded (match-string 2 line)))
        (unless (and key (not (plist-member fields key)))
          (error "Unknown or repeated proposal field"))
        (setq fields (plist-put fields key
                                (json-parse-string encoded :null-object :null
                                                   :false-object :false)))))
    (cons fields body)))

(defun mevedel-memory-proposal--disposition (text notes)
  "Decode one shared-note disposition TEXT for admitted NOTES.
NOTES maps each admitted note ID to (ROOT-ID . RELATIVE-PATH)."
  (let* ((decoded (mevedel-memory-proposal--fenced text mevedel-memory-proposal--note-fields))
         (fields (car decoded)) (body (cdr decoded))
         (note (assoc (plist-get fields :note) notes))
         (action (cdr (assoc (plist-get fields :disposition) mevedel-memory-proposal--dispositions)))
         (reason (plist-get fields :reason)))
    (unless (= (length fields) (* 2 (length mevedel-memory-proposal--note-fields)))
      (error "Note disposition fields are incomplete"))
    (unless (and note action (stringp reason) (not (string-blank-p reason))
                 (not (string-match-p "[[:cntrl:]]" reason)))
      (error "Note disposition names an unadmitted note or unknown disposition"))
    (unless (if (eq action 'note-condense) (not (string-blank-p body)) (string-empty-p body))
      (error "Note disposition body does not match its disposition"))
    (list :action action :root (cadr note) :file (cddr note) :note (car note)
          :type "note" :title (cddr note) :hook "" :reason reason
          :evidence (list (car note)) :body body)))

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
      (unless (and root (mevedel-memory-proposal--file-p file)
                   (vectorp ids)
                   (= (length ids) (length (delete-dups (append ids nil))))
                   (cl-every (lambda (id) (and (stringp id) (member id evidence))) ids))
        (error "Proposal target or evidence is outside the captured scope"))
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
has JSON-valued header lines, a `---' separator, and the replacement body.
Its closing backtick fence must match its opener, allowing shorter fences
inside Markdown bodies. Remove proposals have empty bodies. No action has
plain bullets. Conflicting file operations make the entire reply invalid."
  (unless (and (stringp text) (<= (string-bytes text) mevedel-memory-proposal--max-bytes)
               (not (string-blank-p text)))
    (error "Consolidation reply is empty or exceeds its byte limit"))
  (with-temp-buffer
    (setq-local case-fold-search nil)
    (insert (string-trim text) "\n")
    (goto-char (point-min))
    (let ((evidence (append evidence (mapcar #'car notes)))
          proposals touched no-action disposed)
      (cl-labels ((blank-lines ()
                    (while (looking-at "[ \t]*\n") (forward-line 1))))
        (dolist (section mevedel-memory-proposal--sections)
          (blank-lines)
          (cond
           ((not (looking-at (concat (regexp-quote (concat "## " (car section))) "\n")))
            ;; Only an optional Notes section may be absent.
            (unless (and (eq (cdr section) 'notes) (null notes))
              (error "Expected consolidation heading: %s" (car section))))
           (t
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
                    (unless (re-search-forward (concat "^" (regexp-quote fence) "$") nil t)
                      (error "Unclosed proposal fence"))
                    (setq end (match-beginning 0))
                    (forward-line 1)
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
                (when (= count 0) (error "Action section requires proposals or none"))))))))
        (unless (= (length disposed) (length notes))
          (error "Every admitted note requires exactly one disposition"))
        (blank-lines)
        (unless (eobp) (error "Text outside consolidation sections")))
      (list :proposals (nreverse proposals) :no-action no-action))))

(provide 'mevedel-memory-proposal)
;;; mevedel-memory-proposal.el ends here
