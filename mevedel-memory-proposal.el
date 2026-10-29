;;; mevedel-memory-proposal.el -- Validated consolidation proposals -*- lexical-binding: t -*-

;;; Commentary:

;; Decodes one bounded consolidation reply against its captured file and
;; evidence scope. Invalid note dispositions are isolated as no-ops; memory,
;; instruction and structural failures still invalidate the complete reply.
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
Notes may be omitted; omitted dispositions leave their notes unchanged.")

(defconst mevedel-memory-proposal--fields
  '(("root" . :root) ("file" . :file) ("type" . :type)
    ("title" . :title) ("hook" . :hook) ("reason" . :reason)
    ("evidence" . :evidence) ("merged-files" . :merged-files))
  "Closed proposal header fields; values use JSON syntax.")

(defconst mevedel-memory-proposal--note-fields
  '(("note" . :note) ("disposition" . :disposition) ("reason" . :reason))
  "Closed shared-note deletion header fields; values use JSON syntax.")

(defconst mevedel-memory-proposal--dispositions
  '(("delete" . note-delete))
  "Model-selectable shared-note dispositions and their actions.")

(defconst mevedel-memory-proposal-note-actions '(note-delete note-prune)
  "Proposal actions disposing of shared working files, not memory topics.")

(defun mevedel-memory-proposal-note-action-p (action)
  "Return non-nil when ACTION disposes of a shared working file."
  (memq action mevedel-memory-proposal-note-actions))

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
    (let ((lines (split-string (string-remove-suffix "\n" header) "\n")))
     (while lines
      (let ((line (pop lines)))
      (unless (string-match "\\`\\([a-z-]+\\): \\(.+\\)\\'" line)
        (error "Invalid proposal header line"))
      (let ((key (cdr (assoc (match-string 1 line) names)))
            (encoded (match-string 2 line)))
        ;; A JSON array or object value spread over following lines is
        ;; joined only until it parses; anything else stays invalid.
        (while (and (string-match-p "\\`[[{]" encoded) lines
                    (not (ignore-errors (and (json-parse-string encoded) t))))
          (setq encoded (concat encoded "\n" (pop lines))))
        (let ((value (and key (json-parse-string encoded :null-object :null
                                                 :false-object :false))))
          ;; An exactly repeated line is unambiguous; a differing repeat is not.
          (unless (and key (or (not (plist-member fields key))
                               (and repeats (equal value (plist-get fields key)))))
            (error "Unknown or repeated proposal field"))
          (setq fields (plist-put fields key value)))))))
    (cons fields body)))

(defun mevedel-memory-proposal--note-claims (text)
  "Return all distinct explicit note IDs in the header of block TEXT.
Read literal column-zero note fields independently of other field JSON.
JSON object keys and strings cannot contain such literal header lines.
Never infer an ID from a filename or body.  Reject undecodable ownership
rather than letting a later same-note proposal escape quarantine."
  (let* ((separator (string-match "^-\\{3,\\}\\(?:\n\\|\\'\\)" text))
         (lines (split-string (string-remove-suffix "\n" (if separator (substring text 0 separator) text)) "\n"))
         claims)
    (while lines
      (let ((line (pop lines)))
        (when (string-prefix-p "note:" line)
          (unless (string-match "\\`note: \\(.+\\)\\'" line)
            (error "Ambiguous note disposition target"))
          (let ((value (condition-case nil
                           (json-parse-string (match-string 1 line) :null-object :null :false-object :false)
                         (error (error "Ambiguous note disposition target")))))
            (unless (and (stringp value) (not (string-blank-p value))
                         (not (string-match-p "[[:cntrl:]]" value)))
              (error "Ambiguous note disposition target"))
            (cl-pushnew value claims :test #'equal)))))
    (unless claims (error "Missing note disposition target; ownership is ambiguous"))
    (nreverse claims)))

(defun mevedel-memory-proposal--disposition (text notes)
  "Decode one shared-note disposition TEXT for admitted NOTES.
NOTES maps each admitted note ID to (ROOT-ID . RELATIVE-PATH).
Only deletion with an empty body acknowledges a note."
  (let* ((decoded (mevedel-memory-proposal--fenced text mevedel-memory-proposal--note-fields t))
         (fields (car decoded)) (body (cdr decoded))
         (note (assoc (plist-get fields :note) notes))
         (action (cdr (assoc (plist-get fields :disposition) mevedel-memory-proposal--dispositions)))
         (reason (plist-get fields :reason)))
    (unless (cl-every (lambda (key) (plist-member fields key)) '(:note :disposition :reason))
      (error "Note disposition fields are incomplete"))
    (unless (and note action (stringp reason) (not (string-blank-p reason))
                 (not (string-match-p "[[:cntrl:]]" reason)))
      (error "Note disposition names an unadmitted note or unknown disposition"))
    (unless (string-blank-p body)
      (error "Note disposition body does not match its disposition"))
    (list :action action :root (cadr note) :file (cddr note) :note (car note)
          :type "note" :title (cddr note) :hook "" :reason reason
          :evidence (list (car note)) :body "")))

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
      (unless (pcase action
                ('remove (string-empty-p body))
                ('instructions t)
                (_ (not (string-blank-p body))))
        (error "Proposal replacement body does not match its action"))
      (setq fields (plist-put fields :evidence (append ids nil)))
      (when merged
        (setq fields (plist-put fields :merged-files (append merged nil))))
      (append (list :action action) fields (list :body body)))))

(defun mevedel-memory-proposal--block-end (fence &optional note-p)
  "Find the end of the proposal block opened by FENCE, starting at point.
Return the end of its content and leave point after the block.  The body
of a block ends only at a line of FENCE plus optional horizontal whitespace,
so Markdown code blocks
inside replacement bodies stay intact.  These unambiguous variants of real
replies are accepted too: a separator of three or more dashes; a
backtick-only line or the next proposal opener closing a block whose body is
still empty; and a section heading closing a header-only block without a
separator.  After a separator a heading is body text.
With NOTE-P, implicit boundaries require empty deletion metadata; only
a matching-width proposal opener or a final No action section can end it.
A note body without its matching fence consumes the unresolved tail and returns
an (END . ERROR) pair instead of rejecting unrelated proposals.  Header-looking
body lines never admit another target.  A same-width proposal opener in a
body is ambiguous and quarantines the tail too; its apparent closer might
belong to another note.  Missing required memory sections
inside that tail remain a structural error in the complete reply."
  (let ((start (point)) (body nil) (empty t) end failure)
    (cl-labels ((bodyless-note-p ()
                  (and note-p empty
                       (ignore-errors
                         (member (plist-get (car (mevedel-memory-proposal--fenced
                                                  (buffer-substring-no-properties start (point))
                                                  mevedel-memory-proposal--note-fields t))
                                            :disposition)
                                 '("delete"))))))
      (while (not end)
        (cond
         ((eobp)
          (if note-p (setq end (point) failure "Unclosed note proposal fence; unresolved tail skipped")
            (error "Unclosed proposal fence")))
         ((and (looking-at (if note-p (concat (regexp-quote fence) "proposal[ \t]*$")
                            "`\\{3,\\}proposal[ \t]*$"))
               (if note-p (bodyless-note-p) empty))
          (setq end (point)))
         ((and note-p (looking-at (concat (regexp-quote fence) "proposal[ \t]*$")))
          (setq end (point-max) failure "Ambiguous nested note proposal fence; unresolved tail skipped")
          (goto-char end))
         ((and (not note-p) (looking-at "`\\{3,\\}proposal[ \t]*$"))
          (error "Unclosed proposal fence"))
         ((or (and (if note-p (bodyless-note-p) empty)
                   (looking-at "`\\{3,\\}[ \t]*$"))
              (and (not note-p) (not body) (looking-at "## "))
              (and (looking-at "## No action$") (bodyless-note-p)
                   (save-excursion
                     (forward-line 1)
                     (while (looking-at "\\(?:- \\S-.*\\|[ \t]*\\)\n") (forward-line 1))
                     (eobp))))
          (setq end (point))
          (unless (looking-at "## ") (forward-line 1)))
         ((and (or body note-p) (looking-at (concat (regexp-quote fence) "[ \t]*$")))
          (setq end (point))
          (forward-line 1))
         ((and (not body) (looking-at "-\\{3,\\}$"))
          (setq body t)
          (forward-line 1))
         (t
          (when (and body (not (looking-at "[ \t]*$"))) (setq empty nil))
          (forward-line 1)))))
    (if failure (cons end failure) end)))

(defun mevedel-memory-proposal-parse (text roots evidence &optional notes)
  "Return validated proposals, no-action text and skipped notes from TEXT.
ROOTS maps opaque root IDs to captured scope plists. Each root has :kind
`memory' or `instructions', and :files naming admitted before-state files.
Memory roots additionally have :existing, the captured directory names,
and :complete, non-nil only when that observation proves new-file absence.
Instruction :files may include explicitly captured absent applicable files.
EVIDENCE lists the digest IDs admitted to this pass. This interface checks
names only; the scope owner retains canonical locators and content snapshots.
NOTES maps each admitted shared working note ID to (ROOT-ID . RELATIVE).
Memory proposals may cite those IDs as evidence.  Missing or invalid note
dispositions produce no proposal, leaving the note for a later review.
Duplicate note targets quarantine every attempt on that target, even when
one block is invalid.  Unknown IDs never resolve to admitted targets.
:skipped-notes is an alist of (ADMITTED-ID . ERROR), one bounded diagnostic
per skipped admitted note, also summarized in :no-action for durable review
visibility.  Its length is bounded by the captured NOTES roster.
:note-format-failure-count additionally counts unadmitted note blocks.

Each ordered section contains `- none' or fenced proposal blocks. A block
has JSON-valued header lines, a `---' separator, and the replacement body;
a block with an empty body may omit the separator.
Its closing backtick fence must match its opener, allowing shorter fences
inside Markdown bodies and trailing horizontal whitespace on fence lines.
Unclosed note bodies quarantine the unresolved tail rather than interpreting
its header-like content as more proposals.  Remove proposals have empty
bodies.  No action has
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
          proposals touched no-action disposed seen note-attempts skipped
          (unknown-count 0))
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
              (forward-line 1))
             (t
              (let ((count 0))
                (while (looking-at "\\(`\\{3,\\}\\)proposal[ \t]*\n")
                  (let ((fence (match-string-no-properties 1))
                        start end boundary)
                    (forward-line 1)
                    (setq start (point))
                    (setq boundary (mevedel-memory-proposal--block-end fence (eq (cdr section) 'notes))
                          end (if (consp boundary) (car boundary) boundary))
                    (if (eq (cdr section) 'notes)
                        (let* ((block (buffer-substring-no-properties start end))
                               (claims (mevedel-memory-proposal--note-claims block))
                               item (failure (and (consp boundary) (cdr boundary))))
                          (unless failure
                            (condition-case err
                                (setq item (mevedel-memory-proposal--disposition block notes))
                              (error (setq failure (error-message-string err)))))
                          (push (list claims item failure) note-attempts))
                      (let* ((item (mevedel-memory-proposal--item
                                    (buffer-substring-no-properties start end)
                                    (cdr section) roots evidence))
                             (files (delete-dups
                                     (cons (plist-get item :file)
                                           (copy-sequence (plist-get item :merged-files))))))
                        (dolist (file files)
                          (let ((target (cons (plist-get item :root) file)))
                            (when (member target touched)
                              (error "Conflicting proposals for the same file"))
                            (push target touched)))
                        (push item proposals)))
                    (cl-incf count)
                    (blank-lines)))
                (when (= count 0) (error "Action section requires proposals or none"))))))
          (blank-lines))
        ;; Every action section is required, so a reply truncated after an
        ;; early section cannot pass as complete.  Notes can be missing:
        ;; unlike a memory edit, an omitted disposition changes nothing.
        (dolist (section mevedel-memory-proposal--sections)
          (unless (or (member (car section) seen) (null (cdr section))
                      (eq (cdr section) 'notes))
            (error "Expected consolidation heading: %s" (car section))))
        (unless no-action (setq no-action "- none"))
        (let (claimed blocked failures)
          ;; Reserve targets from invalid blocks too, before admitting any
          ;; valid block.  Captured aliases of the same file also conflict.
          (dolist (attempt note-attempts)
            (dolist (id (car attempt))
              (when-let* ((note (assoc id notes)) (target (cdr note)))
                (if (member target claimed) (push target blocked)
                  (push target claimed)))))
          (dolist (attempt (nreverse note-attempts))
            (let* ((ids (car attempt)) (item (cadr attempt))
                   (failure (nth 2 attempt))
                   (admitted (cl-remove-if-not (lambda (id) (assoc id notes)) ids)))
              (if (and item (not (member (cdr (assoc (plist-get item :note) notes)) blocked)))
                  (let ((target (cons (plist-get item :root) (plist-get item :file))))
                    (when (member target touched)
                      (error "Conflicting proposals for the same file"))
                    (push target touched)
                    (push (plist-get item :note) disposed)
                    (push item proposals))
                (dolist (id admitted)
                  (push (cons id (if (member (cdr (assoc id notes)) blocked)
                                    "Duplicate note target; all attempts skipped"
                                  failure)) failures))
                (unless admitted (cl-incf unknown-count)))))
          (dolist (note notes)
            (unless (member (car note) disposed)
              (push (cons (car note)
                          (truncate-string-to-width
                           (replace-regexp-in-string
                            "[[:cntrl:]]" " "
                            (or (cdr (assoc (car note) failures)) "Missing note disposition"))
                           240 nil nil t)) skipped)))))
      (setq skipped (nreverse skipped))
      (when skipped
        (setq no-action
              (concat no-action "\n"
                      (mapconcat (lambda (entry)
                                   (format "- Note NO OP: %s (%s)." (car entry) (cdr entry)))
                                 skipped "\n"))))
      (when (> unknown-count 0)
        (setq no-action (concat no-action (format "\n- Note NO OP: %d unadmitted note blocks skipped." unknown-count))))
      (let ((rank (lambda (item)
                    (cl-position (if (mevedel-memory-proposal-note-action-p (plist-get item :action))
                                     'notes (plist-get item :action))
                                 mevedel-memory-proposal--sections :key #'cdr))))
        (list :proposals (cl-stable-sort (nreverse proposals) #'< :key rank)
              :no-action no-action :skipped-notes skipped
              :note-format-failure-count (+ (length skipped) unknown-count))))))

(provide 'mevedel-memory-proposal)
;;; mevedel-memory-proposal.el ends here
