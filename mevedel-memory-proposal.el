;;; mevedel-memory-proposal.el -- Validated consolidation proposals -*- lexical-binding: t -*-

;;; Commentary:

;; Decodes one bounded consolidation reply against its captured file and
;; evidence scope.  Invalid memory, instruction or structural output invalidates
;; the complete reply.  Shared-note disposal is owned by the accepting store.
;; This module performs no filesystem access or writes.  Scope capture owns
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
    ("Remove" . remove) ("Instructions" . instructions)
    ("No action" . nil))
  "Closed ordered consolidation sections and their actions.")

(defconst mevedel-memory-proposal--fields
  '(("root" . :root) ("file" . :file) ("type" . :type)
    ("title" . :title) ("hook" . :hook) ("reason" . :reason)
    ("evidence" . :evidence) ("merged-files" . :merged-files))
  "Closed proposal header fields; values use JSON syntax.")

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

(defun mevedel-memory-proposal--fenced (text names)
  "Split fenced proposal TEXT into its decoded header plist and body.
NAMES maps the closed header field names to keywords.  Without a `---'
separator the body is empty and every line must be a header line; the
action's own body rule then decides whether an empty body is valid."
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
          (unless (and key (not (plist-member fields key)))
            (error "Unknown or repeated proposal field"))
          (setq fields (plist-put fields key value)))))))
    (cons fields body)))

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

(defun mevedel-memory-proposal--block-end (fence)
  "Find the end of the proposal block opened by FENCE, starting at point.
Return the end of its content and leave point after the block.  The body
of a block ends only at a line of FENCE plus optional horizontal whitespace,
so Markdown code blocks
inside replacement bodies stay intact.  These unambiguous variants of real
replies are accepted too: a separator of three or more dashes; a
backtick-only line or the next proposal opener closing a block whose body is
still empty; and a section heading closing a header-only block without a
separator.  After a separator a heading is body text."
  (let ((body nil) (empty t) end)
    (while (not end)
      (cond
       ((eobp) (error "Unclosed proposal fence"))
       ((and empty (looking-at "`\\{3,\\}proposal[ \t]*$"))
        (setq end (point)))
       ((looking-at "`\\{3,\\}proposal[ \t]*$")
        (error "Unclosed proposal fence"))
       ((or (and empty (looking-at "`\\{3,\\}[ \t]*$"))
            (and (not body) (looking-at "## ")))
        (setq end (point))
        (unless (looking-at "## ") (forward-line 1)))
       ((and body (looking-at (concat (regexp-quote fence) "[ \t]*$")))
        (setq end (point))
        (forward-line 1))
       ((and (not body) (looking-at "-\\{3,\\}$"))
        (setq body t)
        (forward-line 1))
       (t
        (when (and body (not (looking-at "[ \t]*$"))) (setq empty nil))
        (forward-line 1))))
    end))

(defun mevedel-memory-proposal-parse (text roots evidence)
  "Return validated memory/instruction proposals and no-action text from TEXT.
ROOTS maps opaque root IDs to captured scope plists. Each root has :kind
`memory' or `instructions', and :files naming admitted before-state files.
Memory roots additionally have :existing, the captured directory names,
and :complete, non-nil only when that observation proves new-file absence.
Instruction :files may include explicitly captured absent applicable files.
EVIDENCE lists the digest and shared-note IDs admitted to this pass.
This interface checks
names only; the scope owner retains canonical locators and content snapshots.

Each ordered section contains `- none' or fenced proposal blocks. A block
has JSON-valued header lines, a `---' separator, and the replacement body;
a block with an empty body may omit the separator.
Its closing backtick fence must match its opener, allowing shorter fences
inside Markdown bodies and trailing horizontal whitespace on fence lines.
Remove proposals have empty bodies.  No action has
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
    (let (proposals touched no-action seen)
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
                  (let ((fence (match-string-no-properties 1)) start end)
                    (forward-line 1)
                    (setq start (point))
                    (setq end (mevedel-memory-proposal--block-end fence))
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
                        (push item proposals))
                    (cl-incf count)
                    (blank-lines)))
                (when (= count 0) (error "Action section requires proposals or none"))))))
          (blank-lines))
        ;; Every action section is required, so a reply truncated after an
        ;; early section cannot pass as complete.
        (dolist (section mevedel-memory-proposal--sections)
          (unless (or (member (car section) seen) (null (cdr section)))
            (error "Expected consolidation heading: %s" (car section))))
        (unless no-action (setq no-action "- none")))
      (let ((rank (lambda (item)
                    (cl-position (plist-get item :action)
                                 mevedel-memory-proposal--sections :key #'cdr))))
        (list :proposals (cl-stable-sort (nreverse proposals) #'< :key rank)
              :no-action no-action)))))

(provide 'mevedel-memory-proposal)
;;; mevedel-memory-proposal.el ends here
