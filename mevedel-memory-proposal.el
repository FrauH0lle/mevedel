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
    ("Remove" . remove) ("Instructions" . instructions) ("No action" . nil))
  "Closed ordered consolidation sections and their actions.")

(defconst mevedel-memory-proposal--fields
  '(("root" . :root) ("file" . :file) ("type" . :type)
    ("title" . :title) ("hook" . :hook) ("reason" . :reason)
    ("evidence" . :evidence) ("merged-files" . :merged-files))
  "Closed proposal header fields; values use JSON syntax.")

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
       (cl-every (lambda (part) (not (member part '("" "." ".mevedel-memory-write"))))
                 (split-string file "/"))))

(defun mevedel-memory-proposal--item (text action roots evidence)
  "Decode proposal TEXT for ACTION against captured ROOTS and EVIDENCE."
  (unless (string-match "^---\n" text)
    (error "Proposal lacks its header/body separator"))
  (let ((header (substring text 0 (match-beginning 0)))
        (body (string-remove-suffix "\n" (substring text (match-end 0))))
        fields seen)
    (dolist (line (split-string (string-remove-suffix "\n" header) "\n"))
      (unless (string-match "\\`\\([a-z-]+\\): \\(.+\\)\\'" line)
        (error "Invalid proposal header line"))
      (let ((key (cdr (assoc (match-string 1 line) mevedel-memory-proposal--fields)))
            (encoded (match-string 2 line)))
        (unless (and key (not (memq key seen)))
          (error "Unknown or repeated proposal field"))
        (push key seen)
        (setq fields (plist-put fields key
                                (json-parse-string encoded :null-object :null
                                                   :false-object :false)))))
    (unless (and (= (length seen) (if (eq action 'merge) 8 7))
                 (eq (not (null (memq :merged-files seen))) (eq action 'merge)))
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

(defun mevedel-memory-proposal-parse (text roots evidence)
  "Return proposals and no-action text from a fully validated reply TEXT.
ROOTS maps opaque root IDs to captured scope plists. Each root has :kind
`memory' or `instructions', and :files naming admitted before-state files.
Memory roots additionally have :existing, the captured directory names,
and :complete, non-nil only when that observation proves new-file absence.
Instruction :files may include explicitly captured absent applicable files.
EVIDENCE lists the digest IDs admitted to this pass. This interface checks
names only; the scope owner retains canonical locators and content snapshots.

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
    (let (proposals touched no-action)
      (cl-labels ((blank-lines ()
                    (while (looking-at "[ \t]*\n") (forward-line 1))))
        (dolist (section mevedel-memory-proposal--sections)
          (blank-lines)
          (unless (looking-at (concat (regexp-quote (concat "## " (car section))) "\n"))
            (error "Expected consolidation heading: %s" (car section)))
          (forward-line 1)
          (blank-lines)
          (if (null (cdr section))
              (let ((start (point)))
                (while (looking-at "- \\S-.*\n") (forward-line 1))
                (when (= start (point)) (error "No action section requires a bullet"))
                (setq no-action (string-trim (buffer-substring-no-properties start (point)))))
            (if (looking-at "- none\n")
                (forward-line 1)
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
        (blank-lines)
        (unless (eobp) (error "Text outside consolidation sections")))
      (list :proposals (nreverse proposals) :no-action no-action))))

(provide 'mevedel-memory-proposal)
;;; mevedel-memory-proposal.el ends here
