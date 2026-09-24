;;; mevedel-memory-apply.el -- Prepare checked memory transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Derive complete topic/index or instruction changes from accepted before-state.
;; Preparation is read-only. The decision owner must retain a durable write
;; intent and hold workspace/target authority before calling the shared patch
;; transaction with these changes.

;;; Code:

(eval-when-compile (require 'cl-lib) (require 'rx))
(require 'mevedel-memory-scope)
(require 'url-util)

(defun mevedel-memory-apply--index-lines (text)
  "Parse index TEXT into (DESTINATION . LINE) rows, retaining all lines.
Non-entry lines have a nil destination. Reject ambiguous or unsafe indexes."
  (let (seen rows)
    (dolist (line (split-string text "\n") (nreverse rows))
      (let (target)
        (if (string-match
             (rx string-start "- [" (* (or (seq "\\" nonl) (not (any "]" "\\" "\n"))))
                 "](" (group (+ (not (any ")" "\n")))) ")" (or " - " ": ") (+ nonl) string-end)
             line)
            (progn
              (setq target (decode-coding-string (url-unhex-string (match-string 1 line)) 'utf-8-unix))
              (unless (and (mevedel-memory-proposal--file-p target)
                           (not (equal (file-name-nondirectory target) "MEMORY.md"))
                           (not (member target seen)))
                (error "Memory index has a duplicate or invalid destination"))
              (push target seen))
          (when (string-match-p "\\[.*\\](" line)
            (error "Memory index has unsupported link syntax")))
        (push (cons target line) rows)))))

(defun mevedel-memory-apply--index-conflicts (before current proposal)
  "Return PROPOSAL destinations whose entries differ in BEFORE and CURRENT.
Both arguments are index snapshots. Absence means an empty index. Unrelated
entries, prose, and entry positions do not participate in the comparison."
  (let ((old (mevedel-memory-apply--index-lines
              (decode-coding-string (or (plist-get before :bytes) "") 'utf-8-unix)))
        (new (mevedel-memory-apply--index-lines
              (decode-coding-string (or (plist-get current :bytes) "") 'utf-8-unix))))
    (seq-filter (lambda (file) (not (equal (cdr (assoc file old)) (cdr (assoc file new)))))
                (delete-dups (cons (plist-get proposal :file) (copy-sequence (plist-get proposal :merged-files)))))))

(defun mevedel-memory-apply--index (text proposal)
  "Return index TEXT with PROPOSAL's entries replaced, merged, or removed.
Preserve unrelated lines. Reject duplicate destinations, out-of-root links,
and unsupported link syntax instead of silently creating conflicting entries."
  (let* ((file (plist-get proposal :file))
         (affected (cons file (plist-get proposal :merged-files)))
         (label (mapconcat (lambda (char) (concat (when (memq char '(?\[ ?\] ?\\)) "\\") (char-to-string char)))
                           (plist-get proposal :title) ""))
         (replacement (unless (eq (plist-get proposal :action) 'remove)
                        (format "- [%s](%s) - %s" label
                                (mapconcat #'url-hexify-string (split-string file "/") "/")
                                (plist-get proposal :hook))))
         lines inserted)
    (dolist (row (mevedel-memory-apply--index-lines text))
      (if (member (car row) affected)
          (when (and replacement (not inserted)) (push replacement lines) (setq inserted t))
        (push (cdr row) lines)))
    (let ((result (string-join (nreverse lines) "\n")))
      (if (or (not replacement) inserted) result
        (concat result (unless (or (string-empty-p result) (string-suffix-p "\n" result)) "\n") replacement "\n")))))

(defun mevedel-memory-apply-changes (scope proposal)
  "Prepare complete patch changes for accepted PROPOSAL in original SCOPE.
Each change includes :before and exact proposed bytes; writes also carry
decoded :content for visited-buffer synchronization. No target is written.
The caller must persist these states and check every captured dependency,
including unchanged indexes, before applying with workspace and target
ownership. The patch transaction rechecks each change's expected snapshot."
  (let* ((id (plist-get proposal :root))
         (root (mevedel-memory-scope--root scope id))
         (action (plist-get proposal :action))
         (file (plist-get proposal :file))
         (body (plist-get proposal :body))
         (before (plist-get proposal :before))
         (affected (delete-dups (append (list file) (plist-get proposal :merged-files)
                                        (unless (eq action 'instructions) (list "MEMORY.md")))))
         changes)
    (unless (and (memq action '(promote update merge remove instructions))
                 (eq (eq action 'instructions) (eq (plist-get root :kind) 'instructions))
                 (stringp body) (not (string-search "\0" body))
                 (not (cl-some #'mevedel--invalid-message-char-p body)))
      (error "Invalid accepted memory action"))
    (dolist (name affected)
      (unless (equal (cdr (assoc name before)) (mevedel-memory-scope-before scope id name))
        (error "Proposal before-state differs from its captured scope")))
    (cl-labels
        ((snapshot (name) (or (cdr (assoc name before)) (error "Missing proposal before-state")))
         (emit (name text exists)
           (let* ((original (snapshot name))
                  (bytes (and exists (encode-coding-string text 'utf-8-unix))))
             (when (and bytes (> (length bytes) mevedel-memory-scope--max-file-bytes))
               (error "Proposed memory file exceeds its readable byte limit"))
             (unless (and (eq exists (plist-get original :exists)) (equal bytes (plist-get original :bytes)))
               (push (append (list :path (plist-get original :path) :before original :action (if exists 'write 'delete))
                             (when exists (list :content text :bytes bytes
                                                :mode (or (plist-get original :mode) (logand #o666 (default-file-modes))))))
                     changes)))))
      (if (eq action 'instructions)
          (let ((original (decode-coding-string (or (plist-get (snapshot file) :bytes) "") 'utf-8-unix)))
            (emit file (concat original (unless (string-empty-p original)
                                         (if (string-suffix-p "\n" original) "\n" "\n\n"))
                               (string-trim-right body "\n") "\n") t))
        (let* ((index-before (snapshot "MEMORY.md"))
               (index (mevedel-memory-apply--index
                       (decode-coding-string (or (plist-get index-before :bytes) "") 'utf-8-unix) proposal)))
          (if (eq action 'remove) (emit file nil nil)
            (unless (member (plist-get proposal :type) '("user" "feedback" "project" "reference"))
              (error "Invalid accepted memory type"))
            (emit file (format "---\nname: %s\ndescription: %s\ntype: %s\n---\n\n%s\n"
                               (decode-coding-string (json-serialize (plist-get proposal :title)) 'utf-8-unix)
                               (decode-coding-string (json-serialize (plist-get proposal :hook)) 'utf-8-unix)
                               (plist-get proposal :type) (string-trim-right body "\n")) t))
          (dolist (source (plist-get proposal :merged-files))
            (unless (equal file source) (emit source nil nil)))
          (emit "MEMORY.md" index (or (plist-get index-before :exists) (not (string-empty-p index))))))
      (nreverse changes))))

(provide 'mevedel-memory-apply)
;;; mevedel-memory-apply.el ends here
