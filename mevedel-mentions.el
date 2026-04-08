;;; mevedel-mentions.el -- @ref and @file mention system -*- lexical-binding: t -*-

;;; Commentary:

;; @ref/@file expansion, completion-at-point, and font-lock support
;; for gptel-mode chat buffers.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-overlays)

;; `mevedel-persistence'
(declare-function mevedel--restore-file-instructions "mevedel-persistence" (file &optional message))

;; `mevedel-utilities'
(declare-function mevedel--overlay-region-info "mevedel-utilities" (overlay))
(declare-function mevedel--delimiting-markdown-backticks "mevedel-utilities" (str))
(declare-function mevedel--markdown-enquote "mevedel-utilities" (text))

;; `mevedel-workspace'
(declare-function mevedel-workspace--root "mevedel-workspace" (workspace))
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;; `text-property-search'
(declare-function text-property-search-backward "text-property-search" (property &optional value predicate not-current))


;;
;;; Reference expansion in gptel-mode buffers

(defun mevedel--parse-ref-mentions (text)
  "Extract all @ref mentions from TEXT.

Returns a list of plists with :type, :value, :start, and :end keys. Type
is either \\='id or \\='tag, value is the ID number or tag query string."
  (let ((mentions ())
        (pos 0))
    ;; Parse @ref:123 (ID-based)
    (while (string-match "@ref:\\([0-9]+\\)" text pos)
      (push (list :type 'id
                  :value (string-to-number (match-string 1 text))
                  :start (match-beginning 0)
                  :end (match-end 0)
                  :match-text (match-string 0 text))
            mentions)
      (setq pos (match-end 0)))
    ;; Parse @ref{tag query} (tag-based)
    (setq pos 0)
    (while (string-match "@ref{\\([^}]+\\)}" text pos)
      (push (list :type 'tag
                  :value (match-string 1 text)
                  :start (match-beginning 0)
                  :end (match-end 0)
                  :match-text (match-string 0 text))
            mentions)
      (setq pos (match-end 0)))
    ;; Sort by position (earliest first)
    (sort mentions (lambda (a b) (< (plist-get a :start) (plist-get b :start))))))

(defun mevedel--resolve-ref-by-id (id)
  "Look up reference by numeric ID.
Returns the reference overlay or nil if not found or not a reference."
  (when-let* ((instr (mevedel--instruction-with-id id)))
    (when (mevedel--referencep instr)
      instr)))

(defun mevedel--resolve-refs-by-tag-query (query-string)
  "Filter references by tag QUERY-STRING.
Returns list of reference overlays matching the query.
For @ref mentions, excludes untagged references even if
`mevedel-always-match-untagged-references' is t."
  (condition-case _err
      (let* ((query (mevedel--tag-query-prefix-from-infix
                     (read (concat "(" query-string ")"))))
             ;; Temporarily disable always-match-untagged for explicit @ref queries
             (mevedel-always-match-untagged-references nil)
             (refs (mevedel--filter-references query)))
        refs)
    (error
     ;; Return empty list if query is invalid
     nil)))

(defun mevedel--format-single-reference (ref)
  "Format a single reference REF as markdown.
Returns a string with the reference header and content."
  (cl-destructuring-bind (ref-info-string ref-string)
      (mevedel--overlay-region-info ref)
    (let ((markdown-delimiter
           (mevedel--delimiting-markdown-backticks ref-string))
          (rel-path (file-relative-name
                     (buffer-file-name (overlay-buffer ref))
                     (mevedel-workspace--root (mevedel-workspace)))))
      (concat
       (format "#### Reference #%d\n\n" (mevedel--instruction-id ref))
       (if (mevedel--instruction-bufferlevel-p ref)
           (format "File `%s`." rel-path)
         (format "In file `%s`, %s" rel-path ref-info-string))
       (if (or (mevedel--instruction-bufferlevel-p ref)
               (not mevedel-include-full-instructions))
           "."
         (concat
          ":"
          (format "\n\n%s\n%s\n%s"
                  markdown-delimiter
                  ref-string
                  markdown-delimiter)))
       (let ((commentary (mevedel--commentary-text ref)))
         (if (string-empty-p commentary)
             ""
           (format "\n\nCommentary:\n\n%s"
                   (mevedel--markdown-enquote commentary))))))))

(defun mevedel--format-ref-section (refs)
  "Generate markdown References section for REFS list.
Returns a string with all references formatted."
  (if (null refs)
      ""
    (concat
     (format "### Reference%s\n" (if (> (length refs) 1) "s" ""))
     (mapconcat #'mevedel--format-single-reference refs "\n\n"))))

(defun mevedel--expand-ref-mentions-in-string (text)
  "Expand all @ref mentions in TEXT to reference content.

Collects all mentioned references and adds them as a References section
at the beginning of the text. Replaces @ref mentions with readable
references to the Reference section.

Returns the expanded text."
  (let* ((mentions (mevedel--parse-ref-mentions text))
         (refs-seen (make-hash-table :test 'equal))
         (all-refs ()))

    ;; Collect all referenced instructions
    (dolist (mention mentions)
      (pcase (plist-get mention :type)
        ('id
         (when-let* ((id (plist-get mention :value))
                     (ref (mevedel--resolve-ref-by-id id)))
           (unless (gethash id refs-seen)
             (puthash id ref refs-seen)
             (push ref all-refs))))
        ('tag
         (let* ((query (plist-get mention :value))
                (matching-refs (mevedel--resolve-refs-by-tag-query query)))
           (dolist (ref matching-refs)
             (let ((id (mevedel--instruction-id ref)))
               (unless (gethash id refs-seen)
                 (puthash id ref refs-seen)
                 (push ref all-refs))))))))

    ;; Sort refs by ID for consistent output
    (setq all-refs (sort all-refs
                         (lambda (a b)
                           (< (mevedel--instruction-id a)
                              (mevedel--instruction-id b)))))

    ;; Replace mentions in text (in reverse order to preserve positions)
    (let ((result text))
      (dolist (mention (nreverse mentions))
        (let* ((start (plist-get mention :start))
               (end (plist-get mention :end))
               (type (plist-get mention :type))
               (replacement
                (pcase type
                  ('id
                   (let ((id (plist-get mention :value)))
                     (if (gethash id refs-seen)
                         (format "Reference #%d" id)
                       ;; Invalid reference
                       (format "[invalid @ref:%d]" id))))
                  ('tag
                   (let* ((query (plist-get mention :value))
                          (matching-refs (mevedel--resolve-refs-by-tag-query query))
                          (count (length matching-refs)))
                     (cond
                      ((zerop count)
                       (format "[no references matching '%s']" query))
                      ((= count 1)
                       (format "Reference #%d"
                               (mevedel--instruction-id (car matching-refs))))
                      (t
                       (format "References %s"
                               (mapconcat
                                (lambda (ref)
                                  (format "#%d" (mevedel--instruction-id ref)))
                                matching-refs
                                ", ")))))))))
          (setq result (concat (substring result 0 start)
                               replacement
                               (substring result end)))))

      ;; Add references section at the beginning if any refs found
      (if (null all-refs)
          result
        (concat (mevedel--format-ref-section all-refs)
                "\n\n"
                result)))))

(defun mevedel--transform-expand-refs (&optional _fsm)
  "GPtel transform function to expand @ref mentions.
Operates on the current buffer (the prompt buffer) to expand @ref
mentions inline. Only processes the last user prompt, not the entire
conversation history. This is a synchronous transform function."
  ;; Only process if we're in a buffer with mevedel instructions
  (when (and (boundp 'mevedel--instructions)
             mevedel--instructions)
    ;; Find the start of the last user prompt (like gptel--transform-apply-preset does)
    (text-property-search-backward 'gptel nil t)
    (let ((prompt-start (point)))
      ;; Search for @ref mentions from this point forward
      (when (re-search-forward "@ref" nil t)
        ;; Get text from start of last prompt to end of buffer
        (let* ((prompt-text (buffer-substring-no-properties prompt-start (point-max)))
               (expanded-text (mevedel--expand-ref-mentions-in-string prompt-text)))
          ;; Replace just this prompt's content with expanded text
          (delete-region prompt-start (point-max))
          (goto-char prompt-start)
          (insert expanded-text))))))


;;
;;; Completion-at-point support for @ref mentions

(defun mevedel-ref-capf ()
  "Completion-at-point function for @ref mentions.
Provides completion for both @ref:ID and @ref{tag-query} syntax."
  (when (bound-and-true-p mevedel--instructions)
    (save-excursion
      (let ((orig-point (point)))
        ;; Try to match @ref:ID pattern
        (if (and (skip-chars-backward "0-9")
                 (looking-back "@ref:" (- (point) 5)))
            (let* ((start (point))
                   (end (+ start (skip-chars-forward "0-9")))
                   ;; Build completion table from all reference IDs
                   (candidates
                    (delq nil
                          (mapcar
                           (lambda (ref)
                             (let* ((id (mevedel--instruction-id ref))
                                    (id-str (number-to-string id))
                                    (buffer (overlay-buffer ref))
                                    (file-name (buffer-file-name buffer))
                                    (rel-path (when file-name
                                                (file-relative-name
                                                 file-name
                                                 (mevedel-workspace--root (mevedel-workspace)))))
                                    (line (with-current-buffer buffer
                                            (line-number-at-pos (overlay-start ref))))
                                    (content (with-current-buffer buffer
                                               (buffer-substring-no-properties
                                                (overlay-start ref)
                                                (overlay-end ref))))
                                    (preview (truncate-string-to-width
                                              (string-trim (replace-regexp-in-string "[\n\r]+" " " content))
                                              50 nil nil "...")))
                               ;; Store reference info as text property for annotation
                               (propertize id-str
                                           'mevedel-ref ref
                                           'mevedel-file rel-path
                                           'mevedel-line line
                                           'mevedel-preview preview)))
                           (mevedel--foreach-instruction instr
                                                         when (mevedel--referencep instr)
                                                         collect instr)))))
              (list start end candidates
                    :exclusive 'no
                    :annotation-function
                    #'(lambda (cand)
                        (let ((file (get-text-property 0 'mevedel-file cand))
                              (line (get-text-property 0 'mevedel-line cand))
                              (preview (get-text-property 0 'mevedel-preview cand)))
                          (format " %s [%s:%d]" preview file line)))))

          ;; Try to match @ref{tag-query} pattern
          (goto-char orig-point)
          (when (looking-back "@ref{[^}]*" (line-beginning-position))
            (let* ((start (save-excursion
                            (search-backward "{")
                            (1+ (point))))
                   (end (save-excursion
                          (skip-chars-forward "^}")
                          (point)))
                   ;; Build completion table from all tags
                   (candidates
                    (let ((all-tags (mevedel--available-tags)))
                      ;; Return tag completions with match counts
                      (mapcar
                       (lambda (tag)
                         ;; Convert tag to string if it's a symbol
                         (let* ((tag-str (if (symbolp tag) (symbol-name tag) tag))
                                (refs (mevedel--resolve-refs-by-tag-query tag-str))
                                (count (length refs)))
                           (propertize tag-str
                                       'mevedel-match-count count)))
                       all-tags))))
              (list start end candidates
                    :exclusive 'no
                    :annotation-function
                    (lambda (cand)
                      (let ((count (get-text-property 0 'mevedel-match-count cand)))
                        (format " [%d ref%s]" count (if (= count 1) "" "s"))))))))))))

(defun mevedel-file-capf ()
  "Completion-at-point function for @file: mentions.
Provides hierarchical directory-by-directory file completion.
When a file is selected, replaces @file:path with the absolute path."
  (save-excursion
    (let ((orig-point (point)))
      ;; Look back to find @file: pattern
      (when (re-search-backward "@file:" (line-beginning-position) t)
        (let* ((prefix-start (point))  ; Start of @file:
               (path-start (+ prefix-start 6))  ; Position after @file:
               (path-end (progn
                           (goto-char orig-point)
                           (skip-chars-forward "^ \t\n")
                           (point)))
               (current-input (buffer-substring-no-properties path-start path-end))
               (workspace (mevedel-workspace))
               (workspace-root (when workspace (mevedel-workspace--root workspace))))
          (when workspace-root
            ;; Parse current input to determine directory context
            (let* ((dir-part (file-name-directory current-input))
                   (current-dir (expand-file-name (or dir-part "") workspace-root))
                   ;; Get immediate children of current directory
                   (candidates
                    (when (file-directory-p current-dir)
                      (let* ((entries (directory-files current-dir nil "^[^.]"))
                             (file-entries
                              (delq nil
                                    (mapcar
                                     (lambda (entry)
                                       (let* ((full-path (expand-file-name entry current-dir))
                                              (is-dir (file-directory-p full-path)))
                                         ;; Skip excluded directories
                                         (unless (and is-dir
                                                      (member entry '(".git" ".svn" "node_modules"
                                                                      ".venv" "__pycache__" "build" "dist")))
                                           ;; Construct candidate in context of current input
                                           (propertize (concat (or dir-part "")
                                                               entry
                                                               (when is-dir "/"))
                                                       'mevedel-abs-path full-path
                                                       'mevedel-is-dir is-dir))))
                                     entries)))
                             (parent-dir (expand-file-name ".." current-dir))
                             ;; Add "." to represent current directory (for finalizing on it)
                             (current-dir-entry
                              (when dir-part  ; Only show when we're in a subdirectory
                                (propertize (concat (or dir-part "") ".")
                                            'mevedel-abs-path current-dir
                                            'mevedel-is-dir 'current))))
                        ;; Build final candidate list: special entries + file entries
                        (append (delq nil
                                      (list
                                       ;; Add .. unless at filesystem root
                                       (when (not (string= current-dir "/"))
                                         (propertize (concat (or dir-part "") "../")
                                                     'mevedel-abs-path parent-dir
                                                     'mevedel-is-dir t))
                                       ;; Add . to finalize on current directory
                                       current-dir-entry))
                                file-entries)))))
              (when candidates
                (list path-start path-end candidates
                      :exclusive 'no
                      :exit-function
                      (lambda (str status)
                        ;; Replace @file:path with absolute path for:
                        ;; 1. Files (not directories) - always expand
                        ;; 2. Special "." entry (current dir) - expand to finalize on directory
                        ;; Regular directories don't expand to allow navigation
                        (when (memq status '(finished sole exact))
                          (let ((abs-path (get-text-property 0 'mevedel-abs-path str))
                                (is-dir (get-text-property 0 'mevedel-is-dir str)))
                            ;; Convert to absolute if: it's a file OR it's the current dir marker
                            (when (and abs-path
                                       (or (not is-dir)              ; Files
                                           (eq is-dir 'current)))    ; Current dir marker "."
                              ;; Delete from @file: to current point and insert absolute path
                              (let ((end-pos (point)))
                                (delete-region prefix-start end-pos)
                                (insert abs-path)
                                ;; Add trailing / for current directory marker
                                (when (and (eq is-dir 'current)
                                           (not (eq (char-before) ?/)))
                                  (insert "/")))))))
                      :annotation-function
                      (lambda (cand)
                        (let ((is-dir (get-text-property 0 'mevedel-is-dir cand)))
                          (cond
                           ((eq is-dir 'current) " [current dir]")
                           (is-dir " [dir]")
                           (t " [file]")))))))))))))


;;
;;; Font-lock support for @ref mentions

(defun mevedel--fontify-ref-id-keyword (end)
  "Font-lock matcher for @ref:ID mentions up to END.
Highlights valid reference IDs."
  (and (re-search-forward "@ref:\\([0-9]+\\)" end t)
       ;; Check if preceded by whitespace or at beginning
       (or (= (match-beginning 0) (point-min))
           (memq (char-syntax (char-before (match-beginning 0))) '(32 62))
           (not (plist-get (text-properties-at (match-beginning 1)) 'gptel)))))

(defun mevedel--fontify-ref-tag-keyword (end)
  "Font-lock matcher for @ref{tag} mentions up to END.
Highlights valid tag queries."
  (and (re-search-forward "@ref{\\([^}]+\\)}" end t)
       ;; Check if preceded by whitespace or at beginning
       (or (= (match-beginning 0) (point-min))
           (memq (char-syntax (char-before (match-beginning 0))) '(32 62))
           (not (plist-get (text-properties-at (match-beginning 1)) 'gptel)))))

(defun mevedel--fontify-file-keyword (end)
  "Font-lock matcher for @file:path mentions up to END.
Highlights file path references."
  (and (re-search-forward "@file:\\([^ \t\n]+\\)" end t)
       ;; Check if preceded by whitespace or at beginning
       (or (= (match-beginning 0) (point-min))
           (memq (char-syntax (char-before (match-beginning 0))) '(32 62))
           (not (plist-get (text-properties-at (match-beginning 1)) 'gptel)))))

(defun mevedel--prettify-ref-mentions ()
  "Setup or remove font-lock and completion for @ref and @file mentions.
Should be called when entering or leaving a mode that supports these mentions."
  (let ((id-keyword '((mevedel--fontify-ref-id-keyword
                       0 (let ((id (string-to-number (match-string 1))))
                           (if (mevedel--resolve-ref-by-id id)
                               '(:box (:line-width -1) :inherit success)
                             '(:box (:line-width -1) :inherit shadow)))
                       prepend)))
        (tag-keyword '((mevedel--fontify-ref-tag-keyword
                        0 (let* ((query (match-string 1))
                                 (refs (mevedel--resolve-refs-by-tag-query query)))
                            (if (and refs (> (length refs) 0))
                                '(:box (:line-width -1) :inherit success)
                              '(:box (:line-width -1) :inherit shadow)))
                        prepend)))
        (file-keyword '((mevedel--fontify-file-keyword
                         0 (let ((filepath (match-string 1)))
                             (if (file-exists-p filepath)
                                 '(:box (:line-width -1) :inherit link)
                               '(:box (:line-width -1) :inherit shadow)))
                         prepend))))

    (cond
     ;; Enable when gptel-mode is active
     ((bound-and-true-p gptel-mode)
      (font-lock-add-keywords nil id-keyword t)
      (font-lock-add-keywords nil tag-keyword t)
      (font-lock-add-keywords nil file-keyword t)
      (add-hook 'completion-at-point-functions #'mevedel-ref-capf nil t)
      (add-hook 'completion-at-point-functions #'mevedel-file-capf nil t))
     ;; Disable otherwise
     (t
      (font-lock-remove-keywords nil id-keyword)
      (font-lock-remove-keywords nil tag-keyword)
      (font-lock-remove-keywords nil file-keyword)
      (remove-hook 'completion-at-point-functions #'mevedel-ref-capf t)
      (remove-hook 'completion-at-point-functions #'mevedel-file-capf t)))))

(provide 'mevedel-mentions)
;;; mevedel-mentions.el ends here
