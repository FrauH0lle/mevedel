;;; niwa-skills-syntax.el --- Shared authored skill syntax -*- lexical-binding: t; -*-

;;; Commentary:

;; Parses authored Markdown regions and recursive required-skill declarations.
;; It is dependency-free so discovery, input scanning, and body preparation can
;; share the syntax without depending on one another.

;;; Code:

(require 'cl-lib)

(defconst niwa-skills-syntax--dependency-name-regexp
  "\\(?:[A-Za-z0-9_.-]+:[a-z0-9-]+\\|[a-z0-9-]+\\)"
  "Regexp matching an authored required-skill name.")

(defun niwa-skills-syntax--author-ranges-p (text &rest ranges)
  "Return non-nil when every range in RANGES is author-written in TEXT."
  (let ((author-p t))
    (while (and ranges author-p)
      (let ((start (pop ranges))
            (end (pop ranges)))
        (when (text-property-not-all
               start end 'niwa-skills-non-author-text nil text)
          (setq author-p nil))))
    author-p))

(defun niwa-skills-syntax--ranges-overlap-p (ranges start end)
  "Return non-nil when any range in RANGES overlaps START to END."
  (cl-some (lambda (range)
             (and (< start (cdr range)) (< (car range) end)))
           ranges))

(defun niwa-skills-syntax--injection-fence-opener-p (line marker)
  "Return non-nil when LINE and MARKER open a body-injection fence."
  (and (string= marker "```")
       (or (string= line "```!")
           (string-match-p "\\````!el[ \t]*\\'" line))))

(defun niwa-skills-syntax--authored-fence-close-end (text close-re start)
  "Return end of the next CLOSE-RE match in TEXT after START."
  (let ((search start)
        close-end)
    (while (and (not close-end) (string-match close-re text search))
      (if (niwa-skills-syntax--author-ranges-p
           text (match-beginning 0) (match-end 0))
          (setq close-end (match-end 0))
        (setq search (match-end 0))))
    close-end))

(defun niwa-skills-syntax--code-fence-ranges
    (text &optional include-injections-p)
  "Return ordinary Markdown code-fence ranges in TEXT.
When INCLUDE-INJECTIONS-P is non-nil, include body-injection fences."
  (let ((ranges nil)
        (pos 0)
        (len (length text)))
    (while (and (< pos len)
                (string-match "\\(^\\|\n\\)\\(```+\\)[^\n]*\\(\n\\|\\'\\)"
                              text pos))
      (let* ((line-start (+ (match-beginning 0)
                            (length (match-string 1 text))))
             (marker (match-string 2 text))
             (line-end (if (string= (match-string 3 text) "\n")
                           (1- (match-end 0))
                         (match-end 0)))
             (line (substring text line-start line-end))
             (body-start (match-end 0))
             (close-re (concat "\\(^\\|\n\\)"
                               (regexp-quote marker)
                               "\\(\n\\|\\'\\)"))
             (end (or (niwa-skills-syntax--authored-fence-close-end
                       text close-re body-start)
                      len)))
        (unless (and (not include-injections-p)
                     (niwa-skills-syntax--injection-fence-opener-p line marker))
          (push (cons line-start end) ranges))
        (setq pos end)))
    (nreverse ranges)))

(defun niwa-skills-syntax--injection-inline-marker-start (text position)
  "Return the injection marker start before TEXT's backtick at POSITION."
  (cond
   ((and (> position 0) (= (aref text (1- position)) ?!))
    (1- position))
   ((and (>= position 3)
         (string= (substring text (- position 3) position) "!el"))
    (- position 3))))

(defun niwa-skills-syntax--injection-inline-span-end
    (text position line-end)
  "Return end of authored inline injection at POSITION, or nil."
  (when-let* ((marker-start
               (niwa-skills-syntax--injection-inline-marker-start
                text position))
              ((niwa-skills-syntax--author-ranges-p
                text marker-start (1+ position))))
    (let ((search (1+ position))
          span-end)
      (while (and (not span-end)
                  (string-match "`" text search)
                  (<= (match-end 0) line-end))
        (let ((close-start (match-beginning 0))
              (close-end (match-end 0)))
          (if (niwa-skills-syntax--author-ranges-p
               text close-start close-end)
              (setq span-end close-end)
            (setq search close-end))))
      span-end)))

(defun niwa-skills-syntax--inline-code-ranges
    (text fence-ranges &optional include-injections-p)
  "Return Markdown inline code-span ranges in TEXT outside FENCE-RANGES."
  (let ((ranges nil)
        (line-start 0)
        (len (length text)))
    (while (< line-start len)
      (let* ((line-end (or (string-match "\n" text line-start) len))
             (pos line-start))
        (while (and (< pos line-end) (string-match "`+" text pos))
          (let* ((run-start (match-beginning 0))
                 (run-end (match-end 0))
                 (run (match-string 0 text)))
            (cond
             ((or (>= run-start line-end)
                  (niwa-skills-syntax--ranges-overlap-p
                   fence-ranges run-start run-end))
              (setq pos run-end))
             ((and (not include-injections-p)
                   (if-let* ((injection-end
                              (niwa-skills-syntax--injection-inline-span-end
                               text run-start line-end)))
                       (setq pos injection-end))))
             ((and (string-match (regexp-quote run) text run-end)
                   (<= (match-end 0) line-end))
              (push (cons run-start (match-end 0)) ranges)
              (setq pos (match-end 0)))
             (t (setq pos run-end)))))
        (setq line-start (if (< line-end len) (1+ line-end) len))))
    (nreverse ranges)))

(defun niwa-skills-syntax-markdown-code-ranges
    (text &optional include-injections-p)
  "Return Markdown code ranges in TEXT.
When INCLUDE-INJECTIONS-P is non-nil, include body-injection syntax too."
  (let* ((fence-ranges
          (niwa-skills-syntax--code-fence-ranges
           text include-injections-p))
         (inline-ranges
          (niwa-skills-syntax--inline-code-ranges
           text fence-ranges include-injections-p)))
    (sort (append fence-ranges inline-ranges)
          (lambda (a b) (< (car a) (car b))))))

(defun niwa-skills-syntax--dependency-name-char-p (ch)
  "Return non-nil when CH could continue a required-skill name."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (memq ch '(?- ?: ?_)))))

(defun niwa-skills-syntax--dependency-escaped-p (text position)
  "Return non-nil when authored backslashes escape TEXT at POSITION."
  (let ((pos (1- position))
        (count 0))
    (while (and (>= pos 0)
                (eq (aref text pos) ?\\)
                (niwa-skills-syntax--author-ranges-p
                 text pos (1+ pos)))
      (cl-incf count)
      (cl-decf pos))
    (not (zerop (% count 2)))))

(defun niwa-skills-syntax-parse-dependencies (text)
  "Parse and replace authored required-skill declarations in TEXT.

Return a plist with `:body' and source-ordered `:dependencies'.  Each
dependency records `:name', `:argument-template', `:start', and `:end'.
Escaped declarations, Markdown code, injection code, and non-author text
are inert."
  (let ((regexp (concat "!\\$\\("
                        niwa-skills-syntax--dependency-name-regexp
                        "\\)"))
        (code-ranges (niwa-skills-syntax-markdown-code-ranges text t))
        (position 0)
        dependencies)
    (while (string-match regexp text position)
      (let* ((start (match-beginning 0))
             (marker-end (match-end 0))
             (name (match-string 1 text))
             (next (and (< marker-end (length text))
                        (aref text marker-end)))
             (line-start (1+ (or (cl-position ?\n text
                                             :end start :from-end t)
                                  -1)))
             (line-end (or (cl-position ?\n text :start marker-end)
                           (length text)))
             (prefix (substring text line-start start))
             (suffix (substring text marker-end line-end))
             argument-start end)
        (when (and (not (niwa-skills-syntax--dependency-name-char-p next))
                   (niwa-skills-syntax--author-ranges-p
                    text start marker-end)
                   (not (niwa-skills-syntax--dependency-escaped-p
                         text start))
                   (not (niwa-skills-syntax--ranges-overlap-p
                         code-ranges start marker-end)))
          (when (and (string-match-p "\\`[ \t]*\\'" prefix)
                     (string-match "\\`[ \t]+--[ \t]*" suffix)
                     (niwa-skills-syntax--author-ranges-p
                      text
                      (if (> line-start 0) (1- line-start) line-start)
                      start marker-end (+ marker-end (match-end 0))))
            (setq argument-start (+ marker-end (match-end 0))
                  end line-end))
          (setq end (or end marker-end))
          (push (list :name name
                      :argument-template
                      (and argument-start (substring text argument-start end))
                      :start start :end end)
                dependencies))
        (setq position (max (1+ start) marker-end))))
    (setq dependencies (nreverse dependencies))
    (let ((body text))
      (dolist (dependency (reverse dependencies))
        (let ((start (plist-get dependency :start))
              (end (plist-get dependency :end))
              (name (plist-get dependency :name)))
          (setq body (concat (substring body 0 start)
                             (format "[skill:%s -- attached]" name)
                             (substring body end)))))
      (list :body body :dependencies dependencies))))

(provide 'niwa-skills-syntax)
;;; niwa-skills-syntax.el ends here
