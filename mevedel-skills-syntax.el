;;; mevedel-skills-syntax.el --- Shared authored skill syntax -*- lexical-binding: t; -*-

;;; Commentary:

;; Parses authored Markdown regions and recursive required-skill declarations.
;; It is dependency-free so discovery, input scanning, and body preparation can
;; share the syntax without depending on one another.

;;; Code:

(require 'cl-lib)

(defconst mevedel-skills-syntax--dependency-name-regexp
  "\\(?:[A-Za-z0-9_.-]+:[a-z0-9-]+\\|[a-z0-9-]+\\)"
  "Regexp matching an authored required-skill name.")

(defun mevedel-skills-syntax--author-ranges-p (text &rest ranges)
  "Return non-nil when every range in RANGES is author-written in TEXT."
  (let ((author-p t))
    (while (and ranges author-p)
      (let ((start (pop ranges))
            (end (pop ranges)))
        (when (text-property-not-all
               start end 'mevedel-skills-non-author-text nil text)
          (setq author-p nil))))
    author-p))

(defun mevedel-skills-syntax--ranges-overlap-p (ranges start end)
  "Return non-nil when any range in RANGES overlaps START to END."
  (cl-some (lambda (range)
             (and (< start (cdr range)) (< (car range) end)))
           ranges))

(defun mevedel-skills-syntax--injection-fence-opener-p (line marker)
  "Return non-nil when LINE and MARKER open a body-injection fence."
  (and (string= marker "```")
       (or (string= line "```!")
           (string-match-p "\\````!el[ \t]*\\'" line))))

(defun mevedel-skills-syntax--authored-fence-close-end (text close-re start)
  "Return end of the next CLOSE-RE match in TEXT after START."
  (let ((search start)
        close-end)
    (while (and (not close-end) (string-match close-re text search))
      (if (mevedel-skills-syntax--author-ranges-p
           text (match-beginning 0) (match-end 0))
          (setq close-end (match-end 0))
        (setq search (match-end 0))))
    close-end))

(defun mevedel-skills-syntax--code-fence-ranges
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
             (close-end (mevedel-skills-syntax--authored-fence-close-end
                         text close-re body-start)))
        (if (and (not include-injections-p)
                 (mevedel-skills-syntax--injection-fence-opener-p line marker))
            (setq pos (or close-end len))
          (if close-end
              (progn
                (push (cons line-start close-end) ranges)
                (setq pos close-end))
            (push (cons line-start len) ranges)
            (setq pos len)))))
    (nreverse ranges)))

(defun mevedel-skills-syntax--injection-inline-marker-start (text position)
  "Return the injection marker start before TEXT's backtick at POSITION."
  (cond
   ((and (> position 0) (= (aref text (1- position)) ?!))
    (1- position))
   ((and (>= position 3)
         (string= (substring text (- position 3) position) "!el"))
    (- position 3))))

(defun mevedel-skills-syntax--injection-inline-span-end
    (text position line-end)
  "Return end of authored inline injection at POSITION, or nil."
  (when-let* ((marker-start
               (mevedel-skills-syntax--injection-inline-marker-start
                text position))
              ((mevedel-skills-syntax--author-ranges-p
                text marker-start (1+ position))))
    (let ((search (1+ position))
          span-end)
      (while (and (not span-end)
                  (string-match "`" text search)
                  (<= (match-end 0) line-end))
        (let ((close-start (match-beginning 0))
              (close-end (match-end 0)))
          (if (mevedel-skills-syntax--author-ranges-p
               text close-start close-end)
              (setq span-end close-end)
            (setq search close-end))))
      span-end)))

(defun mevedel-skills-syntax--inline-code-ranges
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
                  (mevedel-skills-syntax--ranges-overlap-p
                   fence-ranges run-start run-end))
              (setq pos run-end))
             ((and (not include-injections-p)
                   (if-let* ((injection-end
                              (mevedel-skills-syntax--injection-inline-span-end
                               text run-start line-end)))
                       (setq pos injection-end))))
             ((and (string-match (regexp-quote run) text run-end)
                   (<= (match-end 0) line-end))
              (push (cons run-start (match-end 0)) ranges)
              (setq pos (match-end 0)))
             (t (setq pos run-end)))))
        (setq line-start (if (< line-end len) (1+ line-end) len))))
    (nreverse ranges)))

(defun mevedel-skills-syntax-markdown-code-ranges
    (text &optional include-injections-p)
  "Return Markdown code ranges in TEXT.
When INCLUDE-INJECTIONS-P is non-nil, include body-injection syntax too."
  (let* ((fence-ranges
          (mevedel-skills-syntax--code-fence-ranges
           text include-injections-p))
         (inline-ranges
          (mevedel-skills-syntax--inline-code-ranges
           text fence-ranges include-injections-p)))
    (sort (append fence-ranges inline-ranges)
          (lambda (a b) (< (car a) (car b))))))

(defun mevedel-skills-syntax--dependency-name-char-p (ch)
  "Return non-nil when CH could continue a required-skill name."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (memq ch '(?- ?: ?_)))))

(defun mevedel-skills-syntax--dependency-escaped-p (text position)
  "Return non-nil when authored backslashes escape TEXT at POSITION."
  (let ((pos (1- position))
        (count 0))
    (while (and (>= pos 0)
                (eq (aref text pos) ?\\)
                (mevedel-skills-syntax--author-ranges-p
                 text pos (1+ pos)))
      (cl-incf count)
      (cl-decf pos))
    (not (zerop (% count 2)))))

(defun mevedel-skills-syntax-parse-dependencies (text)
  "Parse and replace authored required-skill declarations in TEXT.

Return a plist with `:body' and source-ordered `:dependencies'.  Each
dependency records `:name', `:argument-template', `:start', and `:end'.
Escaped declarations, Markdown code, injection code, and non-author text
are inert."
  (let ((regexp (concat "!\\$\\("
                        mevedel-skills-syntax--dependency-name-regexp
                        "\\)"))
        (code-ranges (mevedel-skills-syntax-markdown-code-ranges text t))
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
        (when (and (not (mevedel-skills-syntax--dependency-name-char-p next))
                   (mevedel-skills-syntax--author-ranges-p
                    text start marker-end)
                   (not (mevedel-skills-syntax--dependency-escaped-p
                         text start))
                   (not (mevedel-skills-syntax--ranges-overlap-p
                         code-ranges start marker-end)))
          (when (and (string-match-p "\\`[ \t]*\\'" prefix)
                     (string-match "\\`[ \t]+--[ \t]*" suffix)
                     (mevedel-skills-syntax--author-ranges-p
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

(provide 'mevedel-skills-syntax)
;;; mevedel-skills-syntax.el ends here
