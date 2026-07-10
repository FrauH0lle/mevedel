;;; mevedel-view-markdown.el --- Markdown projection -*- lexical-binding: t -*-

;;; Commentary:

;; Owns Markdown tables, links, local images, paths, and fenced source panels
;; in mevedel views.

;;; Code:

;; `mevedel-view'
(declare-function mevedel-view--fontify-as "mevedel-view" (text mode))
(declare-function mevedel-view--line-ref-list-start-line
                  "mevedel-view" (text))
(declare-function mevedel-view--line-ref-start-line "mevedel-view" (text))
(declare-function mevedel-view--make-file-button
                  "mevedel-view" (start end path line))
(declare-function mevedel-view--normalize-local-file-uri-path
                  "mevedel-view" (path))
(declare-function mevedel-view--open-url-action "mevedel-view" (button))
(declare-function mevedel-view--path-candidate-p "mevedel-view" (text))
(declare-function mevedel-view--path-context-candidate-p
                  "mevedel-view" (start raw))
(declare-function mevedel-view--resolve-path "mevedel-view" (raw))
(defvar mevedel-view--direct-line-ref-list-regexp)
(defvar mevedel-view--line-ref-regexp)
(defvar mevedel-view--line-ref-suffix-regexp)
(defvar mevedel-view--link-action-properties)
(defvar mevedel-view--linkify-path-regexp)
(defvar mevedel-view-inline-image-max-width)

(defun mevedel-view--markdown-code-blocks (start end)
  "Return fenced Markdown code blocks between START and END."
  (let (blocks
        (case-fold-search nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^[ \t]*```[^`\n]*\n" end t)
        (let ((fence-start (match-beginning 0))
              (fence-end (match-end 0))
              (body-start (point)))
          (if (re-search-forward "^[ \t]*```[ \t]*$" end t)
              (push (list :fence-start fence-start
                          :fence-end fence-end
                          :body-start body-start
                          :body-end (match-beginning 0)
                          :end-fence-start (match-beginning 0)
                          :end-fence-end (match-end 0))
                    blocks)
            (goto-char end)))))
    (nreverse blocks)))

(defun mevedel-view--src-block-body-ranges (start end)
  "Return code block body ranges between START and END."
  (let (ranges
        (case-fold-search t))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^[ 	]*#\\+begin_src\\b.*\n" end t)
        (let ((body-start (point)))
          (if (re-search-forward "^[ 	]*#\\+end_src\\b.*$" end t)
              (when (< body-start (match-beginning 0))
                (push (cons body-start (match-beginning 0)) ranges))
            (goto-char end)))))
    (dolist (block (mevedel-view--markdown-code-blocks start end))
      (when (< (plist-get block :body-start)
               (plist-get block :body-end))
        (push (cons (plist-get block :body-start)
                    (plist-get block :body-end))
              ranges)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((next (or (next-single-property-change
                         (point) 'mevedel-view-code-block-body nil end)
                        end)))
          (when (get-text-property (point) 'mevedel-view-code-block-body)
            (push (cons (point) next) ranges))
          (goto-char next))))
    (nreverse ranges)))

(defun mevedel-view--position-in-ranges-p (position ranges)
  "Return non-nil when POSITION is inside one of RANGES."
  (let (found)
    (while (and ranges (not found))
      (let ((range (car ranges)))
        (when (and (<= (car range) position)
                   (< position (cdr range)))
          (setq found t)))
      (setq ranges (cdr ranges)))
    found))

(defconst mevedel-view--markdown-table-line-regexp
  "^[ \t]*|.*|[ \t]*$"
  "Regexp matching one simple Markdown pipe table line.")

(defun mevedel-view--markdown-table-row-cells (start end)
  "Return Markdown table cells between START and END.
Pipes inside simple backtick code spans or after backslash escapes are
not treated as delimiters."
  (let (cells)
    (save-excursion
      (goto-char start)
      (skip-chars-forward " \t" end)
      (when (and (< (point) end) (eq (char-after) ?|))
        (forward-char 1)
        (let ((cell-start (point)))
          (while (< (point) end)
            (let ((ch (char-after)))
              (cond
               ((eq ch ?\\)
                (forward-char 1)
                (when (< (point) end)
                  (forward-char 1)))
               ((eq ch ?`)
                (let ((ticks-start (point)))
                  (skip-chars-forward "`" end)
                  (let* ((ticks (buffer-substring-no-properties
                                 ticks-start (point)))
                         (close (save-excursion
                                  (search-forward ticks end t))))
                    (when close
                      (goto-char close)))))
               ((eq ch ?|)
                (push (list :start cell-start
                            :end (point)
                            :content (buffer-substring-no-properties
                                      cell-start (point)))
                      cells)
                (forward-char 1)
                (setq cell-start (point)))
               (t
                (forward-char 1))))))))
    (nreverse cells)))

(defun mevedel-view--markdown-table-separator-row-p (cells)
  "Return non-nil when CELLS are a Markdown table separator row."
  (let ((ok cells))
    (dolist (cell cells ok)
      (let ((content (string-trim (plist-get cell :content))))
        (unless (and (string-match-p "-" content)
                     (string-match-p "\\`[:-]+\\'" content))
          (setq ok nil))))))

(defun mevedel-view--markdown-table-unmatched-backtick-p (text)
  "Return non-nil when TEXT contains an unmatched backtick run."
  (let ((pos 0)
        unmatched)
    (while (and (not unmatched)
                (string-match "`+" text pos))
      (let* ((ticks (match-string 0 text))
             (after (match-end 0))
             (close (string-search ticks text after)))
        (if close
            (setq pos (+ close (length ticks)))
          (setq unmatched t))))
    unmatched))

(defun mevedel-view--markdown-table-visible-width (text)
  "Return a cheap visible width estimate for Markdown table cell TEXT."
  (let ((text (string-trim (or text ""))))
    (setq text
          (replace-regexp-in-string
           "\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))" "\\1" text))
    (setq text
          (replace-regexp-in-string
           "\\(?:\\*\\*\\|__\\)" "" text))
    (string-width text)))

(defun mevedel-view--markdown-table-normalize-cell-face (cell)
  "Render malformed Markdown table CELL as literal table text."
  (when (mevedel-view--markdown-table-unmatched-backtick-p
         (plist-get cell :content))
    (let ((start (plist-get cell :start))
          (end (plist-get cell :end)))
      (remove-text-properties
       start end
       '(face nil font-lock-face nil display nil invisible nil composition nil))
      (put-text-property start end 'font-lock-face 'markdown-table-face))))

(defun mevedel-view--markdown-table-valid-p (rows)
  "Return non-nil when ROWS form one simple Markdown table."
  (let ((count (and rows (length (plist-get (car rows) :cells))))
        (ok (and (>= (length rows) 2)
                 (not (plist-get (car rows) :separator))
                 (plist-get (nth 1 rows) :separator))))
    (dolist (row rows ok)
      (unless (= count (length (plist-get row :cells)))
        (setq ok nil)))))

(defun mevedel-view--markdown-table-collect (start end)
  "Return simple Markdown tables between START and END."
  (let ((code-ranges (mevedel-view--src-block-body-ranges start end))
        tables)
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (< (point) end)
        (let ((line-start (line-beginning-position)))
          (if (or (mevedel-view--position-in-ranges-p line-start code-ranges)
                  (not (looking-at mevedel-view--markdown-table-line-regexp)))
              (forward-line 1)
            (let ((table-start line-start)
                  rows)
              (while (and (< (point) end)
                          (not (mevedel-view--position-in-ranges-p
                                (line-beginning-position) code-ranges))
                          (looking-at mevedel-view--markdown-table-line-regexp))
                (let* ((row-start (line-beginning-position))
                       (row-end (line-end-position))
                       (cells (mevedel-view--markdown-table-row-cells
                               row-start row-end)))
                  (push (list :start row-start
                              :end row-end
                              :cells cells
                              :separator
                              (mevedel-view--markdown-table-separator-row-p
                               cells))
                        rows))
                (forward-line 1))
              (setq rows (nreverse rows))
              (when (mevedel-view--markdown-table-valid-p rows)
                (push (list :start table-start
                            :end (plist-get (car (last rows)) :end)
                            :rows rows)
                      tables)))))))
    (nreverse tables)))

(defun mevedel-view--markdown-table-widths (rows)
  "Return visible column widths for Markdown table ROWS."
  (let* ((count (length (plist-get (car rows) :cells)))
         (widths (make-vector count 0)))
    (dolist (row rows)
      (unless (plist-get row :separator)
        (let ((i 0))
          (dolist (cell (plist-get row :cells))
            (aset widths i
                  (max (aref widths i)
                       (mevedel-view--markdown-table-visible-width
                        (plist-get cell :content))))
            (setq i (1+ i))))))
    (append widths nil)))

(defconst mevedel-view--markdown-table-pad-properties
  '(face
    font-lock-face
    mevedel-view-source
    mevedel-view-source-key
    mevedel-view-type
    mevedel-view-collapsed
    mevedel-view-turn-id
    read-only
    keymap
    front-sticky
    rear-nonsticky)
  "Text properties copied onto inserted Markdown table padding.")

(defun mevedel-view--selected-text-properties (position properties)
  "Return plist of PROPERTIES present at POSITION."
  (let (props)
    (dolist (prop properties)
      (let ((value (get-text-property position prop)))
        (when value
          (setq props (plist-put props prop value)))))
    props))

(defun mevedel-view--markdown-table-pad-string (text position)
  "Return TEXT with structural properties copied from POSITION."
  (let ((props (mevedel-view--selected-text-properties
                position
                mevedel-view--markdown-table-pad-properties)))
    (if props
        (apply #'propertize text props)
      text)))

(defun mevedel-view--prettify-markdown-table (table)
  "Pad one Markdown TABLE so columns line up in the view."
  (let* ((rows (plist-get table :rows))
         (widths (mevedel-view--markdown-table-widths rows)))
    (dolist (row (reverse rows))
      (let ((separator (plist-get row :separator))
            indexed
            (i 0))
        (dolist (cell (plist-get row :cells))
          (push (cons i cell) indexed)
          (setq i (1+ i)))
        (dolist (entry indexed)
          (let* ((index (car entry))
                 (cell (cdr entry))
                 (content (plist-get cell :content))
                 (target (if separator
                             (+ 2 (nth index widths))
                           (nth index widths)))
                 (width (if separator
                            (string-width (string-trim content))
                          (mevedel-view--markdown-table-visible-width
                           content)))
                 (pad (- target width)))
            (mevedel-view--markdown-table-normalize-cell-face cell)
            (when (> pad 0)
              (goto-char (plist-get cell :end))
              (insert
               (mevedel-view--markdown-table-pad-string
                (make-string pad (if separator ?- ?\s))
                (if (> (point) (point-min)) (1- (point)) (point)))))))))))

(defun mevedel-view--prettify-markdown-tables-in-range (start end)
  "Pad Markdown pipe tables between START and END."
  (save-excursion
    (dolist (table (reverse (mevedel-view--markdown-table-collect start end)))
      (mevedel-view--prettify-markdown-table table))))

(defconst mevedel-view--image-extensions
  '("png" "jpg" "jpeg" "gif" "webp")
  "Image filename extensions rendered inline in the view.")

(defun mevedel-view--image-file-p (path)
  "Return non-nil when PATH names a supported local image file."
  (and (stringp path)
       (file-exists-p path)
       (member (downcase (or (file-name-extension path) ""))
               mevedel-view--image-extensions)))

(defun mevedel-view--local-link-target (url)
  "Resolve URL or path string to an existing local file path."
  (when (and (stringp url)
             (not (string-empty-p url))
             (not (string-match-p "\\`https?://" url)))
    (let* ((without-fragment
            (replace-regexp-in-string
             (concat mevedel-view--line-ref-suffix-regexp "\\'")
             "" url))
           (raw (if (string-prefix-p "file://" without-fragment)
                    (mevedel-view--normalize-local-file-uri-path
                     (substring without-fragment 7))
                  without-fragment))
           (resolved (mevedel-view--resolve-path raw)))
      (and resolved (file-exists-p resolved) resolved))))

(defun mevedel-view--local-link-line (url)
  "Return URL's trailing #L line number, or nil."
  (when (and (stringp url)
             (string-match
              (concat mevedel-view--line-ref-suffix-regexp "\\'")
              url))
    (mevedel-view--line-ref-list-start-line
     (or (match-string-no-properties 1 url)
         (match-string-no-properties 2 url)))))

(defun mevedel-view--image-display (path)
  "Return an image display spec for PATH, or nil."
  (when (and (display-images-p)
             (mevedel-view--image-file-p path))
    (condition-case nil
        (create-image path nil nil :max-width mevedel-view-inline-image-max-width)
      (error nil))))

(defun mevedel-view--put-image-display (start end path)
  "Display PATH as an image over START..END when possible."
  (when-let* ((image (mevedel-view--image-display path)))
    (add-text-properties
     start end
     `(display ,image
       help-echo ,(format "Image: %s" path)
       rear-nonsticky (display help-echo)))))

(defun mevedel-view--decorate-local-images-in-range (start end)
  "Render local Markdown image links and bare image paths between START and END."
  (let ((code-ranges (mevedel-view--src-block-body-ranges start end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "!\\[[^]\n]*\\](\\([^)]+\\))" end t)
        (let* ((mb (match-beginning 0))
               (me (match-end 0))
               (url (match-string-no-properties 1))
               (path (and (not (mevedel-view--position-in-ranges-p
                                mb code-ranges))
                          (mevedel-view--local-link-target url))))
          (when (and path (mevedel-view--image-file-p path))
            (mevedel-view--put-image-display mb me path)))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward mevedel-view--linkify-path-regexp end t)
        (let* ((mb (match-beginning 0))
               (me (match-end 0))
               (raw (match-string-no-properties 0))
               (path (and (not (get-text-property mb 'display))
                          (not (mevedel-view--position-in-ranges-p
                                mb code-ranges))
                          (mevedel-view--path-candidate-p raw)
                          (mevedel-view--path-context-candidate-p mb raw)
                          (mevedel-view--resolve-path raw))))
          (when (and path (mevedel-view--image-file-p path))
            (mevedel-view--put-image-display mb me path)))))))

(defconst mevedel-view--file-mention-regexp
  (concat "@file:\\({\\(?:\\\\.\\|[^}]\\)+}\\|[^ \t\n#]+\\)"
          "\\(" mevedel-view--direct-line-ref-list-regexp "\\)?")
  "Regexp matching rendered `@file' mentions.")

(defun mevedel-view--unescape-braced-file-path (token)
  "Return TOKEN decoded as a braced file path."
  (with-temp-buffer
    (let ((i 0))
      (while (< i (length token))
        (let ((ch (aref token i)))
          (if (and (= ch ?\\)
                   (< (1+ i) (length token)))
              (progn
                (cl-incf i)
                (insert-char (aref token i)))
            (insert-char ch)))
        (cl-incf i)))
    (buffer-string)))

(defun mevedel-view--file-mention-token-path (token)
  "Return the file path encoded by @file TOKEN."
  (if (and (>= (length token) 2)
           (= (aref token 0) ?{)
           (= (aref token (1- (length token))) ?}))
      (mevedel-view--unescape-braced-file-path
       (substring token 1 -1))
    token))

(defun mevedel-view--linkify-file-mentions-in-range (start end)
  "Turn rendered `@file' mentions into file buttons between START and END."
  (let (ranges)
    (save-excursion
      (goto-char start)
      (while (re-search-forward mevedel-view--file-mention-regexp end t)
        (let* ((mb (match-beginning 0))
               (me (match-end 0))
               (raw (mevedel-view--file-mention-token-path
                     (match-string-no-properties 1)))
               (line (and (match-beginning 2)
                          (mevedel-view--line-ref-list-start-line
                           (match-string-no-properties 2))))
               (resolved (mevedel-view--resolve-path raw)))
          (push (cons mb me) ranges)
          (when (and resolved (file-exists-p resolved))
            (mevedel-view--make-file-button mb me resolved line)))))
    (nreverse ranges)))

(defun mevedel-view--linkify-markdown-file-links-in-range (start end)
  "Turn local Markdown links into file buttons between START and END."
  (let (ranges)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\[\\([^]\n]+\\)\\](\\([^)]+\\))" end t)
        (let* ((mb (match-beginning 1))
               (me (match-end 1))
               (whole-start (match-beginning 0))
               (whole-end (match-end 0))
               (url (match-string-no-properties 2))
               (path (mevedel-view--local-link-target url))
               (line (mevedel-view--local-link-line url)))
          (push (cons whole-start whole-end) ranges)
          (when path
            (mevedel-view--make-file-button mb me path line)))))
    (nreverse ranges)))

(defun mevedel-view--render-markdown-url-links-in-range (start end)
  "Render Markdown URL links between START and END as clickable labels."
  (let ((src-ranges (mevedel-view--src-block-body-ranges start end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\[\\([^]\n]+\\)\\](\\(https?://[^)\n]+\\))"
                                end t)
        (let* ((whole-start (match-beginning 0))
               (whole-end (match-end 0))
               (title (buffer-substring (match-beginning 1) (match-end 1)))
               (url (match-string-no-properties 2)))
          (unless (or (and (> whole-start (point-min))
                           (eq (char-before whole-start) ?!))
                      (mevedel-view--position-in-ranges-p
                       whole-start src-ranges))
            (remove-text-properties
             0 (length title) mevedel-view--link-action-properties title)
            (delete-region whole-start whole-end)
            (goto-char whole-start)
            (insert title)
            (make-text-button
             whole-start (point)
             'action #'mevedel-view--open-url-action
             'mevedel-view-url url
             'follow-link t
             'face 'link
             'mouse-face 'highlight
             'help-echo (format "Visit %s" url))))))))

(defun mevedel-view--linkify-path-reference
    (path-start path-end suffix-start suffix-end path)
  "Create file buttons for PATH reference at PATH-START..PATH-END.
When SUFFIX-START and SUFFIX-END delimit a line-reference list, create
one button per reference.  The first button includes the path text."
  (if (not (and suffix-start suffix-end (< suffix-start suffix-end)))
      (mevedel-view--make-file-button path-start path-end path nil)
    (let ((first t))
      (save-excursion
        (goto-char suffix-start)
        (while (re-search-forward mevedel-view--line-ref-regexp suffix-end t)
          (mevedel-view--make-file-button
           (if first path-start (match-beginning 0))
           (match-end 0)
           path
           (mevedel-view--line-ref-start-line
            (match-string-no-properties 0)))
          (setq first nil))))))

(defun mevedel-view--linkify-paths-in-range (start end)
  "Scan the buffer between START and END and turn paths into text buttons.
Clickable targets are resolved to absolute paths via
`mevedel-view--resolve-path' and gated on `file-exists-p' -- paths that
don't resolve to an existing file stay as plain text.  References may
include line suffixes, such as file.el:12, file.el:L12-L20, and
file.el#L12."
  (let ((regexp (concat "\\(" mevedel-view--linkify-path-regexp "\\)"
                        "\\(?:" mevedel-view--line-ref-suffix-regexp "\\)?"))
        (src-ranges (mevedel-view--src-block-body-ranges start end)))
    (setq src-ranges
          (append (mevedel-view--linkify-file-mentions-in-range start end)
                  (mevedel-view--linkify-markdown-file-links-in-range start end)
                  src-ranges))
    (save-excursion
      (goto-char start)
      (while (re-search-forward regexp end t)
        (let* ((mb (match-beginning 1))
               (me (match-end 0))
               (raw (buffer-substring-no-properties mb (match-end 1)))
               (suffix-start (or (match-beginning 2) (match-beginning 3)))
               (suffix-end (or (match-end 2) (match-end 3)))
               (resolved (and (not (mevedel-view--position-in-ranges-p
                                    mb src-ranges))
                              (mevedel-view--path-candidate-p raw)
                              (mevedel-view--path-context-candidate-p mb raw)
                              (mevedel-view--resolve-path raw))))
          (when (and resolved (file-exists-p resolved))
            (mevedel-view--linkify-path-reference
             mb me suffix-start suffix-end resolved)))))))

(defun mevedel-view--decorate-markdown-in-range (start end)
  "Apply Markdown view affordances between START and END."
  (let ((end-marker (copy-marker end t)))
    (unwind-protect
        (progn
          (mevedel-view--decorate-code-blocks-in-range start end-marker)
          (mevedel-view--prettify-markdown-tables-in-range start end-marker)
          (mevedel-view--decorate-local-images-in-range start end-marker)
          (mevedel-view--render-markdown-url-links-in-range start end-marker)
          (mevedel-view--linkify-paths-in-range start end-marker))
      (set-marker end-marker nil))))

(defun mevedel-view--copy-code-block-button-action (button)
  "Copy BUTTON's fenced code block body."
  (let ((start (button-get button 'mevedel-view-copy-start))
        (end (button-get button 'mevedel-view-copy-end)))
    (when (and start end (<= start end))
      (kill-new (buffer-substring-no-properties start end))
      (message "Copied"))))

(defconst mevedel-view--source-block-carried-properties
  '(mevedel-view-source
    mevedel-view-source-key
    mevedel-view-type
    mevedel-view-collapsed
    mevedel-view-turn-id
    read-only
    keymap
    front-sticky
    rear-nonsticky)
  "Text properties copied onto inserted Markdown source panel text.")

(defconst mevedel-view--source-block-mode-alist
  '(("bash" . sh-mode)
    ("c" . c-mode)
    ("c++" . c++-mode)
    ("cpp" . c++-mode)
    ("elisp" . emacs-lisp-mode)
    ("emacs-lisp" . emacs-lisp-mode)
    ("javascript" . js-mode)
    ("js" . js-mode)
    ("lisp" . lisp-mode)
    ("python" . python-mode)
    ("sh" . sh-mode)
    ("shell" . sh-mode))
  "Best-effort major modes for common Markdown source block languages.")

(defun mevedel-view--markdown-code-block-language (block)
  "Return BLOCK's fenced language string, or nil."
  (let ((fence (buffer-substring-no-properties
                (plist-get block :fence-start)
                (plist-get block :fence-end))))
    (when (string-match "\\`[ \t]*```[ \t]*\\([^ \t\n`]*\\)" fence)
      (let ((lang (string-trim (match-string 1 fence))))
        (unless (string-empty-p lang)
          lang)))))

(defun mevedel-view--source-block-mode (language)
  "Return a major mode for source block LANGUAGE, or nil."
  (when-let* ((language (and (stringp language)
                             (downcase language))))
    (let ((mode (or (cdr (assoc language
                                mevedel-view--source-block-mode-alist))
                    (intern-soft (concat language "-mode")))))
      (and (fboundp mode) mode))))

(defun mevedel-view--source-block-font-lock-face (face)
  "Return FACE combined with the source block panel face."
  (cond
   ((null face) 'mevedel-view-source-block)
   ((listp face) (append face '(mevedel-view-source-block)))
   (t (list face 'mevedel-view-source-block))))

(defun mevedel-view--fontify-source-block-body (start end language)
  "Apply best-effort LANGUAGE fontification to source body START..END."
  (let* ((mode (mevedel-view--source-block-mode language))
         (text (buffer-substring-no-properties start end))
         (fontified (and mode (mevedel-view--fontify-as text mode)))
         (limit (and fontified
                     (min (length fontified) (- end start)))))
    (put-text-property start end 'font-lock-face 'mevedel-view-source-block)
    (when fontified
      (let ((pos 0))
        (while (< pos limit)
          (let* ((next (or (next-single-property-change
                            pos 'font-lock-face fontified limit)
                           limit))
                 (face (get-text-property pos 'font-lock-face fontified)))
            (when face
              (put-text-property
               (+ start pos) (+ start next)
               'font-lock-face
               (mevedel-view--source-block-font-lock-face face)))
            (setq pos next)))))))

(defun mevedel-view--add-source-block-line-properties (start end)
  "Add source panel line wrapping properties to START..END."
  (let ((prefix (propertize "    " 'font-lock-face
                            'mevedel-view-source-block)))
    (add-text-properties
     start end
     `(line-prefix ,prefix
       wrap-prefix ,prefix
       rear-nonsticky (line-prefix wrap-prefix)))))

(defun mevedel-view--decorate-code-blocks-in-range (start end)
  "Render fenced Markdown code blocks as source panels in START..END."
  (let ((inhibit-read-only t))
    (dolist (block (reverse (mevedel-view--markdown-code-blocks start end)))
      (let* ((fence-start (plist-get block :fence-start))
             (fence-end (plist-get block :fence-end))
             (body-start (copy-marker (plist-get block :body-start) t))
             (body-end (copy-marker (plist-get block :body-end) t))
             (content-end (copy-marker
                           (if (and (< (plist-get block :body-start)
                                       (plist-get block :body-end))
                                    (eq (char-before
                                         (plist-get block :body-end))
                                        ?\n))
                               (1- (plist-get block :body-end))
                             (plist-get block :body-end))
                           t))
             (end-fence-start (plist-get block :end-fence-start))
             (end-fence-end (plist-get block :end-fence-end))
             (end-fence-delete-end
              (if (and (< end-fence-end (point-max))
                       (eq (char-after end-fence-end) ?\n))
                  (1+ end-fence-end)
                end-fence-end))
             (language (mevedel-view--markdown-code-block-language block))
             (label (concat (or language "snippet") " ⧉"))
             (carried
              (mevedel-view--selected-text-properties
               fence-start
               mevedel-view--source-block-carried-properties)))
        (delete-region end-fence-start end-fence-delete-end)
        (delete-region fence-start fence-end)
        (let* ((panel-start (marker-position body-start))
               (panel-padding-line (propertize "\n"
                                                'font-lock-face
                                                'mevedel-view-source-block))
               (header (concat label
                               panel-padding-line
                               panel-padding-line)))
          (goto-char body-start)
          (insert header)
          (when carried
            (add-text-properties panel-start (marker-position body-start)
                                 carried))
          (mevedel-view--add-source-block-line-properties
           panel-start (marker-position body-start))
          (goto-char panel-start)
          (make-text-button
           (point) (+ (point) (length label))
           'action #'mevedel-view--copy-code-block-button-action
           'follow-link t
           'help-echo "Copy code block"
           'mevedel-view-copy-start (marker-position body-start)
           'mevedel-view-copy-end (marker-position content-end)
           'font-lock-face 'mevedel-view-source-block-language
           'mouse-face 'highlight
           'pointer 'hand)
          (when (< (marker-position body-start)
                   (marker-position body-end))
            (mevedel-view--fontify-source-block-body
             (marker-position body-start)
             (marker-position body-end)
             language)
            (mevedel-view--add-source-block-line-properties
             (marker-position body-start)
             (marker-position body-end)))
          (when (< (marker-position body-start)
                   (marker-position content-end))
            (put-text-property (marker-position body-start)
                               (marker-position content-end)
                               'mevedel-view-code-block-body t))
          (goto-char body-end)
          (let ((pad-start (point)))
            (insert panel-padding-line)
            (when carried
              (add-text-properties pad-start (point) carried))
            (mevedel-view--add-source-block-line-properties
             pad-start (point))))
        (set-marker body-start nil)
        (set-marker body-end nil)
        (set-marker content-end nil)))))

(provide 'mevedel-view-markdown)

;;; mevedel-view-markdown.el ends here
