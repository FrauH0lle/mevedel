;;; mevedel-view-markdown.el --- Markdown projection -*- lexical-binding: t -*-

;;; Commentary:

;; Owns Markdown links, local images, paths, fenced source panels, rendered
;; table copying, and window-relative Markdown realignment in mevedel views.

;;; Code:

;; `browse-url'
(declare-function browse-url "browse-url" (url &optional new-window))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-prefix
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(autoload 'mevedel-execution-target-expand-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-prefix "mevedel-execution-target")
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-find-artifact-noselect
                  "mevedel-session-artifacts"
                  (session logical &optional inspection))
(declare-function mevedel-session-artifacts-read-artifact
                  "mevedel-session-artifacts"
                  (session logical &optional committed-only))
(autoload 'mevedel-session-artifacts-find-artifact-noselect
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-artifact
  "mevedel-session-artifacts")

;; `mevedel-session-durability'
(declare-function mevedel-session-publication-logical-path-p
                  "mevedel-session-durability" (path))
(autoload 'mevedel-session-publication-logical-path-p
  "mevedel-session-durability")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-uncommitted-artifact
                  "mevedel-session-publication" (session logical))
(autoload 'mevedel-session-publication-uncommitted-artifact
  "mevedel-session-publication")

;; `mevedel-structs'
(declare-function mevedel-session-execution-target "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-view'
(defvar mevedel-view-inline-image-max-width)

;; `mevedel-view-fontify'
(declare-function mevedel-view--fontify-as "mevedel-view-fontify" (text mode))

;; `mevedel-view-table'
(declare-function mevedel-view-table-decorate "mevedel-view-table"
                  (start end avoid-ranges))
(declare-function mevedel-view-table-rerender "mevedel-view-table"
                  (&optional window))

(require 'mevedel-view-path)
(require 'mevedel-view-table)
(require 'text-property-search)

(defun mevedel-view--normalize-local-file-uri-path (path)
  "Normalize local file URI PATH for Emacs file APIs."
  (let ((path (and path (subst-char-in-string ?\\ ?/ path))))
    (if (and path
             (eq system-type 'windows-nt)
             (string-match "\\`/+\\([A-Za-z]:/.*\\)\\'" path))
        (match-string 1 path)
      path)))

(defvar mevedel-view--linkify-path-regexp
  ;; Absolute paths, relative paths containing a slash, or names with an
  ;; extension.  Keep trailing `-' last in character classes.
  (concat "\\(?:[A-Za-z]:/[[:alnum:]_./+@~-]+"
          "\\|/[[:alnum:]_./+@~-]+"
          "\\|[[:alnum:]_.+@~-]+\\(?:/[[:alnum:]_./+@~-]+\\)+"
          "\\|[[:alnum:]_+-]+\\(?:\\.[[:alnum:]_+-]+\\)+\\)")
  "Regular expression matching candidate file paths in rendered bodies.")

(defun mevedel-view--path-candidate-p (text)
  "Return non-nil when TEXT resembles a real path worth linkifying.
Accepts slash-containing paths and slashless filenames with an
extension.  Guards against matching URLs."
  (and (stringp text)
       (not (string-prefix-p "//" text))
       (not (string-match-p "\\`https?:" text))
       (or (string-search "/" text)
           (string-match-p "\\`[[:alnum:]_+-]+\\(?:\\.[[:alnum:]_+-]+\\)+\\'"
                           text))))

(defun mevedel-view--path-context-candidate-p (start raw)
  "Return non-nil when RAW at START is not part of a larger locator."
  (and (not (eq (char-before start) ?:))
       (or (string-search "/" raw)
           (not (memq (char-before start) '(?@ ?/))))))

(defun mevedel-view--foreign-spelling-p (raw target)
  "Return non-nil when RAW names a filesystem other than TARGET's.

A response may print a path already qualified for somebody else's machine --
another TRAMP host, or a `/:' quoted client-local path.  Qualifying one of
those into TARGET yields a spelling that names nothing there, so it must not
become a link.

The rejection is structural on purpose.  It used to fall out of the
`file-exists-p' gate in `mevedel-view--path-target', which no longer runs on
the redraw path; leaving a target boundary resting on a stat meant it held
only for as long as the renderer was allowed to touch the connection."
  (let ((remote (file-remote-p raw)))
    (or (string-prefix-p "/:" raw)
        (and remote
             (not (equal remote (mevedel-execution-target-prefix target)))))))

(defun mevedel-view--resolve-path (raw)
  "Return an absolute path for RAW, or nil when no sensible anchor exists.
Resolve RAW in the execution target of the session tied to the current data
buffer."
  (when (stringp raw)
    (if-let* ((session (and (boundp 'mevedel--session) mevedel--session))
              (target (ignore-errors
                        (mevedel-session-execution-target session))))
        (unless (mevedel-view--foreign-spelling-p raw target)
          (ignore-errors (mevedel-execution-target-expand-path target raw)))
      (if (file-name-absolute-p raw)
          raw
        (when-let* ((session (and (boundp 'mevedel--session)
                                  mevedel--session))
                    (workspace (ignore-errors
                                 (mevedel-session-workspace session)))
                    (root (ignore-errors
                            (mevedel-workspace-root workspace))))
          (expand-file-name raw root))))))

(defun mevedel-view--path-target (path)
  "Return PATH's link target, or nil when it is unavailable.

An ordinary file returns t.  An available active-session artifact returns
`(SESSION . LOGICAL)'.

Answers entirely from memory.  This runs from a redraw timer,
`mevedel-view--flush-scheduled-render'; target I/O there can nest into an
active TRAMP command and cross the two replies.

An unverified remote path is therefore left undecorated rather than linked
optimistically.  `mevedel-view--verify-paths' promotes it after an idle,
deferrable check; the next redraw then links it."
  (when (stringp path)
    (let* ((session (bound-and-true-p mevedel--session))
           (target (and session
                        (mevedel-session-execution-target session)))
           (save-path (and session
                           target
                           (mevedel-execution-target-remote-p target)
                           (mevedel-session-save-path session)))
           (root (and save-path
                      (file-name-as-directory
                       (expand-file-name save-path))))
           (expanded (expand-file-name path)))
      (cond
       ((and root (string-prefix-p root expanded))
        (let ((logical (substring expanded (length root))))
          (when (and (mevedel-session-publication-logical-path-p logical)
                     (mevedel-view--artifact-present-p session logical))
            (cons session logical))))
       ;; A local path costs no connection, so it is still decided exactly.
       ((not (file-remote-p expanded))
        (and (file-exists-p expanded) t))
       ((mevedel-view--path-known-p expanded) t)
       (t
        (mevedel-view--note-unverified-path expanded)
        nil)))))

(defun mevedel-view--artifact-present-p (session logical)
  "Return non-nil when SESSION holds artifact LOGICAL, without target I/O.

`mevedel-session-artifacts-artifact-present-p' cannot be used here: on a
portable session it takes a lease reading through
`mevedel-session-durability--target-time', which runs a control-filesystem
program.  From a redraw that both nests and contends with publication for
the same control filesystem -- the \"target is already in use\" refusal.
Both sources consulted here are already in memory."
  (or (and (mevedel-session-publication-uncommitted-artifact session logical) t)
      (when-let* ((publication (mevedel-session-publication session)))
        (and (assoc logical (plist-get publication :artifacts)) t))))

(defun mevedel-view--linkify-path-action (button)
  "Open the file or published session artifact referenced by BUTTON."
  (let ((path (button-get button 'mevedel-view-path))
        (line (button-get button 'mevedel-view-line))
        (session (button-get button 'mevedel-view-session))
        (logical (button-get button 'mevedel-view-session-artifact)))
    (when-let* ((buffer
                (cond
                 ((and session logical)
                  (pop-to-buffer
                   (mevedel-session-artifacts-find-artifact-noselect
                    session logical t)))
                 ((and path (file-exists-p path))
                  (find-file-other-window path)))))
      (when (and line (integerp line) (> line 0))
        (with-current-buffer buffer
          (goto-char (point-min))
          (forward-line (1- line))
          (when-let* ((window (get-buffer-window buffer t)))
            (set-window-point window (point))))))))

(defconst mevedel-view--link-action-properties
  '(keymap nil
    follow-link nil
    mouse-face nil
    help-echo nil
    button nil
    category nil
    action nil
    pointer nil)
  "Text properties that make rendered text act like a link.")

(defun mevedel-view--open-url-action (button)
  "Open BUTTON's URL with `browse-url'."
  (when-let* ((url (button-get button 'mevedel-view-url)))
    (browse-url url)))

(defconst mevedel-view--line-ref-atom-regexp
  "\\(?:#L\\|L\\)?[1-9][0-9]*"
  "Regexp matching one line number atom in a file reference.")

(defconst mevedel-view--line-ref-regexp
  (concat mevedel-view--line-ref-atom-regexp
          "\\(?:-" mevedel-view--line-ref-atom-regexp "\\)?")
  "Regexp matching one line or line-range reference.")

(defconst mevedel-view--line-ref-list-regexp
  (concat mevedel-view--line-ref-regexp
          "\\(?:," mevedel-view--line-ref-regexp "\\)*")
  "Regexp matching comma-separated line references.")

(defconst mevedel-view--direct-line-ref-list-regexp
  (concat "#L[1-9][0-9]*"
          "\\(?:-" mevedel-view--line-ref-atom-regexp "\\)?"
          "\\(?:," mevedel-view--line-ref-regexp "\\)*")
  "Regexp matching line references that immediately follow a path.")

(defconst mevedel-view--line-ref-suffix-regexp
  (concat "\\(?:"
          ":\\(" mevedel-view--line-ref-list-regexp "\\)"
          "\\|\\(" mevedel-view--direct-line-ref-list-regexp "\\)"
          "\\)")
  "Regexp matching a file-reference line suffix.")

(defun mevedel-view--line-ref-start-line (text)
  "Return the first line number in line reference TEXT, or nil."
  (save-match-data
    (when (and (stringp text)
               (string-match
                "\\`\\(?:#L\\|L\\)?\\([1-9][0-9]*\\)\\(?:-\\(?:#L\\|L\\)?[1-9][0-9]*\\)?\\'"
                text))
      (string-to-number (match-string 1 text)))))

(defun mevedel-view--line-ref-list-start-line (text)
  "Return the first line number in line reference list TEXT, or nil."
  (when-let* ((first (car (split-string (or text "") "," t))))
    (mevedel-view--line-ref-start-line first)))

(defun mevedel-view--make-file-button (start end path line)
  "Make START..END a button visiting PATH at optional LINE."
  (let ((target (mevedel-view--path-target path)))
    (make-text-button
     start end
     'action #'mevedel-view--linkify-path-action
     'mevedel-view-path path
     'mevedel-view-line line
     'mevedel-view-session (car-safe target)
     'mevedel-view-session-artifact (cdr-safe target)
     'follow-link t
     'help-echo (if line
                    (format "Visit %s:%d" path line)
                  (format "Visit %s" path)))))

(defun mevedel-view--markdown-code-blocks (start end &optional incomplete)
  "Return fenced Markdown code blocks between START and END.
When INCOMPLETE is non-nil, include an unclosed final block whose body
ends at END."
  (let (blocks
        (case-fold-search nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward
              "^ \\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)\\([^\n]*\\)\n" end t)
        (let* ((fence (match-string-no-properties 1))
               (delimiter (aref fence 0))
               (info (match-string-no-properties 2))
               (fence-start (match-beginning 0))
               (fence-end (match-end 0))
               (body-start (point))
               (language (car (split-string info nil t)))
               (closing-regexp
                (concat "^ \\{0,3\\}" (regexp-quote fence)
                        (regexp-quote (char-to-string delimiter))
                        "*[ \t]*$")))
          (unless (and (= delimiter ?`)
                       (string-search "`" info))
            (if (re-search-forward closing-regexp end t)
                (push (list :fence-start fence-start
                            :fence-end fence-end
                            :language language
                            :body-start body-start
                            :body-end (match-beginning 0)
                            :end-fence-start (match-beginning 0)
                            :end-fence-end (match-end 0))
                      blocks)
              (when incomplete
                (push (list :fence-start fence-start
                            :fence-end fence-end
                            :language language
                            :body-start body-start
                            :body-end end)
                      blocks))
              (goto-char end))))))
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
    (dolist (block (mevedel-view--markdown-code-blocks start end t))
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

(defun mevedel-view--last-live-response-boundary (data-buf start end)
  "Return the last safe Markdown block boundary in DATA-BUF START..END."
  (with-current-buffer data-buf
    (let ((ranges
           (mapcar
            (lambda (block)
              (cons (plist-get block :fence-start)
                    (or (plist-get block :end-fence-end)
                        (plist-get block :body-end))))
            (mevedel-view--markdown-code-blocks start end t)))
          boundary)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let ((line-start (point)))
            (forward-line 1)
            (when (and (save-excursion
                         (goto-char line-start)
                         (looking-at "[ \t]*$"))
                       (not (mevedel-view--position-in-ranges-p
                             line-start ranges)))
              (setq boundary (min (point) end))))))
      (and boundary (< boundary end) boundary))))

(defun mevedel-view--linkify-exempt-p (position)
  "Return non-nil when POSITION opted out of Markdown decoration.
Renderers stamp `mevedel-view-no-linkify' on verbatim content, such as
diff lines, that must never be linkified, rewritten, or inlined."
  (get-text-property position 'mevedel-view-no-linkify))

(defun mevedel-view--decoration-blocked-p (position ranges)
  "Return non-nil when POSITION must not be decorated.
True for linkify-exempt text and for positions inside RANGES."
  (or (mevedel-view--linkify-exempt-p position)
      (mevedel-view--position-in-ranges-p position ranges)))

(defconst mevedel-view--image-extensions
  '("png" "jpg" "jpeg" "gif" "webp")
  "Image filename extensions rendered inline in the view.")

(defun mevedel-view--image-file-p (path)
  "Return non-nil when PATH names a supported image file."
  (and (stringp path)
       (member (downcase (or (file-name-extension path) ""))
               mevedel-view--image-extensions)))

(defun mevedel-view--local-link-target (url)
  "Resolve URL or path string to an available file path."
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
      (and resolved (mevedel-view--path-target resolved) resolved))))

(defun mevedel-view--local-link-line (url)
  "Return URL's trailing #L line number, or nil."
  (when (and (stringp url)
             (string-match
              (concat mevedel-view--line-ref-suffix-regexp "\\'")
              url))
    (mevedel-view--line-ref-list-start-line
     (or (match-string-no-properties 1 url)
         (match-string-no-properties 2 url)))))

(defun mevedel-view--image-sizing (window)
  "Return inline-image sizing as a cons (MAX-WIDTH . MEASURED).
MAX-WIDTH is the pixel width limit derived from
`mevedel-view-inline-image-max-width'.  MEASURED is the window pixel
width a ratio setting was resolved against, or nil for fixed-pixel
sizing or when WINDOW is not live."
  (let ((setting mevedel-view-inline-image-max-width))
    (cond
     ((and (integerp setting) (> setting 0)) (cons setting nil))
     ((not (and (floatp setting) (< 0.0 setting) (<= setting 1.0)))
      (error "Invalid inline image maximum width: %S" setting))
     ((and window (window-live-p window))
      (let ((width (window-body-width window t)))
        (cons (max 1 (floor (* setting width))) width)))
     (t (cons (max 1 (floor (* setting (frame-pixel-width)))) nil)))))

(defun mevedel-view--image-display (path max-width)
  "Return an image display spec for PATH capped at MAX-WIDTH pixels."
  ;; ponytail: a remote artifact image re-reads its published bytes on
  ;; every recreation; retain the decoded bytes alongside the source
  ;; path if ratio-sized artifact images make TRAMP resizes slow.
  (when (and (display-images-p)
             (mevedel-view--image-file-p path))
    (when-let* ((target (mevedel-view--path-target path)))
      (condition-case nil
          (if (consp target)
              (create-image
               (mevedel-session-artifacts-read-artifact
                (car target) (cdr target) t)
               nil t :max-width max-width)
            (create-image path nil nil :max-width max-width))
        (error nil)))))

(defun mevedel-view--put-image-display (start end path &optional window)
  "Display PATH as an image over START..END when possible.
Under a ratio `mevedel-view-inline-image-max-width', the image region
retains PATH, that ratio, and the measured width of WINDOW (defaulting
to a window showing the buffer) so `mevedel-view--rerender-images'
recreates only stale images."
  (let* ((window (if (and window (window-live-p window))
                     window
                   (get-buffer-window (current-buffer) t)))
         (sizing (mevedel-view--image-sizing window)))
    (when-let* ((image (mevedel-view--image-display path (car sizing))))
      (add-text-properties
       start end
       (if (floatp mevedel-view-inline-image-max-width)
           `(display ,image
             help-echo ,(format "Image: %s" path)
             mevedel-view-image-source ,path
             mevedel-view-image-ratio ,mevedel-view-inline-image-max-width
             mevedel-view-image-width ,(cdr sizing)
             rear-nonsticky (display help-echo mevedel-view-image-source
                                     mevedel-view-image-ratio
                                     mevedel-view-image-width))
         `(display ,image
           help-echo ,(format "Image: %s" path)
           rear-nonsticky (display help-echo)))))))

(defun mevedel-view--rerender-images (&optional window)
  "Recreate ratio-sized inline images stale for WINDOW.
WINDOW defaults to a window showing the buffer.  Each image retains
the ratio it was rendered with, independent of later configuration
changes.  A no-op when the buffer is undisplayed or every tracked
image already matches the window.  Callers own undo and modified-flag
discipline."
  (when-let* ((window (if (and window (window-live-p window))
                          window
                        (get-buffer-window (current-buffer) t)))
              (width (window-body-width window t)))
    (save-excursion
      (goto-char (point-min))
      (let (match)
        (while (setq match (text-property-search-forward
                            'mevedel-view-image-ratio))
          (let ((beg (prop-match-beginning match))
                (ratio (prop-match-value match)))
            (unless (eql width (get-text-property
                                beg 'mevedel-view-image-width))
              (let ((mevedel-view-inline-image-max-width ratio))
                (mevedel-view--put-image-display
                 beg (prop-match-end match)
                 (get-text-property beg 'mevedel-view-image-source)
                 window)))))))))

(defun mevedel-view--decorate-local-images-in-range (start end)
  "Render local Markdown image links and bare image paths between START and END."
  (let ((code-ranges (mevedel-view--src-block-body-ranges start end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "!\\[[^]\n]*\\](\\([^)]+\\))" end t)
        (let* ((mb (match-beginning 0))
               (me (match-end 0))
               (url (match-string-no-properties 1))
               (path (and (not (mevedel-view--decoration-blocked-p
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
                          (not (mevedel-view--decoration-blocked-p
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
          (when (and resolved (mevedel-view--path-target resolved)
                     (not (mevedel-view--linkify-exempt-p mb)))
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
          (when (and path
                     (not (mevedel-view--linkify-exempt-p whole-start)))
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
               (source (buffer-substring-no-properties
                        whole-start whole-end))
               (title (buffer-substring (match-beginning 1) (match-end 1)))
               (url (match-string-no-properties 2)))
          (unless (or (and (> whole-start (point-min))
                           (eq (char-before whole-start) ?!))
                      (mevedel-view--decoration-blocked-p
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
             'help-echo (format "Visit %s" url))
            (put-text-property
             whole-start (point) 'mevedel-view-markdown-source source)))))))

(defun mevedel-view--markdown-source (start end)
  "Return START..END with stripped Markdown constructs restored."
  (let ((pos start)
        parts)
    (while (< pos end)
      (let* ((source (get-text-property
                      pos 'mevedel-view-markdown-source))
             (run-end (next-single-property-change
                       pos 'mevedel-view-markdown-source nil (point-max)))
             (limit (min run-end end)))
        (push (if (and source
                       (<= run-end end)
                       (>= (previous-single-property-change
                            (min (1+ pos) (point-max))
                            'mevedel-view-markdown-source nil (point-min))
                           start))
                  source
                (buffer-substring pos limit))
              parts)
        (setq pos limit)))
    (apply #'concat (nreverse parts))))

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
               (resolved (and (not (mevedel-view--decoration-blocked-p
                                    mb src-ranges))
                              (mevedel-view--path-candidate-p raw)
                              (mevedel-view--path-context-candidate-p mb raw)
                              (mevedel-view--resolve-path raw))))
          (when (and resolved (mevedel-view--path-target resolved))
            (mevedel-view--linkify-path-reference
             mb me suffix-start suffix-end resolved)))))))

(defun mevedel-view--decorate-markdown-in-range (start end)
  "Apply Markdown view affordances between START and END."
  (let ((end-marker (copy-marker end t)))
    (unwind-protect
        (progn
          (mevedel-view--decorate-code-blocks-in-range start end-marker)
          (mevedel-view--decorate-local-images-in-range start end-marker)
          (mevedel-view--render-markdown-url-links-in-range start end-marker)
          (mevedel-view--linkify-paths-in-range start end-marker)
          ;; Run after links and paths so their buttons and faces carry into
          ;; rendered cells.
          (mevedel-view-table-decorate
           start end-marker
           (mevedel-view--src-block-body-ranges start end-marker)))
      (set-marker end-marker nil))))

(defun mevedel-view--copy-code-block-button-action (button)
  "Copy BUTTON's fenced code block body."
  (when-let* ((range (button-get button 'mevedel-view-code-block-range))
              (start (marker-position (car range)))
              (end (marker-position (cdr range)))
              ((<= start end)))
    (kill-new (buffer-substring-no-properties start end))
    (message "Copied")))

(defun mevedel-view--selected-text-properties (position properties)
  "Return plist of PROPERTIES present at POSITION."
  (let (props)
    (dolist (prop properties)
      (when-let* ((value (get-text-property position prop)))
        (setq props (plist-put props prop value))))
    props))

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
             (language (plist-get block :language))
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
          (goto-char panel-start)
          (make-text-button
           (point) (+ (point) (length label))
           'action #'mevedel-view--copy-code-block-button-action
           'follow-link t
           'help-echo "Copy code block"
           'mevedel-view-code-block-range (cons body-start content-end)
           'font-lock-face 'mevedel-view-source-block-language
           'mouse-face 'highlight
           'pointer 'hand)
          (when (< (marker-position body-start)
                   (marker-position body-end))
            (mevedel-view--fontify-source-block-body
             (marker-position body-start)
             (marker-position body-end)
             language))
          (when (< (marker-position body-start)
                   (marker-position content-end))
            (put-text-property (marker-position body-start)
                               (marker-position content-end)
                               'mevedel-view-code-block-body t))
          (goto-char body-end)
          (let ((pad-start (point)))
            (insert panel-padding-line)
            (when carried
              (add-text-properties pad-start (point) carried))))
        (set-marker-insertion-type body-start nil)
        (set-marker body-end nil)))))


;;
;;; Window realignment

(defvar-local mevedel-view--realign-timer nil
  "Pending idle timer that re-lays out this view's tables and images.")
;; A view rebuild re-invokes `mevedel-view-mode' on the live buffer;
;; `kill-all-local-variables' must not orphan a scheduled timer.
(put 'mevedel-view--realign-timer 'permanent-local t)

(defun mevedel-view--realign-markdown (&optional buffer window)
  "Re-lay out stale rendered tables and ratio images in BUFFER.
WINDOW, when live and still showing BUFFER, is the window the layout
targets -- the one whose change scheduled this job.  A pure re-layout:
it stays off the undo list and leaves the modified flag, point, and
the data buffer unchanged.  A no-op when BUFFER is not displayed in
any window or nothing is stale."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq mevedel-view--realign-timer nil)
        (let ((window (and window
                           (window-live-p window)
                           (eq (window-buffer window) buffer)
                           window))
              (inhibit-read-only t)
              (buffer-undo-list t)
              (modified (buffer-modified-p)))
          (unwind-protect
              (progn
                (mevedel-view-table-rerender window)
                (mevedel-view--rerender-images window))
            (restore-buffer-modified-p modified)))))))

(defun mevedel-view--realign-on-window-change (window)
  "Schedule a deferred re-layout for the buffer shown in WINDOW.
Installed buffer-locally on `window-size-change-functions' and
`window-buffer-change-functions'.  Mutating a buffer inside those
redisplay hooks is unsafe, so the work is debounced onto one idle
timer; the buffer-change hook also catches first display and content
rendered while off-screen.  WINDOW rides along so the deferred job
lays out for the window that actually changed, not an arbitrary one."
  (when (window-live-p window)
    (mevedel-view--cancel-realign-timer)
    (setq mevedel-view--realign-timer
          (run-with-idle-timer 0.15 nil #'mevedel-view--realign-markdown
                               (current-buffer) window))))

(defun mevedel-view--cancel-realign-timer ()
  "Cancel any pending re-layout timer for the current buffer."
  (when (timerp mevedel-view--realign-timer)
    (cancel-timer mevedel-view--realign-timer))
  (setq mevedel-view--realign-timer nil))

(defun mevedel-view--enable-markdown-realign ()
  "Keep the current buffer's rendered tables and images window-aligned.
The handlers are buffer-local, so they run only for this buffer's
windows and disappear with the buffer; the pending timer is cancelled
on kill so no timer outlives its view."
  (add-hook 'window-size-change-functions
            #'mevedel-view--realign-on-window-change nil t)
  (add-hook 'window-buffer-change-functions
            #'mevedel-view--realign-on-window-change nil t)
  (add-hook 'kill-buffer-hook #'mevedel-view--cancel-realign-timer nil t))


;;
;;; Copying rendered Markdown

(defun mevedel-view--buffer-substring-filter (beg end &optional delete)
  "Return BEG..END with rendered tables restored to canonical Markdown.
A region overlapping a rendered table yields that table's complete
pipe-table source, spliced into the surrounding copied text, so pastes
stay valid Markdown.  With DELETE non-nil the region is also deleted,
matching `buffer-substring--filter'."
  (let ((start (min beg end))
        (finish (max beg end)))
    (if (not (text-property-not-all
              start finish 'mevedel-view-table-source nil))
        ;; No rendered table in the region: keep the stock filter's
        ;; behavior for every ordinary copy and kill.
        (buffer-substring--filter beg end delete)
      (let ((parts nil)
            (pos start))
        (while (< pos finish)
          (let ((source (get-text-property pos 'mevedel-view-table-source))
                (next (or (next-single-property-change
                           pos 'mevedel-view-table-source nil finish)
                          finish)))
            (push (if source
                      (substring-no-properties source)
                    (buffer-substring pos next))
                  parts)
            (setq pos next)))
        (prog1 (apply #'concat (nreverse parts))
          (when delete (delete-region start finish)))))))

(provide 'mevedel-view-markdown)

;;; mevedel-view-markdown.el ends here
