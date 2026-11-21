;;; niwa-view-markdown.el --- Markdown projection -*- lexical-binding: t -*-

;;; Commentary:

;; Owns Markdown links, local images, paths, fenced source panels, rendered
;; table copying, and window-relative Markdown realignment in niwa views.

;;; Code:

;; `browse-url'
(declare-function browse-url "browse-url" (url &optional new-window))

;; `niwa-collaboration'
(declare-function niwa-collaboration-open-shared-item
                  "niwa-collaboration" (data-buffer id))
(autoload 'niwa-collaboration-open-shared-item "niwa-collaboration")

;; `niwa-execution-target'
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(declare-function niwa-execution-target-prefix
                  "niwa-execution-target" (cl-x) t)
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(autoload 'niwa-execution-target-expand-path "niwa-execution-target")
(autoload 'niwa-execution-target-prefix "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-resource'
(declare-function niwa-resource-parse-address "niwa-resource" (address))
(declare-function niwa-resource-visit-path "niwa-resource"
                  (address &optional context))
(autoload 'niwa-resource-parse-address "niwa-resource")
(autoload 'niwa-resource-visit-path "niwa-resource")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-find-artifact-noselect
                  "niwa-session-artifacts"
                  (session logical &optional inspection committed-only))
(declare-function niwa-session-artifacts-read-artifact
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(autoload 'niwa-session-artifacts-find-artifact-noselect
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-artifact
  "niwa-session-artifacts")

;; `niwa-session-publication'
(declare-function niwa-session-publication-logical-path-p
                  "niwa-session-publication" (path))
(autoload 'niwa-session-publication-logical-path-p
  "niwa-session-publication")
(declare-function niwa-session-publication-uncommitted-artifact
                  "niwa-session-publication" (session logical))
(autoload 'niwa-session-publication-uncommitted-artifact
  "niwa-session-publication")

;; `niwa-structs'
(declare-function niwa-session-execution-target "niwa-structs" (cl-x) t)
(declare-function niwa-session-publication "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--session)

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-view'
(defvar niwa--data-buffer)
(defvar niwa-view-inline-image-max-width)

;; `niwa-view-agent'
(declare-function niwa-view-open-agent-transcript "niwa-view-agent"
                  (agent-path))
(autoload 'niwa-view-open-agent-transcript "niwa-view-agent")

;; `niwa-view-fontify'
(declare-function niwa-view--fontify-as "niwa-view-fontify" (text mode))

;; `niwa-view-render'
(declare-function niwa-view-render-mutate "niwa-view-render"
                  (key function &optional replacement cleanup))
(autoload 'niwa-view-render-mutate "niwa-view-render")

;; `niwa-view-table'
(declare-function niwa-view-table-decorate "niwa-view-table"
                  (start end avoid-ranges))
(declare-function niwa-view-table-rerender "niwa-view-table"
                  (&optional window))

;; `niwa-view-viewport'
(declare-function niwa-view--call-preserving-window-state
                  "niwa-view-viewport"
                  (thunk &optional positions-preserved region follow-output))

(require 'niwa-mention-bindings)
(require 'niwa-view-path)
(require 'niwa-view-table)
(require 'text-property-search)

(defun niwa-view--normalize-local-file-uri-path (path)
  "Normalize local file URI PATH for Emacs file APIs."
  (let ((path (and path (subst-char-in-string ?\\ ?/ path))))
    (if (and path
             (eq system-type 'windows-nt)
             (string-match "\\`/+\\([A-Za-z]:/.*\\)\\'" path))
        (match-string 1 path)
      path)))

(defvar niwa-view--linkify-path-regexp
  ;; Absolute paths, relative paths containing a slash, or names with an
  ;; extension.  Keep trailing `-' last in character classes.
  (concat "\\(?:[A-Za-z]:/[[:alnum:]_./+@~-]+"
          "\\|/[[:alnum:]_./+@~-]+"
          "\\|[[:alnum:]_.+@~-]+\\(?:/[[:alnum:]_./+@~-]+\\)+"
          "\\|[[:alnum:]_+-]+\\(?:\\.[[:alnum:]_+-]+\\)+\\)")
  "Regular expression matching candidate file paths in rendered bodies.")

(defun niwa-view--path-candidate-p (text)
  "Return non-nil when TEXT resembles a real path worth linkifying.
Accepts slash-containing paths and slashless filenames with an
extension.  Guards against matching URLs."
  (and (stringp text)
       (not (string-prefix-p "//" text))
       (not (string-match-p "\\`https?:" text))
       (or (string-search "/" text)
           (string-match-p "\\`[[:alnum:]_+-]+\\(?:\\.[[:alnum:]_+-]+\\)+\\'"
                           text))))

(defun niwa-view--path-context-candidate-p (start raw)
  "Return non-nil when RAW at START is not part of a larger locator."
  (and (not (eq (char-before start) ?:))
       (or (string-search "/" raw)
           (not (memq (char-before start) '(?@ ?/))))))

(defun niwa-view--foreign-spelling-p (raw target)
  "Return non-nil when RAW names a filesystem other than TARGET's.

A response may print a path already qualified for somebody else's machine --
another TRAMP host, or a `/:' quoted client-local path.  Qualifying one of
those into TARGET yields a spelling that names nothing there, so it must not
become a link.

The rejection is structural on purpose.  It used to fall out of the
`file-exists-p' gate in `niwa-view--path-target', which no longer runs on
the redraw path; leaving a target boundary resting on a stat meant it held
only for as long as the renderer was allowed to touch the connection."
  (let ((remote (file-remote-p raw)))
    (or (string-prefix-p "/:" raw)
        (and remote
             (not (equal remote (niwa-execution-target-prefix target)))))))

(defun niwa-view--resolve-path (raw)
  "Return an absolute path for RAW, or nil when no sensible anchor exists.
Resolve RAW in the execution target of the session tied to the current data
buffer."
  (when (stringp raw)
    (if-let* ((session (and (boundp 'niwa--session) niwa--session))
              (target (ignore-errors
                        (niwa-session-execution-target session))))
        (unless (niwa-view--foreign-spelling-p raw target)
          (ignore-errors (niwa-execution-target-expand-path target raw)))
      (if (file-name-absolute-p raw)
          raw
        (when-let* ((session (and (boundp 'niwa--session)
                                  niwa--session))
                    (workspace (ignore-errors
                                 (niwa-session-workspace session)))
                    (root (ignore-errors
                            (niwa-workspace-root workspace))))
          (expand-file-name raw root))))))

(defun niwa-view--path-target (path)
  "Return PATH's link target, or nil when it is unavailable.

An ordinary file returns t.  An available active-session artifact returns
`(SESSION . LOGICAL)'.

Answers entirely from memory.  This runs from a redraw timer,
`niwa-view--flush-scheduled-render'; target I/O there can nest into an
active TRAMP command and cross the two replies.

An unverified remote path is therefore left undecorated rather than linked
optimistically.  `niwa-view--verify-paths' promotes it after an idle,
deferrable check; the next redraw then links it."
  (when (stringp path)
    (let* ((session (bound-and-true-p niwa--session))
           (target (and session
                        (niwa-session-execution-target session)))
           (save-path (and session
                           target
                           (niwa-execution-target-remote-p target)
                           (niwa-session-save-path session)))
           (root (and save-path
                      (file-name-as-directory
                       (expand-file-name save-path))))
           (expanded (expand-file-name path)))
      (cond
       ((and root (string-prefix-p root expanded))
        (let ((logical (substring expanded (length root))))
          (when (and (niwa-session-publication-logical-path-p logical)
                     (niwa-view--artifact-present-p session logical))
            (cons session logical))))
       ;; A local path costs no connection, so it is still decided exactly.
       ((not (file-remote-p expanded))
        (and (file-exists-p expanded) t))
       ((niwa-view--path-known-p expanded) t)
       (t
        (niwa-view--note-unverified-path expanded)
        nil)))))

(defun niwa-view--artifact-present-p (session logical)
  "Return non-nil when SESSION holds artifact LOGICAL, without target I/O.

`niwa-session-artifacts-artifact-present-p' cannot be used here: on a
portable session it takes a lease reading through
`niwa-session-durability--target-time', which runs a control-filesystem
program.  From a redraw that both nests and contends with publication for
the same control filesystem -- the \"target is already in use\" refusal.
Both sources consulted here are already in memory."
  (or (and (niwa-session-publication-uncommitted-artifact session logical) t)
      (when-let* ((publication (niwa-session-publication session)))
        (and (assoc logical (plist-get publication :artifacts)) t))))

(defun niwa-view--linkify-path-action (button)
  "Open the file or published session artifact referenced by BUTTON."
  (let ((path (button-get button 'niwa-view-path))
        (line (button-get button 'niwa-view-line))
        (session (button-get button 'niwa-view-session))
        (logical (button-get button 'niwa-view-session-artifact)))
    (when-let* ((buffer
                (cond
                 ((and session logical)
                  (pop-to-buffer
                   (niwa-session-artifacts-find-artifact-noselect
                    session logical t
                    (button-get button 'niwa-view-committed-only))))
                 ((and path (file-exists-p path))
                  (find-file-other-window path)))))
      (when (and line (integerp line) (> line 0))
        (with-current-buffer buffer
          (goto-char (point-min))
          (forward-line (1- line))
          (when-let* ((window (get-buffer-window buffer t)))
            (set-window-point window (point))))))))

(defconst niwa-view--link-action-properties
  '(keymap nil
    follow-link nil
    mouse-face nil
    help-echo nil
    button nil
    category nil
    action nil
    pointer nil)
  "Text properties that make rendered text act like a link.")

(defconst niwa-view--resource-address-regexp
  (concat "\\(?:work\\|artifact\\|skill\\|memory\\|niwa\\|agent\\|history\\|shared\\)"
          "://[^][ \t\n`'\"()<>{}]*[^][ \t\n`'\"()<>{}.,;:!?]")
  "Regexp matching a resource address the view can open.
Trailing sentence punctuation is not part of the address.")

(defun niwa-view--visit-resource (address)
  "Open resource ADDRESS from the transcript.
A shared item opens in the room, an agent address its transcript, and
a file-backed address the file.  Resolution runs here, on click, never
during a redraw."
  (setq address
        (condition-case err
            (plist-get (niwa-resource-parse-address address) :canonical)
          (error (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "%s" (error-message-string err)))))
  (cond
   ;; Browser-made item ids are UUIDs; `shared://library' is no item.
   ((and (string-match "\\`shared://\\([a-zA-Z0-9_-]+\\)\\(?:/\\|\\'\\)" address)
         (not (equal (match-string 1 address) "library")))
    (niwa-collaboration-open-shared-item
     (bound-and-true-p niwa--data-buffer) (match-string 1 address)))
   ((string-match "\\`\\(?:agent\\|history\\)://\\(root/[^#]+\\)" address)
    (niwa-view-open-agent-transcript (concat "/" (match-string 1 address))))
   (t
    (let ((path (condition-case err
                    (niwa-resource-visit-path
                     address (list :session (bound-and-true-p niwa--session)))
                  (error (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "%s" (error-message-string err))))))
      (unless (and path (file-exists-p path))
        (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Nothing to open for %s" address))
      ;; Only working files and memory are editable through their address.
      (if (string-match-p "\\`\\(?:work\\|memory\\)://" address)
          (find-file-other-window path)
        (find-file-read-only-other-window path))))))

(defun niwa-view--open-url-action (button)
  "Open BUTTON's URL with `browse-url', or its resource address."
  (when-let* ((url (button-get button 'niwa-view-url)))
    (if (string-match-p (concat "\\`" niwa-view--resource-address-regexp) url)
        (niwa-view--visit-resource url)
      (browse-url url))))

(defconst niwa-view--line-ref-atom-regexp
  "\\(?:#L\\|L\\)?[1-9][0-9]*"
  "Regexp matching one line number atom in a file reference.")

(defconst niwa-view--line-ref-regexp
  (concat niwa-view--line-ref-atom-regexp
          "\\(?:-" niwa-view--line-ref-atom-regexp "\\)?")
  "Regexp matching one line or line-range reference.")

(defconst niwa-view--line-ref-list-regexp
  (concat niwa-view--line-ref-regexp
          "\\(?:," niwa-view--line-ref-regexp "\\)*")
  "Regexp matching comma-separated line references.")

(defconst niwa-view--direct-line-ref-list-regexp
  (concat "#L[1-9][0-9]*"
          "\\(?:-" niwa-view--line-ref-atom-regexp "\\)?"
          "\\(?:," niwa-view--line-ref-regexp "\\)*")
  "Regexp matching line references that immediately follow a path.")

(defconst niwa-view--line-ref-suffix-regexp
  (concat "\\(?:"
          ":\\(" niwa-view--line-ref-list-regexp "\\)"
          "\\|\\(" niwa-view--direct-line-ref-list-regexp "\\)"
          "\\)")
  "Regexp matching a file-reference line suffix.")

(defun niwa-view--line-ref-start-line (text)
  "Return the first line number in line reference TEXT, or nil."
  (save-match-data
    (when (and (stringp text)
               (string-match
                "\\`\\(?:#L\\|L\\)?\\([1-9][0-9]*\\)\\(?:-\\(?:#L\\|L\\)?[1-9][0-9]*\\)?\\'"
                text))
      (string-to-number (match-string 1 text)))))

(defun niwa-view--line-ref-list-start-line (text)
  "Return the first line number in line reference list TEXT, or nil."
  (when-let* ((first (car (split-string (or text "") "," t))))
    (niwa-view--line-ref-start-line first)))

(define-button-type 'niwa-view-image-button
  'face nil)

(defun niwa-view--make-file-button (start end path line &optional type)
  "Make START..END a button visiting PATH at optional LINE.
TYPE is a button type, defaulting to the standard one."
  (let ((target (niwa-view--path-target path)))
    (apply #'make-text-button
           start end
           'action #'niwa-view--linkify-path-action
           'niwa-view-path path
           'niwa-view-line line
           'niwa-view-session (car-safe target)
           'niwa-view-session-artifact (cdr-safe target)
           'follow-link t
           'help-echo (if line
                          (format "Visit %s:%d" path line)
                        (format "Visit %s" path))
           (and type (list :type type)))))

(defun niwa-view--markdown-code-blocks (start end &optional incomplete)
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

(defun niwa-view--src-block-body-ranges (start end)
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
    (dolist (block (niwa-view--markdown-code-blocks start end t))
      (when (< (plist-get block :body-start)
               (plist-get block :body-end))
        (push (cons (plist-get block :body-start)
                    (plist-get block :body-end))
              ranges)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((next (or (next-single-property-change
                         (point) 'niwa-view-code-block-body nil end)
                        end)))
          (when (get-text-property (point) 'niwa-view-code-block-body)
            (push (cons (point) next) ranges))
          (goto-char next))))
    (nreverse ranges)))

(defun niwa-view--position-in-ranges-p (position ranges)
  "Return non-nil when POSITION is inside one of RANGES."
  (seq-some (lambda (range)
              (and (<= (car range) position)
                   (< position (cdr range))))
            ranges))

(defun niwa-view--last-live-response-boundary (data-buf start end)
  "Return the last safe Markdown block boundary in DATA-BUF START..END."
  (with-current-buffer data-buf
    (let ((ranges
           (mapcar
            (lambda (block)
              (cons (plist-get block :fence-start)
                    (or (plist-get block :end-fence-end)
                        (plist-get block :body-end))))
            (niwa-view--markdown-code-blocks start end t)))
          boundary)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let ((line-start (point)))
            (forward-line 1)
            (when (and (save-excursion
                         (goto-char line-start)
                         (looking-at "[ \t]*$"))
                       (not (niwa-view--position-in-ranges-p
                             line-start ranges)))
              (setq boundary (min (point) end))))))
      (and boundary (< boundary end) boundary))))

(defun niwa-view--linkify-exempt-p (position)
  "Return non-nil when POSITION opted out of Markdown decoration.
Renderers stamp `niwa-view-no-linkify' on verbatim content, such as
diff lines, that must never be linkified, rewritten, or inlined."
  (get-text-property position 'niwa-view-no-linkify))

(defun niwa-view--decoration-blocked-p (position ranges)
  "Return non-nil when POSITION must not be decorated.
True for linkify-exempt text and for positions inside RANGES."
  (or (niwa-view--linkify-exempt-p position)
      (niwa-view--position-in-ranges-p position ranges)))

(defconst niwa-view--image-extensions
  '("png" "jpg" "jpeg" "gif" "webp")
  "Image filename extensions rendered inline in the view.")

(defun niwa-view--image-file-p (path)
  "Return non-nil when PATH names a supported image file."
  (and (stringp path)
       (member (downcase (or (file-name-extension path) ""))
               niwa-view--image-extensions)))

(defun niwa-view--local-link-target (url)
  "Resolve URL or path string to an available file path."
  (when (and (stringp url)
             (not (string-empty-p url))
             (not (string-match-p "\\`https?://" url)))
    (let* ((without-fragment
            (replace-regexp-in-string
             (concat niwa-view--line-ref-suffix-regexp "\\'")
             "" url))
           (raw (if (string-prefix-p "file://" without-fragment)
                    (niwa-view--normalize-local-file-uri-path
                     (substring without-fragment 7))
                  without-fragment))
           (resolved (niwa-view--resolve-path raw)))
      (and resolved (niwa-view--path-target resolved) resolved))))

(defun niwa-view--local-link-line (url)
  "Return URL's trailing #L line number, or nil."
  (when (and (stringp url)
             (string-match
              (concat niwa-view--line-ref-suffix-regexp "\\'")
              url))
    (niwa-view--line-ref-list-start-line
     (or (match-string-no-properties 1 url)
         (match-string-no-properties 2 url)))))

(defun niwa-view--image-sizing (window)
  "Return inline-image sizing as a cons (MAX-WIDTH . MEASURED).
MAX-WIDTH is the pixel width limit derived from
`niwa-view-inline-image-max-width'.  MEASURED is the window pixel
width a ratio setting was resolved against, or nil for fixed-pixel
sizing or when WINDOW is not live."
  (let ((setting niwa-view-inline-image-max-width))
    (cond
     ((and (integerp setting) (> setting 0)) (cons setting nil))
     ((not (and (floatp setting) (< 0.0 setting) (<= setting 1.0)))
      (error "Invalid inline image maximum width: %S" setting))
     ((and window (window-live-p window))
      (let ((width (window-body-width window t)))
        (cons (max 1 (floor (* setting width))) width)))
     (t (cons (max 1 (floor (* setting (frame-pixel-width)))) nil)))))

(defun niwa-view--image-display (path max-width)
  "Return an image display spec for PATH capped at MAX-WIDTH pixels."
  ;; ponytail: a remote artifact image re-reads its published bytes on
  ;; every recreation; retain the decoded bytes alongside the source
  ;; path if ratio-sized artifact images make TRAMP resizes slow.
  (when (and (display-images-p)
             (niwa-view--image-file-p path))
    (when-let* ((target (niwa-view--path-target path)))
      (condition-case nil
          (if (consp target)
              (create-image
               (niwa-session-artifacts-read-artifact
                (car target) (cdr target) t)
               nil t :max-width max-width)
            (create-image path nil nil :max-width max-width))
        (error nil)))))

(defvar-local niwa-view--image-layout-width nil
  "Window body width the last image layout pass used, or nil.
Placing an image clears it, so the next pass checks again.")

(defun niwa-view--put-image-display (start end path &optional window)
  "Display PATH as an image over START..END when possible.
Under a ratio `niwa-view-inline-image-max-width', the image region
retains PATH, that ratio, and the measured width of WINDOW (defaulting
to a window showing the buffer) so `niwa-view--rerender-images'
recreates only stale images."
  (let* ((window (if (and window (window-live-p window))
                     window
                   (get-buffer-window (current-buffer) t)))
         (sizing (niwa-view--image-sizing window)))
    (setq niwa-view--image-layout-width nil)
    (when-let* ((image (niwa-view--image-display path (car sizing))))
      ;; The image button type has no category face: font-lock's unfontify
      ;; pass strips `face', and the default button category's underlined
      ;; face would then draw across the image's text baseline.  The
      ;; `font-lock-face' copy survives fontification.
      (niwa-view--make-file-button
       start end path nil 'niwa-view-image-button)
      ;; Override the transcript's explicit keymap, not just its category.
      (let ((plain '(:underline nil :overline nil
                     :strike-through nil :box nil)))
        (add-text-properties start end
                             `(keymap ,button-map pointer hand
                               face ,plain font-lock-face ,plain
                               niwa-view-committed-only t)))
      (add-text-properties
       start end
       (if (floatp niwa-view-inline-image-max-width)
           `(display ,image
             help-echo ,(format "Open image: %s (RET or click)" path)
             niwa-view-image-source ,path
             niwa-view-image-ratio ,niwa-view-inline-image-max-width
             niwa-view-image-width ,(cdr sizing)
             rear-nonsticky (display help-echo niwa-view-image-source
                                     niwa-view-image-ratio
                                     niwa-view-image-width))
         `(display ,image
           help-echo ,(format "Open image: %s (RET or click)" path)
           rear-nonsticky (display help-echo)))))))

(defun niwa-view--rerender-images (&optional window)
  "Recreate ratio-sized inline images stale for WINDOW.
WINDOW defaults to a window showing the buffer.  Each image retains
the ratio it was rendered with, independent of later configuration
changes.  A no-op when the buffer is undisplayed or every tracked
image already matches the window.  Callers own undo and modified-flag
discipline."
  (when-let* ((window (if (and window (window-live-p window))
                          window
                        (get-buffer-window (current-buffer) t)))
              (width (window-body-width window t))
              ((not (eql width niwa-view--image-layout-width))))
    (save-excursion
      (goto-char (point-min))
      (let (match)
        (while (setq match (text-property-search-forward
                            'niwa-view-image-ratio))
          (let ((beg (prop-match-beginning match))
                (ratio (prop-match-value match)))
            (unless (eql width (get-text-property
                                beg 'niwa-view-image-width))
              (let ((niwa-view-inline-image-max-width ratio))
                (niwa-view--put-image-display
                 beg (prop-match-end match)
                 (get-text-property beg 'niwa-view-image-source)
                 window)))))))
    ;; The scan walks the whole buffer; repeat it only for a new width or
    ;; after an image was placed.
    (setq niwa-view--image-layout-width width)))

(defun niwa-view--decorate-local-images-in-range (start end)
  "Render local Markdown image links and bare image paths between START and END."
  (let ((code-ranges (niwa-view--src-block-body-ranges start end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "!\\[[^]\n]*\\](\\([^)]+\\))" end t)
        (let* ((mb (match-beginning 0))
               (me (match-end 0))
               (url (match-string-no-properties 1))
               (path (and (not (niwa-view--decoration-blocked-p
                                mb code-ranges))
                          (niwa-view--local-link-target url))))
          (when (and path (niwa-view--image-file-p path))
            (niwa-view--put-image-display mb me path)))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward niwa-view--linkify-path-regexp end t)
        (let* ((mb (match-beginning 0))
               (me (match-end 0))
               (raw (match-string-no-properties 0))
               ;; Resolving keeps the extension; test it before paying for
               ;; target path expansion on every path-like word.
               (path (and (niwa-view--image-file-p raw)
                          (not (get-text-property mb 'display))
                          (not (niwa-view--decoration-blocked-p
                                mb code-ranges))
                          (niwa-view--path-candidate-p raw)
                          (niwa-view--path-context-candidate-p mb raw)
                          (niwa-view--resolve-path raw))))
          (when (and path (niwa-view--image-file-p path))
            (niwa-view--put-image-display mb me path)))))))

(defconst niwa-view--file-mention-regexp
  (concat "@file:" niwa-mention-bindings-file-token-regexp
          "\\(" niwa-view--direct-line-ref-list-regexp "\\)?")
  "Regexp matching rendered `@file' mentions.")

(defun niwa-view--linkify-file-mentions-in-range (start end)
  "Turn rendered `@file' mentions into file buttons between START and END."
  (let (ranges)
    (save-excursion
      (goto-char start)
      (while (re-search-forward niwa-view--file-mention-regexp end t)
        (let* ((mb (match-beginning 0))
               (me (match-end 0))
               (raw (niwa-mention-bindings-file-token-path
                     (match-string-no-properties 1)))
               (line (and (match-beginning 2)
                          (niwa-view--line-ref-list-start-line
                           (match-string-no-properties 2))))
               (resolved (niwa-view--resolve-path raw)))
          (push (cons mb me) ranges)
          (when (and resolved (niwa-view--path-target resolved)
                     (not (niwa-view--linkify-exempt-p mb)))
            (niwa-view--make-file-button mb me resolved line)))))
    (nreverse ranges)))

(defun niwa-view--linkify-markdown-file-links-in-range (start end)
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
               (path (niwa-view--local-link-target url))
               (line (niwa-view--local-link-line url)))
          (push (cons whole-start whole-end) ranges)
          (when (and path
                     (not (get-text-property whole-start 'display))
                     (not (niwa-view--linkify-exempt-p whole-start)))
            (niwa-view--make-file-button mb me path line)))))
    (nreverse ranges)))

(defun niwa-view--render-markdown-url-links-in-range (start end)
  "Render Markdown URL links between START and END as clickable labels.
Besides web URLs, a link may target a resource address."
  (let ((src-ranges (niwa-view--src-block-body-ranges start end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward
              (concat "\\[\\([^]\n]+\\)\\](\\(https?://[^)\n]+\\|"
                      niwa-view--resource-address-regexp "\\))")
              end t)
        (let* ((whole-start (match-beginning 0))
               (whole-end (match-end 0))
               (source (buffer-substring-no-properties
                        whole-start whole-end))
               (title (buffer-substring (match-beginning 1) (match-end 1)))
               (url (match-string-no-properties 2)))
          (unless (or (and (> whole-start (point-min))
                           (eq (char-before whole-start) ?!))
                      (niwa-view--decoration-blocked-p
                       whole-start src-ranges))
            (remove-text-properties
             0 (length title) niwa-view--link-action-properties title)
            (delete-region whole-start whole-end)
            (goto-char whole-start)
            (insert title)
            (make-text-button
             whole-start (point)
             'action #'niwa-view--open-url-action
             'niwa-view-url url
             'follow-link t
             'face 'link
             'mouse-face 'highlight
             'help-echo (format "Visit %s" url))
            (put-text-property
             whole-start (point) 'niwa-view-markdown-source source)))))))

(defun niwa-view--markdown-source (start end)
  "Return START..END with stripped Markdown constructs restored."
  (let ((pos start)
        parts)
    (while (< pos end)
      (let* ((source (get-text-property
                      pos 'niwa-view-markdown-source))
             (run-end (next-single-property-change
                       pos 'niwa-view-markdown-source nil (point-max)))
             (limit (min run-end end)))
        (push (if (and source
                       (<= run-end end)
                       (>= (previous-single-property-change
                            (min (1+ pos) (point-max))
                            'niwa-view-markdown-source nil (point-min))
                           start))
                  source
                (buffer-substring pos limit))
              parts)
        (setq pos limit)))
    (apply #'concat (nreverse parts))))

(defun niwa-view--linkify-path-reference
    (path-start path-end suffix-start suffix-end path)
  "Create file buttons for PATH reference at PATH-START..PATH-END.
When SUFFIX-START and SUFFIX-END delimit a line-reference list, create
one button per reference.  The first button includes the path text."
  (if (not (and suffix-start suffix-end (< suffix-start suffix-end)))
      (niwa-view--make-file-button path-start path-end path nil)
    (let ((first t))
      (save-excursion
        (goto-char suffix-start)
        (while (re-search-forward niwa-view--line-ref-regexp suffix-end t)
          (niwa-view--make-file-button
           (if first path-start (match-beginning 0))
           (match-end 0)
           path
           (niwa-view--line-ref-start-line
            (match-string-no-properties 0)))
          (setq first nil))))))

(defvar niwa-view--path-link-memo nil
  "Hash of raw path text to its link, scoped to one projection.
A live turn repeats the same paths across many tool rows, and each
lookup expands the path in the execution target.")

(defun niwa-view--path-link (raw)
  "Return RAW's resolved absolute path when it should become a link."
  (let ((memo niwa-view--path-link-memo))
    (if-let* ((hit (and memo (gethash raw memo))))
        (car hit)
      (let ((resolved (when-let* ((resolved (niwa-view--resolve-path raw))
                                  ((niwa-view--path-target resolved)))
                        resolved)))
        (when memo
          (puthash raw (list resolved) memo))
        resolved))))

(defun niwa-view--linkify-paths-in-range (start end)
  "Scan the buffer between START and END and turn paths into text buttons.
Clickable targets are resolved to absolute paths via
`niwa-view--resolve-path' and gated on `file-exists-p' -- paths that
don't resolve to an existing file stay as plain text.  References may
include line suffixes, such as file.el:12, file.el:L12-L20, and
file.el#L12."
  (let ((regexp (concat "\\(" niwa-view--linkify-path-regexp "\\)"
                        "\\(?:" niwa-view--line-ref-suffix-regexp "\\)?"))
        (src-ranges (niwa-view--src-block-body-ranges start end)))
    (setq src-ranges
          (append (niwa-view--linkify-file-mentions-in-range start end)
                  (niwa-view--linkify-markdown-file-links-in-range start end)
                  src-ranges))
    (save-excursion
      (goto-char start)
      (while (re-search-forward regexp end t)
        (let* ((mb (match-beginning 1))
               (me (match-end 0))
               (raw (buffer-substring-no-properties mb (match-end 1)))
               (suffix-start (or (match-beginning 2) (match-beginning 3)))
               (suffix-end (or (match-end 2) (match-end 3)))
               (resolved (and (not (get-text-property mb 'display))
                              (not (niwa-view--decoration-blocked-p
                                    mb src-ranges))
                              (niwa-view--path-candidate-p raw)
                              (niwa-view--path-context-candidate-p mb raw)
                              (niwa-view--path-link raw))))
          (when resolved
            (niwa-view--linkify-path-reference
             mb me suffix-start suffix-end resolved)))))))

(defun niwa-view--linkify-resource-addresses-in-range (start end)
  "Make bare resource addresses between START and END open their resource.
Inline code counts; fenced code and verbatim text do not.  Resolution
waits for the click, so a redraw does no target I/O."
  (let ((src-ranges (niwa-view--src-block-body-ranges start end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward niwa-view--resource-address-regexp end t)
        (let ((mb (match-beginning 0))
              (me (match-end 0)))
          (unless (or (button-at mb)
                      (memq (char-syntax (or (char-before mb) ?\s)) '(?w ?_))
                      (niwa-view--decoration-blocked-p mb src-ranges))
            (make-text-button
             mb me
             'action #'niwa-view--open-url-action
             'niwa-view-url (match-string-no-properties 0)
             'follow-link t
             'help-echo (format "Open %s" (match-string-no-properties 0)))))))))

(defun niwa-view--decorate-markdown-in-range (start end)
  "Apply Markdown view affordances between START and END."
  (let ((end-marker (copy-marker end t)))
    (unwind-protect
        (progn
          (niwa-view--decorate-code-blocks-in-range start end-marker)
          (niwa-view--decorate-local-images-in-range start end-marker)
          (niwa-view--render-markdown-url-links-in-range start end-marker)
          (niwa-view--linkify-paths-in-range start end-marker)
          (niwa-view--linkify-resource-addresses-in-range start end-marker)
          ;; Run after links and paths so their buttons and faces carry into
          ;; rendered cells.
          (niwa-view-table-decorate
           start end-marker
           (niwa-view--src-block-body-ranges start end-marker))
          (when (derived-mode-p 'niwa-view-mode)
            (niwa-view--realign-on-window-change)))
      (set-marker end-marker nil))))

(defun niwa-view--copy-code-block-button-action (button)
  "Copy BUTTON's fenced code block body."
  (when-let* ((range (button-get button 'niwa-view-code-block-range))
              (start (marker-position (car range)))
              (end (marker-position (cdr range)))
              ((<= start end)))
    (kill-new (buffer-substring-no-properties start end))
    (message "Copied")))

(defun niwa-view--selected-text-properties (position properties)
  "Return plist of PROPERTIES present at POSITION."
  (let (props)
    (dolist (prop properties)
      (when-let* ((value (get-text-property position prop)))
        (setq props (plist-put props prop value))))
    props))

(defconst niwa-view--source-block-carried-properties
  '(niwa-view-source
    niwa-view-source-key
    niwa-view-type
    niwa-view-collapsed
    niwa-view-turn-id
    read-only
    keymap
    front-sticky
    rear-nonsticky)
  "Text properties copied onto inserted Markdown source panel text.")

(defconst niwa-view--source-block-mode-alist
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

(defun niwa-view--source-block-mode (language)
  "Return a major mode for source block LANGUAGE, or nil."
  (when-let* ((language (and (stringp language)
                             (downcase language))))
    (let ((mode (or (cdr (assoc language
                                niwa-view--source-block-mode-alist))
                    (intern-soft (concat language "-mode")))))
      (and (fboundp mode) mode))))

(defun niwa-view--source-block-font-lock-face (face)
  "Return FACE combined with the source block panel face."
  (cond
   ((null face) 'niwa-view-source-block)
   ((listp face) (append face '(niwa-view-source-block)))
   (t (list face 'niwa-view-source-block))))

(defun niwa-view--fontify-source-block-body (start end language)
  "Apply best-effort LANGUAGE fontification to source body START..END."
  (let* ((mode (niwa-view--source-block-mode language))
         (text (buffer-substring-no-properties start end))
         (fontified (and mode (niwa-view--fontify-as text mode)))
         (limit (and fontified
                     (min (length fontified) (- end start)))))
    (put-text-property start end 'font-lock-face 'niwa-view-source-block)
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
               (niwa-view--source-block-font-lock-face face)))
            (setq pos next)))))))

(defun niwa-view--decorate-code-blocks-in-range (start end)
  "Render fenced Markdown code blocks as source panels in START..END."
  (let ((inhibit-read-only t))
    (dolist (block (reverse (niwa-view--markdown-code-blocks start end)))
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
              (niwa-view--selected-text-properties
               fence-start
               niwa-view--source-block-carried-properties)))
        (delete-region end-fence-start end-fence-delete-end)
        (delete-region fence-start fence-end)
        (let* ((panel-start (marker-position body-start))
               (panel-padding-line (propertize "\n"
                                                'font-lock-face
                                                'niwa-view-source-block))
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
           'action #'niwa-view--copy-code-block-button-action
           'follow-link t
           'help-echo "Copy code block"
           'niwa-view-code-block-range (cons body-start content-end)
           'font-lock-face 'niwa-view-source-block-language
           'mouse-face 'highlight
           'pointer 'hand)
          (when (< (marker-position body-start)
                   (marker-position body-end))
            (niwa-view--fontify-source-block-body
             (marker-position body-start)
             (marker-position body-end)
             language))
          (when (< (marker-position body-start)
                   (marker-position content-end))
            (put-text-property (marker-position body-start)
                               (marker-position content-end)
                               'niwa-view-code-block-body t))
          (goto-char body-end)
          (let ((pad-start (point)))
            (insert panel-padding-line)
            (when carried
              (add-text-properties pad-start (point) carried))))
        (set-marker-insertion-type body-start nil)
        (set-marker body-end nil)))))


;;
;;; Window realignment

(defvar-local niwa-view--realign-timer nil
  "Pending idle timer that re-lays out this view's tables and images.")
;; A view rebuild re-invokes `niwa-view-mode' on the live buffer;
;; `kill-all-local-variables' must not orphan a scheduled timer.
(put 'niwa-view--realign-timer 'permanent-local t)

(defun niwa-view--realign-markdown (&optional buffer window)
  "Format one visible table and re-lay out stale ratio images in BUFFER.
Resolve positions inside the view's mutation owner, including queued work.
WINDOW supplies the target width if it still displays BUFFER.  Preserve
undo state, the modified flag, selections and the authoritative transcript."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (niwa-view--cancel-realign-timer)
        (cond
         ((input-pending-p)
          (niwa-view--realign-on-window-change window))
         ;; Streaming scrolls the view after every render.  Take the view's
         ;; mutation ownership only when a table or image needs this width.
         ((not (when-let* ((target (if (and (window-live-p window)
                                            (eq (window-buffer window) buffer))
                                       window
                                     (get-buffer-window buffer t))))
                 (or (not (eql (window-body-width target t)
                               niwa-view--image-layout-width))
                     (niwa-view-table--visible-stale target))))
          nil)
         (t
          (niwa-view-render-mutate
           'markdown-realign
           (lambda ()
             (let ((window (if (and (window-live-p window)
                                    (eq (window-buffer window) (current-buffer)))
                               window
                             (get-buffer-window (current-buffer) t)))
                   (inhibit-read-only t)
                   (deactivate-mark deactivate-mark)
                   (buffer-undo-list t)
                   (modified (buffer-modified-p))
                   rendered)
               (unwind-protect
                   (when window
                     ;; A table or image changes height above the composer.
                     ;; The splice maps logical positions by table cell, so
                     ;; do not overwrite them with source-run offsets.  The
                     ;; composer keeps its pixel Y, and a window
                     ;; left past its shortened end gets its last page back.
                     (niwa-view--call-preserving-window-state
                      (lambda ()
                        (setq rendered (niwa-view-table-rerender window))
                        (niwa-view--rerender-images window))
                      t))
                 (restore-buffer-modified-p modified))
               (when rendered
                 (niwa-view--realign-on-window-change window)))))))))))

(defun niwa-view--realign-on-window-change (&optional window &rest _)
  "Schedule idle formatting after display, scrolling, commands or resize.
Coalesce work into one timer.  Each table yields for another 250 ms of idle
before the next pass; an idle timer deadline is absolute within its idle period.
Do not wake the editor when the visible layout is already current."
  (niwa-view--cancel-realign-timer)
  (when-let* ((window (if (and (window-live-p window)
                              (eq (window-buffer window) (current-buffer)))
                         window
                       (get-buffer-window (current-buffer) t)))
              ((or (not (eql (window-body-width window t)
                             niwa-view--image-layout-width))
                   (niwa-view-table--visible-stale window))))
    (setq niwa-view--realign-timer
          (run-with-idle-timer
           (+ 0.25 (if-let* ((idle (current-idle-time))) (float-time idle) 0))
           nil #'niwa-view--realign-markdown (current-buffer) window))))

(defun niwa-view--cancel-realign-timer ()
  "Cancel any pending re-layout timer for the current buffer."
  (when (timerp niwa-view--realign-timer)
    (cancel-timer niwa-view--realign-timer))
  (setq niwa-view--realign-timer nil))

(defun niwa-view--enable-markdown-realign ()
  "Keep the current buffer's rendered tables and images window-aligned.
The handlers are buffer-local, so they run only for this buffer's
windows and disappear with the buffer; the pending timer is cancelled
on kill so no timer outlives its view."
  (add-hook 'window-size-change-functions
            #'niwa-view--realign-on-window-change nil t)
  (add-hook 'window-buffer-change-functions
            #'niwa-view--realign-on-window-change nil t)
  (add-hook 'window-scroll-functions #'niwa-view--realign-on-window-change nil t)
  (add-hook 'post-command-hook #'niwa-view--realign-on-window-change nil t)
  (add-hook 'change-major-mode-hook #'niwa-view--cancel-realign-timer nil t)
  (add-hook 'kill-buffer-hook #'niwa-view--cancel-realign-timer nil t))


;;
;;; Copying rendered Markdown

(defun niwa-view--buffer-substring-filter (beg end &optional delete)
  "Return BEG..END with rendered tables restored to copyable text.
A table the region covers completely yields its pipe-table source, so
pastes stay valid Markdown.  A partly covered table yields the selected
cell text without borders.  With DELETE non-nil the region is also
deleted, matching `buffer-substring--filter'."
  (let ((start (min beg end))
        (finish (max beg end)))
    (if (not (text-property-not-all
              start finish 'niwa-view-table-source nil))
        ;; No rendered table in the region: keep the stock filter's
        ;; behavior for every ordinary copy and kill.
        (buffer-substring--filter beg end delete)
      (let ((parts nil)
            (pos start))
        (while (< pos finish)
          (let ((source (get-text-property pos 'niwa-view-table-source))
                (next (or (next-single-property-change
                           pos 'niwa-view-table-source nil finish)
                          finish)))
            (push (cond
                   ((not source) (buffer-substring pos next))
                   ((and (or (= pos (point-min))
                             (not (eq source (get-text-property
                                              (1- pos)
                                              'niwa-view-table-source))))
                         (or (= next (point-max))
                             (not (eq source (get-text-property
                                              next
                                              'niwa-view-table-source)))))
                    (substring-no-properties source))
                   (t (niwa-view-table-plain-text pos next)))
                  parts)
            (setq pos next)))
        (prog1 (apply #'concat (nreverse parts))
          (when delete (delete-region start finish)))))))

(provide 'niwa-view-markdown)

;;; niwa-view-markdown.el ends here
