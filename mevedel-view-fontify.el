;;; mevedel-view-fontify.el --- View text fontification -*- lexical-binding: t -*-

;;; Commentary:

;; Owns quiet major-mode setup, generic view-text fontification, and the
;; reusable Markdown fontification buffer.

;;; Code:

;; `markdown-ts-mode'
(declare-function markdown-ts-mode "ext:markdown-ts-mode" ())
(defvar markdown-ts-enable-code-block-context-mode)
(defvar markdown-ts-enable-table-mode)
(defvar markdown-ts-hide-markup)

;; `mevedel-view-render'
(defvar mevedel-view-hide-markdown-markup)

;; `org'
(defvar org-mode-hook)


;;
;;; Mode setup

(defmacro mevedel-view--with-quiet-mode-setup (&rest body)
  "Run BODY with user mode hooks and mode chatter suppressed.
BODY must not change buffers: `delay-mode-hooks\=' makes its flag
buffer-local before binding it, so the suppression covers only the buffer
current on entry.  Use `mevedel-view--with-render-temp-buffer\=' to set up a
mode in a fresh buffer."
  (declare (indent 0) (debug t))
  ;; Modes chatter while they set themselves up (`sh-mode' announces its
  ;; indentation setup, `python-mode' guesses its offset).  Rendering must
  ;; not push that into the echo area.
  ;;
  ;; `hack-local-variables-hook' is deliberately not bound here.  No body
  ;; visits a file, so the hook never runs, while several mode bodies
  ;; (`sh-mode', `bash-ts-mode', `sql-mode') register on it buffer-locally.
  ;; Binding it made every one of those calls print "Making
  ;; hack-local-variables-hook buffer-local while locally let-bound!", which
  ;; on a transcript full of shell blocks is thousands of lines of noise.
  `(let ((change-major-mode-after-body-hook nil)
         (after-change-major-mode-hook nil)
         (enable-local-variables nil)
         (font-lock-mode-hook nil)
         (inhibit-message t)
         (org-mode-hook nil))
     (delay-mode-hooks
       ,@body)))

(defmacro mevedel-view--with-render-temp-buffer (&rest body)
  "Run BODY in a temporary buffer with user mode hooks suppressed."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (mevedel-view--with-quiet-mode-setup
       ,@body)))

(defun mevedel-view--promote-face-to-font-lock-face (s)
  "Rename `face' text properties on S to `font-lock-face' in place.
`text-mode' (and most other major modes) enable `font-lock-mode'
through `global-font-lock-mode'.  Font-lock's unfontify pass strips
the `face' property from any region it touches, which would wipe
out the faces pre-applied to view text.  `font-lock-face' survives
unfontify and is rendered identically in font-lock-enabled buffers,
so promoting the property keeps highlighting through font-lock
refontification cycles.  Returns S."
  (let ((pos 0)
        (end (length s)))
    (while (< pos end)
      (let* ((next (or (next-single-property-change pos 'face s) end))
             (face (get-text-property pos 'face s)))
        (when face
          (remove-text-properties pos next '(face nil) s)
          (put-text-property pos next 'font-lock-face face s))
        (setq pos next)))
    s))


;;
;;; Markdown target

(defun mevedel-view--markdown-grammars-ready-p ()
  "Return non-nil when both Markdown tree-sitter grammars are installed.
`markdown-ts-mode\=' calls `treesit-ensure-installed\=', which offers to clone
and compile a missing grammar.  A render must never raise that prompt, so
availability is checked before the mode is ever invoked."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'markdown)
       (treesit-language-available-p 'markdown-inline)))

(defun mevedel-view--markdown-fontify-mode ()
  "Return the Markdown major mode to fontify view text with, or nil.
Emacs 31.1 ships `markdown-ts-mode\=', so mevedel needs no Markdown package.
Returns nil when its grammars are missing, which leaves view text as plain
unfontified Markdown."
  (and (fboundp 'markdown-ts-mode)
       (mevedel-view--markdown-grammars-ready-p)
       'markdown-ts-mode))

(defvar mevedel-view--markdown-fontify-buffer nil
  "Reusable buffer whose major mode fontifies Markdown view text.
`markdown-ts-mode\=' setup costs about 4.4ms -- two parsers, range rules
for the embedded grammars, `outline-minor-mode\=', and a `jit-lock\='
registration -- against roughly 0.1ms to fontify a typical response
segment.  A fresh temp buffer per call would pay that setup on every
streaming redraw, so the buffer and its mode are set up once and only the
content is swapped.")

(defun mevedel-view--markdown-fontify-target ()
  "Return the live reusable Markdown fontification buffer, or nil.
Returns nil when no Markdown mode is available."
  (if (buffer-live-p mevedel-view--markdown-fontify-buffer)
      mevedel-view--markdown-fontify-buffer
    (when-let* ((mode (mevedel-view--markdown-fontify-mode)))
      (setq mevedel-view--markdown-fontify-buffer
            (with-current-buffer
                (get-buffer-create " *mevedel-markdown-fontify*" t)
              (mevedel-view--with-quiet-mode-setup
                ;; Table and code-block context modes only add commands and
                ;; keys; a buffer nobody visits needs neither, and the
                ;; latter clones regions into indirect buffers.
                (let ((markdown-ts-enable-table-mode nil)
                      (markdown-ts-enable-code-block-context-mode nil))
                  (funcall mode)))
              ;; Read by the font-lock rules that put the `invisible'
              ;; property on markup, so it must be set before fontifying,
              ;; not just before the mode realizes its invisibility spec.
              (setq-local markdown-ts-hide-markup
                          mevedel-view-hide-markdown-markup)
              ;; The mode installs its own `font-lock-defaults' after
              ;; `outline-minor-mode' may already have locked in the
              ;; parent's; clearing the flag lets the real ones take.
              (setq font-lock-set-defaults nil)
              ;; `markdown-ts-mode' registers jit-lock.  The buffer is never
              ;; displayed, so only a stealth pass could touch it.
              (setq-local jit-lock-stealth-time nil)
              (buffer-disable-undo)
              (current-buffer))))))

(defun mevedel-view--release-markdown-fontify-buffer ()
  "Kill the reusable Markdown fontification buffer."
  (when (buffer-live-p mevedel-view--markdown-fontify-buffer)
    (kill-buffer mevedel-view--markdown-fontify-buffer))
  (setq mevedel-view--markdown-fontify-buffer nil))


;;
;;; Fontification

(defun mevedel-view--fontify-as (text mode)
  "Return TEXT fontified as if displayed in MODE.
MODE is a major-mode symbol.  Unknown or nil MODE returns TEXT verbatim.
`markdown-mode' is a tag rather than a mode to call: it routes to
`mevedel-view--markdown-fontify-mode' in the reusable buffer that mode was
set up in.  Any other MODE uses a throwaway temp buffer with mode hooks and
local variables disabled, and `font-lock-ensure' to force a full pass.
Faces are promoted to `font-lock-face' so they survive the view
buffer's font-lock refontification cycles."
  (condition-case _
      (cond
       ;; `markdown-mode' is the tag for "this body is Markdown", never a
       ;; mode to call: which mode renders Markdown is
       ;; `mevedel-view--markdown-fontify-mode's decision.
       ((eq mode 'markdown-mode)
        (if-let* ((buffer (mevedel-view--markdown-fontify-target)))
            (mevedel-view--promote-face-to-font-lock-face
             (with-current-buffer buffer
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert text)
                 (font-lock-ensure)
                 (buffer-string))))
          text))
       ((or (null mode)
            (memq mode '(text-mode fundamental-mode))
            (not (fboundp mode)))
        text)
       (t
        (mevedel-view--promote-face-to-font-lock-face
         (mevedel-view--with-render-temp-buffer
           (insert text)
           (funcall mode)
           ;; A mode that installs its `font-lock-defaults' after something
           ;; in its body has already called `font-lock-set-defaults' leaves
           ;; the buffer wired to the stale defaults and fontifies nothing.
           (setq font-lock-set-defaults nil)
           (font-lock-ensure)
           (buffer-string)))))
    (error text)))

(provide 'mevedel-view-fontify)
;;; mevedel-view-fontify.el ends here
