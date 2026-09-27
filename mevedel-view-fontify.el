;;; mevedel-view-fontify.el --- View text fontification -*- lexical-binding: t -*-

;;; Commentary:

;; Owns quiet major-mode setup, generic view-text fontification, and the
;; reusable Markdown fontification buffer.

;;; Code:

(eval-when-compile (require 'cl-lib))

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
  ;; `diff-mode' installs a local `font-lock-mode-hook'.  Suppress the
  ;; default hook without let-binding it in this buffer, so the mode can
  ;; register its own hook without the same local-binding warning.
  `(cl-letf (((default-value 'font-lock-mode-hook) nil))
     (let ((change-major-mode-after-body-hook nil)
           (after-change-major-mode-hook nil)
           (enable-local-variables nil)
           (inhibit-message t)
           (org-mode-hook nil))
       (delay-mode-hooks
         ,@body))))

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

(defvar mevedel-view--markdown-fontify-active-buffers nil
  "Dynamically bound buffers owned by active Markdown fontification calls.
This spans mode setup as well as fontification, across all view buffers.")

(defun mevedel-view--markdown-fontify-setup (buffer mode)
  "Set up BUFFER to fontify Markdown using MODE."
  (with-current-buffer buffer
    (mevedel-view--with-quiet-mode-setup
      ;; Table and code-block context modes only add commands and keys; a
      ;; buffer nobody visits needs neither, and the latter clones regions.
      (let ((markdown-ts-enable-table-mode nil)
            (markdown-ts-enable-code-block-context-mode nil))
        (funcall mode)))
    (setq-local markdown-ts-hide-markup mevedel-view-hide-markdown-markup)
    ;; Some modes install defaults after another mode has locked in stale ones.
    (setq font-lock-set-defaults nil)
    (setq-local jit-lock-stealth-time nil)
    (buffer-disable-undo)))

(defun mevedel-view--markdown-fontify-target ()
  "Return a Markdown fontification buffer, or nil when no mode is available.
Ordinary calls reuse one buffer.  Nested calls receive a fresh buffer that
`mevedel-view--fontify-as' owns and kills on return."
  (if (and (null mevedel-view--markdown-fontify-active-buffers)
           (buffer-live-p mevedel-view--markdown-fontify-buffer))
      mevedel-view--markdown-fontify-buffer
    (when-let* ((mode (mevedel-view--markdown-fontify-mode)))
      (let ((buffer (generate-new-buffer " *mevedel-markdown-fontify*" t))
            initialized)
        (unless mevedel-view--markdown-fontify-active-buffers
          (setq mevedel-view--markdown-fontify-buffer buffer))
        (unwind-protect
            (let ((mevedel-view--markdown-fontify-active-buffers
                   (cons buffer mevedel-view--markdown-fontify-active-buffers)))
              (mevedel-view--markdown-fontify-setup buffer mode)
              (setq initialized t)
              buffer)
          (unless initialized
            (when (eq buffer mevedel-view--markdown-fontify-buffer)
              (setq mevedel-view--markdown-fontify-buffer nil))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(defun mevedel-view--release-markdown-fontify-buffer ()
  "Invalidate the reusable Markdown fontification buffer.
An active owner finishes reading it before killing it on return."
  (let ((buffer mevedel-view--markdown-fontify-buffer))
    (setq mevedel-view--markdown-fontify-buffer nil)
    (when (and (buffer-live-p buffer)
               (not (memq buffer mevedel-view--markdown-fontify-active-buffers)))
      (kill-buffer buffer))))


;;
;;; Incremental Markdown fontification

(cl-defstruct (mevedel-view--markdown-fontify-job
               (:constructor mevedel-view--markdown-fontify-job--create))
  "A whole-response Markdown buffer fontified in bounded regions."
  text buffer next output done cancelled)

(defun mevedel-view--markdown-fontify-job-start (text)
  "Start fontifying complete Markdown response TEXT in a private buffer.
The text, including a final newline sentinel, is inserted once.  A private
buffer keeps its syntax context intact between steps and cannot be overwritten
by unrelated synchronous Markdown rendering.  If no mode is available or
setup fails, the job completes with the original TEXT."
  (let ((job (mevedel-view--markdown-fontify-job--create :text text))
        buffer)
    (condition-case nil
        (if-let* ((mode (mevedel-view--markdown-fontify-mode)))
            (progn
              (setq buffer (generate-new-buffer " *mevedel-markdown-fontify-job*" t))
              (let (prepared)
                (unwind-protect
                    (let ((mevedel-view--markdown-fontify-active-buffers
                           (cons buffer mevedel-view--markdown-fontify-active-buffers)))
                      (mevedel-view--markdown-fontify-setup buffer mode)
                      (with-current-buffer buffer
                        (insert text "\n"))
                      (setf (mevedel-view--markdown-fontify-job-buffer job) buffer
                            (mevedel-view--markdown-fontify-job-next job) 1)
                      (setq prepared t))
                  (unless prepared
                    (when (buffer-live-p buffer) (kill-buffer buffer))))))
          (setf (mevedel-view--markdown-fontify-job-output job) text
                (mevedel-view--markdown-fontify-job-done job) t))
      (error
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (setf (mevedel-view--markdown-fontify-job-output job) text
             (mevedel-view--markdown-fontify-job-done job) t)))
    job))

(defun mevedel-view--markdown-fontify-job-cancel (job)
  "Cancel JOB and release its private fontification buffer.
It is safe to cancel a completed or already cancelled job."
  (when-let* ((buffer (mevedel-view--markdown-fontify-job-buffer job)))
    (when (buffer-live-p buffer) (kill-buffer buffer)))
  (setf (mevedel-view--markdown-fontify-job-buffer job) nil
        (mevedel-view--markdown-fontify-job-text job) nil
        (mevedel-view--markdown-fontify-job-output job) nil
        (mevedel-view--markdown-fontify-job-cancelled job) t))

(defun mevedel-view--markdown-fontify-job-step (job chunk-size)
  "Fontify at most CHUNK-SIZE characters plus one line in JOB.
Return non-nil when the complete response is ready.  A single long line
remains indivisible.  Syntax parsing always sees the complete response;
only the font-lock region is divided.  On font-lock failure return the
original text, as `mevedel-view--fontify-as' does.  JOB must not be cancelled."
  (when (mevedel-view--markdown-fontify-job-cancelled job)
    (error "Markdown fontification job was cancelled"))
  (unless (and (integerp chunk-size) (> chunk-size 0))
    (error "Invalid Markdown fontification chunk size: %S" chunk-size))
  (unless (mevedel-view--markdown-fontify-job-done job)
    (let ((buffer (mevedel-view--markdown-fontify-job-buffer job))
          returned)
      (unwind-protect
          (progn
            (condition-case nil
                (with-current-buffer buffer
                  (let* ((start (mevedel-view--markdown-fontify-job-next job))
                         ;; Align to the end of the line containing the target,
                         ;; including its newline when present.
                         (end (save-excursion
                                (goto-char (min (point-max) (+ start chunk-size)))
                                (min (point-max) (1+ (line-end-position))))))
                    (font-lock-ensure start end)
                    (setf (mevedel-view--markdown-fontify-job-next job) end)
                    (when (= end (point-max))
                      (setf (mevedel-view--markdown-fontify-job-done job) t))))
              (error
               (setf (mevedel-view--markdown-fontify-job-output job)
                     (mevedel-view--markdown-fontify-job-text job)
                     (mevedel-view--markdown-fontify-job-done job) t)))
            (when (mevedel-view--markdown-fontify-job-output job)
              (when (buffer-live-p buffer) (kill-buffer buffer))
              (setf (mevedel-view--markdown-fontify-job-buffer job) nil
                    (mevedel-view--markdown-fontify-job-text job) nil))
            (setq returned t))
        (unless returned
          (mevedel-view--markdown-fontify-job-cancel job)))))
  (mevedel-view--markdown-fontify-job-done job))

(defun mevedel-view--markdown-fontify-job-result (job)
  "Return JOB's complete propertized text, or nil if still pending.
Copying the completed response is deferred until this call; it is not part of
the bounded fontification step.  The result remains available until
`mevedel-view--markdown-fontify-job-cancel'."
  (when (mevedel-view--markdown-fontify-job-done job)
    (when-let* ((buffer (mevedel-view--markdown-fontify-job-buffer job)))
      (unwind-protect
          (setf (mevedel-view--markdown-fontify-job-output job)
                (condition-case nil
                    (if (buffer-live-p buffer)
                        (mevedel-view--promote-face-to-font-lock-face
                         (with-current-buffer buffer
                           (buffer-substring (point-min) (1- (point-max)))))
                      (mevedel-view--markdown-fontify-job-text job))
                  (error (mevedel-view--markdown-fontify-job-text job))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (setf (mevedel-view--markdown-fontify-job-buffer job) nil
              (mevedel-view--markdown-fontify-job-text job) nil)))
    (mevedel-view--markdown-fontify-job-output job)))


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
            (let ((mevedel-view--markdown-fontify-active-buffers
                   (cons buffer mevedel-view--markdown-fontify-active-buffers)))
              (unwind-protect
                  (mevedel-view--promote-face-to-font-lock-face
                   (with-current-buffer buffer
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       ;; Markdown heading faces assume a terminating newline.
                       ;; Keep that sentinel out of the returned view text.
                       (insert text "\n")
                       (font-lock-ensure)
                       (buffer-substring (point-min) (1- (point-max))))))
                ;; Nested buffers and an invalidated reusable buffer belong
                ;; only to this call, including on error or nonlocal exit.
                (when (and (not (eq buffer mevedel-view--markdown-fontify-buffer))
                           (buffer-live-p buffer))
                  (kill-buffer buffer))))
          text))
       ((or (null mode)
            (memq mode '(text-mode fundamental-mode))
            (not (fboundp mode)))
        text)
       (t
        (mevedel-view--promote-face-to-font-lock-face
         (mevedel-view--with-render-temp-buffer
           ;; Some modes (notably `diff-mode') require a terminating
           ;; newline to fontify the last line.  Exclude it from the result.
           (insert text "\n")
           (funcall mode)
           ;; A mode that installs its `font-lock-defaults' after something
           ;; in its body has already called `font-lock-set-defaults' leaves
           ;; the buffer wired to the stale defaults and fontifies nothing.
           (setq font-lock-set-defaults nil)
           (font-lock-ensure)
           (buffer-substring (point-min) (1- (point-max)))))))
    (error text)))

(provide 'mevedel-view-fontify)
;;; mevedel-view-fontify.el ends here
