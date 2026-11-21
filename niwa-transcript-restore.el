;;; niwa-transcript-restore.el --- Restore transcript properties -*- lexical-binding: t -*-

;;; Commentary:

;; Restores persisted gptel text properties, then delegates structural
;; recovery to the canonical transcript grammar.

;;; Code:

(require 'niwa-utilities)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'gptel)
(require 'niwa-transcript)

;; `gptel'
(declare-function gptel--restore-props "ext:gptel" (bounds-alist))
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel-mode)

;; `niwa-transcript'
(declare-function niwa-transcript-normalize-properties
                  "niwa-transcript" ())
(declare-function niwa-transcript--skip-leading-properties-drawer
                  "niwa-transcript" (pos))
(declare-function niwa-transcript--skip-leading-summary-block
                  "niwa-transcript" (pos))

;; `org'
(declare-function org-entry-delete "ext:org" (pom property))
(declare-function org-entry-get
                  "ext:org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "ext:org" (pom property value))


;;
;;; Persisted bounds

(defun niwa-transcript-restore--sanitize-bounds-once ()
  "Run one `GPTEL_BOUNDS' sanitation pass.
Return non-nil when the property drawer changed."
  (when-let* ((raw (org-entry-get (point-min) "GPTEL_BOUNDS")))
    (let* ((invalid (make-symbol "invalid"))
           (bounds (condition-case nil
                       (read raw)
                     (error invalid)))
           changed)
      (if (or (eq bounds invalid)
              (not (proper-list-p bounds)))
          (progn
            (org-entry-delete (point-min) "GPTEL_BOUNDS")
            (setq changed t))
        (cl-labels
            ((sanitize-range (range)
               (when (and (consp range)
                          (proper-list-p range)
                          (integerp (car range))
                          (integerp (cadr range)))
                 (let ((start (max (point-min)
                                   (min (car range) (point-max))))
                       (end (max (point-min)
                                 (min (cadr range) (point-max)))))
                   (when (< start end)
                     (append (list start end) (cddr range))))))
             (sanitize-entry (entry)
               (when (and (consp entry)
                          (proper-list-p entry)
                          (symbolp (car entry)))
                 (let (ranges)
                   (dolist (range (cdr entry))
                     (when-let* ((sanitized (sanitize-range range)))
                       (push sanitized ranges)))
                   (when ranges
                     (cons (car entry) (nreverse ranges)))))))
          (let (sanitized)
            (dolist (entry bounds)
              (when-let* ((sanitized-entry (sanitize-entry entry)))
                (push sanitized-entry sanitized)))
            (setq sanitized (nreverse sanitized))
            (unless (equal sanitized bounds)
              (if sanitized
                  (let ((print-length nil)
                        (print-level nil)
                        (markers
                         (mapcar
                          (lambda (entry)
                            (cons (car entry)
                                  (mapcar
                                   (lambda (range)
                                     (append
                                      (list (copy-marker (car range))
                                            (copy-marker (cadr range)))
                                      (cddr range)))
                                   (cdr entry))))
                          sanitized))
                        last
                        (attempts 0))
                    ;; Drawer rewrites move transcript positions.  Keep the
                    ;; repaired ranges anchored until serialization settles.
                    (unwind-protect
                        (while (< attempts 8)
                          (setq attempts (1+ attempts))
                          (let ((serialized
                                 (prin1-to-string
                                  (mapcar
                                   (lambda (entry)
                                     (cons (car entry)
                                           (mapcar
                                            (lambda (range)
                                              (append
                                               (list (marker-position (car range))
                                                     (marker-position (cadr range)))
                                               (cddr range)))
                                            (cdr entry))))
                                   markers))))
                            (if (equal serialized last)
                                (setq attempts 8)
                              (setq last serialized)
                              (org-entry-put (point-min) "GPTEL_BOUNDS"
                                             serialized))))
                      (dolist (entry markers)
                        (dolist (range (cdr entry))
                          (set-marker (car range) nil)
                          (set-marker (cadr range) nil)))))
                (org-entry-delete (point-min) "GPTEL_BOUNDS"))
              (setq changed t)))))
      changed)))

(defun niwa-transcript-restore-sanitize-bounds ()
  "Clamp malformed top-level `GPTEL_BOUNDS' ranges to the current buffer.

Rewriting the Org property drawer can move every stored position, so
repeat until its serialized bounds settle.  Derived repairs do not mark
the buffer modified."
  (when (derived-mode-p 'org-mode)
    (let ((was-modified (buffer-modified-p))
          (changed nil)
          (again t)
          (attempts 0))
      (unwind-protect
          (while (and again (< attempts 8))
            (setq attempts (1+ attempts)
                  again (niwa-transcript-restore--sanitize-bounds-once)
                  changed (or changed again)))
        (set-buffer-modified-p was-modified))
      changed)))

(defun niwa-transcript-restore-properties-present-p (start end)
  "Return non-nil when START..END contain a `gptel' text property."
  (and (< start end)
       (not (null (text-property-not-all start end 'gptel nil)))))

(defun niwa-transcript-restore-properties (&optional only-if-missing)
  "Restore and normalize transcript properties in the current Org buffer.

When ONLY-IF-MISSING is non-nil, do not overwrite existing live
properties with persisted bounds.  Canonical structural normalization
still runs whenever transcript properties are present."
  (when (derived-mode-p 'org-mode)
    (let* ((scan-start
            (niwa-transcript--skip-leading-summary-block
             (niwa-transcript--skip-leading-properties-drawer
              (point-min))))
           (inhibit-read-only t)
           (present
            (niwa-transcript-restore-properties-present-p
             scan-start (point-max))))
      (when (and (org-entry-get (point-min) "GPTEL_BOUNDS")
                 (or (not only-if-missing) (not present)))
        (niwa-transcript-restore-sanitize-bounds)
        ;; Sanitation can shrink the drawer past the old scan start.  Use
        ;; the repaired body's position for the normalization check below.
        (setq scan-start
              (niwa-transcript--skip-leading-summary-block
               (niwa-transcript--skip-leading-properties-drawer
                (point-min))))
        (when-let* ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
          (condition-case err
              (gptel--restore-props (read bounds))
            (error
             (niwa-diagnostic-warning
              (niwa-diagnostic-scope (current-buffer)) 'niwa
              (format "Could not restore transcript GPTEL_BOUNDS: %s"
                      (error-message-string err)))))))
      (when (niwa-transcript-restore-properties-present-p
             scan-start (point-max))
        (niwa-transcript-normalize-properties)))))

(defun niwa-transcript-enable-gptel-mode ()
  "Enable `gptel-mode' with gptel's own state restoration disabled.

Niwa transcripts restore their bounds through
`niwa-transcript-restore-properties', and request configuration
comes from the session sidecar and agent registry, never from Org
properties -- whose top-level persisted copies niwa strips.  Letting
gptel's restore run on a file-visiting transcript would only duplicate
bounds restoration and complain about the missing backend."
  (unless (bound-and-true-p gptel-mode)
    (cl-letf (((symbol-function 'gptel--restore-state) #'ignore))
      (gptel-mode +1))
    ;; gptel's Org branch registers a JIT annotator, which switches
    ;; `jit-lock-mode' back on.  Its after-change hook silently writes
    ;; `fontified' and would invalidate transcript change proofs.  Undo that
    ;; registration and keep storage unfontified; the user's global font-lock
    ;; hooks must not run for a transcript.
    (jit-lock-unregister 'gptel-org--annotate-links)
    (let ((font-lock-mode-hook nil))
      (font-lock-mode -1)
      (jit-lock-mode nil))))

(defun niwa-transcript-restore-gptel-state ()
  "Restore gptel state without dirtying the current transcript buffer."
  (let ((was-modified (buffer-modified-p)))
    (unwind-protect
        (progn
          (niwa-transcript-restore-sanitize-bounds)
          (niwa-transcript-enable-gptel-mode)
          (niwa-transcript-restore-properties t))
      (set-buffer-modified-p was-modified))))

(provide 'niwa-transcript-restore)
;;; niwa-transcript-restore.el ends here
