;;; mevedel-transcript-restore.el --- Restore transcript properties -*- lexical-binding: t -*-

;;; Commentary:

;; Restores persisted gptel text properties, then delegates structural
;; recovery to the canonical transcript grammar.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `gptel'
(declare-function gptel--restore-props "ext:gptel" (bounds-alist))

;; `mevedel-transcript'
(declare-function mevedel-transcript-normalize-properties
                  "mevedel-transcript" ())
(declare-function mevedel-transcript--skip-leading-properties-drawer
                  "mevedel-transcript" (pos))
(declare-function mevedel-transcript--skip-leading-summary-block
                  "mevedel-transcript" (pos))

;; `org'
(declare-function org-entry-delete "ext:org" (pom property))
(declare-function org-entry-get
                  "ext:org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "ext:org" (pom property value))


;;
;;; Persisted bounds

(defun mevedel-transcript-restore--sanitize-bounds-once ()
  "Run one `GPTEL_BOUNDS' sanitation pass.
Return non-nil when the property drawer changed."
  (when-let* ((raw (org-entry-get (point-min) "GPTEL_BOUNDS")))
    (let* ((invalid (make-symbol "invalid"))
           (bounds (condition-case nil
                       (read raw)
                     (error invalid)))
           changed)
      (if (or (eq bounds invalid)
              (not (listp bounds)))
          (progn
            (org-entry-delete (point-min) "GPTEL_BOUNDS")
            (setq changed t))
        (cl-labels
            ((sanitize-range (range)
               (when (and (consp range)
                          (integerp (car range))
                          (integerp (cadr range)))
                 (let ((start (max (point-min)
                                   (min (car range) (point-max))))
                       (end (max (point-min)
                                 (min (cadr range) (point-max)))))
                   (when (< start end)
                     (append (list start end) (cddr range))))))
             (sanitize-entry (entry)
               (when (consp entry)
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
                  (org-entry-put (point-min) "GPTEL_BOUNDS"
                                 (prin1-to-string sanitized))
                (org-entry-delete (point-min) "GPTEL_BOUNDS"))
              (setq changed t)))))
      changed)))

(defun mevedel-transcript-restore-sanitize-bounds ()
  "Clamp malformed top-level `GPTEL_BOUNDS' ranges to the current buffer.

Rewriting the Org property drawer can move every stored position, so
repeat until its serialized bounds settle.  Derived repairs do not mark
the buffer modified."
  (when (derived-mode-p 'org-mode)
    (require 'org)
    (let ((was-modified (buffer-modified-p))
          (changed nil)
          (again t)
          (attempts 0))
      (unwind-protect
          (while (and again (< attempts 8))
            (setq attempts (1+ attempts)
                  again (mevedel-transcript-restore--sanitize-bounds-once)
                  changed (or changed again)))
        (set-buffer-modified-p was-modified))
      changed)))

(defun mevedel-transcript-restore-properties-present-p (start end)
  "Return non-nil when START..END contain a `gptel' text property."
  (let ((pos start)
        found)
    (while (and (< pos end) (not found))
      (when (get-text-property pos 'gptel)
        (setq found t))
      (setq pos (or (next-single-property-change pos 'gptel nil end)
                    end)))
    found))

(defun mevedel-transcript-restore-properties (&optional only-if-missing)
  "Restore and normalize transcript properties in the current Org buffer.

When ONLY-IF-MISSING is non-nil, do not overwrite existing live
properties with persisted bounds.  Canonical structural normalization
still runs whenever transcript properties are present."
  (when (derived-mode-p 'org-mode)
    (require 'gptel)
    (require 'mevedel-transcript)
    (let* ((scan-start
            (mevedel-transcript--skip-leading-summary-block
             (mevedel-transcript--skip-leading-properties-drawer
              (point-min))))
           (inhibit-read-only t)
           (present
            (mevedel-transcript-restore-properties-present-p
             scan-start (point-max))))
      (when (and (org-entry-get (point-min) "GPTEL_BOUNDS")
                 (or (not only-if-missing) (not present)))
        (mevedel-transcript-restore-sanitize-bounds)
        (when-let* ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
          (condition-case err
              (gptel--restore-props (read bounds))
            (error
             (display-warning
              'mevedel
              (format "Could not restore transcript GPTEL_BOUNDS: %s"
                      (error-message-string err)))))))
      (when (mevedel-transcript-restore-properties-present-p
             scan-start (point-max))
        (mevedel-transcript-normalize-properties)))))

(provide 'mevedel-transcript-restore)
;;; mevedel-transcript-restore.el ends here
