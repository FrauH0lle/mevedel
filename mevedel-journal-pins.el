;;; mevedel-journal-pins.el -- Retain journal source evidence -*- lexical-binding: t -*-

;;; Commentary:

;; Pending journal captures pin their source session and immutable publication
;; heads.  Pins live beside the source, so session cleanup does not need to
;; discover or open a different workspace's journal.  A capture owner releases
;; its pin only after publication or explicit discard.  Disabled journaling
;; leaves these records intact.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-store)
(require 'mevedel-session-control-fs)

(defconst mevedel-journal-pins--name-regexp
  (concat "\\`" mevedel-journal-store-hash-regexp "\\.json\\'")
  "Exact capture pin filename spelling.")

(defconst mevedel-journal-pins--head-regexp
  "\\`\\.publications/generation-[0-9a-f]\\{20\\}/manifest\\.el\\'"
  "Exact immutable publication head accepted by an evidence pin.")

(defun mevedel-journal-pins--directory (session-dir)
  "Return the private evidence pin directory for SESSION-DIR."
  (file-name-concat
   (mevedel-session-control-fs-physical-path session-dir) ".journal-pins"))

(defun mevedel-journal-pins--path (session-dir capture-id)
  "Return the pin path for CAPTURE-ID below SESSION-DIR."
  (unless (mevedel-journal-store-id-p capture-id)
    (error "Invalid journal capture identity"))
  (file-name-concat (mevedel-journal-pins--directory session-dir)
                    (concat capture-id ".json")))

(defun mevedel-journal-pins--read (path)
  "Return validated publication heads retained by the pin at PATH.
Malformed pins signal an error so collection cannot silently delete evidence."
  (let* ((object (json-parse-string
                  (mevedel-session-control-fs-read-file path)))
         (id (and (hash-table-p object) (gethash "capture-id" object)))
         (heads (and (hash-table-p object) (gethash "heads" object))))
    (unless (and (hash-table-p object) (= 2 (hash-table-count object))
                 (stringp id)
                 (equal (concat id ".json") (file-name-nondirectory path))
                 (vectorp heads)
                 (cl-every (lambda (head)
                             (and (stringp head)
                                  (string-match-p mevedel-journal-pins--head-regexp head)))
                           heads))
      (error "Invalid journal evidence pin: %s" path))
    (append heads nil)))

(defun mevedel-journal-pins-retain (session-dir capture-id heads)
  "Pin source SESSION-DIR and immutable HEADS for CAPTURE-ID.
HEADS may be nil for a file session whose evidence is captured separately.
The caller must own the source session's mutation authority and have frozen
its capture descriptor before retaining the source.  Repeating the same pin
is harmless; a conflicting pin is never replaced."
  (unless (and (proper-list-p heads)
               (cl-every (lambda (head)
                           (and (stringp head)
                                (string-match-p mevedel-journal-pins--head-regexp head)))
                         heads))
    (error "Invalid journal source publication heads"))
  (let ((path (mevedel-journal-pins--path session-dir capture-id)))
    (mevedel-session-control-fs-make-directory (file-name-directory path) t)
    (unless (mevedel-session-control-fs-create-or-verify
             path (json-serialize (list :capture-id capture-id :heads (vconcat heads))))
      (error "Conflicting journal evidence pin"))
    capture-id))

(defun mevedel-journal-pins-release (session-dir capture-id)
  "Release CAPTURE-ID's source pin below SESSION-DIR.
Only completed publication or explicit discard may release a capture pin."
  (condition-case nil
      (mevedel-session-control-fs-delete-file
       (mevedel-journal-pins--path session-dir capture-id))
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-journal-pins-heads (session-dir)
  "Return all immutable publication heads pinned below SESSION-DIR.
An unreadable pin is an error, never an empty pin set."
  (let (heads)
    (dolist (path (mevedel-session-control-fs-list-directory
                  (mevedel-journal-pins--directory session-dir)
                  mevedel-journal-pins--name-regexp))
      (condition-case nil
          (setq heads (append (mevedel-journal-pins--read path) heads))
        ;; A concurrent completed capture may release its own pin.
        (mevedel-session-control-fs-absent nil)))
    (delete-dups heads)))

(defun mevedel-journal-pins-present-p (session-dir)
  "Return non-nil if SESSION-DIR has capture pins, including malformed pins.
Session cleanup must preserve even a pin that needs explicit recovery."
  (and (mevedel-session-control-fs-list-directory
        (mevedel-journal-pins--directory session-dir)
        mevedel-journal-pins--name-regexp)
       t))

(provide 'mevedel-journal-pins)
;;; mevedel-journal-pins.el ends here
