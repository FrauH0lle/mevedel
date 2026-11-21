;;; niwa-journal-pins.el -- Retain journal source evidence -*- lexical-binding: t -*-

;;; Commentary:

;; Pending journal captures pin their source session and immutable publication
;; heads.  Pins live beside the source, so session cleanup does not need to
;; discover or open a different workspace's journal.  A capture owner releases
;; its pin only after publication or explicit discard.  Disabled journaling
;; leaves these records intact.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-journal-store)
(require 'niwa-session-control-fs)

(defconst niwa-journal-pins--name-regexp
  (concat "\\`" niwa-journal-store-hash-regexp "\\.json\\'")
  "Exact capture pin filename spelling.")

(defconst niwa-journal-pins--head-regexp
  "\\`\\.publications/generation-[0-9a-f]\\{20\\}/manifest\\.el\\'"
  "Exact immutable publication head accepted by an evidence pin.")

(defun niwa-journal-pins--directory (session-dir)
  "Return the private evidence pin directory for SESSION-DIR."
  (file-name-concat
   (niwa-session-control-fs-physical-path session-dir) ".journal-pins"))

(defun niwa-journal-pins--path (session-dir capture-id)
  "Return the pin path for CAPTURE-ID below SESSION-DIR."
  (unless (niwa-journal-store-id-p capture-id)
    (error "Invalid journal capture identity"))
  (file-name-concat (niwa-journal-pins--directory session-dir)
                    (concat capture-id ".json")))

(defun niwa-journal-pins--parse (path bytes)
  "Return validated publication heads retained by pin BYTES read from PATH.
Malformed pins signal an error so collection cannot silently delete evidence."
  (let* ((object (json-parse-string bytes))
         (id (and (hash-table-p object) (gethash "capture-id" object)))
         (heads (and (hash-table-p object) (gethash "heads" object))))
    (unless (and (hash-table-p object) (= 2 (hash-table-count object))
                 (stringp id)
                 (equal (concat id ".json") (file-name-nondirectory path))
                 (vectorp heads)
                 (cl-every (lambda (head)
                             (and (stringp head)
                                  (string-match-p niwa-journal-pins--head-regexp head)))
                           heads))
      (error "Invalid journal evidence pin: %s" path))
    (append heads nil)))

(defun niwa-journal-pins--read (path)
  "Return validated publication heads retained by the pin at PATH."
  (niwa-journal-pins--parse path (niwa-session-control-fs-read-file path)))

(defun niwa-journal-pins-retain (session-dir capture-id heads)
  "Pin source SESSION-DIR and immutable HEADS for CAPTURE-ID.
HEADS may be nil for a file session whose evidence is captured separately.
The caller must own the source session's mutation authority and have frozen
its capture descriptor before retaining the source.  Repeating the same pin
is harmless; a conflicting pin is never replaced."
  (unless (and (proper-list-p heads)
               (cl-every (lambda (head)
                           (and (stringp head)
                                (string-match-p niwa-journal-pins--head-regexp head)))
                         heads))
    (error "Invalid journal source publication heads"))
  (let ((path (niwa-journal-pins--path session-dir capture-id)))
    (niwa-session-control-fs-make-directory (file-name-directory path) t)
    (unless (niwa-session-control-fs-create-or-verify
             path (json-serialize (list :capture-id capture-id :heads (vconcat heads))))
      (error "Conflicting journal evidence pin"))
    capture-id))

(defun niwa-journal-pins-release (session-dir capture-id)
  "Release CAPTURE-ID's source pin below SESSION-DIR.
Only completed publication or explicit discard may release a capture pin."
  (condition-case nil
      (niwa-session-control-fs-delete-file
       (niwa-journal-pins--path session-dir capture-id))
    (niwa-session-control-fs-absent nil)))

(defun niwa-journal-pins-heads (session-dir)
  "Return all immutable publication heads pinned below SESSION-DIR.
An unreadable pin is an error, never an empty pin set."
  (car (niwa-journal-pins-observe session-dir)))

(defun niwa-journal-pins-observe (session-dir)
  "Return (HEADS . PROOF) for the pins below SESSION-DIR.
HEADS are as `niwa-journal-pins-heads' returns them.  PROOF is a list of
control-program operations that fail unless the pin directory still holds
exactly the observed entries with the observed bytes, so a later program
that deletes publications can make the unchanged pin set its precondition."
  (let* ((directory (niwa-journal-pins--directory session-dir))
         (entries (niwa-session-control-fs-list-directory directory "."))
         heads proof)
    (dolist (path entries)
      (when (string-match-p niwa-journal-pins--name-regexp
                            (file-name-nondirectory path))
        (condition-case nil
            (let ((bytes (niwa-session-control-fs-read-file path)))
              (setq heads (append (niwa-journal-pins--parse path bytes) heads))
              (push (list :op 'verify :path path :content bytes) proof))
          ;; A concurrent completed capture may release its own pin.
          (niwa-session-control-fs-absent
           (setq entries (delete path entries))))))
    (cons (delete-dups heads)
          (cons (niwa-session-control-fs-listing-proof directory entries)
                (nreverse proof)))))

(defun niwa-journal-pins-present-p (session-dir)
  "Return non-nil if SESSION-DIR has capture pins, including malformed pins.
Session cleanup must preserve even a pin that needs explicit recovery."
  (and (niwa-session-control-fs-list-directory
        (niwa-journal-pins--directory session-dir)
        niwa-journal-pins--name-regexp)
       t))

(provide 'niwa-journal-pins)
;;; niwa-journal-pins.el ends here
