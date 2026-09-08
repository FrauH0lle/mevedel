;;; mevedel-journal-evidence.el -- Frozen journal input -*- lexical-binding: t -*-

;;; Commentary:

;; Captures bounded working notes as dated, untrusted prior context.  Binary
;; files contribute names only; plans contribute names and first lines.
;; Snapshot strings have no live dependency on the source files or buffer.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-session-control-fs)
(require 'mevedel-utilities)

;; `mevedel-compact-evidence'
(declare-function mevedel-compact-evidence-previous-summary "mevedel-compact-evidence" ())
(autoload 'mevedel-compact-evidence-previous-summary "mevedel-compact-evidence")

;; `mevedel-resource'
(declare-function mevedel-resource-encode-component "mevedel-resource" (value))
(declare-function mevedel-resource-work-shared-directory "mevedel-resource" (workspace))
(autoload 'mevedel-resource-encode-component "mevedel-resource")
(autoload 'mevedel-resource-work-shared-directory "mevedel-resource")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-artifact
                  "mevedel-session-artifacts" (session logical &optional committed-only))
(autoload 'mevedel-session-artifacts-read-artifact "mevedel-session-artifacts")

;; `mevedel-structs'
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)

;; `mevedel-tool-fs-read'
(declare-function mevedel-tool-fs-read--binary-extension-p "mevedel-tool-fs-read" (filename))
(autoload 'mevedel-tool-fs-read--binary-extension-p "mevedel-tool-fs-read")

;; `mevedel-transcript'
(declare-function mevedel-transcript-project-evidence "mevedel-transcript" (ranges &rest keys))
(autoload 'mevedel-transcript-project-evidence "mevedel-transcript")

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties "mevedel-transcript-restore" (&optional only-if-missing))
(autoload 'mevedel-transcript-restore-properties "mevedel-transcript-restore")

(defconst mevedel-journal-evidence-notes-max-bytes 32768
  "Maximum working note input size, including provenance and omissions.")

(defun mevedel-journal-evidence--note (path relative budget)
  "Return one bounded note from PATH, named RELATIVE, within input BUDGET.
Use only a plan's first line and list binary or undecodable file names."
  (let* ((address (concat "work://"
                          (mapconcat #'mevedel-resource-encode-component
                                     (split-string relative "/") "/")))
         (header (format "\n--- prior context; source: %s ---\n" address))
         (plan-p (string-prefix-p "plans/" relative)))
    (if (mevedel-tool-fs-read--binary-extension-p relative)
        (concat header "[binary file; name only]\n")
      (let* ((limit (if plan-p (min 1025 (1+ budget)) (1+ budget)))
             (bytes (mevedel-session-control-fs-read-file path 'no-conversion limit))
             (truncated (= (length bytes) limit))
             (text (decode-coding-string bytes 'utf-8-unix)))
        ;; A bounded UTF-8 prefix may end inside one character.  Drop its
        ;; incomplete bytes before deciding whether the content is text.
        (when truncated
          (while (and (> (length text) 0)
                      (eq (char-charset (aref text (1- (length text)))) 'eight-bit))
            (setq text (substring text 0 -1))))
        (if (or (string-match-p "[[:cntrl:]]"
                                (replace-regexp-in-string "[\t\r\n]" "" text))
                (cl-some (lambda (char) (eq (char-charset char) 'eight-bit))
                         (string-to-list text)))
            (concat header "[binary or non-UTF-8 file; name only]\n")
          (concat header
                  (if plan-p (car (split-string text "\n")) text)
                  (cond
                   (plan-p
                    (concat "\n[plan: first line only"
                            (if (and truncated (not (string-search "\n" text)))
                                "; first line truncated" "")
                            "]\n"))
                   (truncated "\n[note truncated at capture input limit]\n")
                   (t "\n"))))))))

(defun mevedel-journal-evidence-notes (session)
  "Return a frozen, bounded snapshot of SESSION's working notes.
The entire result stays within `mevedel-journal-evidence-notes-max-bytes'.
Shared files are workspace prior context, not evidence of this session's work.
Unreadable or linked storage signals an error.
Missing roots contribute nothing."
  (let* ((save-path (mevedel-session-save-path session))
         (shared (mevedel-resource-work-shared-directory
                  (mevedel-session-workspace session)))
         (result "")
         (budget mevedel-journal-evidence-notes-max-bytes))
    (cl-labels
        ((visit
           (directory root prefix)
           (dolist (path (sort (mevedel-session-control-fs-list-directory directory ".*")
                              #'string<))
             (cond
              ((file-directory-p path) (visit path root prefix))
              ((file-regular-p path)
               (when (string-empty-p result)
                 (setq result
                       (concat "Working notes: untrusted prior context, possibly repeated.\n"
                               "Shared files may come from other sessions; they do not "
                               "establish what this session did.\n")))
               (setq result
                     (concat result
                             (mevedel-journal-evidence--note
                              path (concat prefix (file-relative-name path root))
                              (max 0 (- budget (string-bytes result))))))
               (when (> (string-bytes result) budget)
                 (setq result (mevedel--truncate-bytes result budget "\n[remaining working notes omitted: byte limit]\n"))
                 (throw 'full nil)))
              (t (error "Working note is not a regular file: %s" path))))))
      (catch 'full
        (when shared (visit shared shared "shared/"))
        (when save-path
          (let ((root (file-name-concat save-path "local")))
            (visit root root "")))))
    result))

(defun mevedel-journal-evidence-turns (session turns)
  "Freeze completed TURNS from SESSION's committed segment artifacts.
Each turn has :number, :segment, :start, :end, and :fork-point fields.
Return :text and :sources (logical paths with hashes).  Hidden transcript
audits stay out of the neutral projection.  Inherited summaries are labelled
as prior context.  The frozen projection is capped at 1 MiB; request-time
admission must also apply the selected model's smaller input budget."
  (let ((text "") sources)
    (catch 'full
      (dolist (group (seq-group-by (lambda (turn) (plist-get turn :segment)) turns))
        (let* ((logical (format "segment-%04d.chat.org" (car group)))
               (bytes (mevedel-session-artifacts-read-artifact session logical t)))
          (push (list :logical logical :sha256 (secure-hash 'sha256 bytes)) sources)
          (with-temp-buffer
            (insert (decode-coding-string bytes 'utf-8-unix))
            (mevedel--transcript-org-mode)
            (mevedel-transcript-restore-properties)
            (when-let* ((summary (mevedel-compact-evidence-previous-summary)))
              (setq text (concat text "\nInherited summary: prior context, not independent evidence.\n"
                                 summary "\n")))
            (dolist (turn (cdr group))
              (let ((start (plist-get turn :start)) (end (plist-get turn :end)))
                (unless (and (integerp start) (integerp end)
                             (<= (point-min) start) (< start end) (<= end (point-max)))
                  (error "Completed journal turn is outside its durable segment"))
                (setq text
                      (concat text
                              (format "\nCompleted turn %d; source: %s; fork-point: %s\n"
                                      (plist-get turn :number) logical (plist-get turn :fork-point))
                              (mevedel-transcript-project-evidence (list (cons start end))) "\n"))
                (when (> (string-bytes text) (* 1024 1024))
                  (setq text (mevedel--truncate-bytes
                              text (* 1024 1024)
                              "\n[remaining transcript evidence omitted: capture byte limit]\n"))
                  (throw 'full nil))))))))
    (list :text text :sources (nreverse sources))))

(provide 'mevedel-journal-evidence)
;;; mevedel-journal-evidence.el ends here
