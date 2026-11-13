;;; mevedel-journal-evidence.el -- Frozen journal input -*- lexical-binding: t -*-

;;; Commentary:

;; Captures bounded session working notes as dated, untrusted prior context.
;; Binary files contribute names only; plans contribute names and first lines.
;; Shared `work://shared/' notes are not journal input: memory consolidation
;; reviews them directly and then cleans them up.
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
(autoload 'mevedel-resource-encode-component "mevedel-resource")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-artifact
                  "mevedel-session-artifacts" (session logical &optional committed-only))
(autoload 'mevedel-session-artifacts-read-artifact "mevedel-session-artifacts")

;; `mevedel-structs'
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)

;; `mevedel-tool-fs-read'
(declare-function mevedel-tool-fs-read--binary-extension-p "mevedel-tool-fs-read" (filename))
(autoload 'mevedel-tool-fs-read--binary-extension-p "mevedel-tool-fs-read")

;; `mevedel-transcript'
(declare-function mevedel-transcript-project-segments "mevedel-transcript" (segments &rest keys))
(declare-function mevedel-transcript-segments "mevedel-transcript" (&optional beg end))
(autoload 'mevedel-transcript-project-segments "mevedel-transcript")
(autoload 'mevedel-transcript-segments "mevedel-transcript")

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties "mevedel-transcript-restore" (&optional only-if-missing))
(autoload 'mevedel-transcript-restore-properties "mevedel-transcript-restore")

(defconst mevedel-journal-evidence-notes-max-bytes 32768
  "Maximum working note input size, including provenance and omissions.")

(defconst mevedel-journal-evidence-digest-max-bytes (* 128 1024)
  "Maximum frozen evidence bytes admitted to a journal digest request.")

(defun mevedel-journal-evidence-digest-source (source &optional budget)
  "Return a compact digest view of frozen untrusted SOURCE.
Keep the opening context and closing conclusions when the source exceeds
the byte BUDGET (default `mevedel-journal-evidence-digest-max-bytes').
Mark the missing middle explicitly; never interpret evidence delimiters
in untrusted text as syntax.
The original capture remains intact for inspection and recovery."
  (let* ((source (substring-no-properties source))
         (budget (or budget mevedel-journal-evidence-digest-max-bytes))
         (marker "\n[omitted completed-work evidence: digest byte limit; middle of capture unavailable]\n")
         (half (/ (- budget (string-bytes marker)) 2)))
    (if (<= (string-bytes source) budget)
        source
      (concat (mevedel--truncate-bytes source half "")
              marker
              (reverse (mevedel--truncate-bytes (reverse source) half ""))))))

(defun mevedel-journal-evidence--turn (start end)
  "Return dialogue and supporting evidence for the completed START to END span.
Classify canonical transcript segments, not delimiters in untrusted text.
Keep user and assistant prose separately so tool volume cannot displace it.
Supporting evidence keeps bounded tool results and agent messages; reminders,
reasoning and hidden audits are represented by an explicit omission notice."
  (let (dialogue supporting)
    (dolist (span (mevedel-transcript-segments start end))
      (let ((segment (list (car span) (max start (cadr span)) (min end (caddr span)))))
        (pcase (car segment)
          ((or 'user 'prompt 'response) (push segment dialogue))
          ((or 'tool 'mailbox 'hook-context 'task-background)
           (push (mevedel--truncate-bytes
                  (mevedel-transcript-project-segments (list segment) :tool-output-max 512)
                  2048 "\n[supporting evidence truncated: digest item limit]\n")
                 supporting)))))
    (cons (mevedel-transcript-project-segments (nreverse dialogue))
          (mapconcat #'identity (nreverse supporting) "\n"))))

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
                         text))
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
  "Return a frozen, bounded snapshot of SESSION's own working notes.
The entire result stays within `mevedel-journal-evidence-notes-max-bytes'.
Shared working files are excluded; consolidation reviews them directly.
Unreadable or linked storage signals an error.
Missing roots contribute nothing."
  (let* ((save-path (mevedel-session-save-path session))
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
                       "Working notes: untrusted prior context, possibly repeated.\n"))
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
        (when save-path
          (let ((root (file-name-concat save-path "local")))
            (visit root root "")))))
    result))

(defun mevedel-journal-evidence-turns (session turns)
  "Freeze completed TURNS from SESSION's committed segment artifacts.
Each turn has :number, :segment, :start, :end, and :fork-point fields.
Return :text, :sources (logical paths with hashes), and :unavailable, the
numbers of turns whose indexed bounds lie outside their committed segment.
Such a turn is named in the text as unavailable rather than failing the
capture: its bytes are gone from the artifact, and one damaged historical
turn otherwise blocked every later capture of the session.  Hidden
transcript audits, reminders and reasoning stay out of the projection.
Inherited summaries are labelled as prior context.  Dialogue has a 64 KiB
allowance and supporting evidence a separate 32 KiB allowance; omissions
are explicit.  Request-time admission also applies the model input budget."
  (let ((text "") (supporting "") sources unavailable)
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
              (if (not (and (integerp start) (integerp end)
                            (<= (point-min) start) (< start end) (<= end (point-max))))
                  (progn
                    (push (plist-get turn :number) unavailable)
                    (setq text
                          (concat text
                                  (format "\nCompleted turn %d; source: %s: evidence unavailable, its indexed bounds lie outside the committed segment\n"
                                          (plist-get turn :number) logical))))
                (let ((label (format "\nCompleted turn %d; source: %s; fork-point: %s\n"
                                     (plist-get turn :number) logical (plist-get turn :fork-point)))
                      (projection (mevedel-journal-evidence--turn start end)))
                  (setq text (concat text label (car projection) "\n")
                        supporting (concat supporting label (cdr projection) "\n")))))))))
    (list :text
          (concat
           (mevedel-journal-evidence-digest-source text (* 64 1024))
           "\nSupporting evidence (reminders, reasoning and hidden audits omitted):\n"
           (mevedel--truncate-bytes
            supporting (- (* 32 1024) 128)
            "\n[remaining supporting evidence omitted: digest capture limit]\n"))
          :sources (nreverse sources) :unavailable (nreverse unavailable))))

(provide 'mevedel-journal-evidence)
;;; mevedel-journal-evidence.el ends here
