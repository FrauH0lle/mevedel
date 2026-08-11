;;; mevedel-tool-patch.el -- Patch-oriented file mutation -*- lexical-binding: t -*-

;;; Commentary:

;; Implements the ApplyPatch tool.  One Codex-style patch proposal may add,
;; update, delete, or move multiple files.  Parsing and filesystem validation
;; happen before any write; selected changes are then committed as one
;; rollback-backed transaction.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry)
  (require 'subr-x))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))

(defvar mevedel-tool-patch--prepared-proposal nil)

;; `mevedel-file-state'
(declare-function mevedel-session-record-file-access
                  "mevedel-file-state" (session path kind &optional offset limit))

;; `mevedel-patch-review'
(declare-function mevedel-patch-review-start
                  "mevedel-patch-review" (proposal callback data-buffer))

;; `mevedel-permissions'
(defvar mevedel-permission-mode)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-canonical-path
                  "mevedel-pipeline" (path))
(defvar mevedel-pipeline--auto-apply-edit-p)

;; `mevedel-resource'
(declare-function mevedel-resource-address-like-p
                  "mevedel-resource" (value))
(declare-function mevedel-resource-execute
                  "mevedel-resource" (attempt &optional executor options))
(declare-function mevedel-resource-prepare
                  "mevedel-resource" (operation address context))

;; `mevedel-reminders'
(declare-function mevedel-reminders-diagnostics-after-edit
                  "mevedel-reminders" (buffer path continuation))
(declare-function mevedel-reminders-diagnostics-before-edit
                  "mevedel-reminders" (buffer path))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--shallow-ensure-files
                  "mevedel-session-persistence" (session buffer))

;; `mevedel-structs'
(declare-function mevedel-request-ephemeral-p "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-one-shot-mutations-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)

(defconst mevedel-tool-patch--begin "*** Begin Patch")
(defconst mevedel-tool-patch--end "*** End Patch")
(defconst mevedel-tool-patch--add "*** Add File: ")
(defconst mevedel-tool-patch--delete "*** Delete File: ")
(defconst mevedel-tool-patch--update "*** Update File: ")
(defconst mevedel-tool-patch--move "*** Move to: ")
(defconst mevedel-tool-patch--eof "*** End of File")


(defun mevedel-tool-patch--resource-address-p (value)
  "Return non-nil when VALUE is a resource-address-shaped operand."
  (and (stringp value)
       (require 'mevedel-resource)
       (mevedel-resource-address-like-p value)))

(defun mevedel-tool-patch--physical-path (operation)
  "Return the resolved filesystem path for OPERATION's source."
  (or (plist-get operation :physical-path)
      (plist-get operation :path)))

(defun mevedel-tool-patch--physical-move-path (operation)
  "Return the resolved filesystem path for OPERATION's destination."
  (or (plist-get operation :physical-move-path)
      (plist-get operation :move-path)))

(defun mevedel-tool-patch--local-source-p (operation)
  "Return non-nil when OPERATION's source is session-local."
  (plist-get operation :local-path-p))

(defun mevedel-tool-patch--local-destination-p (operation)
  "Return non-nil when OPERATION's destination is session-local."
  (plist-get operation :local-move-path-p))

(defun mevedel-tool-patch--sanitize-error (message proposal)
  "Replace private local paths in MESSAGE with authored operands.
PROPOSAL is the prepared proposal whose physical fields are private."
  (dolist (operation (and proposal (plist-get proposal :operations)) message)
    (dolist (entry '((:physical-path :rel-path :local-path-p)
                     (:physical-move-path :move-rel-path
                                          :local-move-path-p)))
      (let ((physical (plist-get operation (nth 0 entry)))
            (authored (plist-get operation (nth 1 entry)))
            (local-p (plist-get operation (nth 2 entry))))
        (when (and local-p physical authored)
          (setq message
                (replace-regexp-in-string
                 (regexp-quote physical) authored message t t)))))))

(defun mevedel-tool-patch--ensure-local-session ()
  "Return the current durable session for a local ApplyPatch operand."
  (let ((session (and (boundp 'mevedel--session) mevedel--session))
        (request (and (boundp 'mevedel--current-request)
                      mevedel--current-request)))
    (unless session
      (error "Local resources require a session"))
    (when (and request (mevedel-request-ephemeral-p request))
      (error "Ephemeral requests cannot own local resources"))
    (unless (mevedel-session-save-path session)
      (require 'mevedel-session-persistence)
      (unless (mevedel-session-persistence--shallow-ensure-files
               session (current-buffer))
        (error "Could not materialize the local resource session")))
    (make-directory
     (file-name-concat (mevedel-session-save-path session) "local") t)
    session))

(defun mevedel-tool-patch--prepare-resources (proposal &optional materialize)
  "Prepare resource operands in PROPOSAL once.

The authored operands stay in the regular operation fields.  Resolved
filesystem targets are kept in private fields consumed by the existing patch
transaction, so review and result data continue to use authored addresses.
When MATERIALIZE is non-nil, establish the durable local-resource directory
and resolve its physical targets.  Preparation used by the pipeline leaves
local sessions untouched until permission and plan checks have completed."
  (let (operands local-p session (local-only-p t))
    (dolist (operation (plist-get proposal :operations))
      (dolist (address (delq nil (list (plist-get operation :rel-path)
                                       (plist-get operation :move-rel-path))))
        (unless (and (string-prefix-p "local://" address)
                     (> (length address) (length "local://")))
          (setq local-only-p nil)))
      (dolist (entry '((:rel-path :physical-path :resource-attempt-path
                        :local-path-p)
                       (:move-rel-path :physical-move-path
                                       :resource-attempt-move-path
                                       :local-move-path-p)))
        (let ((address (plist-get operation (nth 0 entry))))
          (when (mevedel-tool-patch--resource-address-p address)
            (when (string-prefix-p "local://" address)
              (setq local-p t))
            (push (list operation address (nth 1 entry) (nth 2 entry)
                        (nth 3 entry))
                  operands)))))
    (unless (and operands local-only-p)
      (setq local-only-p nil))
    (setq session (and (boundp 'mevedel--session) mevedel--session))
    (when (and local-p materialize)
      (setq session (mevedel-tool-patch--ensure-local-session)))
    (when operands
      (require 'mevedel-resource))
    (dolist (entry operands)
      (pcase-let ((`(,operation ,address ,physical-key ,attempt-key ,local-key)
                   entry))
        (let* ((attempt
                (mevedel-resource-prepare
                 'apply-patch address
                 (list :session session
                       :workspace (and session
                                       (mevedel-session-workspace session)))))
               physical)
          (plist-put operation attempt-key attempt)
          (when materialize
            (mevedel-resource-execute
             attempt (lambda (path _authored) (setq physical path)))
            (unless physical
              (error "Resource has no writable filesystem target: %s" address))
            (plist-put operation physical-key physical)
            (plist-put operation attempt-key nil))
          (let ((local-p (string-prefix-p "local://" address)))
            (plist-put operation local-key local-p)
            (plist-put operation :local-resource-p
                       (or (plist-get operation :local-resource-p)
                           local-p))))))
    (plist-put proposal :local-only-p local-only-p)))

(defun mevedel-tool-patch--materialize-resources (proposal)
  "Materialize prepared resource attempts in PROPOSAL after authorization."
  (let (local-p)
    (dolist (operation (plist-get proposal :operations))
      (when (or (plist-get operation :local-path-p)
                (plist-get operation :local-move-path-p))
        (setq local-p t)))
    (when local-p
      (mevedel-tool-patch--ensure-local-session))
    (dolist (operation (plist-get proposal :operations))
      (dolist (entry '((:resource-attempt-path :physical-path)
                       (:resource-attempt-move-path :physical-move-path)))
        (when-let* ((attempt (plist-get operation (car entry))))
          (let (physical)
            (mevedel-resource-execute
             attempt (lambda (path _authored) (setq physical path)))
            (unless physical
              (error "Resource has no writable filesystem target"))
            (plist-put operation (nth 1 entry) physical)
            (plist-put operation (car entry) nil)))))
    proposal))

(defun mevedel-tool-patch--operation-marker-p (line)
  "Return non-nil when LINE begins a patch file operation."
  (or (string-prefix-p mevedel-tool-patch--add line)
      (string-prefix-p mevedel-tool-patch--delete line)
      (string-prefix-p mevedel-tool-patch--update line)))

(defun mevedel-tool-patch--marker-path (line marker line-number)
  "Return the path following MARKER in LINE at LINE-NUMBER."
  (let ((path (substring line (length marker))))
    (when (string-empty-p path)
      (error "Invalid patch at line %d: Missing file path" line-number))
    path))

(defun mevedel-tool-patch--parse-update-lines (lines first-line)
  "Parse update payload LINES beginning at FIRST-LINE into hunks."
  (let (hunks current changed-p)
    (cl-labels
        ((new-hunk (context)
           (list :context context :old-lines nil :new-lines nil
                 :diff-lines nil :eof nil :selected t))
         (ensure-current ()
           (unless current
             (setq current (new-hunk nil))))
         (finish-current ()
           (when current
             (unless changed-p
               (error "Invalid patch at line %d: Update hunk has no changes"
                      first-line))
             (setq current
                   (plist-put current :old-lines
                              (nreverse (plist-get current :old-lines))))
             (setq current
                   (plist-put current :new-lines
                              (nreverse (plist-get current :new-lines))))
             (setq current
                   (plist-put current :diff-lines
                              (nreverse (plist-get current :diff-lines))))
             (push current hunks)
             (setq current nil changed-p nil))))
      (cl-loop for line in lines
               for line-number from first-line
               ;; Codex matches update markers after stripping trailing
               ;; whitespace; content lines stay raw.
               for marker = (string-trim-right line)
               do
               (cond
                ((or (string= marker "@@")
                     (string-prefix-p "@@ " marker))
                 (finish-current)
                 (setq current
                       (new-hunk (unless (string= marker "@@")
                                   (substring marker 3)))))
                ((string= marker mevedel-tool-patch--eof)
                 (ensure-current)
                 (setq current (plist-put current :eof t)))
                ((string-empty-p line)
                 ;; Codex leniency: a bare empty line is an empty context
                 ;; line whose space marker the model dropped.
                 (ensure-current)
                 (push " " (plist-get current :diff-lines))
                 (push "" (plist-get current :old-lines))
                 (push "" (plist-get current :new-lines)))
                ((memq (aref line 0) '(?\s ?+ ?-))
                 (ensure-current)
                 (let ((text (substring line 1)))
                   (push line (plist-get current :diff-lines))
                   (pcase (aref line 0)
                     (?\s
                      (push text (plist-get current :old-lines))
                      (push text (plist-get current :new-lines)))
                     (?+
                      (push text (plist-get current :new-lines))
                      (setq changed-p t))
                     (?-
                      (push text (plist-get current :old-lines))
                      (setq changed-p t)))))
                (t
                 (error "Invalid patch at line %d: Invalid update line"
                        line-number))))
      (finish-current))
    (nreverse hunks)))

(defun mevedel-tool-patch--parse (patch &optional root)
  "Parse PATCH relative to ROOT and return a patch proposal plist.
Like Codex, structural markers are matched after trimming surrounding
whitespace, one `*** Environment ID:' line after the opening marker is
tolerated, and content lines are kept raw."
  (unless (stringp patch)
    (error "Parameter patch is required"))
  (require 'mevedel-execution-target)
  (let* ((root (file-name-as-directory (expand-file-name (or root default-directory))))
         (target
          (or (and (boundp 'mevedel--session)
                   mevedel--session
                   (mevedel-session-execution-target mevedel--session))
              (mevedel-execution-target-create root)))
         (resolve
          (lambda (path)
            (if (mevedel-tool-patch--resource-address-p path)
                path
              (mevedel-pipeline-canonical-path
               (mevedel-execution-target-expand-path target path root)))))
         (normalized (replace-regexp-in-string "\r\n?" "\n" patch))
         (lines (split-string (string-trim normalized) "\n" nil))
         (count (length lines))
         (index 1)
         operations)
    (unless (and (>= count 3)
                 (string= (string-trim (car lines))
                          mevedel-tool-patch--begin))
      (error "Invalid patch: The first line must be '%s'"
             mevedel-tool-patch--begin))
    (unless (string= (string-trim (car (last lines)))
                     mevedel-tool-patch--end)
      (error "Invalid patch: The last line must be '%s'"
             mevedel-tool-patch--end))
    (when (string-prefix-p "*** Environment ID: "
                           (string-trim (nth 1 lines)))
      (setq index 2))
    (while (< index (1- count))
      (let* ((line (string-trim (nth index lines)))
             (line-number (1+ index)))
        (cond
         ((string-prefix-p mevedel-tool-patch--add line)
          (let ((rel-path (mevedel-tool-patch--marker-path
                           line mevedel-tool-patch--add line-number))
                added)
            (setq index (1+ index))
            (while (and (< index (1- count))
                        (not (mevedel-tool-patch--operation-marker-p
                              (string-trim (nth index lines)))))
              (let ((added-line (nth index lines)))
                (unless (string-prefix-p "+" added-line)
                  (error "Invalid patch at line %d: Add lines must start with '+'"
                         (1+ index)))
                (push (substring added-line 1) added))
              (setq index (1+ index)))
            (unless added
              (error "Invalid patch at line %d: Add File requires content"
                     line-number))
            (push (list :kind 'add :path (funcall resolve rel-path)
                        :rel-path rel-path
                        :content (concat (string-join (nreverse added) "\n") "\n")
                        :selected t)
                  operations)))
         ((string-prefix-p mevedel-tool-patch--delete line)
          (let ((rel-path (mevedel-tool-patch--marker-path
                           line mevedel-tool-patch--delete line-number)))
            (push (list :kind 'delete :path (funcall resolve rel-path)
                        :rel-path rel-path :selected t)
                  operations)
            (setq index (1+ index))))
         ((string-prefix-p mevedel-tool-patch--update line)
          (let* ((rel-path (mevedel-tool-patch--marker-path
                            line mevedel-tool-patch--update line-number))
                 (path (funcall resolve rel-path))
                 move-rel payload)
            (setq index (1+ index))
            (when (and (< index (1- count))
                       (string-prefix-p mevedel-tool-patch--move
                                        (string-trim-right
                                         (nth index lines))))
              (setq move-rel
                    (mevedel-tool-patch--marker-path
                     (string-trim-right (nth index lines))
                     mevedel-tool-patch--move (1+ index)))
              (setq index (1+ index)))
            (let ((payload-first-line (1+ index)))
              ;; Update payload markers only tolerate trailing
              ;; whitespace, like Codex: an indented header inside an
              ;; update body stays an ordinary context line.
              (while (and (< index (1- count))
                          (not (mevedel-tool-patch--operation-marker-p
                                (string-trim-right (nth index lines)))))
                (push (nth index lines) payload)
                (setq index (1+ index)))
              (setq payload (nreverse payload))
              (when (and (null move-rel) (null payload))
                (error "Invalid patch at line %d: Update File requires a hunk"
                       line-number))
              (push (list :kind (if move-rel 'move 'update)
                          :path path :rel-path rel-path
                          :move-path (and move-rel (funcall resolve move-rel))
                          :move-rel-path move-rel
                          :hunks (and payload
                                      (mevedel-tool-patch--parse-update-lines
                                       payload payload-first-line))
                          :selected t)
                    operations))))
         (t
          (error "Invalid patch at line %d: Expected a file operation"
                 line-number)))))
    (unless operations
      (error "Invalid patch: At least one file operation is required"))
    (list :patch normalized :root root :operations (nreverse operations))))

(defun mevedel-tool-patch--read-file (path)
  "Return text contents of regular file PATH."
  (unless (file-exists-p path)
    (error "File does not exist: %s" path))
  (when (file-directory-p path)
    (error "Cannot patch a directory: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun mevedel-tool-patch--lines (text)
  "Return TEXT as LF-normalized lines, retaining a final empty line."
  (split-string (replace-regexp-in-string "\r\n?" "\n" text) "\n" nil))

(defun mevedel-tool-patch--content-lines (content)
  "Return logical diff lines for complete file CONTENT."
  (unless (string-empty-p content)
    (let ((normalized (replace-regexp-in-string "\r\n?" "\n" content)))
      (split-string (string-remove-suffix "\n" normalized) "\n" nil))))

(defconst mevedel-tool-patch--ascii-folds
  (append
   (mapcar (lambda (char) (cons char ?-))
           '(#x2010 #x2011 #x2012 #x2013 #x2014 #x2015 #x2212))
   (mapcar (lambda (char) (cons char ?'))
           '(#x2018 #x2019 #x201A #x201B))
   (mapcar (lambda (char) (cons char ?\"))
           '(#x201C #x201D #x201E #x201F))
   (mapcar (lambda (char) (cons char ?\s))
           '(#xA0 #x2002 #x2003 #x2004 #x2005 #x2006 #x2007
             #x2008 #x2009 #x200A #x202F #x205F #x3000)))
  "Typographic punctuation folded to ASCII during fuzzy hunk matching.
Dash and hyphen codepoints fold to `-', curly quotes to their ASCII
quotes, and exotic spaces to a plain space, mirroring Codex.")

(defun mevedel-tool-patch--rstrip (line)
  "Return LINE without trailing horizontal whitespace."
  (string-trim-right line "[ \t]+"))

(defun mevedel-tool-patch--normalize-line (line)
  "Return LINE trimmed, with typographic punctuation folded to ASCII."
  (string-trim
   (mapconcat (lambda (char)
                (char-to-string
                 (alist-get char mevedel-tool-patch--ascii-folds char)))
              line "")))

(defconst mevedel-tool-patch--match-passes
  (list (cons #'identity nil)
        (cons #'mevedel-tool-patch--rstrip 'trailing-whitespace)
        (cons #'string-trim 'whitespace)
        (cons #'mevedel-tool-patch--normalize-line 'punctuation))
  "Line normalizers with their pass labels, in decreasing strictness.
The label nil marks an exact match; fuzzy labels are recorded on the
hunk and surfaced in the review and the tool result.")

(defun mevedel-tool-patch--match-pass-description (pass)
  "Return the human-readable description of fuzzy match PASS."
  (pcase pass
    ('trailing-whitespace "ignoring trailing whitespace")
    ('whitespace "ignoring surrounding whitespace")
    ('punctuation "normalizing typographic punctuation")))

(defun mevedel-tool-patch--sequence-match-p (lines pattern start normalize)
  "Return non-nil when PATTERN matches LINES at START under NORMALIZE."
  (and (<= (+ start (length pattern)) (length lines))
       (cl-loop for expected in pattern
                for actual in (nthcdr start lines)
                always (string= (funcall normalize actual)
                                (funcall normalize expected)))))

(defun mevedel-tool-patch--candidate-starts
    (lines hunk normalize &optional ignore-eof)
  "Return candidate match starts for HUNK in LINES under NORMALIZE.
IGNORE-EOF drops the end-of-file anchoring requirement of an
`*** End of File' hunk."
  (let* ((pattern (plist-get hunk :old-lines))
         (context (plist-get hunk :context))
         (limit (- (length lines) (length pattern)))
         starts)
    (when (>= limit 0)
      (dotimes (start (1+ limit))
        (when (and (or (null context)
                       (cl-loop for anchor from 0 below start
                                thereis
                                (string= (funcall normalize
                                                  (nth anchor lines))
                                         (funcall normalize context))))
                   (mevedel-tool-patch--sequence-match-p
                    lines pattern start normalize)
                   (or ignore-eof
                       (not (plist-get hunk :eof))
                       (= (+ start (length pattern))
                          (if (string-empty-p (car (last lines)))
                              (1- (length lines))
                            (length lines)))))
          (push start starts))))
    (nreverse starts)))

(defun mevedel-tool-patch--match-start (lines hunk path &optional after)
  "Return the unique start for HUNK in LINES from PATH.
Matching passes decrease in strictness: exact, ignoring trailing
whitespace, ignoring surrounding whitespace, then folding typographic
punctuation to ASCII.  An `*** End of File' hunk is first anchored to
the end of LINES, then retried unanchored.  Unlike Codex, every pass
still requires a unique match; when AFTER is non-nil, an otherwise
ambiguous match is retried among the candidates at or after that line
index, since hunks arrive in file order.  The winning pass label is
recorded on HUNK as `:match-pass' (nil for an exact match)."
  (let (starts pass)
    (cl-loop
     for ignore-eof in (if (plist-get hunk :eof) '(nil t) '(nil))
     do (cl-loop for (normalize . label) in mevedel-tool-patch--match-passes
                 do (setq starts (mevedel-tool-patch--candidate-starts
                                  lines hunk normalize ignore-eof)
                          pass label)
                 until starts)
     until starts)
    (when (and after (> (length starts) 1))
      (let ((ordered (seq-filter (lambda (start) (>= start after)) starts)))
        (when (= 1 (length ordered))
          (setq starts ordered))))
    (plist-put hunk :match-pass (and starts pass))
    (pcase (length starts)
      (0 (error "Patch hunk does not match %s" path))
      (1 (car starts))
      (_ (error "Patch hunk is ambiguous in %s; provide more context" path)))))

(defun mevedel-tool-patch--section-label (lines start)
  "Return a diff-style section label for a hunk at START in LINES.
Like diff's function context: the closest line at or above START whose
first character is not whitespace or a closing bracket."
  (cl-loop for index downfrom start to 0
           for line = (nth index lines)
           when (and (> (length line) 0)
                     (not (memq (aref line 0) '(?\s ?\t ?\) ?\] ?\}))))
           return (truncate-string-to-width line 48 nil nil "…")))

(defun mevedel-tool-patch--annotate-line-numbers (proposal)
  "Attach display start lines and section labels to hunks in PROPOSAL.
Old and new file positions are computed against the captured baseline
content.  A hunk that no longer matches uniquely keeps nil positions
and renders without gutter numbers."
  (dolist (operation (plist-get proposal :operations))
    (when (memq (plist-get operation :kind) '(update move))
      (let* ((content (plist-get operation :baseline-content))
             (lines (and content (mevedel-tool-patch--lines content)))
             (offset 0)
             (after 0))
        (dolist (hunk (plist-get operation :hunks))
          (let ((start (and lines
                            (ignore-errors
                              (mevedel-tool-patch--match-start
                               lines hunk
                               (mevedel-tool-patch--physical-path operation)
                               after)))))
            (plist-put hunk :old-start (and start (1+ start)))
            (plist-put hunk :new-start (and start (+ start 1 offset)))
            (plist-put hunk :section
                       (and start
                            (mevedel-tool-patch--section-label lines start)))
            (when start
              (setq after (+ start (length (plist-get hunk :old-lines))))
              (cl-incf offset
                       (- (length (plist-get hunk :new-lines))
                          (length (plist-get hunk :old-lines)))))))))))

(defun mevedel-tool-patch--hunk-replacement (lines start hunk)
  "Return HUNK's replacement lines when matched at START in LINES.
Context lines come verbatim from LINES rather than the patch, so the
fuzzy matching passes only ever decide where the hunk lands and what
gets deleted; untouched lines stay byte-identical.  Only added lines
come from the patch."
  (let ((remaining (nthcdr start lines))
        replacement)
    (dolist (line (plist-get hunk :diff-lines))
      (pcase (aref line 0)
        (?\s (push (car remaining) replacement)
             (setq remaining (cdr remaining)))
        (?- (setq remaining (cdr remaining)))
        (?+ (push (substring line 1) replacement))))
    (nreverse replacement)))

(defun mevedel-tool-patch--apply-hunks (content hunks path)
  "Return CONTENT with selected HUNKS applied for PATH.
The file's line-ending style is preserved: content containing CRLF
line endings is written back with CRLF.  Deselected hunks still
advance the order-disambiguation cursor when they match, so a later
hunk that previewed cleanly stays unambiguous after a deselection."
  (let ((eol (if (string-search "\r\n" content) "\r\n" "\n"))
        (lines (mevedel-tool-patch--lines content))
        (after 0))
    (dolist (hunk hunks)
      (if (plist-get hunk :selected)
          (let* ((start (mevedel-tool-patch--match-start
                         lines hunk path after))
                 (old-count (length (plist-get hunk :old-lines)))
                 (replacement (mevedel-tool-patch--hunk-replacement
                               lines start hunk)))
            (setq lines
                  (append (seq-take lines start)
                          replacement
                          (nthcdr (+ start old-count) lines)))
            (setq after (+ start (length replacement))))
        (when-let* ((start (ignore-errors
                             (mevedel-tool-patch--match-start
                              lines hunk path after))))
          (setq after (+ start (length (plist-get hunk :old-lines)))))))
    (string-join lines eol)))

(defun mevedel-tool-patch--affected-paths (operation)
  "Return all filesystem paths touched by OPERATION."
  (delq nil (list (mevedel-tool-patch--physical-path operation)
                  (mevedel-tool-patch--physical-move-path operation))))

(defun mevedel-tool-patch--path-state (path)
  "Return the exact filesystem state needed to revalidate PATH."
  (cond
   ((file-symlink-p path) (list 'symlink (file-symlink-p path)))
   ((not (file-exists-p path)) '(missing))
   ((file-directory-p path) '(directory))
   (t (list 'file (mevedel-tool-patch--read-file path)))))

(defun mevedel-tool-patch--capture-baseline (proposal)
  "Capture every affected path in PROPOSAL for later stale detection."
  (let (baseline)
    (dolist (operation (plist-get proposal :operations))
      (let ((paths (mevedel-tool-patch--affected-paths operation))
            (relative (delq nil
                            (list (plist-get operation :rel-path)
                                  (plist-get operation :move-rel-path)))))
        (cl-mapc
         (lambda (path rel-path)
           (let ((state (mevedel-tool-patch--path-state path)))
             (when (and (equal path
                              (mevedel-tool-patch--physical-path operation))
                        (eq (car state) 'file))
               (plist-put operation :baseline-content (cadr state)))
             (push (list :path path :rel-path rel-path :state state)
                   baseline)))
         paths relative)))
    (plist-put proposal :baseline (nreverse baseline))))

(defun mevedel-tool-patch--selected-paths (proposal)
  "Return filesystem paths selected in PROPOSAL."
  (let (paths)
    (dolist (operation (plist-get proposal :operations))
      (when (if (eq (plist-get operation :kind) 'update)
                (cl-some (lambda (hunk) (plist-get hunk :selected))
                         (plist-get operation :hunks))
              (plist-get operation :selected))
        (setq paths (append paths
                            (mevedel-tool-patch--affected-paths operation)))))
    paths))

(defun mevedel-tool-patch--assert-baseline (proposal)
  "Signal when a selected path in PROPOSAL changed since review began."
  (dolist (path (mevedel-tool-patch--selected-paths proposal))
    (let ((entry (seq-find (lambda (item)
                             (equal path (plist-get item :path)))
                           (plist-get proposal :baseline))))
      (unless (and entry
                   (equal (plist-get entry :state)
                          (mevedel-tool-patch--path-state path)))
        (error "File changed during review: %s"
               (or (plist-get entry :rel-path) path))))))

(defun mevedel-tool-patch--validate-distinct-paths (operations)
  "Reject overlapping file paths across OPERATIONS."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (operation operations)
      (dolist (path (mevedel-tool-patch--affected-paths operation))
        ;; ponytail: repeated-path operations are intentionally rejected until
        ;; a real patch needs sequential virtual-file semantics.
        (when (gethash path seen)
          (error "Patch touches path more than once: %s" path))
        (puthash path t seen)))))

(defun mevedel-tool-patch--planned-changes (proposal)
  "Validate PROPOSAL and return its selected filesystem changes."
  (let ((operations (plist-get proposal :operations))
        changes)
    (mevedel-tool-patch--validate-distinct-paths operations)
    (dolist (operation operations)
      (pcase (plist-get operation :kind)
        ('add
         (let ((path (mevedel-tool-patch--physical-path operation)))
           (when (plist-get operation :selected)
             (when (file-exists-p path)
               (error "Cannot add existing file: %s" path))
             (push (append (list :action 'write :path path)
                           (and (mevedel-tool-patch--local-source-p
                                 operation)
                                '(:local-p t))
                           (list :content (plist-get operation :content)))
                   changes))))
        ('delete
         (let ((path (mevedel-tool-patch--physical-path operation)))
           (when (plist-get operation :selected)
             (mevedel-tool-patch--read-file path)
             (push (append (list :action 'delete :path path)
                           (and (mevedel-tool-patch--local-source-p
                                 operation)
                                '(:local-p t)))
                   changes))))
        ('update
         (let ((path (mevedel-tool-patch--physical-path operation))
               (hunks (plist-get operation :hunks)))
           ;; Pass every hunk: deselected ones advance the ordering
           ;; cursor inside `mevedel-tool-patch--apply-hunks' so the
           ;; apply-time disambiguation window matches the preview's.
           (when (cl-some (lambda (hunk) (plist-get hunk :selected)) hunks)
             (let ((content (mevedel-tool-patch--read-file path)))
               (push (append (list :action 'write :path path)
                             (and (mevedel-tool-patch--local-source-p
                                   operation)
                                  '(:local-p t))
                             (list :content
                                   (mevedel-tool-patch--apply-hunks
                                    content hunks path)))
                     changes)))))
        ('move
         (let ((path (mevedel-tool-patch--physical-path operation))
               (destination
                (mevedel-tool-patch--physical-move-path operation)))
           (when (plist-get operation :selected)
             (let ((content (mevedel-tool-patch--read-file path)))
               (when (file-exists-p destination)
                 (error "Cannot move to existing file: %s" destination))
               (push (append (list :action 'delete :path path)
                             (and (mevedel-tool-patch--local-source-p
                                   operation)
                                  '(:local-p t)))
                     changes)
               (push (append (list :action 'write :path destination)
                             (and (mevedel-tool-patch--local-destination-p
                                   operation)
                                  '(:local-p t))
                             (list :mode (file-modes path)
                                   :content
                                   (if-let* ((hunks
                                              (plist-get operation :hunks)))
                                       (mevedel-tool-patch--apply-hunks
                                        content hunks path)
                                     content)))
                     changes)))))))
    (nreverse changes)))

(defun mevedel-tool-patch--snapshot (path)
  "Return a rollback snapshot plist for PATH."
  (list :path path
        :exists (file-exists-p path)
        :content (and (file-exists-p path)
                      (mevedel-tool-patch--read-file path))
        :mode (and (file-exists-p path) (file-modes path))))

(defun mevedel-tool-patch--write-file (path content &optional mode)
  "Replace PATH with CONTENT and restore MODE when non-nil."
  (let ((directory (file-name-directory path)))
    (make-directory directory t)
    (let ((temporary (make-temp-file
                      (file-name-concat directory ".mevedel-patch-") nil)))
      (unwind-protect
          (progn
            (with-temp-file temporary (insert content))
            (when mode (set-file-modes temporary mode))
            (rename-file temporary path t))
        (when (file-exists-p temporary)
          (delete-file temporary))))))

(defun mevedel-tool-patch--restore-snapshots (snapshots)
  "Best-effort restore of filesystem SNAPSHOTS."
  (dolist (snapshot snapshots)
    (let ((path (plist-get snapshot :path)))
      (condition-case nil
          (if (plist-get snapshot :exists)
              (mevedel-tool-patch--write-file
               path (plist-get snapshot :content) (plist-get snapshot :mode))
            (when (file-exists-p path) (delete-file path)))
        (error nil)))))

(defun mevedel-tool-patch--missing-parent-directories (path)
  "Return missing parent directories for PATH, outermost first."
  (let ((directory (file-name-directory path)) missing)
    (while (and directory (not (file-exists-p directory)))
      (push directory missing)
      (setq directory
            (file-name-directory (directory-file-name directory))))
    missing))

(defun mevedel-tool-patch--commit (changes)
  "Commit CHANGES, restoring every touched file if a write fails."
  (let ((snapshots (mapcar (lambda (change)
                             (mevedel-tool-patch--snapshot
                              (plist-get change :path)))
                           changes))
        (created-directories
         (delete-dups
          (cl-mapcan
           (lambda (change)
             (when (eq (plist-get change :action) 'write)
               (mevedel-tool-patch--missing-parent-directories
                (plist-get change :path))))
           changes))))
    (condition-case err
        (dolist (change changes)
          (pcase (plist-get change :action)
            ('write
             (let* ((path (plist-get change :path))
                    (snapshot (seq-find
                               (lambda (item)
                                 (equal path (plist-get item :path)))
                               snapshots)))
               (mevedel-tool-patch--write-file
                path (plist-get change :content)
                (or (plist-get change :mode)
                    (plist-get snapshot :mode)
                    (default-file-modes)))))
            ('delete
             (delete-file (plist-get change :path)))))
      (error
       (mevedel-tool-patch--restore-snapshots snapshots)
       (dolist (directory (reverse created-directories))
         (when (and (file-directory-p directory)
                    (null (directory-files directory nil
                                           directory-files-no-dot-files-regexp)))
           (delete-directory directory)))
       (signal (car err) (cdr err))))
    (dolist (change changes)
      (when-let* ((buffer (find-buffer-visiting
                           (plist-get change :path))))
        (with-current-buffer buffer
          (if (eq (plist-get change :action) 'delete)
              (set-visited-file-modtime 0)
            (let ((source (generate-new-buffer " *mevedel patch sync*")))
              (unwind-protect
                  (progn
                    (with-current-buffer source
                      (insert (plist-get change :content)))
                    (set-visited-file-modtime)
                    (replace-buffer-contents source)
                    (set-visited-file-modtime)
                    (set-buffer-modified-p nil))
                (kill-buffer source)))))))))

(defun mevedel-tool-patch--assert-buffers-unmodified (changes)
  "Signal when a visited file in CHANGES has unsaved edits."
  (dolist (change changes)
    (when-let* ((buffer (find-buffer-visiting (plist-get change :path)))
                ((buffer-modified-p buffer)))
      (error "File has unsaved buffer changes: %s"
             (plist-get change :path)))))

(defun mevedel-tool-patch--apply (data-buffer changes continuation)
  "Apply CHANGES for DATA-BUFFER, then call CONTINUATION.
Refresh file tracking immediately and diagnostics before continuation."
  (require 'mevedel-file-state)
  (require 'mevedel-reminders)
  (mevedel-tool-patch--assert-buffers-unmodified changes)
  (dolist (change changes)
    (unless (plist-get change :local-p)
      (mevedel-reminders-diagnostics-before-edit
       data-buffer (plist-get change :path))))
  (mevedel-tool-patch--commit changes)
  (when-let* ((session (and (buffer-live-p data-buffer)
                            (buffer-local-value 'mevedel--session
                                                data-buffer))))
    (dolist (change changes)
      (unless (plist-get change :local-p)
        (ignore-errors
          (mevedel-session-record-file-access
           session (plist-get change :path) 'modify)))))
  (cl-labels
      ((finish (remaining)
         (let ((remaining
                (cl-remove-if (lambda (change) (plist-get change :local-p))
                              remaining)))
           (if remaining
               (mevedel-reminders-diagnostics-after-edit
                data-buffer (plist-get (car remaining) :path)
                (lambda () (finish (cdr remaining))))
             (funcall continuation)))))
    (finish changes)))

(defun mevedel-tool-patch--selected-count (proposal)
  "Return the number of selected user-visible changes in PROPOSAL."
  (cl-loop for operation in (plist-get proposal :operations)
           sum (if (eq (plist-get operation :kind) 'update)
                   (cl-count-if (lambda (hunk) (plist-get hunk :selected))
                                (plist-get operation :hunks))
                 (if (plist-get operation :selected) 1 0))))

(defun mevedel-tool-patch--selected-file-data (operation)
  "Return persisted per-file data for selected OPERATION changes."
  (let* ((kind (plist-get operation :kind))
         (hunks (pcase kind
                  ('update
                   (seq-filter (lambda (hunk) (plist-get hunk :selected))
                               (plist-get operation :hunks)))
                  ('move
                   (and (plist-get operation :selected)
                        (plist-get operation :hunks)))))
         (selected-p (if (eq kind 'update)
                         hunks
                       (plist-get operation :selected)))
         lines)
    (when selected-p
      (setq lines
            (pcase kind
              ('add
               (mapcar (lambda (line) (concat "+" line))
                       (mevedel-tool-patch--content-lines
                        (plist-get operation :content))))
              ('delete
               (mapcar (lambda (line) (concat "-" line))
                       (mevedel-tool-patch--content-lines
                        (plist-get operation :baseline-content))))
              ((or 'update 'move)
               (cl-mapcan
                (lambda (hunk)
                  (cons (if-let* ((context (plist-get hunk :context)))
                            (concat "@@ " context)
                          "@@")
                        (copy-sequence (plist-get hunk :diff-lines))))
                hunks))))
      (list :path (plist-get operation :rel-path)
            :move-path (plist-get operation :move-rel-path)
            :kind kind
            :added (cl-count-if (lambda (line) (string-prefix-p "+" line))
                                lines)
            :deleted (cl-count-if (lambda (line) (string-prefix-p "-" line))
                                  lines)
            :diff (string-join lines "\n")))))

(defun mevedel-tool-patch--result (proposal changes)
  "Return the aggregate handler result for PROPOSAL and CHANGES.
Rejected and feedback detail lines double as render-data notes so the
settled transcript badge stays expandable without applied diffs."
  (let (details notes)
    (cl-flet ((note (line)
                (push line details)
                (push line notes)))
      (when-let* ((feedback (plist-get proposal :feedback)))
        (note (format "Feedback (whole patch): %s" feedback)))
      (dolist (operation (plist-get proposal :operations))
        (if (eq (plist-get operation :kind) 'update)
            (cl-loop for hunk in (plist-get operation :hunks)
                     for number from 1
                     do
                     (when (plist-get hunk :modified)
                       (push (format "Modified (authoritative): %s hunk %d\n%s"
                                     (plist-get operation :rel-path) number
                                     (string-join
                                      (plist-get hunk :diff-lines) "\n"))
                             details))
                     (when (plist-get hunk :selected)
                       (push (format "Applied: %s hunk %d"
                                     (plist-get operation :rel-path) number)
                             details)
                       (when-let* ((pass (plist-get hunk :match-pass)))
                         (note (format "Fuzzy: %s hunk %d matched while %s"
                                       (plist-get operation :rel-path) number
                                       (mevedel-tool-patch--match-pass-description
                                        pass)))))
                     (unless (plist-get hunk :selected)
                       (note (format "Rejected: %s hunk %d"
                                     (plist-get operation :rel-path) number)))
                     (when-let* ((feedback (plist-get hunk :feedback)))
                       (note (format "Feedback: %s hunk %d: %s"
                                     (plist-get operation :rel-path) number
                                     feedback))))
          (if (plist-get operation :selected)
              (progn
                (push (format "Applied: %s%s"
                              (plist-get operation :rel-path)
                              (if-let* ((destination
                                         (plist-get operation
                                                    :move-rel-path)))
                                  (format " -> %s" destination)
                                ""))
                      details)
                (when-let* ((pass (cl-some
                                   (lambda (hunk)
                                     (plist-get hunk :match-pass))
                                   (plist-get operation :hunks))))
                  (note (format "Fuzzy: %s matched while %s"
                                (plist-get operation :rel-path)
                                (mevedel-tool-patch--match-pass-description
                                 pass)))))
            (note (format "Rejected: %s" (plist-get operation :rel-path)))))
        (when-let* ((feedback (plist-get operation :feedback)))
          (note (format "Feedback: %s: %s"
                        (plist-get operation :rel-path) feedback)))))
    (let ((stats (mevedel-tool-patch--proposal-stats proposal)))
      (list :result (string-join
                     (cons (if changes
                               (format "Applied patch: %d changes"
                                       (mevedel-tool-patch--selected-count
                                        proposal))
                             "Patch revision requested")
                           (nreverse details))
                     "\n")
            :render-data
            (list :kind 'patch
                  :applied (if changes (plist-get stats :selected) 0)
                  :total (plist-get stats :total)
                  :comments (plist-get stats :comments)
                  :notes (nreverse notes)
                  :files
                  (delq nil
                        (mapcar #'mevedel-tool-patch--selected-file-data
                                (plist-get proposal :operations))))))))

(defun mevedel-tool-patch--hunk-counts (hunk)
  "Return (ADDED . DELETED) line counts for HUNK."
  (let ((added 0) (deleted 0))
    (dolist (line (plist-get hunk :diff-lines))
      (pcase (aref line 0)
        (?+ (cl-incf added))
        (?- (cl-incf deleted))))
    (cons added deleted)))

(defun mevedel-tool-patch--operation-stats (operation)
  "Return selection and line-count stats for OPERATION.
The plist carries :selected and :total change counts plus :added and
:deleted line counts.  Update line counts cover selected hunks only;
whole-operation kinds always report their full size."
  (let ((selected-p (if (plist-get operation :selected) 1 0)))
    (pcase (plist-get operation :kind)
      ('add
       (list :selected selected-p :total 1
             :added (length (mevedel-tool-patch--content-lines
                             (plist-get operation :content)))
             :deleted 0))
      ('delete
       (list :selected selected-p :total 1
             :added 0
             :deleted (length (mevedel-tool-patch--content-lines
                               (or (plist-get operation :baseline-content)
                                   "")))))
      ('move
       (let ((added 0) (deleted 0))
         (dolist (hunk (plist-get operation :hunks))
           (pcase-let ((`(,a . ,d) (mevedel-tool-patch--hunk-counts hunk)))
             (cl-incf added a)
             (cl-incf deleted d)))
         (list :selected selected-p :total 1 :added added :deleted deleted)))
      ('update
       (let ((selected 0) (total 0) (added 0) (deleted 0))
         (dolist (hunk (plist-get operation :hunks))
           (cl-incf total)
           (when (plist-get hunk :selected)
             (cl-incf selected)
             (pcase-let ((`(,a . ,d) (mevedel-tool-patch--hunk-counts hunk)))
               (cl-incf added a)
               (cl-incf deleted d))))
         (list :selected selected :total total
               :added added :deleted deleted))))))

(defun mevedel-tool-patch--proposal-stats (proposal)
  "Return aggregate selection stats for PROPOSAL.
Line counts include selected changes only.  The plist carries
:selected, :total, :added, :deleted, :files-selected, and :comments."
  (let ((selected 0) (total 0) (added 0) (deleted 0)
        (files-selected 0)
        (comments (if (plist-get proposal :feedback) 1 0)))
    (dolist (operation (plist-get proposal :operations))
      (let* ((stats (mevedel-tool-patch--operation-stats operation))
             (op-selected (plist-get stats :selected)))
        (cl-incf selected op-selected)
        (cl-incf total (plist-get stats :total))
        (when (> op-selected 0)
          (cl-incf files-selected)
          (cl-incf added (plist-get stats :added))
          (cl-incf deleted (plist-get stats :deleted)))
        (cl-incf comments
                 (+ (if (plist-get operation :feedback) 1 0)
                    (cl-count-if (lambda (hunk) (plist-get hunk :feedback))
                                 (plist-get operation :hunks))))))
    (list :selected selected :total total :added added :deleted deleted
          :files-selected files-selected :comments comments)))

(defun mevedel-tool-patch--status (operation)
  "Return the display status character for OPERATION."
  (pcase (plist-get operation :kind)
    ('add "A")
    ('delete "D")
    ('move "R")
    (_ "M")))

(defun mevedel-tool-patch--kind-face (kind)
  "Return the status-letter face for operation KIND."
  (pcase kind
    ('add 'success)
    ('delete 'error)
    ('move 'font-lock-keyword-face)
    (_ 'font-lock-function-name-face)))

(defun mevedel-tool-patch--fontify-diff (diff)
  "Return DIFF fontified line-wise with the review's diff faces.
Each line's newline carries the face too, so the `:extend' background
tint reaches the window edge like in the live review.  Every line is
marked linkify-exempt: diff content is verbatim and must never grow
file buttons, rewritten links, or inline images."
  (string-remove-suffix
   "\n"
   (mapconcat
    (lambda (line)
      (let ((face (pcase (and (> (length line) 0) (aref line 0))
                    (?+ 'mevedel-patch-review-added)
                    (?- 'mevedel-patch-review-removed)
                    (?@ 'diff-hunk-header))))
        (apply #'propertize (concat line "\n")
               'mevedel-view-no-linkify t
               (and face (list 'font-lock-face face)))))
    (split-string diff "\n") "")))

(defun mevedel-tool-patch--effective-mode ()
  "Return the active permission mode for patch review."
  (or (and (boundp 'mevedel--current-request)
           mevedel--current-request
           (mevedel-request-one-shot-mutations-p mevedel--current-request)
           'ask)
      (and (boundp 'mevedel-pipeline--auto-apply-edit-p)
           mevedel-pipeline--auto-apply-edit-p
           'edits)
      (and (boundp 'mevedel--session)
           mevedel--session
           (mevedel-session-permission-mode mevedel--session))
      (and (boundp 'mevedel-permission-mode) mevedel-permission-mode)
      'ask))

(defun mevedel-tool-patch-handler (callback args)
  "Handle an ApplyPatch call with CALLBACK and ARGS."
  (let (proposal)
    (condition-case err
        (let* ((prepared mevedel-tool-patch--prepared-proposal)
               (_ (setq proposal
                        (or prepared
                            (mevedel-tool-patch--parse
                             (plist-get args :patch)))))
             (_resources (if prepared
                             (mevedel-tool-patch--materialize-resources
                              proposal)
                           (mevedel-tool-patch--prepare-resources
                            proposal t)))
             (_baseline (mevedel-tool-patch--capture-baseline proposal))
             (changes (mevedel-tool-patch--planned-changes proposal)))
        (mevedel-tool-patch--assert-baseline proposal)
        (if (memq (mevedel-tool-patch--effective-mode) '(edits full-auto))
            (let ((data-buffer (current-buffer))
                  (result (mevedel-tool-patch--result proposal changes)))
              (mevedel-tool-patch--apply
               data-buffer changes (lambda () (funcall callback result))))
          (require 'mevedel-patch-review)
          (mevedel-patch-review-start proposal callback (current-buffer))))
      (error
       (funcall callback
                (list :result
                      (format "Error: %s"
                              (mevedel-tool-patch--sanitize-error
                               (error-message-string err) proposal))
                      :status 'error))))))

(defun mevedel-tool-patch--get-paths (args)
  "Return every path affected by the patch in ARGS."
  (mevedel-tool-patch--get-paths-from-proposal
   (mevedel-tool-patch--parse (plist-get args :patch))))

(defun mevedel-tool-patch--get-paths-from-proposal (proposal)
  "Return ordinary filesystem paths affected by prepared PROPOSAL.

Authored resource operands are deliberately excluded even when preparation
has added private physical paths to their operation records."
  (delete-dups
   (cl-mapcan
    (lambda (operation)
      (delq nil
            (cl-mapcar
             (lambda (address path)
               (unless (mevedel-tool-patch--resource-address-p address)
                 path))
             (list (plist-get operation :rel-path)
                   (plist-get operation :move-rel-path))
             (list (mevedel-tool-patch--physical-path operation)
                   (mevedel-tool-patch--physical-move-path operation)))))
    (plist-get proposal :operations))))

(defun mevedel-tool-patch--render
    (name _args _result render-data)
  "Render an ApplyPatch result named NAME from RENDER-DATA."
  (when (eq (plist-get render-data :kind) 'patch)
    (let* ((files (plist-get render-data :files))
           (added (cl-loop for file in files sum (plist-get file :added)))
           (deleted (cl-loop for file in files sum (plist-get file :deleted)))
           (file-count (length files))
           (applied (or (plist-get render-data :applied) 0))
           (total (or (plist-get render-data :total) applied))
           (comments (or (plist-get render-data :comments) 0)))
      ;; "Name: arg (+N -M)" is the view's tool summary grammar; the
      ;; trailing count group picks up the green/red summary faces.
      (list :header
            (concat
             (or name "ApplyPatch") ": "
             (if (zerop applied)
                 "revision requested"
               (format "%d %s · %s"
                       file-count (if (= file-count 1) "file" "files")
                       (if (= applied total)
                           (format "%d change%s" applied
                                   (if (= applied 1) "" "s"))
                         (format "%d/%d changes" applied total))))
             (when (> comments 0)
               (format " · %d comment%s sent"
                       comments (if (= comments 1) "" "s")))
             (unless (zerop applied)
               (format " (+%d -%d)" added deleted)))
            :body
            (string-join
             (delq nil
                   (list
                    (and files
                         (mapconcat
                          (lambda (file)
                            (let ((path (if-let* ((destination
                                                   (plist-get file
                                                              :move-path)))
                                            (format "%s → %s"
                                                    (plist-get file :path)
                                                    destination)
                                          (plist-get file :path))))
                              (string-join
                               (delq nil
                                     (list
                                      (concat
                                       (propertize
                                        (mevedel-tool-patch--status file)
                                        'font-lock-face
                                        (mevedel-tool-patch--kind-face
                                         (plist-get file :kind)))
                                       " " path " · "
                                       (propertize
                                        (format "+%d" (plist-get file :added))
                                        'font-lock-face
                                        'mevedel-view-tool-diff-added)
                                       " "
                                       (propertize
                                        (format "−%d"
                                                (plist-get file :deleted))
                                        'font-lock-face
                                        'mevedel-view-tool-diff-removed))
                                      (unless (string-empty-p
                                               (plist-get file :diff))
                                        (mevedel-tool-patch--fontify-diff
                                         (plist-get file :diff)))))
                               "\n")))
                          files "\n\n"))
                    (when-let* ((notes (plist-get render-data :notes)))
                      (propertize (string-join notes "\n")
                                  'font-lock-face 'shadow))))
             "\n\n")
            :expandable-p t
            :initially-collapsed-p t))))

(defun mevedel-tool-patch-register ()
  "Register the ApplyPatch tool."
  (mevedel-define-tool
    :name "ApplyPatch"
    :description "Apply one patch that may add, update, delete, or move files."
    :summary "Apply a multi-file patch."
    :prompt-file "tools/applypatch.md"
    :handler #'mevedel-tool-patch-handler
    :args ((patch string :required
                  "A complete *** Begin Patch / *** End Patch patch."))
    :async-p t
    :groups (edit reviewed-edit)
    :snapshot-p t
    :get-paths #'mevedel-tool-patch--get-paths
    :display-arg (lambda (args)
                   (let ((proposal
                          (ignore-errors
                            (mevedel-tool-patch--parse
                             (plist-get args :patch)))))
                     (when proposal
                       (let ((count (length (plist-get proposal :operations))))
                         (format "%d %s" count
                                 (if (= count 1) "file" "files"))))))
    :renderer #'mevedel-tool-patch--render))

(provide 'mevedel-tool-patch)
;;; mevedel-tool-patch.el ends here
