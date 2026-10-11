;;; mevedel-memory-notes.el -- Shared working-note lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Shared working notes under `work://shared/' are temporary, machine-local
;; material.  Memory consolidation reviews quiet note versions, extracts their
;; durable knowledge into memory, and then disposes of each note: delete, keep,
;; or condense.  Quiet attachments that no remaining note references and quiet
;; empty directories receive housekeeping decisions without inference.
;;
;; This module owns the note inventory, readiness, reference protection,
;; eligibility derived from retained decisions, and the checked mutations that
;; apply note decisions.  Accepted reviews and their decisions are the only
;; processing record.  Mutations retain the previous bytes as private
;; content-addressed blobs so decisions applied without prompts stay undoable.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-memory-decision)
(require 'mevedel-memory-scope)
(require 'mevedel-session-control-fs)
(require 'url-util)

;; `mevedel-resource'
(declare-function mevedel-resource-encode-component "mevedel-resource" (value))
(declare-function mevedel-resource-work-shared-directory "mevedel-resource" (workspace))
(autoload 'mevedel-resource-encode-component "mevedel-resource")
(autoload 'mevedel-resource-work-shared-directory "mevedel-resource")

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-tool-fs-read'
(declare-function mevedel-tool-fs-read--binary-extension-p "mevedel-tool-fs-read" (filename))
(autoload 'mevedel-tool-fs-read--binary-extension-p "mevedel-tool-fs-read")

;; `mevedel-turn'
(declare-function mevedel-turn-busy-p "mevedel-turn" (&optional buffer))
(autoload 'mevedel-turn-busy-p "mevedel-turn")

(defvar read-circle)
(defvar read-eval)

(defcustom mevedel-memory-notes-quiet-hours 24
  "Hours a shared working file must stay unchanged before consolidation.
Younger notes are treated as active work and left alone."
  :type '(number :tag "Hours") :group 'mevedel)

(defcustom mevedel-memory-notes-review-days 7
  "Days before a kept, condensed, restored, or retained note is reviewed again.
An edit makes the new note version eligible after the ordinary quiet period."
  :type '(number :tag "Days") :group 'mevedel)

(defcustom mevedel-memory-notes-undo-quota (* 1024 1024 1024)
  "Maximum bytes of retained shared-note undo content per workspace.
A deletion or rewrite that would exceed it is refused rather than applied
without its undo copy."
  :type '(integer :tag "Bytes") :group 'mevedel)

(defconst mevedel-memory-notes--max-notes 16
  "Maximum complete text notes captured for one review.")

(defconst mevedel-memory-notes--max-bytes (* 96 1024)
  "Maximum total text-note bytes captured for one review.")

(defconst mevedel-memory-notes--max-note-bytes (* 32 1024)
  "Largest text note a review can read completely.")

(defconst mevedel-memory-notes--max-attachments 128
  "Maximum attachment and directory housekeeping items per review.")

(defconst mevedel-memory-notes--max-entries 4096
  "Maximum shared-folder entries inventoried in one observation.")

(defconst mevedel-memory-notes--scan-bytes (* 1024 1024)
  "Largest text file scanned completely for attachment references.")

(defconst mevedel-memory-notes--max-blocked 64
  "Maximum blocked items listed with reasons by one observation.
The total count of blocked items is always reported.")

(defconst mevedel-memory-notes-superseded-reason
  "Superseded by a later review of this shared file."
  "Rejection reason recording a disposition replaced by a later review.
Unlike a user's rejection, it does not defer the file's next review.")

(defun mevedel-memory-notes-directory (workspace)
  "Return WORKSPACE's configured shared working directory, or nil."
  (mevedel-resource-work-shared-directory workspace))

(defun mevedel-memory-notes-control-directory (directory)
  "Return shared-note coordination for physical shared DIRECTORY, or nil.
Coordination lives in `.mevedel/state/shared-notes/', outside the folder
whose contents it deletes."
  (let* ((directory (directory-file-name directory))
         (parent (file-name-directory directory)))
    (when (and (equal (file-name-nondirectory directory) "shared")
               (equal (file-name-nondirectory (directory-file-name parent)) ".mevedel"))
      (file-name-concat parent "state" "shared-notes"))))

(defun mevedel-memory-notes-blob-directory (directory)
  "Return the private undo blob directory for physical shared DIRECTORY."
  (file-name-concat (mevedel-memory-notes-control-directory directory) "blobs"))

(defun mevedel-memory-notes-address (relative)
  "Return the canonical `work://shared/' address naming RELATIVE."
  (concat "work://shared/"
          (mapconcat #'mevedel-resource-encode-component (split-string relative "/") "/")))

(defun mevedel-memory-notes-workspace-busy-p (workspace)
  "Return non-nil while a conversation in WORKSPACE runs or settles a turn."
  (let ((root (file-name-as-directory (expand-file-name (mevedel-workspace-root workspace)))))
    (cl-some (lambda (buffer)
               (when-let* ((session (buffer-local-value 'mevedel--session buffer))
                           (other (mevedel-session-workspace session)))
                 (and (equal root (file-name-as-directory (expand-file-name (mevedel-workspace-root other))))
                      (mevedel-turn-busy-p buffer))))
             (buffer-list))))


;;
;;; Inventory and classification

(defun mevedel-memory-notes--inventory (directory)
  "Walk shared DIRECTORY without following links.
Return :files and :directories as (RELATIVE . ATTRIBUTES) rows, plus
:complete.  Directory rows record :empty.  Links make the observation
incomplete instead of being followed."
  (let ((pending (list "")) (remaining mevedel-memory-notes--max-entries)
        (complete t) files directories problems)
    (while pending
      (let* ((relative (pop pending))
             (path (if (string-empty-p relative) directory (file-name-concat directory relative)))
             (children (condition-case err
                           (directory-files-and-attributes path nil directory-files-no-dot-files-regexp t)
                         (error (setq complete nil)
                                (push (cons relative (error-message-string err)) problems)
                                nil))))
        (unless (string-empty-p relative)
          (push (cons relative (list :empty (null children)
                                     :modified (file-attribute-modification-time
                                                (file-attributes path))))
                directories))
        (dolist (child children)
          (if (<= remaining 0)
              (setq complete nil)
            (cl-decf remaining)
            (let* ((name (car child)) (attributes (cdr child))
                   (type (file-attribute-type attributes))
                   (child-relative (if (string-empty-p relative) name (concat relative "/" name))))
              (cond
               ((stringp type)
                (setq complete nil)
                (push (cons child-relative "Linked shared entries are not followed") problems))
               ((eq type t) (push child-relative pending))
               (t (push (cons child-relative
                              (list :size (file-attribute-size attributes)
                                    :modified (file-attribute-modification-time attributes)))
                        files))))))))
    (list :files (sort files (lambda (a b) (string< (car a) (car b))))
          :directories (sort directories (lambda (a b) (string< (car a) (car b))))
          :complete complete :problems (nreverse problems))))

(defun mevedel-memory-notes--text (bytes &optional truncated)
  "Return BYTES decoded as UTF-8 text, or nil when they are binary.
TRUNCATED says BYTES may end inside one character."
  (let ((text (decode-coding-string bytes 'utf-8-unix)))
    (when truncated
      (while (and (> (length text) 0)
                  (eq (char-charset (aref text (1- (length text)))) 'eight-bit))
        (setq text (substring text 0 -1))))
    (unless (or (string-search "\0" bytes)
                (cl-some #'mevedel--invalid-message-char-p text))
      text)))

(defun mevedel-memory-notes--read (directory rows)
  "Read bounded prefixes of shared file ROWS below DIRECTORY.
Return rows extended with :kind `text', `attachment' or `unknown'.  Text
rows carry :text, the complete :bytes unless :truncated, and unreadable rows
carry :error."
  (let (pending results)
    (dolist (row rows)
      (if (mevedel-tool-fs-read--binary-extension-p (car row))
          (push (append row (list :kind 'attachment)) results)
        (push row pending)))
    (setq pending (nreverse pending))
    (while pending
      (let* ((batch (seq-take pending 8))
             (reads (condition-case err
                        (mevedel-session-control-fs-run-program
                         (mapcar (lambda (row)
                                   (list :op 'read :optional t :coding 'no-conversion
                                         :path (file-name-concat directory (car row))
                                         :max-bytes (1+ mevedel-memory-notes--scan-bytes)))
                                 batch))
                      (error (mapcar (lambda (_) (list :status 'failed :diagnostic (error-message-string err)))
                                     batch)))))
        (setq pending (nthcdr 8 pending))
        (cl-mapc
         (lambda (row result)
           (push
            (if (not (eq (plist-get result :status) 'ok))
                (append row (list :kind 'unknown :error (format "Unreadable shared file (%s)" (plist-get result :status))))
              (let* ((bytes (plist-get result :value))
                     (truncated (> (length bytes) mevedel-memory-notes--scan-bytes))
                     (text (mevedel-memory-notes--text
                            (if truncated (substring bytes 0 mevedel-memory-notes--scan-bytes) bytes)
                            truncated)))
                (if (not text)
                    (append row (list :kind 'attachment))
                  (append row (list :kind 'text :text text :bytes (unless truncated bytes)
                                    :truncated truncated)))))
            results))
         batch reads)))
    (sort results (lambda (a b) (string< (car a) (car b))))))

(defun mevedel-memory-notes--texts (rows)
  "Return (RELATIVE . TEXT) for the text files among read ROWS."
  (mapcar (lambda (row) (cons (car row) (plist-get (cdr row) :text)))
          (seq-filter (lambda (row) (eq (plist-get (cdr row) :kind) 'text)) rows)))

(defun mevedel-memory-notes--unscanned-p (rows)
  "Return non-nil when any of read ROWS could not be scanned completely."
  (seq-find (lambda (row) (or (plist-get (cdr row) :error) (plist-get (cdr row) :truncated))) rows))


;;
;;; Attachment references

(defun mevedel-memory-notes--spellings (attachment note)
  "Return reference spellings for ATTACHMENT seen from text file NOTE.
Both are relative to the shared root."
  (let* ((relative (file-relative-name attachment (or (file-name-directory note) "./")))
         (encoded (lambda (path) (mapconcat #'url-hexify-string (split-string path "/") "/"))))
    (delete-dups
     (list attachment relative (funcall encoded attachment) (funcall encoded relative)
           (mevedel-memory-notes-address attachment)))))

(defun mevedel-memory-notes--referenced (attachments texts)
  "Return the members of ATTACHMENTS referenced by any of TEXTS.
TEXTS are (RELATIVE . TEXT) rows.  Matching is conservative substring
matching of root-relative, note-relative, URL-encoded, and address forms, so
Markdown links, backticked paths, prose paths and addresses all protect."
  (seq-filter
   (lambda (attachment)
     (cl-some (lambda (row)
                (cl-some (lambda (spelling) (string-search spelling (cdr row)))
                         (mevedel-memory-notes--spellings attachment (car row))))
              texts))
   attachments))

(defun mevedel-memory-notes--external-p (relative texts)
  "Return non-nil when TEXTS outside the shared folder cite RELATIVE.
TEXTS are memory topics and instruction files.  A citation of the file or of
any containing folder counts, by address or by `.mevedel/shared/' path."
  (let ((parts (split-string relative "/")) prefixes)
    (dotimes (index (length parts))
      (push (string-join (seq-take parts (1+ index)) "/") prefixes))
    (cl-some (lambda (text)
               (cl-some (lambda (prefix)
                          (or (string-search (mevedel-memory-notes-address prefix) text)
                              (string-search (concat ".mevedel/shared/" prefix) text)))
                        prefixes))
             texts)))


;;
;;; Eligibility from retained decisions

(defun mevedel-memory-notes-passes (workspace entries)
  "Return accepted, unretired consolidation bundles in WORKSPACE.
ENTRIES is one public journal observation.  Accepted private bundles that
have not been published are included.  Unreadable accepted state signals."
  (let* ((root (mevedel-workspace-root workspace))
         (ids (delete-dups
               (append
                (mapcar (lambda (entry) (plist-get entry :pass-id))
                        (seq-filter (lambda (entry) (eq (plist-get entry :kind) 'consolidation)) entries))
                (mapcar (lambda (directory) (file-name-nondirectory (directory-file-name directory)))
                        (seq-filter
                         (lambda (directory)
                           (mevedel-session-control-fs-path-exists-p (file-name-concat directory "accepted.el")))
                         (mevedel-session-control-fs-list-directory
                          (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory root))
                                            "passes")
                          mevedel-journal-store-id-regexp))))))
         bundles)
    (dolist (id ids (nreverse bundles))
      (unless (mevedel-journal-cleanup-pass-retired-p root id)
        (when-let* ((accepted (mevedel-memory-store-accepted workspace id)))
          (push accepted bundles))))))

(defun mevedel-memory-notes--history (workspace entries)
  "Return a table from shared relative path to retained disposition observations.
Each observation has :action, :hash (the reviewed version), :result (the
condensed version), :status, :superseded, and :at, the decision or review
time."
  (let ((table (make-hash-table :test #'equal))
        (passes (make-hash-table :test #'equal)))
    (dolist (accepted (mevedel-memory-notes-passes workspace entries) table)
      (clrhash passes)
      (puthash (plist-get (plist-get accepted :prepared) :id) accepted passes)
      (dolist (proposal (plist-get accepted :proposals))
        (when (mevedel-memory-proposal-note-action-p (plist-get proposal :action))
          (let* ((decision (mevedel-memory-decision-status workspace (plist-get proposal :id) entries passes))
                 (before (cdr (assoc (plist-get proposal :file) (plist-get proposal :before)))))
            (push (list :action (plist-get proposal :action)
                        :hash (if (plist-get before :directory) 'directory (plist-get before :hash))
                        :result (plist-get proposal :result)
                        :status (or (plist-get decision :status) 'pending)
                        :superseded (equal (plist-get decision :reason) mevedel-memory-notes-superseded-reason)
                        :at (float-time (date-to-time (or (plist-get decision :created)
                                                          (plist-get (plist-get accepted :review) :created)))))
                  (gethash (plist-get proposal :file) table))))))))

(defun mevedel-memory-notes--suppressed (observations version now review-days)
  "Return why VERSION is not eligible given OBSERVATIONS at NOW, or nil.
REVIEW-DAYS sets the reconsideration period after a retained outcome."
  (let ((period (* review-days 86400)) reason)
    (dolist (observation observations reason)
      (let ((action (plist-get observation :action))
            (status (plist-get observation :status))
            (until (+ (plist-get observation :at) period)))
        (when (or (equal version (plist-get observation :hash))
                  (and (eq action 'note-condense) (equal version (plist-get observation :result))))
          (pcase status
            ((or 'pending 'recovery-required) (setq reason "A previous decision is still pending"))
            ((or 'rejected 'reversed)
             (when (and (< now until) (not (plist-get observation :superseded)))
               (setq reason (or reason "Recently retained by a user decision"))))
            ('applied
             (when (and (memq action '(note-keep note-condense)) (< now until))
               (setq reason (or reason "Recently kept or condensed"))))))))))


;;
;;; Capture

(cl-defun mevedel-memory-notes-capture (workspace entries &key (quiet-hours mevedel-memory-notes-quiet-hours)
                                                  (review-days mevedel-memory-notes-review-days)
                                                  (quota mevedel-memory-notes-undo-quota)
                                                  mode external)
  "Select WORKSPACE's ready shared working files for consolidation.
ENTRIES is one public journal observation.  Return nil without a shared
folder, otherwise a plist with :root, a (ID . ROOT) scope row or nil when
nothing is ready, :notes, :attachments, :directories and :remaining counts,
:blocked, at most `mevedel-memory-notes--max-blocked' (RELATIVE . REASON)
rows, and :blocked-count.  MODE freezes the consolidation mode.  EXTERNAL
lists memory and instruction texts; files they cite are never removed as
housekeeping, and cited notes are flagged for the review.

Only complete UTF-8 notes within the review bounds are admitted, oldest
first.  Attachments are admitted only when quiet, unreferenced by any
surviving text file, and the reference scan is complete.  No shared file is
changed; the summary is recorded as the latest observation."
  (let* ((configured (mevedel-memory-notes-directory workspace)))
    (when (and configured (mevedel-session-control-fs-directory-p configured))
      (let* ((directory (file-name-as-directory (file-truename configured)))
             (client (unless (file-remote-p directory) (mevedel-workspace-identity-client)))
             (now (mevedel-session-control-fs-target-time directory))
             (quiet (* quiet-hours 3600))
             (history (mevedel-memory-notes--history workspace entries))
             (inventory (mevedel-memory-notes--inventory directory))
             (rows (mevedel-memory-notes--read directory (plist-get inventory :files)))
             (scan-complete (and (plist-get inventory :complete)
                                 (not (mevedel-memory-notes--unscanned-p rows))))
             (referenced (mevedel-memory-notes--referenced
                          (mapcar #'car (seq-filter (lambda (row) (eq (plist-get (cdr row) :kind) 'attachment)) rows))
                          (mevedel-memory-notes--texts rows)))
             (budget mevedel-memory-notes--max-bytes)
             notes attachments directories before blocked (blocked-count 0) (remaining 0) (housekeeping 0))
        (cl-labels
            ((block (relative reason)
               (cl-incf blocked-count)
               (when (< (length blocked) mevedel-memory-notes--max-blocked)
                 (push (cons relative reason) blocked)))
             (ready-p (relative modified version)
               (and (<= (float-time modified) (- now quiet))
                    (not (mevedel-memory-notes--suppressed
                          (gethash relative history) version now review-days)))))
          (dolist (row (sort (copy-sequence rows)
                             (lambda (a b) (time-less-p (plist-get (cdr a) :modified)
                                                        (plist-get (cdr b) :modified)))))
            (let* ((relative (car row))
                   (path (file-name-concat directory relative))
                   (modified (plist-get (cdr row) :modified)))
              (pcase (plist-get (cdr row) :kind)
                ('unknown (block relative (plist-get (cdr row) :error)))
                ('text
                 (let ((bytes (plist-get (cdr row) :bytes)))
                   (cond
                    ((or (null bytes) (> (length bytes) mevedel-memory-notes--max-note-bytes))
                     (block relative (format "Text exceeds the %d KiB review limit"
                                             (/ mevedel-memory-notes--max-note-bytes 1024))))
                    ((not (ready-p relative modified (secure-hash 'sha256 bytes))))
                    ((or (>= (length notes) mevedel-memory-notes--max-notes) (> (length bytes) budget))
                     (cl-incf remaining))
                    (t (condition-case err
                           (let ((snapshot (mevedel-memory-scope--snapshot
                                            path mevedel-memory-notes--max-note-bytes
                                            (list :path path :status 'ok :value bytes))))
                             (cl-decf budget (length bytes))
                             (push (cons relative snapshot) before)
                             (push (list relative :id (mevedel-memory-notes-address relative)
                                         :modified (format-time-string "%FT%TZ" modified t)
                                         :cited (and (mevedel-memory-notes--external-p relative external) t))
                                   notes))
                         (error (block relative (error-message-string err))))))))
                ('attachment
                 (cond
                  ((> (float-time modified) (- now quiet)))
                  ((member relative referenced))
                  ((mevedel-memory-notes--external-p relative external))
                  ((not scan-complete)
                   (block relative "Attachment references could not be checked completely"))
                  ((> (or (plist-get (cdr row) :size) 0) quota)
                   (block relative "Attachment exceeds the shared-note undo quota"))
                  ((>= housekeeping mevedel-memory-notes--max-attachments) (cl-incf remaining))
                  (t (condition-case err
                         (let ((snapshot (mevedel-memory-notes-observe (list :path path))))
                           (when (ready-p relative modified (plist-get snapshot :hash))
                             (cl-incf housekeeping)
                             (push (cons relative snapshot) before)
                             (push relative attachments)))
                       (error (block relative (error-message-string err))))))))))
          (dolist (row (plist-get inventory :directories))
            (let ((relative (car row)))
              (when (and (plist-get (cdr row) :empty)
                         (not (mevedel-memory-notes--external-p relative external))
                         (ready-p relative (plist-get (cdr row) :modified) 'directory))
                (if (>= housekeeping mevedel-memory-notes--max-attachments)
                    (cl-incf remaining)
                  (cl-incf housekeeping)
                  (push (cons relative (list :path (file-name-concat directory relative)
                                             :exists t :directory t :empty t))
                        before)
                  (push relative directories)))))
          (dolist (problem (plist-get inventory :problems))
            (block (car problem) (cdr problem))))
        (setq notes (nreverse notes) attachments (nreverse attachments)
              directories (nreverse directories) blocked (nreverse blocked))
        (let ((summary (list :notes (length notes) :attachments (length attachments)
                             :directories (length directories) :remaining remaining
                             :blocked blocked :blocked-count blocked-count)))
          (mevedel-memory-notes--record
           directory (append (list :at (format-time-string "%FT%TZ" (seconds-to-time now) t)) summary))
          (append
           (list :root (when (or notes attachments directories)
                         (let* ((target (mevedel-execution-target-create directory))
                                (id (secure-hash 'sha256 (prin1-to-string (list 'notes directory client)))))
                           (cons id (list :kind 'notes :label "Shared working notes"
                                          :dir directory :configured-dir (file-name-as-directory configured)
                                          :client client :target (mevedel-execution-target-identity target)
                                          :mode mode :captured now
                                          :files (mapcar #'car notes) :notes notes
                                          :attachments attachments :directories directories
                                          :before (nreverse before))))))
           summary))))))

(defun mevedel-memory-notes-root (scope)
  "Return SCOPE's captured (ID . ROOT) shared-notes row, or nil."
  (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'notes)) (plist-get scope :roots)))

(defun mevedel-memory-notes--observation-path (directory)
  "Return the latest-observation record for physical shared DIRECTORY."
  (file-name-concat (mevedel-memory-notes-control-directory directory) "observation.el"))

(defun mevedel-memory-notes--record (directory summary)
  "Record SUMMARY as the latest observation of shared DIRECTORY.
This disposable record only explains blocked and waiting items, so a
failure to write it never blocks the review."
  (ignore-errors
    (let ((path (mevedel-memory-notes--observation-path directory)))
      (mevedel-session-control-fs-make-directory (file-name-directory path) t)
      (mevedel-session-control-fs-write-file
       path (let ((print-length nil) (print-level nil)) (prin1-to-string summary))))))

(defun mevedel-memory-notes-observation (workspace)
  "Return WORKSPACE's latest recorded shared-folder observation, or nil."
  (when-let* ((configured (mevedel-memory-notes-directory workspace))
              ((file-directory-p configured)))
    (condition-case nil
        (let ((read-eval nil) (read-circle nil))
          (car (read-from-string
                (mevedel-session-control-fs-read-file
                 (mevedel-memory-notes--observation-path (file-truename configured))
                 'utf-8-unix (* 256 1024)))))
      (error nil))))

(defun mevedel-memory-notes-documents (scope)
  "Return the complete text notes captured in SCOPE as review documents.
Each document has :id (its address), :path, :modified, :cited and :body."
  (when-let* ((row (mevedel-memory-notes-root scope)))
    (mapcar (lambda (note)
              (list :id (plist-get (cdr note) :id) :path (car note)
                    :modified (plist-get (cdr note) :modified)
                    :cited (plist-get (cdr note) :cited)
                    :body (decode-coding-string
                           (plist-get (cdr (assoc (car note) (plist-get (cdr row) :before))) :bytes)
                           'utf-8-unix)))
            (plist-get (cdr row) :notes))))

(defun mevedel-memory-notes-index (scope)
  "Return an alist from captured note address to (ROOT-ID . RELATIVE) in SCOPE."
  (when-let* ((row (mevedel-memory-notes-root scope)))
    (mapcar (lambda (note) (cons (plist-get (cdr note) :id) (cons (car row) (car note))))
            (plist-get (cdr row) :notes))))

(defun mevedel-memory-notes-recheck (root relative)
  "Return why attachment RELATIVE in captured notes ROOT may not be removed.
Return nil when no text file changed since capture references it.  Files
unchanged since capture were already scanned, so only newer text is read.
An incomplete observation is a reason not to remove."
  (let* ((directory (plist-get root :dir))
         (since (plist-get root :captured))
         (inventory (mevedel-memory-notes--inventory directory))
         (rows (mevedel-memory-notes--read
                directory (seq-filter (lambda (row) (>= (float-time (plist-get (cdr row) :modified)) since))
                                      (plist-get inventory :files)))))
    (cond ((not (plist-get inventory :complete))
           "The shared folder could not be observed completely")
          ((mevedel-memory-notes--unscanned-p rows)
           "References to this attachment could not be rechecked")
          ((mevedel-memory-notes--referenced (list relative) (mevedel-memory-notes--texts rows))
           "A shared note written after capture references this attachment"))))

(defun mevedel-memory-notes-housekeeping (scope)
  "Return structured housekeeping proposals for SCOPE's attachments and folders.
They need no inference: the captured observation already proved quiet,
unreferenced attachments and quiet empty directories."
  (when-let* ((row (mevedel-memory-notes-root scope)))
    (append
     (mapcar (lambda (file)
               (list :action 'note-delete :root (car row) :file file :type "attachment" :title file :hook ""
                     :reason "Quiet attachment that no remaining shared note references."
                     :evidence nil :body ""))
             (plist-get (cdr row) :attachments))
     (mapcar (lambda (file)
               (list :action 'note-prune :root (car row) :file file :type "directory" :title file :hook ""
                     :reason "Quiet empty shared directory."
                     :evidence nil :body ""))
             (plist-get (cdr row) :directories)))))

(defun mevedel-memory-notes-supersede (workspace accepted)
  "Reject earlier undecided dispositions of files ACCEPTED disposes of again.
A later review of the same shared file in WORKSPACE replaces pending, stale
and unavailable dispositions from older reviews, so they no longer wait in
the cockpit or retain their review's history.  Return (REJECTED . FAILED)."
  (let* ((pass (plist-get (plist-get accepted :prepared) :id))
         (files (delq nil (mapcar (lambda (proposal)
                                    (and (mevedel-memory-proposal-note-action-p (plist-get proposal :action))
                                         (plist-get proposal :file)))
                                  (plist-get accepted :proposals))))
         (rejected 0) (failed 0))
    (when files
      (dolist (older (mevedel-memory-notes-passes
                      workspace (mevedel-journal-store-entries (mevedel-workspace-root workspace))))
        (let ((id (plist-get (plist-get older :prepared) :id)))
          (unless (equal id pass)
            (dolist (proposal (plist-get older :proposals))
              (when (and (mevedel-memory-proposal-note-action-p (plist-get proposal :action))
                         (member (plist-get proposal :file) files)
                         (memq (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)
                               '(nil stale unavailable)))
                ;; A pending write or another owner leaves the older
                ;; disposition for a later supersession or the cockpit.
                (condition-case nil
                    (progn
                      (mevedel-memory-decision-reject workspace id (plist-get proposal :id)
                                                      mevedel-memory-notes-superseded-reason)
                      (cl-incf rejected))
                  (error (cl-incf failed)))))))))
    (cons rejected failed)))


;;
;;; Changes, observation and checked mutation

(defun mevedel-memory-notes-changes (proposal)
  "Return checked changes for accepted shared-note PROPOSAL without writing.
Each change carries its exact :before and :after snapshot."
  (let* ((action (plist-get proposal :action))
         (before (cdr (assoc (plist-get proposal :file) (plist-get proposal :before))))
         (path (plist-get before :path)))
    (unless (and before (plist-get before :exists))
      (error "Shared-note proposal lacks its captured before-state"))
    (pcase action
      ('note-keep nil)
      ((or 'note-delete 'note-prune)
       ;; Pruning removes folders; deletion removes files.
       (unless (eq (eq action 'note-prune) (and (plist-get before :directory) t))
         (error "Shared-note action does not match its captured entry"))
       (list (list :path path :before before :action 'delete :after (list :path path :exists nil))))
      ('note-condense
       (let ((bytes (mevedel-memory-proposal-condensed-bytes (plist-get proposal :body))))
         (when (or (plist-get before :directory) (null (plist-get before :bytes)))
           (error "Only captured text notes can be condensed"))
         (when (> (length bytes) mevedel-memory-notes--max-note-bytes)
           (error "Condensed note exceeds its readable byte limit"))
         (list (list :path path :before before :action 'write
                     :content (decode-coding-string bytes 'utf-8-unix) :bytes bytes
                     :mode (plist-get before :mode)
                     :after (list :path path :exists t :bytes bytes :hash (secure-hash 'sha256 bytes)
                                  :mode (plist-get before :mode))))))
      (_ (error "Unknown shared-note action")))))

(defun mevedel-memory-notes-observe (snapshot)
  "Observe the current state at SNAPSHOT's path comparably to SNAPSHOT.
Regular files are hashed on the target; folders record whether they are
empty; absence is a state.  Links and other entries signal."
  (let ((path (plist-get snapshot :path)))
    (condition-case err
        (let ((hash (mevedel-session-control-fs-file-hash path)))
          (list :path path :exists t :hash (plist-get hash :sha256) :size (plist-get hash :size)
                :mode (file-modes path)))
      (mevedel-session-control-fs-absent (list :path path :exists nil))
      ;; Hashing refuses directories, links and other non-regular entries.
      (file-error
       (if (mevedel-session-control-fs-directory-p path)
           (list :path path :exists t :directory t
                 :empty (null (mevedel-session-control-fs-list-directory path ".*")))
         (signal (car err) (cdr err)))))))

(defun mevedel-memory-notes-same-p (left right)
  "Return non-nil when shared-note snapshots LEFT and RIGHT name the same state."
  (and left right
       (equal (plist-get left :path) (plist-get right :path))
       (eq (not (plist-get left :exists)) (not (plist-get right :exists)))
       (or (not (plist-get left :exists))
           (if (plist-get left :directory)
               (and (plist-get right :directory)
                    (eq (not (plist-get left :empty)) (not (plist-get right :empty))))
             (and (not (plist-get right :directory))
                  (equal (plist-get left :hash) (plist-get right :hash))
                  (equal (plist-get left :mode) (plist-get right :mode)))))))

(defun mevedel-memory-notes-current-p (snapshot)
  "Return non-nil when SNAPSHOT still describes its path on the target."
  (mevedel-memory-notes-same-p snapshot (mevedel-memory-notes-observe snapshot)))

(defun mevedel-memory-notes--intent-hashes (intent)
  "Return the blob hashes retained by shared-note write INTENT."
  (let (hashes)
    (dolist (row (append (plist-get intent :before) (plist-get intent :after)) hashes)
      (let ((snapshot (cdr row)))
        (when (and (plist-get snapshot :exists) (not (plist-get snapshot :directory))
                   (plist-get snapshot :hash))
          (push (plist-get snapshot :hash) hashes))))))

(defun mevedel-memory-notes-collect-blobs (workspace directory)
  "Delete undo blobs in DIRECTORY that no retained WORKSPACE write references.
Retained write intents are the authenticated dependency: an unreadable
intent stops collection rather than risking a needed blob.  The caller
holds WORKSPACE's consolidation claim.  Return the number deleted."
  (let ((referenced (make-hash-table :test #'equal)) (deleted 0))
    (dolist (row (mevedel-memory-write-list workspace))
      (when (plist-get row :error)
        (error "Shared-note undo blobs have an unreadable dependency"))
      (dolist (hash (mevedel-memory-notes--intent-hashes (plist-get row :intent)))
        (puthash hash t referenced)))
    (dolist (path (mevedel-session-control-fs-list-directory directory "\\`[0-9a-f]\\{64\\}\\'") deleted)
      (unless (gethash (file-name-nondirectory path) referenced)
        (mevedel-session-control-fs-delete-file path)
        (cl-incf deleted)))))

(defun mevedel-memory-notes--quota (workspace directory needed)
  "Ensure undo blob DIRECTORY in WORKSPACE has room for NEEDED more bytes."
  (cl-labels ((used ()
                (let ((result (car (mevedel-session-control-fs-tree-sizes (list directory)))))
                  (if (eq (plist-get result :status) 'ok) (plist-get result :value) 0))))
    (when (> (+ (used) needed) mevedel-memory-notes-undo-quota)
      (mevedel-memory-notes-collect-blobs workspace directory)
      (when (> (+ (used) needed) mevedel-memory-notes-undo-quota)
        (error "Shared-note undo storage quota would be exceeded")))))

(defun mevedel-memory-notes--ancestors (path root)
  "Return PATH's ancestor directories strictly below ROOT, innermost first."
  (let ((root (file-name-as-directory root))
        (directory (file-name-directory (directory-file-name path)))
        ancestors)
    (while (and directory (string-prefix-p root directory)
                (not (equal (file-name-as-directory directory) root)))
      (push (directory-file-name directory) ancestors)
      (setq directory (file-name-directory (directory-file-name directory))))
    (nreverse ancestors)))

(defun mevedel-memory-notes-commit (workspace root target changes currentp)
  "Apply shared-note CHANGES below captured ROOT under WORKSPACE TARGET.
Each change runs as one program under TARGET's lock: ownership guard, exact
before-state proof, capture of the previous bytes as an undo blob, mutation,
then optional pruning of ancestors left empty.  CURRENTP must keep
authorizing every change.  A failure leaves earlier changes for checked
recovery from the retained intent."
  (let ((blobs (mevedel-memory-notes-blob-directory (plist-get root :dir)))
        (lock (plist-get target :directory)))
    (dolist (change changes)
      (unless (funcall currentp) (error "Shared-note ownership is unavailable"))
      (let* ((before (plist-get change :before))
             (after (or (plist-get change :after)
                        (if (eq (plist-get change :action) 'delete)
                            (list :path (plist-get change :path) :exists nil)
                          (error "Shared-note change lacks its after-state"))))
             (path (plist-get before :path))
             (file-before (and (plist-get before :exists) (not (plist-get before :directory))))
             (restore (and (plist-get after :exists) (not (plist-get after :directory))
                           (null (plist-get after :bytes)))))
        (unless (equal path (plist-get after :path)) (error "Shared-note mutation paths differ"))
        (when-let* ((buffer (find-buffer-visiting path)) ((buffer-modified-p buffer)))
          (error "Shared note has unsaved buffer changes: %s" path))
        (mevedel-session-control-fs-make-directory blobs t)
        (when (and file-before
                   (not (mevedel-session-control-fs-path-exists-p
                         (file-name-concat blobs (plist-get before :hash)))))
          (mevedel-memory-notes--quota workspace blobs
                                       (or (plist-get before :size) (length (plist-get before :bytes)) 0)))
        (when (plist-get after :exists)
          (mevedel-session-control-fs-make-directory
           (if (plist-get after :directory) (file-name-directory (directory-file-name path))
             (file-name-directory path))
           t))
        (unless (funcall currentp) (error "Shared-note ownership is unavailable"))
        (dolist (result
                 (mevedel-session-control-fs-run-program
                  (append
                   (list (list :op 'verify :path (mevedel-journal-claim--path target nil)
                               :content (json-serialize (mevedel-journal-claim--record target)))
                         (list :op 'absent :path (mevedel-journal-claim--path target t))
                         (list :op 'before-time :path lock
                               :content (number-to-string (plist-get target :expires-at))))
                   (cond ((not (plist-get before :exists)) (list (list :op 'absent :path path)))
                         ((plist-get before :directory)
                          (list (list :op 'directory-p :path path)
                                (list :op 'verify-list :path path :content "")))
                         (t (list (list :op 'verify-hash :path path :content (plist-get before :hash))
                                  (list :op 'verify-mode :path path
                                        :content (format "%o" (plist-get before :mode)))
                                  (list :op 'capture-blob :source path
                                        :path (file-name-concat blobs (plist-get before :hash))))))
                   (cond ((not (plist-get after :exists))
                          (cons (list :op (if (plist-get before :directory) 'delete-empty-directory 'delete-file)
                                      :path path)
                                (mapcar (lambda (directory)
                                          (list :op 'delete-empty-directory :path directory :optional t))
                                        (mevedel-memory-notes--ancestors path (plist-get root :dir)))))
                         ((plist-get after :directory) (list (list :op 'make-directory :path path)))
                         (restore (list (list :op 'restore-blob :path path :mode (plist-get after :mode)
                                              :source (file-name-concat blobs (plist-get after :hash)))))
                         (t (list (list :op 'write-mode :path path :coding 'no-conversion
                                        :content (concat (format "%o\n" (plist-get after :mode))
                                                         (plist-get after :bytes)))))))
                  lock))
          ;; Ancestors that are not empty simply stay.
          (unless (or (eq (plist-get result :status) 'ok)
                      (and (eq (plist-get result :op) 'delete-empty-directory)
                           (not (equal (plist-get result :path) path))))
            (mevedel-session-control-fs-program-value result)))
        (when-let* ((buffer (find-buffer-visiting path))
                    ((not (buffer-modified-p buffer)))
                    ((plist-get after :exists))
                    ((not (plist-get after :directory))))
          (with-current-buffer buffer (revert-buffer t t t)))))))

(provide 'mevedel-memory-notes)
;;; mevedel-memory-notes.el ends here
