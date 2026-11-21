;;; niwa-memory-library.el --- Current memory inventory and user deletion -*- lexical-binding: t -*-

;;; Commentary:
;; Discover current topic files independently of proposal history.  User
;; deletion shares captured scope, accepted changes, and checked undo with the
;; consolidation pipeline, without invoking a model.
;;
;; The inventory is display metadata, not a transaction snapshot: each root is
;; listed once with attributes, and only changed files are read, together, by
;; asynchronous pinned programs.  Inspection and deletion read fresh snapshots.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'niwa-memory-decision)
(require 'niwa-report)
(require 'yaml)

;; `niwa-session-control-fs'
(defvar niwa-session-control-fs--async-wait)

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

(defconst niwa-memory-library--batch 64
  "Maximum bounded topic reads in one inventory program.")

(defvar niwa-memory-library--cache (make-hash-table :test #'equal)
  "Display metadata of listed files, keyed by physical root directory.
Each value is an alist of (FILE SIGNATURE . METADATA).  A file whose listed
SIGNATURE is unchanged reuses METADATA without being read; each successful
listing replaces its root's entry, dropping removed files.")

(defvar niwa-memory-library--rows (make-hash-table :test #'equal)
  "Last complete topic rows, keyed by the configured memory roots listed.")

(defun niwa-memory-library-copy (value)
  "Copy display VALUE, including mutable strings.
Consumers then cannot alter cached or canonical state.  Long list spines copy
iteratively."
  (cond ((stringp value) (copy-sequence value))
        ((consp value)
         (let ((tail value) copied)
           (while (consp tail)
             (push (niwa-memory-library-copy (car tail)) copied)
             (setq tail (cdr tail)))
           (nconc (nreverse copied) (niwa-memory-library-copy tail))))
        (t value)))

(defun niwa-memory-library--metadata (text)
  "Read inert YAML metadata from bounded topic TEXT.
Frontmatter exactly as `niwa-memory-apply' writes it -- one-line JSON
name and description strings, a plain memory type word, then an optional
plain `modified' date -- is read without a YAML parse, which dominates
listing cost.  Its strings may only use escapes that YAML decodes as JSON
does, so the result equals the YAML parse.
Any other frontmatter is parsed as YAML."
  (when (string-match "\\`---[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n---[ \t]*\\(?:\n\\|\\'\\)" text)
    (let ((yaml (match-string 1 text))
          (string "\\(\"\\(?:[^\"\\\\\n[:cntrl:]]\\|\\\\[\"\\\\/nt]\\)*\"\\)"))
      (or (and (string-match (concat "\\`name: " string "\ndescription: " string
                                     "\ntype: \\(user\\|feedback\\|project\\|reference\\)"
                                     "\\(?:\nmodified: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\)?\\'")
                             yaml)
               (let ((name (match-string 1 yaml)) (description (match-string 2 yaml))
                     (type (match-string 3 yaml)) (modified (match-string 4 yaml)))
                 (condition-case nil
                     (append (list :name (json-parse-string name) :description (json-parse-string description)
                                   :type type)
                             (and modified (list :modified modified)))
                   (json-error nil))))
          (yaml-parse-string yaml :object-type 'plist :object-key-type 'keyword)))))

(defun niwa-memory-library--signature (attributes)
  "Return the change signature of listed file ATTRIBUTES."
  (list (file-attribute-type attributes) (file-attribute-size attributes)
        (file-attribute-modification-time attributes)
        (file-attribute-status-change-time attributes)
        (file-attribute-inode-number attributes) (file-attribute-device-number attributes)))

(defun niwa-memory-library--read (directory files callback live scope)
  "Read FILES in DIRECTORY by pinned bounded programs, then call CALLBACK.
Programs run asynchronously, `niwa-memory-library--batch' files each.
SCOPE attributes asynchronous program diagnostics to the originating workspace.
CALLBACK receives (FILE . RESULT) pairs and nil, or nil and the condition
that failed a program, exactly once, unless LIVE returns nil before a batch."
  (let ((batches (seq-partition files niwa-memory-library--batch)) results)
    (cl-labels ((next ()
                  (cond
                   ((not (funcall live)))
                   ((null batches) (funcall callback (cl-mapcar #'cons files results) nil))
                   (t
                    (niwa-session-control-fs-run-program-async
                     (mapcar (lambda (file)
                               (list :op 'read :optional t :path (file-name-concat directory file)
                                     :coding 'no-conversion
                                     :max-bytes (1+ niwa-memory-scope--max-file-bytes)))
                             (pop batches))
                     (lambda (batch failure)
                       (if failure (funcall callback nil failure)
                         (setq results (append results batch))
                         (next))) scope)))))
      (next))))

(defun niwa-memory-library--observe (file result)
  "Return display metadata for FILE from its pinned read RESULT.
The index yields :text, absent as empty; an unreadable index signals.  A topic
yields frontmatter :title and :type, or :error."
  (let ((limit niwa-memory-scope--max-file-bytes))
    (if (equal file "MEMORY.md")
        (list :text (condition-case nil
                        (niwa-memory-scope--text (niwa-session-control-fs-program-value result) limit)
                      (niwa-session-control-fs-absent "")))
      (condition-case problem
          (let ((metadata (niwa-memory-library--metadata
                           (niwa-memory-scope--text
                            (niwa-session-control-fs-program-value result) limit)))
                observed)
            (dolist (pair '((:name . :title) (:type . :type)) observed)
              (when (stringp (plist-get metadata (car pair)))
                (setq observed (plist-put observed (cdr pair) (plist-get metadata (car pair)))))))
        (error (list :error (error-message-string problem)))))))

(defun niwa-memory-library--plan (directory)
  "List DIRECTORY and return its topics, reusable metadata and stale files.
A file is stale unless its listed attributes match its cached signature."
  (let* ((inventory (niwa-memory-scope--inventory directory t))
         (attributes (plist-get inventory :attributes))
         (cached (gethash directory niwa-memory-library--cache))
         (topics (seq-filter (lambda (file)
                               (and (niwa-memory-proposal--file-p file)
                                    (not (equal (file-name-nondirectory file) "MEMORY.md"))
                                    (not (member file (plist-get inventory :directories)))))
                             (plist-get inventory :existing)))
         ;; Only a complete listing proves the index absent without reading it.
         (files (if (or (assoc "MEMORY.md" attributes) (not (plist-get inventory :complete)))
                    (cons "MEMORY.md" topics) topics))
         fresh stale)
    (dolist (file files)
      (let* ((listed (cdr (assoc file attributes)))
             (signature (and listed (niwa-memory-library--signature listed)))
             (old (assoc file cached)))
        (if (and signature (equal signature (cadr old)))
            (push old fresh)
          (push (cons file signature) stale))))
    (list :inventory inventory :topics topics :fresh fresh :stale (nreverse stale))))

(defun niwa-memory-library--root-rows (configured directory plan reads)
  "Return CONFIGURED root DIRECTORY's rows from PLAN and its stale READS.
Cache the observed metadata.  An unreadable index signals for the whole root."
  (let ((files (plist-get plan :fresh)) rows)
    (pcase-dolist (`(,file . ,result) reads)
      (push (cons file (cons (cdr (assoc file (plist-get plan :stale)))
                             (niwa-memory-library--observe file result)))
            files))
    (let ((index-text (or (plist-get (cddr (assoc "MEMORY.md" files)) :text) ""))
          (client (unless (file-remote-p directory) (niwa-workspace-identity-client))))
      (puthash directory files niwa-memory-library--cache)
      (dolist (file (plist-get plan :topics))
        (let ((metadata (cddr (assoc file files))))
          (push (append
                 (list :kind 'memory :id (list 'memory directory file) :file file
                       :configured configured :root directory
                       :target (file-name-concat directory file) :client client
                       :title (or (plist-get metadata :title) (file-name-base file))
                       :type (or (plist-get metadata :type) "")
                       :status (if (string-match-p
                                    (regexp-quote (concat "](" (mapconcat #'url-hexify-string (split-string file "/") "/") ")"))
                                    index-text) 'indexed 'unindexed))
                 (when-let* ((problem (plist-get metadata :error))) (list :error problem)))
                rows)))
      (unless (plist-get (plist-get plan :inventory) :complete)
        (push (list :kind 'memory :id (list 'incomplete directory) :title "Incomplete inventory"
                    :status 'unavailable :target directory :error "Memory inventory reached its entry limit")
              rows))
      (nreverse rows))))

(defun niwa-memory-library-refresh (workspace callback &optional livep)
  "Observe WORKSPACE's current topic rows, then call CALLBACK with them.
Each root is listed now; changed files are read by asynchronous programs, so
CALLBACK may run later, exactly once.  Rows include unindexed files; missing
roots are empty; unavailable roots and incomplete inventories remain visible.
Rows are display metadata: a file is reread when its listed attributes change.
Discovery neither creates storage nor changes memory content.
When predicate LIVEP returns nil before a root, batch or cache update, the
observation stops without further IO, caching, or CALLBACK."
  (let* ((roots (niwa-system--memory-roots workspace)) (key (copy-tree roots)) rows seen)
    (cl-labels
        ((live () (or (null livep) (funcall livep)))
         (unavailable (configured err)
           (push (list :kind 'memory :id (list 'unavailable configured) :title "Unavailable memory root"
                       :status 'unavailable :target configured :error (error-message-string err))
                 rows))
         (next ()
           (cond
            ((not (live)))
            ((null roots)
               (let ((result (nreverse rows)))
                 (puthash key result niwa-memory-library--rows)
                 (funcall callback (niwa-memory-library-copy result))))
            (t
             (let ((configured (plist-get (pop roots) :dir)) directory plan)
               ;; Continuations run outside each root's handler, so a later
               ;; root's failure is never attributed to this one.
               (condition-case err
                   (progn
                     (setq directory (file-name-as-directory (file-truename configured)))
                     (unless (member directory seen)
                       (push directory seen)
                       (setq plan (niwa-memory-library--plan directory))))
                 (error (unavailable configured err)))
               (if (not plan) (next)
                 (niwa-memory-library--read
                  directory (mapcar #'car (plist-get plan :stale))
                  (lambda (reads failure)
                    (when (live)
                      (condition-case err
                          (if failure (signal (car failure) (cdr failure))
                            (setq rows (append (reverse (niwa-memory-library--root-rows configured directory plan reads))
                                               rows)))
                        (error (unavailable configured err)))
                      (next)))
                  #'live (niwa-diagnostic-scope workspace))))))))
      (next))))

(defun niwa-memory-library-cached (workspace)
  "Return a copy of WORKSPACE's last complete topic rows, or nil before any."
  (niwa-memory-library-copy
   (gethash (niwa-system--memory-roots workspace) niwa-memory-library--rows)))

(defun niwa-memory-library-list (workspace)
  "Return current topic rows for WORKSPACE as `niwa-memory-library-refresh'.
Wait for its programs instead of returning before they deliver."
  (let ((niwa-session-control-fs--async-wait t) rows)
    (niwa-memory-library-refresh workspace (lambda (result) (setq rows result)))
    rows))

(defun niwa-memory-library-path (workspace item)
  "Revalidate WORKSPACE ITEM's original root and return its topic path."
  (unless (and (eq (plist-get item :kind) 'memory) (plist-get item :file))
    (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Select a stored memory"))
  (let ((directory (plist-get item :root)) (client (plist-get item :client)))
    (unless (and (or (not client) (equal client (niwa-workspace-identity-client)))
                 (seq-some (lambda (root) (equal (plist-get root :dir) (plist-get item :configured)))
                           (niwa-system--memory-roots workspace))
                 (equal directory (file-name-as-directory (file-truename (plist-get item :configured)))))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Original memory root is unavailable"))
    (let ((path (file-name-concat directory (plist-get item :file))))
      (unless (and (niwa-memory-proposal--file-p (plist-get item :file))
                   (equal path (file-truename path)))
        (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Memory topic is outside its original root"))
      path)))

(defun niwa-memory-library-details (workspace item)
  "Return a current-memory report for WORKSPACE ITEM."
  (let* ((path (niwa-memory-library-path workspace item))
         (snapshot (niwa-memory-scope--snapshot path niwa-memory-scope--max-file-bytes)))
    (unless (plist-get snapshot :exists) (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Memory topic no longer exists"))
    (list :title (plist-get item :title) :subtitle path :identity (plist-get item :id)
          :sections (list (list :id 'body :title "Stored memory" :mode 'markdown-mode
                               :body (decode-coding-string (plist-get snapshot :bytes) 'utf-8-unix))
                          (list :id 'location :title "Location" :body (niwa-report-fields
                                                                    (list "File" path)
                                                                    (list "Index" (plist-get item :status))))))))

(defun niwa-memory-library-delete (workspace item)
  "Delete WORKSPACE ITEM and its index entry through a checked user decision.
The caller owns interactive confirmation.  Retain the exact transaction for
recovery and reversal, recording `user' as producer without inference."
  (niwa-memory-library-path workspace item)
  (niwa-workspace-identity-ensure (niwa-workspace-root workspace))
  (let* ((scope (niwa-memory-scope-capture workspace (cons (plist-get item :configured) (plist-get item :file))))
         (root (car (plist-get scope :roots)))
         (claim (or (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180)
                    (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Memory review is busy"))))
    (unwind-protect
        (progn
          (unless (and root
                       (equal (plist-get (cdr root) :dir) (plist-get item :root))
                       (equal (plist-get (cdr root) :client) (plist-get item :client))
                       (member (plist-get item :file) (plist-get (cdr root) :files)))
            (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Memory topic or index could not be captured"))
          (let* ((prepared (niwa-memory-store-prepare workspace claim scope nil "User deletion"))
                 (proposal (list :action 'remove :root (car root) :file (plist-get item :file)
                                 :type (if (member (plist-get item :type) '("user" "feedback" "project" "reference"))
                                           (plist-get item :type) "project")
                                 :title (plist-get item :title) :hook "User deletion"
                                 :reason "Deleted by user from memory cockpit." :evidence nil :body ""))
                 (accepted (niwa-memory-store-accept-proposals
                            workspace prepared (list :proposals (list proposal) :no-action nil) nil "user" nil)))
            (unless accepted (error "Memory deletion lost ownership"))
            (niwa-memory-store-publish workspace (plist-get prepared :id))
            (niwa-memory-decision-apply workspace (plist-get prepared :id)
                                           (plist-get (car (plist-get accepted :proposals)) :id))))
      (niwa-journal-claim-settle claim 'cancelled ""))))

(provide 'niwa-memory-library)
;;; niwa-memory-library.el ends here
