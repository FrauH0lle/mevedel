;;; migrate-from-mevedel.el --- One-off offline niwa cutover -*- lexical-binding: t -*-

;;; Commentary:
;; Stop every editor and host using the source first.  From the checkout,
;; use Eask to launch batch Emacs with the arguments:
;; --batch -L . -l scripts/migrate-from-mevedel.el \
;;   -f niwa-migrate-from-mevedel-main -- DIR...
;; DIR is a workspace root or the old user directory itself.  Sessions are not
;; migrated; the sibling backup retains the complete unmodified source.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'lisp-mode)

(defconst niwa-migrate-from-mevedel--old ".mevedel"
  "Legacy state directory component.")

(defconst niwa-migrate-from-mevedel--marker ".cutover-ready"
  "Receipt in a validated tree awaiting the legacy source backup.")

(defconst niwa-migrate-from-mevedel--legacy
  '((keep-workspace artifacts memory shared journal skills hooks.el hooks.json hooks
     permissions.el workspace-id lobby instructions.el skills-state.el exports
     input-history.el plugins.el recovery-archives)
    (keep-user memory skills skills-state.el permissions.el lobbies.el artifact-rooms.eld
     remote-artifacts.eld whiteboard-libraries plugins hooks.el hooks.json hooks workspace-schedules)
    (drop-workspace sessions sessions.pre-store sessions.pre-v0.5.12 sessions.pre-v0.5.13
     leases wake-ups .mevedel-pending-executions)
    (drop-user trusted-hooks.el native wake-ups claude-agent-logs agent-stderr.log)
    (scratch .mevedel-save-as- .mevedel-spool- .mevedel-spool-view- .mevedel-staging-
     .mevedel-write- .mevedel-control-fs-)
    (properties mevedel-instruction mevedel-id mevedel-uuid mevedel-instruction-type
     mevedel-note-text mevedel-note-tags mevedel-note-author mevedel-note-state
     mevedel-note-session-id mevedel-note-settings mevedel-note-prepared-change
     mevedel-note-applied-changes mevedel-note-attention-state mevedel-note-buddy-severity
     mevedel-note-buddy-code mevedel-note-kept-from-buddy mevedel-span mevedel-links))
  "Explicit legacy state families and instruction property keys.")

(defun niwa-migrate-from-mevedel--legacy-p (family name)
  "Whether NAME occurs in the legacy FAMILY allowlist."
  (memq (intern-soft name) (alist-get family niwa-migrate-from-mevedel--legacy)))

(defun niwa-migrate-from-mevedel--present-p (path)
  "Whether PATH exists, including a dangling symbolic link."
  (or (file-symlink-p path) (file-exists-p path)))

(defun niwa-migrate-from-mevedel--read (path)
  "Read one inert Lisp form from PATH, allowing comments but not references."
  (with-temp-buffer
    (insert-file-contents path)
    (niwa-migrate-from-mevedel--read-form path)))

(defun niwa-migrate-from-mevedel--read-form (path)
  "Read one inert Lisp form in the current buffer, named PATH for errors."
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (goto-char (point-min))
  (let* ((read-circle nil)
         (form (read (current-buffer))))
    (forward-comment (point-max))
    (unless (eobp) (error "Trailing data in %s" path))
    form))

(defun niwa-migrate-from-mevedel--json (path)
  "Read JSON from PATH without evaluating anything."
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-string (buffer-string) :null-object :null :false-object :false)))

(defun niwa-migrate-from-mevedel--paths (text)
  "Rename legacy directory components in TEXT, not similar names."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     (concat "\\(^\\|[/ \t\"'=;:(]\\)" (regexp-quote niwa-migrate-from-mevedel--old)
             "\\(/\\|$\\)")
     "\\1.niwa\\2" text)))

(defun niwa-migrate-from-mevedel--kept-path (path source kept)
  "Rename PATH only when it addresses a KEPT entry under SOURCE."
  (save-match-data
    (let ((legacy (regexp-quote (concat niwa-migrate-from-mevedel--old "/"))))
      (or (when (string-match (concat "\\(?:\\`\\|/\\)\\(" legacy "\\)") path)
            (let* ((begin (match-beginning 1)) (end (match-end 1))
                   (root (expand-file-name (substring path 0 (1- end)) (file-name-directory source)))
                   (leaf (substring path end)))
              (when (and (equal root source) (gethash (directory-file-name leaf) kept))
                (concat (substring path 0 begin) ".niwa/" leaf))))
          path))))

(defun niwa-migrate-from-mevedel--command (command source kept)
  "Rewrite literal path arguments in COMMAND from SOURCE using KEPT entries."
  (let* ((single "'[^'\n]*'")
         (double "\"\\(?:\\\\.\\|[^\"\\\\\n]\\)*\"")
         (quoted (concat single "\\|" double)))
    ;; Keep adjacent fragments in the same word, but do not interpret shell expansion.
    (replace-regexp-in-string
     (concat "\\(?:" quoted "\\|\\\\.\\|[^ \t\n\"'\\\\;|&<>()`]\\)+")
     (lambda (token)
       (save-match-data
         (let* ((assignment (and (string-match "\\`[[:alnum:]_-]+=" token) (match-end 0)))
                (prefix (substring token 0 (or assignment 0)))
                (word (substring token (or assignment 0))))
           (concat prefix
                   (cond
                    ((string-match-p (concat "\\`\\(?:" quoted "\\)\\'") word)
                     (concat (substring word 0 1)
                             (niwa-migrate-from-mevedel--kept-path (substring word 1 -1) source kept)
                             (substring word -1)))
                    ((not (string-match-p "[\"'\\\\$]" word))
                     (niwa-migrate-from-mevedel--kept-path word source kept))
                    (t word))))))
     command t t)))

(defun niwa-migrate-from-mevedel--preset (value)
  "Rename a persisted preset symbol or string VALUE."
  (let ((name (if (symbolp value) (symbol-name value) value)))
    (if (and (stringp name) (string-prefix-p "mevedel-" name))
        (let ((new (concat "niwa-" (substring name 8))))
          (if (symbolp value) (intern new) new))
      value)))

(defun niwa-migrate-from-mevedel--instructions (record source kept)
  "Rename instruction properties, note settings and KEPT anchors from SOURCE in RECORD."
  (dolist (entry (plist-get record :files))
    (let ((path (niwa-migrate-from-mevedel--kept-path (car entry) source kept)))
      (unless (equal path (car entry))
        (when (assoc path (plist-get record :files))
          (error "Instruction anchors collide at %s" path))
        (setcar entry path)))
    (dolist (instruction (plist-get (cdr entry) :instructions))
      (cl-loop for cell on (plist-get instruction :properties) by #'cddr do
               (when (memq (car cell) (alist-get 'properties niwa-migrate-from-mevedel--legacy))
                 (when (eq (car cell) 'mevedel-note-settings)
                   (when-let* ((preset (plist-member (cadr cell) :preset)))
                     (setcar (cdr preset) (niwa-migrate-from-mevedel--preset (cadr preset)))))
                 (setcar cell (niwa-migrate-from-mevedel--preset (car cell)))))))
  record)

(defun niwa-migrate-from-mevedel--path-property (record)
  "Rename the path property of RECORD in place."
  (when-let* ((path (plist-member record :path)))
    (setcar (cdr path) (niwa-migrate-from-mevedel--paths (cadr path)))))

(defun niwa-migrate-from-mevedel--permissions (record)
  "Rename exact path components in RECORD's rules and grants."
  (dolist (rule (plist-get record :rules))
    (niwa-migrate-from-mevedel--path-property (cdr rule))
    (dolist (grant (plist-get (cdr rule) :file-system))
      (niwa-migrate-from-mevedel--path-property grant)))
  (mapc #'niwa-migrate-from-mevedel--path-property (plist-get record :resource-grants))
  record)

(defun niwa-migrate-from-mevedel--hooks (record source kept)
  "Rewrite command paths in Lisp hook RECORD from SOURCE using KEPT entries."
  (dolist (rule record)
    (dolist (group (cdr rule))
      (dolist (hook (plist-get group :hooks))
        (when-let* ((command (plist-member hook :command)))
          (setcar (cdr command) (niwa-migrate-from-mevedel--command (cadr command) source kept))))))
  record)

(defun niwa-migrate-from-mevedel--string-offset (literal prefix start reader)
  "Find PREFIX's end in LITERAL after START, decoding escapes with READER."
  (if (not (string-search "\\" literal))
      (1+ (length prefix))
    (or (cl-loop for end from start below (length literal)
                 when (equal prefix (ignore-errors
                                      (funcall reader (concat (substring literal 0 end) "\""))))
                 return end)
        (error "Cannot locate changed string token"))))

(defun niwa-migrate-from-mevedel--edit-string (start before after reader)
  "Patch the string from START to point from BEFORE to AFTER, using READER."
  (unless (equal before after)
    (let ((literal (buffer-substring-no-properties start (point)))
          (offset 0) (raw-start 1)
          (old "mevedel") (new "niwa") difference)
      ;; Locate each changed spelling in the decoded string, then edit its raw span.
      (while (not (eq t (setq difference (compare-strings before offset nil after offset nil))))
        (let* ((at (+ offset (1- (abs difference))))
               (end (+ at (length old)))
               (left (niwa-migrate-from-mevedel--string-offset literal (substring before 0 at) raw-start reader))
               (right (niwa-migrate-from-mevedel--string-offset literal (substring before 0 end) left reader)))
          (unless (and (equal old (substring before at end)) (string-prefix-p new (substring after at)))
            (error "Unexpected string change during cutover"))
          (setq literal (concat (substring literal 0 left) new (substring literal right))
                before (concat (substring before 0 at) new (substring before end))
                offset (+ at (length new))
                raw-start (+ left (length new)))))
      (unless (equal after (funcall reader literal)) (error "Invalid cutover string edit"))
      (delete-region start (point))
      (insert literal))))

(defun niwa-migrate-from-mevedel--edit-lisp (before after)
  "Walk one Lisp value at point, patching only changes from BEFORE to AFTER."
  (forward-comment (point-max))
  (cond
   ((equal before after) (read (current-buffer)))
   ((and (consp before) (eq (char-after) ?\())
    (forward-char)
    (while (consp before)
      (niwa-migrate-from-mevedel--edit-lisp (pop before) (pop after))
      (forward-comment (point-max))
      (when (looking-at "\\.[ \t\r\n;]")
        (forward-char)
        (niwa-migrate-from-mevedel--edit-lisp before after)
        (setq before nil)
        (forward-comment (point-max))))
    (unless (eq (char-after) ?\)) (error "Cannot locate changed Lisp list"))
    (forward-char))
   (t
    (let ((start (point))
          (value (read (current-buffer))))
      (unless (equal value before) (error "Cannot locate changed Lisp value"))
      (if (stringp before)
          (niwa-migrate-from-mevedel--edit-string start before after #'read)
        (goto-char start)
        (unless (and (symbolp before) (looking-at "mevedel-"))
          (error "Cannot locate legacy Lisp prefix"))
        (replace-match "niwa-" t t)
        (goto-char start)
        (read (current-buffer)))))))

(defun niwa-migrate-from-mevedel--edit-json (keys source kept)
  "Patch JSON hook commands at reversed KEYS using SOURCE and KEPT."
  (skip-chars-forward " \t\r\n")
  (let ((kind (pcase keys
                (`("command" nil "hooks" nil ,_ "hooks") 'command)
                ((or 'nil '("hooks") `(nil ,_ "hooks") `(nil "hooks" nil ,_ "hooks")) ?{)
                ((or `(,_ "hooks") `("hooks" nil ,_ "hooks")) ?\[))))
    (cond
     ((eq kind 'command)
      (let* ((start (point)) (command (json-parse-buffer)))
        (niwa-migrate-from-mevedel--edit-string
         start command (niwa-migrate-from-mevedel--command command source kept) #'json-parse-string)))
     ((eq kind (char-after))
      (forward-char)
      (skip-chars-forward " \t\r\n")
      (while (not (memq (char-after) '(?} ?\])))
        (let ((key (when (eq kind ?{) (prog1 (json-parse-buffer) (skip-chars-forward " \t\r\n:")))))
          (niwa-migrate-from-mevedel--edit-json (cons key keys) source kept))
        (skip-chars-forward " \t\r\n,"))
      (forward-char))
     (t (json-parse-buffer)))))

(defun niwa-migrate-from-mevedel--edit-config (path relative source kept)
  "Return a text-only rewrite of PATH at RELATIVE using SOURCE and KEPT."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (let* ((bytes (buffer-string))
           (original (decode-coding-string bytes 'utf-8-unix))
           (read-circle nil))
      (unless (equal bytes (encode-coding-string original 'utf-8-unix))
        (error "Cannot preserve original encoding of %s" path))
      (erase-buffer)
      (set-buffer-multibyte t)
      (insert original)
      (goto-char (point-min))
      (if (equal relative "hooks.json")
          (progn
            (json-parse-string original)
            (niwa-migrate-from-mevedel--edit-json nil source kept)
            (json-parse-string (buffer-string)))
        (let* ((before (niwa-migrate-from-mevedel--read-form path))
               (after (funcall (if (equal relative "hooks.el") #'niwa-migrate-from-mevedel--hooks
                                 #'niwa-migrate-from-mevedel--instructions)
                               (copy-tree before) source kept)))
          (goto-char (point-min))
          (niwa-migrate-from-mevedel--edit-lisp before after)
          (unless (equal after (niwa-migrate-from-mevedel--read-form path))
            (error "Invalid cutover edit in %s" path))))
      (unless (equal original (buffer-string)) (buffer-string)))))

(defun niwa-migrate-from-mevedel--transform (path relative source kept)
  "Rewrite PATH at RELATIVE from SOURCE using KEPT entries, or return nil."
  (cond
   ((or (member relative '("hooks.json" "hooks.el" "instructions.el"))
        (string-match-p "\\`exports/.+\\.el\\'" relative))
    (niwa-migrate-from-mevedel--edit-config path relative source kept))
   ((or (member relative '("skills-state.el" "permissions.el"))
        (string-match-p "\\`\\(?:schedules/[^/]+/schedule\\.el\\|workspace-schedules/[^/]+\\.el\\)\\'" relative))
    (let* ((record (niwa-migrate-from-mevedel--read path))
           (print-length nil) (print-level nil) (print-quoted t)
           (before (prin1-to-string record))
           (after
            (prin1-to-string
             (pcase relative
               ("permissions.el" (niwa-migrate-from-mevedel--permissions record))
               ((pred (string-prefix-p "workspace-schedules/"))
                (niwa-migrate-from-mevedel--paths record))
               ("skills-state.el"
                (when-let* ((keys (plist-member record :disabled-keys)))
                  (setcar (cdr keys) (mapcar #'niwa-migrate-from-mevedel--paths (cadr keys))))
                record)
               ((pred (string-prefix-p "schedules/"))
                (when-let* ((preset (plist-member record :preset)))
                  (setcar (cdr preset) (niwa-migrate-from-mevedel--preset (cadr preset))))
                record)))))
      (unless (equal before after) (concat after "\n"))))))

(defun niwa-migrate-from-mevedel--listing (directory &optional prefix)
  "Return DIRECTORY's safe tree as relative paths and stat data under PREFIX."
  (cl-loop for name in (directory-files directory nil directory-files-no-dot-files-regexp)
           for path = (file-name-concat directory name)
           for relative = (concat prefix name)
           for attr = (file-attributes path 'integer)
           for dir = (eq (file-attribute-type attr) t)
           append
           (progn
             (unless (and attr (or dir (eq (aref (file-attribute-modes attr) 0) ?-)))
               (error "Symbolic link or special file: %s" path))
             (cons (list relative dir (file-attribute-size attr)
                         (file-attribute-modification-time attr) (file-modes path))
                   (when dir (niwa-migrate-from-mevedel--listing path (concat relative "/")))))))

(defun niwa-migrate-from-mevedel--keep-p (relative user)
  "Classify RELATIVE within a USER or workspace store; refuse unknown families."
  (let* ((name (car (split-string relative "/")))
         (drop (if user 'drop-user 'drop-workspace)))
    (cond
     ((or (niwa-migrate-from-mevedel--legacy-p drop name)
          (equal name (concat niwa-migrate-from-mevedel--old "-auth"))
          (cl-some (lambda (prefix) (string-prefix-p (symbol-name prefix) name))
                   (alist-get 'scratch niwa-migrate-from-mevedel--legacy))) nil)
     ((niwa-migrate-from-mevedel--legacy-p (if user 'keep-user 'keep-workspace) name) t)
     ((and (not user) (string-match-p "\\`lobby-push-[^/]+\\.json\\'" name)) t)
     ((and (not user) (equal name "schedules"))
      (pcase (split-string relative "/")
        ((or '("schedules") `("schedules" ,_) `("schedules" ,_ "schedule.el")) t)
        ((or `("schedules" ,_ "runs" . ,_) `("schedules" ,_ "notify-receipts.json")) nil)
        (_ (error "Unknown schedule state: %s" relative))))
     ((and (not user) (equal name "state"))
      (pcase (split-string relative "/")
        ((or '("state") `("state" "media" . ,_) `("state" "plugin-data" . ,_)) t)
        (`("state" ,(or "journal" "shared-notes" "diagnostics" "memory-write") . ,_) nil)
        (_ (error "Unknown private state: %s" relative))))
     (t (error "Unknown %s state family: %s" (if user "user" "workspace") relative)))))

(defun niwa-migrate-from-mevedel--lease (directory)
  "Refuse a live or unsettled lease in DIRECTORY."
  (let* ((names (directory-files directory t "\\`[0-9]\\{20\\}\\.el\\'"))
         (head (cl-loop for path in (reverse names)
                        for lease = (niwa-migrate-from-mevedel--read path)
                        for status = (plist-get lease :status)
                        do (unless (memq status '(aborted released active claiming publishing))
                             (error "Invalid lease record: %s" path))
                        unless (eq status 'aborted) return lease)))
    (when (or (and names (not head))
              (and head
                   (or (plist-get head :unsettled-mutation)
                       (not (or (eq 'released (plist-get head :status))
                                (and (eq 'active (plist-get head :status))
                                     (numberp (plist-get head :expires-at))
                                     (<= (plist-get head :expires-at) (float-time))))))))
      (error "Live, invalid or unsettled lease: %s" directory))))

(defun niwa-migrate-from-mevedel--expiry (source path)
  "Refuse an accepted expiry manifest at PATH without a completion receipt in SOURCE."
  (unless (file-exists-p (concat path ".done"))
    (let* ((manifest (niwa-migrate-from-mevedel--json path))
           (generation (gethash "generation" manifest))
           (owner (gethash "owner" manifest)))
      (unless (and (natnump generation) (> generation 0) (stringp owner))
        (error "Invalid journal expiry manifest: %s" path))
      (let ((outcome (file-name-concat source "state/journal/mutation"
                                       (format "%020d.outcome" generation))))
        (when (file-exists-p outcome)
          (let ((record (niwa-migrate-from-mevedel--json outcome)))
            (unless (and (equal generation (gethash "generation" record))
                         (equal owner (gethash "owner" record)))
              (error "Invalid journal expiry settlement: %s" outcome))
            (when (equal "completed" (gethash "status" record))
              (error "Unfinished accepted journal expiry: %s" path))))))))

(defun niwa-migrate-from-mevedel--closed (source rows)
  "Require the legacy SOURCE described by ROWS to be quiescent."
  (dolist (lock '("schedules" "state/memory-write" "state/memory-write/root" "state/shared-notes"))
    (let ((path (file-name-concat source lock)))
      (when (and (file-directory-p path)
                 (not (equal 0 (call-process "flock" nil nil nil "-n" path "true"))))
        (error "Active writer lock: %s" path))))
  (dolist (row rows)
    (let* ((relative (car row)) (dir (cadr row))
           (path (file-name-concat source relative)))
      (cond
       ((and dir (string-prefix-p ".file-batch-" (file-name-nondirectory relative)))
        (error "Unfinished file batch: %s" path))
       ((equal relative "state/shared-notes/cycle.el")
        (when (plist-get (niwa-migrate-from-mevedel--read path) :inflight)
          (error "Inflight shared-note cycle: %s" path)))
       ((and dir (string-match-p "\\(?:\\(?:^\\|/\\)\\.lease\\|^leases/artifacts/[^/]+\\)\\'" relative))
        (niwa-migrate-from-mevedel--lease path))
       ((and (not dir) (string-match-p "\\`sessions/[^/]+/\\.lock\\'" relative))
        (error "Live session lock: %s" path))
       ((and (not dir) (string-match-p "\\`schedules/[^/]+/runs/[^/]+\\.el\\'" relative))
        (let ((run (niwa-migrate-from-mevedel--read path)))
          (unless (or (plist-get run :outcome) (plist-get run :settled-at))
            (error "Unsettled schedule run: %s" path))))
       ((and (not dir) (string-match-p
                        "\\`state/\\(?:memory-write\\|shared-notes\\)/\\(?:[^/]+/\\)*pending/"
                        relative))
        (error "Unfinished memory or shared-note write: %s" path))
       ((and (not dir) (string-match-p "\\`state/journal/expiry/[^/]+\\.json\\'" relative))
        (niwa-migrate-from-mevedel--expiry source path))))))

(defun niwa-migrate-from-mevedel--pending-captures (source rows)
  "Return pending capture IDs from legacy SOURCE listed in ROWS."
  (cl-loop for (relative dir) in rows
           for path = (file-name-concat source relative)
           when (and dir (string-match-p "\\`state/journal/captures/[^/]+\\'" relative)
                     (file-exists-p (file-name-concat path "ready"))
                     (not (file-exists-p (file-name-concat path "retired")))
                     (not (file-exists-p (file-name-concat path "discard.json"))))
           collect (file-name-nondirectory relative)))

(defun niwa-migrate-from-mevedel--hash (path)
  "Return SHA-256 of the literal bytes at PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun niwa-migrate-from-mevedel--stage (source stage rows)
  "Copy and validate selected ROWS from SOURCE into the private STAGE."
  (let ((kept (make-hash-table :test #'equal)))
    (dolist (row rows) (puthash (car row) t kept))
    (dolist (row rows)
      (pcase-let* ((`(,relative ,dir ,_size ,_mtime ,mode) row)
                   (from (file-name-concat source relative))
                   (to (file-name-concat stage relative)))
        (if dir
            (make-directory to)
          (copy-file from to nil t nil t)
          (if-let* ((text (niwa-migrate-from-mevedel--transform from relative source kept)))
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region text nil to nil 'silent)
                (unless (equal (secure-hash 'sha256 (encode-coding-string text 'utf-8-unix))
                               (niwa-migrate-from-mevedel--hash to))
                  (error "Staging rewrite changed bytes: %s" to)))
            (unless (equal (niwa-migrate-from-mevedel--hash from)
                           (niwa-migrate-from-mevedel--hash to))
              (error "Staging copy changed bytes: %s" to)))
          (set-file-modes to mode)))))
  (dolist (row (reverse rows))
    (when (cadr row)
      (set-file-modes (file-name-concat stage (car row)) (nth 4 row))))
  (unless (equal (mapcar #'car rows)
                 (mapcar #'car (niwa-migrate-from-mevedel--listing stage)))
    (error "Unexpected paths in stage %s" stage)))

(defun niwa-migrate-from-mevedel--tracked-p (parent)
  "Return non-nil if PARENT has Git-tracked legacy state."
  (when (executable-find "git")
    (let ((default-directory (file-name-as-directory parent)))
      (with-temp-buffer
        (and (equal 0 (call-process "git" nil (list t nil) nil
                                    "ls-files" "--" (concat niwa-migrate-from-mevedel--old "/")))
             (> (buffer-size) 0))))))

(defun niwa-migrate-from-mevedel--receipt (source rows)
  "Bind a validated promotion to the identity and observed ROWS of SOURCE."
  (let ((attributes (file-attributes source 'integer))
        (print-length nil) (print-level nil))
    (secure-hash 'sha256
                 (prin1-to-string (list source (file-attribute-device-number attributes)
                                        (file-attribute-inode-number attributes) rows)))))

(defun niwa-migrate-from-mevedel--physical (directory)
  "Return physical DIRECTORY, refusing any linked path component."
  (when (file-remote-p directory) (error "Migration requires a local directory"))
  (let ((path (directory-file-name directory)))
    ;; Walk the original spelling, so a parent traversal cannot hide a link.
    (while path
      (unless (equal (directory-file-name (expand-file-name path))
                     (directory-file-name (file-truename path)))
        (error "Refusing symbolic link in path: %s" path))
      (let ((parent (and (file-name-directory path)
                         (directory-file-name (file-name-directory path)))))
        (setq path (unless (equal parent path) parent))))
    (directory-file-name (expand-file-name directory))))

(defun niwa-migrate-from-mevedel--migrate (directory)
  "Migrate DIRECTORY without changing the original bytes.
Return the backup name, or nil when already migrated."
  (let* ((case-fold-search nil)
         (directory (niwa-migrate-from-mevedel--physical directory))
         (user (equal (file-name-nondirectory directory) niwa-migrate-from-mevedel--old))
         (parent (if user (file-name-directory directory) directory))
         (source (file-name-concat parent niwa-migrate-from-mevedel--old))
         (target (file-name-concat parent ".niwa"))
         (stage (file-name-concat parent ".niwa.staging"))
         (marker (file-name-concat target niwa-migrate-from-mevedel--marker))
         (backup (concat source ".pre-niwa-" (format-time-string "%Y%m%d-%H%M%S"))))
    (when (file-symlink-p source) (error "Refusing symbolic link: %s" source))
    (if (and (not (niwa-migrate-from-mevedel--present-p source))
             (file-directory-p target) (not (file-symlink-p target)))
        nil
      (unless (file-directory-p source) (error "No legacy directory: %s" source))
      (when (niwa-migrate-from-mevedel--present-p backup)
        (error "Backup already exists: %s" backup))
      (let* ((recover (niwa-migrate-from-mevedel--present-p target))
             (rows (niwa-migrate-from-mevedel--listing source))
             (receipt (niwa-migrate-from-mevedel--receipt source rows))
             (captures (niwa-migrate-from-mevedel--pending-captures source rows))
             (tracked (niwa-migrate-from-mevedel--tracked-p parent)))
        (niwa-migrate-from-mevedel--closed source rows)
        (if recover
            (progn
              (unless (and (not (file-symlink-p target)) (file-directory-p target)
                           (not (file-symlink-p marker)) (file-regular-p marker)
                           (= 64 (file-attribute-size (file-attributes marker)))
                           (equal (secure-hash 'sha256 receipt) (niwa-migrate-from-mevedel--hash marker)))
                (error "Destination exists without a matching cutover receipt: %s" target))
              (rename-file source backup))
          (let ((keep (cl-remove-if-not (lambda (row) (niwa-migrate-from-mevedel--keep-p (car row) user))
                                        rows))
                promoted)
            (when (niwa-migrate-from-mevedel--present-p stage)
              (unless (and (file-directory-p stage) (not (file-symlink-p stage)))
                (error "Unsafe staging directory: %s" stage))
              (delete-directory stage t))
            (make-directory stage)
            (set-file-modes stage #o700)
            (unwind-protect
                (progn
                  (niwa-migrate-from-mevedel--stage source stage keep)
                  (let ((current (niwa-migrate-from-mevedel--listing source)))
                    (unless (equal receipt (niwa-migrate-from-mevedel--receipt source current))
                      (error "Source changed during staging: %s" source))
                    (niwa-migrate-from-mevedel--closed source current))
                  (write-region receipt nil (file-name-concat stage niwa-migrate-from-mevedel--marker)
                                nil 'silent)
                  (rename-file stage target)
                  (setq promoted t)
                  (rename-file source backup)
                  (setq promoted nil))
              (when promoted (rename-file target stage))
              (when (file-directory-p stage) (delete-directory stage t)))))
        (delete-file marker)
        (when captures
          (message "Pending journal captures left in backup for %s: %s" directory (string-join captures ", ")))
        (when tracked (message "Commit the tracked state move for %s" parent))
        (when (file-exists-p (file-name-concat backup "trusted-hooks.el"))
          (message "Hook approvals were not carried over; re-approve the migrated hook configs"))
        backup))))

(defun niwa-migrate-from-mevedel-main ()
  "Migrate the command-line directories and exit nonzero on refusal."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (when (equal (car args) "--") (pop args))
    (condition-case err
        (progn
          (unless args (error "Supply a workspace root or legacy user directory"))
          (dolist (directory args)
            (if-let* ((backup (niwa-migrate-from-mevedel--migrate directory)))
                (princ (format "Migrated %s (backup: %s)\n" directory backup))
              (princ (format "Already migrated: %s\n" directory))))
          (kill-emacs 0))
      (error (message "niwa cutover refused: %s" (error-message-string err))
             (kill-emacs 1)))))

(provide 'niwa-migrate-from-mevedel)
;;; migrate-from-mevedel.el ends here
