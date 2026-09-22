;;; mevedel-memory-scope.el -- Captured consolidation file scope -*- lexical-binding: t -*-

;;; Commentary:

;; Captures bounded memory and instruction before-state without changing files.
;; Root IDs retain their original physical path and local client identity;
;; they must never be resolved through a later client's memory configuration.
;; Complete name observations prove absence, not freshness at application.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-execution-target)
(require 'mevedel-memory-proposal)
(require 'mevedel-resource)
(require 'mevedel-session-control-fs)
(require 'mevedel-structs)
(require 'mevedel-system)
(require 'mevedel-utilities)
(require 'mevedel-workspace-identity)

(define-error 'mevedel-memory-scope-unavailable "Original memory scope is unavailable")

(defconst mevedel-memory-scope--max-entries 256
  "Maximum directory entries examined per memory root, including directories.")

(defconst mevedel-memory-scope--max-file-bytes (* 32 1024)
  "Maximum complete bytes admitted from any one memory or instruction file.")

(defconst mevedel-memory-scope--max-bytes (* 96 1024)
  "Maximum total before-state bytes captured for one consolidation pass.")

(defun mevedel-memory-scope--inventory (directory)
  "Return DIRECTORY's bounded names and whether their observation is complete.
Include directory and symlink names but never descend through symlinks."
  (let ((pending (list directory)) (remaining mevedel-memory-scope--max-entries)
        names directories (complete t))
    (while (and pending (> remaining 0))
      (let* ((parent (pop pending))
             (children (and (file-exists-p parent)
                            (directory-files parent t directory-files-no-dot-files-regexp
                                             nil (1+ remaining)))))
        (when (> (length children) remaining)
          (setq complete nil children (seq-take children remaining)))
        (dolist (path children)
          (cl-decf remaining)
          (push (file-relative-name path directory) names)
          (when (and (not (equal (file-name-nondirectory path) ".mevedel"))
                     (not (file-symlink-p path)) (file-directory-p path))
            (push (file-relative-name path directory) directories)
            (push path pending)))))
    (list :existing (sort names #'string<) :directories (sort directories #'string<)
          :complete (and complete (not pending)))))

(defun mevedel-memory-scope--snapshot (path limit &optional result)
  "Capture complete literal PATH bytes, bounded by LIMIT, or expected absence.
Use the pinned target read so a changed parent cannot redirect the read.
RESULT, when supplied, is the pinned read result for PATH from a bounded batch."
  (condition-case nil
      (let* ((bytes (if result
                        (progn
                          (unless (equal path (plist-get result :path))
                            (error "Snapshot result names another path"))
                          (mevedel-session-control-fs-program-value result))
                      (mevedel-session-control-fs-read-file path 'no-conversion (1+ limit))))
             (text (decode-coding-string bytes 'utf-8-unix)))
        (unless (and (<= (length bytes) limit)
                     (not (string-search "\0" bytes))
                     (not (cl-some #'mevedel--invalid-message-char-p text))
                     (equal bytes (encode-coding-string text 'utf-8-unix)))
          (error "Memory file exceeds its bound or is not UTF-8 text"))
        (list :path path :exists t :bytes bytes :hash (secure-hash 'sha256 bytes)
              :mode (file-modes path)))
    (mevedel-session-control-fs-absent (list :path path :exists nil))))

(defun mevedel-memory-scope-capture (workspace &optional selection configuration)
  "Capture WORKSPACE's memory and root instruction scopes without writes.
Optional SELECTION is (CONFIGURED-ROOT . TOPIC) for a direct user operation;
only that topic and its index are captured from the configured root.
CONFIGURATION optionally freezes :roots and :instructions for a storage worker.
Return :roots, an alist usable by `mevedel-memory-proposal-parse', together
with the original workspace path and identity. Each root retains :dir,
:configured-dir, :client (for local paths), and :before snapshots keyed by
relative file name. :files contains only admitted topics or instructions.
Memory indexes are retained in :before for transactional application.

An incomplete name observation never authorizes creation. Oversized,
unreadable, linked, or non-text files are omitted as complete files, with
reasons in :omissions. An unavailable index excludes its entire memory root.
The request owner must account for these snapshots in context admission."
  (let* ((workspace-root (file-name-as-directory
                          (file-truename (mevedel-workspace-root workspace))))
         (client (mevedel-workspace-identity-client))
         (remaining mevedel-memory-scope--max-bytes)
         (memory-roots (if configuration (plist-get configuration :roots)
                         (mevedel-system--memory-roots workspace)))
         roots omissions excluded-roots)
    (when (> (length memory-roots) 16)
      (error "Too many configured memory roots for one review"))
    (when selection
      (unless (and (mevedel-memory-proposal--file-p (cdr selection))
                   (not (equal (file-name-nondirectory (cdr selection)) "MEMORY.md")))
        (error "Invalid selected memory topic"))
      (setq memory-roots (seq-filter
                          (lambda (root) (equal (file-name-as-directory (plist-get root :dir))
                                                (file-name-as-directory (car selection)))) memory-roots))
      (unless memory-roots (error "Selected memory root is no longer configured")))
    (dolist (descriptor (append memory-roots (unless selection (list (list :dir workspace-root :kind 'instructions)))))
      (let* ((configured (file-name-as-directory (expand-file-name (plist-get descriptor :dir))))
             (directory (file-name-as-directory (file-truename configured)))
             (kind (or (plist-get descriptor :kind) 'memory))
             (origin (unless (file-remote-p directory) client))
             (id (secure-hash 'sha256 (prin1-to-string (list kind directory origin))))
             (target (mevedel-execution-target-create directory))
             (root (list :kind kind :label (or (plist-get descriptor :label) "Workspace instructions")
                         :dir directory :configured-dir configured :client origin
                         :target (mevedel-execution-target-identity target)))
             files before unavailable)
        (when (eq kind 'memory)
          (push configured excluded-roots)
          (push directory excluded-roots))
        (unless (assoc id roots)
          (condition-case err
              (let* ((inventory (and (eq kind 'memory) (mevedel-memory-scope--inventory directory)))
                     (candidates
                      (if (eq kind 'memory)
                          (cons "MEMORY.md" (cl-remove-if-not
                                             (lambda (name) (and (string-suffix-p ".md" name)
                                                                 (not (equal name "MEMORY.md"))))
                                             (plist-get inventory :existing)))
                        (delete-dups
                         (append '("AGENTS.md" "AGENTS.local.md")
                                 (mapcar (lambda (path) (file-relative-name path directory))
                                         (if configuration (plist-get configuration :instructions)
                                           (mevedel-system-workspace-config-files workspace))))))))
                (when selection (setq candidates (list "MEMORY.md" (cdr selection))))
                (setq root (append root inventory))
                (when (and inventory (not (plist-get inventory :complete)))
                  (push (list :root id :reason "Directory inventory reached its entry limit") omissions))
                (let (results)
                  (while candidates
                    ;; Cap transfer/allocation independently of the admitted
                    ;; scope.  Each file still gets the shrinking byte limit.
                    (unless results
                      (setq results
                            (mevedel-session-control-fs-run-program
                             (mapcar
                              (lambda (file)
                                (list :op 'read :optional t
                                      :path (file-name-concat directory file)
                                      :coding 'no-conversion
                                      :max-bytes (1+ (min remaining mevedel-memory-scope--max-file-bytes))))
                              (seq-take candidates 8)))))
                    (let ((file (pop candidates)) (result (pop results)))
                      (condition-case failure
                          (let* ((path (file-name-concat directory file))
                                 (snapshot (mevedel-memory-scope--snapshot
                                            path (min remaining mevedel-memory-scope--max-file-bytes) result)))
                            ;; A name observed and then removed is not an admitted
                            ;; existing topic, nor proof of absence at capture time.
                            (when (and (eq kind 'memory) (not (equal file "MEMORY.md"))
                                       (not (plist-get snapshot :exists)))
                              (error "Memory file disappeared during capture"))
                            (cl-decf remaining (length (plist-get snapshot :bytes)))
                            (push (cons file snapshot) before)
                            (unless (and (eq kind 'memory) (equal file "MEMORY.md"))
                              (push file files)))
                        (error
                         (push (list :root id :file file :reason (error-message-string failure)) omissions)
                         (when (equal file "MEMORY.md") (setq unavailable t)))))))
                (unless unavailable
                  (push (cons id (append root (list :files (nreverse files) :before (nreverse before)))) roots)))
            (error (push (list :root id :reason (error-message-string err)) omissions))))))
    (list :workspace-root workspace-root :workspace (mevedel-workspace-identity-read workspace-root)
          :workspace-client (unless (file-remote-p workspace-root) client)
          :workspace-target (mevedel-execution-target-identity
                             (mevedel-execution-target-create workspace-root))
          :excluded-roots (delete-dups excluded-roots)
          :roots (nreverse roots) :omissions (nreverse omissions))))

(defun mevedel-memory-scope-source-path (scope relative)
  "Resolve workspace source RELATIVE within the original captured SCOPE.
This grants no writes. Private state, VCS metadata, and all configured memory
roots are excluded, including roots whose contents could not be captured.
The returned path can be absent; the caller chooses the bounded read or search.
Symlinks, traversal, other targets, and another local client are rejected."
  (let ((root (plist-get scope :workspace-root))
        (client (plist-get scope :workspace-client))
        (remote-file-name-inhibit-cache t))
    (unless (or (not client) (equal client (mevedel-workspace-identity-client)))
      (signal 'mevedel-memory-scope-unavailable '("Workspace belongs to another client")))
    (unless (and (mevedel-memory-proposal-relative-path-p relative)
                 (not (cl-intersection '(".git" ".hg" ".svn" ".mevedel")
                                       (split-string relative "/") :test #'equal)))
      (error "Path is outside the captured workspace source scope"))
    (unless (and (equal root (file-name-as-directory (file-truename root)))
                 (equal (plist-get scope :workspace) (mevedel-workspace-identity-read root))
                 (equal (plist-get scope :workspace-target)
                        (mevedel-execution-target-identity (mevedel-execution-target-create root))))
      (signal 'mevedel-memory-scope-unavailable '("Original workspace target changed")))
    (let ((path (expand-file-name relative root)))
      (unless (and (not (cl-some
                         (lambda (memory)
                           (or (equal (directory-file-name memory) (directory-file-name path))
                               (string-prefix-p memory path)))
                         (plist-get scope :excluded-roots)))
                   (mevedel-resource-within-root-p path root))
        (error "Path is outside the captured workspace source scope"))
      path)))

(defun mevedel-memory-scope--root (scope id)
  "Return captured root ID in SCOPE after checking its original authority."
  (let* ((root (cdr (assoc id (plist-get scope :roots))))
         (directory (plist-get root :dir))
         (client (plist-get root :client)))
    ;; Check local origin before any filesystem access, including inspection.
    (unless (and root (or (not client) (equal client (mevedel-workspace-identity-client))))
      (signal 'mevedel-memory-scope-unavailable '("Root belongs to another client or is unknown")))
    (unless (and (equal directory (file-name-as-directory
                                   (file-truename (plist-get root :configured-dir))))
                 (equal (plist-get scope :workspace)
                        (mevedel-workspace-identity-read (plist-get scope :workspace-root)))
                 (equal (plist-get root :target)
                        (mevedel-execution-target-identity (mevedel-execution-target-create directory))))
      (signal 'mevedel-memory-scope-unavailable '("Original root or workspace identity changed")))
    root))

(defun mevedel-memory-scope-before (scope id file)
  "Return captured before-state for FILE in SCOPE's root ID.
Check original root authority even for inspection. New memory paths are
admitted only by a complete frozen name observation, including their parent
components. Never sample a newly proposed target to manufacture before-state."
  (let* ((root (mevedel-memory-scope--root scope id))
         (before (cdr (assoc file (plist-get root :before))))
         (path (file-name-concat (plist-get root :dir) file)))
    (unless (mevedel-memory-proposal--file-p file)
      (error "Invalid captured memory file"))
    (or before
        (when (and (eq (plist-get root :kind) 'memory)
                   (plist-get root :complete)
                   (not (member file (plist-get root :existing))))
          (let ((parent (file-name-directory file)))
            (while parent
              (setq parent (directory-file-name parent))
              (when (and (member parent (plist-get root :existing))
                         (not (member parent (plist-get root :directories))))
                (error "Proposed parent was not an ordinary captured directory"))
              (setq parent (file-name-directory parent))))
          (list :path path :exists nil))
        (error "File was not admitted to this review"))))

(defun mevedel-memory-scope-check (scope id files)
  "Return freshness status for FILES in captured root ID of SCOPE.
The result has :status `fresh', `stale', or `unavailable', and a :reason for
non-fresh states. This is a read-only check, not write coordination: callers
must hold current ownership and target coordination across check and commit."
  (condition-case err
      (let ((root (mevedel-memory-scope--root scope id)) stale)
        (dolist (file files)
          (let* ((before (mevedel-memory-scope-before scope id file))
                 (path (file-name-concat (plist-get root :dir) file))
                 (bytes (plist-get before :bytes))
                 (current
                  (condition-case nil
                      (list :exists t :bytes
                            (mevedel-session-control-fs-read-file path 'no-conversion (1+ (length bytes))))
                    (mevedel-session-control-fs-absent (list :exists nil)))))
            (unless (and (equal path (plist-get before :path))
                         (or (not (plist-get before :exists))
                             (equal (plist-get before :hash) (secure-hash 'sha256 bytes))))
              (error "Captured before-state is inconsistent"))
            (unless (and (eq (plist-get before :exists) (plist-get current :exists))
                         (equal bytes (plist-get current :bytes)))
              (push file stale))))
        (if stale (list :status 'stale :reason (format "Files changed since capture: %s" (string-join (nreverse stale) ", ")))
          (list :status 'fresh)))
    (error (list :status 'unavailable :reason (error-message-string err)))))

(provide 'mevedel-memory-scope)
;;; mevedel-memory-scope.el ends here
