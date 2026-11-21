;;; niwa-collaboration-files.el --- Browser project files -*- lexical-binding: t; -*-

;;; Commentary:

;; Lets browser guests with a full or owner link browse, read, upload and
;; remove the files of a workspace's project.  The lobby serves all four;
;; a session room accepts uploads only, so a prompt's attachments can also
;; be added to the project.
;;
;; The project's own file listing is the authority: `project-files', which
;; follows the VC ignore rules in a repository and the transient project's
;; ignores elsewhere, plus the same listing of each Git repository cloned
;; untracked inside it, minus the workspace state under `.niwa/'.  A guest
;; path or folder is accepted only when that listing shows it, then
;; re-verified beneath the root before any I/O, so an ignored, private or
;; symlinked file is neither readable nor writable from a browser.  An
;; upload writes one new file into a listed folder and never replaces one;
;; removal moves a file to the trash.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'project)

;; `niwa-collaboration'
(declare-function niwa-collaboration--guest
                  "niwa-collaboration" (room peer))

;; `niwa-collaboration-artifact'
(declare-function niwa-collaboration--artifact-mime
                  "niwa-collaboration-artifact" (name))
(declare-function niwa-collaboration--send-chunked
                  "niwa-collaboration-artifact"
                  (transport peer meta content))
(defvar niwa-collaboration--max-artifact-bytes)

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--request-id-p
                  "niwa-collaboration-guest" (value))
(declare-function niwa-collaboration--utf8-text-p
                  "niwa-collaboration-guest" (bytes))

;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-send
                  "niwa-collaboration-transport" (transport peer frame))

;; `niwa-resource'
(declare-function niwa-resource-within-root-p
                  "niwa-resource" (path root))
(autoload 'niwa-resource-within-root-p "niwa-resource")


;;
;;; Listing

(defconst niwa-collaboration-files--max-entries 1000
  "Most entries one folder listing carries, folders first.")

(defconst niwa-collaboration-files--max-upload-bytes (* 16 1024 1024)
  "Largest file a guest may upload into the project.")

(defconst niwa-collaboration-files--private-path-regexp
  "\\(?:\\`\\|/\\)\\.niwa\\(?:/\\|\\'\\)"
  "Workspace state component excluded from every project file path.")

(defun niwa-collaboration-files--listing (root)
  "Return the project files under ROOT as sorted names relative to ROOT.
A Git repository nested untracked in ROOT contributes its own listing.
The workspace state under `.niwa/' is never listed, at any depth."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (default-directory root)
         ;; The VC backend, not whatever the user registered: a cached
         ;; backend such as projectile's lists stale files.
         (project (or (let ((project-find-functions (list #'project-try-vc)))
                        (project-current nil root))
                      (cons 'transient root)))
         (project-files-relative-names nil))
    (sort (nconc
           (cl-loop for file in (project-files project (list root))
                    for name = (file-relative-name file root)
                    unless (or (string-prefix-p "../" name)
                               (string-match-p niwa-collaboration-files--private-path-regexp name))
                    collect name)
           (cl-loop for nested in (niwa-collaboration-files--nested-repositories
                                   project root)
                    nconc (mapcar (lambda (name) (concat nested name))
                                  (niwa-collaboration-files--listing
                                   (file-name-concat root nested)))))
          #'string-lessp)))

(defun niwa-collaboration-files--nested-repositories (project root)
  "Return the Git repositories nested untracked in PROJECT under ROOT.
Names are relative to ROOT and end in a slash.  Git reports such a
repository, a clone kept inside the project, as one directory entry,
which `project-files' drops; the model's search still reads its files.
One the project ignores stays out, and untracked material at all only
when `project-vc-include-untracked' allows it."
  (when (and (eq (car-safe project) 'vc)
             project-vc-include-untracked
             (eq (ignore-errors (vc-responsible-backend root)) 'Git))
    (with-temp-buffer
      (setq default-directory (file-name-as-directory root))
      (when (zerop (process-file "git" nil t nil "ls-files" "-z" "--others"
                                 "--exclude-standard"))
        (cl-loop for name in (split-string (buffer-string) "\0" t)
                 when (and (directory-name-p name)
                           (not (string-match-p niwa-collaboration-files--private-path-regexp name))
                           (file-exists-p (file-name-concat name ".git")))
                 collect name)))))

(defun niwa-collaboration-files--folder-p (listing dir)
  "Return non-nil when DIR is the root \"\" or a folder holding LISTING files."
  (and (stringp dir)
       (or (string-empty-p dir)
           (let ((prefix (concat dir "/")))
             (cl-some (lambda (name) (string-prefix-p prefix name)) listing)))))

(defun niwa-collaboration-files--entries (root listing dir)
  "Return the entries of folder DIR of LISTING under ROOT, and the omitted count.
The value is (ENTRIES . OMITTED).  Each entry is a plist of `:name',
`:kind' \"dir\" or \"file\" and, for a file, `:size'.  Folders lead and
both groups keep LISTING's order.  A listed name that is no longer a
plain file or folder on disk -- deleted, or a symlink -- is left out."
  (let* ((prefix (if (string-empty-p dir) "" (concat dir "/")))
         (attributes (make-hash-table :test #'equal))
         dirs files)
    (pcase-dolist (`(,name . ,attrs)
                   (ignore-errors
                     (directory-files-and-attributes
                      (file-name-concat root dir) nil nil t)))
      (puthash name attrs attributes))
    (dolist (name listing)
      (when (string-prefix-p prefix name)
        (let* ((rest (substring name (length prefix)))
               (slash (string-search "/" rest)))
          (if slash
              ;; A folder's files are contiguous in the sorted listing.
              (let ((folder (substring rest 0 slash)))
                (unless (equal folder (car dirs))
                  (push folder dirs)))
            (push rest files)))))
    (let ((entries
           (nconc
            (cl-loop for folder in (nreverse dirs)
                     when (eq t (file-attribute-type
                                 (gethash folder attributes)))
                     collect (list :name folder :kind "dir"))
            (cl-loop for file in (nreverse files)
                     for attrs = (gethash file attributes)
                     when (and attrs (null (file-attribute-type attrs)))
                     collect (list :name file :kind "file"
                                   :size (file-attribute-size attrs))))))
      (cons (seq-take entries niwa-collaboration-files--max-entries)
            (max 0 (- (length entries)
                      niwa-collaboration-files--max-entries))))))

(defun niwa-collaboration-files--file (root listing path)
  "Return the absolute name of listed regular file PATH under ROOT.
Signal an error unless LISTING shows PATH and it still resolves to a
plain file beneath ROOT."
  (let ((file (and (stringp path) (member path listing)
                   (file-name-concat root path))))
    (unless (and file
                 (niwa-resource-within-root-p file root)
                 (file-regular-p file))
      (error "This file is not in the project"))
    file))

(defun niwa-collaboration-files--parent (path)
  "Return the folder holding project PATH, \"\" for the root."
  (directory-file-name (or (file-name-directory path) "")))

(defun niwa-collaboration-files--name-p (name)
  "Return non-nil when NAME is a plain new file name a guest may choose.
It must be one path component that is neither hidden nor a home
directory reference."
  (and (stringp name)
       (<= 1 (string-bytes name) 255)
       (not (string-match-p "[/\\\\[:cntrl:]]" name))
       (not (string-match-p "\\`[.~]" name))))


;;
;;; Guest frames

(defun niwa-collaboration-files--reply (owner peer type req-id result)
  "Send PEER in OWNER the TYPE answer to REQ-ID with plist RESULT."
  (niwa-collaboration--transport-send
   (plist-get owner :transport) peer
   (append (list :t type :reqId req-id) result)))

(defun niwa-collaboration-files--announce (owner dir)
  "Tell OWNER's writable guests that folder DIR changed.
View links see no project files, so they are not told either."
  (maphash (lambda (peer guest)
             (when (plist-get guest :writable)
               (niwa-collaboration--transport-send
                (plist-get owner :transport) peer
                (list :t "files-changed" :dir dir))))
           (plist-get owner :guests)))

(defmacro niwa-collaboration-files--answer (owner peer frame type &rest body)
  "Answer PEER's request FRAME in OWNER with TYPE and BODY's result plist.
BODY runs only for a registered guest with a valid request id, bound to
`guest' and `req-id'.  A guest without a full link and any error BODY
signals are answered as `:error' messages, never raised: a fault here is
this request's problem, not the room's."
  (declare (indent 4) (debug t))
  `(let ((guest (niwa-collaboration--guest ,owner ,peer))
         (req-id (plist-get ,frame :reqId)))
     (when (and guest (niwa-collaboration--request-id-p req-id))
       (niwa-collaboration-files--reply
        ,owner ,peer ,type req-id
        (condition-case err
            (if (plist-get guest :writable)
                (progn ,@body)
              (error "A view link cannot use the project files"))
          (error (list :error (error-message-string err))))))))

(defun niwa-collaboration-files-handle-list (owner peer frame root)
  "Answer PEER's folder listing FRAME in OWNER for the project at ROOT."
  (niwa-collaboration-files--answer owner peer frame "files"
    (let ((listing (niwa-collaboration-files--listing root))
          (dir (plist-get frame :dir)))
      (unless (niwa-collaboration-files--folder-p listing dir)
        (error "This folder is not in the project"))
      (pcase-let ((`(,entries . ,omitted)
                   (niwa-collaboration-files--entries root listing dir)))
        (list :dir dir :entries (vconcat entries) :omitted omitted)))))

(defun niwa-collaboration-files--mime (name content)
  "Return the transfer type for project file NAME holding CONTENT.
Source and configuration files have no type of their own, so any
untyped UTF-8 text previews as plain text."
  (let ((mime (niwa-collaboration--artifact-mime name)))
    (if (and (equal mime "application/octet-stream")
             (niwa-collaboration--utf8-text-p content))
        "text/plain"
      mime)))

(defun niwa-collaboration-files-handle-get (owner peer frame root)
  "Send PEER in OWNER the project file FRAME names under ROOT.
The bytes travel as `file' chunk frames; a refusal is one `file' frame
with an `:error'."
  (let ((guest (niwa-collaboration--guest owner peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (niwa-collaboration--request-id-p req-id))
      (condition-case err
          (let* ((_ (unless (plist-get guest :writable)
                      (error "A view link cannot use the project files")))
                 (path (plist-get frame :path))
                 (file (niwa-collaboration-files--file
                        root (niwa-collaboration-files--listing root) path))
                 (content (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (insert-file-contents-literally
                             file nil 0
                             (1+ niwa-collaboration--max-artifact-bytes))
                            (buffer-string))))
            (when (> (length content) niwa-collaboration--max-artifact-bytes)
              (error "File too large to send (over %d MB); open it on the host"
                     (/ niwa-collaboration--max-artifact-bytes 1024 1024)))
            (niwa-collaboration--send-chunked
             (plist-get owner :transport) peer
             (list :t "file" :reqId req-id :path path
                   :name (file-name-nondirectory path)
                   :mime (niwa-collaboration-files--mime path content)
                   :size (length content))
             content))
        (error
         (niwa-collaboration-files--reply
          owner peer "file" req-id
          (list :error (error-message-string err))))))))

(defun niwa-collaboration-files--free-path (root path)
  "Return PATH under ROOT, or its first free numbered variant, or nil.
\"a.png\" becomes \"a-2.png\", then \"a-3.png\", up to a bound."
  (let ((base (file-name-sans-extension path))
        (extension (file-name-extension path t)))
    (cl-loop for n from 1 to 99
             for candidate = (if (= n 1) path
                               (format "%s-%d%s" base n extension))
             unless (file-exists-p (file-name-concat root candidate))
             return candidate)))

(defun niwa-collaboration-files--begin-upload (root frame req-id)
  "Return the upload state for the first chunk FRAME of REQ-ID under ROOT.
Signal an error unless FRAME names a listed folder, a plain new file
name, and a size within the upload bound.  A taken name is refused,
unless FRAME asks to `:rename', which takes the first free numbered
variant instead."
  (let ((dir (plist-get frame :dir))
        (name (plist-get frame :name))
        (size (plist-get frame :size)))
    (unless (niwa-collaboration-files--folder-p
             (niwa-collaboration-files--listing root) dir)
      (error "This folder is not in the project"))
    (unless (niwa-collaboration-files--name-p name)
      (error "Choose a plain file name without slashes or a leading dot"))
    (unless (and (natnump size)
                 (<= size niwa-collaboration-files--max-upload-bytes))
      (error "Uploads are limited to %d MB"
             (/ niwa-collaboration-files--max-upload-bytes 1024 1024)))
    (let* ((wanted (if (string-empty-p dir) name (concat dir "/" name)))
           (path (if (eq t (plist-get frame :rename))
                     (niwa-collaboration-files--free-path root wanted)
                   (and (not (file-exists-p (file-name-concat root wanted)))
                        wanted))))
      (unless path
        (error "%s already exists" wanted))
      (list :req-id req-id :path path :size size :received 0 :parts nil))))

(defun niwa-collaboration-files--finish-upload (root upload)
  "Write completed UPLOAD as a new file under ROOT and return its path.
A file the project's ignore rules would hide is removed again and
refused: the browser could neither list nor remove it."
  (let ((path (plist-get upload :path))
        (content (apply #'concat (reverse (plist-get upload :parts)))))
    (unless (= (length content) (plist-get upload :size))
      (error "Upload ended short of its announced size"))
    (let ((file (file-name-concat root path)))
      (unless (niwa-resource-within-root-p file root)
        (error "This folder is not in the project"))
      (condition-case nil
          (let ((coding-system-for-write 'binary))
            (write-region content nil file nil 'silent nil 'excl))
        (file-already-exists (error "%s already exists" path)))
      (unless (member path (niwa-collaboration-files--listing root))
        (delete-file file)
        (error "The project ignores %s, so it was not added" path))
      path)))

(defun niwa-collaboration-files-handle-upload (owner peer frame root)
  "Take one chunk FRAME of PEER's upload in OWNER into the project at ROOT.
The first chunk of a request id announces `:dir', `:name', `:size' and
optionally `:rename', and starts a new upload, dropping any unfinished
one; every chunk carries base64 `:data', and the `:final' one writes
the file.  Each chunk is acknowledged, so the browser sends the next
only after the host took this one, and any refusal ends the upload."
  (niwa-collaboration-files--answer owner peer frame "file-upload"
    (condition-case err
        (let ((upload (plist-get guest :upload)))
          (unless (equal req-id (plist-get upload :req-id))
            (setq upload (niwa-collaboration-files--begin-upload
                          root frame req-id))
            (plist-put guest :upload upload))
          (let ((bytes (and (stringp (plist-get frame :data))
                            (ignore-errors
                              (base64-decode-string
                               (plist-get frame :data))))))
            (unless bytes
              (error "Upload chunk is not base64 data"))
            (plist-put upload :parts (cons bytes (plist-get upload :parts)))
            (plist-put upload :received
                       (+ (length bytes) (plist-get upload :received)))
            (when (> (plist-get upload :received) (plist-get upload :size))
              (error "Upload is larger than it announced")))
          (if (eq t (plist-get frame :final))
              (let ((path (niwa-collaboration-files--finish-upload
                           root upload)))
                (plist-put guest :upload nil)
                (niwa-collaboration-files--announce
                 owner (niwa-collaboration-files--parent path))
                (list :ok t :path path))
            (list :ok t :received (plist-get upload :received))))
      (error
       (plist-put guest :upload nil)
       (signal (car err) (cdr err))))))

(defun niwa-collaboration-files-handle-remove (owner peer frame root)
  "Move the project file PEER's FRAME names under ROOT to the trash.
A file with unsaved changes in an Emacs buffer is refused: its buffer
would otherwise save it straight back."
  (niwa-collaboration-files--answer owner peer frame "file-remove"
    (let* ((path (plist-get frame :path))
           (file (niwa-collaboration-files--file
                  root (niwa-collaboration-files--listing root) path))
           (buffer (find-buffer-visiting file)))
      (when (and buffer (buffer-modified-p buffer))
        (error "%s has unsaved changes in Emacs" path))
      (move-file-to-trash file)
      (niwa-collaboration-files--announce
       owner (niwa-collaboration-files--parent path))
      (list :ok t :path path))))

(provide 'niwa-collaboration-files)
;;; niwa-collaboration-files.el ends here
