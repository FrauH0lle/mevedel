;;; mevedel-remote-artifact.el --- Artifacts attached through an artifact link -*- lexical-binding: t -*-

;;; Commentary:

;; A remote artifact is a whiteboard, document or HTML artifact that lives in
;; another mevedel's workspace, its origin, and is reached here through an
;; artifact link the origin shared.  `mevedel-remote-artifact-attach' reads
;; the link in the minibuffer, never the chat, and registers the artifact with
;; the workspace under its origin id or a chosen name (its alias).  From then on
;; it is served by the remote adapter behind `mevedel-artifact-backend': it
;; is listed in Project artifacts, attaches to sessions, and agents read and
;; edit it with unchanged `shared://' addresses, SharedEdit and ApplyPatch.
;; Every read and edit goes live to the origin, which executes it; nothing
;; about the artifact is written into the local store and nothing is cached.
;;
;; The relay transport is not reimplemented here.  The remote-artifact
;; package (`remote-artifact/' in this repository) runs as one private
;; subprocess speaking newline-delimited JSON (its `bridge' command): a
;; request names the link and the display name, and the package's pool
;; connects to the artifact room on first use and closes it after five idle
;; minutes.  Each agent's edits carry its own display name, USER@HOST
;; followed by the agent path, so the origin attributes them to
;; \"Agent: NAME (remote)\".
;;
;; The attached links are secrets.  They are kept per user, not in the
;; project: `mevedel-user-dir'/remote-artifacts.eld, mode 0600, lists each
;; workspace root's attachments, and the backend restores them on its first
;; lookup of that workspace.
;;
;; Cancellation is "reply discarded": an edit already sent may still land
;; at the origin, attributed and revertible there, but a cancelled caller
;; never receives its reply.  An unreachable origin makes the artifact
;; unavailable with the package's generic error.
;;
;; The origin vocabulary each local editing action maps to is
;; `mevedel-remote-artifact--translate'.  An HTML artifact's files have
;; their own actions there, which ApplyPatch uses for the addresses
;; shared://ALIAS/files/PATH: read a file with its hash, list the files,
;; and write or delete one only if its hash still matches.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'mevedel-artifact-backend)
(require 'mevedel-artifact-store)
(require 'mevedel-structs)

;; `mevedel-resource'
(declare-function mevedel-resource-encode-component "mevedel-resource" (component))
(autoload 'mevedel-resource-encode-component "mevedel-resource")

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing--json "mevedel-shared-editing" (value))
(defvar mevedel-shared-editing-node-program)
(autoload 'mevedel-shared-editing--json "mevedel-shared-editing")

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(autoload 'mevedel-workspace "mevedel-workspace")

(defcustom mevedel-remote-artifact-command nil
  "Command running the remote-artifact package's bridge, as a list of strings.
nil runs the copy shipped with mevedel with
`mevedel-shared-editing-node-program'.  For a separately installed package
use (\"mevedel-remote-artifact\" \"bridge\").  A change takes effect
when the bridge next starts."
  :type '(choice (const :tag "Package shipped with mevedel" nil)
                 (repeat :tag "Command and arguments" string))
  :group 'mevedel)

(defcustom mevedel-remote-artifact-display-name nil
  "Name the origin shows for this Emacs's remote edits, before the agent path.
nil means USER@HOST.  An agent's edit is attributed at the origin to
\"Agent: NAME PATH (remote)\"."
  :type '(choice (const :tag "USER@HOST" nil) string)
  :group 'mevedel)

(defconst mevedel-remote-artifact--directory
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "remote-artifact")
  "Directory of the remote-artifact package shipped with mevedel.")

(defconst mevedel-remote-artifact--kinds '("whiteboard" "document" "html")
  "Origin artifact kinds that can be attached.")

(defconst mevedel-remote-artifact--unavailable
  "Origin unavailable (revoked, deleted, or offline)"
  "The generic error of an origin that cannot be reached.")

(defconst mevedel-remote-artifact--adapter
  (list :call #'mevedel-remote-artifact--call
        :catalog #'mevedel-remote-artifact--catalog)
  "The artifact-backend adapter of every remote artifact.")


;;
;;; Attached links

(defvar mevedel-remote-artifact--records :unread
  "Attached links by workspace root, or `:unread' before the record is read.
An alist (ROOT . ENTRIES); ENTRIES is an alist (ALIAS . ENTRY) and ENTRY
a plist with `:link', the origin's `:id', `:kind', `:title', the
`:origin' name its welcome gave, `:tier' (\"view\" or \"edit\"),
`:guest-id' and `:attached' (a float time).")

(defun mevedel-remote-artifact--record-path ()
  "Return the per-user file of attached links."
  (file-name-concat mevedel-user-dir "remote-artifacts.eld"))

(defun mevedel-remote-artifact--load ()
  "Return the attached links, reading the record on first use.
An unreadable or malformed record yields none."
  (when (eq mevedel-remote-artifact--records :unread)
    (setq mevedel-remote-artifact--records
          (let ((path (mevedel-remote-artifact--record-path)))
            (when (file-readable-p path)
              (let ((stored (condition-case nil
                                (with-temp-buffer
                                  (insert-file-contents path)
                                  (read (current-buffer)))
                              (error nil))))
                (and (proper-list-p stored)
                     (cl-every (lambda (cell) (and (consp cell) (stringp (car cell))
                                                   (proper-list-p (cdr cell))))
                               stored)
                     stored))))))
  mevedel-remote-artifact--records)

(defun mevedel-remote-artifact--save (records)
  "Make RECORDS the attached links and write them to the private record."
  (setq mevedel-remote-artifact--records records)
  (let ((path (mevedel-remote-artifact--record-path)))
    (if (null records)
        (when (file-exists-p path) (delete-file path))
      (make-directory (file-name-directory path) t)
      (with-file-modes #o600
        (with-temp-file path
          (let ((print-length nil) (print-level nil))
            (prin1 records (current-buffer)))))
      (set-file-modes path #o600))))

(defun mevedel-remote-artifact--records-of (workspace)
  "Return WORKSPACE's attached links, an alist (ALIAS . ENTRY)."
  (cdr (assoc (mevedel-workspace-root workspace) (mevedel-remote-artifact--load))))

(defun mevedel-remote-artifact--entry (workspace alias)
  "Return the entry WORKSPACE attached as ALIAS, or nil."
  (cdr (assoc alias (mevedel-remote-artifact--records-of workspace))))

(defun mevedel-remote-artifact--set-records-of (workspace entries)
  "Record ENTRIES as WORKSPACE's attached links."
  (let ((root (mevedel-workspace-root workspace))
        (records (copy-sequence (mevedel-remote-artifact--load))))
    (mevedel-remote-artifact--save
     (if entries
         (cons (cons root entries) (assoc-delete-all root records))
       (assoc-delete-all root records)))))

;;;###autoload
(defun mevedel-remote-artifact-registrations (workspace)
  "Return WORKSPACE's attached remote artifacts as (ALIAS . ADAPTER) pairs.
The artifact backend calls this once per workspace to restore them."
  (when-let* ((entries (mevedel-remote-artifact--records-of workspace)))
    (mevedel-remote-artifact--remember workspace)
    (mapcar (lambda (cell) (cons (car cell) mevedel-remote-artifact--adapter)) entries)))


;;
;;; Reachability and origin changes

(defvar mevedel-remote-artifact--status (make-hash-table :test #'equal)
  "What this Emacs last learned of each link, keyed by its ref.
A plist: `:reachable' is `yes' or `no', `:error' the last failure,
`:checked' its float time, and `:title', `:revision' and `:origin' the
artifact's as the origin last reported them.")

(defvar mevedel-remote-artifact--workspaces (make-hash-table :test #'equal)
  "Workspace objects by root, for announcing origin changes to them.")

(defconst mevedel-remote-artifact--check-interval 60
  "Seconds a link's reachability is trusted before a listing checks it again.")

(defun mevedel-remote-artifact--ref (link)
  "Return the opaque reference of LINK: its events and status carry it."
  (substring (secure-hash 'sha256 link) 0 16))

(defun mevedel-remote-artifact--remember (workspace)
  "Remember WORKSPACE so origin changes can be announced to it."
  (puthash (mevedel-workspace-root workspace) workspace mevedel-remote-artifact--workspaces))

(defun mevedel-remote-artifact--note (link outcome)
  "Record the bridge OUTCOME of a request on LINK.
Return non-nil when the link's reachability changed."
  (let* ((ref (mevedel-remote-artifact--ref link))
         (status (gethash ref mevedel-remote-artifact--status))
         (welcome (plist-get outcome :welcome))
         (reachable (if welcome 'yes 'no)))
    (puthash ref (append (list :reachable reachable :checked (float-time)
                               :error (plist-get outcome :error))
                         (when welcome
                           (list :title (plist-get welcome :title)
                                 :origin (plist-get welcome :origin)
                                 :revision (plist-get status :revision))))
             mevedel-remote-artifact--status)
    (not (eq reachable (plist-get status :reachable)))))

(defun mevedel-remote-artifact--event (ref event)
  "Announce the origin's change EVENT of the link REF where it is attached."
  (when (equal (plist-get event :event) "changed")
    (let ((status (gethash ref mevedel-remote-artifact--status)))
      (puthash ref (plist-put (plist-put (copy-sequence status) :title (plist-get event :title))
                              :revision (plist-get event :revision))
               mevedel-remote-artifact--status))
    (dolist (record (mevedel-remote-artifact--load))
      (when-let* ((workspace (gethash (car record) mevedel-remote-artifact--workspaces)))
        (dolist (cell (cdr record))
          (when (equal ref (mevedel-remote-artifact--ref (plist-get (cdr cell) :link)))
            (mevedel-artifact-backend-notify
             workspace (list :id (car cell) :kind (plist-get event :kind)
                             :title (plist-get event :title)
                             :revision (plist-get event :revision))
             nil)
            (mevedel-artifact-store--changed workspace)))))))


;;
;;; The bridge

(defvar mevedel-remote-artifact--transport #'mevedel-remote-artifact--bridge-send
  "Function (REQUEST CALLBACK) sending REQUEST to an origin.
REQUEST is a plist with `:link', `:guestId', `:name', `:ref' and, unless
only the welcome is wanted, the origin request `:body'.  CALLBACK
receives (:welcome WELCOME :reply REPLY) or (:error MESSAGE), later.")

(defvar mevedel-remote-artifact--process nil
  "The running bridge process, or nil.")

(defvar mevedel-remote-artifact--pending (make-hash-table :test #'eql)
  "Callbacks of requests sent to the bridge, by request id.")

(defvar mevedel-remote-artifact--next-id 0
  "Id of the last request sent to the bridge.")

(defun mevedel-remote-artifact--command ()
  "Return the bridge command, or signal why it cannot run."
  (or mevedel-remote-artifact-command
      (progn
        (require 'mevedel-shared-editing)
        (list (or (executable-find mevedel-shared-editing-node-program)
                  (error "Install Node 22.4+ or set mevedel-shared-editing-node-program to reach remote artifacts"))
              (file-name-concat mevedel-remote-artifact--directory
                                "bin" "mevedel-remote-artifact.mjs")
              "bridge"))))

(defun mevedel-remote-artifact--fail-pending (message)
  "Answer every request still waiting for the bridge with MESSAGE."
  (let (callbacks)
    (maphash (lambda (_id callback) (push callback callbacks))
             mevedel-remote-artifact--pending)
    (clrhash mevedel-remote-artifact--pending)
    (dolist (callback callbacks)
      (run-at-time 0 nil callback (list :error message)))))

(defun mevedel-remote-artifact--receive (line)
  "Handle the bridge's output LINE."
  (let ((message (json-parse-string line :object-type 'plist :array-type 'array
                                    :null-object nil :false-object :json-false)))
    (if-let* ((ref (plist-get message :ref)))
        (mevedel-remote-artifact--event ref (plist-get message :event))
      (when-let* ((id (plist-get message :id))
                  (callback (gethash id mevedel-remote-artifact--pending)))
        (remhash id mevedel-remote-artifact--pending)
        (run-at-time 0 nil callback message)))))

(defun mevedel-remote-artifact--filter (process chunk)
  "Split the bridge PROCESS's output CHUNK into lines and handle each."
  (let ((start 0))
    (while (< start (length chunk))
      (let* ((end (string-search "\n" chunk start))
             (part (substring chunk start end))
             (size (+ (or (process-get process :partial-bytes) 0) (string-bytes part))))
        (setq start (if end (1+ end) (length chunk)))
        (if (> size (* 64 1024 1024))
            (progn (delete-process process) (setq start (length chunk)))
          (process-put process :partial-bytes size)
          (process-put process :partial (cons part (process-get process :partial)))
          (when end
            (let ((line (apply #'concat (nreverse (process-get process :partial)))))
              (process-put process :partial nil)
              (process-put process :partial-bytes 0)
              (condition-case err
                  (mevedel-remote-artifact--receive line)
                (error (message "mevedel: unreadable remote-artifact reply: %s"
                                (error-message-string err)))))))))))

(defun mevedel-remote-artifact--bridge ()
  "Return the running bridge process, starting it when needed."
  (if (process-live-p mevedel-remote-artifact--process)
      mevedel-remote-artifact--process
    (let ((default-directory temporary-file-directory))
      (setq mevedel-remote-artifact--process
            (make-process
             :name "mevedel-remote-artifact" :buffer nil
             :command (mevedel-remote-artifact--command)
             :connection-type 'pipe :coding 'utf-8-unix :noquery t
             :stderr (get-buffer-create " *mevedel-remote-artifact stderr*")
             :filter #'mevedel-remote-artifact--filter
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (when (eq process mevedel-remote-artifact--process)
                   (setq mevedel-remote-artifact--process nil))
                 (mevedel-remote-artifact--fail-pending
                  "The remote-artifact package stopped; check Node 22.4+ and mevedel-remote-artifact-command, then retry"))))))))

(defun mevedel-remote-artifact--bridge-send (request callback)
  "Send REQUEST to the bridge; CALLBACK receives its answer.
See `mevedel-remote-artifact--transport'."
  (condition-case err
      (let* ((process (mevedel-remote-artifact--bridge))
             (id (cl-incf mevedel-remote-artifact--next-id))
             (text (concat (mevedel-shared-editing--json (cons :id (cons id request))) "\n")))
        (puthash id callback mevedel-remote-artifact--pending)
        ;; Small writes avoid Emacs's large pipe-write pauses.
        (let ((offset 0))
          (while (< offset (length text))
            (let ((end (min (length text) (+ offset 1024))))
              (process-send-string process (substring text offset end))
              (setq offset end)))))
    (error (run-at-time 0 nil callback (list :error (error-message-string err))))))

(defun mevedel-remote-artifact-stop ()
  "Stop the bridge; requests still waiting are answered as failed."
  (when (process-live-p mevedel-remote-artifact--process)
    (delete-process mevedel-remote-artifact--process))
  (setq mevedel-remote-artifact--process nil)
  (mevedel-remote-artifact--fail-pending "The remote-artifact package was stopped"))

(defun mevedel-remote-artifact--display-name ()
  "Return the name this Emacs's remote edits carry at an origin."
  (or mevedel-remote-artifact-display-name
      (format "%s@%s" (user-login-name) (system-name))))

(defun mevedel-remote-artifact--request (entry &optional actor body)
  "Return the bridge request for ENTRY's link, as ACTOR, sending BODY.
An ACTOR \"Agent: PATH\" adds PATH to the display name, so the origin
attributes the edit to that agent."
  (let ((agent (and (stringp actor) (string-prefix-p "Agent: " actor)
                    (substring actor (length "Agent: ")))))
    (append (list :link (plist-get entry :link)
                  :guestId (plist-get entry :guest-id)
                  :name (if agent
                            (concat (mevedel-remote-artifact--display-name) " " agent)
                          (mevedel-remote-artifact--display-name))
                  :ref (mevedel-remote-artifact--ref (plist-get entry :link)))
            (when body (list :body body)))))

(defun mevedel-remote-artifact--request-sync (request)
  "Send REQUEST and wait for its answer, at most 30 seconds."
  (let ((outcome nil)
        (deadline (+ (float-time) 30)))
    (funcall mevedel-remote-artifact--transport request
             (lambda (value) (setq outcome value)))
    (while (and (not outcome) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (or outcome (list :error "The origin did not answer in time; try again"))))


;;
;;; The remote adapter

(defconst mevedel-remote-artifact--edit-keys
  '(("patch" :changes)
    ("rename" :title)
    ("background" :background)
    ("image" :data :x :y :width :alt :afterId)
    ("insert" :item :x :y)
    ("revert" :transaction))
  "SharedEdit actions an artifact link carries, with the keys each sends.
An image travels as `:data', its base64 content; an insert names its
library item as LIBRARY/ITEM-ID, resolved by the origin.")

(defconst mevedel-remote-artifact--file-actions
  '(("file" "read" :file)
    ("files" "files")
    ("write-file" "write-file" :file :hash :text :data)
    ("delete-file" "delete-file" :file :hash))
  "HTML file actions, each with the origin action and the keys it sends.
`file' reads one file, the primary one without `:file', with its
`:hash'; `files' lists them.  A write or deletion names the `:hash' the
file was read with, none to create it, and the origin refuses it as
stale when the file changed since.  A key without a value is not sent.")

(defun mevedel-remote-artifact--translate (alias args)
  "Return how local editing ARGS for the remote artifact ALIAS are served.
The result is (:body BODY :shape SHAPE): BODY is the origin request and
SHAPE (`text', `search', `image', `library', `library-search', `edit' or
`file') how its reply maps back.  Signal what a remote artifact cannot
do."
  (let ((action (plist-get args :action)))
    (pcase action
      ("view"
       (let ((text-shape (if (plist-get args :search) 'search 'text)))
         (pcase (plist-get args :part)
           ("overview" (list :body (list :action "read") :shape text-shape))
           ("history" (list :body (list :action "read" :history t) :shape text-shape))
           ("png" (list :body (list :action "view") :shape 'image))
           ("image" (list :body (list :action "view" :image (plist-get args :image))
                          :shape 'image))
           ("discussions"
            (error "Discussions of a remote artifact stay at its origin"))
           (_ (error "Read shared://%s for the elements of a remote artifact" alias)))))
      ;; The origin's own libraries, as shared://ALIAS/library shows them.
      ("library-view"
       (list :body (append (list :action "library" :part (plist-get args :part))
                           (when-let* ((name (plist-get args :library))) (list :library name))
                           (when-let* ((item (plist-get args :item))) (list :item item)))
             :shape (if (plist-get args :search) 'library-search 'library)))
      ("delete"
       (error "A remote artifact is deleted at its origin; detach it here instead"))
      ((and (pred stringp) (let file-action (assoc action mevedel-remote-artifact--file-actions))
            (guard file-action))
       (let ((body (list :action (cadr file-action))))
         (dolist (key (cddr file-action))
           (when (plist-get args key)
             (setq body (plist-put body key (plist-get args key)))))
         (list :body body :shape 'file)))
      (_
       (if-let* ((keys (cdr (assoc action mevedel-remote-artifact--edit-keys))))
           (let ((body (list :action action)))
             (dolist (key keys)
               (when (plist-member args key)
                 (setq body (plist-put body key (plist-get args key)))))
             (list :body body :shape 'edit))
         (error "A remote artifact does not support %s"
                (if (stringp action) action "that")))))))

(defun mevedel-remote-artifact--search-document (address text)
  "Return TEXT, read at ADDRESS, as a search document.
Its spans map each byte range of the searched text to the line Read shows,
joining a split string's chunks so one match can span them."
  (let* ((lines (split-string (string-remove-suffix "\n" text) "\n"))
         (spans nil) (searched nil) (bytes 0) (line 1) (at 0))
    (cl-flet* ((append-span (value source-line)
                 (unless (string-empty-p value)
                   (let ((end (+ bytes (string-bytes value))))
                     (push (vector bytes end source-line) spans)
                     (setq bytes end)
                     (push value searched))))
               (ordinary (value)
                 (let ((pieces (split-string value "\n")))
                   (cl-loop for (piece . more) on pieces
                            do (append-span (if more (concat piece "\n") piece) line)
                            (when more (cl-incf line))))))
      (while (string-match "string-chunks(\n\\(\\(?:[ \t]*\"\\(?:[^\"\\\n]\\|\\\\.\\)*\"\n\\)+\\)[ \t]*)"
                           text at)
        (let ((start (match-beginning 0))
              (stop (match-end 0))
              (parts (split-string (string-trim-right (match-string 1 text)) "\n")))
          (ordinary (substring text at start))
          (cl-incf line)
          (append-span "\"" line)
          (dolist (part parts)
            (let ((trimmed (string-trim part)))
              (append-span (substring trimmed 1 -1) line))
            (cl-incf line))
          (append-span "\"" (1- line))
          (setq at stop)))
      (ordinary (substring text at)))
    (list :address address :text (apply #'concat (nreverse searched))
          :lines (vconcat lines) :spans (vconcat (nreverse spans)))))

(defun mevedel-remote-artifact--edit-text (alias entry text)
  "Return the origin's edit result TEXT naming the artifact by ALIAS.
ENTRY is its attached link.  The origin names its own id and address;
the model knows the artifact by its alias here."
  (let ((model (json-parse-string text :object-type 'alist :array-type 'array
                                  :null-object :null :false-object :false)))
    (when (equal (alist-get 'id model) (plist-get entry :id))
      (setf (alist-get 'id model) alias))
    (when (alist-get 'address model)
      (setf (alist-get 'address model) (concat "shared://" alias)))
    (decode-coding-string (json-serialize model :null-object :null :false-object :false)
                          'utf-8-unix)))

(defun mevedel-remote-artifact--reply (alias entry args shape reply)
  "Return the local editing reply to the origin's REPLY.
ALIAS, ENTRY and the local ARGS name the request; SHAPE is from
`mevedel-remote-artifact--translate'.  The origin names its libraries
shared://library, which here are ALIAS's."
  (let* ((result (plist-get reply :result))
         (library (concat "shared://" alias "/library"))
         (library-text (lambda ()
                         (string-replace "shared://library" library
                                         (or (plist-get result :text) "")))))
    (cond
     ((plist-get reply :error)
      (cond
       ((not (equal (plist-get reply :code) "stale")) (list :error (plist-get reply :error)))
       ;; A file's refusal carries its current hash, and content when small.
       ((eq shape 'file)
        (append (list :error (plist-get reply :error) :code "stale")
                (cl-loop for key in '(:file :hash :bytes :text :data)
                         when (plist-member reply key)
                         append (list key (plist-get reply key)))))
       (t (list :error (plist-get reply :error) :code "stale"
                :targets (plist-get reply :targets) :revision (plist-get reply :revision)))))
     ((eq shape 'file) (list :result result))
     ((and (eq shape 'library) (plist-get result :text))
      (list :result (list :text (funcall library-text))))
     ((eq shape 'library-search)
      (list :result
            (list :documents
                  (vector (mevedel-remote-artifact--search-document
                           (if-let* ((name (plist-get args :library)))
                               (concat library "/" (mevedel-resource-encode-component name))
                             library)
                           (funcall library-text))))))
     ((eq shape 'text) (list :result (list :text (plist-get result :text))))
     ((eq shape 'search)
      (list :result
            (list :documents
                  (vector (mevedel-remote-artifact--search-document
                           (concat "shared://" alias
                                   (if (equal (plist-get args :part) "history") "/history" ""))
                           (or (plist-get result :text) ""))))))
     ((memq shape '(image library))
      (list :result (list :png (plist-get result :png)
                          :mime (or (plist-get result :mime) "image/png"))))
     (t
      (list :result
            (append (list :model (list :id alias)
                          :text (mevedel-remote-artifact--edit-text
                                 alias entry (plist-get result :text)))
                    (when-let* ((png (plist-get result :png))) (list :png png))))))))

(defun mevedel-remote-artifact--call (workspace args callback &optional authorize commit)
  "Serve editing ARGS for WORKSPACE's remote artifact `:id' from its origin.
CALLBACK receives one reply, in the buffer current now, as from
`mevedel-shared-editing-call'.  AUTHORIZE must permit the request when it
is sent and when its reply arrives; COMMIT runs the delivery of a
successful reply.  Return a cancellation function: a cancelled request's
reply is discarded, though an edit already sent may land at the origin."
  (let* ((alias (plist-get args :id))
         (entry (mevedel-remote-artifact--entry workspace alias))
         (buffer (current-buffer))
         (cancelled nil)
         (deliver (lambda (reply)
                    (if (buffer-live-p buffer)
                        (with-current-buffer buffer (funcall callback reply))
                      (funcall callback reply))))
         (refuse (lambda (message) (run-at-time 0 nil deliver (list :error message)))))
    (mevedel-remote-artifact--remember workspace)
    (condition-case err
        (let ((plan (mevedel-remote-artifact--translate alias args)))
          (cond
           ((null entry) (funcall refuse "This item no longer exists"))
           ((and authorize (not (funcall authorize))) (funcall refuse "Editing authority ended"))
           (t
            (funcall
             mevedel-remote-artifact--transport
             (mevedel-remote-artifact--request entry (plist-get args :actor) (plist-get plan :body))
             (lambda (outcome)
               (when (mevedel-remote-artifact--note (plist-get entry :link) outcome)
                 (mevedel-artifact-store--changed workspace))
               (cond
                (cancelled (funcall deliver (list :error "Editing operation cancelled")))
                ((plist-get outcome :error)
                 (funcall deliver (list :error (plist-get outcome :error))))
                ((and authorize (not (funcall authorize)))
                 (funcall deliver (list :error "Editing authority ended")))
                (t
                 (let ((reply (condition-case err
                                  (mevedel-remote-artifact--reply
                                   alias entry args (plist-get plan :shape)
                                   (plist-get outcome :reply))
                                (error (list :error (error-message-string err))))))
                   (if (plist-get reply :error)
                       (funcall deliver reply)
                     (funcall (or commit #'funcall)
                              (lambda () (funcall deliver reply))))))))))))
      (error (funcall refuse (error-message-string err))))
    (lambda () (setq cancelled t))))

(defun mevedel-remote-artifact--check (workspace entry)
  "Ask ENTRY's origin, in the background, whether it is reachable.
A change of reachability is announced as a change of WORKSPACE's listing."
  (let ((link (plist-get entry :link)))
    (puthash (mevedel-remote-artifact--ref link)
             (plist-put (copy-sequence (gethash (mevedel-remote-artifact--ref link)
                                                mevedel-remote-artifact--status))
                        :checked (float-time))
             mevedel-remote-artifact--status)
    (funcall mevedel-remote-artifact--transport
             (mevedel-remote-artifact--request entry)
             (lambda (outcome)
               (when (mevedel-remote-artifact--note link outcome)
                 (mevedel-artifact-store--changed workspace))))))

(defun mevedel-remote-artifact--catalog (workspace detail)
  "Return WORKSPACE's remote artifacts as catalog rows at DETAIL.
See `mevedel-artifact-backend-catalog'.  A full row adds `:remote' t,
the `:origin' name (the relay host while the origin gave none), the
`:relay' host, the `:origin-id', the link's `:tier' and `:reachable'
\(`yes', `no' or nil while unknown).  Listing them in full checks, in the
background, the reachability of links not checked lately."
  (mevedel-remote-artifact--remember workspace)
  (mapcar
   (lambda (cell)
     (let* ((alias (car cell))
            (entry (cdr cell))
            (status (gethash (mevedel-remote-artifact--ref (plist-get entry :link))
                             mevedel-remote-artifact--status))
            (title (or (plist-get status :title) (plist-get entry :title))))
       (pcase detail
         ('ids (list :id alias))
         ('items (list :id alias :kind (plist-get entry :kind) :title title
                       :revision (plist-get status :revision)))
         (_
          (when (< (or (plist-get status :checked) 0)
                   (- (float-time) mevedel-remote-artifact--check-interval))
            (mevedel-remote-artifact--check workspace entry))
          (let ((relay (url-host (url-generic-parse-url (plist-get entry :link)))))
            (list :id alias :kind (intern (plist-get entry :kind)) :title title
                  :file "state.json" :path nil :size 0 :versions 0
                  :modified (plist-get entry :attached)
                  :remote t
                  :origin (or (plist-get status :origin) (plist-get entry :origin) relay)
                  :relay relay
                  :origin-id (plist-get entry :id)
                  :tier (plist-get entry :tier)
                  :reachable (plist-get status :reachable)
                  :error (plist-get status :error)))))))
   (cl-remove-if-not (lambda (cell) (mevedel-artifact-backend-registered-p workspace (car cell)))
                     (mevedel-remote-artifact--records-of workspace))))


;;
;;; Commands

(defun mevedel-remote-artifact--command-workspace ()
  "Return the workspace of the current buffer for a command, or signal."
  (or (mevedel-workspace) (user-error "No mevedel workspace here")))

(defun mevedel-remote-artifact--check-alias (workspace alias link)
  "Signal unless WORKSPACE may attach LINK under ALIAS."
  (unless (mevedel-artifact-store-id-p alias)
    (user-error "%s is not a valid name: use letters, digits, - and _ (at most 80)" alias))
  (when (member alias (mevedel-artifact-store-ids workspace))
    (user-error "%s names an artifact of this project; choose another name" alias))
  (when-let* ((other (mevedel-remote-artifact--entry workspace alias))
              ((not (equal link (plist-get other :link)))))
    (user-error "%s is already attached (%s); choose another name" alias
                (plist-get other :title))))

;;;###autoload
(defun mevedel-remote-artifact-attach (workspace link &optional name)
  "Attach the artifact LINK shares to WORKSPACE as a remote artifact.
It is named NAME, by default its id at the origin; return that name.
Interactively the link is read without echo, so it never enters a chat
or the minibuffer history, and a prefix argument asks for the name.
Attaching connects to the origin once to learn what the link shares.
Attaching a link again refreshes it and may rename it."
  (interactive
   (list (mevedel-remote-artifact--command-workspace)
         (read-passwd "Artifact link: ")
         (when current-prefix-arg (read-string "Attach as: "))))
  (let* ((link (string-trim (or link "")))
         (entries (mevedel-remote-artifact--records-of workspace))
         (previous (cl-find link entries :key (lambda (cell) (plist-get (cdr cell) :link))
                            :test #'equal))
         (guest-id (or (plist-get (cdr previous) :guest-id)
                       (substring (secure-hash 'sha256 (format "%s%s" (current-time) (random t)))
                                  0 16))))
    (when (string-empty-p link) (user-error "No artifact link given"))
    (let* ((outcome (mevedel-remote-artifact--request-sync
                     (list :link link :guestId guest-id
                           :name (mevedel-remote-artifact--display-name)
                           :ref (mevedel-remote-artifact--ref link))))
           (welcome (or (plist-get outcome :welcome)
                        (user-error "Could not attach the artifact: %s"
                                    (or (plist-get outcome :error)
                                        mevedel-remote-artifact--unavailable))))
           (kind (plist-get welcome :kind))
           (alias (or name (plist-get welcome :id))))
      (unless (member kind mevedel-remote-artifact--kinds)
        (user-error "%s artifacts cannot be attached in mevedel yet" kind))
      (while (and (null name) (called-interactively-p 'any)
                  (condition-case err
                      (progn (mevedel-remote-artifact--check-alias workspace alias link) nil)
                    (user-error (setq alias (read-string
                                             (format "%s; attach as: " (cadr err))))))))
      (mevedel-remote-artifact--check-alias workspace alias link)
      (mevedel-remote-artifact--note link outcome)
      ;; One name per link: attaching it again under another name renames it.
      (dolist (cell entries)
        (when (and (equal link (plist-get (cdr cell) :link)) (not (equal alias (car cell))))
          (mevedel-artifact-backend-unregister workspace (car cell))))
      (mevedel-remote-artifact--set-records-of
       workspace
       (cons (cons alias (list :link link :id (plist-get welcome :id) :kind kind
                               :title (plist-get welcome :title)
                               :origin (plist-get welcome :origin)
                               :tier (if (eq t (plist-get welcome :readOnly)) "view" "edit")
                               :guest-id guest-id
                               :attached (or (plist-get (cdr previous) :attached) (float-time))))
             (cl-remove-if (lambda (cell) (or (equal alias (car cell))
                                              (equal link (plist-get (cdr cell) :link))))
                           entries)))
      (mevedel-remote-artifact--remember workspace)
      (mevedel-artifact-backend-register workspace alias mevedel-remote-artifact--adapter)
      (message "mevedel: attached shared://%s, %s \"%s\" (%s link)" alias kind
               (plist-get welcome :title)
               (if (eq t (plist-get welcome :readOnly)) "view" "edit"))
      alias)))

;;;###autoload
(defun mevedel-remote-artifact-detach (workspace alias)
  "Forget WORKSPACE's remote artifact ALIAS.
The origin and every other holder of its link are unaffected.  Sessions
that attached it no longer list it."
  (interactive
   (let ((workspace (mevedel-remote-artifact--command-workspace)))
     (list workspace
           (completing-read "Detach remote artifact: "
                            (or (mapcar #'car (mevedel-remote-artifact--records-of workspace))
                                (user-error "No remote artifacts are attached here"))
                            nil t))))
  (unless (mevedel-remote-artifact--entry workspace alias)
    (user-error "No remote artifact %s is attached here" alias))
  (mevedel-remote-artifact--set-records-of
   workspace (assoc-delete-all alias (copy-sequence
                                      (mevedel-remote-artifact--records-of workspace))))
  (mevedel-artifact-backend-unregister workspace alias)
  (message "mevedel: detached %s" alias))

(provide 'mevedel-remote-artifact)
;;; mevedel-remote-artifact.el ends here
