;;; niwa-tool-editing.el --- Model collaboration with shared content -*- lexical-binding: t; -*-

;;; Commentary:

;; The model reads the workspace's shared items through `shared://' addresses
;; with Read and Grep, served here from the workspace editing host, and
;; changes them with SharedCreate and SharedEdit inside the ordinary tool
;; permission pipeline.  Each change names its target by the content hash the
;; model read, and uses the same host commit queue as human edits, including
;; without a browser.  An edit attaches the item to the session, and a turn
;; that edited an item leaves a version of it when it settles.  The model
;; brings an image in from any file or readable resource with SharedEdit's
;; image action, and writes a shared image out to a new file with
;; SharedSaveImage.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)
(require 'niwa-structs)
(require 'niwa-tool-registry)
(autoload 'niwa-artifact-backend-call "niwa-artifact-backend")
(autoload 'niwa-artifact-backend-registered-p "niwa-artifact-backend")
(autoload 'niwa-shared-editing--json "niwa-shared-editing")
(autoload 'niwa-shared-editing-create "niwa-shared-editing")
(autoload 'niwa-shared-editing-op-id "niwa-shared-editing")
(autoload 'niwa-shared-library-libraries "niwa-shared-library")
(autoload 'niwa-shared-library-text "niwa-shared-library")

;; `niwa-agent-conversation'
(defvar niwa--agent-invocation)

;; `niwa-agents'
(declare-function niwa-agent-invocation-require-path "niwa-agents" (invocation))

;; `niwa-artifact-store'
(declare-function niwa-artifact-store-attach
                  "niwa-artifact-store" (session id &optional buffer))
(autoload 'niwa-artifact-store-attach "niwa-artifact-store")

;; `niwa-pipeline'
(declare-function niwa-pipeline-canonical-path "niwa-pipeline" (path))
(declare-function niwa-pipeline-handler-resumable "niwa-pipeline" (function))
(defvar niwa-pipeline--handler-active-p)
(defvar niwa-pipeline--handler-commit)

;; `niwa-resource'
(declare-function niwa-resource-address-like-p "niwa-resource" (value))
(declare-function niwa-resource-encode-component "niwa-resource" (component))
(declare-function niwa-resource-execute "niwa-resource" (attempt &optional executor options))
(declare-function niwa-resource-parse-address "niwa-resource" (address))
(autoload 'niwa-resource-address-like-p "niwa-resource")
(autoload 'niwa-resource-encode-component "niwa-resource")
(autoload 'niwa-resource-execute "niwa-resource")
(autoload 'niwa-resource-parse-address "niwa-resource")

;; `niwa-structs'
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-tool-fs'
(declare-function niwa-tool-fs-model-path "niwa-tool-fs" (path))
(declare-function niwa-tool-fs-resource-attempt "niwa-tool-fs" (address))
(autoload 'niwa-tool-fs-model-path "niwa-tool-fs")
(autoload 'niwa-tool-fs-resource-attempt "niwa-tool-fs")

(defun niwa-tool-editing--restore-nulls (value)
  "Map gptel's lossless null marker in VALUE to codec JSON nulls.
Tool-call arguments arrive decoded from gptel responses, which mark JSON null
as the keyword `:null' so it is not confused with an empty object.  When such
arguments are re-encoded for the editor host the marker must become nil again,
because the host codec serializes nil as JSON null.
ToolCall also permits list arrays, including coordinates nested in objects;
encode these as vectors so the host does not mistake them for objects."
  (cond
   ((and (symbolp value) (equal (symbol-name value) ":null")) nil)
   ((vectorp value)
    (vconcat (mapcar #'niwa-tool-editing--restore-nulls value)))
   ((and (consp value) (not (symbolp (car value))))
    (vconcat (mapcar #'niwa-tool-editing--restore-nulls value)))
   ((listp value)
    (let (result)
      (while value
        (setq result
              (plist-put result (pop value)
                         (niwa-tool-editing--restore-nulls (pop value)))))
      result))
   (t value)))

(defun niwa-tool-editing--workspace ()
  "Return the current session's workspace, or signal."
  (unless niwa--session (error "No active session"))
  (niwa-session-workspace niwa--session))

(defun niwa-tool-editing-actor ()
  "Return the attribution of the agent calling an editing tool."
  (concat "Agent: "
          (if (bound-and-true-p niwa--agent-invocation)
              (niwa-agent-invocation-require-path niwa--agent-invocation)
            "/root")))

(defun niwa-tool-editing--deliver (callback create)
  "Return a reply handler delivering a pipeline result to CALLBACK.
CREATE is non-nil for a creation, which the creation core has attached
already.  The handler attaches an edited item to the current session and
versions it when the current request settles."
  (let ((session niwa--session)
        (workspace (niwa-tool-editing--workspace))
        (request (bound-and-true-p niwa--current-request)))
    (lambda (reply)
      (if-let* ((error-text (plist-get reply :error)))
          (funcall callback (list :result
                                  (if (equal (plist-get reply :code) "stale")
                                      (niwa-shared-editing--json
                                       (list :error error-text :code "stale"
                                             :targets (plist-get reply :targets)
                                             :revision (plist-get reply :revision)))
                                    (concat "Error: " error-text)) :status 'error))
        ;; The model sees what it changed, not the browsers' full state.
        (let* ((result (plist-get reply :result))
               (model (plist-get result :model))
               (png (plist-get result :png))
               (id (plist-get model :id)))
          (when (stringp id)
            (unless create
              (niwa-artifact-store-attach session id))
            ;; A no-op edit makes no version: it would push a real one out.
            ;; Creating records no contribution but makes the item.
            (when (and request (or create (plist-get result :transaction)))
              (cl-pushnew (cons workspace id) (niwa-request-edited-artifacts request)
                          :test #'equal)))
          (funcall callback
                   ;; A remote artifact's reply carries the origin's text.
                   (append (list :result (or (plist-get result :text)
                                             (niwa-shared-editing--json model)))
                           (when png
                             (list :media
                                   (list (list :path (format "shared://%s/view.png"
                                                             (plist-get model :id))
                                               :kind 'image :mime "image/png" :data png)))))))))))

(defun niwa-tool-editing--call (callback args)
  "Run validated editing ARGS and deliver a pipeline result to CALLBACK."
  (unless niwa--session (error "No active session"))
  (setq args (plist-put (copy-sequence args) :actor (niwa-tool-editing-actor)))
  (setq args (plist-put args :opId (niwa-shared-editing-op-id)))
  (setq args (niwa-tool-editing--restore-nulls args))
  (niwa-artifact-backend-call
   (niwa-tool-editing--workspace) args
   (niwa-tool-editing--deliver callback nil)
   niwa-pipeline--handler-active-p niwa-pipeline--handler-commit))

(defun niwa-tool-editing--library-args (rest &optional id)
  "Return the host request for `shared://library' components REST.
With ID, REST follows `shared://ID/library' and names the libraries of
remote whiteboard ID's origin, which serves them."
  (let* ((item (and (equal (car (last rest)) "view.png") (nth 1 rest)))
         (part (cond (item "item") ((equal (car (last rest)) "sheet.png") "sheet") (t "list")))
         (name (car (if (equal part "sheet") (butlast rest) rest)))
         (ref (and item (concat name "/" item))))
    (if id
        (append (list :action "library-view" :id id :part part)
                (when name (list :library name))
                (when ref (list :item ref)))
      (let ((libraries (cl-remove-if-not
                        (lambda (library) (or (null name) (equal name (plist-get library :name))))
                        (niwa-shared-library-libraries))))
        (when (and name (null libraries))
          (error "No element library %s; Read shared://library for installed libraries" name))
        (append
         (list :action "library-view" :part part
               :at (if name
                       (concat "shared://library/" (niwa-resource-encode-component name))
                     "shared://library")
               :libraries (vconcat
                           (mapcar (lambda (library)
                                     (list :name (plist-get library :name)
                                           :text (plist-get library :text)))
                                   libraries)))
         (when ref (list :item ref)))))))

(defun niwa-tool-editing-view (components callback &optional search)
  "Fetch the current workspace's `shared://' view named by COMPONENTS.
COMPONENTS are the decoded address components after `shared://'.  Call
CALLBACK once with (:text TEXT), (:data BASE64 :mime MIME) or
\(:error MESSAGE).  SEARCH instead returns mapped search documents derived
from the same Read lines, joining output string chunks only for matching.
A remote HTML artifact's `files' lists its files and `files/PATH' is one
file's text; searched, they are every text file at or under the path."
  (condition-case err
      (if-let* ((search)
                (files (pcase components (`(,id "files" . ,path) (cons id path)))))
          (niwa-tool-editing--search-files (car files) (string-join (cdr files) "/") callback)
        (let ((args
               (pcase components
               (`("library" . ,rest) (niwa-tool-editing--library-args rest))
               (`(,id "library" . ,rest) (niwa-tool-editing--library-args rest id))
               (`(,id) (list :action "view" :id id :part "overview"))
               (`(,id "view.png") (list :action "view" :id id :part "png"))
               (`(,id ,(and part (or "discussions" "history")))
                (list :action "view" :id id :part part))
               (`(,id "elements" ,element)
                (list :action "view" :id id :part "element" :element element))
               (`(,id "images" ,image)
                (list :action "view" :id id :part "image" :image image))
               (`(,id "files") (list :action "files" :id id))
               (`(,id "files" . ,path)
                (list :action "file" :id id :file (string-join path "/")))
               (_ (error "Unknown shared:// address")))))
        (when search (setq args (plist-put args :search t)))
        (niwa-artifact-backend-call
         (niwa-tool-editing--workspace) args
         (lambda (reply)
           (funcall callback
                    (if-let* ((message (plist-get reply :error)))
                        (list :error message)
                      (let ((result (plist-get reply :result)))
                        (cond
                         (search (list :documents (plist-get result :documents)))
                         ((plist-get result :text) (list :text (plist-get result :text)))
                         ((equal (plist-get args :action) "files")
                          (list :text (niwa-tool-editing--files-text
                                       (plist-get args :id) result)))
                         ((equal (plist-get args :action) "file")
                          (list :error (format "%s is not text" (plist-get args :file))))
                         (t (list :data (plist-get result :png)
                                  :mime (plist-get result :mime)))))))))))
    (error (funcall callback (list :error (error-message-string err))))))

(defun niwa-tool-editing--text-document (address text)
  "Return file TEXT, read at ADDRESS, as a search document of its Read lines."
  (let ((lines (split-string (string-remove-suffix "\n" text) "\n"))
        (bytes 0) (line 0) spans)
    (dolist (value lines)
      (let ((end (+ bytes (string-bytes value) 1)))
        (push (vector bytes end (cl-incf line)) spans)
        (setq bytes end)))
    (list :address address :text text :lines (vconcat lines) :spans (vconcat (nreverse spans)))))

(defun niwa-tool-editing--search-files (id prefix callback)
  "Call CALLBACK with search documents of remote artifact ID's files.
They are the text files at PREFIX or beneath it, every one for an empty
PREFIX, each read from the origin in turn; other files are skipped.
CALLBACK receives (:documents DOCUMENTS) or (:error MESSAGE)."
  (let ((workspace (niwa-tool-editing--workspace))
        (documents nil))
    (cl-labels
        ((address (file)
           (format "shared://%s/files/%s" id
                   (mapconcat #'niwa-resource-encode-component (split-string file "/") "/")))
         (next (files)
           (if (null files)
               (funcall callback (list :documents (vconcat (nreverse documents))))
             (niwa-artifact-backend-call
              workspace (list :action "file" :id id :file (car files))
              (lambda (reply)
                (if-let* ((message (plist-get reply :error)))
                    (funcall callback (list :error message))
                  (when-let* ((text (plist-get (plist-get reply :result) :text)))
                    (push (niwa-tool-editing--text-document (address (car files)) text)
                          documents))
                  (next (cdr files))))))))
      (niwa-artifact-backend-call
       workspace (list :action "files" :id id)
       (lambda (reply)
         (if-let* ((message (plist-get reply :error)))
             (funcall callback (list :error message))
           (let ((files (cl-loop for entry across (plist-get (plist-get reply :result) :files)
                                 for file = (plist-get entry :file)
                                 when (or (string-empty-p prefix) (equal file prefix)
                                          (string-prefix-p (concat prefix "/") file))
                                 collect file)))
             (if files
                 (next files)
               (funcall callback
                        (list :error (format "No file %s in this artifact"
                                             (address prefix))))))))))))

(defun niwa-tool-editing--files-text (id result)
  "Return the files RESULT lists for remote artifact ID, one address a line."
  (mapconcat (lambda (entry)
               (format "shared://%s/files/%s\t%d bytes%s" id (plist-get entry :file)
                       (or (plist-get entry :bytes) 0)
                       (if (equal (plist-get entry :file) (plist-get result :primary))
                           " · primary" "")))
             (plist-get result :files) "\n"))

(defun niwa-tool-editing--create (callback args)
  "Create the item ARGS describe and deliver a pipeline result to CALLBACK."
  (unless niwa--session (error "No active session"))
  (niwa-shared-editing-create
   (niwa-tool-editing--workspace) (plist-get args :kind) (plist-get args :title)
   (niwa-tool-editing-actor) (niwa-tool-editing--deliver callback t)
   :session niwa--session
   :authorize niwa-pipeline--handler-active-p
   :commit niwa-pipeline--handler-commit))

(defun niwa-tool-editing--edit (callback args)
  "Apply a targeted mutation using CALLBACK and ARGS."
  (unless (member (plist-get args :action) '("patch" "insert" "image" "rename" "background" "revert"))
    (error "Unknown editing action"))
  (if (equal (plist-get args :action) "image")
      (niwa-tool-editing--image callback args)
    (niwa-tool-editing--edit-targeted callback args)))

(defun niwa-tool-editing--edit-targeted (callback args)
  "Apply a mutation of the item's own content using CALLBACK and ARGS."
  (let ((args (plist-put (copy-sequence args) :image t)))
    ;; A remote artifact's origin resolves the reference in its own libraries.
    (when (and (equal (plist-get args :action) "insert")
               (not (niwa-artifact-backend-registered-p
                     (niwa-tool-editing--workspace) (plist-get args :id))))
      ;; Library item references are LIBRARY/ITEM-ID from shared://library.
      (let* ((ref (plist-get args :item))
             (slash (and (stringp ref) (string-search "/" ref))))
        (unless slash (error "insert needs item as LIBRARY/ITEM-ID from shared://library"))
        (setq args (plist-put args :library
                              (niwa-shared-library-text (substring ref 0 slash)))
              args (plist-put args :item (substring ref (1+ slash))))))
    (niwa-tool-editing--call callback args)))

(defconst niwa-tool-editing--image-max-bytes 4700000
  "Largest image file the model may bring into a shared item.
An embedded image is a base64 data URL of at most 6 MiB, which this many
bytes fill with room to spare.")

(defun niwa-tool-editing--file-base64 (path address)
  "Return the base64 content of the image file PATH, named by ADDRESS.
Signal a message the model can act on when PATH is not a readable, small
enough file."
  (let ((attributes (file-attributes path)))
    (unless attributes (error "File not found: %s" address))
    (when (eq t (file-attribute-type attributes))
      (error "%s is a directory, not an image file" address))
    (let ((size (file-attribute-size attributes)))
      (when (> size niwa-tool-editing--image-max-bytes)
        (error "%s is %d bytes; shared images are limited to %d bytes. Re-render at a lower resolution or save as JPEG or WebP"
               address size niwa-tool-editing--image-max-bytes)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (base64-encode-region (point-min) (point-max) t)
      (buffer-string))))

(defun niwa-tool-editing--image-source (source continue)
  "Read the image SOURCE names, then call CONTINUE with its base64 content.
SOURCE is a file path or a readable resource address; resource reads use
the attempt the pipeline prepared and authorized, as Read does.  Signal an
error when SOURCE is no readable image, or call CONTINUE from the host's
reply for a shared:// image."
  (if-let* ((attempt (niwa-tool-fs-resource-attempt source)))
      (niwa-resource-execute
       attempt
       (lambda (path authored)
         (cond
          ((and (listp path) (plist-member path :shared-view))
           (niwa-tool-editing-view
            (plist-get path :shared-view)
            (niwa-pipeline-handler-resumable
             (lambda (view)
               (condition-case err
                   (cond
                    ((plist-get view :error) (error "Cannot read %s: %s" authored (plist-get view :error)))
                    ((plist-get view :data) (funcall continue (plist-get view :data)))
                    (t (error "%s is not an image: it reads as text" authored)))
                 (error (funcall continue (list :error (error-message-string err)))))))))
          ((listp path) (error "%s is not an image file: it reads as text" authored))
          (t (funcall continue (niwa-tool-editing--file-base64 path authored))))))
    ;; The path the permission step authorized, with symlinks resolved.
    (funcall continue (niwa-tool-editing--file-base64
                       (niwa-pipeline-canonical-path source) source))))

(defun niwa-tool-editing--image (callback args)
  "Put the image ARGS names in `:source' into a shared item, then call CALLBACK."
  (let ((source (plist-get args :source))
        (request (list :id (plist-get args :id) :action "image" :image t)))
    (unless (and (stringp source) (not (string-empty-p source)))
      (error "The image action needs source: the path or resource address of the image"))
    (dolist (key '(:x :y :width :alt :afterId))
      (when (plist-member args key)
        (setq request (plist-put request key (plist-get args key)))))
    (niwa-tool-editing--image-source
     source
     (niwa-pipeline-handler-resumable
      (lambda (data)
        (if (stringp data)
            (niwa-tool-editing--call callback (plist-put request :data data))
          (funcall callback (list :result (concat "Error: " (plist-get data :error))
                                  :status 'error))))))))

(defun niwa-tool-editing--save-image (callback args)
  "Write the shared image ARGS name in `:source' to the new file `:file_path'.
Deliver the outcome to CALLBACK."
  (let* ((source (plist-get args :source))
         (file (plist-get args :file_path))
         (parsed (and (stringp source)
                      (niwa-resource-address-like-p source)
                      (niwa-resource-parse-address source)))
         (components (and (eq 'shared (plist-get parsed :scheme))
                          (plist-get parsed :components))))
    (unless (pcase components
              ((or `(,_ "images" ,_) `(,_ "view.png")) t))
      (error "SharedSaveImage reads shared://ID/images/KEY or shared://ID/view.png; copy ordinary files with Bash"))
    (when (file-exists-p file)
      (error "%s already exists; choose another name or remove it first"
             (niwa-tool-fs-model-path file)))
    (niwa-tool-editing-view
     components
     (niwa-pipeline-handler-resumable
      (lambda (view)
        (funcall
         callback
         (condition-case err
             (let ((data (plist-get view :data)))
               (unless data
                 (error "Cannot read %s: %s" source
                        (or (plist-get view :error) "it is not an image")))
               (let ((bytes (base64-decode-string data))
                     (coding-system-for-write 'no-conversion))
                 (make-directory (file-name-directory file) t)
                 ;; MUSTBENEW: a file created since the check above is not overwritten.
                 (write-region bytes nil file nil 'silent nil 'excl)
                 (list :result (format "Saved %s to %s (%s, %d bytes)"
                                       source (niwa-tool-fs-model-path file)
                                       (plist-get view :mime) (length bytes)))))
           (error (list :result (concat "Error: " (error-message-string err))
                        :status 'error)))))))))

(defun niwa-tool-editing--register ()
  "Register shared content tools."
  (niwa-define-tool
   :name "SharedCreate" :handler #'niwa-tool-editing--create
   :summary "Start a whiteboard or document that people and agents edit together."
   :description "Open a new named collaborative whiteboard or document in the project's artifact store, readable at the shared:// address the result names, and attach it to this session. It appears under Shared work in every room of the project. All full/owner participants and agents can edit concurrently."
   :args ((kind string :required "Editor kind." :enum ["whiteboard" "document"])
          (title string :required "Item title, 1 to 200 characters after trimming."))
   :async-p t :groups (edit))
  (niwa-define-tool
   :name "SharedEdit" :handler #'niwa-tool-editing--edit
   :summary "Draw on a shared whiteboard or edit a shared document."
   :description "Edit a shared whiteboard or document. Read shared://ID first: each line is HASH then an element's or block's JSON. patch: changes are {id,hash,set?,unset?,after?}; set merges fields and unset removes them, after replaces the whole element or block and null deletes it; a new element has no hash. A stale hash rejects the whole patch and returns current lines; unrelated edits survive. Whiteboards hold Excalidraw elements {id,type,x,y,width,height,...} of type rectangle/diamond/ellipse/text/arrow/line/freedraw/image/stickynote/frame, with Excalidraw's field names and values. Absent fields take Excalidraw defaults (strokeColor #1e1e1e, backgroundColor transparent, fillStyle solid, strokeWidth 2, roughness 1, opacity 100); omit version, versionNonce, updated, isDeleted and boundElements, which are derived. Label a shape or arrow with a text element whose containerId is that element; the label wraps and centres inside it. Connect shapes with an arrow whose startBinding/endBinding are {elementId,fixedPoint:[0.5,0.5],mode:\"orbit\"}; bound ends follow their shapes. Line, arrow and freedraw points are relative to x,y and stored at 0.1 units. Elements draw in fractional index order; new ones without an index go on top in the order listed. Existing images are referenced by fileId. image adds a new one from source: a file path, or an artifact://, work:// or shared:// address (shared://ID/images/KEY or shared://ID/view.png) the model can read. PNG, JPEG, WebP or SVG up to 4.7 MB; an SVG is rasterized. A board image goes at x,y (default below the content) and a document image after afterId (default the end); width sets its display width and alt describes a document image. insert places a library item (ref from shared://library; a remote whiteboard uses its origin's, from shared://ID/library) with its top-left at x,y as new elements. A document is a list of top-level ProseMirror blocks, each with a unique attrs.id (nested nodes get theirs); a new block goes after afterId, default the end. Blocks: paragraph, heading (attrs.level 1-6), blockquote, codeBlock (attrs.language), horizontalRule, image, bulletList or orderedList (attrs.start) of listItem (a paragraph, then blocks), table of tableRow of tableHeader or tableCell (each holds blocks). Inline content is {type:\"text\",text,marks} and hardBreak; marks: bold, italic, underline, strike, code, link (attrs.href http(s), mailto or #). Images keep their shared://ID/images/KEY src. rename uses title. background sets a whiteboard's canvas colour (#rrggbb; empty for the room theme). revert uses a contribution ID from shared://ID/history and refuses if its targets changed. All mutations are attributed and committed on the host. Results list the stored lines of changed elements; whiteboard edits also return the resulting PNG."
   :args ((id string :required "Item ID.")
          (action string :required "Operation." :enum ["patch" "insert" "image" "rename" "background" "revert"])
          (changes array :optional "Targeted changes: {id,hash,set,unset,after,afterId}." :items (:type object))
          (title string :optional "New title for rename.")
          (background string :optional "Canvas colour for background: #rrggbb, or empty for the room theme.")
          (transaction string :optional "Contribution ID for revert, from shared://ID/history.")
          (item string :optional "Library item reference for insert.")
          (x number :optional "Left edge for insert or image.")
          (y number :optional "Top edge for insert or image.")
          (source path-or-resource :optional "Image to add for image: a file path or readable resource address." :access read)
          (width number :optional "Display width of an added image, 1 to 8192; its height follows.")
          (alt string :optional "Description of an added document image.")
          (afterId string :optional "Document block the added image follows; default the end."))
   :async-p t :groups (edit))
  (niwa-define-tool
   :name "SharedSaveImage" :handler #'niwa-tool-editing--save-image
   :summary "Save an image from a shared whiteboard or document to a new file."
   :description "Write an image from a shared item to a new file. source is shared://ID/images/KEY for an embedded image (a board file by its fileId, a document image by the key its src shows) or shared://ID/view.png for the board as drawn. The file keeps the image's own format, so name it accordingly (.png, .jpg or .webp). Refuses an existing file; choose a new name or remove the old file first. Copy ordinary files with Bash."
   :args ((source string :required "shared://ID/images/KEY or shared://ID/view.png.")
          (file_path path :required "New file to write, with its extension."))
   :async-p t :snapshot-p t :groups (edit)
   :get-path (lambda (args) (plist-get args :file_path))))

(provide 'niwa-tool-editing)
;;; niwa-tool-editing.el ends here
