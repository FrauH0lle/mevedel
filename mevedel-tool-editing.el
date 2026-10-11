;;; mevedel-tool-editing.el --- Model collaboration with shared content -*- lexical-binding: t; -*-

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
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(autoload 'mevedel-shared-editing--json "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-call "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-create "mevedel-shared-editing")
(autoload 'mevedel-shared-library-libraries "mevedel-shared-library")
(autoload 'mevedel-shared-library-text "mevedel-shared-library")

;; `mevedel-agent-conversation'
(defvar mevedel--agent-invocation)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-require-path "mevedel-agents" (invocation))

;; `mevedel-artifact-store'
(declare-function mevedel-artifact-store-attach
                  "mevedel-artifact-store" (session id &optional buffer))
(autoload 'mevedel-artifact-store-attach "mevedel-artifact-store")

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-canonical-path "mevedel-pipeline" (path))
(declare-function mevedel-pipeline-handler-resumable "mevedel-pipeline" (function))
(defvar mevedel-pipeline--handler-active-p)
(defvar mevedel-pipeline--handler-commit)

;; `mevedel-resource'
(declare-function mevedel-resource-address-like-p "mevedel-resource" (value))
(declare-function mevedel-resource-encode-component "mevedel-resource" (component))
(declare-function mevedel-resource-execute "mevedel-resource" (attempt &optional executor options))
(declare-function mevedel-resource-parse-address "mevedel-resource" (address))
(autoload 'mevedel-resource-address-like-p "mevedel-resource")
(autoload 'mevedel-resource-execute "mevedel-resource")
(autoload 'mevedel-resource-parse-address "mevedel-resource")

;; `mevedel-structs'
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-tool-fs'
(declare-function mevedel-tool-fs-model-path "mevedel-tool-fs" (path))
(declare-function mevedel-tool-fs-resource-attempt "mevedel-tool-fs" (address))
(autoload 'mevedel-tool-fs-model-path "mevedel-tool-fs")
(autoload 'mevedel-tool-fs-resource-attempt "mevedel-tool-fs")

(defun mevedel-tool-editing--restore-nulls (value)
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
    (vconcat (mapcar #'mevedel-tool-editing--restore-nulls value)))
   ((and (consp value) (not (symbolp (car value))))
    (vconcat (mapcar #'mevedel-tool-editing--restore-nulls value)))
   ((listp value)
    (let (result)
      (while value
        (setq result
              (plist-put result (pop value)
                         (mevedel-tool-editing--restore-nulls (pop value)))))
      result))
   (t value)))

(defun mevedel-tool-editing--workspace ()
  "Return the current session's workspace, or signal."
  (unless mevedel--session (error "No active session"))
  (mevedel-session-workspace mevedel--session))

(defun mevedel-tool-editing--actor ()
  "Return the attribution of the agent calling an editing tool."
  (concat "Agent: "
          (if (bound-and-true-p mevedel--agent-invocation)
              (mevedel-agent-invocation-require-path mevedel--agent-invocation)
            "/root")))

(defun mevedel-tool-editing--deliver (callback create)
  "Return a reply handler delivering a pipeline result to CALLBACK.
CREATE is non-nil for a creation, which the creation core has attached
already.  The handler attaches an edited item to the current session and
versions it when the current request settles."
  (let ((session mevedel--session)
        (workspace (mevedel-tool-editing--workspace))
        (request (bound-and-true-p mevedel--current-request)))
    (lambda (reply)
      (if-let* ((error-text (plist-get reply :error)))
          (funcall callback (list :result
                                  (if (equal (plist-get reply :code) "stale")
                                      (mevedel-shared-editing--json
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
              (mevedel-artifact-store-attach session id))
            ;; A no-op edit makes no version: it would push a real one out.
            ;; Creating records no contribution but makes the item.
            (when (and request (or create (plist-get result :transaction)))
              (cl-pushnew (cons workspace id) (mevedel-request-edited-artifacts request)
                          :test #'equal)))
          (funcall callback
                   (append (list :result (mevedel-shared-editing--json model))
                           (when png
                             (list :media
                                   (list (list :path (format "shared://%s/view.png"
                                                             (plist-get model :id))
                                               :kind 'image :mime "image/png" :data png)))))))))))

(defun mevedel-tool-editing--call (callback args)
  "Run validated editing ARGS and deliver a pipeline result to CALLBACK."
  (unless mevedel--session (error "No active session"))
  (setq args (plist-put (copy-sequence args) :actor (mevedel-tool-editing--actor)))
  (setq args (plist-put args :opId (secure-hash 'sha256 (format "%s%s" (current-time) (random t)))))
  (setq args (mevedel-tool-editing--restore-nulls args))
  (mevedel-shared-editing-call
   (mevedel-tool-editing--workspace) args
   (mevedel-tool-editing--deliver callback nil)
   mevedel-pipeline--handler-active-p mevedel-pipeline--handler-commit))

(defun mevedel-tool-editing--library-args (rest)
  "Return the host request for `shared://library' components REST."
  (let* ((sheet (equal (car (last rest)) "sheet.png"))
         (name (car (if sheet (butlast rest) rest)))
         (libraries (cl-remove-if-not
                     (lambda (library) (or (null name) (equal name (plist-get library :name))))
                     (mevedel-shared-library-libraries))))
    (when (and name (null libraries))
      (error "No element library %s; Read shared://library for installed libraries" name))
    (list :action "library-view" :part (if sheet "sheet" "list")
          :at (if name
                  (concat "shared://library/" (mevedel-resource-encode-component name))
                "shared://library")
          :libraries (vconcat
                      (mapcar (lambda (library)
                                (list :name (plist-get library :name)
                                      :text (plist-get library :text)))
                              libraries)))))

(defun mevedel-tool-editing-view (components callback)
  "Fetch the current workspace's `shared://' view named by COMPONENTS.
COMPONENTS are the decoded address components after `shared://'.  Call
CALLBACK once with (:text TEXT), (:data BASE64 :mime MIME) or
\(:error MESSAGE)."
  (condition-case err
      (let ((args
             (pcase components
               (`("library" . ,rest) (mevedel-tool-editing--library-args rest))
               (`(,id) (list :action "view" :id id :part "overview"))
               (`(,id "view.png") (list :action "view" :id id :part "png"))
               (`(,id ,(and part (or "comments" "history")))
                (list :action "view" :id id :part part))
               (`(,id "elements" ,element)
                (list :action "view" :id id :part "element" :element element))
               (`(,id "images" ,image)
                (list :action "view" :id id :part "image" :image image))
               (_ (error "Unknown shared:// address")))))
        (mevedel-shared-editing-call
         (mevedel-tool-editing--workspace) args
         (lambda (reply)
           (funcall callback
                    (if-let* ((message (plist-get reply :error)))
                        (list :error message)
                      (let ((result (plist-get reply :result)))
                        (if (plist-get result :text)
                            (list :text (plist-get result :text))
                          (list :data (plist-get result :png)
                                :mime (plist-get result :mime)))))))))
    (error (funcall callback (list :error (error-message-string err))))))

(defun mevedel-tool-editing--create (callback args)
  "Create the item ARGS describe and deliver a pipeline result to CALLBACK."
  (unless mevedel--session (error "No active session"))
  (mevedel-shared-editing-create
   (mevedel-tool-editing--workspace) (plist-get args :kind) (plist-get args :title)
   (mevedel-tool-editing--actor) (mevedel-tool-editing--deliver callback t)
   :session mevedel--session
   :authorize mevedel-pipeline--handler-active-p
   :commit mevedel-pipeline--handler-commit))

(defun mevedel-tool-editing--edit (callback args)
  "Apply a targeted mutation using CALLBACK and ARGS."
  (unless (member (plist-get args :action) '("patch" "insert" "image" "rename" "background" "revert"))
    (error "Unknown editing action"))
  (if (equal (plist-get args :action) "image")
      (mevedel-tool-editing--image callback args)
    (mevedel-tool-editing--edit-targeted callback args)))

(defun mevedel-tool-editing--edit-targeted (callback args)
  "Apply a mutation of the item's own content using CALLBACK and ARGS."
  (let ((args (plist-put (copy-sequence args) :image t)))
    (when (equal (plist-get args :action) "insert")
      ;; Library item references are LIBRARY/ITEM-ID from shared://library.
      (let* ((ref (plist-get args :item))
             (slash (and (stringp ref) (string-search "/" ref))))
        (unless slash (error "insert needs item as LIBRARY/ITEM-ID from shared://library"))
        (setq args (plist-put args :library
                              (mevedel-shared-library-text (substring ref 0 slash)))
              args (plist-put args :item (substring ref (1+ slash))))))
    (mevedel-tool-editing--call callback args)))

(defconst mevedel-tool-editing--image-max-bytes 4700000
  "Largest image file the model may bring into a shared item.
An embedded image is a base64 data URL of at most 6 MiB, which this many
bytes fill with room to spare.")

(defun mevedel-tool-editing--file-base64 (path address)
  "Return the base64 content of the image file PATH, named by ADDRESS.
Signal a message the model can act on when PATH is not a readable, small
enough file."
  (let ((attributes (file-attributes path)))
    (unless attributes (error "File not found: %s" address))
    (when (eq t (file-attribute-type attributes))
      (error "%s is a directory, not an image file" address))
    (let ((size (file-attribute-size attributes)))
      (when (> size mevedel-tool-editing--image-max-bytes)
        (error "%s is %d bytes; shared images are limited to %d bytes. Re-render at a lower resolution or save as JPEG or WebP"
               address size mevedel-tool-editing--image-max-bytes)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (base64-encode-region (point-min) (point-max) t)
      (buffer-string))))

(defun mevedel-tool-editing--image-source (source continue)
  "Read the image SOURCE names, then call CONTINUE with its base64 content.
SOURCE is a file path or a readable resource address; resource reads use
the attempt the pipeline prepared and authorized, as Read does.  Signal an
error when SOURCE is no readable image, or call CONTINUE from the host's
reply for a shared:// image."
  (if-let* ((attempt (mevedel-tool-fs-resource-attempt source)))
      (mevedel-resource-execute
       attempt
       (lambda (path authored)
         (cond
          ((and (listp path) (plist-member path :shared-view))
           (mevedel-tool-editing-view
            (plist-get path :shared-view)
            (mevedel-pipeline-handler-resumable
             (lambda (view)
               (condition-case err
                   (cond
                    ((plist-get view :error) (error "Cannot read %s: %s" authored (plist-get view :error)))
                    ((plist-get view :data) (funcall continue (plist-get view :data)))
                    (t (error "%s is not an image: it reads as text" authored)))
                 (error (funcall continue (list :error (error-message-string err)))))))))
          ((listp path) (error "%s is not an image file: it reads as text" authored))
          (t (funcall continue (mevedel-tool-editing--file-base64 path authored))))))
    ;; The path the permission step authorized, with symlinks resolved.
    (funcall continue (mevedel-tool-editing--file-base64
                       (mevedel-pipeline-canonical-path source) source))))

(defun mevedel-tool-editing--image (callback args)
  "Put the image ARGS names in `:source' into a shared item, then call CALLBACK."
  (let ((source (plist-get args :source))
        (request (list :id (plist-get args :id) :action "image" :image t)))
    (unless (and (stringp source) (not (string-empty-p source)))
      (error "The image action needs source: the path or resource address of the image"))
    (dolist (key '(:x :y :width :alt :afterId))
      (when (plist-member args key)
        (setq request (plist-put request key (plist-get args key)))))
    (mevedel-tool-editing--image-source
     source
     (mevedel-pipeline-handler-resumable
      (lambda (data)
        (if (stringp data)
            (mevedel-tool-editing--call callback (plist-put request :data data))
          (funcall callback (list :result (concat "Error: " (plist-get data :error))
                                  :status 'error))))))))

(defun mevedel-tool-editing--save-image (callback args)
  "Write the shared image ARGS name in `:source' to the new file `:file_path'.
Deliver the outcome to CALLBACK."
  (let* ((source (plist-get args :source))
         (file (plist-get args :file_path))
         (parsed (and (stringp source)
                      (mevedel-resource-address-like-p source)
                      (mevedel-resource-parse-address source)))
         (components (and (eq 'shared (plist-get parsed :scheme))
                          (plist-get parsed :components))))
    (unless (pcase components
              ((or `(,_ "images" ,_) `(,_ "view.png")) t))
      (error "SharedSaveImage reads shared://ID/images/KEY or shared://ID/view.png; copy ordinary files with Bash"))
    (when (file-exists-p file)
      (error "%s already exists; choose another name or remove it first"
             (mevedel-tool-fs-model-path file)))
    (mevedel-tool-editing-view
     components
     (mevedel-pipeline-handler-resumable
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
                                       source (mevedel-tool-fs-model-path file)
                                       (plist-get view :mime) (length bytes)))))
           (error (list :result (concat "Error: " (error-message-string err))
                        :status 'error)))))))))

(defun mevedel-tool-editing--register ()
  "Register shared content tools."
  (mevedel-define-tool
   :name "SharedCreate" :handler #'mevedel-tool-editing--create
   :summary "Start a whiteboard or document that people and agents edit together."
   :description "Open a new named collaborative whiteboard or document in the project's artifact store, readable at the shared:// address the result names, and attach it to this session. It appears under Shared work in every room of the project. All full/owner participants and agents can edit concurrently."
   :args ((kind string :required "Editor kind." :enum ["whiteboard" "document"])
          (title string :required "Item title, 1 to 200 characters after trimming."))
   :async-p t :groups (edit))
  (mevedel-define-tool
   :name "SharedEdit" :handler #'mevedel-tool-editing--edit
   :summary "Draw on a shared whiteboard or edit a shared document."
   :description "Edit a shared whiteboard or document. Read shared://ID first: each line is HASH then an element's or block's JSON. patch: changes are {id,hash,set?,unset?,after?}; set merges fields and unset removes them, after replaces the whole element or block and null deletes it; a new element has no hash. A stale hash rejects the whole patch and returns current lines; unrelated edits survive. Whiteboards hold Excalidraw elements {id,type,x,y,width,height,...} of type rectangle/diamond/ellipse/text/arrow/line/freedraw/image/stickynote/frame, with Excalidraw's field names and values. Absent fields take Excalidraw defaults (strokeColor #1e1e1e, backgroundColor transparent, fillStyle solid, strokeWidth 2, roughness 1, opacity 100); omit version, versionNonce, updated, isDeleted and boundElements, which are derived. Label a shape or arrow with a text element whose containerId is that element; the label wraps and centres inside it. Connect shapes with an arrow whose startBinding/endBinding are {elementId,fixedPoint:[0.5,0.5],mode:\"orbit\"}; bound ends follow their shapes. Line, arrow and freedraw points are relative to x,y and stored at 0.1 units. Elements draw in fractional index order; one without an index draws on top. Existing images are referenced by fileId. image adds a new one from source: a file path, or an artifact://, work:// or shared:// address (shared://ID/images/KEY or shared://ID/view.png) the model can read. PNG, JPEG, WebP or SVG up to 4.7 MB; an SVG is rasterized. A board image goes at x,y (default below the content) and a document image after afterId (default the end); width sets its display width and alt describes a document image. insert places a library item (ref from shared://library) with its top-left at x,y as new elements. Documents use top-level ProseMirror blocks with attrs.id and optional afterId insertion anchor; images keep their shared://ID/images/KEY src. rename uses title. background sets a whiteboard's canvas colour (#rrggbb; empty for the room theme). revert uses a contribution ID from shared://ID/history and refuses if its targets changed. All mutations are attributed and committed on the host. Results list the stored lines of changed elements; whiteboard edits also return the resulting PNG."
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
  (mevedel-define-tool
   :name "SharedSaveImage" :handler #'mevedel-tool-editing--save-image
   :summary "Save an image from a shared whiteboard or document to a new file."
   :description "Write an image from a shared item to a new file. source is shared://ID/images/KEY for an embedded image (a board file by its fileId, a document image by the key its src shows) or shared://ID/view.png for the board as drawn. The file keeps the image's own format, so name it accordingly (.png, .jpg or .webp). Refuses an existing file; choose a new name or remove the old file first. Copy ordinary files with Bash."
   :args ((source string :required "shared://ID/images/KEY or shared://ID/view.png.")
          (file_path path :required "New file to write, with its extension."))
   :async-p t :snapshot-p t :groups (edit)
   :get-path (lambda (args) (plist-get args :file_path))))

(provide 'mevedel-tool-editing)
;;; mevedel-tool-editing.el ends here
