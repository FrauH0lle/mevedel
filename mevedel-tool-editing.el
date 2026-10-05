;;; mevedel-tool-editing.el --- Model collaboration with shared content -*- lexical-binding: t; -*-

;;; Commentary:

;; The model reads shared items through `shared://' addresses with Read and
;; Grep, served here from the session's editing host, and changes them with
;; SharedCreate and SharedEdit inside the ordinary tool permission pipeline.
;; Each change names its target by the content hash the model read, and uses
;; the same host commit queue as human edits, including without a browser.

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-shared-editing)
(require 'mevedel-shared-library)

;; `mevedel-agent-conversation'
(defvar mevedel--agent-invocation)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-require-path "mevedel-agents" (invocation))

;; `mevedel-pipeline'
(defvar mevedel-pipeline--handler-active-p)
(defvar mevedel-pipeline--handler-commit)

;; `mevedel-resource'
(declare-function mevedel-resource-encode-component "mevedel-resource" (component))

;; `mevedel-structs'
(defvar mevedel--session)

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

(defun mevedel-tool-editing--call (callback args)
  "Run validated editing ARGS and deliver a pipeline result to CALLBACK."
  (unless mevedel--session (error "No active session"))
  (setq args (plist-put (copy-sequence args) :actor
                        (concat "Agent: "
                                (if (bound-and-true-p mevedel--agent-invocation)
                                    (mevedel-agent-invocation-require-path mevedel--agent-invocation)
                                  "/root"))))
  (setq args (plist-put args :opId (secure-hash 'sha256 (format "%s%s" (current-time) (random t)))))
  (when (equal (plist-get args :action) "create")
    (setq args (plist-put args :id (secure-hash 'sha256 (format "%s%s" (current-time) (random t))))))
  (setq args (mevedel-tool-editing--restore-nulls args))
  (mevedel-shared-editing-call
   mevedel--session args
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
              (png (plist-get result :png)))
         (funcall callback
                  (append (list :result (mevedel-shared-editing--json model))
                          (when png
                            (list :media
                                  (list (list :path (format "shared://%s/view.png"
                                                            (plist-get model :id))
                                              :kind 'image :mime "image/png" :data png)))))))))
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
  "Fetch the current session's `shared://' view named by COMPONENTS.
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
        (unless mevedel--session (error "No active session"))
        (mevedel-shared-editing-call
         mevedel--session args
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
  "Create content using CALLBACK and ARGS."
  (mevedel-tool-editing--call callback (cons :action (cons "create" args))))

(defun mevedel-tool-editing--edit (callback args)
  "Apply a targeted mutation using CALLBACK and ARGS."
  (unless (member (plist-get args :action) '("patch" "insert" "rename" "background" "revert"))
    (error "Unknown editing action"))
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

(defun mevedel-tool-editing--register ()
  "Register shared content tools."
  (mevedel-define-tool
   :name "SharedCreate" :handler #'mevedel-tool-editing--create
   :summary "Start a whiteboard or document that people and agents edit together."
   :description "Open a new named collaborative whiteboard or document in this session, readable at the shared:// address the result names. It appears under the room's Shared work. All full/owner participants and agents can edit concurrently."
   :args ((kind string :required "Editor kind." :enum ["whiteboard" "document"])
          (title string :required "Item title."))
   :async-p t :groups (edit))
  (mevedel-define-tool
   :name "SharedEdit" :handler #'mevedel-tool-editing--edit
   :summary "Draw on a shared whiteboard or edit a shared document."
   :description "Edit a shared whiteboard or document. Read shared://ID first: each line is HASH then an element's or block's JSON. patch: changes are {id,hash,set?,unset?,after?}; set merges fields and unset removes them, after replaces the whole element or block and null deletes it; a new element has no hash. A stale hash rejects the whole patch and returns current lines; unrelated edits survive. Whiteboards hold Excalidraw elements {id,type,x,y,width,height,...} of type rectangle/diamond/ellipse/text/arrow/line/freedraw/image/stickynote/frame, with Excalidraw's field names and values. Absent fields take Excalidraw defaults (strokeColor #1e1e1e, backgroundColor transparent, fillStyle solid, strokeWidth 2, roughness 1, opacity 100); omit version, versionNonce, updated, isDeleted and boundElements, which are derived. Label a shape or arrow with a text element whose containerId is that element; the label wraps and centres inside it. Connect shapes with an arrow whose startBinding/endBinding are {elementId,fixedPoint:[0.5,0.5],mode:\"orbit\"}; bound ends follow their shapes. Line, arrow and freedraw points are relative to x,y and stored at 0.1 units. Elements draw in fractional index order; one without an index draws on top. Images reference an existing fileId. insert places a library item (ref from shared://library) with its top-left at x,y as new elements. Documents use top-level ProseMirror blocks with attrs.id and optional afterId insertion anchor; images keep their shared://ID/images/KEY src. rename uses title. background sets a whiteboard's canvas colour (#rrggbb; empty for the room theme). revert uses a contribution ID from shared://ID/history and refuses if its targets changed. All mutations are attributed and committed on the host. Results list the stored lines of changed elements; whiteboard edits also return the resulting PNG."
   :args ((id string :required "Item ID.")
          (action string :required "Operation." :enum ["patch" "insert" "rename" "background" "revert"])
          (changes array :optional "Targeted changes: {id,hash,set,unset,after,afterId}." :items (:type object))
          (title string :optional "New title for rename.")
          (background string :optional "Canvas colour for background: #rrggbb, or empty for the room theme.")
          (transaction string :optional "Contribution ID for revert, from shared://ID/history.")
          (item string :optional "Library item reference for insert.")
          (x number :optional "Left edge for insert.")
          (y number :optional "Top edge for insert."))
   :async-p t :groups (edit)))

(provide 'mevedel-tool-editing)
;;; mevedel-tool-editing.el ends here
