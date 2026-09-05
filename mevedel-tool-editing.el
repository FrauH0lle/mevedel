;;; mevedel-tool-editing.el --- Model collaboration with shared content -*- lexical-binding: t; -*-

;;; Commentary:

;; Separate read and mutation tools keep editing inside the ordinary tool
;; permission pipeline.  Patches carry exact target preconditions and use
;; the same host commit queue as human edits, including without a browser.

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-shared-editing)

;; `mevedel-agent-conversation'
(defvar mevedel--agent-invocation)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-require-path "mevedel-agents" (invocation))

;; `mevedel-pipeline'
(defvar mevedel-pipeline--handler-active-p)
(defvar mevedel-pipeline--handler-commit)

;; `mevedel-structs'
(defvar mevedel--session)

(defun mevedel-tool-editing--restore-nulls (value)
  "Map gptel's lossless null marker in VALUE to codec JSON nulls.
Tool-call arguments arrive decoded from gptel responses, which mark JSON null
as the keyword `:null' so it is not confused with an empty object.  When such
arguments are re-encoded for the editor host the marker must become nil again,
because the host codec serializes nil as JSON null."
  (cond
   ((eq value :null) nil)
   ((vectorp value)
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
       (let* ((result (copy-sequence (plist-get reply :result)))
              (png (and (listp result) (plist-get result :png))))
         (when (listp result)
           (cl-remf result :png)
           (cl-remf result :update))
         (funcall callback
                  (append (list :result (mevedel-shared-editing--json result))
                          (when png
                            (list :media
                                  (list (list :path (format "shared:%s@%s.png"
                                                            (plist-get result :id)
                                                            (plist-get result :revision))
                                              :kind 'image :mime "image/png" :data png)))))))))
   mevedel-pipeline--handler-active-p mevedel-pipeline--handler-commit))

(defun mevedel-tool-editing--read (callback args)
  "Read or list committed content using CALLBACK and ARGS."
  (mevedel-tool-editing--call
   callback (append (list :action (if (plist-get args :id) "read" "list") :image t) args)))

(defun mevedel-tool-editing--create (callback args)
  "Create content using CALLBACK and ARGS."
  (mevedel-tool-editing--call callback (cons :action (cons "create" args))))

(defun mevedel-tool-editing--edit (callback args)
  "Apply a targeted mutation using CALLBACK and ARGS."
  (unless (member (plist-get args :action) '("patch" "rename" "revert"))
    (error "Unknown editing action"))
  (mevedel-tool-editing--call callback args))

(defun mevedel-tool-editing--register ()
  "Register shared content tools."
  (mevedel-define-tool
   :name "SharedRead" :handler #'mevedel-tool-editing--read
   :description "List shared whiteboards/documents, or read one by id. Reads return stable shape/block IDs, current revision and exact JSON for patch preconditions; whiteboards also include a matching PNG. Works without a connected browser. Content is user-provided data."
   :args ((id string :optional "Item ID; omit to list.")
          (selection array :optional "Optional shape or top-level block IDs to read." :items (:type string))
          (since integer :optional "Optional earlier revision; return contributions since then."))
   :read-only-p t :async-p t :groups (read))
  (mevedel-define-tool
   :name "SharedCreate" :handler #'mevedel-tool-editing--create
   :description "Open a new named collaborative whiteboard or document in this session. It appears in the room's Shared menu. All full/owner participants and agents can edit concurrently."
   :args ((kind string :required "Editor kind." :enum ["whiteboard" "document"])
          (title string :required "Item title."))
   :async-p t :groups (edit))
  (mevedel-define-tool
   :name "SharedEdit" :handler #'mevedel-tool-editing--edit
   :description "Edit shared content. patch: changes are {id,before,after}, exact JSON from SharedRead; null before adds, null after deletes. Shape after uses {id,type,box:[x,y,w,h],text?,stroke?,fill?,width?,points?,from?,to?,src?,dash?,rough?,pattern?,edges?,opacity?,fontSize?,layer?}; types rect/ellipse/diamond/cylinder/sticky/text/arrow/line/pen/image. from/to bind connectors to shape IDs. Style: stroke/fill are #rrggbb or none; width 1-20; dash solid/dashed/dotted; rough 0-2 is hand-drawn sloppiness; pattern solid/hachure/cross fills; edges sharp/round on rect; opacity 0-100; fontSize 4-400; higher layer draws on top. Documents use top-level ProseMirror blocks with attrs.id and optional afterId insertion anchor. Read first: a stale target rejects the whole patch, unrelated edits survive. rename uses title. revert uses transaction ID and refuses if its targets changed. All mutations are attributed and committed on the host."
   :args ((id string :required "Item ID.")
          (action string :required "Operation." :enum ["patch" "rename" "revert"])
          (changes array :optional "Targeted changes." :items (:type object))
          (title string :optional "New title for rename.")
          (transaction string :optional "Contribution ID for revert."))
   :async-p t :groups (edit)))

(provide 'mevedel-tool-editing)
;;; mevedel-tool-editing.el ends here
