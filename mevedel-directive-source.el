;;; mevedel-directive-source.el -- Durable directive source presentations -*- lexical-binding: t -*-

;;; Commentary:

;; Owns directive anchors and every durable-record/source-presentation mutation.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-instruction-registry))

;; `mevedel-directive'
(declare-function mevedel-directive-add-subdirective
                  "mevedel-directive" (directive subdirective))
(declare-function mevedel-directive-has-activity-p
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-invalidate-plan
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-remove-subdirective
                  "mevedel-directive" (directive subdirective))
(declare-function mevedel-directive-set-request
                  "mevedel-directive" (directive request))

;; `mevedel-instruction-registry'
(declare-function mevedel--create-id "mevedel-instruction-registry" ())
(declare-function mevedel--instruction-activate-buffer
                  "mevedel-instruction-registry" (&optional buffer))
(declare-function mevedel--instruction-activate-workspace
                  "mevedel-instruction-registry" (&optional workspace))
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-instruction-registry" (buffer))
(declare-function mevedel--instruction-id
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-inlinks
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-operation-state-key
                  "mevedel-instruction-registry" ())
(declare-function mevedel--instruction-outlinks
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-state
                  "mevedel-instruction-registry" (&optional key))
(declare-function mevedel--instruction-with-uuid
                  "mevedel-instruction-registry"
                  (uuid &optional workspace))
(declare-function mevedel--instruction-workspace-key
                  "mevedel-instruction-registry" (&optional workspace))
(declare-function mevedel--retire-id "mevedel-instruction-registry" (id))
(declare-function mevedel-unlink-instructions
                  "mevedel-instruction-registry" (from-list to-list))
(defvar mevedel--instruction-state-key-override)

;; `mevedel-overlay-ui'
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlay-ui"
                  (instruction &optional update-children))

;; `mevedel-overlays'
(declare-function mevedel--child-instructions
                  "mevedel-overlays" (instruction))
(declare-function mevedel--directivep "mevedel-overlays" (instruction))
(declare-function mevedel--instructions-at
                  "mevedel-overlays" (point &optional type))
(declare-function mevedel--instructions-in
                  "mevedel-overlays" (start end &optional type))
(declare-function mevedel--nested-directives
                  "mevedel-overlays" (directive))
(declare-function mevedel--parent-instruction
                  "mevedel-overlays" (instruction &optional of-type))
(declare-function mevedel--read-directive "mevedel-overlays" (directive))
(declare-function mevedel--subinstruction-of-p
                  "mevedel-overlays" (sub parent))
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction &optional of-type pred))
(defvar mevedel--default-instruction-priority)

;; `mevedel-persistence'
(declare-function mevedel--setup-buffer-hooks
                  "mevedel-persistence" (buffer))
(declare-function mevedel-persistence-resolve-instruction-anchor
                  "mevedel-persistence"
                  (overlay-start overlay-end anchor parent-range))

;; `mevedel-structs'
(declare-function mevedel-directive--create "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-p "mevedel-structs" (cl-x))
(declare-function mevedel-directive-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-set-anchor
                  "mevedel-structs" (directive anchor))
(declare-function mevedel-directive-set-state
                  "mevedel-structs" (directive state))
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-subdirective-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-copy
                  "mevedel-structs" (subdirective))
(declare-function mevedel-subdirective-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-set-anchor
                  "mevedel-structs" (subdirective anchor))
(declare-function mevedel-subdirective-set-request
                  "mevedel-structs" (subdirective request))
(declare-function mevedel-workspace-add-directive
                  "mevedel-structs" (workspace directive))
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-remove-directive
                  "mevedel-structs" (workspace directive))

(defcustom mevedel-instruction-anchor-context-chars 160
  "Number of surrounding characters stored in instruction anchors."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-instruction-anchor-text-max-chars 8192
  "Maximum selected text size stored directly in instruction anchors.

Selections larger than this are represented by hashes and boundary
context only."
  :type 'integer
  :group 'mevedel)

(defvar-local mevedel--pending-directive-detachments nil
  "Directives wholly covered by the current buffer edit.")

(defconst mevedel--persisted-instruction-properties
  '(mevedel-instruction
    mevedel-id
    mevedel-uuid
    mevedel-instruction-type
    mevedel-instruction-collapse-p
    mevedel-links
    mevedel-reference-tags
    mevedel-commentary
    mevedel-commentary-truncated
    mevedel-directive-fail-reason
    mevedel-directive-action
    mevedel-directive-model-provider
    mevedel-directive-reasoning-effort
    mevedel-directive-prefix-tag-query
    mevedel-directive-infix-tag-query-string
    mevedel-subdirective-typename
    evaporate)
  "Overlay properties that are part of the instruction data model.

Visual and runtime properties such as faces, keymaps, display strings,
markers, and buffers are rebuilt from these values when an instruction
overlay is restored.")

(defun mevedel--instruction-serializable-value (value)
  "Return VALUE with transient text properties stripped from strings."
  (cond
   ((stringp value)
    (substring-no-properties value))
   ((consp value)
    (cons (mevedel--instruction-serializable-value (car value))
          (mevedel--instruction-serializable-value (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'mevedel--instruction-serializable-value value)))
   (t value)))

(defun mevedel--instruction-persisted-properties (instruction)
  "Return serializable persisted properties for INSTRUCTION."
  (let ((raw-properties (overlay-properties instruction))
        properties)
    (dolist (prop mevedel--persisted-instruction-properties)
      (when (memq prop raw-properties)
        (setq properties
              (plist-put properties prop
                         (mevedel--instruction-serializable-value
                          (overlay-get instruction prop))))))
    properties))

(defun mevedel--instruction-anchor-substring (start end)
  "Return buffer substring between START and END, without properties."
  (buffer-substring-no-properties
   (max (point-min) start)
   (min (point-max) end)))

(defun mevedel--instruction-anchor-for-instruction (instruction)
  "Return a lightweight restore anchor for INSTRUCTION."
  (require 'mevedel-overlays)
  (when-let* ((buffer (overlay-buffer instruction)))
    (with-current-buffer buffer
      (let* ((start (overlay-start instruction))
             (end (overlay-end instruction))
             (bodyless (= start end))
             (length (- end start))
             (parent (mevedel--parent-instruction instruction))
             (text (unless bodyless
                     (mevedel--instruction-anchor-substring start end)))
             (stored-text (and text
                               (<= (length text)
                                   mevedel-instruction-anchor-text-max-chars)
                               text))
             (context mevedel-instruction-anchor-context-chars))
        (list :schema 1
              :uuid (overlay-get instruction 'mevedel-uuid)
              :parent-uuid (and parent
                                (overlay-get parent 'mevedel-uuid))
              :bodyless bodyless
              :text-hash (and text (secure-hash 'sha256 text))
              :text stored-text
              :prefix (mevedel--instruction-anchor-substring
                       (- start context) start)
              :suffix (mevedel--instruction-anchor-substring
                       end (+ end context))
              :length length)))))

(defun mevedel--directive-record (directive)
  "Return the workspace record presented by DIRECTIVE."
  (require 'mevedel-instruction-registry)
  (require 'mevedel-structs)
  (when-let* ((buffer (overlay-buffer directive))
              (workspace (mevedel--instruction-buffer-workspace buffer))
              (id (overlay-get directive 'mevedel-uuid)))
    (cl-find id (mevedel-workspace-directives workspace)
             :key #'mevedel-directive-id :test #'equal)))

(defun mevedel--workspace-subdirective-owner (workspace id)
  "Return the parent and nested record owning ID in WORKSPACE."
  (cl-loop
   for directive in (mevedel-workspace-directives workspace)
   for subdirective =
   (cl-find id (mevedel-directive-subdirectives directive)
            :key #'mevedel-subdirective-id :test #'equal)
   when subdirective return (cons directive subdirective)))

(defun mevedel--subdirective-owner (directive &optional workspace)
  "Return the parent and nested record presented by DIRECTIVE."
  (when-let* ((id (overlay-get directive 'mevedel-uuid))
              (workspace
               (or workspace
                   (when-let* ((buffer (overlay-buffer directive)))
                     (mevedel--instruction-buffer-workspace buffer)))))
    (mevedel--workspace-subdirective-owner workspace id)))

(defun mevedel--subdirective-record (directive &optional workspace)
  "Return the parent-owned nested record presented by DIRECTIVE."
  (cdr (mevedel--subdirective-owner directive workspace)))

(defun mevedel--directive-source-record (directive)
  "Return the authored record directly presented by DIRECTIVE."
  (or (mevedel--directive-record directive)
      (mevedel--subdirective-record directive)))

(defun mevedel--detached-directive-p (directive)
  "Return non-nil when DIRECTIVE presents a detached durable record."
  (when-let* ((record (mevedel--directive-record directive)))
    (eq 'detached (plist-get (mevedel-directive-anchor record) :state))))

(defun mevedel--directive-anchor (directive)
  "Return DIRECTIVE's current attached anchor description."
  (when-let* ((buffer (overlay-buffer directive)))
    (list :state 'attached
          :file (buffer-file-name buffer)
          :start (overlay-start directive)
          :end (overlay-end directive)
          :evidence (mevedel--instruction-anchor-for-instruction directive))))

(defun mevedel--refresh-directive-anchor (directive)
  "Refresh the durable anchor presented by DIRECTIVE."
  (when-let* (((not (overlay-get directive
                                 'mevedel-transient-source-missing)))
              (record (mevedel--directive-source-record directive)))
    (let ((anchor (if (mevedel-directive-p record)
                      (mevedel-directive-anchor record)
                    (mevedel-subdirective-anchor record))))
      (if (and (eq 'detached (plist-get anchor :state))
               (= (overlay-start directive) (overlay-end directive)))
          (progn
            (setq anchor (copy-tree anchor))
            (setq anchor (plist-put anchor :file
                                    (buffer-file-name
                                     (overlay-buffer directive))))
            (setq anchor (plist-put anchor :position
                                    (overlay-start directive)))
            (if (mevedel-directive-p record)
                (mevedel-directive-set-anchor record anchor)
              (mevedel-subdirective-set-anchor record anchor)))
        (let ((new-anchor (mevedel--directive-anchor directive)))
          (if (mevedel-directive-p record)
              (mevedel-directive-set-anchor record new-anchor)
            (mevedel-subdirective-set-anchor
             record
             (append new-anchor
                     (list :properties
                           (mevedel--instruction-persisted-properties
                            directive)))))))))
  directive)

(defun mevedel--remove-directive-presentation (directive &optional buffer)
  "Remove DIRECTIVE's source presentation without deleting its record.
BUFFER identifies the former owner when DIRECTIVE has already evaporated."
  (when-let* ((buffer (or (overlay-buffer directive) buffer)))
    (mevedel--instruction-activate-buffer buffer)
    (setf (alist-get buffer (mevedel--instruction-alist))
          (delq directive (alist-get buffer (mevedel--instruction-alist))))
    (delete-overlay directive))
  directive)

(defun mevedel--mark-buffer-source-missing (buffer)
  "Remove BUFFER's source presentations and retain directives as Source missing."
  (require 'mevedel-instruction-registry)
  (require 'mevedel-overlays)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (mevedel--instruction-activate-buffer buffer)
      (let ((file (buffer-file-name buffer))
            (instructions
             (copy-sequence
              (alist-get buffer (mevedel--instruction-alist)))))
        (dolist (instruction instructions)
          (if (mevedel--directivep instruction)
              (progn
                (when-let* ((record (mevedel--directive-record instruction)))
                  (let ((anchor (mevedel-directive-anchor record)))
                    (mevedel-directive-set-anchor
                     record
                     (list :state 'source-missing
                           :file file
                           :start (overlay-start instruction)
                           :end (overlay-end instruction)
                           :evidence (or (plist-get anchor :evidence)
                                         (mevedel--instruction-anchor-for-instruction
                                          instruction))
                           :properties
                           (mevedel--instruction-persisted-properties
                            instruction)))))
                (mevedel--remove-directive-presentation instruction))
            (mevedel--delete-instruction instruction buffer)))
        (setf (mevedel--instruction-alist)
              (assq-delete-all buffer (mevedel--instruction-alist))))))
  buffer)

(defun mevedel--reconcile-directive-sources (workspace)
  "Mark live directive buffers in WORKSPACE Source missing when files vanish."
  (require 'mevedel-instruction-registry)
  (when workspace
    (mevedel--instruction-activate-workspace workspace)
    (dolist (entry (copy-sequence (mevedel--instruction-alist)))
      (when-let* (((bufferp (car entry)))
                  (buffer (car entry))
                  ((buffer-live-p buffer))
                  (file (buffer-file-name buffer))
                  ((not (file-exists-p file))))
        (mevedel--mark-buffer-source-missing buffer))))
  workspace)

(defun mevedel--reattach-directive-overlay
    (id anchor workspace buffer start end)
  "Restore ID with ANCHOR in WORKSPACE and BUFFER from START to END."
  (require 'mevedel-instruction-registry)
  (require 'mevedel-overlays)
  (with-current-buffer buffer
    (unless (and (integer-or-marker-p start)
                 (integer-or-marker-p end)
                 (<= (point-min) start end (point-max)))
      (user-error "Invalid reattachment bounds"))
    (setq-local mevedel--workspace workspace)
    (let* ((properties (plist-get anchor :properties))
           (overlay (mevedel--create-instruction-overlay-in buffer start end))
           (generated-id (overlay-get overlay 'mevedel-id)))
      (cl-loop for (property value) on properties by #'cddr
               do (overlay-put overlay property value))
      (unless (equal generated-id (overlay-get overlay 'mevedel-id))
        (mevedel--retire-id generated-id))
      (overlay-put overlay 'mevedel-instruction t)
      (overlay-put overlay 'mevedel-instruction-type 'directive)
      (overlay-put overlay 'mevedel-uuid id)
      overlay)))

(defun mevedel--reattach-directive (record workspace buffer start end)
  "Reattach source-missing RECORD in WORKSPACE to BUFFER from START to END."
  (require 'mevedel-overlay-ui)
  (unless (and (memq record (mevedel-workspace-directives workspace))
               (eq 'source-missing
                   (plist-get (mevedel-directive-anchor record) :state)))
    (user-error "Directive is not Source missing in this workspace"))
  (let ((overlay
         (mevedel--reattach-directive-overlay
          (mevedel-directive-id record)
          (mevedel-directive-anchor record)
          workspace buffer start end)))
    (mevedel-directive-set-anchor record (mevedel--directive-anchor overlay))
    (mevedel--update-instruction-overlay overlay t)
    overlay))

(defun mevedel--reattach-subdirective
    (record owner workspace buffer start end)
  "Reattach nested RECORD owned by OWNER in WORKSPACE from START to END."
  (unless (and (memq owner (mevedel-workspace-directives workspace))
               (memq record (mevedel-directive-subdirectives owner)))
    (user-error "Nested directive does not belong to this workspace"))
  (let ((parent
         (with-current-buffer buffer
           (mevedel--instruction-activate-buffer buffer)
           (cl-find (mevedel-directive-id owner)
                    (cdr (assq buffer (mevedel--instruction-alist)))
                    :key (lambda (instruction)
                           (overlay-get instruction 'mevedel-uuid))
                    :test #'equal))))
    (unless (and parent
                 (< (overlay-start parent) start)
                 (<= start end)
                 (< end (overlay-end parent)))
      (user-error "Nested directive is outside its parent"))
    (let ((overlay
           (mevedel--reattach-directive-overlay
            (mevedel-subdirective-id record)
            (mevedel-subdirective-anchor record)
            workspace buffer start end)))
      (mevedel-subdirective-set-anchor
       record
       (append (mevedel--directive-anchor overlay)
               (list :properties
                     (mevedel--instruction-persisted-properties overlay))))
      (mevedel--update-instruction-overlay overlay t)
      overlay)))

(defun mevedel-archive-directive (record workspace)
  "Archive activity-owning RECORD in WORKSPACE and hide its source presentation."
  (require 'mevedel-directive)
  (require 'mevedel-instruction-registry)
  (require 'mevedel-overlay-ui)
  (require 'mevedel-overlays)
  (require 'mevedel-structs)
  (unless (memq record (mevedel-workspace-directives workspace))
    (user-error "Directive does not belong to this workspace"))
  (unless (mevedel-directive-has-activity-p record)
    (user-error "Directive has no activity to archive"))
  (let ((properties nil))
    (when-let* ((overlay
                 (mevedel--instruction-with-uuid
                  (mevedel-directive-id record) workspace)))
      (setq properties (mevedel--instruction-persisted-properties overlay))
      (dolist (nested (mevedel--nested-directives overlay))
        (mevedel--remove-directive-presentation nested))
      (mevedel--remove-directive-presentation overlay))
    ;; Normalize every prior anchor shape (attached, detached,
    ;; source-missing) into the one archived shape the codec accepts; a
    ;; detached anchor carries only a zero-width :position.
    (let* ((anchor (mevedel-directive-anchor record))
           (position (plist-get anchor :position)))
      (mevedel-directive-set-anchor
       record
       (list :state 'archived
             :file (plist-get anchor :file)
             :start (or (plist-get anchor :start) position)
             :end (or (plist-get anchor :end) position)
             :evidence (plist-get anchor :evidence)
             :properties (or (plist-get anchor :properties) properties)))))
  record)

(defun mevedel--detach-directive (entry)
  "Replace the evaporated directive described by ENTRY at a zero-width anchor."
  (let* ((old (plist-get entry :overlay))
         (record (plist-get entry :record))
         (marker (plist-get entry :marker))
         (position (marker-position marker))
         (properties (plist-get entry :properties))
         (old-anchor (mevedel-directive-anchor record))
         (new (make-overlay position position (current-buffer))))
    (cl-loop for (property value) on properties by #'cddr
             unless (eq property 'evaporate)
             do (overlay-put new property value))
    (overlay-put new 'mevedel-instruction-collapse-p nil)
    (delete-overlay old)
    (setf (alist-get (current-buffer) (mevedel--instruction-alist))
          (cons new
                (delq old
                      (alist-get (current-buffer)
                                 (mevedel--instruction-alist)))))
    (mevedel-directive-set-anchor
     record
     (list :state 'detached
           :file (buffer-file-name)
           :position position
           :source-order (list (plist-get entry :start)
                               (plist-get entry :end))
           :evidence (plist-get old-anchor :evidence)))
    (set-marker marker nil)
    new))

(defun mevedel--instruction-before-change (beg end)
  "Capture attached directive trees wholly deleted between BEG and END."
  (setq mevedel--pending-directive-detachments nil)
  (when (< beg end)
    (dolist (directive (mevedel--instructions-in beg end 'directive))
      (let ((start (overlay-start directive))
            (finish (overlay-end directive))
            (owner (mevedel--topmost-instruction directive 'directive)))
        (when (and (< start finish)
                   (<= beg start)
                   (<= finish end)
                   (not (mevedel--detached-directive-p directive)))
          (if-let* ((record (mevedel--directive-record directive)))
              (push (list :overlay directive
                          :record record
                          :marker (copy-marker start)
                          :start start
                          :end finish
                          :properties
                          (mevedel--instruction-persisted-properties directive))
                    mevedel--pending-directive-detachments)
            (when (and owner
                       (<= beg (overlay-start owner))
                       (<= (overlay-end owner) end))
              (push (list :overlay directive :buffer (current-buffer))
                    mevedel--pending-directive-detachments))))))))

(defun mevedel--order-detached-directives-at (position)
  "Give detached directives at POSITION stable source-order priorities."
  (let* ((directives
          (sort
           (cl-remove-if-not #'mevedel--detached-directive-p
                             (mevedel--instructions-at position 'directive))
           (lambda (a b)
             (let* ((a-record (mevedel--directive-record a))
                    (b-record (mevedel--directive-record b))
                    (a-order
                     (plist-get (mevedel-directive-anchor a-record)
                                :source-order))
                    (b-order
                     (plist-get (mevedel-directive-anchor b-record)
                                :source-order)))
               (or (< (car a-order) (car b-order))
                   (and (= (car a-order) (car b-order))
                        (or (< (cadr a-order) (cadr b-order))
                            (and (= (cadr a-order) (cadr b-order))
                                 (string-lessp
                                  (mevedel-directive-id a-record)
                                  (mevedel-directive-id b-record))))))))))
         (priority (+ mevedel--default-instruction-priority
                      (length directives))))
    (dolist (directive directives)
      (overlay-put directive 'priority priority)
      (setq priority (1- priority)))))

(defun mevedel--instruction-after-change (beg end _old-length)
  "Detach captured directives, then redraw instructions affected by BEG and END."
  (let ((pending (prog1 mevedel--pending-directive-detachments
                   (setq mevedel--pending-directive-detachments nil))))
    (dolist (entry pending)
      (if (plist-get entry :record)
          (mevedel--detach-directive entry)
        (mevedel--remove-directive-presentation
         (plist-get entry :overlay)
         (plist-get entry :buffer)))))
  (let ((beg (max (point-min) (1- beg)))
        (end (min (point-max) (1+ end))))
    (dolist (instruction (mevedel--instructions-in beg end))
      (mevedel--update-instruction-overlay instruction))))

(defun mevedel--register-directive (directive request)
  "Register DIRECTIVE and REQUEST under their top-level workspace owner."
  (let* ((buffer (overlay-buffer directive))
         (workspace (mevedel--instruction-buffer-workspace buffer)))
    (unless workspace
      (error "Directive has no workspace"))
    (let ((owner (mevedel--topmost-instruction directive 'directive)))
      (if (eq owner directive)
          (let ((record (mevedel-directive--create
                         :id (overlay-get directive 'mevedel-uuid)
                         :request request
                         :anchor (mevedel--directive-anchor directive)
                         :state nil)))
            (mevedel-workspace-add-directive workspace record)
            record)
        (let ((record
               (mevedel-subdirective--create
                :id (overlay-get directive 'mevedel-uuid)
                :request request
                :anchor
                (append
                 (mevedel--directive-anchor directive)
                 (list :properties
                       (mevedel--instruction-persisted-properties
                        directive))))))
          (mevedel-directive-add-subdirective
           (or (mevedel--directive-record owner)
               (error "Parent directive record not found"))
           record)
          record)))))

(defun mevedel--directive-state (directive)
  "Return DIRECTIVE's lifecycle state."
  (or (when-let* ((owner
                   (mevedel--topmost-instruction directive 'directive))
                  (record (mevedel--directive-record owner)))
        (mevedel-directive-state record))
      'ready))

(defun mevedel--directive-status (directive)
  "Return DIRECTIVE's presentation status, or nil when Ready."
  (let* ((owner (mevedel--topmost-instruction directive 'directive))
         (record (and owner (mevedel--directive-record owner)))
         (planning-status (and record
                               (plist-get (mevedel-directive-plan record)
                                          :status)))
         (state (mevedel--directive-state directive)))
    (or (pcase planning-status
          ('planning 'planning)
          ('proposed 'plan-ready)
          ('accepted 'plan-accepted)
          ('implementing 'implementing))
        (unless (eq state 'ready) state))))

(defun mevedel--set-directive-status (directive status)
  "Set DIRECTIVE's workspace-owned transient STATUS."
  (when-let* ((owner (mevedel--topmost-instruction directive 'directive))
              (record (mevedel--directive-record owner)))
    (mevedel-directive-set-state record status))
  status)

(defun mevedel--create-instruction-overlay-in (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (require 'mevedel-instruction-registry)
  (require 'mevedel-persistence)
  (make-local-variable 'mevedel--after-change-functions-hooked)
  (mevedel--instruction-activate-buffer buffer)
  (with-current-buffer buffer
    (let ((is-bufferlevel
           ;; Check if the overlay spans the start and end of the buffer. If it
           ;; does, make it sticky so that additions to edges of the buffer will
           ;; cause it to expand there. This is useful for when we want to
           ;; append new text to the end of the buffer but don't want to
           ;; "invalidate" the buffer-level status of the instruction.
           (and (= start (point-min)) (= end (point-max)))))
      (let ((overlay (make-overlay start end (current-buffer) (not is-bufferlevel) is-bufferlevel)))
        (overlay-put overlay 'mevedel-instruction t)
        (overlay-put overlay 'mevedel-id (mevedel--create-id))
        (unless (overlay-get overlay 'mevedel-uuid)
          (overlay-put overlay 'mevedel-uuid (mevedel--create-uuid)))
        (push overlay (alist-get buffer (mevedel--instruction-alist)))
        (unless (bound-and-true-p mevedel--after-change-functions-hooked)
          (setq-local mevedel--after-change-functions-hooked t)
          (add-hook 'before-change-functions
                    #'mevedel--instruction-before-change nil t)
          (add-hook 'after-change-functions
                    #'mevedel--instruction-after-change nil t))
        (mevedel--setup-buffer-hooks buffer)
        overlay))))

(defun mevedel--create-uuid ()
  "Generate a random UUID."
  (let ((s (md5 (format "%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        (current-time)
                        (emacs-uptime)
                        (random)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (format "%x" (+ 8 (random 4)))
            (substring s 17 20)
            (substring s 20 32))))

(defun mevedel--submitted-subdirectives (directive)
  "Return immutable snapshots of DIRECTIVE's currently submitted details."
  (mapcar #'mevedel-subdirective-copy
          (mevedel-directive-subdirectives
           (or (mevedel--directive-record directive)
               (error "Directive record not found")))))

(defun mevedel--create-reference-in (buffer start end)
  "Create a region reference from START to END in BUFFER."
  (require 'mevedel-overlay-ui)
  (require 'mevedel-overlays)
  (let ((ov (mevedel--create-instruction-overlay-in buffer start end)))
    (overlay-put ov 'mevedel-instruction-type 'reference)
    (overlay-put ov 'evaporate t)
    (mevedel--update-instruction-overlay ov t)
    ov))

(defun mevedel--create-directive-in (buffer start end &optional bodyless directive-text)
  "Create a region directive from START to END in BUFFER.

This function switches to another buffer midway of execution.  BODYLESS
controls special formatting if non-nil.

DIRECTIVE-TEXT is used as the default directive.  Having DIRECTIVE-TEXT
be non-nil prevents the opening of a prompt buffer."
  (require 'mevedel-directive)
  (require 'mevedel-overlay-ui)
  (require 'mevedel-overlays)
  (require 'mevedel-structs)
  (let ((ov (mevedel--create-instruction-overlay-in buffer start end)))
    (unless bodyless
      (overlay-put ov 'evaporate t))
    (overlay-put ov 'mevedel-instruction-type 'directive)
    (mevedel--register-directive ov (or directive-text ""))
    (mevedel--update-instruction-overlay ov (not bodyless))
    (unless directive-text
      (deactivate-mark)
      (mevedel--read-directive ov))
    ov))

(defun mevedel--delete-instruction (instruction &optional buffer)
  "Delete the INSTRUCTION overlay and return it.

If the overlay is already dead, just perform the cleanup.
BUFFER is required in order to perform cleanup on a dead instruction."
  (require 'mevedel-directive)
  (require 'mevedel-instruction-registry)
  (require 'mevedel-overlay-ui)
  (require 'mevedel-overlays)
  ;; We want to handle this function in two different ways. The first way
  ;; handles regular deletion, i.e. when the function was invoked on an existing
  ;; instruction. The second way is for when the instruction was deleted
  ;; uncanonically through text manipulation. In the latter case, the function
  ;; will be called during a cleanup routine and the instruction will not be
  ;; alive.
  (when (overlay-get instruction 'mevedel-marked-for-deletion)
    (error "Instruction %s already marked for deletion" instruction))
  (let* ((instruction-buffer
          (or (overlay-buffer instruction) buffer (current-buffer)))
         (workspace
          (mevedel--instruction-buffer-workspace instruction-buffer))
         (record
          (and (mevedel--directivep instruction)
               (mevedel--directive-record instruction)))
         (subdirective-owner
          (and (mevedel--directivep instruction)
               (mevedel--subdirective-owner instruction workspace))))
    (when (and record (mevedel-directive-has-activity-p record))
      (user-error "Archive directives with activity"))
    (mevedel--instruction-activate-workspace workspace)
    (when (and record (overlay-buffer instruction))
      (dolist (nested (reverse (mevedel--nested-directives instruction)))
        (mevedel--delete-instruction nested instruction-buffer)))
    (cond
     (record
      (mevedel-workspace-remove-directive workspace record))
     (subdirective-owner
      (mevedel-directive-remove-subdirective
       (car subdirective-owner) (cdr subdirective-owner))))
    (overlay-put instruction 'mevedel-marked-for-deletion t)
    (cl-labels ((cleanup (instr cleanup-buffer)
                  (let ((id (mevedel--instruction-id instr)))
                    (mevedel--retire-id id)
                    (with-current-buffer cleanup-buffer
                      (mevedel-unlink-instructions
                       `(,id) (mevedel--instruction-outlinks instr))
                      (mevedel-unlink-instructions
                       (mevedel--instruction-inlinks instr) `(,id))))
                  (setf (cdr (assoc cleanup-buffer
                                    (mevedel--instruction-alist)))
                        (delq instr
                              (cdr (assoc cleanup-buffer
                                          (mevedel--instruction-alist)))))))
      (let ((ov-buffer (overlay-buffer instruction)))
        (when (buffer-live-p ov-buffer)
          (let ((children (mevedel--child-instructions instruction)))
            (delete-overlay instruction)
            (dolist (child children)
              (mevedel--update-instruction-overlay child t))))
        (cleanup instruction instruction-buffer))))
  instruction)

(defun mevedel--directive-text (directive)
  "Return the directive text of the DIRECTIVE overlay.

Returns an empty string if there is no directive text."
  (or (when-let* ((record (mevedel--directive-record directive)))
        (mevedel-directive-request record))
      (when-let* ((record (mevedel--subdirective-record directive)))
        (mevedel-subdirective-request record))
      ""))

(defun mevedel--set-directive-request (directive request)
  "Set DIRECTIVE's current REQUEST without changing its identity."
  (let ((record (mevedel--directive-source-record directive)))
    (cond
     ((mevedel-directive-p record)
     (mevedel-directive-set-request record request))
     (record
     (mevedel-subdirective-set-request record request)
      (when-let* ((owner (mevedel--topmost-instruction
                          directive 'directive))
                  (parent-record (mevedel--directive-record owner))
                  ((mevedel-directive-plan parent-record)))
        (mevedel-directive-invalidate-plan parent-record)))
     (t (error "Directive record not found")))))

(defun mevedel--restore-subdirectives-in-buffer (workspace buffer file)
  "Restore parent-owned directive details for FILE in BUFFER."
  (let ((mevedel--instruction-state-key-override
         (mevedel--instruction-workspace-key workspace)))
    (mevedel--instruction-activate-workspace workspace)
    (let ((restored 0)
          (instructions
           (cdr (assq buffer (mevedel--instruction-alist)))))
      (dolist (owner (mevedel-workspace-directives workspace))
        (when-let* ((parent
                     (cl-find (mevedel-directive-id owner) instructions
                              :key (lambda (instruction)
                                     (overlay-get instruction 'mevedel-uuid))
                              :test #'equal)))
          (dolist (record (mevedel-directive-subdirectives owner))
            (let ((anchor (mevedel-subdirective-anchor record)))
              (when (and (stringp (plist-get anchor :file))
                         (file-equal-p file (plist-get anchor :file)))
                (if-let* ((overlay
                           (cl-find (mevedel-subdirective-id record)
                                    instructions
                                    :key (lambda (instruction)
                                           (overlay-get instruction
                                                        'mevedel-uuid))
                                    :test #'equal)))
                    (when (mevedel--subinstruction-of-p overlay parent)
                      (mevedel--refresh-directive-anchor overlay))
                  (with-current-buffer buffer
                    (when-let* ((bounds
                                 (mevedel-persistence-resolve-instruction-anchor
                                  (plist-get anchor :start)
                                  (plist-get anchor :end)
                                  (plist-get anchor :evidence)
                                  (cons (overlay-start parent)
                                        (overlay-end parent)))))
                      (mevedel--reattach-subdirective
                       record owner workspace buffer
                       (car bounds) (cdr bounds))
                      (cl-incf restored)))))))))
      restored)))

(defun mevedel--restore-source-missing-directives (buffer)
  "Reattach exact unambiguous source-missing directives returning in BUFFER."
  (require 'mevedel-instruction-registry)
  (require 'mevedel-overlays)
  (require 'mevedel-persistence)
  (require 'mevedel-structs)
  (let* ((workspace (mevedel--instruction-buffer-workspace buffer))
         (file (buffer-file-name buffer))
         (restored 0))
    (when (and workspace file)
      (dolist (record (mevedel-workspace-directives workspace))
        (let ((anchor (mevedel-directive-anchor record)))
          (when (and (eq 'source-missing (plist-get anchor :state))
                     (file-equal-p file (plist-get anchor :file)))
            (with-current-buffer buffer
              ;; Without evidence there is nothing to resolve against;
              ;; the raw 0/0 fallback would signal inside find-file.
              (when-let* ((evidence (plist-get anchor :evidence))
                          (bounds
                           (mevedel-persistence-resolve-instruction-anchor
                            0 0 evidence nil)))
                (mevedel--reattach-directive
                 record workspace buffer (car bounds) (cdr bounds))
                (cl-incf restored))))))
      (cl-incf restored
               (mevedel--restore-subdirectives-in-buffer
                workspace buffer file)))
    restored))

(provide 'mevedel-directive-source)
;;; mevedel-directive-source.el ends here
