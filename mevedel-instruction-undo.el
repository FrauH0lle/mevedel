;;; mevedel-instruction-undo.el --- Undo source presentations -*- lexical-binding: t; -*-

;;; Commentary:

;; Text undo does not restore overlays.  Fence edge edits and extra movements
;; with paired entries that install geometry after primitive text undo.
;; Only anchors and presentations are restored, never directive activity.

;;; Code:

(require 'cl-lib)
(require 'mevedel-instruction-registry)
(require 'mevedel-structs)

;; `mevedel-directive'
(declare-function mevedel-directive-set-anchor
                  "mevedel-directive" (directive anchor))
(declare-function mevedel-subdirective-set-anchor
                  "mevedel-directive" (directive anchor))

;; `mevedel-directive-source'
(declare-function mevedel--delete-instruction
                  "mevedel-directive-source" (instruction &optional buffer))
(declare-function mevedel--directive-source-record
                  "mevedel-directive-source" (directive))
(declare-function mevedel--instruction-persisted-properties
                  "mevedel-directive-source" (instruction))
(declare-function mevedel--subdirective-owner
                  "mevedel-directive-source" (directive &optional workspace))

;; `mevedel-overlay-ui'
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlay-ui" (instruction &optional update-children))

(defun mevedel-instruction-undo--state (overlay &optional original)
  "Snapshot OVERLAY's source state, retaining ORIGINAL's identity if supplied."
  (let* ((identity (or original overlay))
         (record (mevedel--directive-source-record overlay))
         (owner (mevedel--subdirective-owner overlay))
         (links (overlay-get overlay 'mevedel-links)))
    (list :overlay identity
          :start (overlay-start overlay) :end (overlay-end overlay)
          :properties (mevedel--instruction-persisted-properties overlay)
          :record record :owner (car owner)
          :anchor (copy-tree
                   (cond ((mevedel-directive-p record) (mevedel-directive-anchor record))
                         ((mevedel-subdirective-p record) (mevedel-subdirective-anchor record))))
          :edges
          (append
           (mapcar (lambda (id)
                     (cons (overlay-get identity 'mevedel-uuid)
                           (when-let* ((target (mevedel--instruction-with-id id)))
                             (overlay-get target 'mevedel-uuid))))
                   (plist-get links :to))
           (mapcar (lambda (id)
                     (cons (when-let* ((source (mevedel--instruction-with-id id)))
                             (overlay-get source 'mevedel-uuid))
                           (overlay-get identity 'mevedel-uuid)))
                   (plist-get links :from))))))

(defun mevedel-instruction-undo-capture (beg end instructions)
  "Capture INSTRUCTIONS whose edges touch the edit between BEG and END.
Strictly interior edits rely on ordinary marker shifts.  Keep their undo
checkpoint available so an actual extra geometry change can promote it later."
  (unless (or undo-in-progress (eq buffer-undo-list t))
    (let (states)
      (dolist (overlay instructions)
        (when (and
                   (< (overlay-start overlay) (overlay-end overlay))
                   (not (and (< (overlay-start overlay) beg)
                             (< end (overlay-end overlay)))))
          (push (mevedel-instruction-undo--state overlay) states)))
      (when instructions
        ;; Native insert undo can extend this interval in place.  Retain only
        ;; its two integers, not a copy of the buffer's undo history.
        (let ((head (car buffer-undo-list)))
          (vector buffer-undo-list states
                  (when (and (consp head) (integerp (car head)) (integerp (cdr head)))
                    (cons (car head) (cdr head)))))))))

(defun mevedel-instruction-undo-extend (token overlay start end)
  "Promote TOKEN for an extra OVERLAY movement beyond native marker shifts.
START and END are the original, pre-edit bounds.  Capture properties and
anchors before adjustment, detachment, or redraw changes the presentation."
  (when (and token
             (not (cl-find overlay (aref token 1)
                           :key (lambda (state) (plist-get state :overlay)))))
    (let ((state (mevedel-instruction-undo--state overlay)))
      (setf (plist-get state :start) start
            (plist-get state :end) end)
      (push state (aref token 1)))))

(defun mevedel-instruction-undo-record (token instructions)
  "Fence successful edits from TOKEN after cleanup of local INSTRUCTIONS."
  (when (and token (aref token 1) (not (eq buffer-undo-list t)))
    (let (before after prefix (tail buffer-undo-list))
      (dolist (state (aref token 1))
        (let* ((old (plist-get state :overlay))
               (uuid (overlay-get old 'mevedel-uuid))
               (live (if (overlay-buffer old) old
                       (cl-find uuid instructions
                                :key (lambda (ov) (overlay-get ov 'mevedel-uuid))
                                :test #'equal))))
          (push state before)
          (push (if (and live (overlay-buffer live))
                      (mevedel-instruction-undo--state live old)
                    (list :overlay old
                          :record (plist-get state :record)
                          :owner (plist-get state :owner)
                          :preserve-record
                          (when-let* ((owner (plist-get state :owner)))
                            (eq 'detached
                                (plist-get (mevedel-directive-anchor owner) :state)))))
                after)))
      (while (and (consp tail) (not (eq tail (aref token 0))))
        (push (car tail) prefix)
        (setq tail (cdr tail)))
      (when (eq tail (aref token 0))
        (let ((old (aref token 2))
              (head (car tail)))
          (when (and old (consp head) (eql (car old) (car head))
                     (integerp (cdr head)) (< (cdr old) (cdr head)))
            ;; Split a coalesced append so the fence covers only this edit,
            ;; without boundaries or changes to the preceding undo history.
            (push (cons (cdr old) (cdr head)) prefix)
            (setq tail (cons old (cdr tail)))))
        (when (and before prefix)
          (setq buffer-undo-list
                (append (list (list 'apply #'mevedel-instruction-undo--arm after))
                        (nreverse prefix)
                        (list (list 'apply #'mevedel-instruction-undo--install before))
                        tail)))))))

(defun mevedel-instruction-undo--arm (states)
  "Queue installation of STATES after the inverse text operation.
This fence executes before text undo.  Its inverse will execute after text
redo, so surviving overlay coordinates are never installed in the wrong text."
  (push (list 'apply #'mevedel-instruction-undo--install states) buffer-undo-list))

(defun mevedel-instruction-undo--install (states)
  "Install source STATES after text undo, arming the next inverse operation."
  (let ((buffer (current-buffer))
        (workspace (mevedel--instruction-buffer-workspace (current-buffer)))
        installed redraw)
    (mevedel--instruction-activate-buffer buffer)
    (dolist (state states)
      (let* ((overlay (plist-get state :overlay))
             (uuid (overlay-get overlay 'mevedel-uuid))
             (record (plist-get state :record))
             (owner (plist-get state :owner))
             ;; Detaching a parent removes nested presentations without
             ;; retiring their durable records' reserved numeric identities.
             (owned-id
              (when (and owner
                         (memq record (mevedel-directive-subdirectives owner)))
                (plist-get (plist-get (mevedel-subdirective-anchor record)
                                      :properties)
                           'mevedel-id)))
             (eligible
              (or (not record)
                  (and (memq (or owner record) (mevedel-workspace-directives workspace))
                       (not (memq (plist-get (mevedel-directive-anchor (or owner record)) :state)
                                  '(archived source-missing)))))))
        ;; An explicit archive/removal after the edit wins over old text undo.
        (when eligible
          (dolist (live (copy-sequence (alist-get buffer (mevedel--instruction-alist))))
            (when (equal uuid (overlay-get live 'mevedel-uuid))
              (setq owned-id (overlay-get live 'mevedel-id))
              (if (and (not (plist-get state :start))
                       (not (plist-get state :preserve-record))
                       (not (mevedel-directive-p (mevedel--directive-source-record live))))
                  (mevedel--delete-instruction live buffer)
                (delete-overlay live))
              (setf (alist-get buffer (mevedel--instruction-alist))
                    (delq live (alist-get buffer (mevedel--instruction-alist))))))
          (when (plist-get state :start)
            (overlay-put overlay 'evaporate nil)
            (move-overlay overlay (plist-get state :start) (plist-get state :end) buffer)
            (overlay-put overlay 'mevedel-marked-for-deletion nil)
            (cl-loop for (prop value) on (plist-get state :properties) by #'cddr
                     do (overlay-put overlay prop value))
            (let* ((id (overlay-get overlay 'mevedel-id))
                   (occupied (and (gethash id (mevedel--instruction-id-usage-map))
                                  (not (eql id owned-id)))))
              (if occupied
                  (overlay-put overlay 'mevedel-id (mevedel--create-id))
                (puthash id t (mevedel--instruction-id-usage-map))
                (setf (mevedel--instruction-retired-ids)
                      (delq id (mevedel--instruction-retired-ids)))))
            (overlay-put overlay 'mevedel-links nil)
            (push overlay (alist-get buffer (mevedel--instruction-alist)))
            (when record
              (if owner
                  (progn
                    (cl-pushnew record (mevedel-directive-subdirectives owner))
                    (mevedel-subdirective-set-anchor record (copy-tree (plist-get state :anchor))))
                (mevedel-directive-set-anchor record (copy-tree (plist-get state :anchor)))))
            (push state installed)
            (cl-pushnew overlay redraw)))))
    (dolist (state installed)
      (dolist (edge (plist-get state :edges))
        (when-let* ((from (and (car edge)
                              (mevedel--instruction-with-uuid (car edge) workspace)))
                    (to (and (cdr edge)
                             (mevedel--instruction-with-uuid (cdr edge) workspace))))
          ;; Detachment preserves the surviving endpoint's half of a link.
          ;; Restore each direction independently, rather than requiring both
          ;; halves to have disappeared together.
          (let* ((from-links (copy-tree (overlay-get from 'mevedel-links)))
                 (to-links (copy-tree (overlay-get to 'mevedel-links))))
            (cl-pushnew (overlay-get to 'mevedel-id) (plist-get from-links :to))
            (cl-pushnew (overlay-get from 'mevedel-id) (plist-get to-links :from))
            (overlay-put from 'mevedel-links from-links)
            (overlay-put to 'mevedel-links to-links)
            (cl-pushnew from redraw)
            (cl-pushnew to redraw)))))
    (dolist (overlay redraw)
      (mevedel--update-instruction-overlay overlay t))
    (push (list 'apply #'mevedel-instruction-undo--arm states) buffer-undo-list)))

(provide 'mevedel-instruction-undo)
;;; mevedel-instruction-undo.el ends here
