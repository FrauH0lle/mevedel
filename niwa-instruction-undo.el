;;; niwa-instruction-undo.el --- Undo source presentations -*- lexical-binding: t; -*-

;;; Commentary:

;; Text undo does not restore overlays.  Fence edge edits and extra movements
;; with paired entries that install geometry after primitive text undo.
;; Only anchors and presentations are restored, never conversation activity.

;;; Code:

(require 'cl-lib)
(require 'niwa-instruction-registry)
(require 'niwa-structs)

;; `niwa-note-source'
(declare-function niwa--delete-instruction
                  "niwa-note-source" (instruction &optional buffer))
(declare-function niwa--instruction-persisted-properties
                  "niwa-note-source" (instruction &optional source-only))
(declare-function niwa--instruction-replacement-forget
                  "niwa-note-source" (overlay))
(declare-function niwa--instruction-replacement-restore
                  "niwa-note-source" (overlay entry))
(declare-function niwa--instruction-replacement-settle-undo
                  "niwa-note-source" ())
;; `niwa-overlay-ui'
(declare-function niwa--update-instruction-overlay
                  "niwa-overlay-ui" (instruction &optional update-children))

(defun niwa-instruction-undo--state (overlay &optional original)
  "Snapshot OVERLAY's source state, retaining ORIGINAL's identity if supplied."
  (let* ((identity (or original overlay))
         (links (overlay-get overlay 'niwa-links)))
    (list :overlay identity
          :start (overlay-start overlay) :end (overlay-end overlay)
          :properties
          (niwa--instruction-persisted-properties
           overlay (eq (overlay-get overlay 'niwa-instruction-type) 'note))
          :replacement (overlay-get overlay 'niwa-replacement-pending)
          :replacement-growing (overlay-get overlay 'niwa-replacement-growing)
          :edges
          (append
           (mapcar (lambda (id)
                     (cons (overlay-get identity 'niwa-uuid)
                           (when-let* ((target (niwa--instruction-with-id id)))
                             (overlay-get target 'niwa-uuid))))
                   (plist-get links :to))
           (mapcar (lambda (id)
                     (cons (when-let* ((source (niwa--instruction-with-id id)))
                             (overlay-get source 'niwa-uuid))
                           (overlay-get identity 'niwa-uuid)))
                   (plist-get links :from))))))

(defun niwa-instruction-undo-capture (beg end instructions)
  "Capture INSTRUCTIONS whose edges touch the edit between BEG and END.
Strictly interior edits rely on ordinary marker shifts.  Keep their undo
checkpoint available so an actual extra geometry change can promote it later."
  (unless (or undo-in-progress (eq buffer-undo-list t))
    (let (states)
      (dolist (overlay instructions)
        (when (and
                   (or (< (overlay-start overlay) (overlay-end overlay))
                       (overlay-get overlay 'niwa-replacement-pending))
                   (not (and (< (overlay-start overlay) beg)
                             (< end (overlay-end overlay)))))
          (push (niwa-instruction-undo--state overlay) states)))
      (when instructions
        ;; Native insert undo can extend this interval in place.  Retain only
        ;; its two integers, not a copy of the buffer's undo history.
        (let ((head (car buffer-undo-list)))
          (vector buffer-undo-list states
                  (when (and (consp head) (integerp (car head)) (integerp (cdr head)))
                    (cons (car head) (cdr head)))))))))

(defun niwa-instruction-undo-extend (token overlay start end)
  "Promote TOKEN for an extra OVERLAY movement beyond native marker shifts.
START and END are the original, pre-edit bounds.  Capture properties and
anchors before adjustment, detachment, or redraw changes the presentation."
  (when (and token
             (not (cl-find overlay (aref token 1)
                           :key (lambda (state) (plist-get state :overlay)))))
    (let ((state (niwa-instruction-undo--state overlay)))
      (setf (plist-get state :start) start
            (plist-get state :end) end)
      (push state (aref token 1)))))

(defun niwa-instruction-undo-record (token instructions)
  "Fence successful edits from TOKEN after cleanup of local INSTRUCTIONS."
  (when (and token (aref token 1) (not (eq buffer-undo-list t)))
    (let (before after prefix (tail buffer-undo-list))
      (dolist (state (aref token 1))
        (let* ((old (plist-get state :overlay))
               (uuid (overlay-get old 'niwa-uuid))
               (live (if (overlay-buffer old) old
                       (cl-find uuid instructions
                                :key (lambda (ov) (overlay-get ov 'niwa-uuid))
                                :test #'equal))))
          (push state before)
          (push (if (and live (overlay-buffer live))
                      (niwa-instruction-undo--state live old)
                    (list :overlay old))
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
                (append (list (list 'apply #'niwa-instruction-undo--arm after))
                        (nreverse prefix)
                        (list (list 'apply #'niwa-instruction-undo--install before))
                        tail)))))))

(defun niwa-instruction-undo--arm (states)
  "Queue installation of STATES after the inverse text operation.
This fence executes before text undo.  Its inverse will execute after text
redo, so surviving overlay coordinates are never installed in the wrong text."
  (push (list 'apply #'niwa-instruction-undo--install states) buffer-undo-list))

(defun niwa-instruction-undo--install (states)
  "Install source STATES after text undo, arming the next inverse operation."
  (let ((buffer (current-buffer))
        (workspace (niwa--instruction-buffer-workspace (current-buffer)))
        installed redraw)
    (niwa--instruction-activate-buffer buffer)
    (dolist (state states)
      (let* ((overlay (plist-get state :overlay))
             (uuid (overlay-get overlay 'niwa-uuid))
             owned-id)
        ;; Explicit note deletion wins over old source undo.
        (when (and
                   (not (and (eq (overlay-get overlay 'niwa-instruction-type) 'note)
                             (overlay-get overlay 'niwa-marked-for-deletion))))
          (niwa--instruction-replacement-forget overlay)
          (dolist (live (copy-sequence (alist-get buffer (niwa--instruction-alist))))
            (when (equal uuid (overlay-get live 'niwa-uuid))
              (setq owned-id (overlay-get live 'niwa-id))
              (if (not (plist-get state :start))
                  (niwa--delete-instruction live buffer)
                (niwa--delete-instruction-overlay live))
              (setf (alist-get buffer (niwa--instruction-alist))
                    (delq live (alist-get buffer (niwa--instruction-alist))))))
          (when (plist-get state :start)
            (overlay-put overlay 'evaporate nil)
            (move-overlay overlay (plist-get state :start) (plist-get state :end) buffer)
            (overlay-put overlay 'niwa-marked-for-deletion nil)
            (cl-loop for (prop value) on (plist-get state :properties) by #'cddr
                     do (overlay-put overlay prop value))
            (let* ((id (overlay-get overlay 'niwa-id))
                   (occupied (and (gethash id (niwa--instruction-id-usage-map))
                                  (not (eql id owned-id)))))
              (if occupied
                  (overlay-put overlay 'niwa-id (niwa--create-id))
                (puthash id t (niwa--instruction-id-usage-map))
                (setf (niwa--instruction-retired-ids)
                      (delq id (niwa--instruction-retired-ids)))))
            (overlay-put overlay 'niwa-links nil)
            (push overlay (alist-get buffer (niwa--instruction-alist)))
            (when-let* ((replacement (plist-get state :replacement)))
              (niwa--instruction-replacement-restore overlay replacement))
            (when-let* ((growing (plist-get state :replacement-growing))
                        ((not (plist-get growing :closed))))
              (niwa--instruction-replacement-restore overlay growing)
              (overlay-put overlay 'niwa-replacement-pending nil)
              (overlay-put overlay 'niwa-replacement-growing growing))
            (push state installed)
            (cl-pushnew overlay redraw)))))
    (dolist (state installed)
      (dolist (edge (plist-get state :edges))
        (when-let* ((from (and (car edge)
                              (niwa--instruction-with-uuid (car edge) workspace)))
                    (to (and (cdr edge)
                             (niwa--instruction-with-uuid (cdr edge) workspace))))
          ;; Detachment preserves the surviving endpoint's half of a link.
          ;; Restore each direction independently, rather than requiring both
          ;; halves to have disappeared together.
          (let* ((from-links (copy-tree (overlay-get from 'niwa-links)))
                 (to-links (copy-tree (overlay-get to 'niwa-links))))
            (cl-pushnew (overlay-get to 'niwa-id) (plist-get from-links :to))
            (cl-pushnew (overlay-get from 'niwa-id) (plist-get to-links :from))
            (overlay-put from 'niwa-links from-links)
            (overlay-put to 'niwa-links to-links)
            (cl-pushnew from redraw)
            (cl-pushnew to redraw)))))
    (dolist (overlay redraw)
      (unless (overlay-get overlay 'niwa-replacement-pending)
        (niwa--update-instruction-overlay overlay t)))
    (niwa--instruction-replacement-settle-undo)
    (push (list 'apply #'niwa-instruction-undo--arm states) buffer-undo-list)))

(provide 'niwa-instruction-undo)
;;; niwa-instruction-undo.el ends here
