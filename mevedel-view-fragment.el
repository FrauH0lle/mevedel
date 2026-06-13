;;; mevedel-view-fragment.el --- View fragment primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Internal helpers for rendering disposable, identity-backed blocks of
;; view-owned chrome.  Fragment text is a cache in the view buffer: durable
;; conversation state remains in the data buffer and session structures.

;;; Code:

(eval-when-compile
  (require 'cl-lib))


;;
;;; Region and key helpers

(defun mevedel-view-fragment--region-id (region)
  "Return the identity object used for managed REGION keys."
  (cond
   ((overlayp region) region)
   ((markerp region) region)
   ((consp region) region)
   (region region)
   (t (error "Missing fragment region"))))

(defun mevedel-view-fragment--key (region fragment)
  "Return the container-scoped key for FRAGMENT in REGION."
  (let ((namespace (plist-get fragment :namespace))
        (id (plist-get fragment :id)))
    (unless namespace
      (error "Missing fragment namespace"))
    (unless id
      (error "Missing fragment id"))
    (list (mevedel-view-fragment--region-id region) namespace id)))

(defun mevedel-view-fragment--key-match-p (left right)
  "Return non-nil when LEFT and RIGHT are the same fragment key."
  (and (consp left) (consp right)
       (eq (car left) (car right))
       (eq (cadr left) (cadr right))
       (equal (caddr left) (caddr right))))

(defun mevedel-view-fragment--normalize (fragment)
  "Return FRAGMENT with defaults applied."
  (unless (plist-get fragment :namespace)
    (error "Missing fragment namespace"))
  (unless (plist-get fragment :id)
    (error "Missing fragment id"))
  (let ((normalized (copy-sequence fragment)))
    (unless (plist-member normalized :priority)
      (setq normalized (plist-put normalized :priority 0)))
    (unless (plist-member normalized :read-only)
      (setq normalized (plist-put normalized :read-only t)))
    (unless (plist-member normalized :body)
      (setq normalized (plist-put normalized :body "")))
    normalized))

(defun mevedel-view-fragment--position (position)
  "Return numeric buffer position for POSITION."
  (if (markerp position)
      (marker-position position)
    position))

(defun mevedel-view-fragment--numeric-region-p (region)
  "Return non-nil when REGION is a mutable numeric bounds cons."
  (and (consp region) (integerp (car region)) (integerp (cdr region))))

(defun mevedel-view-fragment--region-bounds (region)
  "Return `(START . END)' bounds for managed REGION."
  (cond
   ((overlayp region)
    (unless (overlay-buffer region)
      (error "Fragment region has no buffer"))
    (cons (overlay-start region) (overlay-end region)))
   ((and (consp region) (integer-or-marker-p (car region))
         (integer-or-marker-p (cdr region)))
    (cons (mevedel-view-fragment--position (car region))
          (mevedel-view-fragment--position (cdr region))))
   (t (error "Unknown fragment region: %S" region))))

(defun mevedel-view-fragment--region-start (region)
  "Return the start position for managed REGION."
  (car (mevedel-view-fragment--region-bounds region)))

(defun mevedel-view-fragment--region-end (region)
  "Return the end position for managed REGION."
  (cdr (mevedel-view-fragment--region-bounds region)))

(defun mevedel-view-fragment--set-region-bounds (region start end)
  "Set managed REGION bounds to START and END when REGION is mutable."
  (cond
   ((overlayp region)
    (move-overlay region start end (current-buffer)))
   ((and (consp region) (integer-or-marker-p (car region))
         (integer-or-marker-p (cdr region)))
    (if (markerp (car region))
        (set-marker (car region) start (current-buffer))
      (setcar region start))
    (if (markerp (cdr region))
        (set-marker (cdr region) end (current-buffer))
      (setcdr region end)))))


;;
;;; Rendering

(defun mevedel-view-fragment--section-properties (region fragment section)
  "Return text properties for FRAGMENT SECTION in REGION."
  (let* ((key (mevedel-view-fragment--key region fragment))
         (read-only (plist-get fragment :read-only))
         (props `(mevedel-view-fragment-key ,key
                  mevedel-view-fragment-region ,(car key)
                  mevedel-view-fragment-namespace ,(plist-get fragment :namespace)
                  mevedel-view-fragment-id ,(plist-get fragment :id)
                  mevedel-view-fragment-section ,section
                  mevedel-view-fragment-navigatable
                  ,(plist-get fragment :navigatable)
                  rear-nonsticky (read-only font-lock-face face keymap help-echo)
                  front-sticky (read-only))))
    (when read-only
      (setq props (append `(read-only ,read-only) props)))
    props))

(defun mevedel-view-fragment--add-default-property (text property value)
  "Return TEXT with PROPERTY set to VALUE only where absent."
  (if (or (null value) (not (stringp text)) (zerop (length text)))
      text
    (let ((copy (copy-sequence text))
          (pos 0)
          next)
      (while (< pos (length copy))
        (setq next (or (next-single-property-change pos property copy)
                       (length copy)))
        (when (null (get-text-property pos property copy))
          (put-text-property pos next property value copy))
        (setq pos next))
      copy)))

(defun mevedel-view-fragment--single-trailing-newline (text &optional force)
  "Return TEXT with exactly one trailing newline when non-empty or FORCE."
  (let* ((copy (copy-sequence text))
         (length (length copy))
         (pos length))
    (cond
     ((zerop length)
      (if force "\n" copy))
     (t
      (while (and (> pos 0)
                  (eq (aref copy (1- pos)) ?\n))
        (setq pos (1- pos)))
      (if (= pos length)
          (concat copy "\n")
        (substring copy 0 (1+ pos)))))))

(defun mevedel-view-fragment--body-text (fragment &optional force-newline)
  "Return FRAGMENT body as a string with default interactive properties.
When FORCE-NEWLINE is non-nil, return one tagged newline for an empty body."
  (let ((body (plist-get fragment :body)))
    (setq body
          (cond
           ((stringp body) (copy-sequence body))
           ((null body) "")
           (t (format "%s" body))))
    (setq body (mevedel-view-fragment--single-trailing-newline
                body force-newline))
    (setq body (mevedel-view-fragment--add-default-property
                body 'keymap (plist-get fragment :keymap)))
    (setq body (mevedel-view-fragment--add-default-property
                body 'help-echo (plist-get fragment :help-echo)))
    body))

(defun mevedel-view-fragment--label-text (fragment)
  "Return FRAGMENT label text, or nil when it has no label."
  (let ((left (plist-get fragment :label-left))
        (right (plist-get fragment :label-right)))
    (cond
     ((and left right) (format "%s %s" left right))
     (left (format "%s" left))
     (right (format "%s" right)))))

(defun mevedel-view-fragment--render (region fragment)
  "Return propertized text for FRAGMENT in managed REGION."
  (let* ((fragment (mevedel-view-fragment--normalize fragment))
         (label (mevedel-view-fragment--label-text fragment))
         (body (mevedel-view-fragment--body-text fragment (not label)))
         (text ""))
    (when label
      (setq label (concat label "\n"))
      (add-text-properties
       0 (length label)
       (mevedel-view-fragment--section-properties region fragment 'label)
       label)
      (setq text (concat text label)))
    (when (> (length body) 0)
      (add-text-properties
       0 (length body)
       (mevedel-view-fragment--section-properties region fragment 'body)
       body)
      (setq text (concat text body)))
    text))


;;
;;; Bounds and mutation

(defun mevedel-view-fragment--bounds-at (&optional position)
  "Return fragment bounds at POSITION, or nil when no fragment is there."
  (let* ((position (or position (point)))
         (key (get-text-property position 'mevedel-view-fragment-key)))
    (when key
      (let ((start position)
            (end position))
        (while (and (> start (point-min))
                    (mevedel-view-fragment--key-match-p
                     (get-text-property (1- start)
                                        'mevedel-view-fragment-key)
                     key))
          (setq start (1- start)))
        (while (and (< end (point-max))
                    (mevedel-view-fragment--key-match-p
                     (get-text-property end
                                        'mevedel-view-fragment-key)
                     key))
          (setq end (1+ end)))
        (list :start start :end end :key key
              :region (nth 0 key)
              :namespace (nth 1 key)
              :id (nth 2 key))))))

(defun mevedel-view-fragment--find-bounds (region namespace id)
  "Return bounds for fragment NAMESPACE ID inside REGION, or nil."
  (let* ((bounds (mevedel-view-fragment--region-bounds region))
         (start (car bounds))
         (end (cdr bounds))
         (key (list (mevedel-view-fragment--region-id region) namespace id))
         found-start found-end pos)
    (setq pos start)
    (while (and (< pos end) (not found-start))
      (if (mevedel-view-fragment--key-match-p
           (get-text-property pos 'mevedel-view-fragment-key) key)
          (setq found-start pos)
        (setq pos (or (next-single-property-change
                       pos 'mevedel-view-fragment-key nil end)
                      end))))
    (when found-start
      (setq found-end found-start)
      (while (and (< found-end end)
                  (mevedel-view-fragment--key-match-p
                   (get-text-property found-end
                                      'mevedel-view-fragment-key)
                   key))
        (setq found-end (1+ found-end)))
      (list :start found-start :end found-end :key key
            :region (nth 0 key) :namespace namespace :id id))))

(defun mevedel-view-fragment--clamp-inside (start end offset)
  "Return the nearest position inside START..END for OFFSET.
END is exclusive.  When the range is empty, return START."
  (if (< start end)
      (min (1- end) (+ start (max 0 offset)))
    start))

(defun mevedel-view-fragment--capture-window-states (start end)
  "Capture point/window-start offsets for windows inside START and END."
  (let (states)
    (dolist (window (get-buffer-window-list (current-buffer) nil t)
                    (nreverse states))
      (let* ((window-point (window-point window))
             (window-start (window-start window))
             (point-inside (and (<= start window-point) (< window-point end)))
             (start-inside (and window-start (<= start window-start)
                                (< window-start end))))
        (when (or point-inside start-inside)
          (push (list window
                      (if point-inside
                          (cons 'offset (- window-point start))
                        (cons 'absolute window-point))
                      (cond
                       (start-inside
                        (cons 'offset (- window-start start)))
                       (window-start
                        (cons 'absolute window-start))))
                states))))))

(defun mevedel-view-fragment--restore-window-states (states start end)
  "Restore window STATES relative to START and END."
  (dolist (state states)
    (pcase-let ((`(,window ,window-point-state ,window-start-state) state))
      (when (and (window-live-p window)
                 (eq (window-buffer window) (current-buffer)))
        (let ((point-pos
               (pcase window-point-state
                 (`(offset . ,offset)
                  (mevedel-view-fragment--clamp-inside start end offset))
                 (`(absolute . ,pos)
                  (min (point-max) pos)))))
          (set-window-point window point-pos)
          (when (eq window (selected-window))
            (goto-char point-pos)))
        (pcase window-start-state
          (`(offset . ,offset)
           (set-window-start
            window (mevedel-view-fragment--clamp-inside start end offset) t))
          (`(absolute . ,pos)
           (when (<= pos (point-max))
             (set-window-start window pos t))))))))

(defun mevedel-view-fragment--check-numeric-region-mutation (region)
  "Signal when numeric REGION overlaps foreign fragment text."
  (when (and (mevedel-view-fragment--numeric-region-p region)
             (mevedel-view-fragment--region-has-foreign-fragments-p region))
    (error "Unsafe overlapping numeric region")))

(defun mevedel-view-fragment--call-mutating (thunk &optional preserve)
  "Call THUNK for a view-owned mutation, optionally wrapped by PRESERVE."
  (let ((call (lambda ()
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t))
                  (funcall thunk)))))
    (if preserve
        (funcall preserve call)
      (funcall call))))

(defun mevedel-view-fragment--replace-region (start end text &optional preserve)
  "Replace START..END with TEXT and preserve local point/window offsets."
  (let* ((point-offset (and (<= start (point)) (< (point) end)
                            (- (point) start)))
         (window-states (mevedel-view-fragment--capture-window-states start end))
         new-end)
    (mevedel-view-fragment--call-mutating
     (lambda ()
       (goto-char start)
       (delete-region start end)
       (insert text)
       (setq new-end (point)))
     preserve)
    (when point-offset
      (goto-char (mevedel-view-fragment--clamp-inside
                  start new-end point-offset)))
    (mevedel-view-fragment--restore-window-states window-states start new-end)
    new-end))

(defun mevedel-view-fragment--insert (region fragment &optional preserve)
  "Insert FRAGMENT in REGION at point and return its bounds plist."
  (let* ((region-bounds (mevedel-view-fragment--region-bounds region))
         (region-start (car-safe region-bounds))
         (region-end (cdr-safe region-bounds))
         (start (point))
         end)
    (mevedel-view-fragment--check-numeric-region-mutation region)
    (unless (mevedel-view-fragment--safe-insert-position start start)
      (error "No safe fragment insertion point"))
    (mevedel-view-fragment--call-mutating
     (lambda ()
       (insert (mevedel-view-fragment--render region fragment))
       (setq end (point)))
     preserve)
    (mevedel-view-fragment--set-region-bounds
     region region-start (+ region-end (- end start)))
    (list :start start :end end
          :key (mevedel-view-fragment--key region fragment))))

(defun mevedel-view-fragment--update (region fragment &optional preserve)
  "Update FRAGMENT inside REGION and return its bounds plist."
  (let* ((fragment (mevedel-view-fragment--normalize fragment))
         (namespace (plist-get fragment :namespace))
         (id (plist-get fragment :id))
         (bounds (mevedel-view-fragment--find-bounds region namespace id)))
    (unless bounds
      (error "Unknown fragment: %S" (mevedel-view-fragment--key region fragment)))
    (mevedel-view-fragment--check-numeric-region-mutation region)
    (let* ((region-bounds (mevedel-view-fragment--region-bounds region))
           (region-start (car-safe region-bounds))
           (region-end (cdr-safe region-bounds))
           (start (plist-get bounds :start))
           (end (plist-get bounds :end))
           (new-end (mevedel-view-fragment--replace-region
                     start end (mevedel-view-fragment--render region fragment)
                     preserve)))
      (mevedel-view-fragment--set-region-bounds
       region region-start (+ region-end (- new-end end)))
      (list :start start :end new-end
            :key (mevedel-view-fragment--key region fragment)))))

(defun mevedel-view-fragment--delete (region namespace id &optional preserve)
  "Delete fragment NAMESPACE ID inside REGION."
  (when-let* ((bounds (mevedel-view-fragment--find-bounds region namespace id)))
    (mevedel-view-fragment--check-numeric-region-mutation region)
    (let* ((region-bounds (mevedel-view-fragment--region-bounds region))
           (region-start (car-safe region-bounds))
           (region-end (cdr-safe region-bounds))
           (start (plist-get bounds :start))
           (end (plist-get bounds :end))
           (new-end (mevedel-view-fragment--replace-region
                     start end "" preserve)))
      (mevedel-view-fragment--set-region-bounds
       region region-start (+ region-end (- new-end end))))
    t))

(defun mevedel-view-fragment--fragment< (left right)
  "Return non-nil when LEFT should render before RIGHT."
  (let ((left-priority (or (plist-get left :priority) 0))
        (right-priority (or (plist-get right :priority) 0))
        (left-order (or (plist-get left :mevedel-view-fragment--order) 0))
        (right-order (or (plist-get right :mevedel-view-fragment--order) 0)))
    (if (= left-priority right-priority)
        (< left-order right-order)
      (> left-priority right-priority))))

(defun mevedel-view-fragment--namespace-spans (region namespace)
  "Return spans for NAMESPACE fragments inside managed REGION."
  (let* ((region-id (mevedel-view-fragment--region-id region))
         (bounds (mevedel-view-fragment--region-bounds region))
         (pos (car bounds))
         (end (cdr bounds))
         spans span-start next)
    (while (< pos end)
      (if (and (eq (get-text-property pos 'mevedel-view-fragment-region)
                   region-id)
               (eq (get-text-property pos 'mevedel-view-fragment-namespace)
                   namespace))
          (progn
            (setq span-start pos)
            (while (and (< pos end)
                        (eq (get-text-property
                             pos 'mevedel-view-fragment-region)
                            region-id)
                        (eq (get-text-property
                             pos 'mevedel-view-fragment-namespace)
                            namespace))
              (setq pos (1+ pos)))
            (push (cons span-start pos) spans))
        (setq next (min (or (next-single-property-change
                             pos 'mevedel-view-fragment-namespace nil end)
                            end)
                        (or (next-single-property-change
                             pos 'mevedel-view-fragment-region nil end)
                            end)))
        (setq pos (max (1+ pos) next))))
    (nreverse spans)))

(defun mevedel-view-fragment--safe-insert-position (start end)
  "Return a START..END position that does not split a fragment run.
Return nil when every candidate inside the range would split existing
fragment text."
  (let ((pos start)
        bounds)
    (catch 'position
      (while (<= pos end)
        (setq bounds (and (< pos (point-max))
                          (mevedel-view-fragment--bounds-at pos)))
        (cond
         ((null bounds)
          (throw 'position pos))
         ((= pos (plist-get bounds :start))
          (throw 'position pos))
         ((<= (plist-get bounds :end) end)
          (setq pos (plist-get bounds :end)))
         (t
          (throw 'position nil))))
      nil)))

(defun mevedel-view-fragment--region-has-fragments-p (region)
  "Return non-nil when REGION already contains its managed fragment text."
  (let* ((region-id (mevedel-view-fragment--region-id region))
         (bounds (mevedel-view-fragment--region-bounds region))
         (pos (car bounds))
         (end (cdr bounds))
         found)
    (while (and (< pos end) (not found))
      (if (eq (get-text-property pos 'mevedel-view-fragment-region)
              region-id)
          (setq found t)
        (setq pos (or (next-single-property-change
                       pos 'mevedel-view-fragment-region nil end)
                      end))))
    found))

(defun mevedel-view-fragment--region-has-any-fragments-p (region)
  "Return non-nil when REGION contains any managed fragment text."
  (let* ((bounds (mevedel-view-fragment--region-bounds region))
         (pos (car bounds))
         (end (cdr bounds))
         found)
    (while (and (< pos end) (not found))
      (if (get-text-property pos 'mevedel-view-fragment-region)
          (setq found t)
        (setq pos (or (next-single-property-change
                       pos 'mevedel-view-fragment-region nil end)
                      end))))
    found))

(defun mevedel-view-fragment--region-has-foreign-fragments-p (region)
  "Return non-nil when REGION contains another region's fragment text."
  (let* ((region-id (mevedel-view-fragment--region-id region))
         (bounds (mevedel-view-fragment--region-bounds region))
         (pos (car bounds))
         (end (cdr bounds))
         found)
    (while (and (< pos end) (not found))
      (let ((owner (get-text-property pos 'mevedel-view-fragment-region)))
        (if (and owner (not (eq owner region-id)))
            (setq found t)
          (setq pos (or (next-single-property-change
                         pos 'mevedel-view-fragment-region nil end)
                        end)))))
    found))

(defun mevedel-view-fragment--reconcile (region namespace fragments &optional preserve)
  "Reconcile FRAGMENTS for NAMESPACE inside managed REGION.
FRAGMENTS are sorted by descending priority and caller order for equal
priorities.  Other namespaces inside REGION and same-namespace fragments
outside REGION are untouched."
  (let* ((region-id (mevedel-view-fragment--region-id region))
         (fragments
          (cl-loop for fragment in fragments
                   for order from 0
                   collect (plist-put (mevedel-view-fragment--normalize fragment)
                                      :mevedel-view-fragment--order order)))
         (fragments (sort fragments #'mevedel-view-fragment--fragment<))
         (text (mapconcat (lambda (fragment)
                            (unless (eq (plist-get fragment :namespace) namespace)
                              (error "Fragment namespace mismatch: %S" fragment))
                            (mevedel-view-fragment--render region fragment))
                          fragments ""))
         (old-bounds (mevedel-view-fragment--region-bounds region))
         (region-start (car old-bounds))
         (region-end (cdr old-bounds))
         (spans (mevedel-view-fragment--namespace-spans region namespace))
         (has-own-fragments
          (mevedel-view-fragment--region-has-fragments-p region))
         (has-any-fragments
          (mevedel-view-fragment--region-has-any-fragments-p region))
         (spans (if (or spans has-own-fragments has-any-fragments)
                    spans
                  (list (cons region-start region-end))))
         (insert-position (cond
                           (spans (caar spans))
                           (has-any-fragments
                            (or (mevedel-view-fragment--safe-insert-position
                                 region-start region-end)
                                (error "No safe fragment insertion point")))
                           (t region-start)))
         (insert-marker (copy-marker insert-position))
         (deleted-length 0)
         new-start new-end)
    (when (and (= region-start region-end)
               (not (mevedel-view-fragment--safe-insert-position
                     region-start region-end)))
      (error "No safe fragment insertion point"))
    (mevedel-view-fragment--check-numeric-region-mutation region)
    (mevedel-view-fragment--call-mutating
     (lambda ()
       (dolist (span (reverse spans))
         (cl-incf deleted-length (- (cdr span) (car span)))
         (delete-region (car span) (cdr span)))
       (goto-char insert-marker)
       (setq new-start (point))
       (insert text)
       (setq new-end (point)))
     preserve)
    (set-marker insert-marker nil)
    (mevedel-view-fragment--set-region-bounds
     region region-start (+ region-end (- (length text) deleted-length)))
    (list :start new-start :end new-end :region region-id :namespace namespace)))

(provide 'mevedel-view-fragment)
;;; mevedel-view-fragment.el ends here
