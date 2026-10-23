;;; mevedel-minimal-edit-twin.el --- Generic text mutation floor -*- lexical-binding: t; -*-

;;; Commentary:

;; Compare a whole update with a character/property-only snapshot replay.
;; The floor is one contiguous replacement, retaining the longest common
;; character prefix and then a nonoverlapping suffix, plus differing property
;; runs on retained characters.  Inserted text carries its final properties.
;; It is not a globally minimal edit script for disjoint changes.
;;
;; Observation advises only delete-region, insert, set-text-properties,
;; add-text-properties and put-text-property.  Counts describe successful
;; nonempty calls to those primitives, including redundant property writes;
;; they are not all underlying C mutations.  Advice sees zone writes even when
;; modification hooks are inhibited.  Other buffers and string properties are
;; excluded.  Compiled insert/delete-region opcodes bypass advice.  For compiled
;; observation, callers must build with those symbols' byte-compile handlers
;; temporarily nil, emitting generic calls instead; time the ordinary build.
;; Observation is separate from timing: callers must use equivalent
;; fresh fixtures for the two updates.  Real timing includes the entire update
;; thunk without observer advice.  Floor timing includes only replay primitives,
;; not snapshot creation, derivation or equality checks.  Neither measurement
;; accounts for overlays, mode state, layout or redisplay equivalence.

;;; Code:

(require 'cl-lib)

(defun mevedel-twin--same-properties-p (left right)
  "Return non-nil if property plists LEFT and RIGHT have equal entries."
  (and (= (length left) (length right))
       (cl-loop for (key value) on left by #'cddr
                always (and (plist-member right key)
                            (equal value (plist-get right key))))))

(defun mevedel-twin--apply (edit)
  "Apply EDIT's replacement and property runs in the current snapshot buffer.
Offsets in EDIT are zero-based.  `insert' carries the payload's properties
without inheriting adjoining properties; derivation uses this same primitive."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (buffer-undo-list t)
        (start (1+ (plist-get edit :start))))
    (when (> (plist-get edit :deleted) 0)
      (delete-region start (+ start (plist-get edit :deleted))))
    (when (> (plist-get edit :inserted) 0)
      (goto-char start)
      (insert (plist-get edit :text)))
    (dolist (run (plist-get edit :property-runs))
      (set-text-properties (1+ (nth 0 run)) (1+ (nth 1 run))
                           (nth 2 run)))))

(defun mevedel-twin--assert (expected)
  "Assert current buffer text equals EXPECTED including all text properties."
  (unless (equal-including-properties expected (buffer-string))
    (error "Snapshot text or properties differ")))

(defun mevedel-twin-derive (before after)
  "Derive a contiguous character/property floor from strings BEFORE and AFTER.
Return an opaque replay plist with :deleted and :inserted character counts
and :property-runs, a list of differing full-property replacement runs.
Character prefix/suffix matching ignores properties and never overlaps.
The inserted substring carries AFTER's properties without extra writes.
Property corrections cover remaining differences on retained characters.
The `insert' primitive does not inherit adjoining properties, unlike
`insert-and-inherit'.
Neither input string is modified.  Derivation is not part of replay timing."
  (let* ((before (copy-sequence before))
         (after (copy-sequence after))
         (old-length (length before))
         (new-length (length after))
         (comparison (compare-strings before nil nil after nil nil))
         (prefix (if (eq comparison t) (min old-length new-length)
                   (1- (abs comparison))))
         (reverse-comparison
          (compare-strings (reverse (substring-no-properties before prefix)) nil nil
                           (reverse (substring-no-properties after prefix)) nil nil))
         (suffix (if (eq reverse-comparison t)
                     (min (- old-length prefix) (- new-length prefix))
                   (1- (abs reverse-comparison)))))
    (let ((edit (list :before before :after after :start prefix
                      :deleted (- old-length prefix suffix)
                      :inserted (- new-length prefix suffix)
                      :text (substring after prefix (- new-length suffix)))))
      (with-temp-buffer
        (let ((inhibit-modification-hooks t))
          (insert before))
        (mevedel-twin--apply edit)
        (let ((offset 0) runs)
          (while (< offset new-length)
            (let* ((desired (text-properties-at offset after))
                   (end (min (next-property-change offset after new-length)
                             (1- (next-property-change
                                  (1+ offset) nil (1+ new-length))))))
              (unless (mevedel-twin--same-properties-p
                       (text-properties-at (1+ offset)) desired)
                (if (and runs (= (nth 1 (car runs)) offset)
                         (mevedel-twin--same-properties-p
                          (nth 2 (car runs)) desired))
                    (setf (nth 1 (car runs)) end)
                  (push (list offset end desired) runs)))
              (setq offset end)))
          (plist-put edit :property-runs (nreverse runs))))
      edit)))

(defun mevedel-twin-replay (edit)
  "Replay EDIT in the current buffer containing its BEFORE snapshot.
Assert equality including properties before and after replay.  Widen for
snapshot comparison; inhibit modification hooks, read-only checks and undo
recording during the synthetic edit.  Return EDIT."
  (save-restriction
    (widen)
    (mevedel-twin--assert (plist-get edit :before))
    (save-excursion (mevedel-twin--apply edit))
    (mevedel-twin--assert (plist-get edit :after)))
  edit)

(defun mevedel-twin-observe (update)
  "Count UPDATE, a no-argument function, in the current buffer.
Leave its real effects in place, including the caller's narrowing semantics.
Return :real-deleted, :real-inserted and :real-property-runs.
Counts are characters and nonempty property primitive calls, not bytes.
Only the five primitives listed in this file's commentary are observed.
Explicit buffer property objects are counted; string objects are excluded.
Compiled insert/delete-region opcodes bypass advice: compile the observation
build with those symbols' byte-compile handlers nil to emit generic calls.
Temporary advice is removed even when UPDATE signals; its error propagates.
Do not nest observations.  Run `mevedel-twin-measure' separately on a fresh,
equivalent fixture so instrumentation does not bias real update timing."
  (let* ((target (current-buffer))
         (deleted 0) (inserted 0) (property-runs 0)
         collecting advice)
    (unwind-protect
        (progn
          (dolist (primitive '(delete-region insert set-text-properties
                              add-text-properties put-text-property))
            (let* ((kind primitive)
                   (observer
                    (lambda (original &rest args)
                      (let* ((object (pcase kind
                                       ((or 'set-text-properties
                                            'add-text-properties) (nth 3 args))
                                       ('put-text-property (nth 4 args))))
                             (observed
                              (and collecting
                                   (if (memq kind '(delete-region insert))
                                       (eq (current-buffer) target)
                                     (eq (or object (current-buffer)) target))))
                             (count
                              (when observed
                                (if (eq kind 'insert)
                                    (cl-loop for arg in args sum
                                             (if (stringp arg) (length arg) 1))
                                  (abs (- (nth 1 args) (nth 0 args))))))
                             (result (apply original args)))
                        (when (and observed (> count 0))
                          (pcase kind
                            ('delete-region (cl-incf deleted count))
                            ('insert (cl-incf inserted count))
                            (_ (cl-incf property-runs))))
                        result))))
              (push (cons primitive observer) advice)
              (advice-add primitive :around observer)))
          (setq collecting t)
          (funcall update))
      (setq collecting nil)
      (dolist (entry advice)
        (advice-remove (car entry) (cdr entry))))
    (list :real-deleted deleted :real-inserted inserted
          :real-property-runs property-runs)))

(defun mevedel-twin-measure (update)
  "Time UPDATE and its minimal snapshot replay in the current buffer.
UPDATE is a no-argument function.  Leave its real effects in place.
Return :floor-deleted, :floor-inserted, :floor-property-runs, :real-seconds
and :floor-seconds.  Snapshot creation, derivation and assertions are untimed.
Real timing covers the whole UPDATE without installing observation advice.
Use `mevedel-twin-observe' on an equivalent fresh fixture for real counts.
UPDATE must not kill the target buffer."
  (let* ((target (current-buffer))
         (before (save-restriction (widen) (buffer-string)))
         (start (current-time))
         (real-seconds
          (progn
            (funcall update)
            (float-time (time-subtract (current-time) start))))
         (after (with-current-buffer target
                  (save-restriction (widen) (buffer-string))))
         (edit (mevedel-twin-derive before after))
         floor-seconds)
    (with-temp-buffer
      (let ((inhibit-modification-hooks t))
        (insert before))
      (let ((start (current-time)))
        (mevedel-twin--apply edit)
        (setq floor-seconds (float-time (time-subtract (current-time) start))))
      (mevedel-twin--assert after))
    (list :floor-deleted (plist-get edit :deleted)
          :floor-inserted (plist-get edit :inserted)
          :floor-property-runs (length (plist-get edit :property-runs))
          :real-seconds real-seconds :floor-seconds floor-seconds)))

(provide 'mevedel-minimal-edit-twin)
;;; mevedel-minimal-edit-twin.el ends here
