;;; mevedel-session-bounds.el --- Incremental gptel response bounds -*- lexical-binding: t -*-

;;; Commentary:

;; `gptel--get-buffer-bounds' walks every `gptel' property run of the buffer,
;; and persisting a transcript asks for the bounds several times: gptel-org
;; writes them twice per attempt and mevedel's stabilization repeats the write
;; until the drawer offsets settle.  On a multi-megabyte transcript almost
;; every run is unchanged between two checkpoints.
;;
;; This module answers the same question from a retained run list.  The runs
;; of the transcript body are kept relative to the content start (the end of
;; the leading property drawer), so drawer growth moves absolute positions
;; without touching a run.  A `mevedel-transcript-source' change proof decides
;; how much of the list is still true; anything it cannot prove, including
;; edits made with modification hooks inhibited, is recomputed from the
;; first doubtful offset, or entirely.  The result is `equal' to the original
;; function's, including run splits between equal but non-`eq' values and the
;; order of the keys.
;;
;; Only mevedel's own writers use the cache: `mevedel-session-bounds-call-
;; with-cache' marks the current buffer for the dynamic extent of its call,
;; and the advice on `gptel--get-buffer-bounds' passes every other caller
;; through unchanged.

;;; Code:

(require 'cl-lib)
(require 'mevedel-transcript-source)

;; `gptel'
(declare-function gptel--get-buffer-bounds "ext:gptel" nil)

;; `mevedel-transcript'
(declare-function mevedel-transcript--skip-leading-properties-drawer
                  "mevedel-transcript" (pos))
(autoload 'mevedel-transcript--skip-leading-properties-drawer
  "mevedel-transcript")

(cl-defstruct (mevedel-session-bounds--cache
               (:constructor mevedel-session-bounds--cache-create))
  "Retained bounds of one buffer.
SOURCE proves how much of RUNS is current.  RUNS is a vector of
`(REL-BEG REL-END PROP)' for the body, positions relative to the source's
origin.  HEAD, ORIGIN and RESULT describe the alist last returned."
  source runs head origin result)

(defvar-local mevedel-session-bounds--cache nil
  "This buffer's retained bounds, or nil.")

(defvar mevedel-session-bounds--active-buffer nil
  "The buffer whose `gptel--get-buffer-bounds' calls use the cache, or nil.")

(defvar mevedel-session-bounds--stats nil
  "Plist counting how bounds were answered, for tests and diagnostics.
Keys: `:hit' (nothing changed), `:rebuilt' (runs unchanged, positions or
head changed), `:incremental', `:full', and `:fallback' (original function).")

(defun mevedel-session-bounds--count (key)
  "Count one answer of kind KEY."
  (setq mevedel-session-bounds--stats
        (plist-put mevedel-session-bounds--stats key
                   (1+ (or (plist-get mevedel-session-bounds--stats key) 0)))))

(defun mevedel-session-bounds--overlay-gptel-p ()
  "Return non-nil when an overlay in the buffer carries a `gptel' property.
The original function reads each run's value with `get-char-property', which
an overlay overrides."
  (cl-some (lambda (overlay) (overlay-get overlay 'gptel))
           (overlays-in (point-min) (point-max))))

(defun mevedel-session-bounds--scan (from to)
  "Return the non-nil `gptel' runs between FROM and TO as `(BEG END PROP)'.
Runs are maximal spans of `eq' text property values, as the original
function's backward scan finds them, in ascending order."
  (let ((pos from)
        runs)
    (while (< pos to)
      (let ((next (or (next-single-property-change pos 'gptel nil to) to))
            (prop (get-char-property pos 'gptel)))
        (when prop
          (push (list pos next prop) runs))
        (setq pos next)))
    (nreverse runs)))

(defun mevedel-session-bounds--alist (runs)
  "Return the bounds alist for ascending RUNS of absolute `(BEG END PROP)'.
Built backward exactly as the original function builds it, so key order and
per-key order match."
  (let (bounds)
    (dolist (run (reverse runs))
      (let* ((prop (nth 2 run))
             (name (if (symbolp prop) prop (car prop)))
             (val (when (consp prop) (cdr prop)))
             (bound (if val
                        (list (nth 0 run) (nth 1 run) val)
                      (list (nth 0 run) (nth 1 run)))))
        (push bound (alist-get name bounds))))
    bounds))

(defun mevedel-session-bounds--content-start ()
  "Return the position after the leading property drawer."
  (mevedel-transcript--skip-leading-properties-drawer (point-min)))

(defun mevedel-session-bounds--reset ()
  "Drop the buffer's retained bounds and release their proof.
A cache a clone inherited holds another buffer's proof, which its owner still
uses: it is dropped here without being released."
  (when-let* ((cache mevedel-session-bounds--cache))
    (when-let* ((source (mevedel-session-bounds--cache-source cache))
                ((let ((owner (marker-buffer
                               (mevedel-transcript-source-origin source))))
                   (or (null owner) (eq owner (current-buffer))))))
      (mevedel-transcript-source-release source))
    (setq mevedel-session-bounds--cache nil)))

(defun mevedel-session-bounds--absolute (runs origin)
  "Return body RUNS, relative to ORIGIN, as absolute runs."
  (mapcar (lambda (run)
            (list (+ origin (nth 0 run)) (+ origin (nth 1 run)) (nth 2 run)))
          (append runs nil)))

(defun mevedel-session-bounds-get (original)
  "Return what ORIGINAL, the unadvised `gptel--get-buffer-bounds', returns.
Answer from the retained runs where the change proof allows it."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((origin (mevedel-session-bounds--content-start))
             (cache mevedel-session-bounds--cache))
        (cond
         ;; Neither condition has a cheap proof: compute the original way.
         ((or (mevedel-session-bounds--overlay-gptel-p)
              ;; Only a present value can continue across the body start.
              (and (> origin (point-min)) (< origin (point-max))
                   (get-text-property (1- origin) 'gptel)
                   (eq (get-text-property (1- origin) 'gptel)
                       (get-text-property origin 'gptel))))
          (mevedel-session-bounds--reset)
          (mevedel-session-bounds--count :fallback)
          (funcall original))
         (t
          (when (and cache
                     (let ((marker (mevedel-transcript-source-origin
                                    (mevedel-session-bounds--cache-source cache))))
                       (or (not (eq (marker-buffer marker) (current-buffer)))
                           (not (eql origin (marker-position marker))))))
            ;; A proof of another buffer, which a clone inherits with its
            ;; variables, or a body that no longer starts where the proof says.
            (mevedel-session-bounds--reset)
            (setq cache nil))
          (unless cache
            (setq cache (mevedel-session-bounds--cache-create
                         :source (mevedel-transcript-source-create origin)
                         :runs [])
                  mevedel-session-bounds--cache cache))
          (mevedel-session-bounds--refresh cache origin)))))))

(defun mevedel-session-bounds--refresh (cache origin)
  "Bring CACHE up to date for body ORIGIN and return the bounds alist."
  (let* ((source (mevedel-session-bounds--cache-source cache))
         (dirty (mevedel-transcript-source-change source))
         (runs (mevedel-session-bounds--cache-runs cache))
         (head (mevedel-session-bounds--scan (point-min) origin))
         (limit (point-max))
         (result (mevedel-session-bounds--cache-result cache)))
    (cond
     ;; Nothing in the body moved: the runs stand.  The leading edits that the
     ;; proof exempts can change the head and the origin, so those are rechecked.
     ((and (null dirty) result
           (eql origin (mevedel-session-bounds--cache-origin cache))
           (equal head (mevedel-session-bounds--cache-head cache)))
      (mevedel-session-bounds--count :hit)
      result)
     (t
      (let* ((count (if (and dirty (> dirty 0))
                        (mevedel-session-bounds--retained runs dirty)
                      (if (and (null dirty) (> (length runs) 0)) (length runs) 0)))
             (kept (substring runs 0 count))
             (from (if (> count 0)
                       (+ origin (nth 1 (aref kept (1- count))))
                     origin))
             ;; An unchanged body has no tail to rescan; only positions moved.
             (fresh (unless (and (null dirty) (> (length runs) 0))
                      (mevedel-session-bounds--count-width (- limit from))
                      (mevedel-session-bounds--scan from limit)))
             (new (vconcat
                   kept
                   (mapcar (lambda (run)
                             (list (- (nth 0 run) origin)
                                   (- (nth 1 run) origin) (nth 2 run)))
                           fresh))))
        (setq result (mevedel-session-bounds--alist
                      (append head (mevedel-session-bounds--absolute new origin))))
        (setf (mevedel-session-bounds--cache-runs cache) new
              (mevedel-session-bounds--cache-head cache) head
              (mevedel-session-bounds--cache-origin cache) origin
              (mevedel-session-bounds--cache-result cache) result)
        (mevedel-transcript-source-accept source)
        (mevedel-session-bounds--count
         (cond ((and (null dirty) (> (length runs) 0)) :rebuilt)
               ((and dirty (> dirty 0) (> count 0)) :incremental)
               (t :full)))
        result)))))

(defun mevedel-session-bounds--retained (runs dirty)
  "Return how many leading RUNS end strictly before body offset DIRTY.
A run reaching the first dirty offset may extend or merge, so it is rescanned."
  (let ((low 0)
        (high (length runs)))
    (while (< low high)
      (let ((middle (/ (+ low high) 2)))
        (if (< (nth 1 (aref runs middle)) dirty)
            (setq low (1+ middle))
          (setq high middle))))
    low))

(defun mevedel-session-bounds--count-width (width)
  "Add WIDTH characters to the body span rescanned so far."
  (setq mevedel-session-bounds--stats
        (plist-put mevedel-session-bounds--stats :scanned
                   (+ width (or (plist-get mevedel-session-bounds--stats :scanned)
                                0)))))

(defun mevedel-session-bounds--advice (original &rest arguments)
  "Answer `gptel--get-buffer-bounds' from the cache for the marked buffer.
ORIGINAL and ARGUMENTS are the advised function and its arguments."
  (if (eq mevedel-session-bounds--active-buffer (current-buffer))
      (mevedel-session-bounds-get (lambda () (apply original arguments)))
    (apply original arguments)))

(defun mevedel-session-bounds-install ()
  "Install the advice on `gptel--get-buffer-bounds', once."
  (unless (advice-member-p #'mevedel-session-bounds--advice
                           'gptel--get-buffer-bounds)
    (advice-add 'gptel--get-buffer-bounds :around
                #'mevedel-session-bounds--advice)))

(defun mevedel-session-bounds-call-with-cache (function)
  "Call FUNCTION with `gptel--get-buffer-bounds' cached for this buffer."
  (mevedel-session-bounds-install)
  (let ((mevedel-session-bounds--active-buffer (current-buffer)))
    (funcall function)))

(provide 'mevedel-session-bounds)

;;; mevedel-session-bounds.el ends here
