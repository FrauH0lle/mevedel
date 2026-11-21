;;; niwa-session-bounds.el --- Incremental gptel response bounds -*- lexical-binding: t -*-

;;; Commentary:

;; `gptel--get-buffer-bounds' walks every `gptel' property run of the buffer,
;; and persisting a transcript asks for the bounds several times: gptel-org
;; writes them twice per attempt and niwa's stabilization repeats the write
;; until the drawer offsets settle.  On a multi-megabyte transcript almost
;; every run is unchanged between two checkpoints.
;;
;; This module answers the same question from a retained run list.  The runs
;; of the transcript body are kept relative to the content start (the end of
;; the leading property drawer), so drawer growth moves absolute positions
;; without touching a run.  A `niwa-transcript-source' change proof decides
;; how much of the list is still true; anything it cannot prove, including
;; edits made with modification hooks inhibited, is recomputed from the
;; first doubtful offset, or entirely.  The result is `equal' to the original
;; function's, including run splits between equal but non-`eq' values and the
;; order of the keys.
;;
;; Only niwa's own writers use the cache: `niwa-session-bounds-call-
;; with-cache' marks the current buffer for the dynamic extent of its call,
;; and the advice on `gptel--get-buffer-bounds' passes every other caller
;; through unchanged.

;;; Code:

(require 'cl-lib)
(require 'niwa-transcript-source)

;; `gptel'
(declare-function gptel--get-buffer-bounds "ext:gptel" nil)

;; `niwa-transcript'
(declare-function niwa-transcript--skip-leading-properties-drawer
                  "niwa-transcript" (pos))
(autoload 'niwa-transcript--skip-leading-properties-drawer
  "niwa-transcript")

(cl-defstruct (niwa-session-bounds--cache
               (:constructor niwa-session-bounds--cache-create))
  "Retained bounds of one buffer.
SOURCE proves how much of RUNS is current.  RUNS is a vector of
`(REL-BEG REL-END PROP)' for the body, positions relative to the source's
origin.  HEAD, ORIGIN and RESULT describe the alist last returned."
  source runs head origin result)

(defvar-local niwa-session-bounds--cache nil
  "This buffer's retained bounds, or nil.")

(defvar niwa-session-bounds--active-buffer nil
  "The buffer whose `gptel--get-buffer-bounds' calls use the cache, or nil.")

(defvar niwa-session-bounds--stats nil
  "Plist counting how bounds were answered, for tests and diagnostics.
Keys: `:hit' (nothing changed), `:rebuilt' (runs unchanged, positions or
head changed), `:incremental', `:full', and `:fallback' (original function).")

(defun niwa-session-bounds--count (key)
  "Count one answer of kind KEY."
  (setq niwa-session-bounds--stats
        (plist-put niwa-session-bounds--stats key
                   (1+ (or (plist-get niwa-session-bounds--stats key) 0)))))

(defun niwa-session-bounds--overlay-gptel-p ()
  "Return non-nil when an overlay in the buffer carries a `gptel' property.
The original function reads each run's value with `get-char-property', which
an overlay overrides."
  (cl-some (lambda (overlay) (overlay-get overlay 'gptel))
           (overlays-in (point-min) (point-max))))

(defun niwa-session-bounds--scan (from to)
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

(defun niwa-session-bounds--alist (runs)
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

(defun niwa-session-bounds--content-start ()
  "Return the position after the leading property drawer."
  (niwa-transcript--skip-leading-properties-drawer (point-min)))

(defun niwa-session-bounds--reset ()
  "Drop the buffer's retained bounds and release their proof.
A cache a clone inherited holds another buffer's proof, which its owner still
uses: it is dropped here without being released."
  (when-let* ((cache niwa-session-bounds--cache))
    (when-let* ((source (niwa-session-bounds--cache-source cache))
                ((let ((owner (marker-buffer
                               (niwa-transcript-source-origin source))))
                   (or (null owner) (eq owner (current-buffer))))))
      (niwa-transcript-source-release source))
    (setq niwa-session-bounds--cache nil)))

(defun niwa-session-bounds--absolute (runs origin)
  "Return body RUNS, relative to ORIGIN, as absolute runs."
  (mapcar (lambda (run)
            (list (+ origin (nth 0 run)) (+ origin (nth 1 run)) (nth 2 run)))
          (append runs nil)))

(defun niwa-session-bounds-get (original)
  "Return what ORIGINAL, the unadvised `gptel--get-buffer-bounds', returns.
Answer from the retained runs where the change proof allows it."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((origin (niwa-session-bounds--content-start))
             (cache niwa-session-bounds--cache))
        (cond
         ;; Neither condition has a cheap proof: compute the original way.
         ((or (niwa-session-bounds--overlay-gptel-p)
              ;; Only a present value can continue across the body start.
              (and (> origin (point-min)) (< origin (point-max))
                   (get-text-property (1- origin) 'gptel)
                   (eq (get-text-property (1- origin) 'gptel)
                       (get-text-property origin 'gptel))))
          (niwa-session-bounds--reset)
          (niwa-session-bounds--count :fallback)
          (funcall original))
         (t
          (when (and cache
                     (let ((marker (niwa-transcript-source-origin
                                    (niwa-session-bounds--cache-source cache))))
                       (or (not (eq (marker-buffer marker) (current-buffer)))
                           (not (eql origin (marker-position marker))))))
            ;; A proof of another buffer, which a clone inherits with its
            ;; variables, or a body that no longer starts where the proof says.
            (niwa-session-bounds--reset)
            (setq cache nil))
          (unless cache
            (setq cache (niwa-session-bounds--cache-create
                         :source (niwa-transcript-source-create origin)
                         :runs [])
                  niwa-session-bounds--cache cache))
          (niwa-session-bounds--refresh cache origin)))))))

(defun niwa-session-bounds--refresh (cache origin)
  "Bring CACHE up to date for body ORIGIN and return the bounds alist."
  (let* ((source (niwa-session-bounds--cache-source cache))
         (dirty (niwa-transcript-source-change source))
         (runs (niwa-session-bounds--cache-runs cache))
         (head (niwa-session-bounds--scan (point-min) origin))
         (limit (point-max))
         (result (niwa-session-bounds--cache-result cache)))
    (cond
     ;; Nothing in the body moved: the runs stand.  The leading edits that the
     ;; proof exempts can change the head and the origin, so those are rechecked.
     ((and (null dirty) result
           (eql origin (niwa-session-bounds--cache-origin cache))
           (equal head (niwa-session-bounds--cache-head cache)))
      (niwa-session-bounds--count :hit)
      result)
     (t
      (let* ((count (if (and dirty (> dirty 0))
                        (niwa-session-bounds--retained runs dirty)
                      (if (and (null dirty) (> (length runs) 0)) (length runs) 0)))
             (kept (substring runs 0 count))
             (from (if (> count 0)
                       (+ origin (nth 1 (aref kept (1- count))))
                     origin))
             ;; An unchanged body has no tail to rescan; only positions moved.
             (fresh (unless (and (null dirty) (> (length runs) 0))
                      (niwa-session-bounds--count-width (- limit from))
                      (niwa-session-bounds--scan from limit)))
             (new (vconcat
                   kept
                   (mapcar (lambda (run)
                             (list (- (nth 0 run) origin)
                                   (- (nth 1 run) origin) (nth 2 run)))
                           fresh))))
        (setq result (niwa-session-bounds--alist
                      (append head (niwa-session-bounds--absolute new origin))))
        (setf (niwa-session-bounds--cache-runs cache) new
              (niwa-session-bounds--cache-head cache) head
              (niwa-session-bounds--cache-origin cache) origin
              (niwa-session-bounds--cache-result cache) result)
        (niwa-transcript-source-accept source)
        (niwa-session-bounds--count
         (cond ((and (null dirty) (> (length runs) 0)) :rebuilt)
               ((and dirty (> dirty 0) (> count 0)) :incremental)
               (t :full)))
        result)))))

(defun niwa-session-bounds--retained (runs dirty)
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

(defun niwa-session-bounds--count-width (width)
  "Add WIDTH characters to the body span rescanned so far."
  (setq niwa-session-bounds--stats
        (plist-put niwa-session-bounds--stats :scanned
                   (+ width (or (plist-get niwa-session-bounds--stats :scanned)
                                0)))))

(defun niwa-session-bounds--advice (original &rest arguments)
  "Answer `gptel--get-buffer-bounds' from the cache for the marked buffer.
ORIGINAL and ARGUMENTS are the advised function and its arguments."
  (if (eq niwa-session-bounds--active-buffer (current-buffer))
      (niwa-session-bounds-get (lambda () (apply original arguments)))
    (apply original arguments)))

(defun niwa-session-bounds-install ()
  "Install the advice on `gptel--get-buffer-bounds', once."
  (unless (advice-member-p #'niwa-session-bounds--advice
                           'gptel--get-buffer-bounds)
    (advice-add 'gptel--get-buffer-bounds :around
                #'niwa-session-bounds--advice)))

(defun niwa-session-bounds-call-with-cache (function)
  "Call FUNCTION with `gptel--get-buffer-bounds' cached for this buffer."
  (niwa-session-bounds-install)
  (let ((niwa-session-bounds--active-buffer (current-buffer)))
    (funcall function)))

(provide 'niwa-session-bounds)

;;; niwa-session-bounds.el ends here
