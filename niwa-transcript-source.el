;;; niwa-transcript-source.el --- Proven transcript change frontiers -*- lexical-binding: t -*-

;;; Commentary:

;; Derived transcript consumers retain a change frontier instead of copying the
;; whole buffer to find a small append.  Every consumer owns its own frontier.
;; Missed modification hooks invalidate the proof; no consumer trusts a cached
;; prefix after an unobserved change.  Known leading metadata edits move origins
;; without changing the body-relative frontier.
;;
;; Hooks run with `inhibit-modification-hooks' bound, so an edit made by another
;; hook is silent.  The proof therefore brackets the other hooks: its guard
;; runs first and its seal last in `before-change-functions', and its guard
;; runs first in `after-change-functions'.  A tick that moved between guard
;; and seal, or hooks no longer in that order, fail the proof closed.
;;
;; Overlay hooks (`modification-hooks', `insert-in-front-hooks' and
;; `insert-behind-hooks') run after `before-change-functions', outside that
;; bracket, and may edit text or properties anywhere.  Any such overlay in the
;; buffer when a change begins or is sealed therefore fails the proof closed.

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct (niwa-transcript-source
               (:constructor niwa-transcript-source--create))
  "One consumer's change proof in its source buffer.
ORIGIN is a marker before the transcript body; DIRTY is its first dirty offset.
TICK records the last observed modification, including property-only edits.
SHAPE is the buffer's multibyteness and size at that point: converting
multibyteness rewrites the text without a tick or a modification hook."
  origin tick mode shape (dirty 0) before before-tick before-shape)

(defun niwa-transcript-source--shape ()
  "Return the current buffer's multibyteness and size."
  (cons enable-multibyte-characters (buffer-size)))

(defvar-local niwa-transcript-source--consumers nil
  "Change proofs owned by consumers of the current buffer.")

(defvar niwa-transcript-source--metadata-change-p nil
  "Non-nil only during known leading metadata writes.")

(defun niwa-transcript-source-create (origin)
  "Create a change proof for the current buffer body starting at ORIGIN.
The first read requires full derivation.  Call
`niwa-transcript-source-accept' only after successfully deriving from the
current source."
  (let ((source (niwa-transcript-source--create
                 :origin (copy-marker origin)
                 :tick (buffer-modified-tick) :mode major-mode
                 :shape (niwa-transcript-source--shape))))
    (push source niwa-transcript-source--consumers)
    (add-hook 'before-change-functions
              #'niwa-transcript-source--before-change -100 t)
    (add-hook 'before-change-functions
              #'niwa-transcript-source--before-seal 100 t)
    (add-hook 'after-change-functions
              #'niwa-transcript-source--after-change -100 t)
    source))

(defun niwa-transcript-source--hooks-ordered-p ()
  "Return non-nil when the proof's hooks bracket every other hook.
The raw local values are compared, so the `t' marker that expands to the
global hooks sits between guard and seal, never outside them."
  (let ((before before-change-functions)
        (after after-change-functions))
    (and (eq (car before) #'niwa-transcript-source--before-change)
         (eq (car (last before)) #'niwa-transcript-source--before-seal)
         (eq (car after) #'niwa-transcript-source--after-change))))

(defun niwa-transcript-source--overlay-hooks-p ()
  "Return non-nil when any overlay of the buffer carries modification hooks.
Every overlay is inspected, whatever its endpoints and the buffer's
narrowing: these hooks run outside the proof's bracket and may edit anywhere."
  (let ((lists (overlay-lists)))
    (catch 'found
      (dolist (list (list (car lists) (cdr lists)))
        (dolist (overlay list)
          (when (or (overlay-get overlay 'modification-hooks)
                    (overlay-get overlay 'insert-in-front-hooks)
                    (overlay-get overlay 'insert-behind-hooks))
            (throw 'found t))))
      nil)))

(defun niwa-transcript-source-change (source)
  "Return SOURCE's first dirty body offset, or nil when unchanged.
An unobserved edit, changed major mode, removed or reordered hooks or wrong
source buffer invalidates the whole proof and returns zero."
  (unless (and (eq (marker-buffer (niwa-transcript-source-origin source))
                   (current-buffer))
               (eq major-mode (niwa-transcript-source-mode source))
               (eql (buffer-modified-tick)
                    (niwa-transcript-source-tick source))
               (equal (niwa-transcript-source--shape)
                      (niwa-transcript-source-shape source))
               (niwa-transcript-source--hooks-ordered-p))
    (setf (niwa-transcript-source-dirty source) 0))
  (niwa-transcript-source-dirty source))

(defun niwa-transcript-source-accept (source)
  "Accept SOURCE's successfully derived current contents as its new baseline."
  (unless (eq (marker-buffer (niwa-transcript-source-origin source))
              (current-buffer))
    (error "Transcript proof belongs to another buffer"))
  (setf (niwa-transcript-source-dirty source) nil
        (niwa-transcript-source-before source) nil
        (niwa-transcript-source-before-tick source) nil
        (niwa-transcript-source-before-shape source) nil
        (niwa-transcript-source-tick source) (buffer-modified-tick)
        (niwa-transcript-source-shape source) (niwa-transcript-source--shape)
        (niwa-transcript-source-mode source) major-mode))

(defun niwa-transcript-source-release (source)
  "Release SOURCE's proof and its origin marker when a consumer is reset."
  (when-let* ((buffer (marker-buffer
                      (niwa-transcript-source-origin source))))
    (with-current-buffer buffer
      (setq niwa-transcript-source--consumers
            (delq source niwa-transcript-source--consumers))))
  (set-marker (niwa-transcript-source-origin source) nil))

(defun niwa-transcript-source--own-consumers ()
  "Return the current buffer's own change proofs.
A cloned buffer inherits the local consumer list and hooks of its original, but
those proofs belong to the original's buffer: the clone's edits must neither
dirty them nor adopt a tick for them."
  (seq-filter (lambda (source)
                (eq (marker-buffer (niwa-transcript-source-origin source))
                    (current-buffer)))
              niwa-transcript-source--consumers))

(defun niwa-transcript-source--before-change (start end)
  "Record the proof boundary before changing START through END.
The hook order and overlay hooks are checked now, not only when the proof is
read: a hook outside the bracket can edit silently and remove itself before
the change completes.  Only this buffer's own proofs are touched."
  ;; One scan per event, shared by every consumer.
  (let* ((own (niwa-transcript-source--own-consumers))
         (clean (and own
                     (niwa-transcript-source--hooks-ordered-p)
                     (not (niwa-transcript-source--overlay-hooks-p)))))
    (dolist (source own)
      (let ((origin (marker-position (niwa-transcript-source-origin source))))
        (when origin
          (unless (and (eql (buffer-modified-tick)
                            (niwa-transcript-source-tick source))
                       (equal (niwa-transcript-source--shape)
                              (niwa-transcript-source-shape source))
                       (not (niwa-transcript-source-before source))
                       clean)
            (setf (niwa-transcript-source-dirty source) 0))
          (unless (and niwa-transcript-source--metadata-change-p
                       (< start origin) (<= end origin))
            (let ((offset (max 0 (- start origin))))
              (setf (niwa-transcript-source-dirty source)
                    (min offset (or (niwa-transcript-source-dirty source)
                                    offset)))))
          (setf (niwa-transcript-source-before source) t
                (niwa-transcript-source-before-tick source)
                (buffer-modified-tick)
                (niwa-transcript-source-before-shape source)
                (niwa-transcript-source--shape)))))))

(defun niwa-transcript-source--before-seal (_start _end)
  "Invalidate proofs whose buffer changed silently since their guard ran.
This runs after every other `before-change-functions' member."
  ;; A hook that joined the lists during the before phase and will remove
  ;; itself afterwards is visible only here.  One scan serves every consumer.
  (let* ((own (niwa-transcript-source--own-consumers))
         (unsafe (and own
                      (or (not (niwa-transcript-source--hooks-ordered-p))
                          (niwa-transcript-source--overlay-hooks-p)))))
    (dolist (source own)
      (when (and (niwa-transcript-source-before source)
                 (or unsafe
                     (not (eql (buffer-modified-tick)
                               (niwa-transcript-source-before-tick source)))
                     (not (equal (niwa-transcript-source--shape)
                                 (niwa-transcript-source-before-shape source)))))
        (setf (niwa-transcript-source-dirty source) 0)))))

(defun niwa-transcript-source--after-change (_start _end _old-length)
  "Finish the paired change proof, including property-only modifications."
  (dolist (source (niwa-transcript-source--own-consumers))
    (unless (niwa-transcript-source-before source)
      (setf (niwa-transcript-source-dirty source) 0))
    (setf (niwa-transcript-source-before source) nil
          (niwa-transcript-source-tick source) (buffer-modified-tick)
          (niwa-transcript-source-shape source)
          (niwa-transcript-source--shape))))

(defun niwa-transcript-source-call-with-metadata-change (function)
  "Call FUNCTION while allowing proven edits strictly before body origins.
Only persistence's known leading metadata writes use this exemption.  Body
edits still invalidate their suffix, and inhibited or unpaired hooks invalidate
the entire prefix.  Origin markers follow leading metadata size changes."
  (let ((niwa-transcript-source--metadata-change-p t))
    (funcall function)))

(defun niwa-transcript-source-begin-change (start end)
  "Announce a known change of START through END and return its finish function.
Call the returned function once the change is complete, before invoking foreign
observers.  It is idempotent so an unwind cleanup can call it again safely."
  (niwa-transcript-source--before-change start end)
  (niwa-transcript-source--before-seal start end)
  (let ((buffer (current-buffer)) done)
    (lambda ()
      (unless done
        (setq done t)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (niwa-transcript-source--after-change
             start end (- end start))))))))

(defun niwa-transcript-source-call-with-change (start end function)
  "Call FUNCTION as one known change of START through END.
Consumers observe this change even when FUNCTION uses silent modifications.
An earlier unobserved edit still invalidates their proof."
  (let ((finish (niwa-transcript-source-begin-change start end)))
    (unwind-protect
        (let ((inhibit-modification-hooks t))
          (funcall function))
      (funcall finish))))

(defun niwa-transcript-source-call-with-unproven-changes (function)
  "Call FUNCTION, failing proofs closed after edits or insertion redirection.
FUNCTION is foreign code, such as a stream transformer or hook chain, whose
edits of the current buffer are unknown in position.  When the buffer's
modification tick moved by the time FUNCTION exits, normally or not, every own
proof's frontier becomes zero.  A changed point or current buffer also fails
closed: a caller may insert at that location after FUNCTION returns.
That frontier is sticky: an enclosing
`niwa-transcript-source-call-with-change' may adopt the tick afterwards but
cannot lower the frontier back.  Edits to other buffers are ignored."
  (let ((buffer (current-buffer))
        (position (point))
        (tick (buffer-modified-tick))
        (shape (niwa-transcript-source--shape)))
    (unwind-protect
        (funcall function)
      (let ((redirected (or (not (eq buffer (current-buffer)))
                            (/= position (point)))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (or redirected
                      (not (eql tick (buffer-modified-tick)))
                      (not (equal shape (niwa-transcript-source--shape))))
              (dolist (source (niwa-transcript-source--own-consumers))
                (setf (niwa-transcript-source-dirty source) 0)))))))))

(provide 'niwa-transcript-source)
;;; niwa-transcript-source.el ends here
