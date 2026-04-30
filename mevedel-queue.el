;;; mevedel-queue.el -- Shared session FIFO queue helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Private FIFO machinery shared by the permission and PresentPlan
;; queues.  Queue semantics live here; permission/plan modules provide
;; session-slot accessors, renderers, callback settlement, and optional
;; coalescing.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-structs)

(defvar mevedel--session)
(defvar mevedel--data-buffer)

(cl-defstruct (mevedel-queue-spec (:constructor mevedel-queue-spec--create))
  name
  get
  set
  render
  settle
  coalesce
  render-error-outcome
  entry-origin)

(defvar mevedel-queue--settled-cells
  (make-hash-table :test #'eq :weakness 'key)
  "Internal entry identity table for exactly-once queue settlement.")

(defun mevedel-queue--current-session ()
  "Resolve the session struct for a queue operation."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer) mevedel--data-buffer
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-queue--name (spec)
  "Return SPEC's debug name."
  (or (mevedel-queue-spec-name spec) 'queue))

(defun mevedel-queue--get (spec session)
  "Return SPEC's queue in SESSION."
  (funcall (mevedel-queue-spec-get spec) session))

(defun mevedel-queue--set (spec session queue)
  "Set SPEC's queue in SESSION to QUEUE."
  (funcall (mevedel-queue-spec-set spec) session queue))

(defun mevedel-queue--ensure-settled-cell (entry)
  "Return ENTRY's settled cell, adding one when absent."
  (or (gethash entry mevedel-queue--settled-cells)
      (let ((cell (cons nil nil)))
        (puthash entry cell mevedel-queue--settled-cells)
        cell)))

(defun mevedel-queue--safe-settle (spec entry outcome phase)
  "Settle ENTRY with OUTCOME through SPEC during PHASE.
Returns non-nil when this call delivered the outcome.  Duplicate
settlement is ignored."
  (let ((cell (mevedel-queue--ensure-settled-cell entry)))
    (unless (car cell)
      (setcar cell t)
      (condition-case err
          (funcall (mevedel-queue-spec-settle spec) entry outcome)
        (error
         (display-warning
          'mevedel
          (format "%s: %s callback error: %S"
                  (mevedel-queue--name spec) phase err)
          :warning)))
      t)))

(defun mevedel-queue--render-head (spec session)
  "Render SPEC's current head in SESSION."
  (when-let* ((queue (mevedel-queue--get spec session))
              (head (car queue)))
    (condition-case err
        (funcall (mevedel-queue-spec-render spec) head)
      (error
       (display-warning
        'mevedel
        (format "%s: render error: %S" (mevedel-queue--name spec) err)
        :warning)
       (mevedel-queue--pop
        spec head
        (if-let* ((fn (mevedel-queue-spec-render-error-outcome spec)))
            (funcall fn head err)
          'aborted))))))

(defun mevedel-queue--enqueue (spec entry &optional session)
  "Append ENTRY to SPEC's queue and render the head when needed."
  (let ((session (or session (mevedel-queue--current-session))))
    (if (not session)
        (progn
          (display-warning
           'mevedel
           (format "%s: enqueue with no session" (mevedel-queue--name spec))
           :warning)
          (mevedel-queue--safe-settle spec entry 'aborted "no-session"))
      (let* ((entry (append entry (list :session session)))
             (_ (mevedel-queue--ensure-settled-cell entry))
             (queue (mevedel-queue--get spec session))
             (was-empty (null queue)))
        (mevedel-queue--set spec session (append queue (list entry)))
        (when was-empty
          (mevedel-queue--render-head spec session))))))

(defun mevedel-queue--pop (spec entry outcome)
  "Settle queue head ENTRY with OUTCOME and render the next head."
  (let* ((session (plist-get entry :session))
         (queue (and session (mevedel-queue--get spec session))))
    (cond
     ((not session)
      (mevedel-queue--safe-settle spec entry outcome "pop"))
     ((not (eq entry (car queue)))
      (display-warning
       'mevedel
       (format "%s: stale queue entry settlement ignored"
               (mevedel-queue--name spec))
       :warning))
     (t
      (mevedel-queue--set spec session (cdr queue))
      (mevedel-queue--safe-settle spec entry outcome "pop")
      (when-let* ((coalesce (mevedel-queue-spec-coalesce spec)))
        (condition-case err
            (funcall coalesce outcome session)
          (error
           (display-warning
            'mevedel
            (format "%s: coalesce error: %S"
                    (mevedel-queue--name spec) err)
            :warning))))
      (mevedel-queue--render-head spec session)))))

(defun mevedel-queue--abort-all (spec reason &optional session)
  "Flush SPEC's queue in SESSION, settling every entry with REASON."
  (let* ((session (or session (mevedel-queue--current-session)))
         (queue (and session (mevedel-queue--get spec session))))
    (when session
      (mevedel-queue--set spec session nil))
    (dolist (entry queue)
      (mevedel-queue--safe-settle spec entry reason "abort"))))

(defun mevedel-queue--sweep-origin (spec origin outcome &optional session)
  "Settle queued entries whose origin equals ORIGIN with OUTCOME."
  (let* ((session (or session (mevedel-queue--current-session)))
         (queue (and session (mevedel-queue--get spec session)))
         (head-before (car queue))
         (kept nil)
         (swept nil))
    (dolist (entry queue)
      (if (equal (funcall (mevedel-queue-spec-entry-origin spec) entry) origin)
          (push entry swept)
        (push entry kept)))
    (when session
      (let ((new-queue (nreverse kept)))
        (mevedel-queue--set spec session new-queue)
        (dolist (entry (nreverse swept))
          (mevedel-queue--safe-settle spec entry outcome "sweep"))
        (when (and new-queue (not (eq head-before (car new-queue))))
          (mevedel-queue--render-head spec session))))))

(provide 'mevedel-queue)

;;; mevedel-queue.el ends here
