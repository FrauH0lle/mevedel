;;; mevedel-view-prepare.el --- Deferred tool source preparation -*- lexical-binding: t -*-

;;; Commentary:
;; Own large tool parsing jobs for each view.  The parser runs sequentially in
;; one Lisp thread, waiting at its explicit checkpoints.  Only the main thread
;; consumes prepared data and invokes renderers.  Jobs retain source identities,
;; never view coordinates, and cannot publish after replacement or teardown.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p "mevedel-transport" (&optional directory))

;; `mevedel-view'
(declare-function mevedel-view--unattended-p "mevedel-view" (&optional buffer))

;; `mevedel-view-render'
(declare-function mevedel-view--debug-log "mevedel-view-render" (event &rest fields))
(declare-function mevedel-view--prepare-tool-segment "mevedel-view-render" (data start end))
(declare-function mevedel-view--render-live-region "mevedel-view-render" (data settle &optional start end))
(declare-function mevedel-view--segment-rendering "mevedel-view-render" (data start end &optional collapsed))
(declare-function mevedel-view--tool-cache-key "mevedel-view-render" (data start end collapsed))
(declare-function mevedel-view-render-batched-full "mevedel-view-render" ())
(declare-function mevedel-view-render--refresh-source "mevedel-view-render" (data start end))
(declare-function mevedel-view-render-invalidate-live-tail "mevedel-view-render" ())
(declare-function mevedel-view-render-mutate "mevedel-view-render" (key function &optional replacement cleanup))
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view-render--parse-checkpoint)

;; `mevedel-view-segments'
(declare-function mevedel-view-segments-display-buffer "mevedel-view-segments" ())

;; `mevedel-view-stream'
(declare-function mevedel-view-stream-in-flight-turn-start-position "mevedel-view-stream" ())

(defvar mevedel-view-prepare-enabled nil
  "Non-nil while a scheduled projection may defer large collapsed tools.
Explicit synchronous projections and expansion leave this nil.")

(defvar mevedel-view-prepare--min-size (* 256 1024)
  "Minimum source characters for staged tool preparation.")

(cl-defstruct (mevedel-view-prepare--job
               (:constructor mevedel-view-prepare--job-create))
  view source start end key tick thread result error kill-hook
  (gate (make-condition-variable (make-mutex) "mevedel tool preparation")))

(defvar-local mevedel-view-prepare--jobs nil
  "Pending jobs in admission order; only the first may have a live thread.")
(defvar-local mevedel-view-prepare--timer nil
  "Main-thread callback which advances this view's preparation queue.")
(defvar-local mevedel-view-prepare--failed nil
  "Source identities whose preparation needs the synchronous fallback.")

(defun mevedel-view-prepare--retire (job)
  "Release JOB's source hook and stop any unfinished parsing."
  (setq mevedel-view-prepare--jobs (delq job mevedel-view-prepare--jobs))
  (when (buffer-live-p (mevedel-view-prepare--job-source job))
    (with-current-buffer (mevedel-view-prepare--job-source job)
      (remove-hook 'kill-buffer-hook (mevedel-view-prepare--job-kill-hook job) t)))
  (when-let* ((thread (mevedel-view-prepare--job-thread job))
              ((thread-live-p thread)))
    (thread-signal thread 'quit nil))
  (setf (mevedel-view-prepare--job-result job) nil))

(defun mevedel-view-prepare-cancel ()
  "Cancel this view's source jobs and release their source-buffer hooks."
  (when mevedel-view-prepare--timer
    (cancel-timer mevedel-view-prepare--timer))
  (setq mevedel-view-prepare--timer nil)
  (dolist (job (copy-sequence mevedel-view-prepare--jobs))
    (mevedel-view-prepare--retire job))
  (setq mevedel-view-prepare--failed nil))

(defun mevedel-view-prepare-initialize ()
  "Initialize preparation ownership in the current view."
  (mevedel-view-prepare-cancel)
  (add-hook 'change-major-mode-hook #'mevedel-view-prepare-cancel nil t)
  (add-hook 'kill-buffer-hook #'mevedel-view-prepare-cancel nil t))

(defun mevedel-view-prepare--current-p (job)
  "Return non-nil when JOB still owns the same source and view lifetime."
  (let ((view (mevedel-view-prepare--job-view job))
        (source (mevedel-view-prepare--job-source job)))
    (and (buffer-live-p view) (buffer-live-p source)
         (with-current-buffer source
           (<= (point-min) (mevedel-view-prepare--job-start job)
               (mevedel-view-prepare--job-end job) (point-max)))
         (with-current-buffer view
           (and (memq job mevedel-view-prepare--jobs)
                (eq source (mevedel-view-segments-display-buffer))
                (let* ((old (mevedel-view-prepare--job-key job))
                       (key (mevedel-view--tool-cache-key
                             source (mevedel-view-prepare--job-start job)
                             (mevedel-view-prepare--job-end job) t)))
                  ;; Bounded rendering identities can be evicted while a job
                  ;; waits.  An unchanged buffer tick proves the same text;
                  ;; session context and provenance must still match exactly.
                  (when (and (= (mevedel-view-prepare--job-tick job)
                                (with-current-buffer source (buffer-modified-tick)))
                             (equal (nthcdr 4 old) (nthcdr 4 key)))
                    (setf (mevedel-view-prepare--job-key job) key))
                  (equal (mevedel-view-prepare--job-key job) key)))))))

(defun mevedel-view-prepare-resume ()
  "Schedule this view's next preparation callback once."
  (when (and mevedel-view-prepare--jobs
             (not mevedel-view-prepare--timer)
             (not (mevedel-view--unattended-p)))
    (setq mevedel-view-prepare--timer
          (run-at-time
           (if (mevedel-transport-busy-p
                (buffer-local-value 'default-directory
                                    (mevedel-view-prepare--job-source
                                     (car mevedel-view-prepare--jobs))))
               0.1 0.001)
           nil #'mevedel-view-prepare--advance (current-buffer)))))

(defun mevedel-view-prepare--checkpoint (job)
  "Validate JOB and return interpreter time to the main thread."
  (unless (mevedel-view-prepare--current-p job) (signal 'quit nil))
  ;; Unlike `sleep-for', this wait cannot dispatch editor timers or process
  ;; sentinels in the worker.  The main-thread advance callback admits each
  ;; stage only while the view is attended and its transport is idle.
  (let ((gate (mevedel-view-prepare--job-gate job)))
    (with-mutex (condition-mutex gate)
      (condition-wait gate)))
  (unless (mevedel-view-prepare--current-p job) (signal 'quit nil)))

(defun mevedel-view-prepare--start (job)
  "Start JOB's parser without inheriting view mutation or renderer bindings."
  (let ((environment (copy-sequence process-environment)))
    (setf (mevedel-view-prepare--job-thread job)
          (make-thread
           (lambda ()
             (condition-case err
                 (let ((process-environment environment)
                       (mevedel-view-render--parse-checkpoint
                        (lambda () (mevedel-view-prepare--checkpoint job))))
                   (mevedel-view-prepare--checkpoint job)
                   (setf (mevedel-view-prepare--job-result job)
                         (mevedel-view--prepare-tool-segment
                          (mevedel-view-prepare--job-source job)
                          (mevedel-view-prepare--job-start job)
                          (mevedel-view-prepare--job-end job))))
               ((error quit) (setf (mevedel-view-prepare--job-error job) err)))
             nil)
           "mevedel tool preparation"))))

(defun mevedel-view-prepare--pending (job)
  "Return an honest, non-toggleable pending row for JOB."
  (let ((name "Tool"))
    (with-current-buffer (mevedel-view-prepare--job-source job)
      (save-excursion
        (goto-char (mevedel-view-prepare--job-start job))
        (when (re-search-forward
               "(\\s-*:name[ \t]+\"\\([^\"\n]+\\)\""
               (min (mevedel-view-prepare--job-end job) (+ (point) 4096)) t)
          (setq name (match-string-no-properties 1)))))
    (list :pending (list :header (concat name ": preparing result…")
                         :vtype 'tool-preparing :status 'running
                         :initially-collapsed-p t))))

(defun mevedel-view-prepare-get (source start end key)
  "Return prepared data or a pending row for a large collapsed source span.
SOURCE, START, END and KEY identify the exact source generation.  Return nil
when ordinary synchronous preparation should handle it.  Call in the view."
  (when (and mevedel-view-prepare-enabled key (featurep 'threads)
             (>= (- end start) mevedel-view-prepare--min-size)
             (not (member key mevedel-view-prepare--failed)))
    (setq start (nth 1 key) end (nth 2 key))
    (dolist (queued mevedel-view-prepare--jobs)
      (when (and (eq source (mevedel-view-prepare--job-source queued))
                 (= start (mevedel-view-prepare--job-start queued))
                 (= end (mevedel-view-prepare--job-end queued)))
        (mevedel-view-prepare--current-p queued)))
    (let ((job (cl-find key mevedel-view-prepare--jobs
                        :key #'mevedel-view-prepare--job-key :test #'equal)))
      (unless job
        (let ((view (current-buffer)))
          (setq job (mevedel-view-prepare--job-create
                     :view view :source source :start start :end end :key key
                     :tick (with-current-buffer source (buffer-modified-tick))
                     :kill-hook (lambda ()
                                  (when (buffer-live-p view)
                                    (with-current-buffer view
                                      (mevedel-view-prepare-cancel))))))
          (with-current-buffer source
            (add-hook 'kill-buffer-hook (mevedel-view-prepare--job-kill-hook job) nil t))
          (setq mevedel-view-prepare--jobs
                (nconc mevedel-view-prepare--jobs (list job)))))
      (if-let* ((thread (mevedel-view-prepare--job-thread job))
                ((not (thread-live-p thread)))
                (result (mevedel-view-prepare--job-result job)))
          (let ((usable (and (mevedel-view-prepare--current-p job)
                             (or (plist-get result :cacheable)
                                 (= (mevedel-view-prepare--job-tick job)
                                    (with-current-buffer source (buffer-modified-tick)))))))
            (mevedel-view-prepare--retire job)
            (mevedel-view-prepare-resume)
            (if usable result
              (push key mevedel-view-prepare--failed)
              nil))
        (mevedel-view-prepare-resume)
        (mevedel-view-prepare--pending job)))))

(defun mevedel-view-prepare--redraw (job &optional invalid)
  "Refresh JOB with projection ownership held.
INVALID requires rebuilding historical context instead of reusing its turn."
  (let ((source (mevedel-view-prepare--job-source job))
        (mevedel-view-prepare-enabled t))
    (if (and (mevedel-view-stream-in-flight-turn-start-position)
             (markerp mevedel-view--data-turn-start)
             (eq (marker-buffer mevedel-view--data-turn-start) source)
             (<= mevedel-view--data-turn-start (mevedel-view-prepare--job-start job)))
        (progn
          (mevedel-view-render-invalidate-live-tail)
          (mevedel-view--render-live-region source nil))
      (unless (and (not invalid)
                   (mevedel-view-render--refresh-source
                    source (mevedel-view-prepare--job-start job)
                    (mevedel-view-prepare--job-end job)))
        (mevedel-view-render-batched-full)))))

(defun mevedel-view-prepare--advance (view)
  "Advance VIEW's first job and publish only a current, completed parse."
  (when (buffer-live-p view)
    (with-current-buffer view
      (when mevedel-view-prepare--timer (cancel-timer mevedel-view-prepare--timer))
      (setq mevedel-view-prepare--timer nil)
      (mevedel-view-render-mutate
       'full-batch
       (lambda ()
         (unwind-protect
             (when-let* ((job (car mevedel-view-prepare--jobs)))
               (cond
                ((or (mevedel-view--unattended-p)
                     (mevedel-transport-busy-p
                      (buffer-local-value 'default-directory
                                          (mevedel-view-prepare--job-source job))))
                 nil)
                ((not (mevedel-view-prepare--current-p job))
                 (mevedel-view-prepare--retire job)
                 (mevedel-view-prepare--redraw job t))
                ((null (mevedel-view-prepare--job-thread job))
                 (mevedel-view-prepare--start job))
                ((thread-live-p (mevedel-view-prepare--job-thread job))
                 (let ((gate (mevedel-view-prepare--job-gate job)))
                   (with-mutex (condition-mutex gate)
                     (condition-notify gate))))
                (t
                 (let* ((result (mevedel-view-prepare--job-result job))
                        (key (mevedel-view-prepare--job-key job))
                        (source (mevedel-view-prepare--job-source job))
                        (usable (and result
                                     (or (plist-get result :cacheable)
                                         (= (mevedel-view-prepare--job-tick job)
                                            (with-current-buffer source
                                              (buffer-modified-tick))))))
                        (mevedel-view-prepare-enabled t))
                   (unwind-protect
                       (when usable
                         (condition-case err
                             (setq usable
                                   (mevedel-view--segment-rendering
                                    source (mevedel-view-prepare--job-start job)
                                    (mevedel-view-prepare--job-end job) t))
                           (error
                            (setq usable nil)
                            (mevedel-view--debug-log 'tool-preparation-recovery :error err))))
                     (mevedel-view-prepare--retire job))
                   (unless (and usable (plist-get result :cacheable))
                     (push key mevedel-view-prepare--failed))
                   (when (> (length mevedel-view-prepare--failed) 64)
                     (setcdr (nthcdr 63 mevedel-view-prepare--failed) nil))
                   (mevedel-view-prepare--redraw job)))))
           (mevedel-view-prepare-resume)))))))

(provide 'mevedel-view-prepare)
;;; mevedel-view-prepare.el ends here
