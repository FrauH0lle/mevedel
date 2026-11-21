;;; niwa-view-prepare.el --- Deferred tool source preparation -*- lexical-binding: t -*-

;;; Commentary:
;; Own large tool parsing jobs for each view.  The parser runs sequentially in
;; one Lisp thread, waiting at its explicit checkpoints.  Only the main thread
;; consumes prepared data and invokes renderers.  Jobs retain source identities,
;; never view coordinates, and cannot publish after replacement or teardown.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)

;; `niwa-transport'
(declare-function niwa-transport-busy-p "niwa-transport" (&optional directory))

;; `niwa-view'
(declare-function niwa-view--unattended-p "niwa-view" (&optional buffer))

;; `niwa-view-render'
(declare-function niwa-view--debug-log "niwa-view-render" (event &rest fields))
(declare-function niwa-view--prepare-tool-segment "niwa-view-render" (data start end))
(declare-function niwa-view--render-live-region "niwa-view-render" (data settle &optional start end))
(declare-function niwa-view--segment-rendering "niwa-view-render" (data start end &optional collapsed))
(declare-function niwa-view--tool-cache-key "niwa-view-render" (data start end collapsed))
(declare-function niwa-view-render-batched-full "niwa-view-render" ())
(declare-function niwa-view-render--refresh-source "niwa-view-render" (data start end))
(declare-function niwa-view-render-mutate "niwa-view-render" (key function &optional replacement cleanup))
(defvar niwa-view--data-turn-start)
(defvar niwa-view-render--parse-checkpoint)

;; `niwa-view-segments'
(declare-function niwa-view-segments-display-buffer "niwa-view-segments" ())

;; `niwa-view-stream'
(declare-function niwa-view-stream-in-flight-turn-start-position "niwa-view-stream" ())

(defvar niwa-view-prepare-enabled nil
  "Non-nil while a scheduled projection may defer large collapsed tools.
Explicit synchronous projections and expansion leave this nil.")

(defvar niwa-view-prepare--min-size (* 256 1024)
  "Minimum source characters for staged tool preparation.")

(cl-defstruct (niwa-view-prepare--job
               (:constructor niwa-view-prepare--job-create))
  view source start end key tick thread result error kill-hook
  (gate (make-condition-variable (make-mutex) "niwa tool preparation")))

(defvar-local niwa-view-prepare--jobs nil
  "Pending jobs in admission order; only the first may have a live thread.")
(defvar-local niwa-view-prepare--timer nil
  "Main-thread callback which advances this view's preparation queue.")
(defvar-local niwa-view-prepare--failed nil
  "Source identities whose preparation needs the synchronous fallback.")

(defun niwa-view-prepare--retire (job)
  "Release JOB's source hook and stop any unfinished parsing."
  (let ((inhibit-quit t))
    (setq niwa-view-prepare--jobs (delq job niwa-view-prepare--jobs))
    (when (buffer-live-p (niwa-view-prepare--job-source job))
      (with-current-buffer (niwa-view-prepare--job-source job)
        (remove-hook 'kill-buffer-hook (niwa-view-prepare--job-kill-hook job) t)))
    (when-let* ((thread (niwa-view-prepare--job-thread job))
                ((thread-live-p thread)))
      (thread-signal thread 'quit nil))
    (setf (niwa-view-prepare--job-result job) nil)))

(defun niwa-view-prepare-cancel ()
  "Cancel this view's source jobs and release their source-buffer hooks."
  (when niwa-view-prepare--timer
    (cancel-timer niwa-view-prepare--timer))
  (setq niwa-view-prepare--timer nil)
  (dolist (job (copy-sequence niwa-view-prepare--jobs))
    (niwa-view-prepare--retire job))
  (setq niwa-view-prepare--failed nil))

(defun niwa-view-prepare-initialize ()
  "Initialize preparation ownership in the current view."
  (niwa-view-prepare-cancel)
  (add-hook 'change-major-mode-hook #'niwa-view-prepare-cancel nil t)
  (add-hook 'kill-buffer-hook #'niwa-view-prepare-cancel nil t))

(defun niwa-view-prepare--current-p (job)
  "Return non-nil when JOB still owns the same source and view lifetime."
  (let ((view (niwa-view-prepare--job-view job))
        (source (niwa-view-prepare--job-source job)))
    (and (buffer-live-p view) (buffer-live-p source)
         (with-current-buffer source
           (<= (point-min) (niwa-view-prepare--job-start job)
               (niwa-view-prepare--job-end job) (point-max)))
         (with-current-buffer view
           (and (memq job niwa-view-prepare--jobs)
                (eq source (niwa-view-segments-display-buffer))
                (let* ((old (niwa-view-prepare--job-key job))
                       (key (niwa-view--tool-cache-key
                             source (niwa-view-prepare--job-start job)
                             (niwa-view-prepare--job-end job) t)))
                  ;; Bounded rendering identities can be evicted while a job
                  ;; waits.  An unchanged buffer tick proves the same text;
                  ;; session context and provenance must still match exactly.
                  (when (and (= (niwa-view-prepare--job-tick job)
                                (with-current-buffer source (buffer-modified-tick)))
                             (equal (nthcdr 4 old) (nthcdr 4 key)))
                    (setf (niwa-view-prepare--job-key job) key))
                  (equal (niwa-view-prepare--job-key job) key)))))))

(defun niwa-view-prepare-resume ()
  "Schedule this view's next preparation callback once."
  (when (and niwa-view-prepare--jobs
             (not niwa-view-prepare--timer)
             (not (niwa-view--unattended-p)))
    (let ((delay
           (if (niwa-transport-busy-p
                (buffer-local-value 'default-directory
                                    (niwa-view-prepare--job-source
                                     (car niwa-view-prepare--jobs))))
               0.1 0.001)))
      (let ((inhibit-quit t))
        (setq niwa-view-prepare--timer
              (run-at-time delay nil #'niwa-view-prepare--advance
                           (current-buffer)))))))

(defun niwa-view-prepare--checkpoint (job)
  "Validate JOB and return interpreter time to the main thread."
  (unless (niwa-view-prepare--current-p job) (signal 'quit nil))
  ;; Unlike `sleep-for', this wait cannot dispatch editor timers or process
  ;; sentinels in the worker.  The main-thread advance callback admits each
  ;; stage only while the view is attended and its transport is idle.
  (let ((gate (niwa-view-prepare--job-gate job)))
    (with-mutex (condition-mutex gate)
      (condition-wait gate)))
  (unless (niwa-view-prepare--current-p job) (signal 'quit nil)))

(defun niwa-view-prepare--start (job)
  "Start JOB's parser without inheriting view mutation or renderer bindings."
  (let ((environment (copy-sequence process-environment)))
    (setf (niwa-view-prepare--job-thread job)
          (make-thread
           (lambda ()
             (condition-case err
                 (let ((process-environment environment)
                       (niwa-view-render--parse-checkpoint
                        (lambda () (niwa-view-prepare--checkpoint job))))
                   (niwa-view-prepare--checkpoint job)
                   (setf (niwa-view-prepare--job-result job)
                         (niwa-view--prepare-tool-segment
                          (niwa-view-prepare--job-source job)
                          (niwa-view-prepare--job-start job)
                          (niwa-view-prepare--job-end job))))
               ((error quit) (setf (niwa-view-prepare--job-error job) err)))
             nil)
           "niwa tool preparation"))))

(defun niwa-view-prepare--pending (job)
  "Return an honest, non-toggleable pending row for JOB."
  (let ((name "Tool"))
    (with-current-buffer (niwa-view-prepare--job-source job)
      (save-excursion
        (goto-char (niwa-view-prepare--job-start job))
        (when (re-search-forward
               "(\\s-*:name[ \t]+\"\\([^\"\n]+\\)\""
               (min (niwa-view-prepare--job-end job) (+ (point) 4096)) t)
          (setq name (match-string-no-properties 1)))))
    (list :pending (list :header (concat name ": preparing result…")
                         :vtype 'tool-preparing :status 'running
                         :group-tool name
                         :initially-collapsed-p t))))

(defun niwa-view-prepare-get (source start end key)
  "Return prepared data or a pending row for a large collapsed source span.
SOURCE, START, END and KEY identify the exact source generation.  Return nil
when ordinary synchronous preparation should handle it.  Call in the view."
  (when (and niwa-view-prepare-enabled key (featurep 'threads)
             (>= (- end start) niwa-view-prepare--min-size)
             (not (member key niwa-view-prepare--failed)))
    (setq start (nth 1 key) end (nth 2 key))
    (dolist (queued niwa-view-prepare--jobs)
      (when (and (eq source (niwa-view-prepare--job-source queued))
                 (= start (niwa-view-prepare--job-start queued))
                 (= end (niwa-view-prepare--job-end queued)))
        (niwa-view-prepare--current-p queued)))
    (let ((job (cl-find key niwa-view-prepare--jobs
                        :key #'niwa-view-prepare--job-key :test #'equal)))
      (unless job
        (let ((view (current-buffer)))
          (setq job (niwa-view-prepare--job-create
                     :view view :source source :start start :end end :key key
                     :tick (with-current-buffer source (buffer-modified-tick))
                     :kill-hook (lambda ()
                                  (when (buffer-live-p view)
                                    (with-current-buffer view
                                      (niwa-view-prepare-cancel))))))
          ;; Input may abandon the caller's pure projection step, but never
          ;; split ownership of a source hook from its discoverable queue entry.
          (let ((inhibit-quit t))
            (with-current-buffer source
              (add-hook 'kill-buffer-hook (niwa-view-prepare--job-kill-hook job) nil t))
            (setq niwa-view-prepare--jobs
                  (nconc niwa-view-prepare--jobs (list job))))))
      (if-let* ((thread (niwa-view-prepare--job-thread job))
                ((not (thread-live-p thread)))
                (result (niwa-view-prepare--job-result job)))
          (let ((usable (and (niwa-view-prepare--current-p job)
                             (or (plist-get result :cacheable)
                                 (= (niwa-view-prepare--job-tick job)
                                    (with-current-buffer source (buffer-modified-tick)))))))
            (niwa-view-prepare--retire job)
            (niwa-view-prepare-resume)
            (if usable result
              (push key niwa-view-prepare--failed)
              nil))
        (niwa-view-prepare-resume)
        (niwa-view-prepare--pending job)))))

(defun niwa-view-prepare--redraw (job &optional invalid)
  "Refresh JOB with projection ownership held.
INVALID requires rebuilding historical context instead of reusing its turn."
  (let ((source (niwa-view-prepare--job-source job))
        (niwa-view-prepare-enabled t))
    (if (and (niwa-view-stream-in-flight-turn-start-position)
             (markerp niwa-view--data-turn-start)
             (eq (marker-buffer niwa-view--data-turn-start) source)
             (<= niwa-view--data-turn-start (niwa-view-prepare--job-start job)))
        (niwa-view--render-live-region
         source nil (marker-position niwa-view--data-turn-start))
      (unless (and (not invalid)
                   (niwa-view-render--refresh-source
                    source (niwa-view-prepare--job-start job)
                    (niwa-view-prepare--job-end job)))
        (niwa-view-render-batched-full)))))

(defun niwa-view-prepare--advance (view)
  "Advance VIEW's first job and publish only a current, completed parse."
  (when (buffer-live-p view)
    (with-current-buffer view
      (when niwa-view-prepare--timer (cancel-timer niwa-view-prepare--timer))
      (setq niwa-view-prepare--timer nil)
      (niwa-view-render-mutate
       'full-batch
       (lambda ()
         (unwind-protect
             (when-let* ((job (car niwa-view-prepare--jobs)))
               (cond
                ((or (niwa-view--unattended-p)
                     (niwa-transport-busy-p
                      (buffer-local-value 'default-directory
                                          (niwa-view-prepare--job-source job))))
                 nil)
                ((not (niwa-view-prepare--current-p job))
                 (niwa-view-prepare--retire job)
                 (niwa-view-prepare--redraw job t))
                ((null (niwa-view-prepare--job-thread job))
                 (niwa-view-prepare--start job))
                ((thread-live-p (niwa-view-prepare--job-thread job))
                 (let ((gate (niwa-view-prepare--job-gate job)))
                   (with-mutex (condition-mutex gate)
                     (condition-notify gate))))
                (t
                 (let* ((result (niwa-view-prepare--job-result job))
                        (key (niwa-view-prepare--job-key job))
                        (source (niwa-view-prepare--job-source job))
                        (usable (and result
                                     (or (plist-get result :cacheable)
                                         (= (niwa-view-prepare--job-tick job)
                                            (with-current-buffer source
                                              (buffer-modified-tick))))))
                        (niwa-view-prepare-enabled t))
                   (unwind-protect
                       (when usable
                         (condition-case err
                             (setq usable
                                   (niwa-view--segment-rendering
                                    source (niwa-view-prepare--job-start job)
                                    (niwa-view-prepare--job-end job) t))
                           (error
                            (setq usable nil)
                            (niwa-view--debug-log 'tool-preparation-recovery :error err))))
                     (niwa-view-prepare--retire job))
                   (unless (and usable (plist-get result :cacheable))
                     (push key niwa-view-prepare--failed))
                   (when (> (length niwa-view-prepare--failed) 64)
                     (setcdr (nthcdr 63 niwa-view-prepare--failed) nil))
                   (niwa-view-prepare--redraw job)))))
           (niwa-view-prepare-resume)))))))

(provide 'niwa-view-prepare)
;;; niwa-view-prepare.el ends here
