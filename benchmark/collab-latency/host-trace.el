;;; host-trace.el --- Time collaboration stages in a running host -*- lexical-binding: t; -*-

;;; Commentary:

;; Temporary instrumentation for a live mevedel host.  Load it into the
;; daemon (emacsclient --eval '(load "host-trace.el")'), drive traffic
;; with loadbot.mjs, then read `(host-trace-report)' and remove the
;; advice with `(host-trace-stop)'.
;;
;; Each traced function records its start time, inclusive duration and
;; GC time spent inside it.  Durations nest: a parent includes its
;; children.  `host-trace-timeline' shows the raw spans of a window, so
;; gaps between spans (timers, the Node helper, the network) stay
;; visible.

;;; Code:

(require 'cl-lib)

(defvar host-trace-functions
  '(;; inbound
    websocket-outer-filter
    mevedel-collaboration--transport-receive
    mevedel-collaboration--unseal
    mevedel-collaboration--frame-decode
    mevedel-collaboration--dispatch-frame
    mevedel-collaboration-editing-handle
    mevedel-collaboration-editing--dispatch
    mevedel-collaboration-editing--presence
    mevedel-collaboration--handle-prompt
    ;; editing queue
    mevedel-shared-editing-call
    mevedel-shared-editing--drain
    mevedel-shared-editing--read
    mevedel-shared-editing--parse
    mevedel-shared-editing--json
    mevedel-shared-editing--send
    mevedel-shared-editing--accept
    mevedel-shared-editing--commit
    mevedel-shared-editing-commit-file
    mevedel-artifact-lease-ensure
    mevedel-artifact-lease-write
    mevedel-shared-editing--finish
    mevedel-collaboration-editing--changed
    mevedel-collaboration-editing--send
    ;; transcript publication
    mevedel-collaboration--publish
    mevedel-collaboration--publish-timer
    mevedel-collaboration--project-records
    mevedel-collaboration--canonical-records
    mevedel-collaboration--reuse-record-ids
    mevedel-collaboration--publish-queue
    mevedel-collaboration--publish-status
    mevedel-collaboration--publish-history
    mevedel-collaboration--publish-agents
    mevedel-collaboration--publish-tasks
    ;; outbound
    mevedel-collaboration--transport-send
    mevedel-collaboration--seal
    json-encode
    websocket-send)
  "Functions `host-trace-start' advises.")

(defvar host-trace--spans nil
  "Recorded spans, newest first: (NAME START DURATION GC DETAIL).")
(defvar host-trace--depth 0)
(defvar host-trace--limit 200000)
(defvar host-trace--count 0)

(defun host-trace--detail (name args)
  "Return a short label for traced NAME called with ARGS."
  (pcase name
    ('mevedel-collaboration--dispatch-frame (plist-get (nth 2 args) :t))
    ('mevedel-collaboration-editing-handle (plist-get (nth 2 args) :t))
    ('mevedel-collaboration-editing--dispatch (plist-get (nth 4 args) :action))
    ('mevedel-collaboration--transport-send (plist-get (nth 2 args) :t))
    ('mevedel-shared-editing-call (plist-get (nth 1 args) :action))
    ('mevedel-session-control-fs-run-program
     (format "%s<-%s" (mapcar (lambda (op) (plist-get op :op)) (car args))
             (host-trace--callers)))
    ((or 'mevedel-collaboration--seal 'json-encode)
     (and (stringp (car (last args))) (string-bytes (car (last args)))))))

(defun host-trace--callers ()
  "Return the nearest mevedel callers outside the control-fs module."
  (let (callers)
    (cl-loop for i from 4 to 60
             for frame = (backtrace-frame i)
             while (and frame (< (length callers) 3))
             do (let ((fn (cadr frame)))
                  (when (and (symbolp fn)
                             (string-prefix-p "mevedel-" (symbol-name fn))
                             (not (string-prefix-p "mevedel-session-control-fs" (symbol-name fn)))
                             (not (memq fn callers)))
                    (push fn callers))))
    (mapconcat #'symbol-name (nreverse callers) "<")))

(defun host-trace--around (name)
  "Return :around advice recording spans of NAME."
  (lambda (fn &rest args)
    (if (>= host-trace--count host-trace--limit)
        (apply fn args)
      (let ((start (float-time)) (gc gc-elapsed)
            (host-trace--depth (1+ host-trace--depth)))
        (unwind-protect (apply fn args)
          (cl-incf host-trace--count)
          (push (list name start (- (float-time) start) (- gc-elapsed gc)
                      (ignore-errors (host-trace--detail name args)))
                host-trace--spans))))))

(defvar host-trace--advice nil)

(defun host-trace-start ()
  "Advise `host-trace-functions' and clear previous spans."
  (host-trace-stop)
  (setq host-trace--spans nil host-trace--count 0)
  (dolist (name host-trace-functions)
    (when (fboundp name)
      (let ((advice (host-trace--around name)))
        (advice-add name :around advice)
        (push (cons name advice) host-trace--advice))))
  (length host-trace--advice))

(defun host-trace-stop ()
  "Remove all advice added by `host-trace-start'."
  (dolist (entry host-trace--advice)
    (advice-remove (car entry) (cdr entry)))
  (setq host-trace--advice nil))

(defun host-trace-reset ()
  "Forget recorded spans."
  (setq host-trace--spans nil host-trace--count 0))

(defun host-trace--quantile (sorted q)
  (nth (min (1- (length sorted)) (floor (* q (length sorted)))) sorted))

(defun host-trace-report ()
  "Return per-function and per-detail statistics in milliseconds."
  (let ((groups (make-hash-table :test #'equal)) rows)
    (dolist (span host-trace--spans)
      (let ((key (if (memq (car span) '(mevedel-collaboration--transport-send
                                        mevedel-collaboration--dispatch-frame
                                        mevedel-collaboration-editing--dispatch
                                        mevedel-shared-editing-call))
                     (format "%s[%s]" (car span) (nth 4 span))
                   (symbol-name (car span)))))
        (push span (gethash key groups))))
    (maphash
     (lambda (key spans)
       (let ((ms (sort (mapcar (lambda (s) (* 1000 (nth 2 s))) spans) #'<))
             (gc (* 1000 (apply #'+ (mapcar (lambda (s) (nth 3 s)) spans)))))
         (push (list key :n (length ms)
                     :p50 (/ (round (* 10 (host-trace--quantile ms 0.5))) 10.0)
                     :p95 (/ (round (* 10 (host-trace--quantile ms 0.95))) 10.0)
                     :max (/ (round (* 10 (car (last ms)))) 10.0)
                     :total (round (apply #'+ ms))
                     :gc (round gc))
               rows)))
     groups)
    (sort rows (lambda (a b) (> (plist-get (cdr a) :total) (plist-get (cdr b) :total))))))

(defun host-trace-timeline (&optional from-end limit)
  "Return the oldest LIMIT spans after skipping FROM-END newest, as text.
Offsets are milliseconds from the first span shown."
  (let* ((spans (sort (seq-take (nthcdr (or from-end 0) host-trace--spans)
                                (or limit 80))
                      (lambda (a b) (< (nth 1 a) (nth 1 b)))))
         (t0 (nth 1 (car spans))))
    (mapconcat (lambda (s)
                 (format "%8.1f %7.2f %s %s" (* 1000 (- (nth 1 s) t0))
                         (* 1000 (nth 2 s)) (car s) (or (nth 4 s) "")))
               spans "\n")))

(provide 'host-trace)
;;; host-trace.el ends here
