;;; mevedel-view-path.el --- Deferred view path verification -*- lexical-binding: t -*-

;;; Commentary:

;; Keeps rendered-path existence checks off an active execution target.
;; Remote paths remain undecorated until an idle verifier records them in
;; buffer-local memory and requests a canonical rerender.

;;; Code:

;; `mevedel-file-state'
(declare-function mevedel-file-cache-get "mevedel-file-state" (cache path))
(autoload 'mevedel-file-cache-get "mevedel-file-state")

;; `mevedel-structs'
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-file-cache "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-transport'
(declare-function mevedel-transport-cancel-pending
                  "mevedel-transport" (&optional key))
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport" (key path thunk &optional on-cancel))
(autoload 'mevedel-transport-cancel-pending "mevedel-transport")
(autoload 'mevedel-transport-run-when-idle "mevedel-transport")

;; `mevedel-view'
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))

(defconst mevedel-view--path-absence-ttl 30
  "Seconds an absent remote path stays memoized before re-verification.")

(defvar-local mevedel-view--path-existence nil
  "Remote path existence memo: PATH -> (EXISTS-P . RECORDED-AT).")

(defvar-local mevedel-view--path-pending nil
  "Remote paths left undecorated while awaiting verification.")

(defvar-local mevedel-view--path-verify-timer nil
  "Idle timer coalescing this buffer's path verification pass.")

(defvar-local mevedel-view--path-torn-down-p nil
  "Non-nil after this buffer's path verifier has been torn down.")

(defun mevedel-view-path-teardown ()
  "Cancel this buffer's path verification work."
  (setq mevedel-view--path-torn-down-p t)
  (when (timerp mevedel-view--path-verify-timer)
    (cancel-timer mevedel-view--path-verify-timer))
  (setq mevedel-view--path-verify-timer nil)
  (mevedel-transport-cancel-pending
   (list 'view-path-verify (current-buffer))))

(defun mevedel-view--path-known-p (path)
  "Return non-nil when PATH is known to exist without target I/O."
  (or (when-let* ((entry (and mevedel-view--path-existence
                              (gethash path mevedel-view--path-existence))))
        (car entry))
      (when-let* ((session (bound-and-true-p mevedel--session))
                  (workspace (ignore-errors
                               (mevedel-session-workspace session)))
                  (cache (ignore-errors
                           (mevedel-workspace-file-cache workspace))))
        (and (ignore-errors (mevedel-file-cache-get cache path)) t))))

(defun mevedel-view--path-memo-fresh-p (path)
  "Return non-nil when PATH's memo still answers."
  (when-let* ((entry (and mevedel-view--path-existence
                          (gethash path mevedel-view--path-existence))))
    (or (car entry)
        (< (- (float-time) (cdr entry)) mevedel-view--path-absence-ttl))))

(defun mevedel-view--note-unverified-path (path)
  "Queue PATH for the idle verification pass."
  (unless (mevedel-view--path-memo-fresh-p path)
    (add-to-list 'mevedel-view--path-pending path)
    (mevedel-view--schedule-path-verification)))

(defun mevedel-view--schedule-path-verification ()
  "Arm this buffer's idle path verification pass."
  (unless (or mevedel-view--path-torn-down-p
              (timerp mevedel-view--path-verify-timer))
    (setq mevedel-view--path-verify-timer
          (run-with-idle-timer
           0.5 nil #'mevedel-view--verify-paths (current-buffer)))))

(defconst mevedel-view--path-probe-budget 2048
  "Largest quoted argument bytes one batched remote existence probe carries.

A TRAMP command line crosses a pty whose canonical line truncates above
4 KiB, so a batch stays well below it.")

(defun mevedel-view--path-probe-batches (paths)
  "Split remote PATHS into (PREFIX . PATHS) batches for one command each."
  (let (batches)
    (dolist (path paths)
      (let* ((prefix (file-remote-p path))
             (quoted (+ 1 (string-bytes
                           (shell-quote-argument (file-local-name path)))))
             (batch (car batches)))
        (if (and batch (equal prefix (car batch))
                 (<= (+ (cadr batch) quoted) mevedel-view--path-probe-budget))
            (setcar batches (list prefix (+ (cadr batch) quoted)
                                  (cons path (nth 2 batch))))
          (push (list prefix quoted (list path)) batches))))
    (mapcar (lambda (batch) (cons (car batch) (nreverse (nth 2 batch))))
            (nreverse batches))))

(defun mevedel-view--probe-paths (prefix paths)
  "Return PATHS' existence on remote PREFIX from one command, or nil.
Nil means the command failed and the caller checks each path itself."
  (condition-case nil
      (with-temp-buffer
        (let ((default-directory (file-name-as-directory prefix)))
          (when (eql 0 (apply #'process-file "sh" nil t nil "-c"
                              "for p; do if test -e \"$p\"; then echo 1; else echo 0; fi; done"
                              "mevedel-path-probe"
                              (mapcar #'file-local-name paths)))
            (let ((answers (split-string (buffer-string) "\n" t)))
              (when (and (= (length answers) (length paths))
                         (seq-every-p (lambda (answer) (member answer '("0" "1")))
                                      answers))
                (mapcar (lambda (answer) (equal answer "1")) answers))))))
    (error nil)))

(defun mevedel-view--path-existences (paths)
  "Return PATHS' existence flags in order, batching remote checks."
  (let ((answers (make-hash-table :test #'equal)))
    (dolist (batch (mevedel-view--path-probe-batches
                    (seq-filter
                     (lambda (path)
                       (and (file-remote-p path)
                            (not (string-match-p "[\n\r]" path))))
                     paths)))
      (seq-mapn (lambda (path exists) (puthash path exists answers))
               (cdr batch)
               (or (mevedel-view--probe-paths (car batch) (cdr batch))
                   (make-list (length (cdr batch)) 'unknown))))
    (mapcar (lambda (path)
              (let ((exists (gethash path answers 'unknown)))
                (if (eq exists 'unknown)
                    (ignore-errors (and (file-exists-p path) t))
                  exists)))
            paths)))

(defun mevedel-view--verify-paths (buffer)
  "Resolve BUFFER's pending paths once the transport is idle."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq mevedel-view--path-verify-timer nil)
      (when mevedel-view--path-pending
        ;; A remote capture spent 104 round trips here in 7.6 minutes, one
        ;; per path; paths on one target now share one command.
        (mevedel-transport-run-when-idle
         (list 'view-path-verify buffer) (car mevedel-view--path-pending)
         (lambda ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               ;; Drain when the coalesced worker starts so paths discovered
               ;; while it waited join this batch.
               (let ((pending (nreverse mevedel-view--path-pending)))
                 (setq mevedel-view--path-pending nil)
                 (seq-mapn
                  (lambda (path exists)
                    (unless mevedel-view--path-existence
                      (setq mevedel-view--path-existence
                            (make-hash-table :test #'equal)))
                    (puthash path (cons exists (float-time))
                             mevedel-view--path-existence))
                  pending (mevedel-view--path-existences pending))
                 (mevedel-view-rerender buffer)))))
         (lambda ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (mevedel-view--schedule-path-verification)))))))))

(provide 'mevedel-view-path)
;;; mevedel-view-path.el ends here
