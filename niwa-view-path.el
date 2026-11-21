;;; niwa-view-path.el --- Deferred view path verification -*- lexical-binding: t -*-

;;; Commentary:

;; Keeps rendered-path existence checks off an active execution target.
;; Remote paths remain undecorated until an idle verifier records them in
;; buffer-local memory and requests a canonical rerender.

;;; Code:

;; `niwa-file-state'
(declare-function niwa-file-cache-get "niwa-file-state" (cache path))
(autoload 'niwa-file-cache-get "niwa-file-state")

;; `niwa-structs'
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-file-cache "niwa-structs" (cl-x) t)
(defvar niwa--session)

;; `niwa-transport'
(declare-function niwa-transport-cancel-pending
                  "niwa-transport" (&optional key))
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk &optional on-cancel))
(autoload 'niwa-transport-cancel-pending "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-view'
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))

(defconst niwa-view--path-absence-ttl 30
  "Seconds an absent remote path stays memoized before re-verification.")

(defvar-local niwa-view--path-existence nil
  "Remote path existence memo: PATH -> (EXISTS-P . RECORDED-AT).")

(defvar-local niwa-view--path-pending nil
  "Remote paths left undecorated while awaiting verification.")

(defvar-local niwa-view--path-verify-timer nil
  "Idle timer coalescing this buffer's path verification pass.")

(defvar-local niwa-view--path-torn-down-p nil
  "Non-nil after this buffer's path verifier has been torn down.")

(defun niwa-view-path-teardown ()
  "Cancel this buffer's path verification work."
  (setq niwa-view--path-torn-down-p t)
  (when (timerp niwa-view--path-verify-timer)
    (cancel-timer niwa-view--path-verify-timer))
  (setq niwa-view--path-verify-timer nil)
  (niwa-transport-cancel-pending
   (list 'view-path-verify (current-buffer))))

(defun niwa-view--path-known-p (path)
  "Return non-nil when PATH is known to exist without target I/O."
  (or (when-let* ((entry (and niwa-view--path-existence
                              (gethash path niwa-view--path-existence))))
        (car entry))
      (when-let* ((session (bound-and-true-p niwa--session))
                  (workspace (ignore-errors
                               (niwa-session-workspace session)))
                  (cache (ignore-errors
                           (niwa-workspace-file-cache workspace))))
        (and (ignore-errors (niwa-file-cache-get cache path)) t))))

(defun niwa-view--path-memo-fresh-p (path)
  "Return non-nil when PATH's memo still answers."
  (when-let* ((entry (and niwa-view--path-existence
                          (gethash path niwa-view--path-existence))))
    (or (car entry)
        (< (- (float-time) (cdr entry)) niwa-view--path-absence-ttl))))

(defun niwa-view--note-unverified-path (path)
  "Queue PATH for the idle verification pass."
  (unless (niwa-view--path-memo-fresh-p path)
    (add-to-list 'niwa-view--path-pending path)
    (niwa-view--schedule-path-verification)))

(defun niwa-view--schedule-path-verification ()
  "Arm this buffer's idle path verification pass."
  (unless (or niwa-view--path-torn-down-p
              (timerp niwa-view--path-verify-timer))
    (setq niwa-view--path-verify-timer
          (run-with-idle-timer
           0.5 nil #'niwa-view--verify-paths (current-buffer)))))

(defconst niwa-view--path-probe-budget 2048
  "Largest quoted argument bytes one batched remote existence probe carries.

A TRAMP command line crosses a pty whose canonical line truncates above
4 KiB, so a batch stays well below it.")

(defun niwa-view--path-probe-batches (paths)
  "Split remote PATHS into (PREFIX . PATHS) batches for one command each."
  (let (batches)
    (dolist (path paths)
      (let* ((prefix (file-remote-p path))
             (quoted (+ 1 (string-bytes
                           (shell-quote-argument (file-local-name path)))))
             (batch (car batches)))
        (if (and batch (equal prefix (car batch))
                 (<= (+ (cadr batch) quoted) niwa-view--path-probe-budget))
            (setcar batches (list prefix (+ (cadr batch) quoted)
                                  (cons path (nth 2 batch))))
          (push (list prefix quoted (list path)) batches))))
    (mapcar (lambda (batch) (cons (car batch) (nreverse (nth 2 batch))))
            (nreverse batches))))

(defun niwa-view--probe-paths (prefix paths)
  "Return PATHS' existence on remote PREFIX from one command, or nil.
Nil means the command failed and the caller checks each path itself."
  (condition-case nil
      (with-temp-buffer
        (let ((default-directory (file-name-as-directory prefix)))
          (when (eql 0 (apply #'process-file "sh" nil t nil "-c"
                              "for p; do if test -e \"$p\"; then echo 1; else echo 0; fi; done"
                              "niwa-path-probe"
                              (mapcar #'file-local-name paths)))
            (let ((answers (split-string (buffer-string) "\n" t)))
              (when (and (= (length answers) (length paths))
                         (seq-every-p (lambda (answer) (member answer '("0" "1")))
                                      answers))
                (mapcar (lambda (answer) (equal answer "1")) answers))))))
    (error nil)))

(defun niwa-view--path-existences (paths)
  "Return PATHS' existence flags in order, batching remote checks."
  (let ((answers (make-hash-table :test #'equal)))
    (dolist (batch (niwa-view--path-probe-batches
                    (seq-filter
                     (lambda (path)
                       (and (file-remote-p path)
                            (not (string-match-p "[\n\r]" path))))
                     paths)))
      (seq-mapn (lambda (path exists) (puthash path exists answers))
               (cdr batch)
               (or (niwa-view--probe-paths (car batch) (cdr batch))
                   (make-list (length (cdr batch)) 'unknown))))
    (mapcar (lambda (path)
              (let ((exists (gethash path answers 'unknown)))
                (if (eq exists 'unknown)
                    (ignore-errors (and (file-exists-p path) t))
                  exists)))
            paths)))

(defun niwa-view--verify-paths (buffer)
  "Resolve BUFFER's pending paths once the transport is idle."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq niwa-view--path-verify-timer nil)
      (when niwa-view--path-pending
        ;; A remote capture spent 104 round trips here in 7.6 minutes, one
        ;; per path; paths on one target now share one command.
        (niwa-transport-run-when-idle
         (list 'view-path-verify buffer) (car niwa-view--path-pending)
         (lambda ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               ;; Drain when the coalesced worker starts so paths discovered
               ;; while it waited join this batch.
               (let ((pending (nreverse niwa-view--path-pending)))
                 (setq niwa-view--path-pending nil)
                 (seq-mapn
                  (lambda (path exists)
                    (unless niwa-view--path-existence
                      (setq niwa-view--path-existence
                            (make-hash-table :test #'equal)))
                    (puthash path (cons exists (float-time))
                             niwa-view--path-existence))
                  pending (niwa-view--path-existences pending))
                 (niwa-view-rerender buffer)))))
         (lambda ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (niwa-view--schedule-path-verification)))))))))

(provide 'niwa-view-path)
;;; niwa-view-path.el ends here
