;;; mevedel-execution-spool.el -- Logical execution output storage -*- lexical-binding: t -*-

;;; Commentary:

;; Terminal local execution output is compressed by a separate process.  The
;; logical .log name does not change.  Authorized resource operations borrow
;; disposable ordinary-file views, so pagination and search need no gzip rules.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'mevedel-utilities)
(require 'mevedel-transport)

(defconst mevedel-execution-spool--worker
  (file-name-concat
   (mevedel-library-source-directory (or load-file-name buffer-file-name))
   "scripts" "mevedel-execution-spool.py")
  "Worker that performs compression and disposable materialization.")

(defvar mevedel-execution-spool--readers nil
  "Active logical file/subtree leases, as unique cells containing paths.")

(defvar mevedel-execution-spool--compressions (make-hash-table :test #'equal)
  "Raw paths awaiting compression completion or the last reader's release.")

(defvar mevedel-execution-spool--worker-timeout 120
  "Maximum worker runtime in seconds before its resources are released.")

(defun mevedel-execution-spool-logical-name (path)
  "Return PATH with an execution log's physical compression suffix removed."
  (if (string-suffix-p ".log.gz" path) (substring path 0 -3) path))

(defun mevedel-execution-spool-private-p (path)
  "Return non-nil when PATH names private spool work rather than an artifact."
  (cl-some (lambda (part)
             (or (equal part ".mevedel-pending-executions")
                 (string-prefix-p ".mevedel-spool-" part)))
           (file-name-split path)))

(defun mevedel-execution-spool-physical-path (path)
  "Return existing storage for logical PATH, preferring its raw bytes.
Compressed fallback never follows a symlink.  Resource callers validate the
containing authority before requesting this storage selection."
  (if (file-exists-p path) path
    (let ((compressed (concat path ".gz")))
      (if (and (string-suffix-p ".log" path)
               (file-regular-p compressed) (not (file-symlink-p compressed)))
          compressed path))))

(defun mevedel-execution-spool--identity (path)
  "Return PATH's non-symlink regular-file identity and modification facts."
  (when-let* ((attrs (file-attributes path 'integer))
              ((null (file-attribute-type attrs))))
    (list (file-attribute-device-number attrs)
          (file-attribute-inode-number attrs)
          (file-attribute-size attrs)
          (car (time-convert (file-attribute-modification-time attrs) 1000000000)))))

(defun mevedel-execution-spool--worker (arguments callback &optional remote-directory)
  "Run the spool worker with ARGUMENTS, then call CALLBACK with result facts.
Failure supplies nil.  No output payload enters the editor.
Return an idempotent canceller.  Both compression and inflation stay outside
the editor.  A two-minute deadline bounds abandoned worker resources."
  (let* ((default-directory (or remote-directory temporary-file-directory))
         (python (if remote-directory "python3" (executable-find "python3")))
         buffer process timer settled
         (stop (lambda ()
                 (when timer (cancel-timer timer))
                 (when (and (processp process) (process-live-p process))
                   (delete-process process)
                   ;; Observe exit before callers remove worker scratch paths.
                   (accept-process-output process 0.01)
                   (let ((deadline (+ (float-time) 1)))
                     (while (and (process-live-p process) (< (float-time) deadline))
                       (accept-process-output process 0.01)))))))
    (unless python (error "'python3' not installed"))
    (setq buffer (generate-new-buffer " *mevedel-spool-worker*"))
    (condition-case err
        (progn
          (setq process
                (make-process
                 :name "mevedel-spool" :buffer buffer :noquery t :connection-type 'pipe
                 :file-handler (and remote-directory t)
                 :command (append (if remote-directory
                                      (list python "-c"
                                            (with-temp-buffer
                                              (insert-file-contents mevedel-execution-spool--worker)
                                              (buffer-string)))
                                    (list python mevedel-execution-spool--worker))
                                  arguments)
                 :sentinel
                 (lambda (child _event)
                   (when (and (not settled) (memq (process-status child) '(exit signal)))
                     (setq settled t)
                     (when timer (cancel-timer timer))
                     (let ((facts
                            (and (eq (process-status child) 'exit)
                                 (zerop (process-exit-status child))
                                 (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (when (< (buffer-size) 4096)
                                     (goto-char (point-min))
                                     (condition-case nil
                                         (json-parse-buffer :object-type 'plist :array-type 'list)
                                       (error nil)))))))
                       (when (buffer-live-p buffer) (kill-buffer buffer))
                       (let ((deliver (lambda () (funcall callback facts))))
                         (unless (and remote-directory
                                      (mevedel-transport-busy-p remote-directory)
                                      (mevedel-transport-run-when-idle
                                       (list 'spool-worker child) remote-directory deliver))
                           (funcall deliver))))))))
          (unless (processp process) (error "Could not start spool worker"))
          (unless settled
            (setq timer (run-at-time mevedel-execution-spool--worker-timeout nil stop))))
      (error
       (setq settled t)
       (funcall stop)
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (signal (car err) (cdr err))))
    (lambda ()
      (unless settled
        (funcall stop)))))

(defun mevedel-execution-spool--finish-compression (path)
  "Retire unchanged raw PATH once its compressed copy and readers are settled."
  (let* ((evidence (gethash path mevedel-execution-spool--compressions))
         (source (and (consp evidence) (plist-get evidence :source)))
         (compressed (and (consp evidence) (plist-get evidence :gzip))))
    (when (and source compressed
               (not (cl-some
                     (lambda (lease)
                       (or (equal path (car lease))
                           (string-prefix-p (file-name-as-directory (car lease)) path)))
                     mevedel-execution-spool--readers)))
      (unwind-protect
          ;; Session-owned storage is trusted, not a hostile same-UID mutation
          ;; boundary: path comparison and unlink cannot be one atomic operation.
          (when (and (< (nth 2 compressed) (nth 2 source))
                     (equal compressed (mevedel-execution-spool--identity (concat path ".gz")))
                     (equal source (mevedel-execution-spool--identity path)))
            (delete-file path))
        (remhash path mevedel-execution-spool--compressions)))))

(defun mevedel-execution-spool-compress (path)
  "Asynchronously compress retired local spool PATH when it saves space.
Only the execution owner calls this, after its terminal polling window.
Never scan or rewrite historical session data.  Readers retain the raw inode
until they finish; errors leave the original evidence untouched."
  (when (and (stringp path) (not (file-remote-p path))
             (not (gethash path mevedel-execution-spool--compressions)))
    (when-let* ((identity (mevedel-execution-spool--identity path))
                ((<= (* 64 1024) (nth 2 identity) (* 64 1024 1024))))
      (let* ((temporary (make-temp-name
                         (file-name-concat (file-name-directory path) ".mevedel-spool-")))
             (cleanup (lambda () (ignore-errors (delete-file temporary)))))
        (puthash path 'running mevedel-execution-spool--compressions)
        (condition-case nil
            (mevedel-execution-spool--worker
             (list "--compress" path "--temporary" temporary)
             (lambda (facts)
               (unwind-protect
                   (if (and (equal (plist-get facts :status) "compressed")
                            (equal identity (plist-get facts :source_identity))
                            (let ((compressed (plist-get facts :gzip_identity)))
                              (and (listp compressed) (= (length compressed) 4)
                                   (cl-every #'integerp compressed))))
                       (progn
                         (puthash path (list :source identity :gzip (plist-get facts :gzip_identity))
                                  mevedel-execution-spool--compressions)
                         (mevedel-execution-spool--finish-compression path))
                     (remhash path mevedel-execution-spool--compressions))
                 (funcall cleanup))))
          (error
           (funcall cleanup)
           (remhash path mevedel-execution-spool--compressions)))))))

(defun mevedel-execution-spool--view-needed-p (path)
  "Return non-nil if logical PATH needs a normalized, inflated view."
  (or (string-suffix-p ".log.gz" path)
      (and (string-suffix-p ".log" path) (file-exists-p (concat path ".gz")))
      ;; Selection and traversal belong to the worker, not the editor thread.
      (file-directory-p path)))

(defun mevedel-execution-spool-with-path (path operation callback executor &optional max-lines)
  "Execute authorized OPERATION on logical PATH with CALLBACK and EXECUTOR.
EXECUTOR receives an ordinary path and a wrapped result callback.  Hold raw
reader leases through asynchronous delivery.  If needed, build a disposable
logical view off-thread, retaining it through success, failure or cancellation.
MAX-LINES bounds prefix inflation for a ranged Read.  Return a canceller."
  (let* ((lease (list (expand-file-name path)))
         (directory nil) (cancels nil) (released nil) (cancelled nil) (delivered nil)
         (release
          (lambda ()
            (unless released
              (setq released t)
              (setq mevedel-execution-spool--readers
                    (delq lease mevedel-execution-spool--readers))
              (when directory
                (let ((cleanup (lambda () (ignore-errors (delete-directory directory t)))))
                  (unless (and (mevedel-transport-busy-p directory)
                               (mevedel-transport-run-when-idle
                                (list 'spool-view directory) directory cleanup))
                    (funcall cleanup))))
              (dolist (raw (hash-table-keys mevedel-execution-spool--compressions))
                (mevedel-execution-spool--finish-compression raw)))))
         (deliver (lambda (result)
                    (unless (or cancelled delivered)
                      (setq delivered t)
                      (unwind-protect (funcall callback result) (funcall release)))))
         (run (lambda (selected)
                (unless cancelled
                  (condition-case nil
                      (push (funcall executor selected deliver) cancels)
                    (error
                     (funcall deliver (list :status 'error
                                            :result "Error: Could not read execution output"))))))))
    (push lease mevedel-execution-spool--readers)
    (condition-case err
        (if (not (mevedel-execution-spool--view-needed-p path))
            (funcall run path)
          (setq directory
                (if (file-remote-p path)
                    (let ((default-directory (file-name-directory path)))
                      (make-nearby-temp-file ".mevedel-spool-view-" t))
                  (make-temp-file "mevedel-spool-view-" t)))
          (let* ((is-directory (file-directory-p path))
                 (destination
                  (file-name-concat
                   directory (if is-directory "view"
                               (mevedel-execution-spool-logical-name
                                (file-name-nondirectory path))))))
            (push (mevedel-execution-spool--worker
                   (append (list "--view" (file-local-name path)
                                 (file-local-name destination) (symbol-name operation))
                           (when max-lines (list "--max-lines" (number-to-string max-lines))))
                   (lambda (success)
                     (unless cancelled
                       (if success (funcall run destination)
                         (funcall deliver (list :status 'error
                                                :result "Error: Could not decode retained execution output")))))
                   (and (file-remote-p path) (file-name-directory path)))
                  cancels)))
      (error
       (if delivered (signal (car err) (cdr err))
         (funcall deliver (list :status 'error
                                :result "Error: Could not prepare retained execution output")))))
    (lambda ()
      (unless cancelled
        (setq cancelled t)
        (unwind-protect
            (dolist (cancel cancels)
              (when (functionp cancel) (ignore-errors (funcall cancel))))
          (funcall release))))))

(provide 'mevedel-execution-spool)
;;; mevedel-execution-spool.el ends here
