;;; test-mevedel-execution-spool-copies.el -- Compressed spool lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:

;; Already-compressed execution output follows existing session ownership:
;; file-workspace Save As copies it, Fork excludes it, and cooperative control
;; transfer keeps it at the same path.  Exercise real compression and Read.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-execution-spool)
(require 'mevedel-session-fork)
(require 'mevedel-tool-fs-read)

(defun test-mevedel-execution-spool-copies--fixture (session)
  "Create compressed output in SESSION and return its physical gzip path.
Run the real worker under a ten-second deadline.  Retire raw fixture bytes
only after successful compression; production retirement is tested elsewhere."
  (let* ((path (file-name-concat (mevedel-session-save-path session)
                                "tool-results" "executions" "retained.log"))
         (gzip (concat path ".gz")))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (dotimes (_ 4096) (insert "retained execution line\n")))
    (with-temp-buffer
      (should (eq 0 (call-process "timeout" nil t nil "10" "python3"
                                  mevedel-execution-spool--worker
                                  "--compress" path))))
    (should (file-regular-p gzip))
    (should (< (file-attribute-size (file-attributes gzip))
               (file-attribute-size (file-attributes path))))
    (delete-file path)
    gzip))

(defun test-mevedel-execution-spool-copies--read (session)
  "Read SESSION's stable execution address through the public Read handler.
The shared Read helper waits at most ten seconds and cancels on unwind."
  (let* ((address "artifact://executions/retained.log")
         (attempt (mevedel-resource-prepare 'read address (list :session session)))
         (mevedel-resource-current-attempts (list (cons address attempt)))
         (mevedel--session session)
         (result (mevedel-test--read (list :file_path address :offset 1 :limit 2))))
    (should-not (eq 'error (plist-get result :status)))
    (should (string-match-p "retained execution line" (plist-get result :result)))
    (should-not (string-search (mevedel-session-save-path session)
                               (plist-get result :result)))
    (should-not (string-search ".log.gz" (plist-get result :result)))
    result))

(mevedel-deftest mevedel-execution-spool-copies/save-as
  (:quiet t
   :doc "file-workspace Save As independently copies compressed output at its stable address")
  (cl-destructuring-bind (workspace . root)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "parent" workspace))
           (buffer (generate-new-buffer " *spool-save-as*"))
           parent-path parent-gzip child-gzip digest)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "Parent transcript\n")
            (mevedel-session-artifacts-save session buffer)
            (setq parent-path (mevedel-session-save-path session)
                  parent-gzip (test-mevedel-execution-spool-copies--fixture session)
                  digest (secure-hash 'sha256
                                      (mevedel-session-artifacts-read-file-raw parent-gzip)))
            (test-mevedel-execution-spool-copies--read session)
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "child")))
              (mevedel-save-session t))
            (should-not (equal parent-path (mevedel-session-save-path session)))
            (setq child-gzip (file-name-concat (mevedel-session-save-path session)
                                               "tool-results" "executions" "retained.log.gz"))
            (should (file-regular-p parent-gzip))
            (should (file-regular-p child-gzip))
            (should-not (file-exists-p (string-remove-suffix ".gz" child-gzip)))
            (should (equal digest (secure-hash 'sha256
                                               (mevedel-session-artifacts-read-file-raw child-gzip))))
            (should-not (file-equal-p parent-gzip child-gzip))
            (test-mevedel-execution-spool-copies--read session)
            ;; Deleting the child's copy must not alter the parent's evidence.
            (delete-file child-gzip)
            (should (equal digest (secure-hash 'sha256
                                               (mevedel-session-artifacts-read-file-raw parent-gzip))))
            (let ((parent (mevedel-session--create :authority-mode 'pid-lock
                                                   :save-path parent-path)))
              (test-mevedel-execution-spool-copies--read parent)))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory root t)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-execution-spool-copies/fork
  (:doc "conversation Fork leaves compressed execution history with Source")
  (let ((fixture (test-mevedel-session-persistence--make-fork-ready))
        child-buffer)
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (gzip (test-mevedel-execution-spool-copies--fixture session))
               (digest (secure-hash 'sha256
                                    (mevedel-session-artifacts-read-file-raw gzip))))
          (cl-letf (((symbol-function 'mevedel--run-session-start-hooks) #'ignore)
                    ((symbol-function 'mevedel-model-apply-session-policy) #'ignore))
            (setq child-buffer
                  (mevedel-session-fork-create
                   (plist-get fixture :buffer)
                   '(:fork-point-id "fixture-fork") 'conversation)))
          (should (buffer-live-p child-buffer))
          (let* ((child (buffer-local-value 'mevedel--session child-buffer))
                 (child-log (file-name-concat (mevedel-session-save-path child)
                                              "tool-results" "executions" "retained.log")))
            (should-not (mevedel-session-execution-state child))
            (should-not (file-exists-p child-log))
            (should-not (file-exists-p (concat child-log ".gz")))
            (let* ((address "artifact://executions/retained.log")
                   (attempt (mevedel-resource-prepare 'read address (list :session child)))
                   (mevedel-resource-current-attempts (list (cons address attempt)))
                   (mevedel--session child))
              (should (eq 'error
                          (plist-get (mevedel-test--read
                                      (list :file_path address :limit 2))
                                     :status)))))
          (should (equal digest (secure-hash 'sha256
                                             (mevedel-session-artifacts-read-file-raw gzip))))
          (test-mevedel-execution-spool-copies--read session))
      (when (buffer-live-p child-buffer)
        (when-let* ((view (buffer-local-value 'mevedel--view-buffer child-buffer)))
          (when (buffer-live-p view) (kill-buffer view)))
        (when (buffer-live-p child-buffer)
          (with-current-buffer child-buffer (set-buffer-modified-p nil))
          (kill-buffer child-buffer)))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))

(mevedel-deftest mevedel-execution-spool-copies/control-transfer
  (:doc "cooperative lease handoff retains compressed bytes at the same session path")
  (let* ((root (make-temp-file "mevedel-spool-transfer-" t))
         (path (file-name-concat root "session"))
         (workspace (mevedel-workspace--create :type 'project :id root
                                              :root root :name "spool-transfer"))
         (owner (mevedel-session-create "owner" workspace))
         (requester (mevedel-session-create "requester" workspace))
         (owner-id (make-string 64 ?a))
         (requester-id (make-string 64 ?b)))
    (make-directory path t)
    (dolist (session (list owner requester))
      (setf (mevedel-session-save-path session) path
            (mevedel-session-session-id session) "spool-transfer"))
    (unwind-protect
        (let* ((gzip (test-mevedel-execution-spool-copies--fixture owner))
               (identity (file-attribute-inode-number (file-attributes gzip)))
               (digest (secure-hash 'sha256
                                    (mevedel-session-artifacts-read-file-raw gzip))))
          (let ((mevedel-session-durability--client-id owner-id))
            (should (mevedel-session-durability-lease-acquire path "owner" owner))
            (let ((mevedel-session-durability--client-id requester-id))
              (should (mevedel-session-transfer-request requester)))
            (should (eq 'requested (plist-get (mevedel-session-transfer-poll owner) :state)))
            (should (eq 'grant (plist-get (mevedel-session-transfer-decide owner 'grant)
                                         :decision)))
            (should (mevedel-session-transfer-release owner)))
          (let ((mevedel-session-durability--client-id requester-id))
            (should (mevedel-session-durability-lease-acquire path "requester" requester))
            (should (mevedel-session-durability-lease-owned-p requester))
            (should (equal path (mevedel-session-save-path requester)))
            (should (= identity (file-attribute-inode-number (file-attributes gzip))))
            (should (equal digest (secure-hash 'sha256
                                               (mevedel-session-artifacts-read-file-raw gzip))))
            (test-mevedel-execution-spool-copies--read requester)
            (should-not (file-exists-p (string-remove-suffix ".gz" gzip)))))
      (dolist (entry (list (cons owner owner-id) (cons requester requester-id)))
        (let ((mevedel-session-durability--client-id (cdr entry)))
          (mevedel-session-persistence-lock-release path (car entry))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-execution-spool-copies)
;;; test-mevedel-execution-spool-copies.el ends here
