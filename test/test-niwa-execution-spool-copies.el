;;; test-niwa-execution-spool-copies.el -- Compressed spool lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:

;; Already-compressed execution output follows existing session ownership:
;; file-workspace Save As copies it, Fork excludes it, and cooperative control
;; transfer keeps it at the same path.  Exercise real compression and Read.

;;; Code:

(require 'niwa-execution)
(require 'niwa-execution-process)
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-execution-spool)
(require 'niwa-session-fork)
(require 'niwa-tool-fs-read)

(defun test-niwa-execution-spool-copies--fixture (session)
  "Create compressed output in SESSION and return its physical gzip path.
Run the real worker under a ten-second deadline.  Retire raw fixture bytes
only after successful compression; production retirement is tested elsewhere."
  (let* ((path (file-name-concat (niwa-session-save-path session)
                                "tool-results" "executions" "retained.log"))
         (gzip (concat path ".gz")))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (dotimes (_ 4096) (insert "retained execution line\n")))
    (with-temp-buffer
      (should (eq 0 (call-process "timeout" nil t nil "10" "python3"
                                  niwa-execution-spool--worker
                                  "--compress" path))))
    (should (file-regular-p gzip))
    (should (< (file-attribute-size (file-attributes gzip))
               (file-attribute-size (file-attributes path))))
    (delete-file path)
    gzip))

(defun test-niwa-execution-spool-copies--read (session)
  "Read SESSION's stable execution address through the public Read handler.
The shared Read helper waits at most ten seconds and cancels on unwind."
  (let* ((address "artifact://executions/retained.log")
         (attempt (niwa-resource-prepare 'read address (list :session session)))
         (niwa-resource-current-attempts (list (cons address attempt)))
         (niwa--session session)
         (result (niwa-test--read (list :file_path address :offset 1 :limit 2))))
    (should-not (eq 'error (plist-get result :status)))
    (should (string-match-p "retained execution line" (plist-get result :result)))
    (should-not (string-search (niwa-session-save-path session)
                               (plist-get result :result)))
    (should-not (string-search ".log.gz" (plist-get result :result)))
    result))

(niwa-deftest niwa-execution-spool-copies/save-as
  (:quiet t
   :doc "file-workspace Save As independently copies compressed output at its stable address")
  (cl-destructuring-bind (workspace . root)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "parent" workspace))
           (buffer (generate-new-buffer " *spool-save-as*"))
           parent-path parent-gzip child-gzip digest)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session)
            (insert "Parent transcript\n")
            (niwa-session-artifacts-save session buffer)
            (setq parent-path (niwa-session-save-path session)
                  parent-gzip (test-niwa-execution-spool-copies--fixture session)
                  digest (secure-hash 'sha256
                                      (niwa-session-artifacts-read-file-raw parent-gzip)))
            (test-niwa-execution-spool-copies--read session)
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "child")))
              (niwa-save-session t))
            (should-not (equal parent-path (niwa-session-save-path session)))
            (setq child-gzip (file-name-concat (niwa-session-save-path session)
                                               "tool-results" "executions" "retained.log.gz"))
            (should (file-regular-p parent-gzip))
            (should (file-regular-p child-gzip))
            (should-not (file-exists-p (string-remove-suffix ".gz" child-gzip)))
            (should (equal digest (secure-hash 'sha256
                                               (niwa-session-artifacts-read-file-raw child-gzip))))
            (should-not (file-equal-p parent-gzip child-gzip))
            (test-niwa-execution-spool-copies--read session)
            ;; Deleting the child's copy must not alter the parent's evidence.
            (delete-file child-gzip)
            (should (equal digest (secure-hash 'sha256
                                               (niwa-session-artifacts-read-file-raw parent-gzip))))
            (let ((parent (niwa-session--create :authority-mode 'portable
                                                   :save-path parent-path)))
              (test-niwa-execution-spool-copies--read parent)))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (delete-directory root t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-execution-session-live-spools
  (:doc "names only unfinished executions' spools, as absolute paths")
  (let* ((root (file-name-as-directory (make-temp-file "niwa-live-spools-" t)))
         (records (make-hash-table :test #'equal))
         (session (niwa-session--create
                   :execution-state (niwa-execution--state-create :records records)))
         (record
          (lambda (id &rest slots)
            (puthash id
                     (apply #'niwa-execution--record-create
                            :execution-id id
                            :child (niwa-execution-process--child-create
                                    :spool-path (concat root "x/../" id ".log"))
                            slots)
                     records))))
    (unwind-protect
        (progn
          (funcall record "running")
          (funcall record "stopping" :stop-p t)
          (funcall record "finished" :finished-p t)
          (funcall record "undelivered" :finished-p t :delivery-pending-p t)
          (puthash "childless" (niwa-execution--record-create
                                :execution-id "childless")
                   records)
          (should (equal (list (concat root "running.log")
                               (concat root "stopping.log"))
                         (sort (niwa-execution-session-live-spools session)
                               #'string<)))
          (should-not (niwa-execution-session-live-spools
                       (niwa-session--create))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-copies/portable-save-as
  (:quiet t
   :doc "portable Save As copies compressed output into the child session")
  (let* ((root (file-name-as-directory (make-temp-file "niwa-spool-portable-" t)))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (session (niwa-session-create "parent" workspace))
         (buffer (generate-new-buffer " *spool-portable-save-as*"))
         parent-gzip child-gzip)
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (setq-local niwa--session session)
          (insert "Parent transcript\n")
          (niwa-session-artifacts-save session buffer)
          (setq parent-gzip (test-niwa-execution-spool-copies--fixture session))
          ;; Foreground output before its first yield, and a spool a live
          ;; execution still writes, stay with the parent.
          (let ((spools (file-name-directory parent-gzip)))
            (make-directory (file-name-concat spools ".niwa-pending-executions") t)
            (write-region "pending\n" nil
                          (file-name-concat spools ".niwa-pending-executions"
                                            "execution-a.log")
                          nil 'silent)
            (write-region "live\n" nil (file-name-concat spools "live.log") nil 'silent)
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "child"))
                      ((symbol-function 'niwa-execution-session-live-spools)
                       (lambda (_) (list (expand-file-name "live.log" spools)))))
              (niwa-save-session t)))
          (let ((child-spools (file-name-concat (niwa-session-save-path session)
                                                "tool-results" "executions")))
            (should-not (file-exists-p (file-name-concat
                                        child-spools ".niwa-pending-executions")))
            (should-not (file-exists-p (file-name-concat child-spools "live.log"))))
          (setq child-gzip (file-name-concat (niwa-session-save-path session)
                                             "tool-results" "executions" "retained.log.gz"))
          (should (file-regular-p parent-gzip))
          (should (file-regular-p child-gzip))
          (should (equal (secure-hash 'sha256 (niwa-session-artifacts-read-file-raw parent-gzip))
                         (secure-hash 'sha256 (niwa-session-artifacts-read-file-raw child-gzip))))
          (test-niwa-execution-spool-copies--read session))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-execution-spool-copies/fork
  (:doc "conversation Fork leaves compressed execution history with Source")
  (let ((fixture (test-niwa-session-persistence--make-fork-ready))
        child-buffer)
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (gzip (test-niwa-execution-spool-copies--fixture session))
               (digest (secure-hash 'sha256
                                    (niwa-session-artifacts-read-file-raw gzip))))
          (cl-letf (((symbol-function 'niwa--run-session-start-hooks) #'ignore)
                    ((symbol-function 'niwa-model-apply-session-policy) #'ignore))
            (setq child-buffer
                  (niwa-session-fork-create
                   (plist-get fixture :buffer)
                   '(:fork-point-id "fixture-fork") 'conversation)))
          (should (buffer-live-p child-buffer))
          (let* ((child (buffer-local-value 'niwa--session child-buffer))
                 (child-log (file-name-concat (niwa-session-save-path child)
                                              "tool-results" "executions" "retained.log")))
            (should-not (niwa-session-execution-state child))
            (should-not (file-exists-p child-log))
            (should-not (file-exists-p (concat child-log ".gz")))
            (let* ((address "artifact://executions/retained.log")
                   (attempt (niwa-resource-prepare 'read address (list :session child)))
                   (niwa-resource-current-attempts (list (cons address attempt)))
                   (niwa--session child))
              (should (eq 'error
                          (plist-get (niwa-test--read
                                      (list :file_path address :limit 2))
                                     :status)))))
          (should (equal digest (secure-hash 'sha256
                                             (niwa-session-artifacts-read-file-raw gzip))))
          (test-niwa-execution-spool-copies--read session))
      (when (buffer-live-p child-buffer)
        (when-let* ((view (buffer-local-value 'niwa--view-buffer child-buffer)))
          (when (buffer-live-p view) (kill-buffer view)))
        (when (buffer-live-p child-buffer)
          (with-current-buffer child-buffer (set-buffer-modified-p nil))
          (kill-buffer child-buffer)))
      (test-niwa-session-persistence--cleanup-fork-fixture fixture))))

(niwa-deftest niwa-execution-spool-copies/control-transfer
  (:doc "cooperative lease handoff retains compressed bytes at the same session path")
  (let* ((root (make-temp-file "niwa-spool-transfer-" t))
         (path (file-name-concat root "session"))
         (workspace (niwa-workspace--create :type 'project :id root
                                              :root root :name "spool-transfer"))
         (owner (niwa-session-create "owner" workspace))
         (requester (niwa-session-create "requester" workspace))
         (owner-id (make-string 64 ?a))
         (requester-id (make-string 64 ?b)))
    (make-directory path t)
    (dolist (session (list owner requester))
      (setf (niwa-session-save-path session) path
            (niwa-session-session-id session) "spool-transfer"))
    (unwind-protect
        (let* ((gzip (test-niwa-execution-spool-copies--fixture owner))
               (identity (file-attribute-inode-number (file-attributes gzip)))
               (digest (secure-hash 'sha256
                                    (niwa-session-artifacts-read-file-raw gzip))))
          (let ((niwa-session-durability--client-id owner-id))
            (should (niwa-session-durability-lease-acquire path "owner" owner))
            (let ((niwa-session-durability--client-id requester-id))
              (should (niwa-session-transfer-request requester)))
            (should (eq 'requested (plist-get (niwa-session-transfer-poll owner) :state)))
            (should (eq 'grant (plist-get (niwa-session-transfer-decide owner 'grant)
                                         :decision)))
            (should (niwa-session-transfer-release owner)))
          (let ((niwa-session-durability--client-id requester-id))
            (should (niwa-session-durability-lease-acquire path "requester" requester))
            (should (niwa-session-durability-lease-owned-p requester))
            (should (equal path (niwa-session-save-path requester)))
            (should (= identity (file-attribute-inode-number (file-attributes gzip))))
            (should (equal digest (secure-hash 'sha256
                                               (niwa-session-artifacts-read-file-raw gzip))))
            (test-niwa-execution-spool-copies--read requester)
            (should-not (file-exists-p (string-remove-suffix ".gz" gzip)))))
      (dolist (entry (list (cons owner owner-id) (cons requester requester-id)))
        (let ((niwa-session-durability--client-id (cdr entry)))
          (niwa-session-persistence-lock-release path (car entry))))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-execution-spool-copies)
;;; test-niwa-execution-spool-copies.el ends here
