;;; test-mevedel-memory-write.el -- Retained memory snapshots -*- lexical-binding: t -*-

;;; Commentary:

;; Check exact reverse changes over real created, deleted, and mode-changed files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-write)

(mevedel-deftest mevedel-memory-write--changes
    (:vars* ((root (make-temp-file "mevedel-memory-reverse-" t))
             (removed (file-name-concat root "removed.md"))
             (created (file-name-concat root "created.md"))
             (changed (file-name-concat root "changed.md")))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "restores deleted content, original modes and line endings, and expected absence"
  (progn
    (write-region "Deleted lesson.\n" nil removed nil 'silent)
    (write-region "Original\r\n" nil changed nil 'silent)
    (set-file-modes removed #o640)
    (set-file-modes changed #o640)
    (let* ((paths (list removed created changed))
           (before (mapcar (lambda (path) (cons (file-name-nondirectory path) (mevedel-memory-scope--snapshot path 32768))) paths)))
      (delete-file removed)
      (write-region "New lesson.\n" nil created nil 'silent)
      (write-region "Replaced.\n" nil changed nil 'silent)
      (set-file-modes changed #o600)
      (let* ((after (mapcar (lambda (path) (cons (file-name-nondirectory path) (mevedel-memory-scope--snapshot path 32768))) paths))
             (changes (mevedel-memory-write--changes after before)))
        (should (= 3 (length changes)))
        (mevedel-tool-patch-commit changes)
        (dolist (row before)
          (should (equal (cdr row) (mevedel-memory-scope--snapshot (plist-get (cdr row) :path) 32768))))
        (should-not (mevedel-memory-write--changes before before))))))

(mevedel-deftest mevedel-memory-write-list
    (:vars* ((root (make-temp-file "mevedel-memory-write-list-" t))
             (workspace (mevedel-workspace--create :root root))
             (id (make-string 64 ?a))
             (directory (mevedel-memory-write--directory workspace))
             (path (file-name-concat directory (concat id ".el"))))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "keeps unreadable private records inspectable without changing their bytes"
  (progn
    (make-directory directory t)
    (write-region "(broken state" nil path nil 'silent)
    (let ((rows (mevedel-memory-write-list workspace)))
      (should (= 1 (length rows)))
      (should (equal id (plist-get (car rows) :id)))
      (should (stringp (plist-get (car rows) :error)))
      (should-not (plist-get (car rows) :intent))
      (should-not (plist-get (car rows) :marked)))
    (should (equal "(broken state" (mevedel-session-control-fs-read-file path)))))

(mevedel-deftest mevedel-memory-write--mutate ()
  ,test
  (test)
  :doc "target fencing rejects stale writes and deletes on local and TRAMP roots"
  (let ((directory (make-temp-file "mevedel-memory-fence-" t)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("memory-fence")
          (dolist (prefix '("" "/mevedelmock:memory-fence:"))
            (let* ((root (file-name-as-directory (concat prefix directory)))
                   (claims (file-name-concat root (if (equal prefix "") "local" "remote")))
                   (path (file-name-concat root "topic.md"))
                   (claim (mevedel-journal-claim-acquire claims 180))
                   (before (list :path path :exists nil))
                   (after (list :path path :exists t :bytes "Owner content.\n" :mode #o640)))
              (mevedel-memory-write--mutate claim before after)
              (should (mevedel-tool-patch--same-snapshot-p
                       after (mevedel-tool-patch--snapshot path)))
              (should (mevedel-journal-claim--finish claim 'expired ""))
              (let* ((successor (mevedel-journal-claim-acquire claims 180))
                     (new (list :path path :exists t :bytes "Successor content.\n" :mode #o600)))
                (mevedel-memory-write--mutate successor after new)
                ;; Even a freshly matching file observation cannot authorize
                ;; an old mutation after takeover has settled its claim.
                (should-error (mevedel-memory-write--mutate claim new after))
                (should-error (mevedel-memory-write--mutate claim new before))
                (should (mevedel-tool-patch--same-snapshot-p
                         new (mevedel-tool-patch--snapshot path)))
                (mevedel-memory-write--mutate successor new before)
                (should-not (file-exists-p path))
                (mevedel-journal-claim-settle successor 'completed "")))))
      (delete-directory directory t)))
  :doc "an admitted paused mutation excludes takeover until its target program finishes"
  (let* ((directory (make-temp-file "mevedel-memory-paused-" t))
         (path (file-name-concat directory "topic.md"))
         (claims (file-name-concat directory "claims"))
         (claim (mevedel-journal-claim-acquire claims 180))
         (before (list :path path :exists nil))
         (after (list :path path :exists t :bytes "Committed before takeover.\n" :mode #o640))
         (pause (file-name-concat directory "paused"))
         (ready (file-name-concat directory "contender-ready"))
         (deadline (+ (float-time) 30)) processes buffers)
    (unwind-protect
        (cl-labels
            ((start (name form)
               (let ((runner (file-name-concat directory (concat name ".el")))
                     (output (generate-new-buffer " *memory-fence-client*")))
                 (push output buffers)
                 (with-temp-file runner
                   (insert ";;; -*- lexical-binding: t -*-\n")
                   (prin1 `(progn (setq load-path ',load-path)
                                  (require 'mevedel-memory-write) ,form)
                          (current-buffer)))
                 (push (make-process :name name :buffer output :noquery t :sentinel #'ignore
                                     :command (list (file-name-concat invocation-directory invocation-name)
                                                    "--batch" "-Q" "-l" runner)) processes)))
             (wait-for (file)
               (while (and (not (file-exists-p file)) (< (float-time) deadline))
                 (accept-process-output nil 0.01))
               (should (file-exists-p file))))
          (start "paused-writer"
                 `(let ((run (symbol-function 'mevedel-session-control-fs-run-program)))
                    (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                               (lambda (operations &optional lock)
                                 (let ((mevedel-session-control-fs--test-pause-file (and lock ,pause)))
                                   (funcall run operations lock)))))
                      (mevedel-memory-write--mutate ',claim ',before ',after))))
          (wait-for pause)
          (start "contender"
                 `(progn (write-region "ready" nil ,ready nil 'silent)
                         (unless (mevedel-journal-claim--finish ',claim 'expired "")
                           (error "Takeover did not settle"))))
          (wait-for ready)
          (accept-process-output nil 0.1)
          (should-not (file-exists-p (mevedel-journal-claim--path claim t)))
          (write-region "continue" nil (concat pause ".continue") nil 'silent)
          (while (and (seq-some #'process-live-p processes) (< (float-time) deadline))
            (accept-process-output nil 0.01))
          (dolist (process processes)
            (ert-info ((with-current-buffer (process-buffer process) (buffer-string)))
              (should-not (process-live-p process))
              (should (= 0 (process-exit-status process)))))
          (should (mevedel-tool-patch--same-snapshot-p after (mevedel-tool-patch--snapshot path)))
          (should (eq 'expired (plist-get (mevedel-journal-claim-outcome claim) :status)))
          (should-error (mevedel-memory-write--mutate claim after before)))
      (write-region "continue" nil (concat pause ".continue") nil 'silent)
      (dolist (process processes) (when (process-live-p process) (delete-process process)))
      (dolist (buffer buffers) (kill-buffer buffer))
      (delete-directory directory t))))

(mevedel-deftest mevedel-memory-write-scope
  (:doc "returns the captured scope of the intent's source pass")
  (progn
    (should (equal '(:workspace "w")
                   (mevedel-memory-write-scope
                    (list :accepted (list :prepared (list :scope '(:workspace "w")))))))
    (should-not (mevedel-memory-write-scope nil))))

(provide 'test-mevedel-memory-write)
;;; test-mevedel-memory-write.el ends here
