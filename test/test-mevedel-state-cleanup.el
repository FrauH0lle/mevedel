;;; test-mevedel-state-cleanup.el --- Artifact retention tests -*- lexical-binding: t -*-

;;; Commentary:
;; Real files exercise reference checks and conservative deletion.

;;; Code:

(require 'mevedel-state-cleanup)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-state-cleanup-retention (:quiet t)
  (let* ((root (make-temp-file "mevedel-state-cleanup-" t))
         (workspace (mevedel-workspace--create :root root))
         (data (file-name-concat root ".mevedel"))
         (media (file-name-concat data "state/media"))
         (reviews (file-name-concat data "state/review-packages"))
         (buffer (generate-new-buffer " *cleanup-live*"))
         (old (seconds-to-time (- (float-time) (* 8 86400))))
         paths)
    (unwind-protect
        (progn
          (make-directory media t)
          (make-directory reviews t)
          (dolist (name '("clipboard-20260101-000000.png"
                          "clipboard-20260101-000001.png"
                          "clipboard-20260101-000002.png"
                          "clipboard-20260101-000003.png"
                          "guest-20260101-000004-1.png"
                          "clipboard-20260101-000005.png"
                          "user.png" "review-orphan.md" "review-kept.md"))
            (let ((path (file-name-concat
                         (if (string-prefix-p "review-" name) reviews media) name)))
              (write-region "payload" nil path nil 'silent)
              (set-file-times path old)
              (push path paths)))
          (setq paths (nreverse paths))
          (set-file-times (nth 5 paths)) ; A fresh image needs no reference.
          (make-directory (file-name-concat data "sessions/test/.publications/old") t)
          (write-region (file-name-nondirectory (nth 1 paths)) nil
                        (file-name-concat data "sessions/test/.publications/old/000001.data")
                        nil 'silent)
          (write-region (file-name-nondirectory (nth 2 paths)) nil
                        (file-name-concat data "input-history.el") nil 'silent)
          (with-current-buffer buffer
            (insert (file-name-nondirectory (nth 3 paths)))
            (narrow-to-region (point-max) (point-max))
            (setq-local gptel-context (list (list (nth 4 paths)))))
          (write-region (file-name-nondirectory (nth 8 paths)) nil
                        (file-name-concat data "sessions/test/transcript") nil 'silent)
          (should (= 2 (mevedel-state-cleanup workspace)))
          (should-not (file-exists-p (nth 0 paths)))
          (should-not (file-exists-p (nth 7 paths)))
          (dolist (index '(1 2 3 4 5 6 8))
            (should (file-exists-p (nth index paths))))
          (should (= 0 (mevedel-state-cleanup workspace))))
      (kill-buffer buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-state-cleanup-fails-closed (:quiet t)
  (let* ((root (make-temp-file "mevedel-state-cleanup-" t))
         (workspace (mevedel-workspace--create :root root))
         (data (file-name-concat root ".mevedel"))
         (path (file-name-concat data "state/review-packages/review-orphan.md"))
         (session (file-name-concat data "sessions/test")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "payload" nil path nil 'silent)
          (set-file-times path (seconds-to-time (- (float-time) (* 8 86400))))
          (cl-letf (((symbol-function 'process-file) (lambda (&rest _) 2)))
            (should-not (mevedel-state-cleanup workspace)))
          (should (file-exists-p path))
          (make-directory (file-name-concat session ".lease") t)
          (cl-letf (((symbol-function 'mevedel-session-durability--lease-state-of)
                     (lambda (&rest _) 'foreign)))
            (should-not (mevedel-state-cleanup workspace)))
          (should (file-exists-p path))
          (let ((record (file-name-concat session ".lease/00000000000000000001.el")))
            (write-region "malformed" nil record nil 'silent)
            (should-not (mevedel-state-cleanup workspace))
            (should (file-exists-p path))
            (delete-file record))
          (delete-directory (file-name-concat session ".lease"))
          ;; Following a broken reference link must fail the complete scan.
          (make-symbolic-link "/no-such-mevedel-cleanup-reference" (file-name-concat session "link"))
          (should-not (mevedel-state-cleanup workspace))
          (should (file-exists-p path)))
      (delete-directory root t))))

(mevedel-deftest mevedel-state-cleanup--foreign-lock-p
  (:vars* ((root (make-temp-file "mevedel-state-cleanup-lock-" t))
           (lock (file-name-concat root ".lock")))
   :after-each ((delete-directory root t)))
  (cl-labels ((write-lock (pid &optional host)
                (with-temp-file lock
                  (prin1 (list :pid pid :hostname (or host (system-name))
                               :emacs-invocation-time (format-time-string "%FT%H-%M-%S")
                               :buffer "session")
                         (current-buffer)))))
    ,test)
  (test)
  :doc "an absent lock does not block"
  (should-not (mevedel-state-cleanup--foreign-lock-p root))
  :doc "this Emacs process's own lock does not block"
  (progn (write-lock (emacs-pid))
         (should-not (mevedel-state-cleanup--foreign-lock-p root)))
  :doc "a same-host lock whose holder is dead is stale and does not block"
  (progn (write-lock 2147483646)
         (should-not (mevedel-state-cleanup--foreign-lock-p root)))
  :doc "a live foreign holder on this host blocks"
  (progn (write-lock 1)
         (should (mevedel-state-cleanup--foreign-lock-p root)))
  :doc "a lock from another host blocks because its holder cannot be probed"
  (progn (write-lock (emacs-pid) "mevedel-other-host.invalid")
         (should (mevedel-state-cleanup--foreign-lock-p root)))
  :doc "an unreadable lock blocks"
  (progn (write-region "(:pid" nil lock nil 'silent)
         (should (mevedel-state-cleanup--foreign-lock-p root))))

(mevedel-deftest mevedel-state-cleanup-session-locks (:quiet t)
  (let* ((root (make-temp-file "mevedel-state-cleanup-" t))
         (workspace (mevedel-workspace--create :root root))
         (data (file-name-concat root ".mevedel"))
         (path (file-name-concat data "state/review-packages/review-orphan.md"))
         (session (file-name-concat data "sessions/test"))
         (lock (file-name-concat session ".lock")))
    (cl-labels ((orphan ()
                  (make-directory (file-name-directory path) t)
                  (write-region "payload" nil path nil 'silent)
                  (set-file-times path (seconds-to-time (- (float-time) (* 8 86400)))))
                (write-lock (pid)
                  (with-temp-file lock
                    (prin1 (list :pid pid :hostname (system-name)
                                 :emacs-invocation-time (format-time-string "%FT%H-%M-%S")
                                 :buffer "session")
                           (current-buffer)))))
      (unwind-protect
          (progn
            (make-directory session t)
            (orphan)
            (write-lock 1)
            (should-not (mevedel-state-cleanup workspace))
            (should (file-exists-p path))
            (write-lock (emacs-pid))
            (should (= 1 (mevedel-state-cleanup workspace)))
            (should-not (file-exists-p path))
            (orphan)
            (write-lock 2147483646)
            (should (= 1 (mevedel-state-cleanup workspace)))
            (should-not (file-exists-p path)))
        (delete-directory root t)))))

(mevedel-deftest mevedel-state-cleanup-keeps-changed-and-linked-files (:quiet t)
  (let* ((root (make-temp-file "mevedel-state-cleanup-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (file-name-concat root ".mevedel/state/review-packages"))
         (path (file-name-concat directory "review-orphan.md"))
         (outside (file-name-concat root "outside")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (write-region "payload" nil path nil 'silent)
          (write-region "outside" nil outside nil 'silent)
          (make-symbolic-link outside (file-name-concat directory "review-linked.md"))
          (set-file-times path (seconds-to-time (- (float-time) (* 8 86400))))
          (should-not (mevedel-state-cleanup workspace))
          (should (file-exists-p path))
          (delete-file (file-name-concat directory "review-linked.md"))
          (cl-letf (((symbol-function 'mevedel-state-cleanup--references)
                     (lambda (&rest _)
                       (write-region "changed" nil path nil 'silent)
                       nil)))
            (should (= 0 (mevedel-state-cleanup workspace))))
          (should (file-exists-p outside))
          (should (equal "changed" (mevedel-session-control-fs-read-file path))))
      (delete-directory root t))))

(mevedel-deftest mevedel-state-cleanup-empty-workspace (:quiet t)
  (let ((root (make-temp-file "mevedel-state-cleanup-" t)))
    (unwind-protect
        (progn
          (should (= 0 (mevedel-state-cleanup (mevedel-workspace--create :root root))))
          (should-not (file-exists-p (file-name-concat root ".mevedel"))))
      (delete-directory root t))))

(mevedel-deftest mevedel-state-cleanup-live-history ()
  (with-temp-buffer
    (setq-local mevedel-view-history--ring (make-ring 2))
    (ring-insert mevedel-view-history--ring "review-fromhistory.md")
    (setq-local mevedel-view-history--stored-incomplete "review-fromdraft.md")
    (should (mevedel-state-cleanup--live-reference-p "review-fromhistory.md"))
    (should (mevedel-state-cleanup--live-reference-p "review-fromdraft.md"))))

(mevedel-deftest mevedel-state-cleanup-batches (:quiet t)
  (let* ((root (make-temp-file "mevedel-state-cleanup-" t))
         (workspace (mevedel-workspace--create :root root))
         (directory (file-name-concat root ".mevedel/state/review-packages")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (dotimes (n 101)
            (let ((path (file-name-concat directory (format "review-%s.md" n))))
              (write-region "payload" nil path nil 'silent)
              (set-file-times path (seconds-to-time (- (float-time) (* 8 86400))))))
          (should (= 100 (mevedel-state-cleanup workspace)))
          (should (= 1 (mevedel-state-cleanup workspace))))
      (delete-directory root t))))

(mevedel-deftest mevedel-state-cleanup-remote (:quiet t)
  (let* ((root (make-temp-file "mevedel-state-cleanup-" t))
         (directory (file-name-concat root ".mevedel/state/review-packages"))
         (path (file-name-concat directory "review-unused.md")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (write-region "payload" nil path nil 'silent)
          (set-file-times path (seconds-to-time (- (float-time) (* 8 86400))))
          (mevedel-test--with-local-shell-tramp '("state-cleanup")
            (should (= 1 (mevedel-state-cleanup
                          (mevedel-workspace--create
                           :root (format "/mevedelmock:state-cleanup:%s/" root))))))
          (should-not (file-exists-p path)))
      (delete-directory root t))))

(provide 'test-mevedel-state-cleanup)
;;; test-mevedel-state-cleanup.el ends here
