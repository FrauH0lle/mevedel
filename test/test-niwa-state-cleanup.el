;;; test-niwa-state-cleanup.el --- Artifact retention tests -*- lexical-binding: t -*-

;;; Commentary:
;; Real files exercise reference checks and conservative deletion.

;;; Code:

(require 'niwa-state-cleanup)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-state-cleanup-retention (:quiet t)
  (let* ((root (make-temp-file "niwa-state-cleanup-" t))
         (workspace (niwa-workspace--create :root root))
         (data (file-name-concat root ".niwa"))
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
          (should (= 2 (niwa-state-cleanup workspace)))
          (should-not (file-exists-p (nth 0 paths)))
          (should-not (file-exists-p (nth 7 paths)))
          (dolist (index '(1 2 3 4 5 6 8))
            (should (file-exists-p (nth index paths))))
          (should (= 0 (niwa-state-cleanup workspace))))
      (kill-buffer buffer)
      (delete-directory root t))))

(niwa-deftest niwa-state-cleanup-fails-closed (:quiet t)
  (let* ((root (make-temp-file "niwa-state-cleanup-" t))
         (workspace (niwa-workspace--create :root root))
         (data (file-name-concat root ".niwa"))
         (path (file-name-concat data "state/review-packages/review-orphan.md"))
         (session (file-name-concat data "sessions/test")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "payload" nil path nil 'silent)
          (set-file-times path (seconds-to-time (- (float-time) (* 8 86400))))
          (cl-letf (((symbol-function 'process-file) (lambda (&rest _) 2)))
            (should-not (niwa-state-cleanup workspace)))
          (should (file-exists-p path))
          (make-directory (file-name-concat session ".lease") t)
          (cl-letf (((symbol-function 'niwa-session-durability--lease-state-of)
                     (lambda (&rest _) 'foreign)))
            (should-not (niwa-state-cleanup workspace)))
          (should (file-exists-p path))
          (let ((record (file-name-concat session ".lease/00000000000000000001.el")))
            (write-region "malformed" nil record nil 'silent)
            (should-not (niwa-state-cleanup workspace))
            (should (file-exists-p path))
            (delete-file record))
          (delete-directory (file-name-concat session ".lease"))
          ;; Following a broken reference link must fail the complete scan.
          (make-symbolic-link "/no-such-niwa-cleanup-reference" (file-name-concat session "link"))
          (should-not (niwa-state-cleanup workspace))
          (should (file-exists-p path)))
      (delete-directory root t))))

(niwa-deftest niwa-state-cleanup-session-owners (:quiet t)
  (let* ((root (make-temp-file "niwa-state-cleanup-" t))
         (workspace (niwa-workspace--create :root root))
         (data (file-name-concat root ".niwa"))
         (path (file-name-concat data "state/review-packages/review-orphan.md"))
         (session (file-name-concat data "sessions/test"))
         (lock (file-name-concat session ".lock"))
         (foreign-client (make-string 64 ?b)))
    (unwind-protect
        (progn
          (make-directory session t)
          (make-directory (file-name-directory path) t)
          (write-region "payload" nil path nil 'silent)
          (set-file-times path (seconds-to-time (- (float-time) (* 8 86400))))
          ;; Pre-lease bytes are never interpreted or reclaimed by retention.
          (dolist (evidence (list "(:pid 1)" (format "(:pid %s)" (emacs-pid))
                                 "(:pid 2147483646)" "malformed"))
            (write-region evidence nil lock nil 'silent)
            (should-not (niwa-state-cleanup workspace))
            (should (file-exists-p path))
            (should (equal evidence (niwa-session-control-fs-read-file lock))))
          (delete-file lock)
          (let ((niwa-session-durability--client-id foreign-client))
            (should (niwa-session-durability-lease-acquire session "foreign")))
          (should-not (niwa-state-cleanup workspace))
          (should (file-exists-p path))
          (let ((niwa-session-durability--client-id foreign-client))
            (niwa-session-durability-lease-release session))
          (should (niwa-session-durability-lease-acquire session "own"))
          (should (= 1 (niwa-state-cleanup workspace)))
          (should-not (file-exists-p path)))
      (niwa-session-durability-lease-release session)
      (delete-directory root t))))

(niwa-deftest niwa-state-cleanup-keeps-changed-and-linked-files (:quiet t)
  (let* ((root (make-temp-file "niwa-state-cleanup-" t))
         (workspace (niwa-workspace--create :root root))
         (directory (file-name-concat root ".niwa/state/review-packages"))
         (path (file-name-concat directory "review-orphan.md"))
         (outside (file-name-concat root "outside")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (write-region "payload" nil path nil 'silent)
          (write-region "outside" nil outside nil 'silent)
          (make-symbolic-link outside (file-name-concat directory "review-linked.md"))
          (set-file-times path (seconds-to-time (- (float-time) (* 8 86400))))
          (should-not (niwa-state-cleanup workspace))
          (should (file-exists-p path))
          (delete-file (file-name-concat directory "review-linked.md"))
          (cl-letf (((symbol-function 'niwa-state-cleanup--references)
                     (lambda (&rest _)
                       (write-region "changed" nil path nil 'silent)
                       nil)))
            (should (= 0 (niwa-state-cleanup workspace))))
          (should (file-exists-p outside))
          (should (equal "changed" (niwa-session-control-fs-read-file path))))
      (delete-directory root t))))

(niwa-deftest niwa-state-cleanup-empty-workspace (:quiet t)
  (let ((root (make-temp-file "niwa-state-cleanup-" t)))
    (unwind-protect
        (progn
          (should (= 0 (niwa-state-cleanup (niwa-workspace--create :root root))))
          (should-not (file-exists-p (file-name-concat root ".niwa"))))
      (delete-directory root t))))

(niwa-deftest niwa-state-cleanup-live-history ()
  (with-temp-buffer
    (setq-local niwa-view-history--ring (make-ring 2))
    (ring-insert niwa-view-history--ring "review-fromhistory.md")
    (setq-local niwa-view-history--stored-incomplete "review-fromdraft.md")
    (should (niwa-state-cleanup--live-reference-p "review-fromhistory.md"))
    (should (niwa-state-cleanup--live-reference-p "review-fromdraft.md"))))

(niwa-deftest niwa-state-cleanup-batches (:quiet t)
  (let* ((root (make-temp-file "niwa-state-cleanup-" t))
         (workspace (niwa-workspace--create :root root))
         (directory (file-name-concat root ".niwa/state/review-packages")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (dotimes (n 101)
            (let ((path (file-name-concat directory (format "review-%s.md" n))))
              (write-region "payload" nil path nil 'silent)
              (set-file-times path (seconds-to-time (- (float-time) (* 8 86400))))))
          (should (= 100 (niwa-state-cleanup workspace)))
          (should (= 1 (niwa-state-cleanup workspace))))
      (delete-directory root t))))

(niwa-deftest niwa-state-cleanup-remote (:quiet t)
  (let* ((root (make-temp-file "niwa-state-cleanup-" t))
         (directory (file-name-concat root ".niwa/state/review-packages"))
         (path (file-name-concat directory "review-unused.md")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (write-region "payload" nil path nil 'silent)
          (set-file-times path (seconds-to-time (- (float-time) (* 8 86400))))
          (niwa-test--with-local-shell-tramp '("state-cleanup")
            (should (= 1 (niwa-state-cleanup
                          (niwa-workspace--create
                           :root (format "/niwamock:state-cleanup:%s/" root))))))
          (should-not (file-exists-p path)))
      (delete-directory root t))))

(provide 'test-niwa-state-cleanup)
;;; test-niwa-state-cleanup.el ends here
