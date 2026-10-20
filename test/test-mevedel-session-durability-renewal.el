;;; test-mevedel-session-durability-renewal.el --- Renewal proof tests -*- lexical-binding: t -*-

;;; Commentary:

;; An unchanged lease still needs its target proof, but not another rename.

;;; Code:

(require 'helpers (file-name-concat (file-name-directory
                                    (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-session-durability)

(mevedel-deftest mevedel-session-durability--commit-lease
  ()
  ,test
  (test)
  :doc "identical renewal proves bytes and observes successors without rewriting"
  (let* ((root (make-temp-file "mevedel-renewal-proof-" t))
         (path (mevedel-session-durability--generation-path root 1))
         (record '(:proof "same"))
         (bytes (mevedel-session-durability--record-bytes record))
         (run (symbol-function 'mevedel-session-control-fs-run-program))
         operations)
    (unwind-protect
        (progn
          (mevedel-session-control-fs-write-file path bytes)
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (ops &rest args)
                       (setq operations ops)
                       (apply run ops args))))
            (should (mevedel-session-durability--commit-lease root 1 bytes record))
            (should (eq 'verify (plist-get (car operations) :op)))
            (should-not (cl-find 'write operations :key (lambda (op) (plist-get op :op))))
            (let* ((reading (cons 123 (float-time)))
                   (mevedel-session-durability--transaction-clock
                    (list (list (cons root reading)))))
              (should (mevedel-session-durability--commit-lease root 1 bytes record))
              (should-not (cl-find 'target-time operations :key (lambda (op) (plist-get op :op))))
              ;; Reuse does not refresh the observation's age.
              (should (eq reading (cdar (car mevedel-session-durability--transaction-clock))))
              (setcdr reading (- (float-time) 2))
              (should (mevedel-session-durability--commit-lease root 1 bytes record))
              (should (cl-find 'target-time operations :key (lambda (op) (plist-get op :op))))
              (should-not (eq reading (cdar (car mevedel-session-durability--transaction-clock)))))
            (mevedel-session-control-fs-write-file path "different")
            (should-not (mevedel-session-durability--commit-lease root 1 bytes record))
            (should (equal "different" (mevedel-session-control-fs-read-file path)))
            (mevedel-session-control-fs-write-file path bytes)
            (let ((next (mevedel-session-durability--generation-path root 2)))
              (mevedel-session-control-fs-write-file next "successor")
              (should (member (file-name-nondirectory next)
                              (mevedel-session-durability--commit-lease root 1 bytes record))))))
      (delete-directory root t)))

  :doc "carries rider operations around its own, in order, and only once"
  (let* ((root (make-temp-file "mevedel-commit-riders-" t))
         (path (mevedel-session-durability--generation-path root 1))
         (bytes (mevedel-session-durability--record-bytes '(:old t)))
         (record '(:new t))
         (before (file-name-concat root "before"))
         (after (file-name-concat root "after"))
         (run (symbol-function 'mevedel-session-control-fs-run-program))
         (riders (list :before (list (list :op 'write :path before :content "b"))
                       :after (list (list :op 'write :path after :content "a"))
                       :consumed nil :before-results nil :after-results nil))
         programs)
    (unwind-protect
        (progn
          (mevedel-session-control-fs-write-file path bytes)
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (ops &rest args)
                       (push (mapcar (lambda (op) (plist-get op :op)) ops) programs)
                       (apply run ops args))))
            (let ((mevedel-session-durability--commit-riders riders))
              (should (mevedel-session-durability--commit-lease root 1 bytes record))
              ;; One program: the prelude, the commit, then the riders after it.
              (should (equal '(write verify write list-directory target-time write)
                             (car programs)))
              (should (plist-get riders :consumed))
              (should (eq 'ok (plist-get (car (plist-get riders :before-results)) :status)))
              (should (eq 'ok (plist-get (car (plist-get riders :after-results)) :status)))
              (should (equal "b" (mevedel-session-control-fs-read-file before)))
              (should (equal "a" (mevedel-session-control-fs-read-file after)))
              ;; A later commit under the same binding carries nothing.
              (should (mevedel-session-durability--commit-lease
                       root 1 (mevedel-session-durability--record-bytes record)
                       '(:newer t)))
              (should (equal 'verify (car (car programs)))))))
      (delete-directory root t)))

  :doc "a failed prelude runs nothing of the commit and signals"
  (let* ((root (make-temp-file "mevedel-commit-prelude-" t))
         (path (mevedel-session-durability--generation-path root 1))
         (bytes (mevedel-session-durability--record-bytes '(:old t)))
         (after (file-name-concat root "after"))
         (riders (list :before (list (list :op 'make-directory :path root))
                       :after (list (list :op 'write :path after :content "a"))
                       :consumed nil :before-results nil :after-results nil)))
    (unwind-protect
        (progn
          (mevedel-session-control-fs-write-file path bytes)
          (let ((mevedel-session-durability--commit-riders riders))
            ;; The directory exists: a non-optional conflict ends the program.
            (let ((err (should-error (mevedel-session-durability--commit-lease
                                      root 1 bytes '(:new t))
                                     :type 'mevedel-session-durability-prelude-failed)))
              (should (eq 'conflict (plist-get (car (cadr err)) :status)))))
          (should (equal bytes (mevedel-session-control-fs-read-file path)))
          (should-not (file-exists-p after)))
      (delete-directory root t)))

  :doc "a failed proof leaves the riders after it unwritten and skipped"
  (let* ((root (make-temp-file "mevedel-commit-proof-" t))
         (path (mevedel-session-durability--generation-path root 1))
         (after (file-name-concat root "after"))
         (riders (list :before nil
                       :after (list (list :op 'write :path after :content "a"))
                       :consumed nil :before-results nil :after-results nil)))
    (unwind-protect
        (progn
          (mevedel-session-control-fs-write-file path "someone else's")
          (let ((mevedel-session-durability--commit-riders riders))
            (should-not (mevedel-session-durability--commit-lease
                         root 1 (mevedel-session-durability--record-bytes '(:old t))
                         '(:new t))))
          (should (eq 'skipped (plist-get (car (plist-get riders :after-results)) :status)))
          (should-not (file-exists-p after)))
      (delete-directory root t)))

  :doc "a deadline lapsed during the prelude stops the commit"
  (let* ((root (make-temp-file "mevedel-commit-deadline-" t))
         (path (mevedel-session-durability--generation-path root 1))
         (bytes (mevedel-session-durability--record-bytes '(:old t)))
         (before (file-name-concat root "before"))
         (riders (list :before (list (list :op 'write :path before :content "b"))
                       :after nil :consumed nil :before-results nil :after-results nil)))
    (unwind-protect
        (progn
          (mevedel-session-control-fs-write-file path bytes)
          (let ((mevedel-session-durability--commit-riders riders))
            (let ((err (should-error (mevedel-session-durability--commit-lease
                                      root 1 bytes '(:new t) 1)
                                     :type 'mevedel-session-durability-prelude-failed)))
              ;; The prelude ran; the clock proof after it refused the commit.
              (should (equal '(ok mismatch)
                             (mapcar (lambda (result) (plist-get result :status))
                                     (cadr err))))))
          (should (equal "b" (mevedel-session-control-fs-read-file before)))
          (should (equal bytes (mevedel-session-control-fs-read-file path))))
      (delete-directory root t))))

(provide 'test-mevedel-session-durability-renewal)
;;; test-mevedel-session-durability-renewal.el ends here
