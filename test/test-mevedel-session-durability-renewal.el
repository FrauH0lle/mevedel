;;; test-mevedel-session-durability-renewal.el --- Renewal proof tests -*- lexical-binding: t -*-

;;; Commentary:

;; An unchanged lease still needs its target proof, but not another rename.

;;; Code:

(require 'helpers (file-name-concat (file-name-directory
                                    (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-session-durability)

(mevedel-deftest mevedel-session-durability--commit-lease
  (:doc "identical renewal proves bytes and observes successors without rewriting")
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
      (delete-directory root t))))

(provide 'test-mevedel-session-durability-renewal)
;;; test-mevedel-session-durability-renewal.el ends here
