;;; test-mevedel-sandbox-memory-roots.el --- Memory root isolation -*- lexical-binding: t -*-

;;; Commentary:

;; Sandbox preparation must not plant workspace state in writable memory roots.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-sandbox)
(require 'mevedel-permission-rules)

(mevedel-deftest mevedel-sandbox-prepare/memory-roots
  (:doc "keeps local and external memory roots unchanged while protecting workspace state")
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache nil))
    (skip-unless (plist-get (mevedel-sandbox-probe) :available))
    (let* ((root (make-temp-file "mevedel-sandbox-memory-" t))
           (external (make-temp-file "mevedel-sandbox-global-memory-" t))
           (secondary (make-temp-file "mevedel-sandbox-secondary-" t))
           (memory (file-name-concat root ".mevedel/memory"))
           (workdir (file-name-concat root "src"))
           prepared)
      (unwind-protect
          (progn
            (make-directory memory t)
            (make-directory workdir)
            (dolist (relative '(".mevedel/artifacts/.state" ".mevedel/leases"))
              (make-directory (file-name-concat secondary relative) t))
            (dolist (directory (list memory external))
              (write-region "# Memory\n" nil
                            (file-name-concat directory "MEMORY.md") nil 'silent))
            (setq prepared
                  (mevedel-sandbox-prepare
                   (list "sh" "-c"
                         (concat
                          "set -e; cd ..; "
                          "if touch .mevedel/artifacts/.state/forged 2>/dev/null; then exit 1; fi; "
                          "if touch .mevedel/leases/forged 2>/dev/null; then exit 2; fi; "
                          "if touch \"$1/.mevedel/artifacts/.state/forged\" 2>/dev/null; then exit 3; fi; "
                          "if touch \"$1/.mevedel/leases/forged\" 2>/dev/null; then exit 4; fi; "
                          "printf authored >.mevedel/artifacts/new.html")
                         "sh" secondary)
                   workdir (list root memory external secondary)))
            (should (eq 'confined (plist-get prepared :state)))
            (dolist (directory (list memory external))
              (should (equal '("MEMORY.md")
                             (directory-files directory nil directory-files-no-dot-files-regexp)))
              (should (equal "# Memory\n"
                             (with-temp-buffer
                               (insert-file-contents (file-name-concat directory "MEMORY.md"))
                               (buffer-string)))))
            (dolist (relative '(".mevedel/artifacts/.state" ".mevedel/leases"))
              (should (file-directory-p (file-name-concat root relative))))
            (with-temp-buffer
              (should (zerop (apply #'call-process (car (plist-get prepared :command))
                                    nil (current-buffer) nil
                                    (cdr (plist-get prepared :command))))))
            (should (file-exists-p (file-name-concat root ".mevedel/artifacts/new.html")))
            (should-not (file-exists-p (file-name-concat root ".mevedel/artifacts/.state/forged")))
            (should-not (file-exists-p (file-name-concat root ".mevedel/leases/forged"))))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (delete-directory secondary t)
        (delete-directory external t)
        (delete-directory root t)))))

(provide 'test-mevedel-sandbox-memory-roots)
;;; test-mevedel-sandbox-memory-roots.el ends here
