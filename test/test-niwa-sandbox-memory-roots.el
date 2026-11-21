;;; test-niwa-sandbox-memory-roots.el --- Memory root isolation -*- lexical-binding: t -*-

;;; Commentary:

;; Sandbox preparation must not plant workspace state in writable memory roots.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-sandbox)
(require 'niwa-permission-rules)

(niwa-deftest niwa-sandbox-prepare/memory-roots
  (:doc "keeps local and external memory roots unchanged while protecting workspace state")
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (skip-unless (plist-get (niwa-sandbox-probe) :available))
    (let* ((root (make-temp-file "niwa-sandbox-memory-" t))
           (external (make-temp-file "niwa-sandbox-global-memory-" t))
           (secondary (make-temp-file "niwa-sandbox-secondary-" t))
           (memory (file-name-concat root ".niwa/memory"))
           (workdir (file-name-concat root "src"))
           prepared)
      (unwind-protect
          (progn
            (make-directory memory t)
            (make-directory workdir)
            (dolist (relative '(".niwa/artifacts/.state" ".niwa/leases"))
              (make-directory (file-name-concat secondary relative) t))
            (dolist (directory (list memory external))
              (write-region "# Memory\n" nil
                            (file-name-concat directory "MEMORY.md") nil 'silent))
            (setq prepared
                  (niwa-sandbox-prepare
                   (list "sh" "-c"
                         (concat
                          "set -e; cd ..; "
                          "if touch .niwa/artifacts/.state/forged 2>/dev/null; then exit 1; fi; "
                          "if touch .niwa/leases/forged 2>/dev/null; then exit 2; fi; "
                          "if touch \"$1/.niwa/artifacts/.state/forged\" 2>/dev/null; then exit 3; fi; "
                          "if touch \"$1/.niwa/leases/forged\" 2>/dev/null; then exit 4; fi; "
                          "printf authored >.niwa/artifacts/new.html")
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
            (dolist (relative '(".niwa/artifacts/.state" ".niwa/leases"))
              (should (file-directory-p (file-name-concat root relative))))
            (with-temp-buffer
              (should (zerop (apply #'call-process (car (plist-get prepared :command))
                                    nil (current-buffer) nil
                                    (cdr (plist-get prepared :command))))))
            (should (file-exists-p (file-name-concat root ".niwa/artifacts/new.html")))
            (should-not (file-exists-p (file-name-concat root ".niwa/artifacts/.state/forged")))
            (should-not (file-exists-p (file-name-concat root ".niwa/leases/forged"))))
        (when prepared (niwa-sandbox-cleanup prepared))
        (delete-directory secondary t)
        (delete-directory external t)
        (delete-directory root t)))))

(provide 'test-niwa-sandbox-memory-roots)
;;; test-niwa-sandbox-memory-roots.el ends here
