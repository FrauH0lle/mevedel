;;; eask-startup.el --- Cold Eask isolation fixture -*- lexical-binding: t -*-

;;; Commentary:

;; Loaded alone by a fresh Eask process, before any shared test helpers.
;; ERT redirects native caches again when running tests, so capture startup
;; state here rather than inspecting that later, already-isolated state.

;;; Code:

(require 'ert)

(defconst mevedel-test-eask--startup-state
  (list (getenv "HOME") (copy-sequence native-comp-eln-load-path)
        (featurep 'gptel-request))
  "HOME, native cache paths, and dependency load state before ERT starts.")

(ert-deftest mevedel-eask-startup/native-cache ()
  (pcase-let ((`(,home ,paths ,gptel-loaded)
               mevedel-test-eask--startup-state))
    (should-not gptel-loaded)
    (should (file-in-directory-p (car paths) home))
    (should (equal (cdr paths)
                   (list (car (last native-comp-eln-load-path)))))
    (should-not package-archives)
    ;; Disabling archives must not disable installed dependency activation.
    (should (require 'gptel-request))))

(provide 'eask-startup)
;;; eask-startup.el ends here
