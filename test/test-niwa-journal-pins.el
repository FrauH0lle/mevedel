;;; test-niwa-journal-pins.el -- Journal evidence retention -*- lexical-binding: t -*-

;;; Commentary:

;; Tests durable capture pins without a live capture buffer.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-journal-pins)

(niwa-deftest niwa-journal-pins-retain
  (:doc "retains independent captures until each is explicitly released")
  (let ((directory (make-temp-file "niwa-journal-pins-" t))
        (first (make-string 64 ?a))
        (second (make-string 64 ?b))
        (head ".publications/generation-0123456789abcdef0123/manifest.el"))
    (unwind-protect
        (progn
          (should-not (niwa-journal-pins-present-p directory))
          (should-not (niwa-journal-pins-heads directory))
          (niwa-journal-pins-retain directory first (list head))
          (niwa-journal-pins-retain directory second nil)
          (should (niwa-journal-pins-present-p directory))
          (should (equal (list head) (niwa-journal-pins-heads directory)))
          (niwa-journal-pins-retain directory first (list head))
          (should-error (niwa-journal-pins-retain directory first nil))
          (niwa-journal-pins-release directory first)
          (should-not (niwa-journal-pins-heads directory))
          (should (niwa-journal-pins-present-p directory))
          (niwa-journal-pins-release directory second)
          (niwa-journal-pins-release directory second)
          (should-not (niwa-journal-pins-present-p directory)))
      (delete-directory directory t))))

(niwa-deftest niwa-journal-pins-heads
  (:doc "fails closed on corrupt pins while session cleanup still sees retention")
  (let* ((directory (make-temp-file "niwa-journal-pins-corrupt-" t))
         (capture (make-string 64 ?a))
         (path (file-name-concat directory ".journal-pins" (concat capture ".json"))))
    (unwind-protect
        (progn
          (should-error (niwa-journal-pins-retain directory "../escape" nil))
          (should-error (niwa-journal-pins-retain directory capture '("../outside")))
          (should-not (niwa-journal-pins-present-p directory))
          (niwa-journal-pins-retain directory capture nil)
          (write-region "{}" nil path nil 'silent)
          (should (niwa-journal-pins-present-p directory))
          (should-error (niwa-journal-pins-heads directory))
          (should-error (niwa-journal-pins-retain directory capture nil))
          (niwa-journal-pins-release directory capture)
          (should-not (niwa-journal-pins-present-p directory)))
      (delete-directory directory t))))

(niwa-deftest niwa-journal-pins-observe
  (:doc "proves an unchanged pin set and refuses one that gained, lost or changed a pin")
  (let* ((directory (make-temp-file "niwa-journal-pins-observe-" t))
         (first (make-string 64 ?a))
         (second (make-string 64 ?b))
         (head ".publications/generation-0123456789abcdef0123/manifest.el")
         (proved (lambda (proof)
                   (cl-every (lambda (result) (eq 'ok (plist-get result :status)))
                             (niwa-session-control-fs-run-program proof)))))
    (unwind-protect
        (let ((empty (niwa-journal-pins-observe directory)))
          (should-not (car empty))
          (should (funcall proved (cdr empty)))
          (niwa-journal-pins-retain directory first (list head))
          (should-not (funcall proved (cdr empty)))
          (let ((one (niwa-journal-pins-observe directory)))
            (should (equal (list head) (car one)))
            (should (funcall proved (cdr one)))
            (niwa-journal-pins-retain directory second nil)
            (should-not (funcall proved (cdr one)))
            (niwa-journal-pins-release directory second)
            (should (funcall proved (cdr one)))
            (write-region (json-serialize (list :capture-id first :heads []))
                          nil (niwa-journal-pins--path directory first) nil 'silent)
            (should-not (funcall proved (cdr one)))
            (niwa-journal-pins-release directory first)
            (should-not (funcall proved (cdr one)))
            (should (funcall proved (cdr empty)))))
      (delete-directory directory t))))

(provide 'test-niwa-journal-pins)
;;; test-niwa-journal-pins.el ends here
