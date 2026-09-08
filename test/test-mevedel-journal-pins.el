;;; test-mevedel-journal-pins.el -- Journal evidence retention -*- lexical-binding: t -*-

;;; Commentary:

;; Tests durable capture pins without a live capture buffer.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-journal-pins)

(mevedel-deftest mevedel-journal-pins-retain
  (:doc "retains independent captures until each is explicitly released")
  (let ((directory (make-temp-file "mevedel-journal-pins-" t))
        (first (make-string 64 ?a))
        (second (make-string 64 ?b))
        (head ".publications/generation-0123456789abcdef0123/manifest.el"))
    (unwind-protect
        (progn
          (should-not (mevedel-journal-pins-present-p directory))
          (should-not (mevedel-journal-pins-heads directory))
          (mevedel-journal-pins-retain directory first (list head))
          (mevedel-journal-pins-retain directory second nil)
          (should (mevedel-journal-pins-present-p directory))
          (should (equal (list head) (mevedel-journal-pins-heads directory)))
          (mevedel-journal-pins-retain directory first (list head))
          (should-error (mevedel-journal-pins-retain directory first nil))
          (mevedel-journal-pins-release directory first)
          (should-not (mevedel-journal-pins-heads directory))
          (should (mevedel-journal-pins-present-p directory))
          (mevedel-journal-pins-release directory second)
          (mevedel-journal-pins-release directory second)
          (should-not (mevedel-journal-pins-present-p directory)))
      (delete-directory directory t))))

(mevedel-deftest mevedel-journal-pins-heads
  (:doc "fails closed on corrupt pins while session cleanup still sees retention")
  (let* ((directory (make-temp-file "mevedel-journal-pins-corrupt-" t))
         (capture (make-string 64 ?a))
         (path (file-name-concat directory ".journal-pins" (concat capture ".json"))))
    (unwind-protect
        (progn
          (should-error (mevedel-journal-pins-retain directory "../escape" nil))
          (should-error (mevedel-journal-pins-retain directory capture '("../outside")))
          (should-not (mevedel-journal-pins-present-p directory))
          (mevedel-journal-pins-retain directory capture nil)
          (write-region "{}" nil path nil 'silent)
          (should (mevedel-journal-pins-present-p directory))
          (should-error (mevedel-journal-pins-heads directory))
          (should-error (mevedel-journal-pins-retain directory capture nil))
          (mevedel-journal-pins-release directory capture)
          (should-not (mevedel-journal-pins-present-p directory)))
      (delete-directory directory t))))

(provide 'test-mevedel-journal-pins)
;;; test-mevedel-journal-pins.el ends here
