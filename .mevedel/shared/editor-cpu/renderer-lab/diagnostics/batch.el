;;; batch.el --- Batch delay diagnostic -*- lexical-binding: t -*-
(let ((delay (getenv "MEVEDEL_LAB_CASE")))
  (when (string-match-p "\\`[0-9.]+\\'" delay)
    (setq mevedel-gptel-stream-bridge-insert-batch-delay (string-to-number delay))))
(provide 'batch)
