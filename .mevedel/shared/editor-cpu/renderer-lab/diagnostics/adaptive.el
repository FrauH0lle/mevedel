;;; adaptive.el --- Adaptive read buffering diagnostic -*- lexical-binding: t -*-
(when (equal (getenv "MEVEDEL_LAB_CASE") "adaptive")
  (setq process-adaptive-read-buffering t))
(provide 'adaptive)
