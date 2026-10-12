;;; view-header-wayland.el --- Archived prompt redisplay regression -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise Emacs's real pixel visibility query during header evaluation.
;; Bound recursive entry so the regression fails instead of freezing the child.

;;; Code:

(setq load-path (read (getenv "MEVEDEL_TEST_LOAD_PATH")))
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-segments)

(run-at-time
 0.1 nil
 (lambda ()
   (let ((status 0) observations (depth 0) (maximum 0))
     (condition-case err
         (progn
           (select-frame (make-frame-on-display
                          (getenv "MEVEDEL_NATIVE_DISPLAY")
                          '((width . 80) (height . 25))))
           (sit-for 0.1)
           (with-temp-buffer
             (switch-to-buffer (current-buffer))
             (insert "Continued answer\n"
                     (propertize "Prompt\n" 'mevedel-view-prompt-preview "Prompt")
                     "More answer\n")
             (setq-local mevedel-view--input-marker (copy-marker (point)))
             (insert "> draft\nsecond line")
             (setq-local mevedel-view--continuation-prompt-cache
                         '(nil nil nil nil (:segment 1 :pos 1 :preview "Archived prompt")))
             (setq-local header-line-format '(:eval (mevedel-view--sticky-prompt-line)))
             (goto-char (point-min))
             (set-window-start nil (point-min) t)
             (let ((probe (lambda (original &rest args)
                            (cl-incf depth)
                            (unwind-protect
                                (progn
                                  (setq maximum (max depth maximum))
                                  (when (< depth 3) (apply original args)))
                              (cl-decf depth)))))
               (advice-add 'mevedel-view--sticky-prompt-line :around probe)
               (unwind-protect
                   (let (visible hidden)
                     (redisplay t)
                     (sit-for 0.05)
                     (set-window-parameter nil 'mevedel-view--prompt-on-screen nil)
                     (setq visible (mevedel-view--sticky-prompt-line))
                     ;; A content change invalidates the per-window answer.
                     (goto-char (point-min))
                     (insert (apply #'concat (make-list 80 "Continued answer\n")))
                     (goto-char (point-min))
                     (set-window-start nil (point-min) t)
                     (redisplay t)
                     (setq hidden (mevedel-view--sticky-prompt-line))
                     (setq observations
                           (list :depth maximum :visible visible
                                 :hidden (and hidden (substring-no-properties hidden))
                                 :draft (buffer-substring-no-properties
                                         mevedel-view--input-marker (point-max))
                                 :header-restored
                                 (equal header-line-format
                                        '(:eval (mevedel-view--sticky-prompt-line))))))
                 (advice-remove 'mevedel-view--sticky-prompt-line probe)))))
       (error (setq status 1 observations (list :error (error-message-string err)))))
     (with-temp-file (getenv "MEVEDEL_NATIVE_RESULT") (prin1 observations (current-buffer)))
     (kill-emacs status))))

;;; view-header-wayland.el ends here
