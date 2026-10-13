;;; test-mevedel-permission-review-lifecycle.el --- Reviewer request lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; Offline transport callbacks exercise the isolated engine and its cleanup.

;;; Code:

(require 'mevedel-permission-review)
(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "helpers"))

(mevedel-deftest mevedel-permission-review/natural-completion ()
  (dolist (stream '(nil t))
    (dolist (synchronous '(nil t))
      (let (wire-callback buffer cancel replies messages)
        (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                   (lambda (&rest _) (list :model gptel-model :backend gptel-backend)))
                  ((symbol-function 'gptel-request)
                   (lambda (_prompt &rest args)
                     (setq wire-callback (plist-get args :callback)
                           buffer (plist-get args :buffer))
                     (when synchronous
                       (funcall wire-callback "approved" (list :stream stream))
                       (when stream (funcall wire-callback t '(:stream t))))))
                  ((symbol-function 'message)
                   (lambda (format &rest args)
                     (when format (push (apply #'format format args) messages)))))
          (unwind-protect
              (progn
                (setq cancel
                      (mevedel-permission-review--model
                       '(:operation "offline")
                       (lambda (reply)
                         (push reply replies)
                         ;; The real owner disposes in its completion callback.
                         (when cancel (funcall cancel)))
                       stream))
                (unless synchronous
                  (funcall wire-callback "approved" (list :stream stream))
                  (when stream (funcall wire-callback t '(:stream t))))
                (funcall cancel)
                (funcall wire-callback "late" nil)
                (should (equal replies '("approved")))
                (should-not (buffer-live-p buffer))
                (should-not (seq-some (lambda (text) (string-match-p "Stopped gptel request" text))
                                      messages)))
            (when cancel (funcall cancel))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(mevedel-deftest mevedel-permission-review/engine-canceller ()
  (dolist (overlong '(nil t))
    (dolist (synchronous '(nil t))
      (let (wire-callback buffer cancel replies (aborts 0))
        (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                   (lambda (&rest _) (list :model gptel-model :backend gptel-backend)))
                  ((symbol-function 'mevedel-engine-request-text)
                   (lambda (_backend _prompt _system callback &rest _)
                     (setq wire-callback callback buffer (current-buffer))
                     (when (and synchronous overlong)
                       (funcall callback (make-string (1+ mevedel-permission-review--reply-limit) ?x)
                                '(:stream t)))
                     (lambda ()
                       (cl-incf aborts)
                       (funcall callback 'abort nil)))))
          (unwind-protect
              (progn
                (setq cancel (mevedel-permission-review--model
                              nil (lambda (reply) (push reply replies)) t))
                (when (and overlong (not synchronous))
                  (funcall wire-callback
                           (make-string (1+ mevedel-permission-review--reply-limit) ?x)
                           '(:stream t)))
                (funcall cancel)
                (funcall cancel)
                (funcall wire-callback t '(:stream t))
                (should (= aborts 1))
                (should (= (length replies) (if overlong 1 0)))
                (should-not (buffer-live-p buffer)))
            (when cancel (funcall cancel))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(provide 'test-mevedel-permission-review-lifecycle)
;;; test-mevedel-permission-review-lifecycle.el ends here
