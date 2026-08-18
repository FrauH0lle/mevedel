;;; test-mevedel-view-composer-external.el --- External follow-up seam tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused coverage for the external follow-up queue seam and its
;; skill-inert submission guarantee.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-mentions)

(mevedel-deftest mevedel-view-enqueue-external-follow-up
  (:doc "queues attributed, granted, skill-inert input through the real session queue")
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "/tmp/ext-seam/" "/tmp/ext-seam/" "ext"))
         (session (mevedel-session-create "main" workspace))
         (data-buffer (generate-new-buffer " *ext-seam-data*"))
         (view-buffer (generate-new-buffer " *ext-seam-view*"))
         (image (make-temp-file "ext-seam-" nil ".jpg" "bytes"))
         rebuilt drained)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer)
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-view--interaction-rebuild)
                     (lambda () (setq rebuilt t)))
                    ((symbol-function 'mevedel-view--schedule-late-follow-up-drain)
                     (lambda () (setq drained t))))
            (let ((entry (mevedel-view-enqueue-external-follow-up
                          data-buffer "look at this $review please"
                          :guest-name "Herr Boing"
                          :paths (list image))))
              (should entry)
              ;; The @file mention and its grant ride the entry; skill
              ;; tokens stay literal at submission.
              (should (string-prefix-p "look at this $review please @file:"
                                       (plist-get entry :input)))
              (should (plist-get entry :inert-skills))
              (should (equal "Herr Boing" (plist-get entry :guest-name)))
              (should (= 1 (length (plist-get entry :dropped-file-grants))))
              (should (equal entry
                             (car (mevedel-session-pending-inputs
                                   session 'follow-up))))))
          (should rebuilt)
          (should drained)
          ;; No live view buffer: nothing queues.
          (kill-buffer view-buffer)
          (should-not (mevedel-view-enqueue-external-follow-up
                       data-buffer "text")))
      (ignore-errors (delete-file image))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-view--submit-planned-input/inert-skills
  (:doc "skips skill planning entirely for inert-skills submissions")
  (let ((data-buffer (generate-new-buffer " *ext-inert-data*"))
        (view-buffer (generate-new-buffer " *ext-inert-view*"))
        forwarded planned)
    (unwind-protect
        (progn
          (with-current-buffer view-buffer
            (setq-local mevedel--data-buffer data-buffer))
          (cl-letf (((symbol-function 'mevedel-view--session)
                     (lambda () 'session))
                    ((symbol-function 'mevedel-skills-plan-user-input)
                     (lambda (&rest _) (setq planned t) nil))
                    ((symbol-function 'mevedel-skills-refresh-bound-input)
                     (lambda (&rest _) nil))
                    ((symbol-function 'mevedel-view--forward-input)
                     (lambda (input &rest _) (setq forwarded input))))
            (with-current-buffer view-buffer
              (mevedel-view--submit-planned-input
               "run $review on this" nil nil nil nil t))
            (should (equal "run $review on this" forwarded))
            (should-not planned)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))

;;; test-mevedel-view-composer-external.el ends here
