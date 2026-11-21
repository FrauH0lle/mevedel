;;; test-niwa-view-composer-external.el --- External follow-up seam tests -*- lexical-binding: t; -*-

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
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-mentions)
(require 'niwa-workspace)
(require 'niwa-pending-inputs)
(require 'niwa-view-input-files)
(require 'niwa-skills-plan)

(niwa-deftest niwa-view-enqueue-external-follow-up
  (:doc "queues attributed, granted, skill-inert input through the real session queue")
  (let* ((workspace (niwa-workspace-get-or-create
                     'project "/tmp/ext-seam/" "/tmp/ext-seam/" "ext"))
         (session (niwa-session-create "main" workspace))
         (data-buffer (generate-new-buffer " *ext-seam-data*"))
         (view-buffer (generate-new-buffer " *ext-seam-view*"))
         (image (make-temp-file "ext-seam-" nil ".jpg" "bytes"))
         rebuilt drained)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local niwa--view-buffer view-buffer)
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-view--interaction-rebuild)
                     (lambda () (setq rebuilt t)))
                    ((symbol-function 'niwa-pending-inputs-schedule-held-input-delivery)
                     (lambda () (setq drained t))))
            (let ((entry (niwa-view-enqueue-external-follow-up
                          data-buffer "look at this $review please"
                          :guest-name "Herr Boing"
                          :skills '("alpha" "beta")
                          :paths (list image))))
              (should entry)
              (should (equal '("alpha" "beta") (plist-get entry :guest-skills)))
              ;; The @file mention and its grant ride the entry; skill
              ;; tokens stay literal at submission.
              (should (string-prefix-p "look at this $review please @file:"
                                       (plist-get entry :input)))
              (should (plist-get entry :inert-skills))
              (should (equal "Herr Boing" (plist-get entry :guest-name)))
              (should (= 1 (length (plist-get entry :dropped-file-grants))))
              (should (equal entry
                             (car (niwa-session-pending-inputs
                                   session 'follow-up))))))
          (should rebuilt)
          (should drained)
          ;; No live view buffer: nothing queues.
          (kill-buffer view-buffer)
          (should-not (niwa-view-enqueue-external-follow-up
                       data-buffer "text")))
      (ignore-errors (delete-file image))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-view--submit-planned-input/inert-skills
  (:doc "skips skill planning entirely for inert-skills submissions")
  (let ((data-buffer (generate-new-buffer " *ext-inert-data*"))
        (view-buffer (generate-new-buffer " *ext-inert-view*"))
        (session (niwa-session--create))
        forwarded planned)
    (unwind-protect
        (progn
          (with-current-buffer view-buffer
            (setq-local niwa--data-buffer data-buffer))
          (cl-letf (((symbol-function 'niwa-view--session)
                     (lambda () session))
                    ((symbol-function 'niwa-skills-plan-user-input)
                     (lambda (&rest _) (setq planned t) nil))
                    ((symbol-function 'niwa-skills-input-refresh-bound-input)
                     (lambda (&rest _) nil))
                    ((symbol-function 'niwa-view--forward-input)
                     (lambda (input &rest _) (setq forwarded input))))
            (with-current-buffer view-buffer
              (niwa-view--submit-planned-input
               "run $review on this" nil nil nil nil t))
            (should (equal "run $review on this" forwarded))
            (should-not planned)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))

(niwa-deftest niwa-view--submit-planned-input/selected-skills
  (:doc "submits one real plan for explicit skills without planning argument tokens")
  (let* ((root (make-temp-file "external-skills-" t))
         (niwa-skills-check-for-modifications nil)
         (skills (mapcar
                  (lambda (name)
                    (let ((source (niwa-skills-test--write-skill
                                   root name
                                   (format "name: %s\ndescription: Test\n" name)
                                   (upcase name))))
                      (niwa-skill--create
                       :name name :source-file source :active-p t
                       :source-dir (file-name-directory source)
                       :user-invocable-p t :context 'inline)))
                  '("alpha" "beta" "gamma")))
         (session (niwa-session--create :name "selected" :skills skills))
         (data (generate-new-buffer " *selected-data*"))
         (view (generate-new-buffer " *selected-view*"))
         plans)
    (unwind-protect
        (progn
          (with-current-buffer data (setq-local niwa--session session))
          (with-current-buffer view
            (setq-local niwa--data-buffer data)
            (cl-letf (((symbol-function 'niwa-skills-plan-prepare)
                       (lambda (plan _callback &optional _cancelled)
                         (push plan plans))))
              (niwa-view--submit-planned-input
               "$gamma is pasted text" nil nil nil nil nil '("alpha" "beta"))))
          (should (= 1 (length plans)))
          (should (equal '("alpha" "beta")
                         (mapcar #'niwa-skill-plan-entry-name
                                 (niwa-skill-invocation-plan-entries (car plans)))))
          (should (equal "$gamma is pasted text"
                         (niwa-skill-invocation-plan-arguments (car plans)))))
      (kill-buffer view)
      (kill-buffer data)
      (delete-directory root t))))

;;; test-niwa-view-composer-external.el ends here
