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
(require 'mevedel-workspace)
(require 'mevedel-pending-inputs)
(require 'mevedel-view-input-files)
(require 'mevedel-skills-plan)

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
                             (car (mevedel-session-pending-inputs
                                   session 'follow-up))))
              ;; Unscoped by default: an external prompt lands in main
              ;; chat unless it names a directive.
              (should-not (plist-get entry :scope)))
            ;; A named directive scopes the entry to that directive's
            ;; discussion -- the one directive action that mutates
            ;; nothing, and the only one external input may reach.
            (let ((scoped (mevedel-view-enqueue-external-follow-up
                           data-buffer "and here?"
                           :guest-name "Herr Boing"
                           :directive-id "dir-7")))
              (should (equal '(:directive-id "dir-7" :action discuss)
                             (plist-get scoped :scope)))
              (should (plist-get scoped :inert-skills))))
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
                    ((symbol-function 'mevedel-skills-input-refresh-bound-input)
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

(mevedel-deftest mevedel-view--submit-planned-input/selected-skills
  (:doc "submits one real plan for explicit skills without planning argument tokens")
  (let* ((root (make-temp-file "external-skills-" t))
         (mevedel-skills-check-for-modifications nil)
         (skills (mapcar
                  (lambda (name)
                    (let ((source (mevedel-skills-test--write-skill
                                   root name
                                   (format "name: %s\ndescription: Test\n" name)
                                   (upcase name))))
                      (mevedel-skill--create
                       :name name :source-file source :active-p t
                       :source-dir (file-name-directory source)
                       :user-invocable-p t :context 'inline)))
                  '("alpha" "beta" "gamma")))
         (session (mevedel-session--create :name "selected" :skills skills))
         (data (generate-new-buffer " *selected-data*"))
         (view (generate-new-buffer " *selected-view*"))
         plans)
    (unwind-protect
        (progn
          (with-current-buffer data (setq-local mevedel--session session))
          (with-current-buffer view
            (setq-local mevedel--data-buffer data)
            (cl-letf (((symbol-function 'mevedel-skills-plan-prepare)
                       (lambda (plan _callback &optional _cancelled)
                         (push plan plans))))
              (mevedel-view--submit-planned-input
               "$gamma is pasted text" nil nil nil nil nil '("alpha" "beta"))))
          (should (= 1 (length plans)))
          (should (equal '("alpha" "beta")
                         (mapcar #'mevedel-skill-plan-entry-name
                                 (mevedel-skill-invocation-plan-entries (car plans)))))
          (should (equal "$gamma is pasted text"
                         (mevedel-skill-invocation-plan-arguments (car plans)))))
      (kill-buffer view)
      (kill-buffer data)
      (delete-directory root t))))

;;; test-mevedel-view-composer-external.el ends here
