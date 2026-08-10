;;; test-mevedel-directive-activity.el -- Directive inspector tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for the read-only workspace directive inspector.

;;; Code:

(require 'mevedel)
(require 'mevedel-directive-activity)
(require 'mevedel-overlays)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-directive-activity-test--fixture ()
  "Return workspace, source buffer, overlay, and durable record."
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "inspector" :root "/tmp"
                     :name "inspector"))
         (source (generate-new-buffer " *directive-inspector-source*")))
    (with-current-buffer source
      (insert "source")
      (setq-local mevedel--workspace workspace)
      (let ((overlay
             (mevedel--create-directive-in
              source (point-min) (point-max) nil "Explain this code")))
        (list workspace source overlay
              (car (mevedel-workspace-directives workspace)))))))

(defun mevedel-directive-activity-test--discard (fixture)
  "Discard buffers created for FIXTURE."
  (dolist (buffer (buffer-list))
    (when (or (eq buffer (nth 1 fixture))
              (and (buffer-live-p buffer)
                   (eq (car fixture)
                       (buffer-local-value
                        'mevedel-directive-activity--workspace buffer))))
      (with-current-buffer buffer
        (setq-local kill-buffer-hook nil))
      (kill-buffer buffer))))

(defun mevedel-directive-activity-test--attempt ()
  "Return one durable implementation attempt."
  (mevedel-directive-attempt--create
   :sequence 1 :action 'request-changes
   :directive-request "Explain this code"
   :request "Implement request" :result "Implemented"
   :outcome 'success :patch "diff --git a/a b/a"
   :capture 'incomplete :covered-files '("/tmp/a.el")
   :gaps '(("/tmp/b.el" . "capture failed"))
   :untracked-effects '(("Bash" . "untracked command effects"))
   :captured-at "2026-08-07T00:00:00+0200"
   :checkpoint '(:session-id "main" :turn 2)))

(mevedel-deftest mevedel-directive-activity--attempt-details
  ()
  ,test
  (test)
  :doc "renders capture quality, coverage, gaps, timestamp, and checkpoint"
  (let ((text
         (mevedel-directive-activity--attempt-details
          (mevedel-directive-activity-test--attempt))))
    (should (string-match-p "Capture: Incomplete" text))
    (should (string-match-p "Captured at: 2026-08-07" text))
    (should (string-match-p "Checkpoint: main turn 2" text))
    (should (string-match-p "Covered files: /tmp/a.el" text))
    (should (string-match-p "File gaps: /tmp/b.el" text))
    (should (string-match-p "Untracked effects: Bash" text))))

(mevedel-deftest mevedel-open-directive-activity
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "opens one read-only inspector without a composer or live transcript"
  (let* ((fixture (mevedel-directive-activity-test--fixture))
         (workspace (nth 0 fixture))
         (overlay (nth 2 fixture))
         (record (nth 3 fixture)))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                   (lambda (buffer &rest _) buffer)))
          (let ((inspector (mevedel-open-directive-activity overlay)))
            (with-current-buffer inspector
              (should (derived-mode-p 'mevedel-directive-activity-mode))
              (should-not (derived-mode-p 'mevedel-view-mode))
              (should buffer-read-only)
              (should (eq workspace mevedel-directive-activity--workspace))
              (should (eq record mevedel-directive-activity--directive))
              (should (string-match-p "Explain this code" (buffer-string)))
              (should (string-match-p "No activity yet" (buffer-string)))
              (should-not (boundp 'mevedel-directive-activity--input-marker)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-refresh
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "projects immutable attempts through the ordinary transcript renderer"
  (let* ((fixture (mevedel-directive-activity-test--fixture))
         (record (nth 3 fixture))
         (inspector (generate-new-buffer " *directive-inspector*")))
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record)
                (list (mevedel-directive-activity-test--attempt))
                (mevedel-directive-planning-enabled record) t
                (mevedel-directive-planning record)
                (list (list :sequence 2 :action 'request-changes
                            :directive-request "Explain this code"
                            :request "Plan request" :result "Plan result"
                            :outcome 'success
                            :checkpoint '(:session-id "main" :turn 3)))
                (mevedel-directive-state record) 'implemented)
          (with-current-buffer inspector
            (mevedel-directive-activity-mode)
            (setq-local mevedel-directive-activity--workspace (nth 0 fixture)
                        mevedel-directive-activity--directive record)
            (mevedel-directive-activity-refresh)
            (should buffer-read-only)
            (should (string-match-p
                     "◆ .+ · Request changes · T2 · Implemented"
                     (buffer-string)))
            (should (string-match-p "Plan · T3" (buffer-string)))
            (should (string-match-p "PLAN.+On" (buffer-string)))
            (should (buffer-live-p
                     mevedel-directive-activity--transcript-buffer))
            (goto-char (point-min))
            (search-forward "◆ ")
            (mevedel-view-toggle-section)
            (should (string-match-p "Implemented" (buffer-string)))
            (should (string-match-p "Capture: Incomplete" (buffer-string)))
            (should (string-match-p "Checkpoint: main turn 2"
                                    (buffer-string)))
            (kill-buffer (nth 1 fixture))
            (mevedel-directive-activity-refresh)
            (should (string-match-p "Request changes · T2 · excluded"
                                    (buffer-string)))
            (should (string-match-p "Implemented" (buffer-string)))
            (mevedel-directive-activity-refresh)
            (should (= 1 (how-many "Request changes · T2"
                                   (point-min) (point-max))))
            (setf (mevedel-directive-attempts record) nil
                  (mevedel-directive-planning record) nil
                  (mevedel-directive-state record) nil)
            (mevedel-directive-activity-refresh)
            (should-not (string-match-p "T2" (buffer-string)))
            (should (string-match-p "No activity yet" (buffer-string)))
            (should-not mevedel-directive-activity--transcript-buffer)))
      (kill-buffer inspector)
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-request-changes
  ()
  ,test
  (test)
  :doc "enters Request changes scope for the inspected directive"
  (let ((record
         (mevedel-directive--create
          :request "Request"
          :attempts
          (list (mevedel-directive-attempt--create
                 :directive-request "Request" :outcome 'success))))
        captured)
    (with-temp-buffer
      (mevedel-directive-activity-mode)
      (setq-local mevedel-directive-activity--workspace 'workspace
                  mevedel-directive-activity--directive record)
      (cl-letf (((symbol-function 'mevedel-view-enter-directive-scope)
                 (lambda (&rest args) (setq captured args))))
        (mevedel-directive-activity-request-changes))
      (should (equal (list record 'request-changes nil 'workspace)
                     captured)))))

(mevedel-deftest mevedel-directive-activity-retry
  ()
  ,test
  (test)
  :doc "enters Retry scope for the inspected directive"
  (let ((record
         (mevedel-directive--create
          :request "Request"
          :attempts
          (list (mevedel-directive-attempt--create
                 :directive-request "Request" :outcome 'failure))))
        captured)
    (with-temp-buffer
      (mevedel-directive-activity-mode)
      (setq-local mevedel-directive-activity--workspace 'workspace
                  mevedel-directive-activity--directive record)
      (cl-letf (((symbol-function 'mevedel-view-enter-directive-scope)
                 (lambda (&rest args) (setq captured args))))
        (mevedel-directive-activity-retry))
      (should (equal (list record 'retry nil 'workspace) captured)))))

(mevedel-deftest mevedel-directive-activity-implement-this
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "dispatches Implement this through the attached source overlay"
  (let* ((fixture (mevedel-directive-activity-test--fixture)) captured)
    (unwind-protect
        (with-temp-buffer
          (mevedel-directive-activity-mode)
          (setq-local mevedel-directive-activity--workspace (nth 0 fixture)
                      mevedel-directive-activity--directive (nth 3 fixture))
          (cl-letf (((symbol-function 'mevedel--implement-discussion)
                     (lambda (directive &optional _) (setq captured directive))))
            (mevedel-directive-activity-implement-this))
          (should (eq (nth 2 fixture) captured)))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-rewind
  ()
  ,test
  (test)
  :doc "rewinds from the selected attempt checkpoint"
  (let* ((attempt (mevedel-directive-activity-test--attempt))
         (record (mevedel-directive--create :attempts (list attempt)))
         captured)
    (with-temp-buffer
      (mevedel-directive-activity-mode)
      (setq-local mevedel-directive-activity--workspace 'workspace
                  mevedel-directive-activity--directive record)
      (let ((inhibit-read-only t))
        (insert "attempt")
        (put-text-property (point-min) (point-max)
                           'mevedel-view-zone-entry attempt))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                 (lambda (_) nil))
                ((symbol-function
                  'mevedel-session-persistence-rewind-checkpoint)
                 (lambda (&rest args) (setq captured args))))
        (mevedel-directive-activity-rewind))
      (should (equal (list 'workspace '(:session-id "main" :turn 2) nil)
                     captured)))))

(mevedel-deftest mevedel-directive-activity-goto-source
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "visits the live attached source overlay"
  (let* ((fixture (mevedel-directive-activity-test--fixture)) visited)
    (unwind-protect
        (with-temp-buffer
          (mevedel-directive-activity-mode)
          (setq-local mevedel-directive-activity--workspace (nth 0 fixture)
                      mevedel-directive-activity--directive (nth 3 fixture))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _)
                       (setq visited buffer)
                       (set-buffer buffer))))
            (mevedel-directive-activity-goto-source))
          (should (eq (nth 1 fixture) visited))
          (with-current-buffer visited
            (should (= (point-min) (point)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-discuss
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "dispatches selected-attempt discussion into the shared composer"
  (let* ((fixture (mevedel-directive-activity-test--fixture))
         (record (nth 3 fixture))
         (attempt (mevedel-directive-activity-test--attempt))
         captured)
    (setf (mevedel-directive-attempts record) (list attempt)
          (mevedel-directive-state record) 'implemented)
    (unwind-protect
        (with-temp-buffer
          (mevedel-directive-activity-mode)
          (setq-local mevedel-directive-activity--workspace (nth 0 fixture)
                      mevedel-directive-activity--directive record)
          (let ((inhibit-read-only t))
            (insert "attempt")
            (put-text-property (point-min) (point-max)
                               'mevedel-view-zone-entry attempt))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'mevedel-view-enter-directive-scope)
                     (lambda (&rest args) (setq captured args))))
            (mevedel-directive-activity-discuss))
          (should (eq record (nth 0 captured)))
          (should (eq 'discuss (nth 1 captured)))
          (should (= 1 (nth 2 captured))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-view-patch
  ()
  ,test
  (test)
  :doc "opens the selected immutable attempt patch"
  (let* ((attempt (mevedel-directive-activity-test--attempt))
         (record (mevedel-directive--create :attempts (list attempt)))
         captured)
    (with-temp-buffer
      (mevedel-directive-activity-mode)
      (setq-local mevedel-directive-activity--directive record)
      (let ((inhibit-read-only t))
        (insert "attempt")
        (put-text-property (point-min) (point-max)
                           'mevedel-view-zone-entry attempt))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'mevedel--replace-patch-buffer)
                 (lambda (patch) (setq captured patch))))
        (mevedel-directive-activity-view-patch))
      (should (string-match-p "diff --git" captured)))))

(provide 'test-mevedel-directive-activity)

;;; test-mevedel-directive-activity.el ends here
