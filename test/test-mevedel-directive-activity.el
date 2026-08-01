;;; test-mevedel-directive-activity.el --- Directive activity tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for workspace-owned directive activity views.

;;; Code:

(require 'mevedel-directive-activity)
(require 'mevedel-overlays)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-directive-activity-test--make-directive (request)
  "Return a workspace, source buffer, and directive for REQUEST."
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "activity" :root "/tmp"
                     :name "activity"))
         (buffer (generate-new-buffer " *directive-activity-source*")))
    (with-current-buffer buffer
      (insert "source")
      (setq-local mevedel--workspace workspace)
      (list workspace buffer
            (mevedel--create-directive-in
             buffer (point-min) (point-max) nil request)))))

(defun mevedel-directive-activity-test--discard (fixture)
  "Discard source and activity buffers belonging to FIXTURE."
  (when-let* ((buffer (cadr fixture))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq-local kill-buffer-hook nil))
    (kill-buffer buffer))
  (when-let* ((workspace (car fixture)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (eq workspace
                     (buffer-local-value
                      'mevedel-directive-activity--workspace buffer)))
        (kill-buffer buffer)))))


;;
;;; Activity surface

(mevedel-deftest mevedel-open-directive-activity
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "opens one managed activity buffer from an attached source directive"
  (let* ((fixture (mevedel-directive-activity-test--make-directive
                   "Initial request"))
         (workspace (car fixture))
         (directive (caddr fixture))
         (record (car (mevedel-workspace-directives workspace))))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer)))
          (let ((activity (mevedel-open-directive-activity directive)))
            (should (buffer-live-p activity))
            (with-current-buffer activity
              (should (derived-mode-p 'mevedel-directive-activity-mode))
              (should (eq workspace mevedel-directive-activity--workspace))
              (should (eq record mevedel-directive-activity--directive))
              (should (string-match-p "Initial request" (buffer-string)))
              (should (string-match-p "Ready" (buffer-string)))
              (should (string-match-p "Attached" (buffer-string)))
              (should (string-match-p "No activity yet" (buffer-string))))
            (should-not
             (string-match-p
              "No activity yet"
              (substring-no-properties
               (overlay-get directive 'before-string))))))
      (mevedel-directive-activity-test--discard fixture)))

  :doc "reuses the activity buffer and its workspace record"
  (let* ((fixture (mevedel-directive-activity-test--make-directive "One"))
         (directive (caddr fixture)))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer)))
          (let ((first (mevedel-open-directive-activity directive))
                (second (mevedel-open-directive-activity directive)))
            (should (eq first second))
            (should
             (eq (buffer-local-value
                  'mevedel-directive-activity--directive first)
                 (buffer-local-value
                  'mevedel-directive-activity--directive second)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-refresh
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global))
   :doc "rerenders the current workspace record after request edits")
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Before"))
         (workspace (car fixture))
         (record (car (mevedel-workspace-directives workspace)))
         activity)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) buffer)))
          (setq activity
                (mevedel-open-directive-activity (caddr fixture)))
          (mevedel-directive-set-request record "After")
          (with-current-buffer activity
            (mevedel-directive-activity-refresh)
            (should (string-match-p "After" (buffer-string)))
            (should-not (string-match-p "Before" (buffer-string)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-view-patch
  ()
  ,test
  (test)
  :doc "projects the selected attempt patch into the reusable patch viewer"
  (let ((attempt
         (mevedel-directive-attempt--create
          :request "Request" :result "Answer" :outcome 'success
          :patch "diff --git a/a b/a\n" :capture 'complete
          :covered-files '("/tmp/a") :gaps nil
          :checkpoint '(:session-id "session-1" :turn 1)))
        projected)
    (with-temp-buffer
      (insert (propertize "Attempt" 'mevedel-directive-attempt attempt))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'mevedel--replace-patch-buffer)
                 (lambda (patch) (setq projected patch))))
        (mevedel-directive-activity-view-patch)
        (should (equal "diff --git a/a b/a\n" projected)))))

  :doc "renders immutable attempt details and complete no-change capture"
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Current"))
         (workspace (car fixture))
         (record (car (mevedel-workspace-directives workspace)))
         activity)
    (unwind-protect
        (progn
          (setf (mevedel-directive-state record) 'implemented
                (mevedel-directive-session-id record) "session-1"
                (mevedel-directive-attempts record)
                (list
                 (mevedel-directive-attempt--create
                  :request "Exact submitted request"
                  :result "Exact answer"
                  :outcome 'success :patch "" :capture 'complete
                  :covered-files '("/tmp/a") :gaps nil
                  :checkpoint '(:session-id "session-1" :turn 2))))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) buffer)))
            (setq activity
                  (mevedel-open-directive-activity (caddr fixture))))
          (with-current-buffer activity
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p "Implemented" text))
              (should (string-match-p "Exact submitted request" text))
              (should (string-match-p "Exact answer" text))
              (should (string-match-p "Complete capture; no changes" text))
              (should (string-match-p "session-1, turn 2" text)))))
      (mevedel-directive-activity-test--discard fixture))))

(mevedel-deftest mevedel-list-directives
  (:doc "opens the selected workspace record without a source point")
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "list" :root "/tmp" :name "list"))
         (record (mevedel-directive--create
                  :id "directive-1" :request "Choose me"
                  :anchor '(:state detached) :state nil))
         selected)
    (mevedel-workspace-add-directive workspace record)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (caar collection)))
              ((symbol-function 'mevedel-open-directive-activity)
               (lambda (directive selected-workspace)
                 (setq selected (list directive selected-workspace)))))
      (mevedel-list-directives workspace)
      (should (equal (list record workspace) selected)))))

(mevedel-deftest mevedel-directive-activity-goto-source
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "navigates from activity to the live attached source anchor"
  (let* ((fixture (mevedel-directive-activity-test--make-directive "Go"))
         (source (cadr fixture))
         (directive (caddr fixture))
         activity)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (set-buffer buffer)
                     buffer)))
          (setq activity (mevedel-open-directive-activity directive))
          (save-current-buffer
            (set-buffer activity)
            (mevedel-directive-activity-goto-source)
            (should (eq source (current-buffer)))
            (should (= (point) (overlay-start directive)))))
      (mevedel-directive-activity-test--discard fixture)))

  :doc "rejects navigation when no attached source is live"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "missing" :root "/tmp" :name "missing"))
         (record (mevedel-directive--create
                  :id "gone" :request "Gone"
                  :anchor '(:state detached) :state nil)))
    (with-temp-buffer
      (mevedel-directive-activity-mode)
      (setq-local mevedel-directive-activity--workspace workspace
                  mevedel-directive-activity--directive record)
      (should-error (mevedel-directive-activity-goto-source)
                    :type 'user-error))))

(provide 'test-mevedel-directive-activity)
;;; test-mevedel-directive-activity.el ends here
