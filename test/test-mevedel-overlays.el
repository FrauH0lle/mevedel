;;; test-mevedel-overlays.el -- Overlay containment and context tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for containment, prompt, context, and tag behavior.

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-instruction-registry)
(require 'mevedel-directive-source)
(require 'mevedel-overlay-ui)
(require 'mevedel-persistence)
(require 'mevedel-chat)
(require 'mevedel-structs)
(require 'mevedel-directive)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(require 'mevedel-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-instruction-test-support"))

(mevedel-deftest mevedel-preview-directive-prompt ()
  ,test
  (test)
  :doc "previews the planning request when Plan before implementation is on"
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace
            (mevedel-workspace--create
             :type 'file :id "preview-plan" :root "/tmp"
             :name "preview-plan"))
           (mevedel--workspace workspace)
           (directive
            (mevedel--create-directive-in
             (current-buffer) (point-min) (point-max) nil "Test"))
           (record (mevedel--directive-record directive)))
      (setf (mevedel-directive-planning-enabled record) t)
      (goto-char (point-min))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "implement")))
              (mevedel-preview-directive-prompt))
            (with-current-buffer "*mevedel-directive-preview*"
              (should (string-search "Create a concrete implementation plan"
                                     (buffer-string)))
              (should (string-search "## TASK: Implement"
                                     (buffer-string)))))
        (when-let* ((buffer (get-buffer "*mevedel-directive-preview*")))
          (kill-buffer buffer)))))

  :doc "previews the read-only discussion request without a planning wrap"
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace
            (mevedel-workspace--create
             :type 'file :id "preview-discuss" :root "/tmp"
             :name "preview-discuss"))
           (mevedel--workspace workspace)
           (directive
            (mevedel--create-directive-in
             (current-buffer) (point-min) (point-max) nil "Test"))
           (record (mevedel--directive-record directive)))
      (setf (mevedel-directive-planning-enabled record) t)
      (goto-char (point-min))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "discuss")))
              (mevedel-preview-directive-prompt))
            (with-current-buffer "*mevedel-directive-preview*"
              (should (string-search "## TASK: Answer the following request."
                                     (buffer-string)))
              (should-not
               (string-search "Create a concrete implementation plan"
                              (buffer-string)))))
        (when-let* ((buffer (get-buffer "*mevedel-directive-preview*")))
          (kill-buffer buffer))))))

(mevedel-deftest mevedel-get-directive-patch ()
  ,test
  (test)
  :doc "reads patch history from the latest workspace-owned attempt"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "patch" :root "/tmp" :name "patch"))
         (record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :anchor '(:state attached) :state 'implemented
                  :attempts
                  (list
                   (mevedel-directive-attempt--create :patch "first")
                   (mevedel-directive-attempt--create :patch "latest"))))
         (buffer (generate-new-buffer " *directive-patch-owner*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "source")
          (setq-local mevedel--workspace workspace)
          (mevedel-workspace-add-directive workspace record)
          (let ((directive (make-overlay (point-min) (point-max))))
            (overlay-put directive 'mevedel-uuid "directive")
            (overlay-put directive 'mevedel-instruction-type 'directive)
            (should (equal "latest"
                           (mevedel-get-directive-patch directive)))
            (setf (mevedel-directive-attempt-patch
                   (car (last (mevedel-directive-attempts record))))
                  "")
            (should-not (mevedel-get-directive-patch directive))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel--filter-references
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "filters only references belonging to the explicit workspace"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'file :id "filter-a" :root "/tmp" :name "filter-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'file :id "filter-b" :root "/tmp" :name "filter-b"))
         (first (mevedel-instruction-test--make-reference "first\n" workspace-a))
         (second (mevedel-instruction-test--make-reference "second\n" workspace-b)))
    (overlay-put (cdr first) 'mevedel-reference-tags '(shared))
    (overlay-put (cdr second) 'mevedel-reference-tags '(shared))
    (unwind-protect
        (progn
          (should (equal (list (cdr first))
                         (mevedel--filter-references 'shared workspace-a)))
          (should (equal (list (cdr second))
                         (mevedel--filter-references 'shared workspace-b))))
      (mevedel-instruction-test--discard first)
      (mevedel-instruction-test--discard second))))

(mevedel-deftest mevedel--directive-action-context
  (:vars
   ((gptel-default-mode 'markdown-mode)
    (mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "uses real prompt construction for attached eligibility"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (overlay (nth 4 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (let ((context (mevedel--directive-action-context record workspace)))
          (should (eq overlay (plist-get context :directive)))
          (should (string-match-p "Preserve this"
                                  (plist-get context :prompt))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "rejects region-backed source-missing submission with one shared error"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (mevedel--mark-buffer-source-missing buffer))
          (should-error
           (mevedel--directive-action-context record workspace)
           :type 'user-error))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "reconstructs a top-level bodyless directive without source text"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         record context transient)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq record
                  (mevedel--directive-record
                   (mevedel--create-directive-in
                    buffer (point-max) (point-max) t "Bodyless request")))
            (mevedel--mark-buffer-source-missing buffer))
          (setq context
                (mevedel--directive-action-context record workspace)
                transient (plist-get context :directive))
          (should (overlay-get transient 'mevedel-transient-source-missing))
          (should (string-match-p "Bodyless request"
                                  (plist-get context :prompt)))
          (should (eq 'source-missing
                      (plist-get (mevedel-directive-anchor record) :state))))
      (when (and transient (overlay-buffer transient))
        (kill-buffer (overlay-buffer transient)))
      (mevedel-instruction-test--discard-source fixture))))

(provide 'test-mevedel-overlays)
;;; test-mevedel-overlays.el ends here
