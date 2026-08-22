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
(require 'mevedel-directive-request)
(require 'mevedel-structs)
(require 'mevedel-directive)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `gptel'
(defvar gptel-default-mode)

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

(mevedel-deftest mevedel--create-instruction ()
  ,test
  (test)
  :doc "refuses a directive in a buffer that visits no file"
  ;; The durable record is anchored to a file: without one nothing can
  ;; reattach it, and archiving or detaching it writes a record that no
  ;; longer loads.
  (with-temp-buffer
    (insert "source text")
    (should-error (mevedel--create-instruction 'directive)
                  :type 'user-error)))

(mevedel-deftest mevedel-convert-instructions
  (:quiet t
   :vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)

  :doc "converting a directive away gives up its workspace record"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         (overlay (nth 4 fixture))
         (start (overlay-start overlay))
         (end (overlay-end overlay)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (1+ start))
          (mevedel-convert-instructions)
          (should-not (mevedel-workspace-directives workspace))
          (let ((converted (car (mevedel--instructions-at (point)))))
            (should converted)
            (should (mevedel--referencep converted))
            (should (= start (overlay-start converted)))
            (should (= end (overlay-end converted)))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "converting a reference creates a durable directive record"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         (overlay (nth 4 fixture)))
    (unwind-protect
        (with-current-buffer buffer
          ;; Turn the fixture directive into a reference first, then
          ;; convert it back and expect a fresh record.
          (goto-char (1+ (overlay-start overlay)))
          (mevedel-convert-instructions)
          (should-not (mevedel-workspace-directives workspace))
          (mevedel-convert-instructions)
          (let ((converted (car (mevedel--instructions-at (point)))))
            (should converted)
            (should (mevedel--directivep converted))
            (should (mevedel--directive-record converted))
            (should (= 1 (length (mevedel-workspace-directives workspace))))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "a directive with recorded activity is skipped, not converted"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         (overlay (nth 4 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (with-current-buffer buffer
          (setf (mevedel-directive-attempts record)
                (list (mevedel-instruction-test--attempt)))
          (goto-char (1+ (overlay-start overlay)))
          (mevedel-convert-instructions)
          (let ((instruction (car (mevedel--instructions-at (point)))))
            (should (eq instruction overlay))
            (should (mevedel--directivep instruction)))
          (should (= 1 (length (mevedel-workspace-directives workspace)))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "refuses reference conversion in a buffer visiting no file"
  (with-temp-buffer
    (insert "source text")
    (setq-local mevedel--workspace
                (mevedel-workspace--create
                 :type 'file :id "convert-nofile" :root "/tmp"
                 :name "convert-nofile"))
    (let ((reference (mevedel--create-reference-in
                      (current-buffer) (point-min) (point-max))))
      (goto-char (1+ (point-min)))
      (should-error (mevedel-convert-instructions) :type 'user-error)
      (should (mevedel--referencep
               (car (mevedel--instructions-at (point)))))
      (mevedel--delete-instruction reference))))

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
