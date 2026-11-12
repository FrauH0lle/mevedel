;;; test-mevedel-overlay-ui.el -- Instruction overlay UI tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for instruction labels, actions, styling, and redraw.

;;; Code:

(require 'mevedel-overlay-ui)
(require 'mevedel-directive-source)
(require 'mevedel-overlays)
(require 'mevedel-notes)
(require 'mevedel-instruction-registry)
(require 'mevedel-chat)
(require 'mevedel-menu)
(require 'mevedel-models)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'mevedel-directive)
(require 'mevedel-workspace)

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

(mevedel-deftest mevedel-overlay-ui-directive-action-label ()
  ,test
  (test)
  :doc "labels known actions and normalizes an unknown action"
  (should (equal "Request changes"
                 (mevedel-overlay-ui-directive-action-label
                  'request-changes)))
  (should (equal "Custom Action"
                 (mevedel-overlay-ui-directive-action-label
                  "custom_action"))))

(mevedel-deftest mevedel--instruction-directive-color ()
  ,test
  (test)
  :doc "inherits implementing and failure colors from the top directive"
  (with-temp-buffer
    (insert "directive")
    (let ((parent (make-overlay 1 5))
          (child (make-overlay 2 4))
          (parent-status 'implementing)
          (mevedel-directive-processing-color "processing")
          (mevedel-directive-fail-color "failed")
          (mevedel-directive-success-color "succeeded"))
      (cl-letf (((symbol-function 'mevedel--topmost-instruction)
                 (lambda (&rest _) parent))
                ((symbol-function 'mevedel--directive-status)
                 (lambda (instruction)
                   (if (eq instruction parent)
                       parent-status
                     'implemented))))
        (should (equal "processing"
                       (mevedel--instruction-directive-color child)))
        (setq parent-status 'failed)
        (should (equal "failed"
                       (mevedel--instruction-directive-color child)))))))

(mevedel-deftest mevedel--instruction-action-setup ()
  ,test
  (test)
  :doc "selects status-specific directive interactions"
  (with-temp-buffer
    (insert "directive")
    (let ((instruction (make-overlay 1 5)))
      (cl-letf (((symbol-function 'mevedel--directive-status)
                 (lambda (_) 'failed)))
        (mevedel--instruction-action-setup instruction 'directive))
      (should (eq (overlay-get instruction 'keymap)
                  mevedel-directive-failed-actions-map))
      (should (string-match-p
               "Request failed"
               (overlay-get instruction 'help-echo))))))

(mevedel-deftest mevedel--instruction-directive-typename ()
  ,test
  (test)
  :doc "derives child directive names without mutating overlays"
  (with-temp-buffer
    (insert "abc")
    (let ((parent (make-overlay 1 3))
          (child (make-overlay 1 2)))
      (overlay-put parent 'mevedel-instruction-type 'directive)
      (cl-letf (((symbol-function 'mevedel--directive-status)
                 (lambda (_) 'implemented)))
        (should (equal "CORRECTION"
                       (mevedel--instruction-directive-typename
                        child parent))))
      (should-not (overlay-get child 'mevedel-subdirective-typename)))))

(mevedel-deftest mevedel--instruction-label ()
  ,test
  (test)
  :doc "distinguishes a whole-buffer directive in its primary label"
  (with-temp-buffer
    (insert "directive")
    (let ((directive (make-overlay (point-min) (point-max))))
      (overlay-put directive 'mevedel-id 2)
      (overlay-put directive 'mevedel-instruction-type 'directive)
      (cl-letf (((symbol-function 'mevedel--directive-status)
                 (lambda (_) nil))
                ((symbol-function 'mevedel--instruction-directive-color)
                 (lambda (_) "directive-color"))
                ((symbol-function 'mevedel--directive-text)
                 (lambda (_) "Update workflow"))
                ((symbol-function 'mevedel--directive-record)
                 (lambda (_) nil))
                ((symbol-function 'mevedel--parent-instruction)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel--topmost-instruction)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel--detached-directive-p)
                 (lambda (_) nil)))
        (let ((presentation
               (mevedel--instruction-label
                (list :instruction directive :type 'directive
                      :directive-typename "DIRECTIVE"
                      :padding "" :bufferlevel-p t))))
          (should
           (string-prefix-p
            "BUFFER DIRECTIVE #2: Update workflow"
            (substring-no-properties (plist-get presentation :label))))))))

  :doc "shows Ready with a request-changed qualifier after an authored edit"
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace (mevedel-workspace--create
                       :type 'file :id "edited" :root "/tmp" :name "edited"))
           (record
            (mevedel-directive--create
             :id "directive" :request "Edited request"
             :anchor '(:state attached) :state nil
             :attempts
             (list
              (mevedel-directive-attempt--create
               :directive-request "Original request" :request "Exact"
               :result "Done" :outcome 'success :patch ""
               :capture 'complete :captured-at "2026-07-01T10:00:00+0200"))))
           (directive (make-overlay (point-min) (point-max))))
      (mevedel-workspace-add-directive workspace record)
      (setq-local mevedel--workspace workspace)
      (overlay-put directive 'mevedel-uuid "directive")
      (overlay-put directive 'mevedel-id 1)
      (overlay-put directive 'mevedel-instruction-type 'directive)
      (let ((presentation
             (mevedel--instruction-label
              (list :instruction directive :type 'directive :padding ""))))
        (should
         (string-match-p
          "READY.*REQUEST CHANGED"
          (substring-no-properties (plist-get presentation :label))))))))

(mevedel-deftest mevedel--instruction-style ()
  ,test
  (test)
  :doc "stores presentation and uses the supplied parent for indentation"
  (with-temp-buffer
    (insert "abc")
    (let ((instruction (make-overlay 1 2))
          (parent (make-overlay 1 3)))
      (overlay-put parent 'mevedel-label-color "parent-label")
      (overlay-put parent 'mevedel-bg-color "parent-bg")
      (cl-letf (((symbol-function 'face-foreground)
                 (lambda (&rest _) "foreground"))
                ((symbol-function 'face-background)
                 (lambda (&rest _) "background"))
                ((symbol-function 'mevedel--tint)
                 (lambda (_source tint &optional _intensity) tint))
                ((symbol-function 'mevedel--parent-instruction)
                 (lambda (&rest _)
                   (ert-fail "Style recomputed the parent"))))
        (mevedel--instruction-style
         (list :instruction instruction
               :type 'directive
               :label "DIRECTIVE #1\n child"
               :color "directive"
               :padding " "
               :priority mevedel--default-instruction-priority
               :parent parent)))
      (should (= (overlay-get instruction 'priority)
                 mevedel--default-instruction-priority))
      (should (string-match-p
               "DIRECTIVE #1"
               (substring-no-properties
                (overlay-get instruction 'before-string))))
      (should (overlay-get instruction 'mevedel-bg-color))
      (should (overlay-get instruction 'face)))))

(mevedel-deftest mevedel--update-instruction-overlay-tree ()
  ,test
  (test)
  :doc "renders priorities and clears stale names under mixed-type parents"
  (with-temp-buffer
    (insert "abcdef")
    (let ((parent (make-overlay 1 6))
          (child (make-overlay 2 4))
          rendered)
      (overlay-put parent 'mevedel-instruction-type 'note)
      (overlay-put child 'mevedel-instruction-type 'directive)
      (overlay-put child 'mevedel-subdirective-typename "STALE")
      (cl-letf (((symbol-function 'mevedel-notes--render)
                 (lambda (note) (push (cons note (overlay-get note 'priority)) rendered)))
                ((symbol-function 'mevedel--instruction-bufferlevel-p)
                 (lambda (_instruction) nil))
                ((symbol-function 'mevedel--instruction-label)
                 (lambda (presentation)
                   (append presentation
                           '(:label "label" :color "color"))))
                ((symbol-function 'mevedel--instruction-action-setup)
                 #'ignore)
                ((symbol-function 'mevedel--instruction-style)
                 (lambda (presentation)
                   (push
                    (cons (plist-get presentation :instruction)
                          (plist-get presentation :priority))
                    rendered)))
                ((symbol-function 'mevedel--child-instructions)
                 (lambda (instruction)
                   (and (eq instruction parent) (list child)))))
        (mevedel--update-instruction-overlay-tree
         parent t mevedel--default-instruction-priority nil))
      (should
       (equal (mapcar #'cdr (nreverse rendered))
              (list mevedel--default-instruction-priority
                    (1+ mevedel--default-instruction-priority))))
      (should-not
       (overlay-get child 'mevedel-subdirective-typename)))))

(mevedel-deftest mevedel--update-instruction-overlay
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "renders coextensive instructions without deleting either identity"
  (with-temp-buffer
    (insert "abc")
    (let ((instruction (mevedel-instruction-test--create-note-in (current-buffer) 1 2))
          (other (mevedel-instruction-test--create-note-in (current-buffer) 1 2)))
      (mevedel--update-instruction-overlay instruction t)
      (mevedel--update-instruction-overlay other t)
      (should (overlay-buffer instruction))
      (should (overlay-buffer other))
      (should (= 2 (length (mevedel--instructions-in 1 2))))))
  :doc "renders a non-conflicting root at the default priority"
  (with-temp-buffer
    (insert "abc")
    (let ((instruction (make-overlay 1 2))
          rendered)
      (cl-letf (((symbol-function 'mevedel--instructions-at)
                 (lambda (_point) (list instruction)))
                ((symbol-function 'mevedel--parent-instruction)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel--update-instruction-overlay-tree)
                 (lambda (value children priority parent)
                   (setq rendered
                         (list value children priority parent)))))
        (mevedel--update-instruction-overlay instruction t))
      (should
       (equal rendered
              (list instruction t
                    mevedel--default-instruction-priority nil))))))

(provide 'test-mevedel-overlay-ui)
;;; test-mevedel-overlay-ui.el ends here
