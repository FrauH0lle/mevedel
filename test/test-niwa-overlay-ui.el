;;; test-niwa-overlay-ui.el -- Instruction overlay UI tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for instruction overlay redraw.

;;; Code:

(require 'niwa-overlay-ui)
(require 'niwa-note-source)
(require 'niwa-overlays)
(require 'niwa-notes)
(require 'niwa-instruction-registry)
(require 'niwa-chat)
(require 'niwa-menu)
(require 'niwa-models)
(require 'niwa-skills-ui)
(require 'niwa-structs)
(require 'niwa-workspace)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))

(niwa-deftest niwa--update-instruction-overlay-tree ()
  ,test
  (test)
  :doc "renders children one priority above their parent"
  (with-temp-buffer
    (insert "abcdef")
    (let ((parent (make-overlay 1 6))
          (child (make-overlay 2 4))
          rendered)
      (dolist (overlay (list parent child))
        (overlay-put overlay 'niwa-instruction-type 'note))
      (cl-letf (((symbol-function 'niwa-notes--render)
                 (lambda (note) (push (cons note (overlay-get note 'priority)) rendered)))
                ((symbol-function 'niwa--child-instructions)
                 (lambda (instruction)
                   (and (eq instruction parent) (list child)))))
        (niwa--update-instruction-overlay-tree
         parent t niwa--default-instruction-priority nil))
      (should
       (equal (nreverse rendered)
              (list (cons parent niwa--default-instruction-priority)
                    (cons child (1+ niwa--default-instruction-priority))))))))

(niwa-deftest niwa--update-instruction-overlay
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "renders coextensive instructions without deleting either identity"
  (with-temp-buffer
    (insert "abc")
    (let ((instruction (niwa-instruction-test--create-note-in (current-buffer) 1 2))
          (other (niwa-instruction-test--create-note-in (current-buffer) 1 2)))
      (niwa--update-instruction-overlay instruction t)
      (niwa--update-instruction-overlay other t)
      (should (overlay-buffer instruction))
      (should (overlay-buffer other))
      (should (= 2 (length (niwa--instructions-in 1 2))))))
  :doc "renders a non-conflicting root at the default priority"
  (with-temp-buffer
    (insert "abc")
    (let ((instruction (make-overlay 1 2))
          rendered)
      (cl-letf (((symbol-function 'niwa--instructions-at)
                 (lambda (_point) (list instruction)))
                ((symbol-function 'niwa--parent-instruction)
                 (lambda (&rest _) nil))
                ((symbol-function 'niwa--update-instruction-overlay-tree)
                 (lambda (value children priority parent)
                   (setq rendered
                         (list value children priority parent)))))
        (niwa--update-instruction-overlay instruction t))
      (should
       (equal rendered
              (list instruction t
                    niwa--default-instruction-priority nil))))))

(provide 'test-niwa-overlay-ui)
;;; test-niwa-overlay-ui.el ends here
