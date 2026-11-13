;;; test-mevedel-overlay-ui.el -- Instruction overlay UI tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for instruction overlay redraw.

;;; Code:

(require 'mevedel-overlay-ui)
(require 'mevedel-note-source)
(require 'mevedel-overlays)
(require 'mevedel-notes)
(require 'mevedel-instruction-registry)
(require 'mevedel-chat)
(require 'mevedel-menu)
(require 'mevedel-models)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
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

(mevedel-deftest mevedel--update-instruction-overlay-tree ()
  ,test
  (test)
  :doc "renders children one priority above their parent"
  (with-temp-buffer
    (insert "abcdef")
    (let ((parent (make-overlay 1 6))
          (child (make-overlay 2 4))
          rendered)
      (dolist (overlay (list parent child))
        (overlay-put overlay 'mevedel-instruction-type 'note))
      (cl-letf (((symbol-function 'mevedel-notes--render)
                 (lambda (note) (push (cons note (overlay-get note 'priority)) rendered)))
                ((symbol-function 'mevedel--child-instructions)
                 (lambda (instruction)
                   (and (eq instruction parent) (list child)))))
        (mevedel--update-instruction-overlay-tree
         parent t mevedel--default-instruction-priority nil))
      (should
       (equal (nreverse rendered)
              (list (cons parent mevedel--default-instruction-priority)
                    (cons child (1+ mevedel--default-instruction-priority))))))))

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
