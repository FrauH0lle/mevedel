;;; test-mevedel-engine.el --- Tests for mevedel-engine.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests turn-engine context shared by native and external engines.

;;; Code:

(require 'mevedel-engine)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(mevedel-deftest mevedel-engine-steerable-p ()
  ,test
  (test)
  :doc "a gptel turn takes steering until its machine terminates"
  (dolist (state '(WAIT TOOL TYPE))
    (should (mevedel-engine-steerable-p (gptel-make-fsm :state state))))
  (dolist (state '(DONE ERRS ABRT))
    (should-not (mevedel-engine-steerable-p (gptel-make-fsm :state state))))
  :doc "an external turn takes steering until it settles or stops at a boundary"
  (let ((request (mevedel-request--create :id "native" :context '(:external-history t))))
    (should (mevedel-engine-steerable-p request))
    (dolist (key '(:mevedel-acp-outcome :mevedel-end-turn))
      (setf (mevedel-request-context request) (list :external-history t key 'stop))
      (should-not (mevedel-engine-steerable-p request))))
  (should-not (mevedel-engine-steerable-p (mevedel-request--create :id "plain"))))

(provide 'test-mevedel-engine)
;;; test-mevedel-engine.el ends here
