;;; test-mevedel-tool-ask.el --- Tests for mevedel-tool-ask.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the Ask tool handler, result renderer, and registration.

;;; Code:

(require 'mevedel-tool-ask)
(require 'mevedel-tools)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Handler and renderer

(mevedel-deftest mevedel-tool-ask--ask ()
  ,test
  (test)
  :doc "validates questions and wraps the questionnaire result"
  (let (delivered)
    (cl-letf (((symbol-function 'mevedel-tool-ask-ui-show)
               (lambda (callback questions)
                 (should (vectorp questions))
                 (funcall callback "answers"))))
      (mevedel-tool-ask--ask
       (lambda (value) (setq delivered value))
       '(:questions [(:question "Proceed?" :options ["Yes" "No"])])))
    (should (equal '(:result "answers") delivered)))

  :doc "a cancelled questionnaire reports an error result, not success"
  ;; Quit settles with the bare symbol `aborted'; passed through, the
  ;; pipeline recorded the call as a success the renderer showed as
  ;; nothing.
  (let (delivered)
    (cl-letf (((symbol-function 'mevedel-tool-ask-ui-show)
               (lambda (callback _questions)
                 (funcall callback 'aborted))))
      (mevedel-tool-ask--ask
       (lambda (value) (setq delivered value))
       '(:questions [(:question "Proceed?" :options ["Yes" "No"])])))
    (should (eq 'error (plist-get delivered :status)))
    (should (string-prefix-p "Error:" (plist-get delivered :result))))

  :doc "rejects a missing questions argument"
  (should-error (mevedel-tool-ask--ask #'ignore nil) :type 'error))

(mevedel-deftest mevedel-tool-ask--question-count ()
  ,test
  (test)
  :doc "counts each supported Ask question container shape"
  (progn
    (should (= 1 (mevedel-tool-ask--question-count
                  [(:question "A?")])))
    (should (= 2 (mevedel-tool-ask--question-count
                  '((:question "A?") (:question "B?")))))
    (should (= 1 (mevedel-tool-ask--question-count 'malformed)))
    (should (= 0 (mevedel-tool-ask--question-count nil)))))

(mevedel-deftest mevedel-tool-ask--result-status ()
  ,test
  (test)
  :doc "marks only string results with an error prefix"
  (progn
    (should (eq 'error
                (mevedel-tool-ask--result-status "Error: unavailable")))
    (should-not (mevedel-tool-ask--result-status "done"))
    (should-not (mevedel-tool-ask--result-status '(:result "done")))))

(mevedel-deftest mevedel-tool-ask--render ()
  ,test
  (test)
  :doc "renders an Ask result with its question count"
  (should
   (equal '(:header "Ask: 2 questions"
            :body "answers"
            :body-mode nil
            :status nil
            :initially-collapsed-p t)
          (mevedel-tool-ask--render
           "Ask"
           '(:questions [(:question "A?") (:question "B?")])
           "answers"
           nil))))

(mevedel-deftest mevedel-tool-ask-register ()
  ,test
  (test)
  :doc "registers the Ask handler and renderer"
  (progn
    (mevedel-tool-ask-register)
    (let ((tool (mevedel-tool-get "Ask")))
      (should tool)
      (should (eq #'mevedel-tool-ask--ask
                  (mevedel-tool-handler tool)))
      (should (eq #'mevedel-tool-ask--render
                  (mevedel-tool-renderer tool))))))

(provide 'test-mevedel-tool-ask)
;;; test-mevedel-tool-ask.el ends here
