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

(mevedel-deftest mevedel-tool-ask--render ()
  ,test
  (test)
  :doc "renders each question container with its count and result status"
  (dolist (case '(([(:question "A?")] "1 question")
                  (((:question "A?") (:question "B?")) "2 questions")
                  (malformed "1 question")
                  (nil "0 questions")))
    (dolist (result '("answers" "Error: unavailable"))
      (should
       (equal (list :header (concat "Ask: " (cadr case))
                    :body result :body-mode nil
                    :status (if (equal result "answers") nil 'error)
                    :initially-collapsed-p t)
              (mevedel-tool-ask--render
               nil (list :questions (car case)) result nil)))))

  :doc "uses the supplied tool name and ignores non-string results"
  (progn
    (should (equal "Questionnaire: 2 questions"
                   (plist-get (mevedel-tool-ask--render
                               "Questionnaire" '(:questions [a b]) "answers" nil)
                              :header)))
    (should-not (mevedel-tool-ask--render
                 "Ask" nil '(:result "done") nil))))

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
      (should (equal '((success . mevedel-tool-ask--render))
                     (mevedel-tool-renderer tool))))))

(provide 'test-mevedel-tool-ask)
;;; test-mevedel-tool-ask.el ends here
