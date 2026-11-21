;;; test-niwa-tool-ask.el --- Tests for niwa-tool-ask.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the Ask tool handler, result renderer, and registration.

;;; Code:

(require 'niwa-tool-ask)
(require 'niwa-tools)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Handler and renderer

(niwa-deftest niwa-tool-ask--ask ()
  ,test
  (test)
  :doc "validates questions and wraps the questionnaire result"
  (let (delivered)
    (cl-letf (((symbol-function 'niwa-tool-ask-ui-show)
               (lambda (callback questions)
                 (should (vectorp questions))
                 (funcall callback "answers"))))
      (niwa-tool-ask--ask
       (lambda (value) (setq delivered value))
       '(:questions [(:question "Proceed?" :options ["Yes" "No"])])))
    (should (equal '(:result "answers") delivered)))

  :doc "malformed questions fail back to the model instead of showing nil"
  (cl-letf (((symbol-function 'niwa-tool-ask-ui-show)
             (lambda (&rest _) (ert-fail "showed a malformed questionnaire"))))
    (dolist (case '(([(:options ["Yes"])] . "needs a non-empty question")
                    ([(:question "  " :options ["Yes"])] . "needs a non-empty question")
                    ([(:question "Go?")] . "needs a non-empty options")
                    ([(:question "Go?" :options [])] . "needs a non-empty options")
                    ([(:question "Go?" :options [(:description "no label")])] . "non-empty label")
                    ([(:question "Go?" :options [""])] . "non-empty label")))
      (let ((err (should-error (niwa-tool-ask--ask #'ignore (list :questions (car case))))))
        (should (string-search (cdr case) (error-message-string err))))))

  :doc "options may mix strings and labelled objects"
  (let (shown)
    (cl-letf (((symbol-function 'niwa-tool-ask-ui-show)
               (lambda (_callback questions) (setq shown questions))))
      (niwa-tool-ask--ask
       #'ignore
       '(:questions [(:question "Where?" :options ["Here" (:label "There" :description "Far")])]))
      (should shown)))

  :doc "the registered schema validates mixed string and object options"
  (progn
    (require 'niwa-tool-repair)
    (niwa-tool-ask-register)
    (let ((tool (niwa-tool-get "Ask")))
      ;; The live failure was the schema check itself, not the arguments.
      (should-not
       (niwa-tool-repair-validate
        tool '(:questions [(:question "Where?"
                            :options ["Here" (:label "There" :sample "far")])])))))

  :doc "a cancelled questionnaire reports an error result, not success"
  ;; Quit settles with the bare symbol `aborted'; passed through, the
  ;; pipeline recorded the call as a success the renderer showed as
  ;; nothing.
  (let (delivered)
    (cl-letf (((symbol-function 'niwa-tool-ask-ui-show)
               (lambda (callback _questions)
                 (funcall callback 'aborted))))
      (niwa-tool-ask--ask
       (lambda (value) (setq delivered value))
       '(:questions [(:question "Proceed?" :options ["Yes" "No"])])))
    (should (eq 'error (plist-get delivered :status)))
    (should (string-prefix-p "Error:" (plist-get delivered :result))))

  :doc "rejects a missing questions argument"
  (should-error (niwa-tool-ask--ask #'ignore nil) :type 'error)

  :doc "multiSelect is an optional boolean, not a truthy mode switch"
  (progn
    (dolist (value '(t nil :json-false))
      (niwa-tool-ask--validate
       (vector (list :question "Which?" :options ["A"] :multiSelect value))))
    (dolist (value '("true" 1 []))
      (should-error
       (niwa-tool-ask--validate
        (vector (list :question "Which?" :options ["A"] :multiSelect value)))))))

(niwa-deftest niwa-tool-ask--validate ()
  ,test
  (test)
  :doc "option labels must be unique after trimming without mutating model arguments"
  (progn
    (dolist (options '(["A" "A"] [" A " (:label "A")] [(:label "A ") (:label " A")]))
      (should-error (niwa-tool-ask--validate
                     (vector (list :question "Which?" :options options)))))
    (let* ((questions [(:question "Which?" :options [" A " (:label " B ")])])
           (before (copy-tree questions t)))
      (niwa-tool-ask--validate questions)
      (should (equal before questions)))))

(niwa-deftest niwa-tool-ask--render ()
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
              (niwa-tool-ask--render
               nil (list :questions (car case)) result nil)))))

  :doc "uses the supplied tool name and ignores non-string results"
  (progn
    (should (equal "Questionnaire: 2 questions"
                   (plist-get (niwa-tool-ask--render
                               "Questionnaire" '(:questions [a b]) "answers" nil)
                              :header)))
    (should-not (niwa-tool-ask--render
                 "Ask" nil '(:result "done") nil))))

(niwa-deftest niwa-tool-ask-register ()
  ,test
  (test)
  :doc "registers the Ask handler and renderer"
  (progn
    (niwa-tool-ask-register)
    (let ((tool (niwa-tool-get "Ask")))
      (should tool)
      (should (equal '(:type boolean :description "Allow multiple answers; defaults to false.")
                     (plist-get (plist-get (plist-get
                                           (nthcdr 4 (assq 'questions (niwa-tool-args tool)))
                                           :items) :properties) :multiSelect)))
      (should (eq #'niwa-tool-ask--ask
                  (niwa-tool-handler tool)))
      (should (equal '((success . niwa-tool-ask--render))
                     (niwa-tool-renderer tool))))))

(provide 'test-niwa-tool-ask)
;;; test-niwa-tool-ask.el ends here
