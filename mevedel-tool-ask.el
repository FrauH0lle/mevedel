;;; mevedel-tool-ask.el -- Ask interaction tool -*- lexical-binding: t -*-

;;; Commentary:

;; Ask tool result adaptation, rendering, and registration.  The interactive
;; questionnaire lives in mevedel-tool-ask-ui.el.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry))

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--positional-to-plist
                  "mevedel-pipeline" (raw-args specs))
(declare-function mevedel-pipeline-run-tool
                  "mevedel-pipeline" (tool callback args))

;; `mevedel-tool-ask-ui'
(declare-function mevedel-tool-ask-ui-show
                  "mevedel-tool-ask-ui" (callback questions))
(autoload 'mevedel-tool-ask-ui-show "mevedel-tool-ask-ui")

;; `mevedel-tool-registry'
(declare-function mevedel-tool--resolve-prompt
                  "mevedel-tool-registry" (prompt))
(declare-function mevedel-tool-register "mevedel-tool-registry" (tool))


;;
;;; Handler

(defun mevedel-tool-ask--ask (callback args)
  "Ask the user questions.
CALLBACK receives the formatted answers.  ARGS is a plist with :questions."
  (let ((questions (plist-get args :questions)))
    (unless questions
      (error "Parameter questions is required"))
    (mevedel-tool-ask-ui-show
     (lambda (value)
       (funcall callback
                (if (stringp value)
                    (list :result value)
                  (list
                   :result
                   "Error: The questionnaire was cancelled before an answer was submitted"
                   :status 'error))))
     questions)))


;;
;;; Renderer

(defun mevedel-tool-ask--question-count (questions)
  "Return the number of QUESTIONS in an Ask call."
  (cond
   ((vectorp questions) (length questions))
   ((listp questions) (length questions))
   (questions 1)
   (t 0)))

(defun mevedel-tool-ask--result-status (result)
  "Return a renderer status for RESULT."
  (and (stringp result)
       (string-prefix-p "Error:" result)
       'error))

(defun mevedel-tool-ask--render (name args result _render-data)
  "Return rendering plist for Ask NAME, ARGS, and RESULT."
  (when (stringp result)
    (let ((count (mevedel-tool-ask--question-count
                  (plist-get args :questions))))
      (list :header (format "%s: %d %s"
                            (or name "Ask")
                            count
                            (if (= count 1) "question" "questions"))
            :body result
            :body-mode nil
            :status (mevedel-tool-ask--result-status result)
            :initially-collapsed-p t))))


;;
;;; Registration

(defun mevedel-tool-ask-register ()
  "Register the Ask interaction tool."
  (require 'mevedel-tool-registry)
  (mevedel-define-tool
    :name "Ask"
    :description "Ask the user one or more questions and wait for their responses."
    :prompt-file "prompts/tools/ask.md"
    :handler #'mevedel-tool-ask--ask
    :args ((questions array :required
                      "Array of question objects. Each question must have predefined answer options. Options may be strings or objects with label, description, and sample fields. Mark exactly one option per question by appending \` (Recommended)\` to that option label."
                      :items (:type object)
                      :minItems 1))
    :async-p t
    :max-result-size 30000
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-ask--render))

(provide 'mevedel-tool-ask)
;;; mevedel-tool-ask.el ends here
