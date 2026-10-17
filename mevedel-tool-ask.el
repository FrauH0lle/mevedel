;;; mevedel-tool-ask.el -- Ask interaction tool -*- lexical-binding: t -*-

;;; Commentary:

;; Ask tool result adaptation, rendering, and registration.  The interactive
;; questionnaire lives in mevedel-tool-ask-ui.el.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry)
  (require 'subr-x))
(require 'seq)

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool
                  "mevedel-pipeline" (tool callback args))

;; `mevedel-tool-ask-ui'
(declare-function mevedel-tool-ask-ui-show
                  "mevedel-tool-ask-ui" (callback questions))
(autoload 'mevedel-tool-ask-ui-show "mevedel-tool-ask-ui")

;; `mevedel-tool-registry'
(declare-function mevedel-tool--positional-to-plist
                  "mevedel-tool-registry" (raw-args specs))
(declare-function mevedel-tool--resolve-prompt
                  "mevedel-tool-registry" (prompt))
(declare-function mevedel-tool-register "mevedel-tool-registry" (tool))


;;
;;; Handler

(defun mevedel-tool-ask--validate (questions)
  "Signal an error naming the first malformed entry of QUESTIONS.
A malformed question must fail back to the model rather than reach the
user as an empty or `nil' form."
  (let ((index 0))
    (seq-doseq (question questions)
      (setq index (1+ index))
      (let ((text (and (listp question) (plist-get question :question)))
            (options (and (listp question) (plist-get question :options))))
        (unless (and (stringp text) (not (string-blank-p text)))
          (error "Ask question %d needs a non-empty question string; got %S"
                 index question))
        (unless (and (sequencep options) (not (stringp options))
                     (> (length options) 0))
          (error "Ask question %d (%s) needs a non-empty options list" index text))
        (seq-doseq (option options)
          (unless (or (and (stringp option) (not (string-blank-p option)))
                      (and (listp option)
                           (stringp (plist-get option :label))
                           (not (string-blank-p (plist-get option :label)))))
            (error "Ask question %d (%s): each option must be a string or an \
object with a non-empty label; got %S" index text option)))))))

(defun mevedel-tool-ask--ask (callback args)
  "Ask the user questions.
CALLBACK receives the formatted answers.  ARGS is a plist with :questions."
  (let ((questions (plist-get args :questions)))
    (unless questions
      (error "Parameter questions is required"))
    (mevedel-tool-ask--validate questions)
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

(defun mevedel-tool-ask--render (name args result _render-data)
  "Return rendering plist for Ask NAME, ARGS, and RESULT."
  (when (stringp result)
    (let* ((questions (plist-get args :questions))
           (count (if (or (vectorp questions) (listp questions))
                      (length questions)
                    1)))
      (list :header (format "%s: %d %s"
                            (or name "Ask")
                            count
                            (if (= count 1) "question" "questions"))
            :body result
            :body-mode nil
            :status (and (string-prefix-p "Error:" result) 'error)
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
                      :items (:type object
                              :properties
                              (:question (:type string :description "The question to ask.")
                               :options
                               (:type array
                                :description "Predefined answers: strings, or objects with label, description and sample."
                                :items (:properties
                                        (:label (:type string)
                                         :description (:type string)
                                         :sample (:type string))))))
                      :minItems 1))
    :async-p t
    :max-result-size 30000
    :read-only-p t
    :groups (util)
    :renderer '((success . mevedel-tool-ask--render))))

(provide 'mevedel-tool-ask)
;;; mevedel-tool-ask.el ends here
