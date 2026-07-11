;;; mevedel-tool-ask.el -- Ask interaction tool -*- lexical-binding: t -*-

;;; Commentary:

;; The Ask questionnaire, its renderer, and tool registration.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--data-buffer "mevedel-interaction-prompt"
                  (&optional buffer))
(declare-function mevedel--prompt--register-canceller
                  "mevedel-interaction-prompt"
                  (&optional source-buffer overlay))
(defvar mevedel--prompt-overlays)

;; `mevedel-view'
(declare-function mevedel-view--interaction-register "mevedel-view"
                  (descriptor))
(declare-function mevedel-view--interaction-target-buffer "mevedel-view"
                  (&optional data-buffer))
(declare-function mevedel-view--interaction-unregister "mevedel-view" (id))


;;
;;; Ask User

(defconst mevedel-tools--ask-recommended-suffix " (Recommended)"
  "Suffix marking a recommended Ask option.")

(defun mevedel-tools--ask-option-field (option key)
  "Return OPTION field KEY from supported option object shapes."
  (let ((string-key (substring (symbol-name key) 1)))
    (cond
     ((hash-table-p option)
      (or (gethash key option)
          (gethash string-key option)
          (gethash (intern string-key) option)))
     ((and (listp option) (plist-member option key))
      (plist-get option key))
     ((listp option)
      (or (cdr (assq key option))
          (cdr (assq (intern string-key) option))
          (cdr (assoc string-key option)))))))

(defun mevedel-tools--ask-option-label (option)
  "Return the answer label for OPTION."
  (let ((label (if (stringp option)
                   option
                 (mevedel-tools--ask-option-field option :label))))
    (cond
     ((stringp label) label)
     (label (format "%s" label))
     (t (format "%s" option)))))

(defun mevedel-tools--ask-option-description (option)
  "Return OPTION's description string, or nil."
  (let ((description
         (and (not (stringp option))
              (mevedel-tools--ask-option-field option :description))))
    (cond
     ((and (stringp description)
           (not (string-blank-p description)))
      description)
     (description (format "%s" description)))))

(defun mevedel-tools--ask-option-preview (option)
  "Return OPTION's preview string, or nil."
  (let ((preview (and (not (stringp option))
                      (mevedel-tools--ask-option-field option :preview))))
    (cond
     ((and (stringp preview)
           (not (string-blank-p preview)))
      preview)
     (preview (format "%s" preview)))))

(defun mevedel-tools--ask-option-labels (options)
  "Return display labels for OPTIONS."
  (mapcar #'mevedel-tools--ask-option-label options))

(defun mevedel-tools--ask-completion-table (choices)
  "Return a completion table that preserves CHOICES order."
  (let ((choices (copy-sequence choices)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          '(metadata
            (category . mevedel-ask)
            (display-sort-function . identity)
            (cycle-sort-function . identity))
        (complete-with-action action choices string predicate)))))

(defun mevedel-tools--ask-option-by-label (label options)
  "Return first option in OPTIONS whose label equals LABEL."
  (cl-find label options
           :test #'equal
           :key #'mevedel-tools--ask-option-label))

(defun mevedel-tools--ask-format-option (option)
  "Return OPTION formatted for display in an Ask prompt."
  (let ((label (mevedel-tools--ask-option-label option)))
    (if (string-suffix-p mevedel-tools--ask-recommended-suffix label)
        (let ((base (substring label 0
                               (- (length label)
                                  (length mevedel-tools--ask-recommended-suffix)))))
          (concat base
                  (propertize mevedel-tools--ask-recommended-suffix
                              'font-lock-face 'success)))
      label)))

(defun mevedel-tools--ask-format-option-line (option)
  "Return a rendered option line for OPTION."
  (let ((description (mevedel-tools--ask-option-description option)))
    (concat
     (format "  - %s" (mevedel-tools--ask-format-option option))
     (when description
       (concat "\n    "
               (propertize description 'font-lock-face 'shadow))))))

(defun mevedel-tools--ask-format-preview (preview)
  "Return a rendered PREVIEW block."
  (when (and (stringp preview)
             (not (string-blank-p preview)))
    (concat
     (propertize "Preview:\n" 'font-lock-face 'font-lock-constant-face)
     (mapconcat (lambda (line) (concat "    " line))
                (split-string preview "\n")
                "\n")
     "\n\n")))

(defun mevedel-tools--ask-format-selected-preview (answer options)
  "Return the preview block for ANSWER selected from OPTIONS."
  (when-let* ((option (mevedel-tools--ask-option-by-label answer options))
              (preview (mevedel-tools--ask-option-preview option)))
    (mevedel-tools--ask-format-preview preview)))

(cl-defun mevedel-tools--ask-user (callback questions)
  "Ask user multiple questions with navigation support using overlays.

CALLBACK is the async callback function to call with results.
QUESTIONS is an array of question plists, each with :question and :options keys."
  (mevedel-tools--validate-params callback mevedel-tools--ask-user
    (questions (vectorp . "array")))

          (require 'mevedel-interaction-prompt)

          (let* ((source-buffer (current-buffer))
                 (questions-list (append questions nil)) ; Convert vector to list
                 (answers (make-vector (length questions-list) nil))
                 (chat-buffer
                  (if (fboundp 'mevedel-view--interaction-target-buffer)
                      (mevedel-view--interaction-target-buffer
                       (with-current-buffer source-buffer
                         (mevedel--prompt--data-buffer)))
                    (error "No live view for Ask prompt")))
                 (interaction-id (list :ask (gensym "ask-")))
                 (overlay nil)
                 (current-index 0))

    (cl-labels
        ((answer-question
           ()
           "Prompt user to answer current question."
           (let* ((q (nth current-index questions-list))
                  (question-text (plist-get q :question))
                  (options (append (plist-get q :options) nil))
                  (all-choices (append (mevedel-tools--ask-option-labels
                                        options)
                                       '("Custom input")))
                  (prev-answer (aref answers current-index))
                  (choice (completing-read
                           (format "[Q%d/%d] %s: "
                                   (1+ current-index)
                                   (length questions-list)
                                   question-text)
                           (mevedel-tools--ask-completion-table
                            all-choices)
                           nil nil
                           prev-answer))
                  (answer (if (equal choice "Custom input")
                              (read-string (concat question-text " (custom): ")
                                           prev-answer)
                            choice)))
             (aset answers current-index answer)
             (cycle-forward)))

         (cycle-forward
           ()
           "Cycle to next question or confirmation screen."
           (interactive)
           (if (eq current-index 'confirm)
               ;; From confirm, go to first question
               (progn
                 (setq current-index 0)
                 (update-overlay current-index))
             ;; From a question
             (if (< current-index (1- (length questions-list)))
                 ;; Go to next question
                 (progn
                   (setq current-index (1+ current-index))
                   (update-overlay current-index))
               ;; At last question, go to confirmation
               (progn
                 (setq current-index 'confirm)
                 (show-confirmation)))))

         (cycle-backward
           ()
           "Cycle to previous question or confirmation screen."
           (interactive)
           (if (eq current-index 'confirm)
               ;; From confirm, go to last question
               (progn
                 (setq current-index (1- (length questions-list)))
                 (update-overlay current-index))
             ;; From a question
             (if (> current-index 0)
                 ;; Go to previous question
                 (progn
                   (setq current-index (1- current-index))
                   (update-overlay current-index))
               ;; At first question, go to confirmation
               (progn
                 (setq current-index 'confirm)
                 (show-confirmation)))))

         (edit-answer
           ()
           "Edit current question's answer."
           (interactive)
           (answer-question))

         (confirm-all
           ()
           "Skip to confirmation screen."
           (interactive)
           (setq current-index 'confirm)
           (show-confirmation))

         (quit-questionnaire
           ()
           "Cancel questionnaire and abort execution."
           (interactive)
           (when overlay
             (when (fboundp 'mevedel-view--interaction-unregister)
               (mevedel-view--interaction-unregister interaction-id))
             (delete-overlay overlay))
           (mevedel-abort))  ; Abort entire execution

         (ask-keymap
           (&optional confirm)
           "Return keymap for the Ask prompt.
When CONFIRM is non-nil, bind submit/edit commands for the review screen."
           (let ((keymap (make-sparse-keymap)))
             (define-key keymap (kbd "TAB") #'cycle-forward)
             (define-key keymap (kbd "<tab>") #'cycle-forward)
             (define-key keymap (kbd "S-TAB") #'cycle-backward)
             (define-key keymap (kbd "<backtab>") #'cycle-backward)
             (define-key keymap (kbd "RET")
                         (if confirm #'submit-answers #'edit-answer))
             (define-key keymap (kbd "<return>")
                         (if confirm #'submit-answers #'edit-answer))
             (when confirm
               (define-key keymap (kbd "C-c C-c") #'submit-answers)
               (define-key keymap (kbd "C-c C-e")
                           #'edit-specific-question)
               (define-key keymap (kbd "e") #'edit-specific-question))
             (define-key keymap (kbd "C-c C-k") #'quit-questionnaire)
             (define-key keymap (kbd "q") #'quit-questionnaire)
             (define-key keymap (kbd "C-g") #'quit-questionnaire)
             keymap))

         (render-ask-body
           (body keymap)
           "Render Ask BODY with KEYMAP through the interaction painter."
           (with-current-buffer chat-buffer
             (setq overlay
                   (mevedel-view--interaction-register
                    (list :kind 'ask
                          :id interaction-id
                          :count 0
                          :body body
                          :priority 150
                          :keymap keymap
                          :help-echo "Ask prompt")))
             (overlay-put overlay 'mevedel-user-request t)
             (overlay-put overlay 'mevedel--callback callback)
             (cl-pushnew overlay mevedel--prompt-overlays :test #'eq)
             (mevedel--prompt--register-canceller source-buffer overlay)))

         (update-overlay
           (index)
           "Update overlay to show question at INDEX."
           (let* ((q (nth index questions-list))
                  (question-text (plist-get q :question))
                  (options (append (plist-get q :options) nil))
                  (prev-answer (aref answers index))
                  (body
                   (concat
                    "\n"
                    (propertize (format "Question %d/%d"
                                        (1+ index)
                                        (length questions-list))
                                'font-lock-face 'font-lock-string-face)
                    (propertize "\n" 'font-lock-face
                                '(:inherit font-lock-string-face
                                  :underline t :extend t))
                    "\n"
                    (propertize question-text
                                'font-lock-face 'font-lock-escape-face)
                    "\n\n"
                    (propertize "Available options:\n"
                                'font-lock-face
                                'font-lock-constant-face)
                    (mapconcat
                     (lambda (opt)
                       (mevedel-tools--ask-format-option-line opt))
                     options "\n")
                    "\n  - Custom input\n\n"
                    (when prev-answer
                      (concat
                       (propertize "Current answer: "
                                   'font-lock-face 'warning)
                       (propertize prev-answer 'font-lock-face 'bold)
                       "\n"
                       (or (mevedel-tools--ask-format-selected-preview
                            prev-answer options)
                           "\n")))
                    (propertize "Keys: "
                                'font-lock-face 'help-key-binding)
                    (propertize "TAB"
                                'font-lock-face 'help-key-binding)
                    " cycle  "
                    (propertize "RET"
                                'font-lock-face 'help-key-binding)
                    " answer next  "
                    (propertize "q"
                                'font-lock-face 'help-key-binding)
                    " cancel\n"
                    (propertize "\n" 'font-lock-face
                                '(:inherit font-lock-string-face
                                  :underline t :extend t)))))
             (render-ask-body body (ask-keymap))))

         (submit-answers
           ()
           "Submit all answers to LLM."
           (interactive)
           (let ((result (with-temp-buffer
                           (insert "User answered the following questions:\n\n")
                           (dotimes (i (length questions-list))
                             (let ((q (nth i questions-list))
                                   (a (aref answers i)))
                               (insert (format "Q%d: %s\n" (1+ i) (plist-get q :question)))
                               (insert (format "A%d: %s\n\n" (1+ i) a))))
                           (buffer-string))))
             (cleanup-and-return result)))

         (edit-specific-question
           ()
           "Edit a specific question by number."
           (interactive)
           (let* ((default-qnum (if (eq current-index 'confirm) 1 (1+ current-index)))
                  (qnum (read-number "Edit question number: "
                                     default-qnum)))
             (when (and (>= qnum 1) (<= qnum (length questions-list)))
               (setq current-index (1- qnum))
               (update-overlay current-index))))

         (show-confirmation
           ()
           "Show all answers in overlay and ask for final confirmation."
           (let ((body
                  (concat
                   "\n"
                   (propertize "Review Your Answers"
                               'font-lock-face 'font-lock-string-face)
                   (propertize "\n" 'font-lock-face
                               '(:inherit font-lock-string-face
                                 :underline t :extend t))
                   "\n"
                   (mapconcat
                    (lambda (i)
                      (let ((q (nth i questions-list))
                            (a (aref answers i)))
                        (concat
                         (propertize (format "%d. " (1+ i))
                                     'font-lock-face 'bold)
                         (plist-get q :question)
                         "\n"
                         (propertize "   -> "
                                     'font-lock-face 'shadow)
                         (if a
                             (concat
                              (propertize a 'font-lock-face 'success)
                              (when-let* ((preview
                                           (mevedel-tools--ask-format-selected-preview
                                            a (append (plist-get q :options) nil))))
                                (concat "\n" preview)))
                           (propertize "(not answered)"
                                       'font-lock-face 'shadow)))))
                    (number-sequence 0 (1- (length questions-list)))
                    "\n\n")
                   "\n\n"
                   (propertize "Keys: "
                               'font-lock-face 'help-key-binding)
                   (propertize "TAB"
                               'font-lock-face 'help-key-binding)
                   " cycle  "
                   (propertize "RET"
                               'font-lock-face 'help-key-binding)
                   " submit  "
                   (propertize "e"
                               'font-lock-face 'help-key-binding)
                   " edit  "
                   (propertize "q"
                               'font-lock-face 'help-key-binding)
                   " cancel\n"
                   (propertize "\n" 'font-lock-face
                               '(:inherit font-lock-string-face
                                 :underline t :extend t)))))
             (render-ask-body body (ask-keymap t))))

         (cleanup-and-return
           (result)
           "Clean up overlay and return RESULT."
           (when overlay
             (when (fboundp 'mevedel-view--interaction-unregister)
               (mevedel-view--interaction-unregister interaction-id))
             (delete-overlay overlay))
           (funcall callback result)))

      ;; Start the questionnaire - show first question
      (update-overlay 0))))

(defun mevedel-tool-ask--ask (callback args)
  "Ask the user questions.
CALLBACK receives the formatted answers.  ARGS is a plist with :questions."
  (let ((questions (plist-get args :questions)))
    (unless questions
      (error "Parameter questions is required"))
    (mevedel-tools--ask-user
     (lambda (value)
       (funcall callback (list :result value)))
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
    :prompt-file "tools/ask.md"
    :handler #'mevedel-tool-ask--ask
    :args ((questions array :required
                      "Array of question objects. Each question must have predefined answer options. Options may be strings or objects with label, description, and preview fields. Mark exactly one option per question by appending ` (Recommended)` to that option label."
                      :items (:type object)
                      :minItems 1))
    :async-p t
    :max-result-size 30000
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-ask--render))

(provide 'mevedel-tool-ask)
;;; mevedel-tool-ask.el ends here
