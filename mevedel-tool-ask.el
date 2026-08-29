;;; mevedel-tool-ask.el -- Ask interaction tool -*- lexical-binding: t -*-

;;; Commentary:

;; The Ask questionnaire, its renderer, and tool registration.

;;; Code:

;; `cl-find', `cl-position' and `cl-count-if' live in `cl-seq' and carry no
;; autoloads, so they have to be present at runtime, not only at compile time.
(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-tool-registry))

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))
(autoload 'mevedel-abort "mevedel-chat")

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--data-buffer "mevedel-interaction-prompt"
                  (&optional buffer))
(declare-function mevedel--prompt--register-canceller
                  "mevedel-interaction-prompt"
                  (&optional source-buffer overlay))
(declare-function mevedel--prompt--settle "mevedel-interaction-prompt"
                  (overlay outcome))
(declare-function mevedel--prompt-announce
                  "mevedel-interaction-prompt" (overlay))
(declare-function mevedel--prompt-attribution-line
                  "mevedel-interaction-prompt" (origin))
(defvar mevedel--prompt-overlays)
(defvar mevedel-interaction-prompt-settled-hook)
(autoload 'mevedel--prompt--data-buffer "mevedel-interaction-prompt")
(autoload 'mevedel--prompt--register-canceller "mevedel-interaction-prompt")
(autoload 'mevedel--prompt--settle "mevedel-interaction-prompt")
(autoload 'mevedel--prompt-announce "mevedel-interaction-prompt")
(autoload 'mevedel--prompt-attribution-line "mevedel-interaction-prompt")

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--positional-to-plist
                  "mevedel-pipeline" (raw-args specs))
(declare-function mevedel-pipeline-run-tool
                  "mevedel-pipeline" (tool callback args))

;; `mevedel-tool-registry'
(declare-function mevedel-tool--resolve-prompt
                  "mevedel-tool-registry" (prompt))
(declare-function mevedel-tool-register "mevedel-tool-registry" (tool))

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())
(autoload 'mevedel-current-origin "mevedel-turn")

;; `mevedel-view-render'
(declare-function mevedel-view--fontify-as "mevedel-view-render" (text mode))
(autoload 'mevedel-view--fontify-as "mevedel-view-render")

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction"
                  (descriptor))
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction"
                  (&optional data-buffer))
(autoload 'mevedel-view--interaction-register "mevedel-view-interaction")
(autoload 'mevedel-view--interaction-target-buffer
  "mevedel-view-interaction")


;;
;;; Options

(defconst mevedel-tools--ask-recommended-suffix " (Recommended)"
  "Suffix marking a recommended Ask option.")

(defconst mevedel-tools--ask-custom-label "Custom input"
  "Label of the free-text entry every question carries.")

(defconst mevedel-tools--ask-no-preference
  "(no preference -- use your judgment)"
  "Answer recorded for a question submitted without a choice.
A questionnaire may be submitted incomplete, and the model has to be
able to tell that from an answered question without reading a nil.")

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

(defun mevedel-tools--ask-option-sample (option)
  "Return OPTION's sample string, or nil.
The sample is the model-authored artifact shown beside the option list;
`preview' names an interaction kind elsewhere in mevedel and is
deliberately not reused here."
  (let ((sample (and (not (stringp option))
                     (mevedel-tools--ask-option-field option :sample))))
    (cond
     ((and (stringp sample)
           (not (string-blank-p sample)))
      sample)
     (sample (format "%s" sample)))))

(defun mevedel-tools--ask-option-by-label (label options)
  "Return first option in OPTIONS whose label equals LABEL."
  (cl-find label options
           :test #'equal
           :key #'mevedel-tools--ask-option-label))

(defun mevedel-tools--ask-selected-sample (answer options)
  "Return the sample of the OPTIONS entry ANSWER selected, or nil."
  (when-let* ((option (mevedel-tools--ask-option-by-label answer options)))
    (mevedel-tools--ask-option-sample option)))

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

(defun mevedel-tools--ask-key (key)
  "Return KEY propertized as a key binding.
Only the key itself carries `help-key-binding': facing the surrounding
label too draws a box around the prose."
  (propertize key 'font-lock-face 'help-key-binding))


;;
;;; Sample frame

(defcustom mevedel-ask-sample-frame-width 0.5
  "Width of the Ask sample frame as a fraction of its parent frame."
  :type 'float
  :group 'mevedel)

(defcustom mevedel-ask-sample-frame-height 20
  "Largest height of the Ask sample frame, in lines."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-ask-sample-frame-min-height 3
  "Smallest height of the Ask sample frame, in lines.
The frame fits itself to its sample between this and
`mevedel-ask-sample-frame-height'."
  :type 'integer
  :group 'mevedel)

(defvar mevedel-tools--ask-sample-frame nil
  "The live Ask sample frame, or nil.")

(defvar mevedel-tools--ask-sample-buffer nil
  "Buffer backing the Ask sample frame.")

(defconst mevedel-tools--ask-sample-parameters
  ;; ponytail: copied from `mevedel-directive-frame--parameters' rather
  ;; than shared, to keep the Ask tool free of a load-time dependency on
  ;; the directive frame.  Extract a child-frame helper if a third one
  ;; appears.  The focus parameters are the deliberate difference: a
  ;; sample is read through the form's scroll keys and never selected.
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (cursor-type . nil)
    (min-width . t)
    (min-height . t)
    (border-width . 0)
    (outer-border-width . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . t)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (fullscreen . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t)
    (inhibit-double-buffering . t))
  "Child frame parameters for the Ask sample frame.")

(defun mevedel-tools--ask-question-start (overlay index)
  "Return where question INDEX's block starts inside OVERLAY, or nil.
Runs in OVERLAY's buffer.  The form is handed to the interaction painter
as one string, so the only way back to a single question's position is
the property `question-block' stamps on it."
  (let ((position (overlay-start overlay))
        (limit (overlay-end overlay))
        (found nil))
    (while (and position (< position limit) (not found))
      (if (eq index (get-text-property position 'mevedel-ask-question))
          (setq found position)
        (setq position (next-single-property-change
                        position 'mevedel-ask-question nil limit))))
    found))

(defun mevedel-tools--ask-question-anchor (overlay index)
  "Return the position just past question INDEX's block in OVERLAY.
Falls back to the end of the whole form when the block cannot be found,
so a sample still opens somewhere sensible if a render is in flight."
  (let ((buffer (and (overlayp overlay) (overlay-buffer overlay))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((limit (overlay-end overlay))
               (start (mevedel-tools--ask-question-start overlay index))
               (next (and start
                          (mevedel-tools--ask-question-start
                           overlay (1+ index)))))
          ;; The character before the next block is the blank line
          ;; between them, which puts the frame under this question's
          ;; options rather than over them.
          (cond ((and next (> next start)) (1- next))
                (t limit)))))))


(defun mevedel-tools--ask-sample-work-buffer ()
  "Return the live buffer backing the Ask sample frame."
  (if (buffer-live-p mevedel-tools--ask-sample-buffer)
      mevedel-tools--ask-sample-buffer
    (setq mevedel-tools--ask-sample-buffer
          (get-buffer-create " *mevedel-ask-sample*"))))

(defun mevedel-tools--ask-sample-close ()
  "Close the Ask sample frame."
  (when (frame-live-p mevedel-tools--ask-sample-frame)
    (delete-frame mevedel-tools--ask-sample-frame))
  (setq mevedel-tools--ask-sample-frame nil))

(defun mevedel-tools--ask-sample-teardown (&rest _)
  "Close the Ask sample frame when any prompt settles.
The frame is a singleton owned by whichever questionnaire is open, so an
unrelated prompt settling under one closes a frame the next cursor move
reopens -- cheaper than tracking which overlay owns it."
  (mevedel-tools--ask-sample-close))

(add-hook 'mevedel-interaction-prompt-settled-hook
          #'mevedel-tools--ask-sample-teardown)

(defun mevedel-tools--ask-sample-ensure-frame (parent)
  "Return the Ask sample child frame of PARENT, creating it when needed."
  (let ((frame mevedel-tools--ask-sample-frame))
    (unless (and (frame-live-p frame)
                 (eq (frame-parent frame) parent))
      (when (frame-live-p frame) (delete-frame frame))
      (setq frame
            (make-frame
             `((name . "mevedel-ask-sample")
               (parent-frame . ,parent)
               (minibuffer . ,(minibuffer-window parent))
               (font . ,(frame-parameter parent 'font))
               (width . 0) (height . 0) (visibility . nil)
               (internal-border-width . 1)
               (child-frame-border-width . 1)
               ,@mevedel-tools--ask-sample-parameters)))
      (let ((window (frame-root-window frame)))
        (set-window-parameter window 'no-delete-other-windows t)
        (set-window-parameter window 'mode-line-format 'none)
        (set-window-parameter window 'header-line-format 'none)
        (set-window-fringes window 0 0))
      (setq mevedel-tools--ask-sample-frame frame))
    frame))

(defun mevedel-tools--ask-sample-place (frame parent window position)
  "Size and place FRAME in PARENT under buffer POSITION in WINDOW."
  (when-let* ((pixels (window-absolute-pixel-position position window)))
    ;; `window-absolute-pixel-position' answers in display coordinates
    ;; while a child frame's position is relative to its parent's native
    ;; frame.  See docs/adr/0113.
    (let* ((native (frame-edges parent 'native-edges))
           (char-height (frame-char-height parent))
           (parent-width (frame-pixel-width parent))
           (parent-height (frame-pixel-height parent))
           (width (round (* mevedel-ask-sample-frame-width parent-width)))
           (height (* mevedel-ask-sample-frame-height char-height))
           (anchor-x (- (car pixels) (nth 0 native)))
           (anchor-y (- (cdr pixels) (nth 1 native)))
           (below (+ anchor-y char-height))
           (x (max 0 (min anchor-x (- parent-width width))))
           ;; Flip above the form when there is no room below it.
           (y (if (<= (+ below height) parent-height)
                  below
                (max 0 (- anchor-y height)))))
      (set-frame-size frame width height t)
      (set-frame-position frame x y)
      ;; `vertically' only: fitting both dimensions widens the frame to
      ;; the sample's longest unwrapped line, which a code block readily
      ;; pushes past the parent.
      (fit-frame-to-buffer frame
                           mevedel-ask-sample-frame-height
                           mevedel-ask-sample-frame-min-height
                           nil nil 'vertically)
      t)))

(defun mevedel-tools--ask-sample-show (sample overlay index)
  "Show SAMPLE in a child frame under question INDEX's block in OVERLAY.
Anchoring to the question rather than to the focused option keeps the
frame still while the cursor walks the options being compared.  Closes
the frame instead when the block is not on screen."
  (let* ((buffer (and (overlayp overlay) (overlay-buffer overlay)))
         (window (and (buffer-live-p buffer) (get-buffer-window buffer t)))
         (position (mevedel-tools--ask-question-anchor overlay index)))
    ;; A batch session has no terminal, so `make-frame' signals there.
    (if (or noninteractive
            (not (and (window-live-p window)
                      (pos-visible-in-window-p position window))))
        (mevedel-tools--ask-sample-close)
      (let ((parent (window-frame window))
            (work (mevedel-tools--ask-sample-work-buffer)))
        (with-current-buffer work
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (mevedel-view--fontify-as sample 'markdown-mode))
            (goto-char (point-min)))
          (setq-local mode-line-format nil)
          (setq-local header-line-format nil)
          (setq-local truncate-lines nil))
        (let ((frame (mevedel-tools--ask-sample-ensure-frame parent)))
          (set-window-buffer (frame-root-window frame) work)
          (if (mevedel-tools--ask-sample-place frame parent window position)
              (make-frame-visible frame)
            (mevedel-tools--ask-sample-close)))))))

(defun mevedel-tools--ask-sample-scroll (command)
  "Run scroll COMMAND inside the sample frame without selecting it."
  (when (frame-live-p mevedel-tools--ask-sample-frame)
    (with-selected-window (frame-root-window mevedel-tools--ask-sample-frame)
      (condition-case nil (funcall command) (error nil)))))


;;
;;; Ask User

(cl-defun mevedel-tools--ask-user (callback questions)
  "Ask user QUESTIONS as one form and settle it through CALLBACK.

CALLBACK is the async callback function to call with results.
QUESTIONS is an array of question plists, each with :question and
:options keys.

Every question is on screen at once.  The question holding the cursor
shows its options; the others collapse to their answer, so which
question is expanded is derived from the cursor rather than tracked
alongside it."
  (mevedel-tools--validate-params callback mevedel-tools--ask-user
    (questions (vectorp . "array")))

  (let* ((source-buffer (current-buffer))
         (origin (mevedel-current-origin))
         (questions-list (append questions nil))
         (count (length questions-list))
         (answers (make-vector count nil))
         (chat-buffer
          (or (mevedel-view--interaction-target-buffer
               (with-current-buffer source-buffer
                 (mevedel--prompt--data-buffer)))
              (error "No live view for Ask prompt")))
         (interaction-id (list :ask (gensym "ask-")))
         (overlay nil)
         (focus-question 0)
         (focus-option 0))

    (cl-labels
        ((question-options
           (index)
           "Return the option objects of question INDEX."
           (append (plist-get (nth index questions-list) :options) nil))

         (entry-count
           (index)
           "Return how many focusable entries question INDEX has.
The free-text entry follows the options and is always present."
           (1+ (length (question-options index))))

         (custom-entry-p
           (index option)
           (= option (length (question-options index))))

         (focused-option
           ()
           (unless (custom-entry-p focus-question focus-option)
             (nth focus-option (question-options focus-question))))

         (answered-entry
           (index)
           "Return the entry index question INDEX's answer selected."
           (let ((answer (aref answers index)))
             (when answer
               (or (cl-position answer (question-options index)
                                :test #'equal
                                :key #'mevedel-tools--ask-option-label)
                   ;; A custom answer matches no option.
                   (length (question-options index))))))

         (enter-question
           (index)
           "Move the cursor into question INDEX, landing on its answer."
           (setq focus-question index
                 focus-option (or (answered-entry index) 0)))

         (answered-count
           ()
           (cl-count-if #'identity (append answers nil)))

         (next-unanswered
           ()
           (cl-loop for step from 1 to count
                    for index = (% (+ focus-question step) count)
                    unless (aref answers index) return index))

         ;;
         ;; Movement

         (move-next
           ()
           (interactive)
           (if (< focus-option (1- (entry-count focus-question)))
               (setq focus-option (1+ focus-option))
             (enter-question (% (1+ focus-question) count)))
           (refresh))

         (move-previous
           ()
           (interactive)
           (if (> focus-option 0)
               (setq focus-option (1- focus-option))
             (enter-question (mod (1- focus-question) count)))
           (refresh))

         (focus-entry
           (entry)
           "Focus ENTRY of the current question, if it has one."
           (when (< entry (entry-count focus-question))
             (setq focus-option entry)
             (refresh)))

         ;;
         ;; Answering

         (record-answer
           (answer)
           (aset answers focus-question answer)
           (when-let* ((next (next-unanswered)))
             (enter-question next))
           ;; An answer is the only change a remote guest needs to see;
           ;; cursor movement is host-local and must not re-announce.
           (refresh t))

         (pick
           ()
           "Record the focused option as this question's answer."
           (interactive)
           (if (custom-entry-p focus-question focus-option)
               (custom-answer)
             (record-answer
              (mevedel-tools--ask-option-label (focused-option)))))

         (custom-answer
           ()
           "Answer the current question with free text."
           (interactive)
           (let ((answer (read-string
                          (concat (format "%s" (plist-get
                                               (nth focus-question
                                                    questions-list)
                                               :question))
                                  " (custom): ")
                          (aref answers focus-question))))
             (unless (string-blank-p answer)
               (record-answer answer))))

         ;;
         ;; Sample frame

         (scroll-sample-forward
           ()
           (interactive)
           (mevedel-tools--ask-sample-scroll #'scroll-up-command))

         (scroll-sample-backward
           ()
           (interactive)
           (mevedel-tools--ask-sample-scroll #'scroll-down-command))

         (sync-sample
           ()
           "Show the focused option's sample, or close the frame."
           (let ((sample (and (not (custom-entry-p focus-question
                                                   focus-option))
                              (mevedel-tools--ask-option-sample
                               (focused-option)))))
             (if sample
                 (mevedel-tools--ask-sample-show sample overlay
                                                 focus-question)
               (mevedel-tools--ask-sample-close))))

         ;;
         ;; Settlement

         (submission-text
           ()
           (with-temp-buffer
             (insert "User answered the following questions:\n\n")
             (dotimes (index count)
               (let ((question (plist-get (nth index questions-list)
                                          :question))
                     (answer (aref answers index)))
                 (insert (format "Q%d: %s\n" (1+ index) question))
                 (insert (format "A%d: %s\n"
                                 (1+ index)
                                 (or answer
                                     mevedel-tools--ask-no-preference)))
                 (when-let* ((answer)
                             (sample (mevedel-tools--ask-selected-sample
                                      answer (question-options index))))
                   (insert (format "Sample shown for A%d:\n%s\n"
                                   (1+ index) sample)))
                 (insert "\n")))
             (buffer-string)))

         (submit-answers
           ()
           "Submit every answer, complete or not."
           (interactive)
           (mevedel-tools--ask-sample-close)
           (mevedel--prompt--settle overlay (submission-text)))

         (cancel-questionnaire
           ()
           "Settle the questionnaire as cancelled, leaving the run alive.
The handler turns this into an error result the model can act on."
           (interactive)
           (mevedel-tools--ask-sample-close)
           (mevedel--prompt--settle overlay 'aborted))

         (abort-run
           ()
           "Cancel the questionnaire and abort the whole execution."
           (interactive)
           (mevedel-tools--ask-sample-close)
           (mevedel--prompt--settle overlay 'aborted)
           (mevedel-abort))

         ;;
         ;; Remote surface

         (remote-questions
           ()
           "Return the questionnaire as JSON-safe alists with answers."
           (cl-loop for q in questions-list
                    for index from 0
                    collect
                    `(("question" . ,(format "%s" (plist-get q :question)))
                      ("options"
                       . ,(vconcat
                           (mapcar
                            (lambda (option)
                              (let ((description
                                     (mevedel-tools--ask-option-description
                                      option))
                                    (sample
                                     (mevedel-tools--ask-option-sample
                                      option)))
                                (append
                                 (list (cons "label"
                                             (mevedel-tools--ask-option-label
                                              option)))
                                 (when description
                                   (list (cons "description" description)))
                                 ;; A guest choosing between artifacts
                                 ;; needs the artifacts.
                                 (when sample
                                   (list (cons "sample" sample))))))
                            (question-options index))))
                      ,@(when-let* ((answer (aref answers index)))
                          `(("answer" . ,answer))))))

         (remote-answer
           (submitted)
           "Adopt the guest's SUBMITTED answers and submit the form."
           (when (= (length submitted) count)
             (cl-loop for answer in submitted
                      for index from 0
                      do (aset answers index answer))
             (submit-answers)))

         ;;
         ;; Rendering

         (ask-keymap
           ()
           "Return the keymap for the Ask form."
           (let ((keymap (make-sparse-keymap)))
             (dolist (key '("n" "C-n" "<down>"))
               (define-key keymap (kbd key) #'move-next))
             (dolist (key '("p" "C-p" "<up>"))
               (define-key keymap (kbd key) #'move-previous))
             ;; Digits move the cursor rather than answer: an option may
             ;; carry a sample, and a sample exists to be looked at
             ;; before it is chosen.
             (dotimes (digit 9)
               (let ((entry digit))
                 (define-key keymap (kbd (number-to-string (1+ entry)))
                             (lambda ()
                               (interactive)
                               (focus-entry entry)))))
             (define-key keymap (kbd "RET") #'pick)
             (define-key keymap (kbd "<return>") #'pick)
             (define-key keymap (kbd "c") #'custom-answer)
             (define-key keymap (kbd "C-v") #'scroll-sample-forward)
             (define-key keymap (kbd "M-v") #'scroll-sample-backward)
             (define-key keymap (kbd "C-c C-c") #'submit-answers)
             (define-key keymap (kbd "q") #'cancel-questionnaire)
             (define-key keymap (kbd "C-g") #'cancel-questionnaire)
             (define-key keymap (kbd "C-c C-k") #'abort-run)
             keymap))

         (divider
           ()
           (propertize "\n" 'font-lock-face
                       '(:inherit font-lock-string-face
                         :underline t :extend t)))

         (header
           ()
           (concat
            (propertize (format "Ask · %d question%s"
                                count (if (= count 1) "" "s"))
                        'font-lock-face 'font-lock-string-face)
            (propertize (format "  ·  %d of %d answered"
                                (answered-count) count)
                        'font-lock-face
                        (if (= (answered-count) count) 'success 'shadow))
            (divider)))

         (option-lines
           (index entry)
           "Return the rendered lines for ENTRY of question INDEX."
           (let* ((options (question-options index))
                  (customp (custom-entry-p index entry))
                  (option (unless customp (nth entry options)))
                  (answer (aref answers index))
                  (selectedp (and answer
                                  (if customp
                                      (not (mevedel-tools--ask-option-by-label
                                            answer options))
                                    (equal answer
                                           (mevedel-tools--ask-option-label
                                            option)))))
                  (focusedp (and (= index focus-question)
                                 (= entry focus-option)))
                  (description (and option
                                    (mevedel-tools--ask-option-description
                                     option)))
                  (lines
                   (list
                    (concat
                     (if focusedp
                         (propertize "    ▸ " 'font-lock-face 'success)
                       "      ")
                     (propertize (if customp
                                     "c"
                                   (number-to-string (1+ entry)))
                                 'font-lock-face 'shadow)
                     " "
                     (if selectedp
                         (propertize "●" 'font-lock-face 'success)
                       (propertize "○" 'font-lock-face 'shadow))
                     " "
                     (if customp
                         (propertize
                          (concat mevedel-tools--ask-custom-label "…")
                          'font-lock-face 'shadow)
                       (mevedel-tools--ask-format-option option))))))
             (when description
               (setq lines
                     (append lines
                             (list (concat
                                    "          "
                                    (propertize description
                                                'font-lock-face 'shadow))))))
             lines))

         (question-block
           (index)
           "Return question INDEX rendered for the form."
           (let* ((question (plist-get (nth index questions-list) :question))
                  (answer (aref answers index))
                  (lines
                   (list
                    (concat
                     (if answer
                         (propertize " ✓ " 'font-lock-face 'success)
                       "   ")
                     (propertize (format "%d  " (1+ index))
                                 'font-lock-face 'bold)
                     (propertize (format "%s" question)
                                 'font-lock-face 'font-lock-escape-face)))))
             (cond
              ((= index focus-question)
               (dolist (entry (number-sequence 0 (1- (entry-count index))))
                 (setq lines (append lines (option-lines index entry)))))
              (answer
               (setq lines
                     (append lines
                             (list (concat
                                    "      "
                                    (propertize answer
                                                'font-lock-face 'success)))))))
             ;; The block carries its own index so the sample frame can
             ;; find where this question landed in the view buffer.
             (propertize (string-join lines "\n")
                         'mevedel-ask-question index)))

         (footer
           ()
           (concat
            (divider)
            (mevedel-tools--ask-key "n/p") " move  "
            (mevedel-tools--ask-key "1-9") " focus  "
            (mevedel-tools--ask-key "RET") " pick  "
            (mevedel-tools--ask-key "c") " custom  "
            (mevedel-tools--ask-key "C-v/M-v") " scroll sample\n"
            (mevedel-tools--ask-key "C-c C-c") " submit  "
            (mevedel-tools--ask-key "q") " cancel  "
            (mevedel-tools--ask-key "C-c C-k") " abort run\n"
            (divider)))

         (form-body
           ()
           (concat
            "\n"
            (header)
            "\n"
            (mapconcat #'question-block
                       (number-sequence 0 (1- count))
                       "\n\n")
            "\n"
            (footer)))

         (render
           (announce)
           "Paint the form.  Announce it remotely only when ANNOUNCE."
           (with-current-buffer chat-buffer
             (setq overlay
                   (mevedel-view--interaction-register
                    (list :kind 'ask
                          :id interaction-id
                          :origin origin
                          ;; The interaction counter multiplies this by
                          ;; the kind, so a three-question form must not
                          ;; report itself as one pending question.
                          :count count
                          :body
                          (concat
                           (mevedel--prompt-attribution-line origin)
                           (form-body))
                          :priority 150
                          :keymap (ask-keymap)
                          :help-echo "Ask prompt")))
             ;; Deliberately NOT `mevedel-user-request': that property
             ;; is the generic approve/deny/feedback surface, and those
             ;; are outcomes this questionnaire never offered.  Abort
             ;; teardown settles through `mevedel--prompt-overlays'
             ;; membership, which is independent of it.
             (overlay-put overlay 'mevedel--callback callback)
             (overlay-put overlay 'mevedel--remote
                          (list :body
                                (format "Ask · %d question%s"
                                        count (if (= count 1) "" "s"))
                                :questions #'remote-questions
                                :answer #'remote-answer))
             (cl-pushnew overlay mevedel--prompt-overlays :test #'eq)
             (mevedel--prompt--register-canceller source-buffer overlay)
             (when announce
               (mevedel--prompt-announce overlay))))

         (refresh
           (&optional announce)
           (render announce)
           (sync-sample)))

      (enter-question 0)
      (refresh t))))


(defun mevedel-tool-ask--ask (callback args)
  "Ask the user questions.
CALLBACK receives the formatted answers.  ARGS is a plist with :questions."
  (let ((questions (plist-get args :questions)))
    (unless questions
      (error "Parameter questions is required"))
    (mevedel-tools--ask-user
     (lambda (value)
       (funcall callback
                (if (stringp value)
                    (list :result value)
                  ;; Cancellation settles with a bare symbol; passing it
                  ;; through would record the call as a success with no
                  ;; renderable result.
                  (list :result
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
                      "Array of question objects. Each question must have predefined answer options. Options may be strings or objects with label, description, and sample fields. Mark exactly one option per question by appending ` (Recommended)` to that option label."
                      :items (:type object)
                      :minItems 1))
    :async-p t
    :max-result-size 30000
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-ask--render))

(provide 'mevedel-tool-ask)
;;; mevedel-tool-ask.el ends here
