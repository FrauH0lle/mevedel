;;; mevedel-tool-ask-ui.el -- Ask questionnaire UI -*- lexical-binding: t -*-

;;; Commentary:

;; Stateful Ask questionnaire presentation and interaction controllers.

;;; Code:

;; `cl-find', `cl-position' and `cl-count-if' live in `cl-seq' and carry no
;; autoloads, so they have to be present at runtime, not only at compile time.
(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-tool-registry))

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))
(autoload 'mevedel-abort "mevedel-chat")

;; `mevedel-directive-frame'
(defvar mevedel--child-frame-parameters)

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

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())
(autoload 'mevedel-current-origin "mevedel-turn")

;; `mevedel-view-fontify'
(declare-function mevedel-view--fontify-as "mevedel-view-fontify" (text mode))
(autoload 'mevedel-view--fontify-as "mevedel-view-fontify")

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

(defconst mevedel-tool-ask-ui--recommended-suffix " (Recommended)"
  "Suffix marking a recommended Ask option.")

(defconst mevedel-tool-ask-ui--custom-label "Custom input"
  "Label of the free-text entry every question carries.")

(defconst mevedel-tool-ask-ui--no-preference
  "(no preference -- use your judgment)"
  "Answer recorded for a question submitted without a choice.
A questionnaire may be submitted incomplete, and the model has to be
able to tell that from an answered question without reading a nil.")

(defun mevedel-tool-ask-ui--option-field (option key)
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

(defun mevedel-tool-ask-ui--option-label (option)
  "Return the answer label for OPTION."
  (let ((label (if (stringp option)
                   option
                 (mevedel-tool-ask-ui--option-field option :label))))
    (cond
     ((stringp label) label)
     (label (format "%s" label))
     (t (format "%s" option)))))

(defun mevedel-tool-ask-ui--option-description (option)
  "Return OPTION's description string, or nil."
  (let ((description
         (and (not (stringp option))
              (mevedel-tool-ask-ui--option-field option :description))))
    (cond
     ((and (stringp description)
           (not (string-blank-p description)))
      description)
     (description (format "%s" description)))))

(defun mevedel-tool-ask-ui--option-sample (option)
  "Return OPTION's sample string, or nil.
The sample is the model-authored artifact shown beside the option list;
`preview' names an interaction kind elsewhere in mevedel and is
deliberately not reused here."
  (let ((sample (and (not (stringp option))
                     (mevedel-tool-ask-ui--option-field option :sample))))
    (cond
     ((and (stringp sample)
           (not (string-blank-p sample)))
      sample)
     (sample (format "%s" sample)))))

(defun mevedel-tool-ask-ui--option-by-label (label options)
  "Return first option in OPTIONS whose label equals LABEL."
  (cl-find label options
           :test #'equal
           :key #'mevedel-tool-ask-ui--option-label))

(defun mevedel-tool-ask-ui--selected-sample (answer options)
  "Return the sample of the OPTIONS entry ANSWER selected, or nil."
  (when-let* ((option (mevedel-tool-ask-ui--option-by-label answer options)))
    (mevedel-tool-ask-ui--option-sample option)))

(defun mevedel-tool-ask-ui--format-option (option)
  "Return OPTION formatted for display in an Ask prompt."
  (let ((label (mevedel-tool-ask-ui--option-label option)))
    (if (string-suffix-p mevedel-tool-ask-ui--recommended-suffix label)
        (let ((base (substring label 0
                               (- (length label)
                                  (length mevedel-tool-ask-ui--recommended-suffix)))))
          (concat base
                  (propertize mevedel-tool-ask-ui--recommended-suffix
                              'font-lock-face 'success)))
      label)))

(defun mevedel-tool-ask-ui--key (key)
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

(defvar mevedel-tool-ask-ui--sample-frame nil
  "The live Ask sample frame, or nil.")

(defvar mevedel-tool-ask-ui--sample-buffer nil
  "Buffer backing the Ask sample frame.")

(defun mevedel-tool-ask-ui--question-start (overlay index)
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

(defun mevedel-tool-ask-ui--question-anchor (overlay index)
  "Return the position just past question INDEX's block in OVERLAY.
Falls back to the end of the whole form when the block cannot be found,
so a sample still opens somewhere sensible if a render is in flight."
  (let ((buffer (and (overlayp overlay) (overlay-buffer overlay))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((limit (overlay-end overlay))
               (start (mevedel-tool-ask-ui--question-start overlay index))
               (next (and start
                          (mevedel-tool-ask-ui--question-start
                           overlay (1+ index)))))
          ;; The character before the next block is the blank line
          ;; between them, which puts the frame under this question's
          ;; options rather than over them.
          (cond ((and next (> next start)) (1- next))
                (t limit)))))))


(defun mevedel-tool-ask-ui--sample-work-buffer ()
  "Return the live buffer backing the Ask sample frame."
  (if (buffer-live-p mevedel-tool-ask-ui--sample-buffer)
      mevedel-tool-ask-ui--sample-buffer
    (setq mevedel-tool-ask-ui--sample-buffer
          (get-buffer-create " *mevedel-ask-sample*"))))

(defun mevedel-tool-ask-ui--sample-close ()
  "Close the Ask sample frame."
  (when (frame-live-p mevedel-tool-ask-ui--sample-frame)
    (delete-frame mevedel-tool-ask-ui--sample-frame))
  (setq mevedel-tool-ask-ui--sample-frame nil))

(defun mevedel-tool-ask-ui--sample-teardown (&rest _)
  "Close the Ask sample frame when any prompt settles.
The frame is a singleton owned by whichever questionnaire is open, so an
unrelated prompt settling under one closes a frame the next cursor move
reopens -- cheaper than tracking which overlay owns it."
  (mevedel-tool-ask-ui--sample-close))

(add-hook 'mevedel-interaction-prompt-settled-hook
          #'mevedel-tool-ask-ui--sample-teardown)

(defun mevedel-tool-ask-ui--sample-ensure-frame (parent)
  "Return the Ask sample child frame of PARENT, creating it when needed."
  (let ((frame mevedel-tool-ask-ui--sample-frame))
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
               (no-accept-focus . t)
               (no-focus-on-map . t)
               (cursor-type . nil)
               ,@mevedel--child-frame-parameters)))
      (let ((window (frame-root-window frame)))
        (set-window-parameter window 'no-delete-other-windows t)
        (set-window-parameter window 'mode-line-format 'none)
        (set-window-parameter window 'header-line-format 'none)
        (set-window-fringes window 0 0))
      (setq mevedel-tool-ask-ui--sample-frame frame))
    frame))

(defun mevedel-tool-ask-ui--sample-place (frame parent window position)
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

(defun mevedel-tool-ask-ui--sample-show (sample overlay index)
  "Show SAMPLE in a child frame under question INDEX's block in OVERLAY.
Anchoring to the question rather than to the focused option keeps the
frame still while the cursor walks the options being compared.  Closes
the frame instead when the block is not on screen."
  (let* ((buffer (and (overlayp overlay) (overlay-buffer overlay)))
         (window (and (buffer-live-p buffer) (get-buffer-window buffer t)))
         (position (mevedel-tool-ask-ui--question-anchor overlay index)))
    ;; A batch session has no terminal, so `make-frame' signals there.
    (if (or noninteractive
            (not (and (window-live-p window)
                      (pos-visible-in-window-p position window))))
        (mevedel-tool-ask-ui--sample-close)
      (let ((parent (window-frame window))
            (work (mevedel-tool-ask-ui--sample-work-buffer)))
        (with-current-buffer work
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (mevedel-view--fontify-as sample 'markdown-mode))
            (goto-char (point-min)))
          (setq-local mode-line-format nil)
          (setq-local header-line-format nil)
          (setq-local truncate-lines nil))
        (let ((frame (mevedel-tool-ask-ui--sample-ensure-frame parent)))
          (set-window-buffer (frame-root-window frame) work)
          (if (mevedel-tool-ask-ui--sample-place frame parent window position)
              (make-frame-visible frame)
            (mevedel-tool-ask-ui--sample-close)))))))

(defun mevedel-tool-ask-ui--sample-scroll (command)
  "Run scroll COMMAND inside the sample frame without selecting it."
  (when (frame-live-p mevedel-tool-ask-ui--sample-frame)
    (with-selected-window (frame-root-window mevedel-tool-ask-ui--sample-frame)
      (condition-case nil (funcall command) (error nil)))))


;;
;;; Ask form state and controllers

(cl-defstruct
    (mevedel-tool-ask-ui--state
     (:constructor mevedel-tool-ask-ui--state-create))
  "Mutable state of one Ask questionnaire."
  callback source-buffer origin questions count answers chat-buffer
  interaction-id overlay focus-question focus-option)

(defun mevedel-tool-ask-ui--question-options (state index)
  "Return option objects for question INDEX in STATE."
  (append (plist-get
           (nth index (mevedel-tool-ask-ui--state-questions state))
           :options)
          nil))

(defun mevedel-tool-ask-ui--entry-count (state index)
  "Return the number of focusable entries for question INDEX in STATE."
  (1+ (length (mevedel-tool-ask-ui--question-options state index))))

(defun mevedel-tool-ask-ui--custom-entry-p (state index option)
  "Return non-nil when OPTION is question INDEX's custom entry in STATE."
  (= option (length (mevedel-tool-ask-ui--question-options state index))))

(defun mevedel-tool-ask-ui--focused-option (state)
  "Return the option object currently focused in STATE, or nil."
  (let ((question (mevedel-tool-ask-ui--state-focus-question state))
        (option (mevedel-tool-ask-ui--state-focus-option state)))
    (unless (mevedel-tool-ask-ui--custom-entry-p state question option)
      (nth option (mevedel-tool-ask-ui--question-options state question)))))

(defun mevedel-tool-ask-ui--answered-entry (state index)
  "Return the entry selected by question INDEX's answer in STATE."
  (let ((answer (aref (mevedel-tool-ask-ui--state-answers state) index))
        (options (mevedel-tool-ask-ui--question-options state index)))
    (when answer
      (or (cl-position answer options
                       :test #'equal
                       :key #'mevedel-tool-ask-ui--option-label)
          (length options)))))

(defun mevedel-tool-ask-ui--enter-question (state index)
  "Focus question INDEX in STATE, landing on its current answer."
  (setf (mevedel-tool-ask-ui--state-focus-question state) index
        (mevedel-tool-ask-ui--state-focus-option state)
        (or (mevedel-tool-ask-ui--answered-entry state index) 0)))

(defun mevedel-tool-ask-ui--answered-count (state)
  "Return the number of answered questions in STATE."
  (cl-count-if #'identity
               (append (mevedel-tool-ask-ui--state-answers state) nil)))

(defun mevedel-tool-ask-ui--next-unanswered (state)
  "Return the next unanswered question index in STATE, or nil."
  (let ((count (mevedel-tool-ask-ui--state-count state))
        (current (mevedel-tool-ask-ui--state-focus-question state))
        (answers (mevedel-tool-ask-ui--state-answers state)))
    (cl-loop for step from 1 to count
             for index = (% (+ current step) count)
             unless (aref answers index) return index)))

(defun mevedel-tool-ask-ui--submission-text (state)
  "Return STATE's complete model-facing answer text."
  (with-temp-buffer
    (insert "User answered the following questions:\n\n")
    (dotimes (index (mevedel-tool-ask-ui--state-count state))
      (let ((question
             (plist-get
              (nth index (mevedel-tool-ask-ui--state-questions state))
              :question))
            (answer (aref (mevedel-tool-ask-ui--state-answers state) index)))
        (insert (format "Q%d: %s\n" (1+ index) question))
        (insert (format "A%d: %s\n" (1+ index)
                        (or answer mevedel-tool-ask-ui--no-preference)))
        (when-let* ((answer)
                    (sample
                     (mevedel-tool-ask-ui--selected-sample
                      answer
                      (mevedel-tool-ask-ui--question-options state index))))
          (insert (format "Sample shown for A%d:\n%s\n" (1+ index) sample)))
        (insert "\n")))
    (buffer-string)))

(defun mevedel-tool-ask-ui--remote-questions (state)
  "Return STATE as JSON-safe questions with current answers."
  (cl-loop
   for question in (mevedel-tool-ask-ui--state-questions state)
   for index from 0
   for answer = (aref (mevedel-tool-ask-ui--state-answers state) index)
   collect
   (append
    (list
     (cons "question" (format "%s" (plist-get question :question)))
     (cons
      "options"
      (vconcat
       (mapcar
        (lambda (option)
          (let ((description
                 (mevedel-tool-ask-ui--option-description option))
                (sample (mevedel-tool-ask-ui--option-sample option)))
            (append
             (list (cons "label"
                         (mevedel-tool-ask-ui--option-label option)))
             (when description
               (list (cons "description" description)))
             (when sample (list (cons "sample" sample))))))
        (mevedel-tool-ask-ui--question-options state index)))))
    (when answer (list (cons "answer" answer))))))

(defun mevedel-tool-ask-ui--sync-sample (state)
  "Show STATE's focused sample, or close the sample frame."
  (let* ((question (mevedel-tool-ask-ui--state-focus-question state))
         (option (mevedel-tool-ask-ui--state-focus-option state))
         (sample
          (and
           (not (mevedel-tool-ask-ui--custom-entry-p
                 state question option))
           (mevedel-tool-ask-ui--option-sample
            (mevedel-tool-ask-ui--focused-option state)))))
    (if sample
        (mevedel-tool-ask-ui--sample-show
         sample (mevedel-tool-ask-ui--state-overlay state) question)
      (mevedel-tool-ask-ui--sample-close))))

(defun mevedel-tool-ask-ui--dispatch (state action &optional value)
  "Apply ACTION with optional VALUE to Ask STATE."
  (let ((question (mevedel-tool-ask-ui--state-focus-question state))
        (option (mevedel-tool-ask-ui--state-focus-option state)))
    (pcase action
      ('next
       (if (< option
              (1- (mevedel-tool-ask-ui--entry-count state question)))
           (setf (mevedel-tool-ask-ui--state-focus-option state) (1+ option))
         (mevedel-tool-ask-ui--enter-question
          state (% (1+ question) (mevedel-tool-ask-ui--state-count state))))
       (mevedel-tool-ask-ui--refresh state))
      ('previous
       (if (> option 0)
           (setf (mevedel-tool-ask-ui--state-focus-option state) (1- option))
         (mevedel-tool-ask-ui--enter-question
          state (mod (1- question) (mevedel-tool-ask-ui--state-count state))))
       (mevedel-tool-ask-ui--refresh state))
      ('focus
       (when (< value (mevedel-tool-ask-ui--entry-count state question))
         (setf (mevedel-tool-ask-ui--state-focus-option state) value)
         (mevedel-tool-ask-ui--refresh state)))
      ('pick
       (if (mevedel-tool-ask-ui--custom-entry-p state question option)
           (mevedel-tool-ask-ui--dispatch state 'custom)
         (mevedel-tool-ask-ui--dispatch
          state 'record
          (mevedel-tool-ask-ui--option-label
           (mevedel-tool-ask-ui--focused-option state)))))
      ('custom
       (let ((answer
              (read-string
               (concat
                (format
                 "%s"
                 (plist-get
                  (nth question
                       (mevedel-tool-ask-ui--state-questions state))
                  :question))
                " (custom): ")
               (aref (mevedel-tool-ask-ui--state-answers state) question))))
         (unless (string-blank-p answer)
           (mevedel-tool-ask-ui--dispatch state 'record answer))))
      ('record
       (aset (mevedel-tool-ask-ui--state-answers state) question value)
       (when-let* ((next (mevedel-tool-ask-ui--next-unanswered state)))
         (mevedel-tool-ask-ui--enter-question state next))
       (mevedel-tool-ask-ui--refresh state t))
      ('submit
       (mevedel-tool-ask-ui--sample-close)
       (mevedel--prompt--settle
        (mevedel-tool-ask-ui--state-overlay state)
        (mevedel-tool-ask-ui--submission-text state)))
      ('cancel
       (mevedel-tool-ask-ui--sample-close)
       (mevedel--prompt--settle
        (mevedel-tool-ask-ui--state-overlay state) 'aborted))
      ('abort
       (mevedel-tool-ask-ui--dispatch state 'cancel)
       (mevedel-abort))
      ('remote-answer
       (when (= (length value) (mevedel-tool-ask-ui--state-count state))
         (cl-loop for answer in value
                  for index from 0
                  do (aset
                      (mevedel-tool-ask-ui--state-answers state)
                      index
                      (unless (string-blank-p answer) answer)))
         (mevedel-tool-ask-ui--dispatch state 'submit))))))

(defun mevedel-tool-ask-ui--keymap (state)
  "Return the interaction keymap controlling STATE."
  (let ((keymap (make-sparse-keymap)))
    (dolist (key '("n" "C-n" "<down>"))
      (define-key keymap (kbd key)
                  (lambda ()
                    (interactive)
                    (mevedel-tool-ask-ui--dispatch state 'next))))
    (dolist (key '("p" "C-p" "<up>"))
      (define-key keymap (kbd key)
                  (lambda ()
                    (interactive)
                    (mevedel-tool-ask-ui--dispatch state 'previous))))
    (dotimes (digit 9)
      (let ((entry digit))
        (define-key
         keymap (kbd (number-to-string (1+ entry)))
         (lambda ()
           (interactive)
           (mevedel-tool-ask-ui--dispatch state 'focus entry)))))
    (dolist (key '("RET" "<return>"))
      (define-key keymap (kbd key)
                  (lambda ()
                    (interactive)
                    (mevedel-tool-ask-ui--dispatch state 'pick))))
    (define-key keymap (kbd "c")
                (lambda ()
                  (interactive)
                  (mevedel-tool-ask-ui--dispatch state 'custom)))
    (define-key
     keymap (kbd "C-v")
     (lambda ()
       (interactive)
       (mevedel-tool-ask-ui--sample-scroll #'scroll-up-command)))
    (define-key
     keymap (kbd "M-v")
     (lambda ()
       (interactive)
       (mevedel-tool-ask-ui--sample-scroll #'scroll-down-command)))
    (define-key keymap (kbd "C-c C-c")
                (lambda ()
                  (interactive)
                  (mevedel-tool-ask-ui--dispatch state 'submit)))
    (dolist (key '("q" "C-g"))
      (define-key keymap (kbd key)
                  (lambda ()
                    (interactive)
                    (mevedel-tool-ask-ui--dispatch state 'cancel))))
    (define-key keymap (kbd "C-c C-k")
                (lambda ()
                  (interactive)
                  (mevedel-tool-ask-ui--dispatch state 'abort)))
    keymap))

(defun mevedel-tool-ask-ui--divider ()
  "Return the rendered Ask divider."
  (propertize "\n" 'font-lock-face
              '(:inherit font-lock-string-face
                :underline t :extend t)))

(defun mevedel-tool-ask-ui--header (state)
  "Return the rendered header for STATE."
  (let ((count (mevedel-tool-ask-ui--state-count state))
        (answered (mevedel-tool-ask-ui--answered-count state)))
    (concat
     (propertize
      (format "Ask · %d question%s" count (if (= count 1) "" "s"))
      'font-lock-face 'font-lock-string-face)
     (propertize
      (format "  ·  %d of %d answered" answered count)
      'font-lock-face (if (= answered count) 'success 'shadow))
     (mevedel-tool-ask-ui--divider))))

(defun mevedel-tool-ask-ui--option-lines (state index entry)
  "Return rendered lines for ENTRY of question INDEX in STATE."
  (let* ((options (mevedel-tool-ask-ui--question-options state index))
         (customp (mevedel-tool-ask-ui--custom-entry-p state index entry))
         (option (unless customp (nth entry options)))
         (answer (aref (mevedel-tool-ask-ui--state-answers state) index))
         (selectedp
          (and answer
               (if customp
                   (not (mevedel-tool-ask-ui--option-by-label
                         answer options))
                 (equal answer
                        (mevedel-tool-ask-ui--option-label option)))))
         (focusedp
          (and (= index (mevedel-tool-ask-ui--state-focus-question state))
               (= entry (mevedel-tool-ask-ui--state-focus-option state))))
         (description
          (and option (mevedel-tool-ask-ui--option-description option)))
         (lines
          (list
           (concat
            (if focusedp
                (propertize "    ▸ " 'font-lock-face 'success)
              "      ")
            (propertize
             (if customp "c" (number-to-string (1+ entry)))
             'font-lock-face 'shadow)
            " "
            (if selectedp
                (propertize "●" 'font-lock-face 'success)
              (propertize "○" 'font-lock-face 'shadow))
            " "
            (if customp
                (propertize
                 (concat mevedel-tool-ask-ui--custom-label "…")
                 'font-lock-face 'shadow)
              (mevedel-tool-ask-ui--format-option option))))))
    (when description
      (setq lines
            (append lines
                    (list
                     (concat
                      "          "
                      (propertize description 'font-lock-face 'shadow))))))
    lines))

(defun mevedel-tool-ask-ui--question-block (state index)
  "Return question INDEX rendered for STATE."
  (let* ((question
          (plist-get
           (nth index (mevedel-tool-ask-ui--state-questions state))
           :question))
         (answer (aref (mevedel-tool-ask-ui--state-answers state) index))
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
     ((= index (mevedel-tool-ask-ui--state-focus-question state))
      (dolist
          (entry
           (number-sequence
            0 (1- (mevedel-tool-ask-ui--entry-count state index))))
        (setq lines
              (append
               lines
               (mevedel-tool-ask-ui--option-lines state index entry)))))
     (answer
      (setq lines
            (append
             lines
             (list
              (concat
               "      "
               (propertize answer 'font-lock-face 'success)))))))
    (propertize (string-join lines "\n")
                'mevedel-ask-question index)))

(defun mevedel-tool-ask-ui--footer ()
  "Return the Ask interaction footer."
  (concat
   (mevedel-tool-ask-ui--divider)
   (mevedel-tool-ask-ui--key "n/p") " move  "
   (mevedel-tool-ask-ui--key "1-9") " focus  "
   (mevedel-tool-ask-ui--key "RET") " pick  "
   (mevedel-tool-ask-ui--key "c") " custom  "
   (mevedel-tool-ask-ui--key "C-v/M-v") " scroll sample\n"
   (mevedel-tool-ask-ui--key "C-c C-c") " submit  "
   (mevedel-tool-ask-ui--key "q") " cancel  "
   (mevedel-tool-ask-ui--key "C-c C-k") " abort run\n"
   (mevedel-tool-ask-ui--divider)))

(defun mevedel-tool-ask-ui--form-body (state)
  "Return the rendered questionnaire body for STATE."
  (concat
   "\n"
   (mevedel-tool-ask-ui--header state)
   "\n"
   (mapconcat
    (lambda (index)
      (mevedel-tool-ask-ui--question-block state index))
    (number-sequence 0 (1- (mevedel-tool-ask-ui--state-count state)))
    "\n\n")
   "\n"
   (mevedel-tool-ask-ui--footer)))

(defun mevedel-tool-ask-ui--render (state announce)
  "Paint STATE and announce it remotely when ANNOUNCE is non-nil."
  (with-current-buffer (mevedel-tool-ask-ui--state-chat-buffer state)
    (setf
     (mevedel-tool-ask-ui--state-overlay state)
     (mevedel-view--interaction-register
      (list
       :kind 'ask
       :id (mevedel-tool-ask-ui--state-interaction-id state)
       :origin (mevedel-tool-ask-ui--state-origin state)
       :count (mevedel-tool-ask-ui--state-count state)
       :body
       (concat
        (mevedel--prompt-attribution-line
         (mevedel-tool-ask-ui--state-origin state))
        (mevedel-tool-ask-ui--form-body state))
       :priority 150
       :keymap (mevedel-tool-ask-ui--keymap state)
       :help-echo "Ask prompt")))
    (let ((overlay (mevedel-tool-ask-ui--state-overlay state))
          (count (mevedel-tool-ask-ui--state-count state)))
      (overlay-put overlay 'mevedel--callback
                   (mevedel-tool-ask-ui--state-callback state))
      (overlay-put
       overlay 'mevedel--remote
       (list
        :body
        (format "Ask · %d question%s" count (if (= count 1) "" "s"))
        :questions
        (lambda ()
          (mevedel-tool-ask-ui--remote-questions state))
        :answer
        (lambda (submitted)
          (mevedel-tool-ask-ui--dispatch state 'remote-answer submitted))
        :cancel
        (lambda ()
          (mevedel-tool-ask-ui--dispatch state 'cancel))))
      (cl-pushnew overlay mevedel--prompt-overlays :test #'eq)
      (mevedel--prompt--register-canceller
       (mevedel-tool-ask-ui--state-source-buffer state) overlay)
      (when announce
        (mevedel--prompt-announce overlay)))))

(defun mevedel-tool-ask-ui--refresh (state &optional announce)
  "Render STATE, announcing it remotely when ANNOUNCE is non-nil."
  (mevedel-tool-ask-ui--render state announce)
  (mevedel-tool-ask-ui--sync-sample state))

(cl-defun mevedel-tool-ask-ui-show (callback questions)
  "Show QUESTIONS as one form and settle it through CALLBACK."
  (mevedel-tools--validate-params callback mevedel-tool-ask-ui-show
    (questions (vectorp . "array")))
  (require 'mevedel-directive-frame)
  (let* ((source-buffer (current-buffer))
         (questions-list (append questions nil))
         (state
          (mevedel-tool-ask-ui--state-create
           :callback callback
           :source-buffer source-buffer
           :origin (mevedel-current-origin)
           :questions questions-list
           :count (length questions-list)
           :answers (make-vector (length questions-list) nil)
           :chat-buffer
           (or
            (mevedel-view--interaction-target-buffer
             (with-current-buffer source-buffer
               (mevedel--prompt--data-buffer)))
            (error "No live view for Ask prompt"))
           :interaction-id (list :ask (gensym "ask-"))
           :focus-question 0
           :focus-option 0)))
    (mevedel-tool-ask-ui--enter-question state 0)
    (mevedel-tool-ask-ui--refresh state t)))

(provide 'mevedel-tool-ask-ui)
;;; mevedel-tool-ask-ui.el ends here
