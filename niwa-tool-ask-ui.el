;;; niwa-tool-ask-ui.el -- Ask questionnaire UI -*- lexical-binding: t -*-

;;; Commentary:

;; Stateful Ask questionnaire presentation and interaction controllers.

;;; Code:

;; `cl-find', `cl-position' and `cl-count-if' live in `cl-seq' and carry no
;; autoloads, so they have to be present at runtime, not only at compile time.
(require 'cl-lib)
(require 'niwa-utilities)

(eval-when-compile
  (require 'niwa-tool-registry))

;; `C source code' (graphical builds only)
(defvar overflow-newline-into-fringe)

;; `niwa-chat'
(declare-function niwa-abort "niwa-chat" (&optional buf))
(autoload 'niwa-abort "niwa-chat" nil t)

;; `niwa-interaction-prompt'
(declare-function niwa--prompt--data-buffer "niwa-interaction-prompt"
                  (&optional buffer))
(declare-function niwa--prompt--register-canceller
                  "niwa-interaction-prompt"
                  (&optional source-buffer overlay))
(declare-function niwa--prompt--settle "niwa-interaction-prompt"
                  (overlay outcome))
(declare-function niwa--prompt-announce
                  "niwa-interaction-prompt" (overlay))
(declare-function niwa--prompt-attribution-line
                  "niwa-interaction-prompt" (origin))
(defvar niwa--prompt-overlays)
(autoload 'niwa--prompt--data-buffer "niwa-interaction-prompt")
(autoload 'niwa--prompt--register-canceller "niwa-interaction-prompt")
(autoload 'niwa--prompt--settle "niwa-interaction-prompt")
(autoload 'niwa--prompt-announce "niwa-interaction-prompt")
(autoload 'niwa--prompt-attribution-line "niwa-interaction-prompt")

;; `niwa-turn'
(declare-function niwa-current-origin "niwa-turn" ())
(autoload 'niwa-current-origin "niwa-turn")

;; `niwa-view-fontify'
(declare-function niwa-view--fontify-as "niwa-view-fontify" (text mode))
(autoload 'niwa-view--fontify-as "niwa-view-fontify")

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-register
                  "niwa-view-interaction"
                  (descriptor))
(declare-function niwa-view--interaction-target-buffer
                  "niwa-view-interaction"
                  (&optional data-buffer))
(autoload 'niwa-view--interaction-register "niwa-view-interaction")
(autoload 'niwa-view--interaction-target-buffer
  "niwa-view-interaction")

;; `niwa-view-markdown'
(declare-function niwa-view--source-block-font-lock-face
                  "niwa-view-markdown" (face))
(autoload 'niwa-view--source-block-font-lock-face "niwa-view-markdown")

;; `niwa-view-zone'
(declare-function niwa-view-zone-width "niwa-view-zone" ())
(defvar niwa-view-zone-width-change-functions)
(autoload 'niwa-view-zone-width "niwa-view-zone")


;;
;;; Options

(defconst niwa-tool-ask-ui--recommended-suffix " (Recommended)"
  "Suffix marking a recommended Ask option.")

(defconst niwa-tool-ask-ui--custom-label "Custom input"
  "Label of the free-text entry every question carries.")

(defconst niwa-tool-ask-ui--no-preference
  "(no preference -- use your judgment)"
  "Answer recorded for a question submitted without a choice.
A questionnaire may be submitted incomplete, and the model has to be
able to tell that from an answered question without reading a nil.")

(defun niwa-tool-ask-ui--option-field (option key)
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

(defun niwa-tool-ask-ui--option-label (option)
  "Return the trimmed answer label for OPTION."
  (let ((label (if (stringp option)
                   option
                 (niwa-tool-ask-ui--option-field option :label))))
    (cond
     ((stringp label) (string-trim label))
     (label (format "%s" label))
     (t (format "%s" option)))))

(defun niwa-tool-ask-ui--option-description (option)
  "Return OPTION's description string, or nil."
  (let ((description
         (and (not (stringp option))
              (niwa-tool-ask-ui--option-field option :description))))
    (cond
     ((and (stringp description)
           (not (string-blank-p description)))
      description)
     (description (format "%s" description)))))

(defun niwa-tool-ask-ui--option-sample (option)
  "Return OPTION's sample string, or nil.
The sample is the model-authored artifact shown in its question's
sample panel; `preview' names an interaction kind elsewhere in niwa
and is deliberately not reused here."
  (let ((sample (and (not (stringp option))
                     (niwa-tool-ask-ui--option-field option :sample))))
    (cond
     ((and (stringp sample)
           (not (string-blank-p sample)))
      sample)
     (sample (format "%s" sample)))))

(defun niwa-tool-ask-ui--option-by-label (label options)
  "Return first option in OPTIONS whose label equals LABEL."
  (cl-find label options
           :test #'equal
           :key #'niwa-tool-ask-ui--option-label))

(defun niwa-tool-ask-ui--selected-sample (answer options)
  "Return the sample of the OPTIONS entry ANSWER selected, or nil."
  (when-let* ((option (niwa-tool-ask-ui--option-by-label answer options)))
    (niwa-tool-ask-ui--option-sample option)))

(defun niwa-tool-ask-ui--format-option (option)
  "Return OPTION formatted for display in an Ask prompt."
  (let ((label (niwa-tool-ask-ui--option-label option)))
    (if (string-suffix-p niwa-tool-ask-ui--recommended-suffix label)
        (let ((base (substring label 0
                               (- (length label)
                                  (length niwa-tool-ask-ui--recommended-suffix)))))
          (concat base
                  (propertize niwa-tool-ask-ui--recommended-suffix
                              'font-lock-face 'success)))
      label)))

(defun niwa-tool-ask-ui--key (key)
  "Return KEY with a quiet, unboxed emphasis."
  (propertize key 'font-lock-face 'default))


;;
;;; Ask form state and controllers

(cl-defstruct
    (niwa-tool-ask-ui--state
     (:constructor niwa-tool-ask-ui--state-create))
  "Mutable state of one Ask questionnaire.
ANSWERS holds strings for single-select questions, lists of strings for
multi-select questions, and nil for unanswered questions.
SAMPLE-SHOWN and SAMPLE-OFFSETS are alists from question index to the
option its sample panel shows and to the panel's first visible row.
SAMPLE-CACHE maps sample strings to their fontified lines; SAMPLE-ROWS
is (WIDTH . TABLE), mapping samples to their rows at WIDTH.  HEIGHT is
(WIDTH . ROWS), the form's reserved height in screen rows at WIDTH."
  callback source-buffer origin questions count answers chat-buffer
  interaction-id overlay open
  sample-shown sample-offsets sample-cache sample-rows height)

(defvar-local niwa-tool-ask-ui--states nil
  "Live Ask forms in this buffer.")

(defvar-local niwa-tool-ask-ui--active-state nil
  "Ask form containing point at the last command.")

(defvar-local niwa-tool-ask-ui--active-entry nil
  "Entry identity at the last command, not a separate selection cursor.")

(defun niwa-tool-ask-ui--entry-at-point (state)
  "Return the entry at point inside STATE's live overlay, or nil."
  (let ((overlay (niwa-tool-ask-ui--state-overlay state)))
    (when (and (overlayp overlay)
               (eq (overlay-buffer overlay) (current-buffer))
               (<= (overlay-start overlay) (point))
               (< (point) (overlay-end overlay)))
      (or (get-text-property (point) 'niwa-ask-entry)
          ;; The newline ending a row belongs to it, so RET after C-e
          ;; still answers the row point is on.
          (and (eq (char-after) ?\n)
               (< (overlay-start overlay) (point))
               (get-text-property (1- (point)) 'niwa-ask-entry))))))

(defun niwa-tool-ask-ui--entry-positions (state)
  "Return ordered (IDENTITY . POSITION) pairs for STATE's visible entries."
  (let* ((overlay (niwa-tool-ask-ui--state-overlay state))
         (position (overlay-start overlay))
         (end (overlay-end overlay)) entries)
    (while (and position (< position end))
      (when-let* ((entry (get-text-property position 'niwa-ask-entry)))
        (push (cons entry (save-excursion
                            (goto-char position)
                            (skip-chars-forward " " (line-end-position))
                            (point)))
              entries))
      (setq position (next-single-property-change
                      position 'niwa-ask-entry nil end)))
    (nreverse entries)))

(defun niwa-tool-ask-ui--goto-entry (state identity)
  "Move point to IDENTITY in STATE, without repainting the form."
  (when-let* ((position (cdr (assoc identity
                                  (niwa-tool-ask-ui--entry-positions state)))))
    (goto-char position))
  (niwa-tool-ask-ui--post-command))

(defun niwa-tool-ask-ui--question-heads (state)
  "Return the first entry of each question in STATE, in form order.
Each element is (QUESTION IDENTITY . POSITION); Submit is its own last
group, named `submit'."
  (let (heads)
    (pcase-dolist (`(,identity . ,position)
                   (niwa-tool-ask-ui--entry-positions state))
      (let ((group (if (consp identity) (car identity) identity)))
        (unless (assoc group heads)
          (push (cons group (cons identity position)) heads))))
    (nreverse heads)))

(defun niwa-tool-ask-ui--post-command ()
  "Track the entry at point and show its sample, whatever moved point."
  ;; Emacs drops a `post-command-hook' function that signals; a broken
  ;; render must not silently stop samples following point for good.
  (let ((scope (niwa-diagnostic-scope (niwa--prompt--data-buffer))))
    (condition-case err
        (let* ((state (cl-find-if #'niwa-tool-ask-ui--entry-at-point
                                  niwa-tool-ask-ui--states))
               (entry (and state (niwa-tool-ask-ui--entry-at-point state))))
          (unless (and (eq state niwa-tool-ask-ui--active-state)
                       (equal entry niwa-tool-ask-ui--active-entry))
            (setq niwa-tool-ask-ui--active-state state
                  niwa-tool-ask-ui--active-entry entry)
            (when state
              (niwa-tool-ask-ui--sync-sample state entry))))
      (error
       (niwa-diagnostic-message scope "Ask sample sync failed: %S" err)))))

(defun niwa-tool-ask-ui--settled-p (state)
  "Return non-nil once STATE's form has been settled.
A minibuffer answer can return after a run abort or a guest answer
settled the form; nothing may repaint or record into it then."
  (when-let* ((overlay (niwa-tool-ask-ui--state-overlay state)))
    (overlay-get overlay 'niwa-settled)))

(defun niwa-tool-ask-ui--retire (state)
  "Remove STATE's motion tracking after settlement."
  (when (buffer-live-p (niwa-tool-ask-ui--state-chat-buffer state))
    (with-current-buffer (niwa-tool-ask-ui--state-chat-buffer state)
      (setq niwa-tool-ask-ui--states
            (delq state niwa-tool-ask-ui--states))
      (niwa-tool-ask-ui--post-command)
      (unless niwa-tool-ask-ui--states
        (remove-hook 'post-command-hook
                     #'niwa-tool-ask-ui--post-command t)))))

(defun niwa-tool-ask-ui--question-options (state index)
  "Return option objects for question INDEX in STATE."
  (append (plist-get
           (nth index (niwa-tool-ask-ui--state-questions state))
           :options)
          nil))

(defun niwa-tool-ask-ui--multi-p (state index)
  "Return non-nil when question INDEX in STATE allows multiple answers."
  (eq t (plist-get (nth index (niwa-tool-ask-ui--state-questions state))
                  :multiSelect)))

(defun niwa-tool-ask-ui--normalize-answer (state index answer)
  "Validate and normalize question INDEX's ANSWER in STATE.
Single answers are strings; multi answers are lists of strings, ordered
by offered options followed by at most one custom text.  Blank means no
preference.  Reject malformed responses before they can settle the form."
  (if (not (niwa-tool-ask-ui--multi-p state index))
      (progn
        (unless (stringp answer)
          (user-error "Question %d needs one answer string" (1+ index)))
        (unless (string-blank-p answer) (string-trim answer)))
    (unless (and (proper-list-p answer) (cl-every #'stringp answer))
      (user-error "Question %d needs an array of answer strings" (1+ index)))
    (let* ((values (delq nil (mapcar (lambda (text)
                                     (unless (string-blank-p text)
                                       (string-trim text))) answer)))
           (labels (mapcar #'niwa-tool-ask-ui--option-label
                           (niwa-tool-ask-ui--question-options state index)))
           (custom (cl-remove-if (lambda (text) (member text labels)) values)))
      (unless (= (length values) (length (delete-dups (copy-sequence values))))
        (user-error "Question %d has duplicate answers" (1+ index)))
      (when (> (length custom) 1)
        (user-error "Question %d allows only one custom answer" (1+ index)))
      (append (cl-remove-if-not (lambda (label) (member label values)) labels)
              custom))))

(defun niwa-tool-ask-ui--entry-count (state index)
  "Return the number of focusable entries for question INDEX in STATE."
  (1+ (length (niwa-tool-ask-ui--question-options state index))))

(defun niwa-tool-ask-ui--custom-entry-p (state index option)
  "Return non-nil when OPTION is question INDEX's custom entry in STATE."
  (= option (length (niwa-tool-ask-ui--question-options state index))))

(defun niwa-tool-ask-ui--entry-option (state entry)
  "Return the option object ENTRY names in STATE, or nil.
Custom rows, summaries and Submit name no option."
  (pcase entry
    (`(,question . ,(and (pred integerp) option))
     (nth option (niwa-tool-ask-ui--question-options state question)))))

(defun niwa-tool-ask-ui--question-open-p (state index)
  "Return non-nil when question INDEX of STATE shows its options.
At most one question is open; the others are one line each."
  (eql index (niwa-tool-ask-ui--state-open state)))

(defun niwa-tool-ask-ui--open-question (state index)
  "Open question INDEX of STATE and put point on its answer or first option.
Opening is always explicit, so the layout changes only on request."
  (let ((identity (cons index (or (niwa-tool-ask-ui--answered-entry
                                   state index)
                                  0))))
    (if (niwa-tool-ask-ui--question-open-p state index)
        (niwa-tool-ask-ui--goto-entry state identity)
      (setf (niwa-tool-ask-ui--state-open state) index)
      (niwa-tool-ask-ui--refresh state nil identity))))

(defun niwa-tool-ask-ui--answered-entry (state index)
  "Return the entry selected by question INDEX's answer in STATE."
  (let ((answer (aref (niwa-tool-ask-ui--state-answers state) index))
        (options (niwa-tool-ask-ui--question-options state index)))
    (when (niwa-tool-ask-ui--multi-p state index)
      (setq answer (car answer)))
    (when answer
      (or (cl-position answer options
                       :test #'equal
                       :key #'niwa-tool-ask-ui--option-label)
          (length options)))))

(defun niwa-tool-ask-ui--answered-count (state)
  "Return the number of answered questions in STATE."
  (cl-count-if #'identity
               (niwa-tool-ask-ui--state-answers state)))

(defun niwa-tool-ask-ui--next-unanswered (state from)
  "Return the next unanswered question index in STATE after FROM, or nil.
The search wraps around to questions before FROM."
  (let ((count (niwa-tool-ask-ui--state-count state))
        (answers (niwa-tool-ask-ui--state-answers state)))
    (cl-loop for step from 1 to count
             for index = (% (+ from step) count)
             unless (aref answers index) return index)))

(defun niwa-tool-ask-ui--submission-text (state)
  "Return STATE's complete model-facing answer text."
  (with-temp-buffer
    (insert "User answered the following questions:\n\n")
    (dotimes (index (niwa-tool-ask-ui--state-count state))
      (let ((question
             (plist-get
              (nth index (niwa-tool-ask-ui--state-questions state))
              :question))
            (answer (aref (niwa-tool-ask-ui--state-answers state) index)))
        (insert (format "Q%d: %s\n" (1+ index) question))
        (if (and answer (niwa-tool-ask-ui--multi-p state index))
            (insert (format "A%d:\n%s\n" (1+ index)
                            (mapconcat (lambda (text) (concat "- " text)) answer "\n")))
          (insert (format "A%d: %s\n" (1+ index)
                          (or answer niwa-tool-ask-ui--no-preference))))
        (dolist (label (if (listp answer) answer (list answer)))
          (when-let* ((label)
                      (sample (niwa-tool-ask-ui--selected-sample
                               label (niwa-tool-ask-ui--question-options state index))))
            (insert (if (niwa-tool-ask-ui--multi-p state index)
                        (format "Sample shown for A%d (%s):\n%s\n" (1+ index) label sample)
                      (format "Sample shown for A%d:\n%s\n" (1+ index) sample)))))
        (insert "\n")))
    (buffer-string)))

(defun niwa-tool-ask-ui--remote-questions (state)
  "Return STATE as JSON-safe questions with current answers."
  (cl-loop
   for question in (niwa-tool-ask-ui--state-questions state)
   for index from 0
   for answer = (aref (niwa-tool-ask-ui--state-answers state) index)
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
                 (niwa-tool-ask-ui--option-description option))
                (sample (niwa-tool-ask-ui--option-sample option)))
            (append
             (list (cons "label"
                         (niwa-tool-ask-ui--option-label option)))
             (when description
               (list (cons "description" description)))
             (when sample (list (cons "sample" sample))))))
        (niwa-tool-ask-ui--question-options state index)))))
    (if (niwa-tool-ask-ui--multi-p state index)
        (list (cons "multiSelect" t) (cons "answer" (vconcat answer)))
      (when answer (list (cons "answer" answer)))))))

(defun niwa-tool-ask-ui--sync-sample (state entry)
  "Show the sample of ENTRY, the row at point, in its question's panel.
STATE holds the form.
Repaints only when point reaches a sample-bearing option its panel does
not already show; all other motion leaves the form untouched."
  (pcase-let ((`(,question . ,option) entry))
    (when (and (niwa-tool-ask-ui--option-sample
                (niwa-tool-ask-ui--entry-option state entry))
               (not (eql option (niwa-tool-ask-ui--shown-sample
                                 state question))))
      (setf (alist-get question
                       (niwa-tool-ask-ui--state-sample-shown state))
            option
            (alist-get question
                       (niwa-tool-ask-ui--state-sample-offsets state)
                       nil t)
            nil)
      (niwa-tool-ask-ui--refresh state))))


;;
;;; Sample panel

(defcustom niwa-ask-sample-height 12
  "Most sample rows an Ask question shows at once.
A question's sample panel is as tall as its longest sample up to this
limit, so moving between its options never shifts the form.  Longer
samples scroll with \\`C-v' and \\`M-v'."
  :type 'natural
  :group 'niwa)

(defun niwa-tool-ask-ui--sample-width (state)
  "Return the available sample row width for STATE's view buffer."
  (max 4 (- (niwa-tool-ask-ui--form-width state) 7)))

(defun niwa-tool-ask-ui--form-width (state)
  "Return the narrowest width of the windows showing STATE's view.
Fitting the narrowest keeps every row one screen row in all of them."
  (with-current-buffer (niwa-tool-ask-ui--state-chat-buffer state)
    (niwa-view-zone-width)))

(defun niwa-tool-ask-ui--wrap-width (state)
  "Return the text columns a wrapped row of STATE's form holds.
Each window showing the view holds its body width, less the last column
when it has no right fringe, as on a terminal, to carry the continuation
glyph; a row filling such a window wraps.  Measure wrapped text against
the smallest of these capacities.  Without a window, assume the selected
one has no fringe."
  (let ((windows (get-buffer-window-list
                  (niwa-tool-ask-ui--state-chat-buffer state) nil t)))
    (if windows
        (let ((capacity most-positive-fixnum))
          (dolist (window windows capacity)
            (setq capacity
                  (min capacity
                       (if (and (display-graphic-p (window-frame window))
                                overflow-newline-into-fringe
                                (> (or (nth 1 (window-fringes window)) 0) 0))
                           (window-body-width window)
                         (1- (window-body-width window)))))))
      (1- (niwa-tool-ask-ui--form-width state)))))

(defun niwa-tool-ask-ui--wrap-sample-line (line width)
  "Split fontified LINE into rows at WIDTH, marking continuation rows.
Prefer whitespace boundaries, preserving all text and its properties.
Long words break only between glyphs.  An oversized glyph gets its own
row, even if it exceeds WIDTH, so wrapping always makes progress.
Expand tabs so their width is independent of the panel's indentation.
Column widths are conservative for composed glyphs such as emoji.
Always returns a fresh list."
  (when (string-match-p "\t" line)
    (setq line (with-temp-buffer
                 (insert line)
                 (untabify (point-min) (point-max))
                 (buffer-string))))
  (if (<= (string-width line) width)
      (list line)
    (let ((glyphs (mapcar
                   (lambda (glyph) (cons glyph (string-width glyph)))
                   ;; Automatic composition needs a displayed buffer, including
                   ;; when a hidden view is rendered before it is first shown.
                   (with-current-buffer
                       (window-buffer (or (get-buffer-window (current-buffer) t)
                                          (selected-window)))
                     (string-glyph-split line))))
          (prefix "") rows)
      (while glyphs
        (let ((columns (string-width prefix)) row word boundary rest)
          ;; Accept at least one whole glyph, even when it cannot fit.
          (while (and glyphs
                      (or (null row) (<= (+ columns (cdar glyphs)) width)))
            (let ((glyph (pop glyphs)))
              (setq columns (+ columns (cdr glyph)))
              (push (car glyph) row)
              (if (string-match-p "\\`[ \t]+\\'" (car glyph))
                  (when word (setq boundary row rest glyphs))
                (setq word t))))
          (when glyphs
            (if boundary
                (setq row boundary glyphs rest)
              ;; A space just beyond a full word belongs to that row, not
              ;; after the continuation marker.  Keep all original text.
              (when word
                (while (and glyphs
                            (string-match-p "\\`[ \t]+\\'" (caar glyphs)))
                  (push (car (pop glyphs)) row)))))
          (push (concat prefix (apply #'concat (nreverse row))) rows)
          (setq prefix (propertize "↪ " 'font-lock-face 'shadow))))
      (nreverse rows))))

(defun niwa-tool-ask-ui--sample-lines (state sample width)
  "Return SAMPLE fontified and wrapped to WIDTH, cached in STATE.
Moving between options repaints the panel, so each sample is fontified
once per form and wrapped once per width.  Only the current width's rows
are kept, so resizing does not accumulate them."
  (let ((fontified (or (niwa-tool-ask-ui--state-sample-cache state)
                       (setf (niwa-tool-ask-ui--state-sample-cache state)
                             (make-hash-table :test #'equal))))
        (rows (niwa-tool-ask-ui--state-sample-rows state)))
    (unless (eql (car rows) width)
      (setq rows (setf (niwa-tool-ask-ui--state-sample-rows state)
                       (cons width (make-hash-table :test #'equal)))))
    (with-memoization (gethash sample (cdr rows))
      ;; `append', not `mapcan': splicing in place would make cached rows
      ;; share structure with whatever a line's wrapping returned.
      (apply #'append
             (mapcar
              (lambda (line)
                (niwa-tool-ask-ui--wrap-sample-line line width))
              (with-memoization (gethash sample fontified)
                (split-string
                 (niwa-view--fontify-as (string-trim-right sample)
                                           'markdown-mode)
                 "\n")))))))

(defun niwa-tool-ask-ui--shown-sample (state index)
  "Return the option index whose sample question INDEX's panel shows.
STATE holds the form.  The panel keeps the sample-bearing option point
visited last, so leaving the question or moving onto an option without
a sample changes nothing.  Before any visit it shows the answer's
sample, else the first one.  Nil when the question has no samples."
  (let ((options (niwa-tool-ask-ui--question-options state index)))
    (or (alist-get index (niwa-tool-ask-ui--state-sample-shown state))
        (when-let* ((answered (niwa-tool-ask-ui--answered-entry
                               state index))
                    ((niwa-tool-ask-ui--option-sample
                      (nth answered options))))
          answered)
        (cl-position-if #'niwa-tool-ask-ui--option-sample options))))

(defun niwa-tool-ask-ui--sample-view (state index)
  "Return what question INDEX's sample panel shows in STATE, or nil.
The value is (OPTION LINES HEIGHT OFFSET): the shown option index, its
sample's fontified rows, the panel height, and the first visible row.
HEIGHT covers the question's longest sample, not the shown one."
  (when-let* ((option (niwa-tool-ask-ui--shown-sample state index)))
    (let* ((width (niwa-tool-ask-ui--sample-width state))
           (options (niwa-tool-ask-ui--question-options state index))
           (lines (niwa-tool-ask-ui--sample-lines
                   state (niwa-tool-ask-ui--option-sample
                          (nth option options)) width))
           (height
            (max 1 (min niwa-ask-sample-height
                        (cl-loop
                         for candidate in options
                         for sample = (niwa-tool-ask-ui--option-sample
                                       candidate)
                         when sample
                         maximize (length (niwa-tool-ask-ui--sample-lines
                                           state sample width)))))))
      (list option lines height
            (min (alist-get index
                            (niwa-tool-ask-ui--state-sample-offsets state)
                            0)
                 (max 0 (- (length lines) height)))))))

(defun niwa-tool-ask-ui--sample-row (text)
  "Return TEXT as one sample panel row, terminating newline included.
The panel face is layered under TEXT's own fontification, as in
rendered source blocks; the newline carries it to the window edge."
  (let* ((row (concat " " text "\n"))
         (end (length row))
         (position 0))
    (while (< position end)
      (let ((next (or (next-single-property-change
                       position 'font-lock-face row end)
                      end)))
        (put-text-property
         position next 'font-lock-face
         (niwa-view--source-block-font-lock-face
          (get-text-property position 'font-lock-face row))
         row)
        (setq position next)))
    (concat "     " (propertize row 'wrap-prefix "      "))))

(defun niwa-tool-ask-ui--sample-panel (state index)
  "Return question INDEX's sample panel for STATE, or the empty string.
The panel is a title row naming the shown option followed by exactly
its question's panel height of sample rows."
  (pcase (niwa-tool-ask-ui--sample-view state index)
    (`(,option ,lines ,height ,offset)
     (let* ((total (length lines))
            (width (niwa-tool-ask-ui--sample-width state))
            (info (if (> total height)
                      (truncate-string-to-width
                       (format "  ·  rows %d–%d of %d  C-v/M-v"
                               (1+ offset) (+ offset height) total)
                       width nil nil "…")
                    "")))
       (concat
        (niwa-tool-ask-ui--sample-row
         (concat
          (when (> width (string-width info))
            (truncate-string-to-width
             (concat
              (propertize "Sample · " 'font-lock-face 'shadow)
              (niwa-tool-ask-ui--format-option
               (nth option (niwa-tool-ask-ui--question-options state index))))
             (- width (string-width info)) nil nil "…"))
          (propertize info 'font-lock-face 'shadow)))
        (mapconcat #'niwa-tool-ask-ui--sample-row
                   (cl-loop for line from offset below (+ offset height)
                            collect (or (nth line lines) ""))
                   ""))))
    (_ "")))

(defun niwa-tool-ask-ui--sample-scroll (state pages)
  "Scroll the sample panel at point in STATE by PAGES.
Without an overflowing visible panel, scroll the window instead, so
\\`C-v' and \\`M-v' keep their usual meaning elsewhere in the form."
  (pcase-let* ((index (get-text-property (point) 'niwa-ask-question))
               (`(,_ ,lines ,height ,offset)
                (and (integerp index)
                     (niwa-tool-ask-ui--question-open-p state index)
                     (niwa-tool-ask-ui--sample-view state index))))
    (if (not (and lines (> (length lines) height)))
        (call-interactively
         (if (> pages 0) #'scroll-up-command #'scroll-down-command))
      (let ((target (max 0 (min (- (length lines) height)
                                (+ offset (* pages (max 1 (1- height))))))))
        (unless (= target offset)
          (setf (alist-get index
                           (niwa-tool-ask-ui--state-sample-offsets state))
                target)
          (niwa-tool-ask-ui--refresh state))))))

(defun niwa-tool-ask-ui--dispatch (state action &optional value)
  "Apply ACTION with optional VALUE to Ask STATE.
Actions act on the entry at point; `record' takes (QUESTION . ANSWER)
instead, because a minibuffer answer returns after point may have moved.
A settled form ignores every action."
  (with-current-buffer (niwa-tool-ask-ui--state-chat-buffer state)
    (unless (niwa-tool-ask-ui--settled-p state)
      (niwa-tool-ask-ui--post-command)
      (let* ((identity (niwa-tool-ask-ui--entry-at-point state))
             (question (or (and (consp identity) (car identity))
                           (get-text-property (point) 'niwa-ask-question)))
             (option (and (consp identity) (cdr identity))))
        (pcase action
          ((or 'next 'previous)
           ;; Measure from the row's glyph, not point: a row's identity
           ;; starts at its indentation, so from column 0 `next' would
           ;; otherwise find the row point is already on.
           (let* ((entries (niwa-tool-ask-ui--entry-positions state))
                  (from (or (cdr (assoc identity entries)) (point)))
                  (target (if (eq action 'next)
                              (cl-find-if (lambda (entry) (> (cdr entry) from))
                                          entries)
                            (cl-find-if (lambda (entry) (< (cdr entry) from))
                                        (reverse entries)))))
             (when target
               (niwa-tool-ask-ui--goto-entry state (car target)))))
          ((or 'next-question 'previous-question)
           (let* ((heads (niwa-tool-ask-ui--question-heads state))
                  (index (cl-position (or question identity) heads
                                      :key #'car :test #'equal))
                  (target
                   (if (eq action 'next-question)
                       (if index
                           (nth (1+ index) heads)
                         (cl-find-if (lambda (head) (> (cddr head) (point)))
                                     heads))
                     (if index
                         (and (> index 0) (nth (1- index) heads))
                       (cl-find-if (lambda (head) (< (cddr head) (point)))
                                   (reverse heads))))))
             (when target
               (if (integerp (car target))
                   (niwa-tool-ask-ui--open-question state (car target))
                 (niwa-tool-ask-ui--goto-entry state (cadr target))))))
          ('focus
           ;; Digits pick among the open question's options, from its
           ;; heading too; the custom row is reached with `c'.
           (when (and (integerp question)
                      (niwa-tool-ask-ui--question-open-p state question)
                      (< value (length (niwa-tool-ask-ui--question-options
                                        state question))))
             (niwa-tool-ask-ui--goto-entry state (cons question value))))
          ('pick
           (cond
            ((eq identity 'submit) (niwa-tool-ask-ui--dispatch state 'submit))
            ((eq option 'summary)
             (niwa-tool-ask-ui--open-question state question))
            ((integerp option)
             (if (niwa-tool-ask-ui--custom-entry-p state question option)
                 (niwa-tool-ask-ui--dispatch state 'custom)
               (let* ((label (niwa-tool-ask-ui--option-label
                              (niwa-tool-ask-ui--entry-option state identity)))
                      (answer (aref (niwa-tool-ask-ui--state-answers state) question)))
                 (niwa-tool-ask-ui--dispatch
                  state 'record
                  (cons question
                        (if (niwa-tool-ask-ui--multi-p state question)
                            (if (member label answer)
                                (remove label answer)
                              (append answer (list label)))
                          label))))))))
          ('custom
           (when (and (integerp question)
                      (niwa-tool-ask-ui--question-open-p state question))
             (let* ((multi (niwa-tool-ask-ui--multi-p state question))
                    (old (aref (niwa-tool-ask-ui--state-answers state) question))
                    (options (niwa-tool-ask-ui--question-options state question))
                    (custom (and multi (cl-find-if-not
                                        (lambda (text) (niwa-tool-ask-ui--option-by-label text options))
                                        old)))
                    (answer (read-string (format "Answer %d: " (1+ question))
                                         (if multi custom old))))
               (when (or multi (not (string-blank-p answer)))
                 (niwa-tool-ask-ui--dispatch state 'record
                  (cons question (if multi
                                     (delete-dups
                                      (append (remove custom old)
                                              (unless (string-blank-p answer)
                                                (list (string-trim answer)))))
                                   answer)))))))
          ('record
           (pcase-let ((`(,answered . ,answer) value))
             (aset (niwa-tool-ask-ui--state-answers state) answered
                   (niwa-tool-ask-ui--normalize-answer state answered answer))
             (if (niwa-tool-ask-ui--multi-p state answered)
                 (niwa-tool-ask-ui--refresh state t)
               (let ((next (niwa-tool-ask-ui--next-unanswered state answered)))
                 (setf (niwa-tool-ask-ui--state-open state) next)
                 (niwa-tool-ask-ui--refresh
                  state t (if next (cons next 0) 'submit))))))
          ('submit
           (niwa--prompt--settle
            (niwa-tool-ask-ui--state-overlay state)
            (niwa-tool-ask-ui--submission-text state)))
          ('cancel
           (niwa--prompt--settle
            (niwa-tool-ask-ui--state-overlay state) 'aborted))
          ('abort
           (niwa-tool-ask-ui--dispatch state 'cancel)
           (niwa-abort))
          ('scroll (niwa-tool-ask-ui--sample-scroll state value))
          ('remote-answer
           (unless (and (proper-list-p value)
                        (= (length value) (niwa-tool-ask-ui--state-count state)))
             (user-error "Answer every question in the questionnaire response"))
           (let ((answers (cl-loop for answer in value for index from 0
                                   collect (niwa-tool-ask-ui--normalize-answer
                                            state index answer))))
             (setf (niwa-tool-ask-ui--state-answers state) (vconcat answers)))
           (niwa-tool-ask-ui--dispatch state 'submit)))))))

(defun niwa-tool-ask-ui--keymap (state)
  "Return the interaction keymap controlling STATE."
  (let ((keymap (make-sparse-keymap)))
    (dolist (binding '(("n" . next) ("C-n" . next) ("<down>" . next)
                       ("p" . previous) ("C-p" . previous) ("<up>" . previous)
                       ("TAB" . next-question) ("<backtab>" . previous-question)
                       ("RET" . pick) ("<return>" . pick)
                       ("c" . custom) ("C-c C-c" . submit)
                       ("q" . cancel) ("C-g" . cancel) ("C-c C-k" . abort)))
      (let ((action (cdr binding)))
        (define-key keymap (kbd (car binding))
                    (lambda ()
                      (interactive)
                      (niwa-tool-ask-ui--dispatch state action)))))
    (dotimes (digit 9)
      (let ((entry digit))
        (define-key
         keymap (kbd (number-to-string (1+ entry)))
         (lambda ()
           (interactive)
           (niwa-tool-ask-ui--dispatch state 'focus entry)))))
    (define-key keymap [mouse-1]
                (lambda (event)
                  (interactive "e")
                  (mouse-set-point event)
                  (niwa-tool-ask-ui--dispatch state 'pick)))
    (define-key
     keymap (kbd "C-v")
     (lambda ()
       (interactive)
       (niwa-tool-ask-ui--dispatch state 'scroll 1)))
    (define-key
     keymap (kbd "M-v")
     (lambda ()
       (interactive)
       (niwa-tool-ask-ui--dispatch state 'scroll -1)))
    keymap))

(defun niwa-tool-ask-ui--option-lines (state index entry)
  "Return rendered lines for ENTRY of question INDEX in STATE."
  (let* ((options (niwa-tool-ask-ui--question-options state index))
         (customp (niwa-tool-ask-ui--custom-entry-p state index entry))
         (option (unless customp (nth entry options)))
         (answer (aref (niwa-tool-ask-ui--state-answers state) index))
         (multi (niwa-tool-ask-ui--multi-p state index))
         (answers (if multi answer (and answer (list answer))))
         (custom (and customp
                      (cl-find-if-not
                       (lambda (text) (niwa-tool-ask-ui--option-by-label text options))
                       answers)))
         (selectedp
          (if customp
              custom
            (member (niwa-tool-ask-ui--option-label option) answers)))
         (description
          (and option (niwa-tool-ask-ui--option-description option)))
         (lines
          (list
           (concat
            "     "
            (if selectedp
                (propertize (if multi "☑" "●") 'font-lock-face 'success)
              (propertize (if multi "☐" "○") 'font-lock-face 'shadow))
            " "
            (propertize
             (if customp "c" (number-to-string (1+ entry)))
             'font-lock-face 'shadow)
            "  "
            (if customp
                (propertize
                 (concat
                  niwa-tool-ask-ui--custom-label "…"
                  (when (and multi custom)
                    (concat " "
                            (truncate-string-to-width
                             (replace-regexp-in-string "[ \t\n]+" " " custom)
                             (max 1 (- (niwa-tool-ask-ui--wrap-width state)
                                       12 (string-width niwa-tool-ask-ui--custom-label)))
                             nil nil "…"))))
                 'font-lock-face 'shadow)
              (niwa-tool-ask-ui--format-option option))))))
    (when description
      (setq lines
            (append lines
                    (list
                     (concat
                      "          "
                      (propertize
                       (replace-regexp-in-string "\n" "\n          " description t t)
                       'font-lock-face 'shadow))))))
    (list (propertize (string-join lines "\n")
                      'niwa-ask-entry (cons index entry)
                      'help-echo custom
                      'wrap-prefix "          "
                      'mouse-face 'highlight))))

(defun niwa-tool-ask-ui--question-block (state index)
  "Return question INDEX rendered for STATE, terminating newline included."
  (let* ((question
          (plist-get
           (nth index (niwa-tool-ask-ui--state-questions state))
           :question))
         (answer (aref (niwa-tool-ask-ui--state-answers state) index))
         (collapsed (not (niwa-tool-ask-ui--question-open-p state index)))
         (gutter
          (concat
           (if answer (propertize "✓ " 'font-lock-face 'success) "  ")
           (propertize (format "%d  " (1+ index)) 'font-lock-face 'bold)))
         (heading
          (propertize
           (if collapsed
               (niwa-tool-ask-ui--closed-heading state gutter question answer)
             (concat
              gutter
              (propertize
               (replace-regexp-in-string "\n" "\n     " (format "%s" question) t t)
               'font-lock-face 'bold)
              (when (niwa-tool-ask-ui--multi-p state index)
                (propertize " (select any)" 'font-lock-face 'shadow))))
           'wrap-prefix "     ")))
    (propertize
     (if collapsed
         (concat
          (propertize heading 'niwa-ask-entry (cons index 'summary)
                      'mouse-face 'highlight
                      'help-echo
                      (format "%s\n\nRET or click to %s" question
                              (if answer "change this answer" "answer")))
          "\n")
       (concat heading "\n"
               (mapconcat
                (lambda (entry)
                  (string-join (niwa-tool-ask-ui--option-lines state index entry) "\n"))
                (number-sequence 0 (1- (niwa-tool-ask-ui--entry-count state index)))
                "\n")
               "\n"
               (niwa-tool-ask-ui--sample-panel state index)))
     'niwa-ask-question index)))

(defun niwa-tool-ask-ui--closed-heading (state gutter question answer)
  "Return a closed question's one-line heading for STATE.
GUTTER holds its mark and number.  QUESTION and ANSWER are flattened
to one line and cut to fit the window, the answer keeping at least half
of the room; the full text is in the help echo and appears when the
question is opened.  Below about 25 columns the line still wraps."
  (let* ((flat (lambda (text)
                 (string-trim (replace-regexp-in-string
                               "[ \t\n]+" " " (format "%s" text)))))
         (text (funcall flat question))
         (reply (and answer (funcall flat (if (listp answer)
                                              (string-join answer ", ")
                                            answer))))
         (room (- (niwa-tool-ask-ui--form-width state) 1
                  (string-width gutter)))
         (reply-room
          (and reply
               (min (string-width reply)
                    (max 10 (/ (- room 4) 2)
                         (- room 4 (string-width text))))))
         (text-room (max 10 (- room (if reply (+ 4 reply-room) 0)))))
    (concat gutter
            (propertize (truncate-string-to-width text text-room nil nil "…")
                        'font-lock-face 'shadow)
            (when reply
              (concat "  → "
                      (propertize
                       (truncate-string-to-width reply reply-room nil nil "…")
                       'font-lock-face 'success))))))

(defun niwa-tool-ask-ui--footer (state)
  "Return the Ask interaction footer for STATE.
Question jumps are only advertised when there is another question."
  (let ((text
         (concat
          "  "
          (niwa-tool-ask-ui--key "n/p") " move  "
          (when (> (niwa-tool-ask-ui--state-count state) 1)
            (concat (niwa-tool-ask-ui--key "TAB") " question  "))
          (niwa-tool-ask-ui--key "1-9") " focus  "
          (niwa-tool-ask-ui--key "RET") " choose/toggle  "
          (niwa-tool-ask-ui--key "c") " custom  "
          (niwa-tool-ask-ui--key "C-c C-c") " submit  "
          (niwa-tool-ask-ui--key "q") " cancel\n")))
    (add-face-text-property 0 (length text) 'shadow t text)
    ;; Narrow windows wrap the hints; keep them under the first one.
    (propertize text 'wrap-prefix "  ")))

(defun niwa-tool-ask-ui--text-rows (text width)
  "Return how many screen rows TEXT takes in a window WIDTH columns wide.
Follows `word-wrap': a line breaks after whitespace before a word that
would cross the edge, a word wider than a row breaks inside, and every
continuation row starts with the line's `wrap-prefix'."
  (let ((rows 0)
        (start 0))
    (dolist (line (split-string text "\n"))
      (let* ((prefix (or (get-text-property start 'wrap-prefix text) ""))
             (indent (min (string-width prefix) (1- width)))
             (column 0)
             (row-start 0))
        (setq rows (1+ rows))
        (let ((position 0))
          ;; Each match is one run of whitespace and the word after it, so
          ;; it is never empty before the end of LINE.
          (while (and (< position (length line))
                      (string-match "\\([ \t]*\\)\\([^ \t]*\\)" line position))
            (let* ((spaces (string-width (match-string 1 line)))
                   (word (string-width (match-string 2 line)))
                   (end (match-end 0)))
              (setq column (+ column spaces))
              (when (and (> word 0) (> (+ column word) width)
                         (> column row-start))
                ;; The word starts a new row, breaking inside it when it is
                ;; wider than a continuation row.
                (setq rows (1+ rows)
                      column indent
                      row-start indent))
              (while (> (+ column word) width)
                (setq word (- word (- width column))
                      rows (1+ rows)
                      column indent
                      row-start indent))
              (setq column (+ column word)
                    position end))))
        (setq start (+ start (length line) 1))))
    rows))

(defun niwa-tool-ask-ui--reserved-rows (state)
  "Return the screen rows STATE's questions reserve at the current width.
That is the tallest its questions get with any one of them open or
none, so opening and answering never change the form's height.  Cached
per width."
  (let ((width (niwa-tool-ask-ui--wrap-width state))
        (cached (niwa-tool-ask-ui--state-height state)))
    (if (eql (car cached) width)
        (cdr cached)
      (let* ((open (niwa-tool-ask-ui--state-open state))
             (rows
              (unwind-protect
                  (cl-loop
                   for candidate in
                   (cons nil (number-sequence
                              0 (1- (niwa-tool-ask-ui--state-count state))))
                   maximize
                   (progn
                     (setf (niwa-tool-ask-ui--state-open state) candidate)
                     (niwa-tool-ask-ui--text-rows
                      (niwa-tool-ask-ui--form-questions state) width)))
                (setf (niwa-tool-ask-ui--state-open state) open))))
        (setf (niwa-tool-ask-ui--state-height state) (cons width rows))
        rows))))

(defun niwa-tool-ask-ui--form-body (state)
  "Return the rendered questionnaire body for STATE.
Blank lines between the questions and Submit fill the form to its
reserved height, so the questions keep their place at the top and
Submit and the hints keep theirs at the bottom."
  (let* ((questions (niwa-tool-ask-ui--form-questions state))
         (gap (- (niwa-tool-ask-ui--reserved-rows state)
                 (niwa-tool-ask-ui--text-rows
                  questions (niwa-tool-ask-ui--wrap-width state))))
         (body
          (concat
           questions
           (make-string (max 0 gap) ?\n)
           "\n"
           (propertize
            (concat
             (propertize "  [ Submit ]" 'font-lock-face 'bold)
             (propertize
              (format "  %d of %d answered"
                      (niwa-tool-ask-ui--answered-count state)
                      (niwa-tool-ask-ui--state-count state))
              'font-lock-face 'shadow))
            'niwa-ask-entry 'submit 'mouse-face 'highlight)
           "\n\n"
           (niwa-tool-ask-ui--footer state)))
         (first (text-property-not-all 0 (length body) 'niwa-ask-entry nil body)))
    (when first
      (while (eq (aref body first) ?\s)
        (setq first (1+ first)))
      (put-text-property first (1+ first) 'niwa-prompt-focus t body))
    body))

(defun niwa-tool-ask-ui--form-questions (state)
  "Return STATE's questions, each closed one a line and the open one in full."
  (concat
   "\n"
   ;; Closed questions stack as a list; a blank line sets off the open one.
   (mapconcat
    (lambda (index)
      (concat
       (when (and (> index 0)
                  (or (niwa-tool-ask-ui--question-open-p state index)
                      (niwa-tool-ask-ui--question-open-p state (1- index))))
         "\n")
       (niwa-tool-ask-ui--question-block state index)))
    (number-sequence 0 (1- (niwa-tool-ask-ui--state-count state)))
    "")))

(defun niwa-tool-ask-ui--render (state announce)
  "Paint STATE and announce it remotely when ANNOUNCE is non-nil."
  (with-current-buffer (niwa-tool-ask-ui--state-chat-buffer state)
    (setf
     (niwa-tool-ask-ui--state-overlay state)
     (niwa-view--interaction-register
      (list
       :kind 'ask
       :id (niwa-tool-ask-ui--state-interaction-id state)
       :origin (niwa-tool-ask-ui--state-origin state)
       :count (niwa-tool-ask-ui--state-count state)
       :body
       (concat
        (niwa--prompt-attribution-line
         (niwa-tool-ask-ui--state-origin state))
        (niwa-tool-ask-ui--form-body state))
       :keymap (niwa-tool-ask-ui--keymap state)
       :help-echo "Ask prompt")))
    (let ((overlay (niwa-tool-ask-ui--state-overlay state))
          (count (niwa-tool-ask-ui--state-count state)))
      (overlay-put overlay 'niwa--callback
                   (lambda (outcome)
                     (niwa-tool-ask-ui--retire state)
                     (funcall (niwa-tool-ask-ui--state-callback state) outcome)))
      (cl-pushnew state niwa-tool-ask-ui--states :test #'eq)
      (add-hook 'post-command-hook #'niwa-tool-ask-ui--post-command nil t)
      (overlay-put
       overlay 'niwa--remote
       (list
        :body
        (format "Ask · %d question%s" count (if (= count 1) "" "s"))
        :questions
        (lambda ()
          (niwa-tool-ask-ui--remote-questions state))
        :answer
        (lambda (submitted)
          (niwa-tool-ask-ui--dispatch state 'remote-answer submitted))
        :cancel
        (lambda ()
          (niwa-tool-ask-ui--dispatch state 'cancel))))
      (cl-pushnew overlay niwa--prompt-overlays :test #'eq)
      (niwa--prompt--register-canceller
       (niwa-tool-ask-ui--state-source-buffer state) overlay)
      (when announce
        (niwa--prompt-announce overlay)))))

(defun niwa-tool-ask-ui--anchor-at-point (state)
  "Return what point is on in STATE's form as (KEY . OFFSET), or nil.
KEY is the entry identity at point, or (question . INDEX) on a
question's heading or sample panel; OFFSET is point's distance from
where KEY starts."
  (let ((overlay (niwa-tool-ask-ui--state-overlay state)))
    (when-let* ((key (or (niwa-tool-ask-ui--entry-at-point state)
                         (and (overlayp overlay)
                              (eq (overlay-buffer overlay) (current-buffer))
                              (<= (overlay-start overlay) (point))
                              (< (point) (overlay-end overlay))
                              (when-let* ((index (get-text-property
                                                  (point) 'niwa-ask-question)))
                                (cons 'question index)))))
                (start (niwa-tool-ask-ui--anchor-start state key)))
      (cons key (- (point) start)))))

(defun niwa-tool-ask-ui--anchor-start (state key)
  "Return where anchor KEY starts in STATE's form, or nil.
An entry starts at its glyph, a question block at its first character."
  (if (eq (car-safe key) 'question)
      (let ((overlay (niwa-tool-ask-ui--state-overlay state)))
        (text-property-any (overlay-start overlay) (overlay-end overlay)
                           'niwa-ask-question (cdr key)))
    (cdr (assoc key (niwa-tool-ask-ui--entry-positions state)))))

(defun niwa-tool-ask-ui--anchor-p (key position)
  "Return non-nil when buffer POSITION still belongs to anchor KEY."
  (and (<= (point-min) position)
       (< position (point-max))
       (if (eq (car-safe key) 'question)
           (eq (cdr key) (get-text-property position 'niwa-ask-question))
         (equal key (get-text-property position 'niwa-ask-entry)))))

(defun niwa-tool-ask-ui--refresh (state &optional announce identity)
  "Render STATE, optionally ANNOUNCE it and move point to IDENTITY.
Without IDENTITY, point stays on its entry, or in its question's heading
or panel, at the same offset into it.  The view restores point by its
offset into the whole form, which lands elsewhere when text above point
changed height, as a re-wrapped sample panel does after a resize."
  (unless (niwa-tool-ask-ui--settled-p state)
    (with-current-buffer (niwa-tool-ask-ui--state-chat-buffer state)
      (let ((anchor (and (not identity)
                         (niwa-tool-ask-ui--anchor-at-point state))))
        (niwa-tool-ask-ui--render state announce)
        (if identity
            (niwa-tool-ask-ui--goto-entry state identity)
          (pcase-let ((`(,key . ,offset) anchor))
            (when-let* ((start (and key (niwa-tool-ask-ui--anchor-start
                                         state key))))
              ;; Keep the offset while it still lies in the same place.
              (goto-char (if (niwa-tool-ask-ui--anchor-p key (+ start offset))
                             (+ start offset)
                           start))))
          (niwa-tool-ask-ui--post-command))))))

(defun niwa-tool-ask-ui--width-changed ()
  "Lay out the current view's pending Ask forms again for its new width.
A form reserves its height at one width; left alone after a resize, its
wrapped rows and padding no longer match, and the next keystroke that
repaints it would change its height."
  (dolist (state niwa-tool-ask-ui--states)
    (unless (eql (car (niwa-tool-ask-ui--state-height state))
                 (niwa-tool-ask-ui--wrap-width state))
      (niwa-tool-ask-ui--refresh state))))

(add-hook 'niwa-view-zone-width-change-functions
          #'niwa-tool-ask-ui--width-changed)

(cl-defun niwa-tool-ask-ui-show (callback questions)
  "Show QUESTIONS as one form and settle it through CALLBACK."
  (niwa-tools--validate-params callback niwa-tool-ask-ui-show
    (questions (vectorp . "array")))
  (let* ((source-buffer (current-buffer))
         (questions-list (append questions nil))
         (state
          (niwa-tool-ask-ui--state-create
           :callback callback
           :source-buffer source-buffer
           :origin (niwa-current-origin)
           :questions questions-list
           :count (length questions-list)
           :answers (make-vector (length questions-list) nil)
           :chat-buffer
           (or
            (niwa-view--interaction-target-buffer
             (with-current-buffer source-buffer
               (niwa--prompt--data-buffer)))
            (error "No live view for Ask prompt"))
           :interaction-id (list :ask (gensym "ask-"))
           :open 0)))
    (niwa-tool-ask-ui--refresh state t)))

(provide 'niwa-tool-ask-ui)
;;; niwa-tool-ask-ui.el ends here
