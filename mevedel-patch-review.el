;;; mevedel-patch-review.el -- Staged ApplyPatch review UI -*- lexical-binding: t -*-

;;; Commentary:

;; Interactive staged review for ApplyPatch proposals: fold and
;; selection state per file and hunk, inline diff rendering with a
;; line-number gutter, side-by-side editing, feedback at hunk, file, and
;; patch scope, and final settlement through the patch engine in
;; `mevedel-tool-patch'.  Nothing touches the filesystem before the
;; final action.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `ediff'
(declare-function ediff-buffers "ediff"
                  (buffer-A buffer-B &optional startup-hooks job-name))

;; `ediff-util'
(declare-function ediff-diff-at-point "ediff-util"
                  (buf-type &optional pos which-diff))
(declare-function ediff-jump-to-difference "ediff-util" (difference-number))
(declare-function ediff-next-difference "ediff-util" (&optional arg))
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-keep-variants)
(defvar ediff-number-of-differences)

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--register-canceller
                  "mevedel-interaction-prompt"
                  (&optional source-buffer overlay))
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay outcome))
(declare-function mevedel--prompt-announce
                  "mevedel-interaction-prompt" (overlay))
(autoload 'mevedel--prompt--register-canceller "mevedel-interaction-prompt")
(autoload 'mevedel--prompt--settle "mevedel-interaction-prompt")
(autoload 'mevedel--prompt-announce "mevedel-interaction-prompt")
(defvar mevedel--prompt-overlays)

;; `mevedel-reminders'
(declare-function mevedel-reminders-make-user-revised-patch
                  "mevedel-reminders" (summary))
(declare-function mevedel-session-ensure-reminder
                  "mevedel-reminders" (session reminder))
(autoload 'mevedel-reminders-make-user-revised-patch "mevedel-reminders")
(autoload 'mevedel-session-ensure-reminder "mevedel-reminders")

;; `mevedel-side-conversation'
(declare-function mevedel-side-conversation-mutation-warning
                  "mevedel-side-conversation" (record effect))
(declare-function mevedel-side-conversation-mutation-warning-pending-p
                  "mevedel-side-conversation" (record))
(autoload 'mevedel-side-conversation-mutation-warning
  "mevedel-side-conversation")
(autoload 'mevedel-side-conversation-mutation-warning-pending-p
  "mevedel-side-conversation")

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-tool-patch'
(declare-function mevedel-tool-patch-annotate-line-numbers
                  "mevedel-tool-patch" (proposal))
(declare-function mevedel-tool-patch-apply
                  "mevedel-tool-patch" (data-buffer changes continuation))
(declare-function mevedel-tool-patch-apply-hunks
                  "mevedel-tool-patch" (content hunks path))
(declare-function mevedel-tool-patch-assert-baseline
                  "mevedel-tool-patch" (proposal))
(declare-function mevedel-tool-patch-content-lines
                  "mevedel-tool-patch" (content))
(declare-function mevedel-tool-patch-hunk-counts
                  "mevedel-tool-patch" (hunk))
(declare-function mevedel-tool-patch-hunks-from-content
                  "mevedel-tool-patch" (baseline content))
(declare-function mevedel-tool-patch-kind-face
                  "mevedel-tool-patch" (kind))
(declare-function mevedel-tool-patch-match-pass-description
                  "mevedel-tool-patch" (pass))
(declare-function mevedel-tool-patch-operation-stats
                  "mevedel-tool-patch" (operation))
(declare-function mevedel-tool-patch-physical-path
                  "mevedel-tool-patch" (operation))
(declare-function mevedel-tool-patch-planned-changes
                  "mevedel-tool-patch" (proposal))
(declare-function mevedel-tool-patch-proposal-stats
                  "mevedel-tool-patch" (proposal))
(declare-function mevedel-tool-patch-resource-address-p
                  "mevedel-tool-patch" (value))
(declare-function mevedel-tool-patch-result
                  "mevedel-tool-patch" (proposal changes &optional reviewed))
(declare-function mevedel-tool-patch-sanitize-error
                  "mevedel-tool-patch" (message proposal))
(declare-function mevedel-tool-patch-status
                  "mevedel-tool-patch" (operation))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction" (&optional data-buffer))

(defvar mevedel-patch-review--counter 0
  "Monotonic id counter for patch review interactions.")

(defface mevedel-patch-review-apply-button
  '((t :inherit success :box (:line-width (1 . -1))))
  "Face for the primary ApplyPatch review action."
  :group 'mevedel)

(defface mevedel-patch-review-reject-button
  '((t :inherit shadow :box (:line-width (1 . -1))))
  "Face for the ApplyPatch reject-all action."
  :group 'mevedel)

(defvar-keymap mevedel-patch-review-map
  :doc "Base keymap covering the whole ApplyPatch review body.
Feedback anywhere outside a file or hunk targets the whole patch."
  "n" #'mevedel-patch-review-next-row
  "p" #'mevedel-patch-review-previous-row
  "e" #'mevedel-patch-review-edit
  "f" #'mevedel-patch-review-feedback
  "C-c C-c" #'mevedel-patch-review-submit
  "C-c C-k" #'mevedel-patch-review-reject)

(defvar-keymap mevedel-patch-review-file-map
  :doc "Keymap for file rows and their content in an ApplyPatch review."
  :parent mevedel-patch-review-map
  "TAB" #'mevedel-patch-review-toggle-fold
  "RET" #'mevedel-patch-review-visit
  "<return>" #'mevedel-patch-review-visit
  "SPC" #'mevedel-patch-review-toggle-selection)

(defvar-keymap mevedel-patch-review-hunk-map
  :doc "Keymap for hunk rows and their diff lines in an ApplyPatch review."
  :parent mevedel-patch-review-map
  "TAB" #'mevedel-patch-review-toggle-fold
  "RET" #'mevedel-patch-review-visit
  "<return>" #'mevedel-patch-review-visit
  "SPC" #'mevedel-patch-review-toggle-selection)

(defvar-keymap mevedel-patch-review-submit-map
  :doc "Keymap for the primary ApplyPatch review action."
  :parent mevedel-patch-review-map
  "RET" #'mevedel-patch-review-submit
  "<return>" #'mevedel-patch-review-submit)

(defvar-keymap mevedel-patch-review-reject-map
  :doc "Keymap for the ApplyPatch reject-all action."
  :parent mevedel-patch-review-map
  "RET" #'mevedel-patch-review-reject
  "<return>" #'mevedel-patch-review-reject)

(defvar-keymap mevedel-patch-review-feedback-map
  :doc "Keymap for an inline ApplyPatch feedback field."
  "C-c C-c" #'mevedel-patch-review-confirm-feedback
  "C-c C-k" #'mevedel-patch-review-cancel-feedback)

(defvar-local mevedel-patch-review--edit-session nil
  "Control buffer of this view's live side-by-side edit session, or nil.
One editor per session: two in the same view could stage conflicting
revisions, while independent session views do not share proposal state.")


(defun mevedel-patch-review--target-at-point (property)
  "Return PROPERTY at point or immediately before point."
  (or (get-char-property (point) property)
      (and (> (point) (point-min))
           (get-char-property (1- (point)) property))))

(defun mevedel-patch-review--insert-propertized (text &rest properties)
  "Insert TEXT with PROPERTIES into the current buffer."
  (unless (plist-member properties 'read-only)
    (setq properties (append '(read-only t) properties)))
  (insert (apply #'propertize text properties)))

(defun mevedel-patch-review--primary-label (proposal)
  "Return the primary action label for PROPOSAL's staged state."
  (let* ((stats (mevedel-tool-patch-proposal-stats proposal))
         (selected (plist-get stats :selected))
         (files (plist-get stats :files-selected))
         (comments (plist-get stats :comments)))
    (cond
     ((> selected 0)
      (concat (format "Apply %d change%s in %d file%s"
                      selected (if (= selected 1) "" "s")
                      files (if (= files 1) "" "s"))
              (when (> comments 0)
                (format " · send %d comment%s"
                        comments (if (= comments 1) "" "s")))))
     ((> comments 0)
      (format "Request revision · %d comment%s"
              comments (if (= comments 1) "" "s")))
     (t "Reject patch"))))

(defun mevedel-patch-review--feedback-body
    (proposal operation target label)
  "Insert feedback UI for TARGET in PROPOSAL under LABEL.
OPERATION owns TARGET and is retained as interaction metadata."
  (let ((properties (list 'keymap mevedel-patch-review-map
                          'mevedel-patch-proposal proposal
                          'mevedel-patch-operation operation
                          'mevedel-patch-feedback-target target)))
    (cond
     ((plist-get target :feedback-editing)
      (apply #'mevedel-patch-review--insert-propertized
             (concat "      "
                     (propertize "✎" 'font-lock-face 'warning)
                     (propertize (format " Feedback for %s:" label)
                                 'font-lock-face 'bold)
                     "\n")
             properties)
      (mevedel-patch-review--insert-propertized
       (concat "      " (or (plist-get target :feedback-draft) "") "\n")
       'read-only nil
       'field target
       'keymap mevedel-patch-review-feedback-map
       'help-echo "C-c C-c stages feedback; C-c C-k cancels"
       'front-sticky '(field keymap mevedel-patch-feedback-input)
       'rear-nonsticky nil
       'mevedel-patch-proposal proposal
       'mevedel-patch-operation operation
       'mevedel-patch-feedback-target target
       'mevedel-patch-feedback-input target)
      (apply #'mevedel-patch-review--insert-propertized
             (concat "      "
                     (propertize "C-c C-c stage feedback · C-c C-k cancel"
                                 'font-lock-face 'shadow)
                     "\n")
             properties))
     ((plist-get target :feedback)
      (cl-loop
       for line in (split-string (plist-get target :feedback) "\n" nil)
       for prefix = (concat "      " (propertize "✎" 'font-lock-face 'warning) " ")
       then "        "
       do (apply #'mevedel-patch-review--insert-propertized
                 (concat prefix (propertize line 'font-lock-face 'shadow) "\n")
                 properties))))))

(defun mevedel-patch-review--body (proposal)
  "Return the hierarchical interaction body for PROPOSAL."
  (let* ((stats (mevedel-tool-patch-proposal-stats proposal))
         (operations (plist-get proposal :operations))
         (file-count (length operations))
         (comments (plist-get stats :comments)))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-labels
            ((ins (text &rest properties)
               (unless (plist-member properties 'keymap)
                 (setq properties
                       (append properties
                               (list 'keymap mevedel-patch-review-map))))
               (apply #'mevedel-patch-review--insert-propertized
                      text 'mevedel-patch-proposal proposal properties))
             (sel-glyph (state)
               (pcase state
                 ('none (propertize "✗" 'font-lock-face 'error))
                 ('partial (propertize "◐" 'font-lock-face 'warning))
                 (_ (propertize "✓" 'font-lock-face 'success))))
             (plus (n dim-p)
               (propertize (format "+%d" n) 'font-lock-face
                           (if dim-p 'shadow
                             'mevedel-view-tool-diff-added)))
             (minus (n dim-p)
               (propertize (format "−%d" n) 'font-lock-face
                           (if dim-p 'shadow
                             'mevedel-view-tool-diff-removed)))
             (sep () (propertize " · " 'font-lock-face 'shadow))
             (disp (path)
               (if (or (mevedel-tool-patch-resource-address-p path)
                       (not (file-name-absolute-p path)))
                   path
                 (if-let* ((root (plist-get proposal :root)))
                     (file-relative-name path root)
                   path)))
             (path-label (rel dim-p)
               (let ((dir (file-name-directory rel))
                     (base (file-name-nondirectory rel)))
                 (concat (and dir (propertize dir 'font-lock-face 'shadow))
                         (if dim-p
                             (propertize base 'font-lock-face 'shadow)
                           base))))
             (diff-line (marker number text dim-p &rest properties)
               (let* ((tint (and (not dim-p)
                                 (pcase marker
                                   (?+ 'mevedel-patch-review-added)
                                   (?- 'mevedel-patch-review-removed))))
                      (content-face
                       (if dim-p '(shadow)
                         (append (and tint (list tint))
                                 (pcase marker
                                   (?+ '(diff-added))
                                   (?- '(diff-removed))))))
                      (gutter-face
                       (if dim-p '(shadow)
                         (append (and tint (list tint)) '(shadow)))))
                 (apply #'ins
                        (concat
                         (propertize
                          (format "   %5s "
                                  (if number (number-to-string number) ""))
                          'font-lock-face gutter-face)
                         (propertize (string marker)
                                     'font-lock-face content-face)
                         (propertize " │ " 'font-lock-face gutter-face)
                         (propertize text 'font-lock-face content-face)
                         (propertize "\n" 'font-lock-face
                                     (and tint (list tint))))
                        properties)))
             (hunk-lines (hunk dim-p &rest properties)
               (let ((old (plist-get hunk :old-start))
                     (new (plist-get hunk :new-start)))
                 (dolist (line (plist-get hunk :diff-lines))
                   (let ((marker (aref line 0)))
                     (apply #'diff-line marker
                            (pcase marker (?+ new) (_ old))
                            (substring line 1) dim-p properties)
                     (pcase marker
                       (?+ (when new (cl-incf new)))
                       (?- (when old (cl-incf old)))
                       (_ (when old (cl-incf old))
                          (when new (cl-incf new)))))))))
          (ins (format "ApplyPatch · %d %s · %d/%d %s · "
                       file-count (if (= file-count 1) "file" "files")
                       (plist-get stats :selected) (plist-get stats :total)
                       (if (= (plist-get stats :total) 1) "change" "changes"))
               'font-lock-face 'bold)
          (ins (format "+%d" (plist-get stats :added))
               'font-lock-face 'mevedel-view-tool-diff-added)
          (ins " ")
          (ins (format "−%d" (plist-get stats :deleted))
               'font-lock-face 'mevedel-view-tool-diff-removed)
          (when (> comments 0)
            (ins (format " · %d comment%s"
                         comments (if (= comments 1) "" "s"))
                 'font-lock-face 'warning))
          (ins "\n")
          (when-let* ((warning (mevedel-side-conversation-mutation-warning
                                proposal "applying this patch")))
            (ins warning 'font-lock-face 'warning))
          (when-let* ((conflict (plist-get proposal :conflict)))
            (ins (format "Conflict: %s\n" conflict) 'font-lock-face 'error)
            (if (plist-get proposal :rollback-incomplete)
                (ins (concat "Rollback was incomplete. Inspect the listed"
                             " paths before retrying.\n")
                     'font-lock-face 'shadow)
              (ins (concat "Deselect the stale file with SPC to apply the"
                           " rest, or C-c C-k to reject so the model"
                           " re-reads it.\n")
                   'font-lock-face 'shadow)))
          (mevedel-patch-review--feedback-body
           proposal nil proposal "the whole patch")
          (ins "\n")
          (dolist (operation operations)
            (let* ((kind (plist-get operation :kind))
                   (ostats (mevedel-tool-patch-operation-stats operation))
                   (osel (plist-get ostats :selected))
                   (ototal (plist-get ostats :total))
                   (state (cond ((zerop osel) 'none)
                                ((= osel ototal) 'all)
                                (t 'partial)))
                   (dim-p (eq state 'none))
                   (expanded (plist-get operation :expanded))
                   (row-props (list 'keymap mevedel-patch-review-file-map
                                    'help-echo
                                    "TAB folds · SPC selects · RET visits"
                                    'mevedel-patch-operation operation
                                    ;; Row values are the distinct target
                                    ;; objects so adjacent rows keep a
                                    ;; property boundary for n/p movement.
                                    'mevedel-patch-row operation))
                   (content-props (list 'keymap mevedel-patch-review-file-map
                                        'mevedel-patch-operation operation))
                   (letter
                    (propertize (mevedel-tool-patch-status operation)
                                'font-lock-face
                                (mevedel-tool-patch-kind-face kind)))
                   (source (or (plist-get operation :rel-path)
                               (plist-get operation :path)))
                   (label
                    (if-let* ((move (or (plist-get operation :move-rel-path)
                                        (plist-get operation :move-path))))
                        (concat (path-label (disp source) dim-p)
                                (propertize " → " 'font-lock-face 'shadow)
                                (path-label (disp move) dim-p))
                      (path-label (disp source) dim-p)))
                   (counts
                    (pcase kind
                      ('add (concat (sep)
                                    (plus (plist-get ostats :added) dim-p)))
                      ('delete (concat (sep)
                                       (minus (plist-get ostats :deleted)
                                              dim-p)))
                      ('update
                       (concat (sep)
                               (propertize (format "%d/%d" osel ototal)
                                           'font-lock-face 'shadow)
                               (when (> osel 0)
                                 (concat (sep)
                                         (plus (plist-get ostats :added)
                                               dim-p)
                                         " "
                                         (minus (plist-get ostats :deleted)
                                                dim-p)))))
                      ('move
                       (and (plist-get operation :hunks)
                            (concat (sep)
                                    (plus (plist-get ostats :added) dim-p)
                                    " "
                                    (minus (plist-get ostats :deleted)
                                           dim-p)))))))
              ;; The trailing newline carries no mouse-face so hovering
              ;; never highlights two adjacent rows as one region.
              (apply #'ins
                     (concat (if expanded "▼" "▶") " "
                             (sel-glyph state) " " letter " " label
                             (or counts "")
                             (and (plist-get operation :modified)
                                  (propertize " · edited"
                                              'font-lock-face 'shadow))
                             (and (plist-get operation :feedback)
                                  (concat " " (propertize "✎" 'font-lock-face 'warning))))
                     'mouse-face 'highlight
                     row-props)
              (apply #'ins "\n" row-props)
              (when expanded
                (pcase kind
                  ('update
                   (cl-loop
                    for hunk in (plist-get operation :hunks)
                    for number from 1
                    do (let* ((selected (plist-get hunk :selected))
                              (hdim (not selected))
                              (folded (plist-get hunk :folded))
                              (context (plist-get hunk :context))
                              (hunk-label
                               (cond (context)
                                     ((plist-get hunk :section))
                                     ((plist-get hunk :old-start)
                                      (format "~%d"
                                              (plist-get hunk :old-start)))))
                              (hcounts (mevedel-tool-patch-hunk-counts
                                        hunk))
                              (hunk-props
                               (list 'keymap mevedel-patch-review-hunk-map
                                     'mevedel-patch-operation operation
                                     'mevedel-patch-hunk hunk)))
                         (apply #'ins
                                (concat
                                 "    " (sel-glyph (if selected 'all 'none))
                                 " "
                                 (propertize
                                  (string-trim-right
                                   (concat "@@ " (or hunk-label "")))
                                  'font-lock-face
                                  (if hdim 'shadow 'diff-hunk-header))
                                 (sep)
                                 (plus (car hcounts) hdim)
                                 " "
                                 (minus (cdr hcounts) hdim)
                                 (and (plist-get hunk :match-pass)
                                      (propertize
                                       " · fuzzy"
                                       'font-lock-face 'warning
                                       'help-echo
                                       (format "Matched %s"
                                               (mevedel-tool-patch-match-pass-description
                                                (plist-get hunk :match-pass)))))
                                 (propertize
                                  (concat (and (plist-get hunk :modified)
                                               " · edited")
                                          (and folded " …"))
                                  'font-lock-face 'shadow)
                                 (and (plist-get hunk :feedback)
                                      (concat " " (propertize
                                                   "✎" 'font-lock-face
                                                   'warning))))
                                'mouse-face 'highlight
                                'help-echo
                                "SPC selects · TAB folds · e edits · f feedback"
                                'mevedel-patch-row hunk
                                hunk-props)
                         (apply #'ins "\n"
                                'mevedel-patch-row hunk hunk-props)
                         (unless folded
                           (apply #'hunk-lines hunk hdim hunk-props))
                         (when (or (not folded)
                                   (plist-get hunk :feedback-editing))
                           (mevedel-patch-review--feedback-body
                            proposal operation hunk
                            (or context (format "hunk %d" number)))))))
                  ('move
                   (dolist (hunk (plist-get operation :hunks))
                     (when-let* ((context (plist-get hunk :context)))
                       (apply #'ins
                              (concat "      "
                                      (propertize (concat "@@ " context)
                                                  'font-lock-face
                                                  (if dim-p 'shadow
                                                    'diff-hunk-header))
                                      "\n")
                              'mevedel-patch-edit-hunk hunk
                              content-props))
                     (apply #'hunk-lines hunk dim-p
                            'mevedel-patch-edit-hunk hunk content-props)))
                  ('add
                   (cl-loop for line in (mevedel-tool-patch-content-lines
                                         (plist-get operation :content))
                            for number from 1
                            do (apply #'diff-line ?+ number line dim-p
                                      content-props)))
                  ('delete
                   (cl-loop for line in (mevedel-tool-patch-content-lines
                                         (or (plist-get operation
                                                        :baseline-content)
                                             ""))
                            for number from 1
                            do (apply #'diff-line ?- number line dim-p
                                      content-props)))))
              ;; An active hunk feedback editor must stay rendered even
              ;; while its file is folded, or its buffer markers go
              ;; stale and later redraws capture garbage drafts.
              (when (and (not expanded) (eq kind 'update))
                (dolist (hunk (plist-get operation :hunks))
                  (when (plist-get hunk :feedback-editing)
                    (mevedel-patch-review--feedback-body
                     proposal operation hunk
                     (or (plist-get hunk :context)
                         (plist-get hunk :section)
                         "hunk")))))
              (mevedel-patch-review--feedback-body
               proposal operation operation
               (plist-get operation :rel-path))))
          (ins "\n")
          (ins (concat "Keys: TAB fold · SPC select · RET visit · e edit"
                       " · f feedback · C-c C-c apply · C-c C-k reject\n")
               'font-lock-face 'help-key-binding)
          (let ((primary (mevedel-patch-review--primary-label proposal)))
            (ins (format "[ %s ]" primary)
                 'font-lock-face 'mevedel-patch-review-apply-button
                 'keymap mevedel-patch-review-submit-map
                 'mouse-face 'highlight
                 'help-echo primary)
            (unless (equal primary "Reject patch")
              (ins "   ")
              (ins "[ Reject all ]"
                   'font-lock-face 'mevedel-patch-review-reject-button
                   'keymap mevedel-patch-review-reject-map
                   'mouse-face 'highlight
                   'help-echo "Reject every change")))
          (ins "\n")
          (buffer-string))))))

(defun mevedel-patch-review--feedback-targets (proposal)
  "Return every feedback-capable target in PROPOSAL.
The whole patch, each operation, and each Update hunk."
  (cons proposal
        (cl-mapcan (lambda (operation)
                     (cons operation
                           (copy-sequence (plist-get operation :hunks))))
                   (plist-get proposal :operations))))

(defun mevedel-patch-review--sync-drafts (proposal)
  "Capture live feedback drafts from their field markers into PROPOSAL."
  (dolist (target (mevedel-patch-review--feedback-targets proposal))
    (when-let* (((plist-get target :feedback-editing))
                (start (plist-get target :feedback-start))
                (end (plist-get target :feedback-end))
                ((markerp start))
                ((markerp end))
                ((marker-buffer start))
                ((marker-buffer end)))
      (plist-put target :feedback-draft
                 (buffer-substring-no-properties start end)))))

(defun mevedel-patch-review--refresh-feedback-markers (proposal)
  "Recreate PROPOSAL's feedback field markers from the rendered text."
  (dolist (target (mevedel-patch-review--feedback-targets proposal))
    (when (plist-get target :feedback-editing)
      (let* ((position (text-property-any
                        (point-min) (point-max)
                        'mevedel-patch-feedback-input target))
             (end (and position
                       (next-single-property-change
                        position 'mevedel-patch-feedback-input
                        nil (point-max)))))
        (mevedel-patch-review--clear-feedback-markers target)
        (if position
            (progn
              (plist-put target :feedback-start
                         (copy-marker (+ position 6)))
              (plist-put target :feedback-end
                         (copy-marker (1- end) t)))
          ;; The field vanished from the rendering; cancel the edit
          ;; rather than let stale markers capture garbage drafts.
          (plist-put target :feedback
                     (plist-get target :feedback-original))
          (plist-put target :selected
                     (plist-get target :selected-original))
          (plist-put target :feedback-editing nil)
          (plist-put target :feedback-draft nil))))))

(defun mevedel-patch-review--render (proposal)
  "Render or redraw PROPOSAL in its view buffer.
The interaction body is a function so every zone render -- including
foreign rebuilds such as the control-transfer poll -- draws the live
review state instead of a stale snapshot that would destroy an active
feedback draft.  Marker upkeep rides the `:after-render' hook for the
same reason."
  (with-current-buffer (plist-get proposal :view-buffer)
    (mevedel-view--interaction-register
     (list :kind 'preview
           :id (plist-get proposal :id)
           :count 1
           :body
           (lambda ()
             (mevedel-patch-review--sync-drafts proposal)
             (if (eq (plist-get proposal :state) 'submitting)
                 (propertize
                  "\nApplyPatch · Applying patch and refreshing diagnostics...\n"
                  'font-lock-face 'shadow)
               (mevedel-patch-review--body proposal)))
           :after-render
           (lambda ()
             (mevedel-patch-review--refresh-feedback-markers proposal))
           :priority 300
           :read-only (eq (plist-get proposal :state) 'submitting)
           :body-properties-owned t))))

(defun mevedel-patch-review--settle (proposal outcome)
  "Settle PROPOSAL exactly once with OUTCOME."
  (unless (eq (plist-get proposal :state) 'settled)
    (plist-put proposal :state 'settled)
    (funcall (plist-get proposal :callback)
             (if (eq outcome 'aborted)
                 '(:result "Error: Patch review aborted" :status error)
               outcome))))

(defun mevedel-patch-review-start (proposal callback data-buffer)
  "Stage PROPOSAL for review and settle it through CALLBACK.
DATA-BUFFER is the tool-calling buffer whose view owns the interaction."
  (mevedel-tool-patch-annotate-line-numbers proposal)
  (plist-put proposal :id
             (list 'patch-review (cl-incf mevedel-patch-review--counter)))
  (plist-put proposal :callback callback)
  (plist-put proposal :data-buffer data-buffer)
  (plist-put proposal :view-buffer
             (mevedel-view--interaction-target-buffer data-buffer))
  (let ((overlay (mevedel-patch-review--render proposal)))
    (plist-put proposal :overlay overlay)
    (with-current-buffer (plist-get proposal :view-buffer)
      (overlay-put overlay 'mevedel-user-request t)
      (overlay-put overlay 'mevedel--callback
                   (lambda (outcome)
                     (mevedel-patch-review--settle proposal outcome)))
      ;; The remote surface gets the two whole-call decisions: apply the
      ;; staged selection, or request a revision with whole-patch
      ;; feedback.  Side-by-side editing and per-hunk feedback stay in
      ;; Emacs.
      (let ((remote
             (list :body
                   (let ((patch (or (plist-get proposal :patch) "")))
                     (if (> (length patch) 60000)
                         (concat (substring patch 0 60000) "\n[truncated]")
                       patch))
                   :body-kind "diff"
                   :options
                   (list (cons (lambda ()
                                 (mevedel-patch-review--submit proposal))
                               "Apply patch"))
                   :feedback
                   (lambda (text)
                     (unless (memq (plist-get proposal :state)
                                   '(submitting settled))
                       (plist-put proposal :feedback text)
                       (mevedel-patch-review--deselect-all proposal)
                       (mevedel-patch-review--submit proposal))))))
        (plist-put proposal :remote remote)
        (overlay-put overlay 'mevedel--remote remote))
      (cl-pushnew overlay mevedel--prompt-overlays :test #'eq)
      (mevedel--prompt--register-canceller data-buffer overlay)
      (mevedel--prompt-announce overlay)))
  nil)

(defun mevedel-patch-review--deselect-all (proposal)
  "Deselect every operation and hunk in PROPOSAL.
Remote whole-patch feedback requests a revision instead of applying."
  (dolist (operation (plist-get proposal :operations))
    (plist-put operation :selected nil)
    (dolist (hunk (plist-get operation :hunks))
      (plist-put hunk :selected nil))))

(defun mevedel-patch-review-toggle-fold ()
  "Fold or unfold the patch file or hunk at point."
  (interactive)
  (let ((proposal (mevedel-patch-review--target-at-point
                   'mevedel-patch-proposal))
        (operation (mevedel-patch-review--target-at-point
                    'mevedel-patch-operation))
        (hunk (mevedel-patch-review--target-at-point
               'mevedel-patch-hunk)))
    (unless (and proposal operation)
      (user-error "No patch change at point"))
    (if hunk
        (plist-put hunk :folded (not (plist-get hunk :folded)))
      (plist-put operation :expanded (not (plist-get operation :expanded))))
    (mevedel-patch-review--render proposal)))

(defun mevedel-patch-review-toggle-selection ()
  "Toggle the selected state of the patch change at point.
On an Update file row this toggles every hunk in the file at once."
  (interactive)
  (let ((proposal (mevedel-patch-review--target-at-point
                   'mevedel-patch-proposal))
        (operation (mevedel-patch-review--target-at-point
                    'mevedel-patch-operation))
        (hunk (mevedel-patch-review--target-at-point
               'mevedel-patch-hunk)))
    (unless (and proposal operation)
      (user-error "No selectable patch change at point"))
    (if (and (eq (plist-get operation :kind) 'update) (not hunk))
        (let* ((hunks (plist-get operation :hunks))
               (any (cl-some (lambda (h) (plist-get h :selected)) hunks)))
          (cond
           (any (dolist (h hunks) (plist-put h :selected nil)))
           ((or (not (cl-some (lambda (h) (plist-get h :feedback)) hunks))
                (yes-or-no-p "Reselect file and clear its feedback? "))
            (dolist (h hunks)
              (plist-put h :feedback nil)
              (plist-put h :selected t)))))
      (let* ((target (or hunk operation))
             (selected (plist-get target :selected)))
        (cond
         (selected (plist-put target :selected nil))
         ((not (plist-get target :feedback))
          (plist-put target :selected t))
         ((yes-or-no-p "Reselect and clear its feedback? ")
          (plist-put target :feedback nil)
          (plist-put target :selected t)))))
    (mevedel-patch-review--render proposal)))

(defun mevedel-patch-review-visit ()
  "Visit the file at point, at the hunk location when known."
  (interactive)
  (let ((operation (mevedel-patch-review--target-at-point
                    'mevedel-patch-operation))
        (hunk (mevedel-patch-review--target-at-point
               'mevedel-patch-hunk)))
    (unless operation
      (user-error "No patch change at point"))
    (when (eq (plist-get operation :kind) 'add)
      (user-error "File is not created until the patch is applied"))
    (let ((path (mevedel-tool-patch-physical-path operation)))
      (unless (file-exists-p path)
        (user-error "File does not exist: %s" path))
      (find-file-other-window path)
      (when-let* ((start (and hunk (plist-get hunk :old-start))))
        (widen)
        (goto-char (point-min))
        (forward-line (1- start))))))

(defun mevedel-patch-review-next-row ()
  "Move point to the next patch review row."
  (interactive)
  (let ((pos (next-single-char-property-change (point) 'mevedel-patch-row)))
    (while (and (< pos (point-max))
                (not (get-char-property pos 'mevedel-patch-row)))
      (setq pos (next-single-char-property-change pos 'mevedel-patch-row)))
    (if (get-char-property pos 'mevedel-patch-row)
        (goto-char pos)
      (user-error "No next change"))))

(defun mevedel-patch-review-previous-row ()
  "Move point to the previous patch review row."
  (interactive)
  (let ((pos (previous-single-char-property-change
              (point) 'mevedel-patch-row)))
    (while (and (> pos (point-min))
                (not (get-char-property (1- pos) 'mevedel-patch-row)))
      (setq pos (previous-single-char-property-change
                 pos 'mevedel-patch-row)))
    (if (and (> pos (point-min))
             (get-char-property (1- pos) 'mevedel-patch-row))
        (progn (goto-char (1- pos))
               (forward-line 0))
      (user-error "No previous change"))))

(defun mevedel-patch-review-reject ()
  "Reject every change in the review at point."
  (interactive)
  (let ((proposal (mevedel-patch-review--target-at-point
                   'mevedel-patch-proposal)))
    (unless proposal
      (user-error "No patch review at point"))
    (plist-put proposal :feedback nil)
    (dolist (operation (plist-get proposal :operations))
      (plist-put operation :selected nil)
      (plist-put operation :feedback nil)
      (dolist (hunk (plist-get operation :hunks))
        (plist-put hunk :selected nil)
        (plist-put hunk :feedback nil)))
    (mevedel-patch-review-submit)))

(defun mevedel-patch-review-feedback ()
  "Open an inline feedback field for the target at point.
Point on a hunk targets the hunk, on a file row or its content the
whole file, anywhere else the whole patch.  Hunk and whole-operation
feedback on indivisible kinds deselects the target; file-level feedback
on Update files and patch-level feedback leave selection untouched."
  (interactive)
  (let* ((proposal (mevedel-patch-review--target-at-point
                    'mevedel-patch-proposal))
         (operation (mevedel-patch-review--target-at-point
                     'mevedel-patch-operation))
         (hunk (mevedel-patch-review--target-at-point
                'mevedel-patch-hunk))
         (target (or hunk operation proposal)))
    (unless (and proposal target)
      (user-error "No patch review at point"))
    (plist-put target :feedback-original (plist-get target :feedback))
    (plist-put target :selected-original (plist-get target :selected))
    (plist-put target :feedback-draft (plist-get target :feedback))
    (plist-put target :feedback-editing t)
    (when (or hunk
              (and (eq target operation)
                   (memq (plist-get operation :kind) '(add delete move))))
      (plist-put target :selected nil))
    (mevedel-patch-review--render proposal)
    (with-current-buffer (plist-get proposal :view-buffer)
      (goto-char (plist-get target :feedback-start)))))

(defun mevedel-patch-review--feedback-input ()
  "Return the inline feedback target and its current text at point."
  (let* ((target (mevedel-patch-review--target-at-point
                  'mevedel-patch-feedback-input))
         (start (and target (plist-get target :feedback-start)))
         (end (and target (plist-get target :feedback-end))))
    (unless (and target (markerp start) (markerp end)
                 (marker-buffer start) (marker-buffer end))
      (user-error "No patch feedback field at point"))
    (cons target
          (string-trim
           (buffer-substring-no-properties start end)))))

(defun mevedel-patch-review--clear-feedback-markers (target)
  "Release TARGET's transient inline feedback markers."
  (dolist (key '(:feedback-start :feedback-end))
    (when-let* ((marker (plist-get target key))
                ((markerp marker)))
      (set-marker marker nil))
    (plist-put target key nil)))

(defun mevedel-patch-review-confirm-feedback ()
  "Stage the inline feedback at point without submitting the review."
  (interactive)
  (pcase-let* ((`(,target . ,feedback)
                 (mevedel-patch-review--feedback-input))
                (proposal (mevedel-patch-review--target-at-point
                           'mevedel-patch-proposal)))
    (when (string-empty-p feedback)
      (user-error "Feedback cannot be empty"))
    (plist-put target :feedback feedback)
    (plist-put target :feedback-editing nil)
    (plist-put target :feedback-draft nil)
    (plist-put target :feedback-original nil)
    (plist-put target :selected-original nil)
    (mevedel-patch-review--clear-feedback-markers target)
    (mevedel-patch-review--render proposal)))

(defun mevedel-patch-review-cancel-feedback ()
  "Cancel the inline feedback edit at point."
  (interactive)
  (pcase-let* ((`(,target . ,_feedback)
                 (mevedel-patch-review--feedback-input))
                (proposal (mevedel-patch-review--target-at-point
                           'mevedel-patch-proposal)))
    (plist-put target :feedback (plist-get target :feedback-original))
    (plist-put target :selected (plist-get target :selected-original))
    (plist-put target :feedback-editing nil)
    (plist-put target :feedback-draft nil)
    (plist-put target :feedback-original nil)
    (plist-put target :selected-original nil)
    (mevedel-patch-review--clear-feedback-markers target)
    (mevedel-patch-review--render proposal)))

(defun mevedel-patch-review-edit ()
  "Edit the proposed change at point side by side before it is applied.
The captured baseline opens on the left, read-only, and the result the
patch would produce opens on the right.  Edit the right buffer as
ordinary file text; quitting ediff offers to adopt those edits into the
staged proposal.  An Update or Move has its hunks re-derived from the
edited result, an Add has its proposed content replaced, and a Delete
becomes keeping the file whenever anything is left on the right."
  (interactive)
  (let ((proposal (mevedel-patch-review--target-at-point
                   'mevedel-patch-proposal))
        (operation (mevedel-patch-review--target-at-point
                    'mevedel-patch-operation))
        (hunk (or (mevedel-patch-review--target-at-point
                   'mevedel-patch-hunk)
                  (mevedel-patch-review--target-at-point
                   'mevedel-patch-edit-hunk))))
    (unless (and proposal operation)
      (user-error "No patch operation at point"))
    (when (buffer-live-p mevedel-patch-review--edit-session)
      (user-error "Another patch edit session is already open"))
    (when (and (memq (plist-get operation :kind) '(update move))
               (null (plist-get operation :hunks)))
      (user-error
       "A pure rename has no content to edit; SPC keeps the current path"))
    (pcase-let ((`(,baseline . ,planned)
                 (mevedel-patch-review--edit-sides operation)))
      ;; Compare logical lines: applying hunks renormalizes line endings,
      ;; so a mixed-ending file differs from its own baseline even with
      ;; every hunk deselected, and that is not a change to edit.
      (when (equal (mevedel-tool-patch-content-lines baseline)
                   (mevedel-tool-patch-content-lines planned))
        (user-error "Nothing to compare; select a change in this file first"))
      ;; Adopting an Add or a Delete selects the operation, and a
      ;; selected operation carries no feedback.  An Update or Move
      ;; needs no such confirmation: hunk feedback deselects its hunk,
      ;; and a deselected hunk survives the edit untouched.
      (when (memq (plist-get operation :kind) '(add delete))
        (mevedel-patch-review--confirm-feedback-loss operation))
      (mevedel-patch-review--edit-start proposal operation
                                       baseline planned hunk))))

(defun mevedel-patch-review--confirm-feedback-loss (target)
  "Signal unless an adopted edit of TARGET may clear its pending feedback.
The feedback itself is cleared only when a revision is adopted, so
declining the adoption costs nothing."
  (when (and (plist-get target :feedback)
             (not (yes-or-no-p "Edit and clear its feedback if adopted? ")))
    (user-error "Edit cancelled")))

(defun mevedel-patch-review--edit-sides (operation)
  "Return OPERATION's baseline content and planned result as a cons.
The planned result is what applying the operation's selected hunks
produces, so a hunk the user deselected is absent from the right-hand
side of an edit session and from the hunks derived out of it.  A
deselected indivisible operation has no planned change, so both sides
are its baseline, except for Add/Delete feedback whose confirmed edit
path may replace the feedback with an adopted revision."
  (let ((baseline (or (plist-get operation :baseline-content) ""))
        (kind (plist-get operation :kind)))
    (if (and (not (eq kind 'update))
             (not (plist-get operation :selected))
             ;; Add/Delete feedback deselects the operation, but the
             ;; confirmed edit path intentionally adopts it as selected.
             (not (and (memq kind '(add delete))
                       (plist-get operation :feedback))))
        (cons baseline baseline)
      (pcase kind
        ('add (cons "" (or (plist-get operation :content) "")))
        ('delete (cons baseline ""))
        (_ (cons baseline
                 (mevedel-tool-patch-apply-hunks
                  baseline (plist-get operation :hunks)
                  (mevedel-tool-patch-physical-path operation))))))))

(defun mevedel-patch-review--edit-buffer (name content path)
  "Return a new buffer NAME holding CONTENT in PATH's major mode."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert content)
      (goto-char (point-min))
      (let ((buffer-file-name path))
        (delay-mode-hooks (set-auto-mode))))
    buffer))

(defun mevedel-patch-review--edit-start
    (proposal operation baseline planned hunk)
  "Open a side-by-side edit session for OPERATION in PROPOSAL.
BASELINE and PLANNED are the two sides.  HUNK, when non-nil, positions
the session at the difference nearest that hunk.  Returns the editable
buffer."
  (require 'ediff)
  (let* ((path (mevedel-tool-patch-physical-path operation))
         (label (plist-get operation :rel-path))
         (view (plist-get proposal :view-buffer))
         (line (and hunk (plist-get hunk :old-start)))
         buffer-a buffer-b handed-off)
    (unwind-protect
        (progn
          (setq buffer-a
                (mevedel-patch-review--edit-buffer
                 (format "*mevedel baseline: %s*" label) baseline path)
                buffer-b
                (mevedel-patch-review--edit-buffer
                 (format "*mevedel result: %s*" label) planned path))
          (with-current-buffer buffer-a
            (setq buffer-read-only t))
          (ediff-buffers
           buffer-a buffer-b
           (list (lambda ()
                   (let ((control (current-buffer)))
                     (with-current-buffer view
                       (setq mevedel-patch-review--edit-session control)))
                   ;; ediff's own janitor would otherwise offer to kill both
                   ;; sides before the quit hook has read them.  These are
                   ;; temporaries the user never opened; disposing of them is
                   ;; this module's job, not a question to answer.
                   (setq-local ediff-keep-variants t)
                   ;; Depth 90 puts this after `ediff-cleanup-mess', which ends
                   ;; by clearing the echo area: an adoption failure reported
                   ;; from an earlier hook would be wiped before it is read.
                   ;; The state comes from this closure rather than from
                   ;; control-buffer locals, because that buffer is gone by
                   ;; then.
                   (add-hook 'ediff-quit-hook
                             (lambda ()
                               (mevedel-patch-review--edit-quit
                                proposal operation planned buffer-a buffer-b))
                             90 t)
                   (mevedel-patch-review--edit-goto-difference line))))
          (setq handed-off t)
          buffer-b)
      (unless handed-off
        (when (buffer-live-p view)
          (with-current-buffer view
            (setq mevedel-patch-review--edit-session nil)))
        (when (buffer-live-p buffer-a) (kill-buffer buffer-a))
        (when (buffer-live-p buffer-b) (kill-buffer buffer-b))))))

(defun mevedel-patch-review--edit-goto-difference (line)
  "Select the difference nearest LINE in the baseline buffer.
LINE is a one-based line in the captured baseline, or nil to start at
the first difference.  The baseline side is the one whose line numbers
are exact: a hunk's new-file position counts every matched hunk, while
the editable buffer holds only the selected ones."
  (when (> ediff-number-of-differences 0)
    (if-let* ((position
               (and line
                    (with-current-buffer ediff-buffer-A
                      (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- line))
                        (point))))))
        (ediff-jump-to-difference
         ;; 0 means "before the first difference", which selects nothing.
         (max 1 (min (ediff-diff-at-point 'A position)
                     ediff-number-of-differences)))
      (ediff-next-difference))))

(defun mevedel-patch-review--edit-quit
    (proposal operation planned buffer-a buffer-b)
  "Offer to adopt BUFFER-B as OPERATION's revision, then dispose of the session.
PROPOSAL owns OPERATION, PLANNED is the result the session opened with,
and BUFFER-A is the baseline side.  Nothing is asked when BUFFER-B still
holds PLANNED."
  (when-let* ((view (plist-get proposal :view-buffer))
              ((buffer-live-p view)))
    (with-current-buffer view
      (setq mevedel-patch-review--edit-session nil)))
  (unwind-protect
      (when-let* (((buffer-live-p buffer-b))
                  (content (with-current-buffer buffer-b
                             (buffer-substring-no-properties
                              (point-min) (point-max))))
                  ((not (equal content planned)))
                  ;; A quit here must not escape: ediff restores the
                  ;; window configuration after this hook returns.
                  ((with-local-quit
                     (y-or-n-p "Adopt your edits into the staged patch? "))))
        (mevedel-patch-review--edit-adopt
         proposal operation planned content))
    (when (buffer-live-p buffer-a) (kill-buffer buffer-a))
    (when (buffer-live-p buffer-b) (kill-buffer buffer-b))))

(defun mevedel-patch-review--edit-adopt (proposal operation planned content)
  "Adopt CONTENT as OPERATION's revised change in PROPOSAL.
PLANNED is the result the edit session opened with; the review stays
interactive while a session is live, so a selection changed underneath
it would silently reinstate a hunk the user had just rejected.
Reports rather than signals: an ediff quit hook must not fail, and a
revision that cannot be staged leaves the proposal exactly as it was."
  (let ((view (plist-get proposal :view-buffer)))
    (cond
     ((memq (plist-get proposal :state) '(submitting settled))
      (message "mevedel: patch review already settled, revision discarded"))
     ((not (buffer-live-p view))
      (message "mevedel: patch review is gone, revision discarded"))
     ((not (equal planned
                  (ignore-errors
                    (cdr (mevedel-patch-review--edit-sides operation)))))
      (message (concat "mevedel: this file's selection changed while the"
                       " editor was open, revision discarded")))
     (t
      (condition-case err
          (progn
            (pcase (plist-get operation :kind)
              ('add (mevedel-patch-review--adopt-content
                     proposal operation content))
              ('delete (mevedel-patch-review--adopt-delete
                        proposal operation content))
              (_ (mevedel-patch-review--adopt-hunks
                  proposal operation content)))
            (mevedel-tool-patch-annotate-line-numbers proposal)
            (with-current-buffer view
              (mevedel-patch-review--render proposal)))
        (error
         ;; A rolled-back revision leaves the restored hunks carrying
         ;; positions computed for the hunk set that was rejected.
         (mevedel-tool-patch-annotate-line-numbers proposal)
         (message "mevedel: revision not adopted, %s"
                  (mevedel-tool-patch-sanitize-error
                   (error-message-string err) proposal))))))))

(defun mevedel-patch-review--adopt-content (proposal operation content)
  "Replace Add OPERATION's proposed content in PROPOSAL with CONTENT."
  (when (string-empty-p (string-trim content))
    (user-error "An added file needs content; reject the file instead"))
  (let ((original (plist-get operation :content)))
    (plist-put operation :content
               (if (string-suffix-p "\n" content)
                   content
                 (concat content "\n")))
    (condition-case err
        (mevedel-tool-patch-planned-changes proposal)
      (error
       (plist-put operation :content original)
       (signal (car err) (cdr err))))
    (plist-put operation :modified t)
    (plist-put operation :selected t)
    ;; A selected operation carries no feedback; the prompt at session
    ;; open said adopting would clear it.
    (plist-put operation :feedback nil)))

(defun mevedel-patch-review--adopt-delete (proposal operation content)
  "Convert Delete OPERATION in PROPOSAL into keeping CONTENT.
The operation becomes an Update whose hunks are derived from the edited
result, so ordinary validation, application, selection, and revision
reporting cover it."
  (when (string-empty-p (string-trim content))
    (user-error "Empty content keeps nothing; the Delete stands as proposed"))
  (let* ((baseline (or (plist-get operation :baseline-content) ""))
         (hunks (mevedel-tool-patch-hunks-from-content baseline content))
         (original (plist-get operation :hunks)))
    (unless hunks
      (user-error "That keeps the file unchanged; deselect the Delete instead"))
    (dolist (hunk hunks)
      (plist-put hunk :modified t)
      (plist-put hunk :selected t))
    (plist-put operation :kind 'update)
    (plist-put operation :hunks hunks)
    (condition-case err
        (mevedel-tool-patch-planned-changes proposal)
      (error
       (plist-put operation :kind 'delete)
       (plist-put operation :hunks original)
       (signal (car err) (cdr err))))
    (plist-put operation :feedback nil)))

(defun mevedel-patch-review--adopt-hunks (proposal operation content)
  "Replace OPERATION's selected hunks in PROPOSAL with ones from CONTENT.
Deselected hunks are carried across untouched and merged back in
baseline file order, so a change the user rejected stays rejected and
stays reselectable.  Reselecting one later fails the ordinary way when
the edit moved the region it matches."
  (let* ((baseline (or (plist-get operation :baseline-content) ""))
         (derived (mevedel-tool-patch-hunks-from-content
                   baseline content))
         (original (plist-get operation :hunks))
         (preserved (cl-remove-if (lambda (hunk) (plist-get hunk :selected))
                                  original)))
    (unless derived
      (user-error
       "That leaves the file unchanged; deselect its hunks instead"))
    (dolist (hunk derived)
      (plist-put hunk :modified t)
      (plist-put hunk :selected t))
    (plist-put operation :hunks (append derived preserved))
    ;; Position every hunk against the unchanged baseline, then order the
    ;; merged set by that position: hunks are matched in file order, and
    ;; the review reads in file order too.
    (mevedel-tool-patch-annotate-line-numbers proposal)
    (plist-put operation :hunks
               (sort (plist-get operation :hunks)
                     ;; Not in place: the merged list shares cells with
                     ;; the original one this rolls back to.
                     :in-place nil
                     :key (lambda (hunk)
                            (or (plist-get hunk :old-start)
                                most-positive-fixnum))))
    (condition-case err
        (mevedel-tool-patch-planned-changes proposal)
      (error
       (plist-put operation :hunks original)
       (signal (car err) (cdr err))))))

(defun mevedel-patch-review--revised-summary (proposal)
  "Return a summary of PROPOSAL's applied user-revised changes, or nil."
  (let (parts)
    (dolist (operation (plist-get proposal :operations))
      (let ((path (plist-get operation :rel-path)))
        (pcase (plist-get operation :kind)
          ;; A revision replaces the file's whole derived hunk set, so
          ;; hunk indices no longer correspond to anything the model
          ;; wrote.  Report the file once instead.
          ('update
           (when (cl-some (lambda (hunk)
                            (and (plist-get hunk :modified)
                                 (plist-get hunk :selected)))
                          (plist-get operation :hunks))
             (push (format "%s (whole file revised)" path) parts)))
          ('move
           (when (and (plist-get operation :selected)
                      (cl-some (lambda (hunk) (plist-get hunk :modified))
                               (plist-get operation :hunks)))
             (push (format "%s (whole file revised)" path) parts)))
          ('add
           (when (and (plist-get operation :selected)
                      (plist-get operation :modified))
             (push (format "%s (new file content)" path) parts))))))
    (when parts
      (string-join (nreverse parts) ", "))))

(defun mevedel-patch-review--remind-of-revisions (proposal)
  "Queue a one-shot reminder about PROPOSAL's user-revised changes."
  (when-let* ((summary (mevedel-patch-review--revised-summary proposal))
              (data-buffer (plist-get proposal :data-buffer))
              ((buffer-live-p data-buffer))
              (session (buffer-local-value 'mevedel--session data-buffer)))
    (mevedel-session-ensure-reminder
     session (mevedel-reminders-make-user-revised-patch summary))))

(defun mevedel-patch-review--submit (proposal)
  "Apply PROPOSAL's selected changes and settle its review."
  (cond
   ((memq (plist-get proposal :state) '(submitting settled)))
   ((mevedel-side-conversation-mutation-warning-pending-p proposal)
    (mevedel-patch-review--render proposal))
   (t
    (plist-put proposal :state 'submitting)
    (condition-case err
        (progn
          (mevedel-tool-patch-assert-baseline proposal)
          (let* ((changes (mevedel-tool-patch-planned-changes proposal))
                 (feedback-p (> (plist-get
                                 (mevedel-tool-patch-proposal-stats
                                  proposal)
                                 :comments)
                                0))
                 (result
                  (if (or changes feedback-p)
                      (mevedel-tool-patch-result proposal changes t)
                    (list :result "Error: Patch rejected" :status 'error)))
                 (overlay (plist-get proposal :overlay)))
            (overlay-put overlay 'mevedel--remote
                         '(:body "ApplyPatch is being applied."
                           :body-kind "text"))
            (mevedel-patch-review--render proposal)
            (mevedel--prompt-announce overlay)
            (when changes
              (mevedel-patch-review--remind-of-revisions proposal))
            (if changes
                (mevedel-tool-patch-apply
                 (plist-get proposal :data-buffer) changes
                 (lambda () (mevedel--prompt--settle overlay result)))
              (mevedel--prompt--settle overlay result))))
      (error
       (plist-put proposal :state nil)
       (overlay-put (plist-get proposal :overlay) 'mevedel--remote
                    (plist-get proposal :remote))
       (plist-put proposal :conflict
                  (mevedel-tool-patch-sanitize-error
                   (error-message-string err) proposal))
       (plist-put proposal :rollback-incomplete
                  (eq (car err) 'mevedel-tool-patch-partial-rollback))
       (let ((overlay (mevedel-patch-review--render proposal)))
         (mevedel--prompt-announce overlay)))))))

(defun mevedel-patch-review-submit ()
  "Apply the selected patch changes in the review at point."
  (interactive)
  (let ((proposal (mevedel-patch-review--target-at-point
                   'mevedel-patch-proposal)))
    (unless proposal
      (user-error "No patch review at point"))
    (mevedel-patch-review--submit proposal)))


(provide 'mevedel-patch-review)
;;; mevedel-patch-review.el ends here
