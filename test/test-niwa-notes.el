;;; test-niwa-notes.el -- Source note command tests -*- lexical-binding: t -*-

;;; Commentary:

;; Drive note commands in real source files and observe their presentations.

;;; Code:

(require 'niwa)
(require 'niwa-buddy-note)
(require 'niwa-persistence)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-transfer)
(defvar niwa-notes-closed-max-count)
(defvar niwa-notes-closed-max-age-days)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-notes-readme ()
  ,test
  (test)
  :doc "README names recently closed as the destination of dismissed notes"
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat (file-name-directory (locate-library "niwa")) "README.md"))
    (should (string-match-p "Done, deleted and dismissed notes[\n ]+share recently closed"
                            (buffer-string)))))

(defmacro niwa-notes-test--with-source (&rest body)
  "Run BODY in a real source buffer in a private workspace."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "niwa-notes-" t))
          (file (file-name-concat directory "source.el"))
          (workspace (niwa-workspace--create
                      :type 'file :id directory :root directory :name "notes"))
          (niwa--instruction-states (make-hash-table :test #'equal))
          (niwa-note-labels 'expanded)
          (niwa-instruction-store--dirty nil)
          (niwa-instruction-store--failed nil)
          source)
     (unwind-protect
         (progn
           (make-directory (file-name-concat directory ".niwa"))
           (write-region "before\nselected code\nafter\n" nil file nil 'silent)
           (setq source (find-file-noselect file))
           (with-current-buffer source
             (setq-local niwa--workspace workspace)
             (goto-char (point-min))
             (forward-line 1)
             ,@body))
       (when (buffer-live-p source)
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source))
       (setq niwa-instruction-store--dirty nil)
       (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
       (delete-directory directory t))))

(defun niwa-notes-test--labels ()
  "Return all visible note label strings in the current buffer."
  (cl-remove-if-not
   (lambda (text) (and (stringp text) (string-match-p "Note [0-9]+" text)))
   (mapcar (lambda (overlay) (overlay-get overlay 'after-string))
           (overlays-in (point-min) (point-max)))))

(niwa-deftest niwa-notes-toggle-labels
  (:quiet t)
  ,test
  (test)
  :doc "labels default to collapsed"
  (should (eq 'collapsed (default-value 'niwa-note-labels)))
  :doc "collapsed labels remain visible; toggles stay stable across point motion"
  (niwa-notes-test--with-source
    (let* ((niwa-note-labels 'collapsed)
           (first (niwa-note-create "First")))
      (goto-char (point-min))
      (let ((second (niwa-note-create "Second")))
        (should (= 2 (length (niwa-notes-test--labels))))
        (should (eq 'niwa-note-you (overlay-get first 'face)))
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?e)))
          (niwa-note-menu first))
        (should (eq 'expanded (overlay-get first 'niwa-note-labels)))
        (should (string-search "First" (string-join (niwa-notes-test--labels))))
        (call-interactively #'niwa-notes-toggle-labels)
        (should (= 2 (length (niwa-notes-test--labels))))
        (goto-char (overlay-start first))
        (run-hooks 'post-command-hook)
        (goto-char (point-max))
        (run-hooks 'post-command-hook)
        (should (= 2 (length (niwa-notes-test--labels))))
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?e)))
          (niwa-note-menu second))
        (should (eq 'collapsed (overlay-get second 'niwa-note-labels)))
        (call-interactively #'niwa-notes-toggle-labels)
        (should (= 2 (length (niwa-notes-test--labels))))
        (call-interactively #'niwa-notes-toggle-labels)
        (should (eq 'collapsed (overlay-get first 'niwa-note-labels)))
        (niwa-note-edit "Changed" first)
        (should (string-search "Changed" (string-join (niwa-notes-test--labels))))
        (call-interactively #'niwa-notes-toggle-labels)
        (niwa-instruction-store-flush t)
        (niwa--clear-instruction-state workspace)
        (niwa-instruction-store-observe workspace)
        (should (= 2 (length (niwa-notes-test--labels)))))))
  :doc "buffer toggles include notes outside the narrowing and preserve point and restriction"
  (niwa-notes-test--with-source
    (let* ((niwa-note-labels 'collapsed)
           (note (niwa-note-create "Outside narrowing")))
      (goto-char (point-min))
      (niwa-note-create "Inside narrowing")
      (narrow-to-region (point-min) (line-end-position))
      (let ((position (point)) (end (point-max)))
        (call-interactively #'niwa-notes-toggle-labels)
        (should (= position (point)))
        (should (= end (point-max)))
        (should (string-search "Outside narrowing"
                               (overlay-get (overlay-get note 'niwa-label-overlay)
                                            'after-string)))
        (should (= (overlay-start note)
                   (overlay-start (overlay-get note 'niwa-label-overlay))))
        (should (= (overlay-end note)
                   (overlay-end (overlay-get note 'niwa-label-overlay))))
        (save-restriction
          (widen)
          (should (= 2 (length (niwa-notes-test--labels)))))
        (widen)))))

(niwa-deftest niwa-note-create
  (:quiet t)
  ,test
  (test)
  :doc "whole-line regions keep their original geometry and newline tint"
  (niwa-notes-test--with-source
    (let ((transient-mark-mode t) (niwa-note-labels 'collapsed))
      (set-mark (point))
      (forward-line 1)
      (activate-mark)
      (let ((note (niwa-note-create "Whole line")))
        (should (= 8 (overlay-start note)))
        (should (= 22 (overlay-end note)))
        (should (= 21 (overlay-end (overlay-get note 'niwa-label-overlay))))
        (should (memq note (overlays-at 21)))
        (should (eq 'niwa-note-you (overlay-get note 'face))))))
  :doc "partial-line regions ending at eol tint only the marked text"
  (niwa-notes-test--with-source
    (let ((transient-mark-mode t))
      (forward-char 9)
      (set-mark (point))
      (end-of-line)
      (activate-mark)
      (let ((note (niwa-note-create "One word")))
        (should (equal "code" (buffer-substring-no-properties
                                (overlay-start note) (overlay-end note))))
        (should (eq 'niwa-note-you (overlay-get note 'face)))
        (should-not (cl-some (lambda (overlay) (overlay-get overlay 'face))
                             (overlays-at (line-end-position)))))))
  :doc "expanded line labels tint the source newline but not the label's trailing margin"
  (dolist (whole-region '(nil t))
    (niwa-notes-test--with-source
      (let* ((transient-mark-mode t)
             (note (progn
                     (when whole-region
                       (set-mark (point))
                       (forward-line 1)
                       (activate-mark))
                     (niwa-note-create "Expanded")))
             (label (car (niwa-notes-test--labels))))
        (should (eq 'niwa-note-you (get-text-property 0 'face label)))
        (should (equal (if whole-region '(:inherit default :extend t) 'default)
                       (get-char-property 21 'face)))
        (should (= 21 (overlay-end (overlay-get note 'niwa-label-overlay)))))))
  :doc "current-line note shows a colored header and normal text without idle state or menu"
  (niwa-notes-test--with-source
    (niwa-note-create "Check this")
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (let ((label (string-join (niwa-notes-test--labels))))
      (should (equal "\n▍ Note 1 · You  Check this" label))
      (should (eq 'niwa-note-label-you
                  (get-text-property (string-search "▍" label) 'face label)))
      (should (eq 'default
                  (get-text-property (string-search "Check this" label) 'face label)))))
  :doc "note colors inherit the theme and preserve customized tint backgrounds"
  (niwa-notes-test--with-source
    (let ((face-spec (get 'niwa-note-agent 'face-override-spec))
          (buddy-spec (get 'niwa-note-buddy 'face-override-spec))
          (theme-properties (copy-sequence (symbol-plist 'niwa-notes-test)))
          (custom-known-themes (copy-sequence custom-known-themes))
          (custom-enabled-themes (copy-sequence custom-enabled-themes)))
      (unwind-protect
          (progn
            (deftheme niwa-notes-test "Private note palette.")
            (custom-theme-set-faces
             'niwa-notes-test
             '(default ((t (:foreground "white" :background "black"))))
             '(font-lock-function-name-face ((t (:foreground "red"))))
             '(font-lock-keyword-face ((t (:foreground "green"))))
             '(font-lock-warning-face ((t (:foreground "blue")))))
            (enable-theme 'niwa-notes-test)
            (niwa-note-create "Theme colors")
            (dolist (entry '((niwa-note-you niwa-note-label-you font-lock-keyword-face "#001900")
                             (niwa-note-agent niwa-note-label-agent font-lock-function-name-face "#190000")
                             (niwa-note-buddy niwa-note-label-buddy font-lock-warning-face "#000019")))
              (pcase-let ((`(,tint ,label ,parent ,background) entry))
                (should (eq parent (face-attribute label :inherit)))
                (should (eq 'bold (face-attribute label :weight)))
                (should (eq t (face-attribute tint :extend)))
                (should (equal background (face-background tint)))))
            (face-spec-set 'niwa-note-agent '((t (:background "purple"))))
            (face-spec-set 'niwa-note-buddy '((t (:inherit default))))
            (custom-theme-set-faces
             'niwa-notes-test
             '(default ((t (:foreground "black" :background "white"))))
             '(font-lock-keyword-face ((t (:foreground "blue")))))
            (enable-theme 'niwa-notes-test)
            (should (equal "#e5e5ff" (face-background 'niwa-note-you)))
            (should (equal "purple" (face-background 'niwa-note-agent)))
            (should (equal "white" (face-background 'niwa-note-buddy nil t))))
        (face-spec-set 'niwa-note-agent face-spec)
        (face-spec-set 'niwa-note-buddy buddy-spec)
        (disable-theme 'niwa-notes-test)
        (setplist 'niwa-notes-test theme-properties))))
  :doc "region note without text labels below the whole line and follows edits"
  (niwa-notes-test--with-source
    (let ((transient-mark-mode t))
      (set-mark (point))
      (forward-char 8)
      (activate-mark)
      (let ((note (niwa-note-create)))
        (should (equal "selected" (buffer-substring-no-properties
                                   (overlay-start note) (overlay-end note))))
        (let ((label (cl-find-if
                      (lambda (overlay) (overlay-get overlay 'after-string))
                      (overlays-in (point-min) (point-max)))))
          (should (= (line-end-position) (overlay-end label))))
        (goto-char (point-min))
        (insert "new line\n")
        (goto-char (+ 2 (overlay-start note)))
        (insert "X")
        (should (equal "seXlected" (buffer-substring-no-properties
                                    (overlay-start note) (overlay-end note))))
        (should (= 1 (length (niwa-notes-test--labels)))))))
  :doc "line notes accept no text and region notes accept text"
  (niwa-notes-test--with-source
    (niwa-note-create)
    (goto-char (point-max))
    (let ((transient-mark-mode t))
      (set-mark (point-min))
      (activate-mark)
      (niwa-note-create "A region"))
    (should (= 2 (length (niwa-notes-test--labels))))
    (should (string-match-p "A region"
                            (string-join (niwa-notes-test--labels)))))
  :doc "tabs and overwide controls in narrow labels wrap without stalling or losing text"
  (niwa-notes-test--with-source
    (let* ((fill-column 4)
           (note (niwa-note-create "\tvalue\u0080"))
           (label (overlay-get (overlay-get note 'niwa-label-overlay) 'after-string)))
      (dolist (line (split-string label "\n" t))
        (should (<= (string-width line) fill-column)))
      (should (string-search "value\\200" (replace-regexp-in-string "[ ▍\n]+" "" label)))
      (should (equal "\tvalue\u0080" (overlay-get note 'niwa-note-text)))))
  :doc "expanded labels repeat the bar at code indentation and fill the full text"
  (niwa-notes-test--with-source
    (insert "    ")
    (let* ((fill-column 60)
           (text (concat "First paragraph has several words.\n\nSecond " (make-string 500 ?x)))
           (note (niwa-note-create text)))
      (let* ((label (car (niwa-notes-test--labels)))
             (lines (split-string label "\n" t)))
        (should (equal "    ▍ Note 1 · You  First paragraph has several words." (car lines)))
        (dolist (line lines)
          (should (string-prefix-p "    ▍ " line))
          (should (<= (string-width line) fill-column)))
        (should (string-search (make-string 500 ?x)
                               (replace-regexp-in-string "[ ▍\n]+" "" label)))
        (should (string-search "First paragraph has several words." label))
        (should (string-search "\n    ▍ \n    ▍ Second" label)))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt initial &rest _)
                   (should (equal text initial))
                   initial)))
        (call-interactively #'niwa-note-edit))
      (should-not (overlay-buffer note)))))

(niwa-deftest niwa-cycle-instructions-at-point/note-label
  (:quiet t)
  ,test
  (test)
  :doc "cycling overlapping notes highlights the selected label and clears the old face"
  (niwa-notes-test--with-source
    (let* ((niwa--highlighted-instruction nil)
           (child (niwa-note-create "Child"))
           (parent (let ((transient-mark-mode t))
                     (goto-char (point-max))
                     (set-mark (point-min))
                     (activate-mark)
                     (niwa-note-create "Parent"))))
      (goto-char (overlay-start child))
      (call-interactively #'niwa-cycle-instructions-at-point)
      (should (eq 'highlight (get-text-property
                              1 'face (overlay-get (overlay-get child 'niwa-label-overlay)
                                                   'after-string))))
      (call-interactively #'niwa-cycle-instructions-at-point)
      (should (eq 'niwa-note-label-you (get-text-property
                           1 'face (overlay-get (overlay-get child 'niwa-label-overlay)
                                                'after-string))))
      (should (eq 'highlight (get-text-property
                              1 'face (overlay-get (overlay-get parent 'niwa-label-overlay)
                                                   'after-string))))
      (call-interactively #'niwa-cycle-instructions-at-point)
      (should (eq 'niwa-note-label-you (get-text-property
                           1 'face (overlay-get (overlay-get parent 'niwa-label-overlay)
                                                'after-string))))
      ;; The noted code carries the author's tint; an AI note gets its own.
      (should (eq 'niwa-note-you (overlay-get child 'face)))
      (overlay-put child 'niwa-note-author 'agent)
      (niwa-notes--render child)
      (should (eq 'niwa-note-agent (overlay-get child 'face)))
      (should (eq 'niwa-note-label-agent
                  (get-text-property
                   1 'face (overlay-get (overlay-get child 'niwa-label-overlay)
                                        'after-string))))
      (overlay-put child 'niwa-note-author 'user)
      (niwa-notes--render child)
      (let* ((label (overlay-get child 'niwa-label-overlay))
             (before (overlay-get label 'after-string)))
        (niwa-note-edit "Child" child)
        (should (eq before (overlay-get label 'after-string)))))))

(niwa-deftest niwa-note-persistence
  (:quiet t)
  ,test
  (test)
  :doc "workspace store reload preserves text and rebuilds exactly one label"
  (niwa-notes-test--with-source
    (let (messages)
      (niwa-test--with-captured-messages messages
        (niwa-note-create "Remember tomorrow"))
      (should (equal "" messages)))
    (should (file-exists-p (niwa-instruction-store-path workspace)))
    (niwa--clear-instruction-state workspace)
    (should-not (niwa-notes-test--labels))
    (niwa-instruction-store-observe workspace)
    (should (= 1 (length (niwa-notes-test--labels))))
    (should (string-match-p "Remember tomorrow"
                            (car (niwa-notes-test--labels)))))
  :doc "resuming either workspace session preserves notes created outside it"
  (niwa-notes-test--with-source
    (niwa-note-create "Shared by sessions")
    (dolist (name '("a" "b"))
      (let ((session (niwa-session-create name workspace)))
        (setf (niwa-session-save-path session) (file-name-concat directory name))
        (make-directory (niwa-session-save-path session))
        (niwa--clear-instruction-state workspace)
        (with-temp-buffer
          (setq-local niwa--workspace workspace)
          (should (niwa-session-artifacts-load-instructions session)))
        (should (= 1 (length (niwa-notes-test--labels))))
        (should (string-match-p "Shared by sessions"
                                (car (niwa-notes-test--labels))))))))

(niwa-deftest niwa-note-edit
  (:quiet t)
  ,test
  (test)
  :doc "labels dim only attention states after the header and leave body text normal"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Goal"))
          (niwa-notes--taken-watches (make-hash-table :test #'equal)))
      (dolist (entry '((unsent . nil) (sent . nil) (ready . "change ready")
                       (stale . "change stale") (working . "working…")
                       (taken . "paused")))
        (overlay-put note 'niwa-note-state (if (eq (car entry) 'taken) 'taken 'unsent))
        (overlay-put note 'niwa-note-attention-state (car entry))
        (overlay-put note 'niwa-note-session-id (and (eq (car entry) 'sent) "sent-note"))
        (niwa-note-edit "Goal" note)
        (let* ((label (car (niwa-notes-test--labels)))
               (status (cdr entry)))
          (should (equal (concat "\n▍ Note 1 · You"
                                 (and status (concat " · " status)) "  Goal") label))
          (when status
            (should (eq 'shadow (get-text-property (string-search status label) 'face label))))
          (should (eq 'default (get-text-property (string-search "Goal" label) 'face label)))))
      (puthash (overlay-get note 'niwa-uuid) t niwa-notes--taken-watches)
      (niwa-note-edit "Goal" note)
      (should (string-search "You · Buddy watches  Goal" (car (niwa-notes-test--labels))))))
  :doc "explicit note target persists to its own workspace from another buffer"
  (niwa-notes-test--with-source
    (let* ((note (niwa-note-create "Before"))
           (other-root (file-name-concat directory "other"))
           (other (niwa-workspace--create
                   :type 'file :id other-root :root other-root :name "other")))
      (make-directory (file-name-concat other-root ".niwa") t)
      (with-temp-buffer
        (setq-local niwa--workspace other)
        (niwa-note-edit "Changed from another workspace" note))
      (niwa-instruction-store-flush t)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (should (string-match-p "Changed from another workspace"
                              (car (niwa-notes-test--labels)))))))

(niwa-deftest niwa-note-menu
  (:quiet t)
  ,test
  (test)
  :doc "overlapping notes offer stable nonempty descriptions even when collapsed"
  (dolist (display '(collapsed expanded))
    (niwa-notes-test--with-source
      (let* ((niwa-note-labels display)
             (child (niwa-note-create "Child\nSecond line"))
             (transient-mark-mode t)
             (kill-ring nil)
             (kill-ring-yank-pointer nil))
        (goto-char (point-max))
        (set-mark (point-min))
        (activate-mark)
        (niwa-note-create "Parent")
        (goto-char (1+ (overlay-start child)))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (equal '("Note 1 · You  Child Second line" "Note 2 · You  Parent")
                                    (sort (copy-sequence candidates) #'string-lessp)))
                     "Note 1 · You  Child Second line"))
                  ((symbol-function 'read-key) (lambda (&rest _) ?w)))
          (call-interactively (key-binding (kbd "M-m"))))
        (should (equal "Child\nSecond line" (current-kill 0))))))
  :doc "one key selects below-label actions without a minibuffer; quit and error restore the label"
  (niwa-notes-test--with-source
    (let* ((note (niwa-note-create "Inline menu"))
           (label (overlay-get note 'niwa-label-overlay))
           (original (overlay-get label 'after-string))
           (position (point))
           (calls 0))
      (dolist (exit '(quit error nil))
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) (ert-fail "Menu opened a minibuffer")))
                  ((symbol-function 'read-key)
                   (lambda (&optional prompt &rest _)
                     (should-not prompt)
                     (cl-incf calls)
                     (let* ((text (overlay-get label 'after-string))
                            (actions (string-search "Actions:" text)))
                       (should (string-prefix-p (concat original "\n  Actions: ") text))
                       (should (eq 'shadow (get-text-property actions 'face text)))
                       (should (text-property-any actions (length text) 'face 'help-key-binding text)))
                     (if exit (signal exit '("Menu input failed")) ?z))))
          (should (eq exit (condition-case condition
                               (progn (niwa-note-menu note) nil)
                             ((quit error) (car condition))))))
        (should (equal original (overlay-get label 'after-string))))
      (should (= 3 calls))
      (should (= position (point)))
      (should (equal "before\nselected code\nafter\n" (buffer-string)))))
  :doc "closing the menu preserves a label rerendered while waiting for input"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Before waiting")))
      (cl-letf (((symbol-function 'read-key)
                 (lambda (&rest _)
                   (niwa-note-edit "Edited while waiting" note)
                   ?z)))
        (niwa-note-menu note))
      (let ((labels (string-join (niwa-notes-test--labels))))
        (should (string-search "Edited while waiting" labels))
        (should-not (string-search "Before waiting" labels))
        (should-not (string-search "Actions:" labels)))))
  :doc "keys for inapplicable actions do nothing without opening another prompt"
  (niwa-notes-test--with-source
    (let* ((note (niwa-note-create "No prepared change"))
           (label (overlay-get note 'niwa-label-overlay))
           (original (overlay-get label 'after-string)))
      (dolist (key '(?a ?m ?r ?b))
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) key)))
          (niwa-note-menu note))
        (should (equal original (overlay-get label 'after-string)))
        (should (eq 'user (overlay-get note 'niwa-note-author))))))
  :doc "sent notes offer revert only with applied history and pending changes offer apply and merge"
  (niwa-notes-test--with-source
    (let* ((note (niwa-note-create "Stateful menu"))
           (session (niwa-session--create :session-id "menu-session"))
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           expected)
      (overlay-put note 'niwa-note-session-id "menu-session")
      (with-temp-buffer
        (setq-local niwa--session session)
        (niwa-session-control-transfer-register-root-buffer session (current-buffer))
        (unwind-protect
            (cl-letf (((symbol-function 'read-key)
                       (lambda (&rest _)
                         (let ((text (downcase (replace-regexp-in-string
                                               "[][]" "" (overlay-get
                                                           (overlay-get note 'niwa-label-overlay)
                                                           'after-string)))))
                           (should (equal expected
                                          (cl-remove-if-not
                                           (lambda (key)
                                             (string-search
                                              (cdr (assq key '((?a . "apply") (?m . "merge") (?r . "revert"))))
                                              text))
                                           '(?r ?a ?m))))
                           ?w))))
              (niwa-note-menu note)
              (setf (niwa-session-file-snapshots session) '((1)))
              (niwa-note-menu note)
              (setf (niwa-session-file-snapshots session) '((1 ("source.el" :backup-name "after"))))
              (setq expected '(?r))
              (niwa-note-menu note)
              (setf (niwa-session-file-snapshots session) nil)
              (overlay-put note 'niwa-note-applied-changes '((:session-id "other" :turn 1)))
              (niwa-note-menu note)
              (overlay-put note 'niwa-note-applied-changes nil)
              (overlay-put note 'niwa-note-prepared-change '(:id "pending"))
              (setq expected '(?a ?m))
              (niwa-note-menu note))
          (niwa-session-control-transfer-unregister-root-buffer session (current-buffer))))))
  :doc "modified-key menu edits note text while plain letters insert source text"
  (niwa-notes-test--with-source
    (niwa-note-create "Before")
    (forward-char 2)
    (dolist (key '("e" "d" "s" "t" "x"))
      (should (eq #'self-insert-command (key-binding (kbd key)))))
    (let ((last-command-event ?e)) (call-interactively (key-binding "e")))
    (should (equal "before\nseelected code\nafter\n" (buffer-string)))
    ;; These stubs supply user input only.
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?i))
              ((symbol-function 'read-string)
               (lambda (&rest _) "After")))
      (call-interactively (key-binding (kbd "M-m"))))
    (should (string-match-p "After" (car (niwa-notes-test--labels))))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should (string-match-p "After" (car (niwa-notes-test--labels)))))
  :doc "copy text copies the full authored text without expanding or changing code"
  (niwa-notes-test--with-source
    (let* ((niwa-note-labels 'collapsed)
           (text (concat "First\n\nSecond " (make-string 500 ?x)))
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (note (niwa-note-create text))
           messages)
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
        (niwa-test--with-captured-messages messages
          (niwa-note-menu note)))
      (should (equal text (current-kill 0)))
      (should (string-match-p "copied" messages))
      (should (string-search "First Second" (car (niwa-notes-test--labels))))
      (should (equal "before\nselected code\nafter\n" (buffer-string)))))
  :doc "tags can be edited from the inline menu without editing source"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Tag me")))
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?g))
                ((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) '("review" "urgent"))))
        (niwa-note-menu note))
      (should (equal '(review urgent) (overlay-get note 'niwa-note-tags)))
      (should (equal "before\nselected code\nafter\n" (buffer-string)))
      (should-not (cl-some (lambda (overlay) (overlay-get overlay 'before-string))
                          (overlays-in (point-min) (point-max))))))
  :doc "menu deletion removes the note and label without changing source"
  (niwa-notes-test--with-source
    (niwa-note-create "Delete me")
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?k)))
      (call-interactively (key-binding (kbd "M-m"))))
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (should-not (niwa-notes-test--labels))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should-not (niwa-notes-test--labels)))
  :doc "done is an explicit modified-key menu action, never a plain source key"
  (niwa-notes-test--with-source
    (niwa-note-create "Close from menu")
    (should (eq #'self-insert-command (key-binding "x")))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?x)))
      (call-interactively (key-binding (kbd "M-m"))))
    (should-not (niwa-notes-test--labels))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should (eq 'done (plist-get (plist-get (car (niwa-notes-closed workspace))
                                           :instruction)
                                :closed-reason)))))

(niwa-deftest niwa-note-entry-hint
  ()
  ,test
  (test)
  :doc "entering a note announces its identity and next actions only once"
  (niwa-notes-test--with-source
    (niwa-note-create "Review the boundary")
    (let (messages)
      (niwa-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (string-match-p "Note 1 · You.*Review the boundary.*M-m.*edit.*delete"
                              messages))
      (niwa-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (equal "" messages))
      (goto-char (point-min))
      (run-hooks 'post-command-hook)
      (forward-line 1)
      (niwa-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (string-match-p "Note 1" messages)))))

(niwa-deftest niwa-note-entry-hint-search
  ()
  ,test
  (test)
  :doc "note entry does not replace an isearch prompt or consume the later hint"
  (niwa-notes-test--with-source
    (niwa-note-create "Search target")
    (let ((isearch-mode t) messages)
      (niwa-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (equal "" messages)))
    (let (messages)
      (niwa-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (string-match-p "Note 1.*Search target" messages))))
  :doc "keyboard macros do not emit note-entry hints"
  (niwa-notes-test--with-source
    (niwa-note-create "Macro target")
    (let ((executing-kbd-macro t) messages)
      (niwa-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (equal "" messages)))))

(niwa-deftest niwa-note-prepared-source-edits
  (:quiet t)
  ,test
  (test)
  :doc "inserting a newline just after noted code preserves its ready change"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Waiting change")))
      (niwa-note-hold-change (list note) '(:test t))
      (goto-char (overlay-end note))
      (insert "\n")
      (should (equal "selected code" (buffer-substring-no-properties
                                      (overlay-start note) (overlay-end note))))
      (should (eq 'ready (niwa-notes-attention-state note)))
      (goto-char (1+ (overlay-start note)))
      (insert "X")
      (should (eq 'stale (niwa-notes-attention-state note)))))
  :doc "deleting the final noted character still makes its change stale"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Waiting change")))
      (niwa-note-hold-change (list note) '(:test t))
      (delete-region (1- (overlay-end note)) (overlay-end note))
      (should (eq 'stale (niwa-notes-attention-state note))))))

(niwa-deftest niwa-note-source-undo
  (:quiet t)
  ,test
  (test)
  :doc "finishing a full code deletion never closes a code-only note"
  (niwa-notes-test--with-source
    (niwa-note-create)
    (run-hooks 'pre-command-hook)
    (erase-buffer)
    (run-hooks 'post-command-hook)
    (should (= 1 (length (niwa-notes-test--labels))))
    (should-not (niwa-notes-closed workspace t))
    (save-buffer)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should (= 1 (length (niwa-notes-test--labels)))))
  :doc "source undo restores note geometry after deleting its entire marked code"
  (niwa-notes-test--with-source
    (niwa-note-create "Keep the anchor")
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (run-hooks 'pre-command-hook)
    (delete-region (line-beginning-position) (line-beginning-position 2))
    (run-hooks 'post-command-hook)
    (undo-boundary)
    (should (= 1 (length (niwa-notes-test--labels))))
    (undo-only 1)
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (should (string-match-p "Keep the anchor" (car (niwa-notes-test--labels)))))
  :doc "source undo restores geometry but never rewinds subsequently edited note text"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Original")))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region (point) (1+ (point)))
      (undo-boundary)
      (niwa-note-edit "Newer text" note)
      (undo-only 1)
      (should (equal "before\nselected code\nafter\n" (buffer-string)))
      (should (string-match-p "Newer text" (car (niwa-notes-test--labels))))))
  :doc "source undo does not resurrect an explicitly deleted note"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Delete me")))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region (point) (1+ (point)))
      (undo-boundary)
      (niwa-note-delete note)
      (undo-only 1)
      (should-not (niwa-notes-test--labels)))))

(niwa-deftest niwa-note-store-format
  (:quiet t)
  ,test
  (test)
  :doc "format 2 notes are refused by a format 1 client without clearing data"
  (niwa-notes-test--with-source
    (niwa-note-create "Keep this")
    (let* ((path (niwa-instruction-store-path workspace))
           (bytes (with-temp-buffer (insert-file-contents path) (buffer-string))))
      (should (= 2 (plist-get (read bytes) :format)))
      ;; Simulate the older reader's supported-format constant, not its codec.
      (let ((niwa-instruction-store-format-version 1))
        (should-error (niwa-instruction-store-sync workspace)
                      :type 'niwa-instruction-store-unreadable))
      (should (equal bytes (with-temp-buffer
                             (insert-file-contents path) (buffer-string))))
      (should (string-match-p "Keep this" (car (niwa-notes-test--labels)))))))

(niwa-deftest niwa-note-done
  (:quiet t)
  ,test
  (test)
  :doc "done removes the label and restores the same note after a store reload"
  (niwa-notes-test--with-source
    (niwa-note-create "Finish this")
    (niwa-note-done)
    (should-not (niwa-notes-test--labels))
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should-not (niwa-notes-test--labels))
    (niwa-note-restore 1 workspace)
    (should (string-match-p "▍ Note 1 · You  Finish this"
                            (car (niwa-notes-test--labels)))))
  :doc "closing a parent note refreshes the surviving nested presentation"
  (niwa-notes-test--with-source
    (let* ((transient-mark-mode t)
           (child (progn (goto-char 8) (set-mark 16) (activate-mark)
                         (niwa-note-create "Child")))
           (before (string-join (niwa-notes-test--labels))))
      (goto-char (point-max))
      (set-mark (point-min))
      (activate-mark)
      (let ((note (niwa-note-create "Parent")))
        (niwa-note-done note))
      (should (overlay-buffer child))
      (should (equal-including-properties before (string-join (niwa-notes-test--labels)))))))

(niwa-deftest niwa-note-restore
  (:quiet t)
  ,test
  (test)
  :doc "a closed blank-line note reanchors after an external source change"
  (niwa-notes-test--with-source
    (setq workspace (niwa-workspace-get-or-create
                     'file directory directory "notes"))
    (setq-local niwa--workspace workspace)
    (delete-region (line-beginning-position) (line-end-position))
    (save-buffer)
    (let ((note (niwa-note-create "Keep the blank line")))
      (should (= (overlay-start note) (overlay-end note)))
      (niwa-note-done note))
    (niwa-instruction-store-flush t)
    (should-not (niwa-notes-test--labels))
    (with-temp-file file (insert "prepended\nbefore\n\nafter\n"))
    (revert-buffer t t)
    (let ((note (niwa-note-restore 1 workspace)))
      (should (= 3 (line-number-at-pos (overlay-start note))))
      (should (= (overlay-start note) (overlay-end note))))
    (should (= 1 (length (niwa-notes-test--labels))))
    (should (string-match-p "Keep the blank line"
                            (car (niwa-notes-test--labels)))))
  :doc "changed code falls back to the old line, not an obsolete character offset"
  (niwa-notes-test--with-source
    (niwa-note-create "Keep my place")
    (niwa-note-delete)
    (niwa-instruction-store-flush t)
    (erase-buffer)
    (insert "a much longer replacement for the first line\nnew code\nlast\n")
    (save-buffer)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (let ((note (niwa-note-restore 1 workspace)))
      (should (= 2 (line-number-at-pos (overlay-start note))))
      (should (equal "new code" (buffer-substring-no-properties
                                 (overlay-start note) (overlay-end note)))))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should (= 1 (length (niwa-notes-test--labels)))))
  :doc "display snapshots cannot mutate the closed note restored by the command"
  (niwa-notes-test--with-source
    (niwa-note-create "Original")
    (niwa-note-delete)
    (let* ((snapshot (car (niwa-notes-closed workspace)))
           (props (plist-get (plist-get snapshot :instruction) :properties)))
      (aset (plist-get props 'niwa-note-text) 0 ?X))
    (niwa-note-restore 1 workspace)
    (should (string-match-p "Original" (car (niwa-notes-test--labels)))))
  :doc "restore outside a narrowing preserves point and places the label at its code"
  (niwa-notes-test--with-source
    (niwa-note-create "Outside restriction")
    (niwa-note-delete)
    (narrow-to-region 1 7)
    (goto-char 3)
    (let ((note (niwa-note-restore 1 workspace)))
      (should (= 3 (point)))
      (should (= 7 (point-max)))
      (widen)
      (should (= 8 (overlay-start note)))
      (let ((label (cl-find-if
                    (lambda (overlay) (overlay-get overlay 'after-string))
                    (overlays-in (point-min) (point-max)))))
        (should (= 21 (overlay-end label))))))
  :doc "restore loads the workspace store on first use without prior activation"
  (niwa-notes-test--with-source
    (niwa-note-create "Cold restore")
    (niwa-note-delete)
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-note-restore 1 workspace)
    (should (string-match-p "Cold restore" (car (niwa-notes-test--labels)))))
  :doc "export and reload keep open and closed notes together for an unopened file"
  (niwa-notes-test--with-source
    (niwa-note-create "Closed")
    (niwa-note-done)
    (forward-line 1)
    (niwa-note-create "Still open")
    (niwa-instruction-store-flush t)
    (kill-buffer source)
    (with-temp-buffer
      (setq-local niwa--workspace workspace)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (let ((path (file-name-concat directory "export.el")))
        (niwa-save-instructions path)
        ;; Inspect the public export, not an in-memory codec helper.
        (let* ((export (with-temp-buffer
                         (insert-file-contents path) (read (current-buffer))))
               (files (plist-get export :files)))
          (should (equal '("source.el") (mapcar #'car files)))
          (should (= 2 (length (plist-get (cdar files) :instructions))))
          (should (stringp (plist-get (cdar files) :content-hash))))
        (niwa-load-instructions path)
        (niwa-note-restore 1 workspace)))
    (setq source (find-buffer-visiting file))
    (with-current-buffer source
      (should (= 2 (length (niwa-notes-test--labels)))))))

(niwa-deftest niwa-note-close
  (:quiet t)
  ,test
  (test)
  :doc "done, deleted and dismissed share one persistent closed list with reasons"
  (niwa-notes-test--with-source
    (niwa-note-create "Done")
    (niwa-note-done)
    (niwa-note-create "Deleted")
    (niwa-note-delete)
    (niwa-note-close (niwa-note-create "Dismissed") 'dismissed)
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should-not (niwa-notes-test--labels))
    (should (equal '(deleted dismissed done)
                   (sort (mapcar (lambda (entry)
                                   (plist-get (plist-get entry :instruction)
                                              :closed-reason))
                                 (niwa-notes-closed workspace))
                         #'string-lessp)))
    (dolist (id '(1 2 3)) (niwa-note-restore id workspace))
    (should (= 3 (length (niwa-notes-test--labels)))))
  :doc "custom count limit keeps the newest closed notes across reload"
  (niwa-notes-test--with-source
    (let ((niwa-notes-closed-max-count 2))
      (dolist (text '("First" "Second" "Third"))
        (niwa-note-create text)
        (niwa-note-done))
      (niwa-instruction-store-flush t)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (should (= 2 (length (niwa-notes-closed workspace))))
      (should-error (niwa-note-restore 1 workspace) :type 'user-error)
      (niwa-note-restore 2 workspace)
      (niwa-note-restore 3 workspace)
      (should (string-match-p "Second" (string-join (niwa-notes-test--labels))))
      (should (string-match-p "Third" (string-join (niwa-notes-test--labels))))))
  :doc "150 dismissed remarks leave the newest 100 done or deleted notes restorable"
  (niwa-notes-test--with-source
    (dotimes (i 101)
      (let ((note (niwa-note-create (format "Closed %s" i) :defer-write t)))
        (if (cl-evenp i) (niwa-note-done note) (niwa-note-delete note))))
    (dotimes (i 150)
      (let ((note (niwa-note-create (format "Remark %s" i) :defer-write t)))
        (overlay-put note 'niwa-note-author 'buddy)
        (niwa-buddy-dismiss-note note)))
    (let ((note (niwa-note-create "Agent note" :defer-write t)))
      (overlay-put note 'niwa-note-author 'agent)
      (niwa-note-dismiss note))
    (niwa-instruction-store-sync workspace)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should (= 251 (length (niwa-notes-closed workspace))))
    (should-error (niwa-note-restore 1 workspace) :type 'user-error)
    (dotimes (i 100)
      (should (overlay-buffer (niwa-note-restore (+ i 2) workspace))))
    (should (= 100 (length (niwa-notes-test--labels)))))
  :doc "custom age limit expires at the boundary and cannot be bypassed by restore"
  (niwa-notes-test--with-source
    (let ((niwa-notes-closed-max-count nil)
          (niwa-notes-closed-max-age-days 1)
          (clock (float-time))
          (real-time (symbol-function 'float-time)))
      ;; Only the wall-clock boundary is replaced; store operations stay real.
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time)
                   (if time (funcall real-time time) clock))))
        (niwa-note-create "Old")
        (niwa-note-delete)
        (niwa-instruction-store-flush t)
        (setq clock (+ clock 86399))
        (should (= 1 (length (niwa-notes-closed workspace))))
        (setq clock (1+ clock))
        (should-error (niwa-note-restore 1 workspace) :type 'user-error)
        (niwa-instruction-store-flush t)
        (niwa--clear-instruction-state workspace)
        (niwa-instruction-store-observe workspace)
        (let ((niwa-notes-closed-max-age-days nil))
          (should-not (niwa-notes-closed workspace))
          (should-error (niwa-note-restore 1 workspace) :type 'user-error)))))
  :doc "dismissed Buddy and agent notes ignore zero count but still expire at 30 days"
  (niwa-notes-test--with-source
    (let ((niwa-notes-closed-max-count 0)
          (clock (float-time))
          (real-time (symbol-function 'float-time)))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time)
                   (if time (funcall real-time time) clock))))
        (let ((buddy (niwa-note-create "Buddy" :defer-write t))
              (agent (niwa-note-create "Agent" :defer-write t)))
          (overlay-put buddy 'niwa-note-author 'buddy)
          (overlay-put agent 'niwa-note-author 'agent)
          (niwa-buddy-dismiss-note buddy)
          (niwa-note-dismiss agent))
        (niwa-instruction-store-sync workspace)
        (niwa--clear-instruction-state workspace)
        (niwa-instruction-store-observe workspace)
        (setq clock (+ clock (* 30 86400) -1))
        (should (= 2 (length (niwa-notes-closed workspace))))
        (setq clock (1+ clock))
        (dolist (id '(1 2))
          (should-error (niwa-note-restore id workspace) :type 'user-error))
        (niwa-instruction-store-sync workspace)
        (niwa--clear-instruction-state workspace)
        (niwa-instruction-store-observe workspace)
        (let ((niwa-notes-closed-max-age-days nil))
          (should-not (niwa-notes-closed workspace))))))
  :doc "a closed import without a timestamp cannot block later store writes"
  (niwa-notes-test--with-source
    (dolist (text '("Undated" "Recent"))
      (niwa-note-create text)
      (niwa-note-done))
    (let ((export (file-name-concat directory "notes.el")))
      (niwa-save-instructions export)
      (let* ((data (with-temp-buffer
                     (insert-file-contents export)
                     (read (current-buffer))))
             (info (cdar (plist-get data :files))))
        (setf (plist-get info :instructions)
              (mapcar (lambda (instruction)
                        (when (= 1 (plist-get (plist-get instruction :properties) 'niwa-id))
                          (cl-remf instruction :closed-at))
                        instruction)
                      (plist-get info :instructions)))
        (write-region (prin1-to-string data) nil export nil 'silent))
      (let ((niwa-notes-closed-max-count nil)
            (niwa-notes-closed-max-age-days nil))
        (niwa-load-instructions export)
        (niwa-instruction-store-sync workspace)
        (should (= 2 (length (niwa-notes-closed workspace))))))
    ;; An undated record is oldest, so ordinary age expiry can remove it.
    (niwa-instruction-store-sync workspace)
    (should (= 1 (length (niwa-notes-closed workspace))))))

(niwa-deftest niwa-note-source-missing
  (:quiet t)
  ,test
  (test)
  :doc "a missing source retains an open note and reattaches it when the file returns"
  (niwa-notes-test--with-source
    (niwa-note-create "Wait for my source")
    (delete-file file)
    (run-hooks 'post-command-hook)
    (should-not (niwa-notes-closed workspace t))
    (niwa-instruction-store-flush t)
    (kill-buffer source)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (let ((export (file-name-concat directory "export.el")))
      (with-temp-buffer
        (setq-local niwa--workspace workspace)
        (niwa-save-instructions export))
      (let* ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer))))
             (instruction (car (plist-get (cdar (plist-get data :files)) :instructions))))
        (should (= 1 (length (plist-get data :files))))
        (should (plist-get (cdar (plist-get data :files)) :source-missing))
        (should (eq 'unsent (plist-get (plist-get instruction :properties)
                                      'niwa-note-state)))))
    (write-region "before\nselected code\nafter\n" nil file nil 'silent)
    (setq source (find-file-noselect file))
    (with-current-buffer source
      (setq-local niwa--workspace workspace)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (should (string-match-p "Wait for my source"
                              (car (niwa-notes-test--labels)))))
    (should-not (niwa-notes-closed workspace t)))
  :doc "notes in an open missing-file buffer stay live and serialize once after save"
  (niwa-notes-test--with-source
    (niwa-note-create "Old")
    (delete-file file)
    (run-hooks 'post-command-hook)
    (should (= 1 (length (niwa-notes-test--labels))))
    (goto-char (point-min))
    (niwa-note-create "New")
    (run-hooks 'post-command-hook)
    (should (= 2 (length (niwa-notes-test--labels))))
    (let ((export (file-name-concat directory "export.el")))
      (niwa-save-instructions export)
      (let* ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer))))
             (files (plist-get data :files)))
        (should (= 1 (length files)))
        (should (plist-get (cdar files) :source-missing))
        (should (= 2 (length (plist-get (cdar files) :instructions)))))
      (save-buffer)
      (niwa-save-instructions export)
      (let* ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer))))
             (files (plist-get data :files)))
        (should (= 1 (length files)))
        (should-not (plist-get (cdar files) :source-missing))
        (should (equal '("New" "Old")
                       (sort (mapcar (lambda (instruction)
                                       (plist-get (plist-get instruction :properties)
                                                  'niwa-note-text))
                                     (plist-get (cdar files) :instructions))
                             #'string-lessp)))))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should (= 2 (length (niwa-notes-test--labels)))))
  :doc "the existing local source observation clears missing state when a file returns"
  (niwa-notes-test--with-source
    (niwa-note-create "Still here")
    (delete-file file)
    (run-hooks 'post-command-hook)
    (write-region "before\nselected code\nafter\n" nil file nil 'silent)
    (run-hooks 'post-command-hook)
    (let ((export (file-name-concat directory "export.el")))
      (niwa-save-instructions export)
      (let ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer)))))
        (should-not (plist-get (cdar (plist-get data :files)) :source-missing))))
    (should (= 1 (length (niwa-notes-test--labels))))))

(niwa-deftest niwa-delete-instructions/recently-closed
  (:quiet t)
  ,test
  (test)
  :doc "legacy deletion at point retains a live note in recently closed"
  (niwa-notes-test--with-source
    (niwa-note-create "Deleted by legacy command")
    (niwa-delete-instructions)
    (should-not (niwa-notes-test--labels))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (niwa-note-restore 1 workspace)
    (should (string-match-p "Deleted by legacy command"
                            (car (niwa-notes-test--labels))))))

(niwa-deftest niwa-delete-instruction/forget-note
  (:quiet t)
  ,test
  (test)
  :doc "automatic retraction forgets a note without closing or recycling its number"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Retracted remark")))
      (niwa--delete-instruction note nil t)
      (should-not (overlay-buffer note)))
    (should-not (niwa-notes-test--labels))
    (should-not (niwa-notes-closed workspace t))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (should-not (niwa-notes-test--labels))
    (should-not (niwa-notes-closed workspace t))
    (niwa-note-create "Next")
    (should (string-match-p "Note 2" (car (niwa-notes-test--labels))))))

(niwa-deftest niwa-notes-closed
  (:quiet t)
  ,test
  (test)
  :doc "loaded-only queries skip cold observation and still return isolated snapshots"
  (niwa-notes-test--with-source
    (niwa-note-create "Original")
    (niwa-note-delete)
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (should-not (niwa-notes-closed workspace t))
    (should (= 1 (length (niwa-notes-closed workspace))))
    (let* ((snapshot (car (niwa-notes-closed workspace t)))
           (props (plist-get (plist-get snapshot :instruction) :properties)))
      (aset (plist-get props 'niwa-note-text) 0 ?X))
    (niwa-note-restore 1 workspace)
    (should (string-match-p "Original" (car (niwa-notes-test--labels))))))

(niwa-deftest niwa-delete-all-instructions/recently-closed
  (:quiet t)
  ,test
  (test)
  :doc "legacy bulk deletion closes notes and retains their history"
  (niwa-notes-test--with-source
    (niwa-note-create "Keep my history")
    (goto-char (point-min))
    (let ((note (niwa-note-create "Another note")))
      (should (= 2 (niwa-delete-all-instructions)))
      (should-not (overlay-buffer note)))
    (should-not (niwa-notes-test--labels))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (niwa-note-restore 1 workspace)
    (should (string-match-p "Keep my history" (car (niwa-notes-test--labels))))))

(defun niwa-notes-test--dirty-p (workspace)
  "Return non-nil when WORKSPACE has an unflushed store touch."
  (and (memq workspace niwa-instruction-store--dirty) t))

(niwa-deftest niwa-note-mark-sent (:quiet t)
  ,test
  (test)
  :doc "marks notes sent, keeps an existing session id, fills an absent one and touches once"
  (niwa-notes-test--with-source
    (let ((first (niwa-note-create "First"))
          (second (progn (goto-char (point-min)) (niwa-note-create "Second"))))
      (niwa-instruction-store-flush t)
      (setq niwa-instruction-store--dirty nil)
      (overlay-put first 'niwa-note-session-id "existing")
      (niwa-note-mark-sent (list first second) "new")
      (should (equal "existing" (overlay-get first 'niwa-note-session-id)))
      (should (equal "new" (overlay-get second 'niwa-note-session-id)))
      (should (eq 'sent (overlay-get first 'niwa-note-state)))
      (should (eq 'sent (overlay-get second 'niwa-note-state)))
      (should (niwa-notes-test--dirty-p workspace)))))

(niwa-deftest niwa-note-hold-change (:quiet t)
  ,test
  (test)
  :doc "holds the record as ready on every note, renders it and touches without flushing"
  (niwa-notes-test--with-source
    (let ((first (niwa-note-create "First"))
          (second (progn (goto-char (point-min)) (niwa-note-create "Second")))
          (record '(:id "r" :session-id "s" :turn 1))
          flushes)
      (niwa-instruction-store-flush t)
      (setq niwa-instruction-store--dirty nil)
      (cl-letf (((symbol-function 'niwa-instruction-store-flush)
                 (lambda (&rest _) (push t flushes))))
        (niwa-note-hold-change (list first second) record))
      (should-not flushes)
      (dolist (note (list first second))
        (should (eq record (overlay-get note 'niwa-note-prepared-change)))
        (should (eq 'ready (niwa-notes-attention-state note)))
        (should (string-search "change ready"
                               (overlay-get (overlay-get note 'niwa-label-overlay)
                                            'after-string))))
      (should (niwa-notes-test--dirty-p workspace)))))

(niwa-deftest niwa-note-record-applied (:quiet t)
  ,test
  (test)
  :doc "records the applied pair on every batch sibling, with files only for a foreign session"
  (niwa-notes-test--with-source
    (let* ((own (niwa-note-create "Own"))
           (foreign (progn (goto-char (point-min)) (niwa-note-create "Foreign")))
           (unrelated (progn (goto-char (point-max)) (forward-line -1)
                             (niwa-note-create "Unrelated")))
           (record '(:id "r" :session-id "s" :turn 2))
           (files '(("a.el" :backup-name "b"))))
      (niwa-note-mark-sent (list own) "s")
      (niwa-note-mark-sent (list foreign) "other")
      (niwa-note-hold-change (list own foreign) record)
      (niwa-note-hold-change (list unrelated) '(:id "x" :session-id "s" :turn 1))
      (setq niwa-instruction-store--dirty nil)
      (niwa-note-record-applied record files workspace)
      (should (equal '((:session-id "s" :turn 2))
                     (overlay-get own 'niwa-note-applied-changes)))
      (should (equal `((:session-id "s" :turn 2 :files ,files))
                     (overlay-get foreign 'niwa-note-applied-changes)))
      (dolist (note (list own foreign))
        (should-not (overlay-get note 'niwa-note-prepared-change))
        (should-not (overlay-get note 'niwa-note-attention-state)))
      (should (overlay-get unrelated 'niwa-note-prepared-change))
      (should-not (overlay-get unrelated 'niwa-note-applied-changes))
      (should (niwa-notes-test--dirty-p workspace)))))

(niwa-deftest niwa-note-mark-stale (:quiet t)
  ,test
  (test)
  :doc "marks the prepared change stale and renders it"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Waiting")))
      (niwa-note-hold-change (list note) '(:id "r"))
      (niwa-note-mark-stale note)
      (should (eq 'stale (niwa-notes-attention-state note)))
      (should (string-search "change stale"
                             (overlay-get (overlay-get note 'niwa-label-overlay)
                                          'after-string))))))

(niwa-deftest niwa-note-set-settings (:quiet t)
  ,test
  (test)
  :doc "merges settings into the note's own and touches the store"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Configure")))
      (overlay-put note 'niwa-note-settings '(:preset a :effort low))
      (setq niwa-instruction-store--dirty nil)
      (niwa-note-set-settings note '(:effort high :provider p))
      (should (equal '(high p a)
                     (mapcar (lambda (key)
                               (plist-get (overlay-get note 'niwa-note-settings) key))
                             '(:effort :provider :preset))))
      (should (niwa-notes-test--dirty-p workspace)))))

(niwa-deftest niwa-note-keep (:quiet t)
  ,test
  (test)
  :doc "keeps a Buddy remark as the user's note and touches the store"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-make source (point) (line-end-position) :text "Hm" :author 'buddy)))
      (setq niwa-instruction-store--dirty nil)
      (niwa-note-keep note)
      (should (eq 'user (overlay-get note 'niwa-note-author)))
      (should (overlay-get note 'niwa-note-kept-from-buddy))
      (should (niwa-notes-test--dirty-p workspace))))
  :doc "refuses a note that is not a Buddy remark"
  (niwa-notes-test--with-source
    (let ((note (niwa-note-create "Mine")))
      (should-error (niwa-note-keep note) :type 'user-error)
      (should (eq 'user (overlay-get note 'niwa-note-author)))
      (should-not (overlay-get note 'niwa-note-kept-from-buddy)))))

(niwa-deftest niwa-note-make (:quiet t)
  ,test
  (test)
  :doc "creates an unsent note in a buffer without a file and neither writes nor touches the store"
  (niwa-notes-test--with-source
    (let ((scratch (generate-new-buffer " *note-make*")))
      (unwind-protect
          (with-current-buffer scratch
            (setq-local niwa--workspace workspace)
            (insert "remark\n")
            (let ((note (niwa-note-make scratch 1 7 :text "Unsaved" :author 'buddy
                                           :span 'line)))
              (should-not buffer-file-name)
              (should (eq 'buddy (overlay-get note 'niwa-note-author)))
              (should (eq 'unsent (overlay-get note 'niwa-note-state)))
              (should (equal "Unsaved" (overlay-get note 'niwa-note-text)))
              (should (overlay-get note 'niwa-label-overlay))))
        (set-buffer-modified-p nil)
        (kill-buffer scratch))
      (should-not niwa-instruction-store--dirty)
      (should-not (file-exists-p (niwa-instruction-store-path workspace)))))
  :doc "leaves an unrelated active region, point and the selected window untouched"
  (niwa-notes-test--with-source
    (let ((transient-mark-mode t) (window (selected-window)))
      (set-window-buffer window source)
      (goto-char (point-max))
      (push-mark (point-min) t t)
      (let ((mark (mark)) (point (point)))
        (niwa-note-make source 8 21 :text "Agent" :author 'agent)
        (should (region-active-p))
        (should (= mark (mark)))
        (should (= point (point)))
        (should (eq window (selected-window)))))))

(niwa-deftest niwa-notes-rewind-live (:quiet t)
  ,test
  (test)
  :doc "live, stashed and closed notes get the same Rewind result"
  (niwa-notes-test--with-source
    (let* ((live (niwa-note-create "Live"))
           (keys '(niwa-note-state niwa-note-prepared-change
                   niwa-note-attention-state niwa-note-applied-changes))
           (build (lambda ()
                    (list :open-state 'sent
                          :properties
                          (list 'niwa-note-state 'sent
                                'niwa-note-session-id "s"
                                'niwa-note-prepared-change '(:session-id "s" :turn 2)
                                'niwa-note-attention-state 'ready
                                'niwa-note-applied-changes
                                '((:session-id "s" :turn 1) (:session-id "s" :turn 3)
                                  (:session-id "o" :turn 3))))))
           (stashed (funcall build))
           (closed (funcall build))
           (state (niwa--instruction-state (niwa--instruction-workspace-key workspace))))
      (niwa-note-hold-change (list live) '(:session-id "s" :turn 2))
      (niwa-note-mark-sent (list live) "s")
      (overlay-put live 'niwa-note-applied-changes
                   '((:session-id "s" :turn 1) (:session-id "s" :turn 3)
                     (:session-id "o" :turn 3)))
      (push (list "stashed.el" :instructions (list stashed)) (plist-get state :instructions))
      ;; Seed the key in place; setf on an absent key would rebind STATE.
      (plist-put state :closed-notes nil)
      (push (list :file "closed.el" :instruction closed) (plist-get state :closed-notes))
      (cl-flet ((summary (plist) (mapcar (lambda (key) (plist-get plist key)) keys)))
        (niwa-notes-rewind-live workspace "s" 1)
        (let ((expected '(sent nil nil ((:session-id "s" :turn 1) (:session-id "o" :turn 3)))))
          (should (equal expected (summary (plist-get stashed :properties))))
          (should (equal expected (summary (plist-get closed :properties))))
          (should (equal expected (mapcar (lambda (key) (overlay-get live key)) keys))))
        ;; Turn 0 returns sent notes, open or closed, to unsent.
        (niwa-notes-rewind-live workspace "s" 0)
        (should (eq 'unsent (overlay-get live 'niwa-note-state)))
        (should (eq 'unsent (plist-get (plist-get stashed :properties) 'niwa-note-state)))
        (should (eq 'unsent (plist-get closed :open-state)))
        (should (equal '((:session-id "o" :turn 3))
                       (overlay-get live 'niwa-note-applied-changes)))))))

(niwa-deftest niwa-notes--label-string
  (:quiet t)
  ,test
  (test)
  :doc "collapsed labels fit indentation and window, keep identity and never change authored text"
  (niwa-notes-test--with-source
    (insert "    ")
    (let* ((niwa-note-labels 'collapsed)
           (fill-column 25)
           (text (concat "First\nSecond " (make-string 100 ?x)))
           (note (niwa-note-create text)))
      (dolist (width '(38 120))
        (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) width)))
          (niwa-notes--refit-labels (selected-window))
          (let* ((label (overlay-get (overlay-get note 'niwa-label-overlay) 'after-string))
                 (lines (split-string label "\n" t)))
            (should (= 1 (length lines)))
            (should (string-prefix-p "    ▍ Note 1 · You  First Second " (car lines)))
            (should (string-suffix-p "…" label))
            (should (= (string-width (car lines)) (min 86 width))))))
      (should (equal text (overlay-get note 'niwa-note-text))))))

(niwa-deftest niwa-notes--at-point
  (:quiet t)
  ,test
  (test)
  :doc "nested source tints prioritize the inner note and commands choose either identity"
  (niwa-notes-test--with-source
    (let* ((child (niwa-note-create "Child"))
           (parent (niwa-note-make source (point-min) (overlay-end child)
                                           :text "Parent" :author 'agent)))
      (should (> (overlay-get child 'priority) (overlay-get parent 'priority)))
      (should (eq 'niwa-note-you (get-char-property (overlay-start child) 'face)))
      (should (= (overlay-end (overlay-get child 'niwa-label-overlay))
                 (overlay-end (overlay-get parent 'niwa-label-overlay))))
      (dolist (note (list parent child))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (should (= 2 (length choices)))
                     (cl-find-if (lambda (choice)
                                   (string-prefix-p
                                    (format "Note %s ·" (overlay-get note 'niwa-id)) choice))
                                 choices))))
          (should (eq note (niwa-notes--at-point)))))
      (let (messages)
        (niwa-test--with-captured-messages messages (niwa-notes--entry-hint))
        (should (string-match-p "Note 1 · You.*Child.*Note 2 · AI.*Parent.*choose note" messages))))))

(niwa-deftest niwa-notes--stacked-newline-face
  (:quiet t :doc "only the first after-string tints the source newline in either priority order")
  (with-temp-buffer
    (insert "outer\ninner\n")
    (let* ((outer (niwa-note-make (current-buffer) 1 (point-max)
                                    :span 'line :text "Outer"))
           (inner (niwa-note-make (current-buffer) 7 (point-max)
                                    :span 'line :author 'buddy :text "Inner"))
           (outer-label (overlay-get outer 'niwa-label-overlay))
           (inner-label (overlay-get inner 'niwa-label-overlay)))
      (should (= (overlay-end outer-label) (overlay-end inner-label)))
      (should (> (overlay-get inner-label 'priority) (overlay-get outer-label 'priority)))
      (should (eq 'niwa-note-buddy
                  (get-text-property 0 'face (overlay-get inner-label 'after-string))))
      (should (eq 'default
                  (get-text-property 0 'face (overlay-get outer-label 'after-string))))
      (overlay-put outer 'priority (1+ (overlay-get inner 'priority)))
      (niwa-notes--render outer)
      (should (> (overlay-get outer-label 'priority) (overlay-get inner-label 'priority)))
      (should (eq 'niwa-note-you
                  (get-text-property 0 'face (overlay-get outer-label 'after-string))))
      (should (eq 'default
                  (get-text-property 0 'face (overlay-get inner-label 'after-string))))
      (niwa--delete-instruction-overlay outer)
      (should (eq 'niwa-note-buddy
                  (get-text-property 0 'face (overlay-get inner-label 'after-string)))))))

(niwa-deftest niwa-notes--nested-region-newline-face
  (:quiet t)
  ,test
  (test)
  :doc "an exact-text child leaves the covering line tint on the source row, not its label"
  (with-temp-buffer
    (insert "(= (car user) id))\nusers))\n")
    (let* ((outer (niwa-note-make (current-buffer) 1 (point-max)
                                    :span 'line :text "Outer"))
           (inner (niwa-note-make (current-buffer) 4 14
                                    :span 'char :author 'agent :text "Inner"))
           (label (overlay-get inner 'niwa-label-overlay))
           (newline (overlay-end label)))
      (should (< newline (overlay-end (overlay-get outer 'niwa-label-overlay))))
      (should (eq 'niwa-note-agent (get-char-property 4 'face)))
      (should (eq 'niwa-note-you (get-char-property 14 'face)))
      (should (eq 'niwa-note-you
                  (get-text-property 0 'face (overlay-get label 'after-string))))
      (should (equal '(:inherit default :extend t) (get-char-property newline 'face)))
      (overlay-put outer 'niwa-note-author 'buddy)
      (niwa-notes--render outer)
      (should (eq 'niwa-note-buddy
                  (get-text-property 0 'face (overlay-get label 'after-string))))
      (niwa--delete-instruction-overlay outer)
      (should-not (get-text-property 0 'face (overlay-get label 'after-string)))
      (should-not (get-char-property newline 'face))))
  :doc "the covering line stays tinted regardless of which label is displayed first"
  (with-temp-buffer
    (insert "(= (car user) id))\n")
    (let* ((outer (niwa-note-make (current-buffer) 1 (point-max)
                                    :span 'line :text "Outer"))
           (inner (niwa-note-make (current-buffer) 4 14
                                    :span 'char :author 'agent :text "Inner"))
           (outer-label (overlay-get outer 'niwa-label-overlay))
           (inner-label (overlay-get inner 'niwa-label-overlay))
           (newline (overlay-end inner-label)))
      (should (= newline (overlay-end outer-label)))
      (dolist (order (list (list inner-label outer-label) (list outer-label inner-label)))
        (let ((first (car order)) (second (cadr order)))
          (should (> (overlay-get first 'priority) (overlay-get second 'priority)))
          (should (eq 'niwa-note-you
                      (get-text-property 0 'face (overlay-get first 'after-string))))
          (should (eq 'default
                      (get-text-property 0 'face (overlay-get second 'after-string))))
          (should (equal '(:inherit default :extend t) (get-char-property newline 'face))))
        (overlay-put outer 'priority (1+ (overlay-get inner 'priority)))
        (niwa-notes--render outer)))))

(provide 'test-niwa-notes)
;;; test-niwa-notes.el ends here
