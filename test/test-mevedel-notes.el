;;; test-mevedel-notes.el -- Source note command tests -*- lexical-binding: t -*-

;;; Commentary:

;; Drive note commands in real source files and observe their presentations.

;;; Code:

(require 'mevedel)
(require 'mevedel-buddy-note)
(require 'mevedel-persistence)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-control-transfer)
(defvar mevedel-notes-closed-max-count)
(defvar mevedel-notes-closed-max-age-days)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-notes-readme ()
  ,test
  (test)
  :doc "README names recently closed as the destination of dismissed notes"
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat (file-name-directory (locate-library "mevedel")) "README.md"))
    (should (string-match-p "Done, deleted and dismissed notes[\n ]+share recently closed"
                            (buffer-string)))))

(defmacro mevedel-notes-test--with-source (&rest body)
  "Run BODY in a real source buffer in a private workspace."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "mevedel-notes-" t))
          (file (file-name-concat directory "source.el"))
          (workspace (mevedel-workspace--create
                      :type 'file :id directory :root directory :name "notes"))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel-note-labels 'expanded)
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          source)
     (unwind-protect
         (progn
           (make-directory (file-name-concat directory ".mevedel"))
           (write-region "before\nselected code\nafter\n" nil file nil 'silent)
           (setq source (find-file-noselect file))
           (with-current-buffer source
             (setq-local mevedel--workspace workspace)
             (goto-char (point-min))
             (forward-line 1)
             ,@body))
       (when (buffer-live-p source)
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source))
       (setq mevedel-instruction-store--dirty nil)
       (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
       (delete-directory directory t))))

(defun mevedel-notes-test--labels ()
  "Return all visible note label strings in the current buffer."
  (cl-remove-if-not
   (lambda (text) (and (stringp text) (string-match-p "Note [0-9]+" text)))
   (mapcar (lambda (overlay) (overlay-get overlay 'after-string))
           (overlays-in (point-min) (point-max)))))

(mevedel-deftest mevedel-notes-toggle-labels
  (:quiet t)
  ,test
  (test)
  :doc "labels default to collapsed"
  (should (eq 'collapsed (default-value 'mevedel-note-labels)))
  :doc "collapsed labels remain visible; toggles stay stable across point motion"
  (mevedel-notes-test--with-source
    (let* ((mevedel-note-labels 'collapsed)
           (first (mevedel-note-create "First")))
      (goto-char (point-min))
      (let ((second (mevedel-note-create "Second")))
        (should (= 2 (length (mevedel-notes-test--labels))))
        (should (eq 'mevedel-note-you (overlay-get first 'face)))
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?e)))
          (mevedel-note-menu first))
        (should (eq 'expanded (overlay-get first 'mevedel-note-labels)))
        (should (string-search "First" (string-join (mevedel-notes-test--labels))))
        (call-interactively #'mevedel-notes-toggle-labels)
        (should (= 2 (length (mevedel-notes-test--labels))))
        (goto-char (overlay-start first))
        (run-hooks 'post-command-hook)
        (goto-char (point-max))
        (run-hooks 'post-command-hook)
        (should (= 2 (length (mevedel-notes-test--labels))))
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?e)))
          (mevedel-note-menu second))
        (should (eq 'collapsed (overlay-get second 'mevedel-note-labels)))
        (call-interactively #'mevedel-notes-toggle-labels)
        (should (= 2 (length (mevedel-notes-test--labels))))
        (call-interactively #'mevedel-notes-toggle-labels)
        (should (eq 'collapsed (overlay-get first 'mevedel-note-labels)))
        (mevedel-note-edit "Changed" first)
        (should (string-search "Changed" (string-join (mevedel-notes-test--labels))))
        (call-interactively #'mevedel-notes-toggle-labels)
        (mevedel-instruction-store-flush t)
        (mevedel--clear-instruction-state workspace)
        (mevedel-instruction-store-observe workspace)
        (should (= 2 (length (mevedel-notes-test--labels)))))))
  :doc "buffer toggles include notes outside the narrowing and preserve point and restriction"
  (mevedel-notes-test--with-source
    (let* ((mevedel-note-labels 'collapsed)
           (note (mevedel-note-create "Outside narrowing")))
      (goto-char (point-min))
      (mevedel-note-create "Inside narrowing")
      (narrow-to-region (point-min) (line-end-position))
      (let ((position (point)) (end (point-max)))
        (call-interactively #'mevedel-notes-toggle-labels)
        (should (= position (point)))
        (should (= end (point-max)))
        (should (string-search "Outside narrowing"
                               (overlay-get (overlay-get note 'mevedel-label-overlay)
                                            'after-string)))
        (should (= (overlay-start note)
                   (overlay-start (overlay-get note 'mevedel-label-overlay))))
        (should (= (overlay-end note)
                   (overlay-end (overlay-get note 'mevedel-label-overlay))))
        (save-restriction
          (widen)
          (should (= 2 (length (mevedel-notes-test--labels)))))
        (widen)))))

(mevedel-deftest mevedel-note-create
  (:quiet t)
  ,test
  (test)
  :doc "whole-line regions keep their original geometry and newline tint"
  (mevedel-notes-test--with-source
    (let ((transient-mark-mode t) (mevedel-note-labels 'collapsed))
      (set-mark (point))
      (forward-line 1)
      (activate-mark)
      (let ((note (mevedel-note-create "Whole line")))
        (should (= 8 (overlay-start note)))
        (should (= 22 (overlay-end note)))
        (should (= 21 (overlay-end (overlay-get note 'mevedel-label-overlay))))
        (should (memq note (overlays-at 21)))
        (should (eq 'mevedel-note-you (overlay-get note 'face))))))
  :doc "partial-line regions ending at eol tint only the marked text"
  (mevedel-notes-test--with-source
    (let ((transient-mark-mode t))
      (forward-char 9)
      (set-mark (point))
      (end-of-line)
      (activate-mark)
      (let ((note (mevedel-note-create "One word")))
        (should (equal "code" (buffer-substring-no-properties
                                (overlay-start note) (overlay-end note))))
        (should (eq 'mevedel-note-you (overlay-get note 'face)))
        (should-not (cl-some (lambda (overlay) (overlay-get overlay 'face))
                             (overlays-at (line-end-position)))))))
  :doc "expanded line labels tint the source newline but not the label's trailing margin"
  (dolist (whole-region '(nil t))
    (mevedel-notes-test--with-source
      (let* ((transient-mark-mode t)
             (note (progn
                     (when whole-region
                       (set-mark (point))
                       (forward-line 1)
                       (activate-mark))
                     (mevedel-note-create "Expanded")))
             (label (car (mevedel-notes-test--labels))))
        (should (eq 'mevedel-note-you (get-text-property 0 'face label)))
        (should (eq 'default (get-char-property 21 'face)))
        (should (= 21 (overlay-end (overlay-get note 'mevedel-label-overlay)))))))
  :doc "current-line note shows a colored header and normal text without idle state or menu"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Check this")
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (let ((label (string-join (mevedel-notes-test--labels))))
      (should (equal "\n▍ Note 1 · You  Check this" label))
      (should (eq 'mevedel-note-label-you
                  (get-text-property (string-search "▍" label) 'face label)))
      (should (eq 'default
                  (get-text-property (string-search "Check this" label) 'face label)))))
  :doc "note colors inherit the theme and preserve customized tint backgrounds"
  (mevedel-notes-test--with-source
    (let ((face-spec (get 'mevedel-note-agent 'face-override-spec))
          (buddy-spec (get 'mevedel-note-buddy 'face-override-spec))
          (theme-properties (copy-sequence (symbol-plist 'mevedel-notes-test)))
          (custom-known-themes (copy-sequence custom-known-themes))
          (custom-enabled-themes (copy-sequence custom-enabled-themes)))
      (unwind-protect
          (progn
            (deftheme mevedel-notes-test "Private note palette.")
            (custom-theme-set-faces
             'mevedel-notes-test
             '(default ((t (:foreground "white" :background "black"))))
             '(font-lock-function-name-face ((t (:foreground "red"))))
             '(font-lock-keyword-face ((t (:foreground "green"))))
             '(font-lock-warning-face ((t (:foreground "blue")))))
            (enable-theme 'mevedel-notes-test)
            (mevedel-note-create "Theme colors")
            (dolist (entry '((mevedel-note-you mevedel-note-label-you font-lock-keyword-face "#001900")
                             (mevedel-note-agent mevedel-note-label-agent font-lock-function-name-face "#190000")
                             (mevedel-note-buddy mevedel-note-label-buddy font-lock-warning-face "#000019")))
              (pcase-let ((`(,tint ,label ,parent ,background) entry))
                (should (eq parent (face-attribute label :inherit)))
                (should (eq 'bold (face-attribute label :weight)))
                (should (eq t (face-attribute tint :extend)))
                (should (equal background (face-background tint)))))
            (face-spec-set 'mevedel-note-agent '((t (:background "purple"))))
            (face-spec-set 'mevedel-note-buddy '((t (:inherit default))))
            (custom-theme-set-faces
             'mevedel-notes-test
             '(default ((t (:foreground "black" :background "white"))))
             '(font-lock-keyword-face ((t (:foreground "blue")))))
            (enable-theme 'mevedel-notes-test)
            (should (equal "#e5e5ff" (face-background 'mevedel-note-you)))
            (should (equal "purple" (face-background 'mevedel-note-agent)))
            (should (equal "white" (face-background 'mevedel-note-buddy nil t))))
        (face-spec-set 'mevedel-note-agent face-spec)
        (face-spec-set 'mevedel-note-buddy buddy-spec)
        (disable-theme 'mevedel-notes-test)
        (setplist 'mevedel-notes-test theme-properties))))
  :doc "region note without text labels below the whole line and follows edits"
  (mevedel-notes-test--with-source
    (let ((transient-mark-mode t))
      (set-mark (point))
      (forward-char 8)
      (activate-mark)
      (let ((note (mevedel-note-create)))
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
        (should (= 1 (length (mevedel-notes-test--labels)))))))
  :doc "line notes accept no text and region notes accept text"
  (mevedel-notes-test--with-source
    (mevedel-note-create)
    (goto-char (point-max))
    (let ((transient-mark-mode t))
      (set-mark (point-min))
      (activate-mark)
      (mevedel-note-create "A region"))
    (should (= 2 (length (mevedel-notes-test--labels))))
    (should (string-match-p "A region"
                            (string-join (mevedel-notes-test--labels)))))
  :doc "tabs and overwide controls in narrow labels wrap without stalling or losing text"
  (mevedel-notes-test--with-source
    (let* ((fill-column 4)
           (note (mevedel-note-create "\tvalue\u0080"))
           (label (overlay-get (overlay-get note 'mevedel-label-overlay) 'after-string)))
      (dolist (line (split-string label "\n" t))
        (should (<= (string-width line) fill-column)))
      (should (string-search "value\\200" (replace-regexp-in-string "[ ▍\n]+" "" label)))
      (should (equal "\tvalue\u0080" (overlay-get note 'mevedel-note-text)))))
  :doc "expanded labels repeat the bar at code indentation and fill the full text"
  (mevedel-notes-test--with-source
    (insert "    ")
    (let* ((fill-column 60)
           (text (concat "First paragraph has several words.\n\nSecond " (make-string 500 ?x)))
           (note (mevedel-note-create text)))
      (let* ((label (car (mevedel-notes-test--labels)))
             (lines (split-string label "\n" t)))
        (should (equal "    ▍ Note 1 · You  First paragraph has several words." (car lines)))
        (dolist (line lines)
          (should (string-prefix-p "    ▍ " line))
          (should (<= (string-width line) fill-column)))
        (should (string-search (make-string 500 ?x)
                               (replace-regexp-in-string "[ ▍\n]+" "" label)))
        (should (string-search "First paragraph has several words." label))
        (should (string-search "\n    ▍ \n    ▍ Second" label)))
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt initial &rest _)
                   (should (equal text initial))
                   initial)))
        (call-interactively #'mevedel-note-edit))
      (should-not (overlay-buffer note)))))

(mevedel-deftest mevedel-cycle-instructions-at-point/note-label
  (:quiet t)
  ,test
  (test)
  :doc "cycling overlapping notes highlights the selected label and clears the old face"
  (mevedel-notes-test--with-source
    (let* ((mevedel--highlighted-instruction nil)
           (child (mevedel-note-create "Child"))
           (parent (let ((transient-mark-mode t))
                     (goto-char (point-max))
                     (set-mark (point-min))
                     (activate-mark)
                     (mevedel-note-create "Parent"))))
      (goto-char (overlay-start child))
      (call-interactively #'mevedel-cycle-instructions-at-point)
      (should (eq 'highlight (get-text-property
                              1 'face (overlay-get (overlay-get child 'mevedel-label-overlay)
                                                   'after-string))))
      (call-interactively #'mevedel-cycle-instructions-at-point)
      (should (eq 'mevedel-note-label-you (get-text-property
                           1 'face (overlay-get (overlay-get child 'mevedel-label-overlay)
                                                'after-string))))
      (should (eq 'highlight (get-text-property
                              1 'face (overlay-get (overlay-get parent 'mevedel-label-overlay)
                                                   'after-string))))
      (call-interactively #'mevedel-cycle-instructions-at-point)
      (should (eq 'mevedel-note-label-you (get-text-property
                           1 'face (overlay-get (overlay-get parent 'mevedel-label-overlay)
                                                'after-string))))
      ;; The noted code carries the author's tint; an AI note gets its own.
      (should (eq 'mevedel-note-you (overlay-get child 'face)))
      (overlay-put child 'mevedel-note-author 'agent)
      (mevedel-notes--render child)
      (should (eq 'mevedel-note-agent (overlay-get child 'face)))
      (should (eq 'mevedel-note-label-agent
                  (get-text-property
                   1 'face (overlay-get (overlay-get child 'mevedel-label-overlay)
                                        'after-string))))
      (overlay-put child 'mevedel-note-author 'user)
      (mevedel-notes--render child)
      (let* ((label (overlay-get child 'mevedel-label-overlay))
             (before (overlay-get label 'after-string)))
        (mevedel-note-edit "Child" child)
        (should (eq before (overlay-get label 'after-string)))))))

(mevedel-deftest mevedel-note-persistence
  (:quiet t)
  ,test
  (test)
  :doc "workspace store reload preserves text and rebuilds exactly one label"
  (mevedel-notes-test--with-source
    (let (messages)
      (mevedel-test--with-captured-messages messages
        (mevedel-note-create "Remember tomorrow"))
      (should (equal "" messages)))
    (should (file-exists-p (mevedel-instruction-store-path workspace)))
    (mevedel--clear-instruction-state workspace)
    (should-not (mevedel-notes-test--labels))
    (mevedel-instruction-store-observe workspace)
    (should (= 1 (length (mevedel-notes-test--labels))))
    (should (string-match-p "Remember tomorrow"
                            (car (mevedel-notes-test--labels)))))
  :doc "resuming either workspace session preserves notes created outside it"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Shared by sessions")
    (dolist (name '("a" "b"))
      (let ((session (mevedel-session-create name workspace)))
        (setf (mevedel-session-save-path session) (file-name-concat directory name))
        (make-directory (mevedel-session-save-path session))
        (mevedel--clear-instruction-state workspace)
        (with-temp-buffer
          (setq-local mevedel--workspace workspace)
          (should (mevedel-session-artifacts-load-instructions session)))
        (should (= 1 (length (mevedel-notes-test--labels))))
        (should (string-match-p "Shared by sessions"
                                (car (mevedel-notes-test--labels))))))))

(mevedel-deftest mevedel-note-edit
  (:quiet t)
  ,test
  (test)
  :doc "labels dim only attention states after the header and leave body text normal"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Goal"))
          (mevedel-notes--taken-watches (make-hash-table :test #'equal)))
      (dolist (entry '((unsent . nil) (sent . nil) (ready . "change ready")
                       (stale . "change stale") (working . "working…")
                       (taken . "paused")))
        (overlay-put note 'mevedel-note-state (if (eq (car entry) 'taken) 'taken 'unsent))
        (overlay-put note 'mevedel-note-attention-state (car entry))
        (overlay-put note 'mevedel-note-session-id (and (eq (car entry) 'sent) "sent-note"))
        (mevedel-note-edit "Goal" note)
        (let* ((label (car (mevedel-notes-test--labels)))
               (status (cdr entry)))
          (should (equal (concat "\n▍ Note 1 · You"
                                 (and status (concat " · " status)) "  Goal") label))
          (when status
            (should (eq 'shadow (get-text-property (string-search status label) 'face label))))
          (should (eq 'default (get-text-property (string-search "Goal" label) 'face label)))))
      (puthash (overlay-get note 'mevedel-uuid) t mevedel-notes--taken-watches)
      (mevedel-note-edit "Goal" note)
      (should (string-search "You · Buddy watches  Goal" (car (mevedel-notes-test--labels))))))
  :doc "explicit note target persists to its own workspace from another buffer"
  (mevedel-notes-test--with-source
    (let* ((note (mevedel-note-create "Before"))
           (other-root (file-name-concat directory "other"))
           (other (mevedel-workspace--create
                   :type 'file :id other-root :root other-root :name "other")))
      (make-directory (file-name-concat other-root ".mevedel") t)
      (with-temp-buffer
        (setq-local mevedel--workspace other)
        (mevedel-note-edit "Changed from another workspace" note))
      (mevedel-instruction-store-flush t)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (should (string-match-p "Changed from another workspace"
                              (car (mevedel-notes-test--labels)))))))

(mevedel-deftest mevedel-note-menu
  (:quiet t)
  ,test
  (test)
  :doc "overlapping notes offer stable nonempty descriptions even when collapsed"
  (dolist (display '(collapsed expanded))
    (mevedel-notes-test--with-source
      (let* ((mevedel-note-labels display)
             (child (mevedel-note-create "Child\nSecond line"))
             (transient-mark-mode t)
             (kill-ring nil)
             (kill-ring-yank-pointer nil))
        (goto-char (point-max))
        (set-mark (point-min))
        (activate-mark)
        (mevedel-note-create "Parent")
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
  (mevedel-notes-test--with-source
    (let* ((note (mevedel-note-create "Inline menu"))
           (label (overlay-get note 'mevedel-label-overlay))
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
                               (progn (mevedel-note-menu note) nil)
                             ((quit error) (car condition))))))
        (should (equal original (overlay-get label 'after-string))))
      (should (= 3 calls))
      (should (= position (point)))
      (should (equal "before\nselected code\nafter\n" (buffer-string)))))
  :doc "closing the menu preserves a label rerendered while waiting for input"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Before waiting")))
      (cl-letf (((symbol-function 'read-key)
                 (lambda (&rest _)
                   (mevedel-note-edit "Edited while waiting" note)
                   ?z)))
        (mevedel-note-menu note))
      (let ((labels (string-join (mevedel-notes-test--labels))))
        (should (string-search "Edited while waiting" labels))
        (should-not (string-search "Before waiting" labels))
        (should-not (string-search "Actions:" labels)))))
  :doc "keys for inapplicable actions do nothing without opening another prompt"
  (mevedel-notes-test--with-source
    (let* ((note (mevedel-note-create "No prepared change"))
           (label (overlay-get note 'mevedel-label-overlay))
           (original (overlay-get label 'after-string)))
      (dolist (key '(?a ?m ?r ?b))
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) key)))
          (mevedel-note-menu note))
        (should (equal original (overlay-get label 'after-string)))
        (should (eq 'user (overlay-get note 'mevedel-note-author))))))
  :doc "sent notes offer revert only with applied history and pending changes offer apply and merge"
  (mevedel-notes-test--with-source
    (let* ((note (mevedel-note-create "Stateful menu"))
           (session (mevedel-session--create :session-id "menu-session"))
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           expected)
      (overlay-put note 'mevedel-note-session-id "menu-session")
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-session-control-transfer-register-root-buffer session (current-buffer))
        (unwind-protect
            (cl-letf (((symbol-function 'read-key)
                       (lambda (&rest _)
                         (let ((text (downcase (replace-regexp-in-string
                                               "[][]" "" (overlay-get
                                                           (overlay-get note 'mevedel-label-overlay)
                                                           'after-string)))))
                           (should (equal expected
                                          (cl-remove-if-not
                                           (lambda (key)
                                             (string-search
                                              (cdr (assq key '((?a . "apply") (?m . "merge") (?r . "revert"))))
                                              text))
                                           '(?r ?a ?m))))
                           ?w))))
              (mevedel-note-menu note)
              (setf (mevedel-session-file-snapshots session) '((1)))
              (mevedel-note-menu note)
              (setf (mevedel-session-file-snapshots session) '((1 ("source.el" :backup-name "after"))))
              (setq expected '(?r))
              (mevedel-note-menu note)
              (setf (mevedel-session-file-snapshots session) nil)
              (overlay-put note 'mevedel-note-applied-changes '((:session-id "other" :turn 1)))
              (mevedel-note-menu note)
              (overlay-put note 'mevedel-note-applied-changes nil)
              (overlay-put note 'mevedel-note-prepared-change '(:id "pending"))
              (setq expected '(?a ?m))
              (mevedel-note-menu note))
          (mevedel-session-control-transfer-unregister-root-buffer session (current-buffer))))))
  :doc "modified-key menu edits note text while plain letters insert source text"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Before")
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
    (should (string-match-p "After" (car (mevedel-notes-test--labels))))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should (string-match-p "After" (car (mevedel-notes-test--labels)))))
  :doc "copy text copies the full authored text without expanding or changing code"
  (mevedel-notes-test--with-source
    (let* ((mevedel-note-labels 'collapsed)
           (text (concat "First\n\nSecond " (make-string 500 ?x)))
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (note (mevedel-note-create text))
           messages)
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
        (mevedel-test--with-captured-messages messages
          (mevedel-note-menu note)))
      (should (equal text (current-kill 0)))
      (should (string-match-p "copied" messages))
      (should (string-search "First Second" (car (mevedel-notes-test--labels))))
      (should (equal "before\nselected code\nafter\n" (buffer-string)))))
  :doc "tags can be edited from the inline menu without editing source"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Tag me")))
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?g))
                ((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) '("review" "urgent"))))
        (mevedel-note-menu note))
      (should (equal '(review urgent) (overlay-get note 'mevedel-note-tags)))
      (should (equal "before\nselected code\nafter\n" (buffer-string)))
      (should-not (cl-some (lambda (overlay) (overlay-get overlay 'before-string))
                          (overlays-in (point-min) (point-max))))))
  :doc "menu deletion removes the note and label without changing source"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Delete me")
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?k)))
      (call-interactively (key-binding (kbd "M-m"))))
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (should-not (mevedel-notes-test--labels))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should-not (mevedel-notes-test--labels)))
  :doc "done is an explicit modified-key menu action, never a plain source key"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Close from menu")
    (should (eq #'self-insert-command (key-binding "x")))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?x)))
      (call-interactively (key-binding (kbd "M-m"))))
    (should-not (mevedel-notes-test--labels))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should (eq 'done (plist-get (plist-get (car (mevedel-notes-closed workspace))
                                           :instruction)
                                :closed-reason)))))

(mevedel-deftest mevedel-note-entry-hint
  ()
  ,test
  (test)
  :doc "entering a note announces its identity and next actions only once"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Review the boundary")
    (let (messages)
      (mevedel-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (string-match-p "Note 1 · You.*Review the boundary.*M-m.*edit.*delete"
                              messages))
      (mevedel-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (equal "" messages))
      (goto-char (point-min))
      (run-hooks 'post-command-hook)
      (forward-line 1)
      (mevedel-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (string-match-p "Note 1" messages)))))

(mevedel-deftest mevedel-note-entry-hint-search
  ()
  ,test
  (test)
  :doc "note entry does not replace an isearch prompt or consume the later hint"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Search target")
    (let ((isearch-mode t) messages)
      (mevedel-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (equal "" messages)))
    (let (messages)
      (mevedel-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (string-match-p "Note 1.*Search target" messages))))
  :doc "keyboard macros do not emit note-entry hints"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Macro target")
    (let ((executing-kbd-macro t) messages)
      (mevedel-test--with-captured-messages messages
        (run-hooks 'post-command-hook))
      (should (equal "" messages)))))

(mevedel-deftest mevedel-note-prepared-source-edits
  (:quiet t)
  ,test
  (test)
  :doc "inserting a newline just after noted code preserves its ready change"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Waiting change")))
      (mevedel-note-hold-change (list note) '(:test t))
      (goto-char (overlay-end note))
      (insert "\n")
      (should (equal "selected code" (buffer-substring-no-properties
                                      (overlay-start note) (overlay-end note))))
      (should (eq 'ready (mevedel-notes-attention-state note)))
      (goto-char (1+ (overlay-start note)))
      (insert "X")
      (should (eq 'stale (mevedel-notes-attention-state note)))))
  :doc "deleting the final noted character still makes its change stale"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Waiting change")))
      (mevedel-note-hold-change (list note) '(:test t))
      (delete-region (1- (overlay-end note)) (overlay-end note))
      (should (eq 'stale (mevedel-notes-attention-state note))))))

(mevedel-deftest mevedel-note-source-undo
  (:quiet t)
  ,test
  (test)
  :doc "finishing a full code deletion never closes a code-only note"
  (mevedel-notes-test--with-source
    (mevedel-note-create)
    (run-hooks 'pre-command-hook)
    (erase-buffer)
    (run-hooks 'post-command-hook)
    (should (= 1 (length (mevedel-notes-test--labels))))
    (should-not (mevedel-notes-closed workspace t))
    (save-buffer)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should (= 1 (length (mevedel-notes-test--labels)))))
  :doc "source undo restores note geometry after deleting its entire marked code"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Keep the anchor")
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (run-hooks 'pre-command-hook)
    (delete-region (line-beginning-position) (line-beginning-position 2))
    (run-hooks 'post-command-hook)
    (undo-boundary)
    (should (= 1 (length (mevedel-notes-test--labels))))
    (undo-only 1)
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (should (string-match-p "Keep the anchor" (car (mevedel-notes-test--labels)))))
  :doc "source undo restores geometry but never rewinds subsequently edited note text"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Original")))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region (point) (1+ (point)))
      (undo-boundary)
      (mevedel-note-edit "Newer text" note)
      (undo-only 1)
      (should (equal "before\nselected code\nafter\n" (buffer-string)))
      (should (string-match-p "Newer text" (car (mevedel-notes-test--labels))))))
  :doc "source undo does not resurrect an explicitly deleted note"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Delete me")))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region (point) (1+ (point)))
      (undo-boundary)
      (mevedel-note-delete note)
      (undo-only 1)
      (should-not (mevedel-notes-test--labels)))))

(mevedel-deftest mevedel-note-store-format
  (:quiet t)
  ,test
  (test)
  :doc "format 2 notes are refused by a format 1 client without clearing data"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Keep this")
    (let* ((path (mevedel-instruction-store-path workspace))
           (bytes (with-temp-buffer (insert-file-contents path) (buffer-string))))
      (should (= 2 (plist-get (read bytes) :format)))
      ;; Simulate the older reader's supported-format constant, not its codec.
      (let ((mevedel-instruction-store-format-version 1))
        (should-error (mevedel-instruction-store-sync workspace)
                      :type 'mevedel-instruction-store-unreadable))
      (should (equal bytes (with-temp-buffer
                             (insert-file-contents path) (buffer-string))))
      (should (string-match-p "Keep this" (car (mevedel-notes-test--labels)))))))

(mevedel-deftest mevedel-note-done
  (:quiet t)
  ,test
  (test)
  :doc "done removes the label and restores the same note after a store reload"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Finish this")
    (mevedel-note-done)
    (should-not (mevedel-notes-test--labels))
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should-not (mevedel-notes-test--labels))
    (mevedel-note-restore 1 workspace)
    (should (string-match-p "▍ Note 1 · You  Finish this"
                            (car (mevedel-notes-test--labels)))))
  :doc "closing a parent note refreshes the surviving nested presentation"
  (mevedel-notes-test--with-source
    (let* ((transient-mark-mode t)
           (child (progn (goto-char 8) (set-mark 16) (activate-mark)
                         (mevedel-note-create "Child")))
           (before (string-join (mevedel-notes-test--labels))))
      (goto-char (point-max))
      (set-mark (point-min))
      (activate-mark)
      (let ((note (mevedel-note-create "Parent")))
        (mevedel-note-done note))
      (should (overlay-buffer child))
      (should (equal-including-properties before (string-join (mevedel-notes-test--labels)))))))

(mevedel-deftest mevedel-note-restore
  (:quiet t)
  ,test
  (test)
  :doc "a closed blank-line note reanchors after an external source change"
  (mevedel-notes-test--with-source
    (setq workspace (mevedel-workspace-get-or-create
                     'file directory directory "notes"))
    (setq-local mevedel--workspace workspace)
    (delete-region (line-beginning-position) (line-end-position))
    (save-buffer)
    (let ((note (mevedel-note-create "Keep the blank line")))
      (should (= (overlay-start note) (overlay-end note)))
      (mevedel-note-done note))
    (mevedel-instruction-store-flush t)
    (should-not (mevedel-notes-test--labels))
    (with-temp-file file (insert "prepended\nbefore\n\nafter\n"))
    (revert-buffer t t)
    (let ((note (mevedel-note-restore 1 workspace)))
      (should (= 3 (line-number-at-pos (overlay-start note))))
      (should (= (overlay-start note) (overlay-end note))))
    (should (= 1 (length (mevedel-notes-test--labels))))
    (should (string-match-p "Keep the blank line"
                            (car (mevedel-notes-test--labels)))))
  :doc "changed code falls back to the old line, not an obsolete character offset"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Keep my place")
    (mevedel-note-delete)
    (mevedel-instruction-store-flush t)
    (erase-buffer)
    (insert "a much longer replacement for the first line\nnew code\nlast\n")
    (save-buffer)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (let ((note (mevedel-note-restore 1 workspace)))
      (should (= 2 (line-number-at-pos (overlay-start note))))
      (should (equal "new code" (buffer-substring-no-properties
                                 (overlay-start note) (overlay-end note)))))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should (= 1 (length (mevedel-notes-test--labels)))))
  :doc "display snapshots cannot mutate the closed note restored by the command"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Original")
    (mevedel-note-delete)
    (let* ((snapshot (car (mevedel-notes-closed workspace)))
           (props (plist-get (plist-get snapshot :instruction) :properties)))
      (aset (plist-get props 'mevedel-note-text) 0 ?X))
    (mevedel-note-restore 1 workspace)
    (should (string-match-p "Original" (car (mevedel-notes-test--labels)))))
  :doc "restore outside a narrowing preserves point and places the label at its code"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Outside restriction")
    (mevedel-note-delete)
    (narrow-to-region 1 7)
    (goto-char 3)
    (let ((note (mevedel-note-restore 1 workspace)))
      (should (= 3 (point)))
      (should (= 7 (point-max)))
      (widen)
      (should (= 8 (overlay-start note)))
      (let ((label (cl-find-if
                    (lambda (overlay) (overlay-get overlay 'after-string))
                    (overlays-in (point-min) (point-max)))))
        (should (= 21 (overlay-end label))))))
  :doc "restore loads the workspace store on first use without prior activation"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Cold restore")
    (mevedel-note-delete)
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-note-restore 1 workspace)
    (should (string-match-p "Cold restore" (car (mevedel-notes-test--labels)))))
  :doc "export and reload keep open and closed notes together for an unopened file"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Closed")
    (mevedel-note-done)
    (forward-line 1)
    (mevedel-note-create "Still open")
    (mevedel-instruction-store-flush t)
    (kill-buffer source)
    (with-temp-buffer
      (setq-local mevedel--workspace workspace)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (let ((path (file-name-concat directory "export.el")))
        (mevedel-save-instructions path)
        ;; Inspect the public export, not an in-memory codec helper.
        (let* ((export (with-temp-buffer
                         (insert-file-contents path) (read (current-buffer))))
               (files (plist-get export :files)))
          (should (equal '("source.el") (mapcar #'car files)))
          (should (= 2 (length (plist-get (cdar files) :instructions))))
          (should (stringp (plist-get (cdar files) :content-hash))))
        (mevedel-load-instructions path)
        (mevedel-note-restore 1 workspace)))
    (setq source (find-buffer-visiting file))
    (with-current-buffer source
      (should (= 2 (length (mevedel-notes-test--labels)))))))

(mevedel-deftest mevedel-note-close
  (:quiet t)
  ,test
  (test)
  :doc "done, deleted and dismissed share one persistent closed list with reasons"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Done")
    (mevedel-note-done)
    (mevedel-note-create "Deleted")
    (mevedel-note-delete)
    (mevedel-note-close (mevedel-note-create "Dismissed") 'dismissed)
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should-not (mevedel-notes-test--labels))
    (should (equal '(deleted dismissed done)
                   (sort (mapcar (lambda (entry)
                                   (plist-get (plist-get entry :instruction)
                                              :closed-reason))
                                 (mevedel-notes-closed workspace))
                         #'string-lessp)))
    (dolist (id '(1 2 3)) (mevedel-note-restore id workspace))
    (should (= 3 (length (mevedel-notes-test--labels)))))
  :doc "custom count limit keeps the newest closed notes across reload"
  (mevedel-notes-test--with-source
    (let ((mevedel-notes-closed-max-count 2))
      (dolist (text '("First" "Second" "Third"))
        (mevedel-note-create text)
        (mevedel-note-done))
      (mevedel-instruction-store-flush t)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (should (= 2 (length (mevedel-notes-closed workspace))))
      (should-error (mevedel-note-restore 1 workspace) :type 'user-error)
      (mevedel-note-restore 2 workspace)
      (mevedel-note-restore 3 workspace)
      (should (string-match-p "Second" (string-join (mevedel-notes-test--labels))))
      (should (string-match-p "Third" (string-join (mevedel-notes-test--labels))))))
  :doc "150 dismissed remarks leave the newest 100 done or deleted notes restorable"
  (mevedel-notes-test--with-source
    (dotimes (i 101)
      (let ((note (mevedel-note-create (format "Closed %s" i) :defer-write t)))
        (if (cl-evenp i) (mevedel-note-done note) (mevedel-note-delete note))))
    (dotimes (i 150)
      (let ((note (mevedel-note-create (format "Remark %s" i) :defer-write t)))
        (overlay-put note 'mevedel-note-author 'buddy)
        (mevedel-buddy-dismiss-note note)))
    (let ((note (mevedel-note-create "Agent note" :defer-write t)))
      (overlay-put note 'mevedel-note-author 'agent)
      (mevedel-note-dismiss note))
    (mevedel-instruction-store-sync workspace)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should (= 251 (length (mevedel-notes-closed workspace))))
    (should-error (mevedel-note-restore 1 workspace) :type 'user-error)
    (dotimes (i 100)
      (should (overlay-buffer (mevedel-note-restore (+ i 2) workspace))))
    (should (= 100 (length (mevedel-notes-test--labels)))))
  :doc "custom age limit expires at the boundary and cannot be bypassed by restore"
  (mevedel-notes-test--with-source
    (let ((mevedel-notes-closed-max-count nil)
          (mevedel-notes-closed-max-age-days 1)
          (clock (float-time))
          (real-time (symbol-function 'float-time)))
      ;; Only the wall-clock boundary is replaced; store operations stay real.
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time)
                   (if time (funcall real-time time) clock))))
        (mevedel-note-create "Old")
        (mevedel-note-delete)
        (mevedel-instruction-store-flush t)
        (setq clock (+ clock 86399))
        (should (= 1 (length (mevedel-notes-closed workspace))))
        (setq clock (1+ clock))
        (should-error (mevedel-note-restore 1 workspace) :type 'user-error)
        (mevedel-instruction-store-flush t)
        (mevedel--clear-instruction-state workspace)
        (mevedel-instruction-store-observe workspace)
        (let ((mevedel-notes-closed-max-age-days nil))
          (should-not (mevedel-notes-closed workspace))
          (should-error (mevedel-note-restore 1 workspace) :type 'user-error)))))
  :doc "dismissed Buddy and agent notes ignore zero count but still expire at 30 days"
  (mevedel-notes-test--with-source
    (let ((mevedel-notes-closed-max-count 0)
          (clock (float-time))
          (real-time (symbol-function 'float-time)))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time)
                   (if time (funcall real-time time) clock))))
        (let ((buddy (mevedel-note-create "Buddy" :defer-write t))
              (agent (mevedel-note-create "Agent" :defer-write t)))
          (overlay-put buddy 'mevedel-note-author 'buddy)
          (overlay-put agent 'mevedel-note-author 'agent)
          (mevedel-buddy-dismiss-note buddy)
          (mevedel-note-dismiss agent))
        (mevedel-instruction-store-sync workspace)
        (mevedel--clear-instruction-state workspace)
        (mevedel-instruction-store-observe workspace)
        (setq clock (+ clock (* 30 86400) -1))
        (should (= 2 (length (mevedel-notes-closed workspace))))
        (setq clock (1+ clock))
        (dolist (id '(1 2))
          (should-error (mevedel-note-restore id workspace) :type 'user-error))
        (mevedel-instruction-store-sync workspace)
        (mevedel--clear-instruction-state workspace)
        (mevedel-instruction-store-observe workspace)
        (let ((mevedel-notes-closed-max-age-days nil))
          (should-not (mevedel-notes-closed workspace))))))
  :doc "a closed import without a timestamp cannot block later store writes"
  (mevedel-notes-test--with-source
    (dolist (text '("Undated" "Recent"))
      (mevedel-note-create text)
      (mevedel-note-done))
    (let ((export (file-name-concat directory "notes.el")))
      (mevedel-save-instructions export)
      (let* ((data (with-temp-buffer
                     (insert-file-contents export)
                     (read (current-buffer))))
             (info (cdar (plist-get data :files))))
        (setf (plist-get info :instructions)
              (mapcar (lambda (instruction)
                        (when (= 1 (plist-get (plist-get instruction :properties) 'mevedel-id))
                          (cl-remf instruction :closed-at))
                        instruction)
                      (plist-get info :instructions)))
        (write-region (prin1-to-string data) nil export nil 'silent))
      (let ((mevedel-notes-closed-max-count nil)
            (mevedel-notes-closed-max-age-days nil))
        (mevedel-load-instructions export)
        (mevedel-instruction-store-sync workspace)
        (should (= 2 (length (mevedel-notes-closed workspace))))))
    ;; An undated record is oldest, so ordinary age expiry can remove it.
    (mevedel-instruction-store-sync workspace)
    (should (= 1 (length (mevedel-notes-closed workspace))))))

(mevedel-deftest mevedel-note-source-missing
  (:quiet t)
  ,test
  (test)
  :doc "a missing source retains an open note and reattaches it when the file returns"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Wait for my source")
    (delete-file file)
    (run-hooks 'post-command-hook)
    (should-not (mevedel-notes-closed workspace t))
    (mevedel-instruction-store-flush t)
    (kill-buffer source)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (let ((export (file-name-concat directory "export.el")))
      (with-temp-buffer
        (setq-local mevedel--workspace workspace)
        (mevedel-save-instructions export))
      (let* ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer))))
             (instruction (car (plist-get (cdar (plist-get data :files)) :instructions))))
        (should (= 1 (length (plist-get data :files))))
        (should (plist-get (cdar (plist-get data :files)) :source-missing))
        (should (eq 'unsent (plist-get (plist-get instruction :properties)
                                      'mevedel-note-state)))))
    (write-region "before\nselected code\nafter\n" nil file nil 'silent)
    (setq source (find-file-noselect file))
    (with-current-buffer source
      (setq-local mevedel--workspace workspace)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (should (string-match-p "Wait for my source"
                              (car (mevedel-notes-test--labels)))))
    (should-not (mevedel-notes-closed workspace t)))
  :doc "notes in an open missing-file buffer stay live and serialize once after save"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Old")
    (delete-file file)
    (run-hooks 'post-command-hook)
    (should (= 1 (length (mevedel-notes-test--labels))))
    (goto-char (point-min))
    (mevedel-note-create "New")
    (run-hooks 'post-command-hook)
    (should (= 2 (length (mevedel-notes-test--labels))))
    (let ((export (file-name-concat directory "export.el")))
      (mevedel-save-instructions export)
      (let* ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer))))
             (files (plist-get data :files)))
        (should (= 1 (length files)))
        (should (plist-get (cdar files) :source-missing))
        (should (= 2 (length (plist-get (cdar files) :instructions)))))
      (save-buffer)
      (mevedel-save-instructions export)
      (let* ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer))))
             (files (plist-get data :files)))
        (should (= 1 (length files)))
        (should-not (plist-get (cdar files) :source-missing))
        (should (equal '("New" "Old")
                       (sort (mapcar (lambda (instruction)
                                       (plist-get (plist-get instruction :properties)
                                                  'mevedel-note-text))
                                     (plist-get (cdar files) :instructions))
                             #'string-lessp)))))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should (= 2 (length (mevedel-notes-test--labels)))))
  :doc "the existing local source observation clears missing state when a file returns"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Still here")
    (delete-file file)
    (run-hooks 'post-command-hook)
    (write-region "before\nselected code\nafter\n" nil file nil 'silent)
    (run-hooks 'post-command-hook)
    (let ((export (file-name-concat directory "export.el")))
      (mevedel-save-instructions export)
      (let ((data (with-temp-buffer (insert-file-contents export) (read (current-buffer)))))
        (should-not (plist-get (cdar (plist-get data :files)) :source-missing))))
    (should (= 1 (length (mevedel-notes-test--labels))))))

(mevedel-deftest mevedel-delete-instructions/recently-closed
  (:quiet t)
  ,test
  (test)
  :doc "legacy deletion at point retains a live note in recently closed"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Deleted by legacy command")
    (mevedel-delete-instructions)
    (should-not (mevedel-notes-test--labels))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (mevedel-note-restore 1 workspace)
    (should (string-match-p "Deleted by legacy command"
                            (car (mevedel-notes-test--labels))))))

(mevedel-deftest mevedel-delete-instruction/forget-note
  (:quiet t)
  ,test
  (test)
  :doc "automatic retraction forgets a note without closing or recycling its number"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Retracted remark")))
      (mevedel--delete-instruction note nil t)
      (should-not (overlay-buffer note)))
    (should-not (mevedel-notes-test--labels))
    (should-not (mevedel-notes-closed workspace t))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should-not (mevedel-notes-test--labels))
    (should-not (mevedel-notes-closed workspace t))
    (mevedel-note-create "Next")
    (should (string-match-p "Note 2" (car (mevedel-notes-test--labels))))))

(mevedel-deftest mevedel-notes-closed
  (:quiet t)
  ,test
  (test)
  :doc "loaded-only queries skip cold observation and still return isolated snapshots"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Original")
    (mevedel-note-delete)
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (should-not (mevedel-notes-closed workspace t))
    (should (= 1 (length (mevedel-notes-closed workspace))))
    (let* ((snapshot (car (mevedel-notes-closed workspace t)))
           (props (plist-get (plist-get snapshot :instruction) :properties)))
      (aset (plist-get props 'mevedel-note-text) 0 ?X))
    (mevedel-note-restore 1 workspace)
    (should (string-match-p "Original" (car (mevedel-notes-test--labels))))))

(mevedel-deftest mevedel-delete-all-instructions/recently-closed
  (:quiet t)
  ,test
  (test)
  :doc "legacy bulk deletion closes notes and retains their history"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Keep my history")
    (goto-char (point-min))
    (let ((note (mevedel-note-create "Another note")))
      (should (= 2 (mevedel-delete-all-instructions)))
      (should-not (overlay-buffer note)))
    (should-not (mevedel-notes-test--labels))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (mevedel-note-restore 1 workspace)
    (should (string-match-p "Keep my history" (car (mevedel-notes-test--labels))))))

(defun mevedel-notes-test--dirty-p (workspace)
  "Return non-nil when WORKSPACE has an unflushed store touch."
  (and (memq workspace mevedel-instruction-store--dirty) t))

(mevedel-deftest mevedel-note-mark-sent (:quiet t)
  ,test
  (test)
  :doc "marks notes sent, keeps an existing session id, fills an absent one and touches once"
  (mevedel-notes-test--with-source
    (let ((first (mevedel-note-create "First"))
          (second (progn (goto-char (point-min)) (mevedel-note-create "Second"))))
      (mevedel-instruction-store-flush t)
      (setq mevedel-instruction-store--dirty nil)
      (overlay-put first 'mevedel-note-session-id "existing")
      (mevedel-note-mark-sent (list first second) "new")
      (should (equal "existing" (overlay-get first 'mevedel-note-session-id)))
      (should (equal "new" (overlay-get second 'mevedel-note-session-id)))
      (should (eq 'sent (overlay-get first 'mevedel-note-state)))
      (should (eq 'sent (overlay-get second 'mevedel-note-state)))
      (should (mevedel-notes-test--dirty-p workspace)))))

(mevedel-deftest mevedel-note-hold-change (:quiet t)
  ,test
  (test)
  :doc "holds the record as ready on every note, renders it and touches without flushing"
  (mevedel-notes-test--with-source
    (let ((first (mevedel-note-create "First"))
          (second (progn (goto-char (point-min)) (mevedel-note-create "Second")))
          (record '(:id "r" :session-id "s" :turn 1))
          flushes)
      (mevedel-instruction-store-flush t)
      (setq mevedel-instruction-store--dirty nil)
      (cl-letf (((symbol-function 'mevedel-instruction-store-flush)
                 (lambda (&rest _) (push t flushes))))
        (mevedel-note-hold-change (list first second) record))
      (should-not flushes)
      (dolist (note (list first second))
        (should (eq record (overlay-get note 'mevedel-note-prepared-change)))
        (should (eq 'ready (mevedel-notes-attention-state note)))
        (should (string-search "change ready"
                               (overlay-get (overlay-get note 'mevedel-label-overlay)
                                            'after-string))))
      (should (mevedel-notes-test--dirty-p workspace)))))

(mevedel-deftest mevedel-note-record-applied (:quiet t)
  ,test
  (test)
  :doc "records the applied pair on every batch sibling, with files only for a foreign session"
  (mevedel-notes-test--with-source
    (let* ((own (mevedel-note-create "Own"))
           (foreign (progn (goto-char (point-min)) (mevedel-note-create "Foreign")))
           (unrelated (progn (goto-char (point-max)) (forward-line -1)
                             (mevedel-note-create "Unrelated")))
           (record '(:id "r" :session-id "s" :turn 2))
           (files '(("a.el" :backup-name "b"))))
      (mevedel-note-mark-sent (list own) "s")
      (mevedel-note-mark-sent (list foreign) "other")
      (mevedel-note-hold-change (list own foreign) record)
      (mevedel-note-hold-change (list unrelated) '(:id "x" :session-id "s" :turn 1))
      (setq mevedel-instruction-store--dirty nil)
      (mevedel-note-record-applied record files workspace)
      (should (equal '((:session-id "s" :turn 2))
                     (overlay-get own 'mevedel-note-applied-changes)))
      (should (equal `((:session-id "s" :turn 2 :files ,files))
                     (overlay-get foreign 'mevedel-note-applied-changes)))
      (dolist (note (list own foreign))
        (should-not (overlay-get note 'mevedel-note-prepared-change))
        (should-not (overlay-get note 'mevedel-note-attention-state)))
      (should (overlay-get unrelated 'mevedel-note-prepared-change))
      (should-not (overlay-get unrelated 'mevedel-note-applied-changes))
      (should (mevedel-notes-test--dirty-p workspace)))))

(mevedel-deftest mevedel-note-mark-stale (:quiet t)
  ,test
  (test)
  :doc "marks the prepared change stale and renders it"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Waiting")))
      (mevedel-note-hold-change (list note) '(:id "r"))
      (mevedel-note-mark-stale note)
      (should (eq 'stale (mevedel-notes-attention-state note)))
      (should (string-search "change stale"
                             (overlay-get (overlay-get note 'mevedel-label-overlay)
                                          'after-string))))))

(mevedel-deftest mevedel-note-set-settings (:quiet t)
  ,test
  (test)
  :doc "merges settings into the note's own and touches the store"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Configure")))
      (overlay-put note 'mevedel-note-settings '(:preset a :effort low))
      (setq mevedel-instruction-store--dirty nil)
      (mevedel-note-set-settings note '(:effort high :provider p))
      (should (equal '(high p a)
                     (mapcar (lambda (key)
                               (plist-get (overlay-get note 'mevedel-note-settings) key))
                             '(:effort :provider :preset))))
      (should (mevedel-notes-test--dirty-p workspace)))))

(mevedel-deftest mevedel-note-keep (:quiet t)
  ,test
  (test)
  :doc "keeps a Buddy remark as the user's note and touches the store"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-make source (point) (line-end-position) :text "Hm" :author 'buddy)))
      (setq mevedel-instruction-store--dirty nil)
      (mevedel-note-keep note)
      (should (eq 'user (overlay-get note 'mevedel-note-author)))
      (should (overlay-get note 'mevedel-note-kept-from-buddy))
      (should (mevedel-notes-test--dirty-p workspace))))
  :doc "refuses a note that is not a Buddy remark"
  (mevedel-notes-test--with-source
    (let ((note (mevedel-note-create "Mine")))
      (should-error (mevedel-note-keep note) :type 'user-error)
      (should (eq 'user (overlay-get note 'mevedel-note-author)))
      (should-not (overlay-get note 'mevedel-note-kept-from-buddy)))))

(mevedel-deftest mevedel-note-make (:quiet t)
  ,test
  (test)
  :doc "creates an unsent note in a buffer without a file and neither writes nor touches the store"
  (mevedel-notes-test--with-source
    (let ((scratch (generate-new-buffer " *note-make*")))
      (unwind-protect
          (with-current-buffer scratch
            (setq-local mevedel--workspace workspace)
            (insert "remark\n")
            (let ((note (mevedel-note-make scratch 1 7 :text "Unsaved" :author 'buddy
                                           :span 'line)))
              (should-not buffer-file-name)
              (should (eq 'buddy (overlay-get note 'mevedel-note-author)))
              (should (eq 'unsent (overlay-get note 'mevedel-note-state)))
              (should (equal "Unsaved" (overlay-get note 'mevedel-note-text)))
              (should (overlay-get note 'mevedel-label-overlay))))
        (set-buffer-modified-p nil)
        (kill-buffer scratch))
      (should-not mevedel-instruction-store--dirty)
      (should-not (file-exists-p (mevedel-instruction-store-path workspace)))))
  :doc "leaves an unrelated active region, point and the selected window untouched"
  (mevedel-notes-test--with-source
    (let ((transient-mark-mode t) (window (selected-window)))
      (set-window-buffer window source)
      (goto-char (point-max))
      (push-mark (point-min) t t)
      (let ((mark (mark)) (point (point)))
        (mevedel-note-make source 8 21 :text "Agent" :author 'agent)
        (should (region-active-p))
        (should (= mark (mark)))
        (should (= point (point)))
        (should (eq window (selected-window)))))))

(mevedel-deftest mevedel-notes-rewind-live (:quiet t)
  ,test
  (test)
  :doc "live, stashed and closed notes get the same Rewind result"
  (mevedel-notes-test--with-source
    (let* ((live (mevedel-note-create "Live"))
           (keys '(mevedel-note-state mevedel-note-prepared-change
                   mevedel-note-attention-state mevedel-note-applied-changes))
           (build (lambda ()
                    (list :open-state 'sent
                          :properties
                          (list 'mevedel-note-state 'sent
                                'mevedel-note-session-id "s"
                                'mevedel-note-prepared-change '(:session-id "s" :turn 2)
                                'mevedel-note-attention-state 'ready
                                'mevedel-note-applied-changes
                                '((:session-id "s" :turn 1) (:session-id "s" :turn 3)
                                  (:session-id "o" :turn 3))))))
           (stashed (funcall build))
           (closed (funcall build))
           (state (mevedel--instruction-state (mevedel--instruction-workspace-key workspace))))
      (mevedel-note-hold-change (list live) '(:session-id "s" :turn 2))
      (mevedel-note-mark-sent (list live) "s")
      (overlay-put live 'mevedel-note-applied-changes
                   '((:session-id "s" :turn 1) (:session-id "s" :turn 3)
                     (:session-id "o" :turn 3)))
      (push (list "stashed.el" :instructions (list stashed)) (plist-get state :instructions))
      ;; Seed the key in place; setf on an absent key would rebind STATE.
      (plist-put state :closed-notes nil)
      (push (list :file "closed.el" :instruction closed) (plist-get state :closed-notes))
      (cl-flet ((summary (plist) (mapcar (lambda (key) (plist-get plist key)) keys)))
        (mevedel-notes-rewind-live workspace "s" 1)
        (let ((expected '(sent nil nil ((:session-id "s" :turn 1) (:session-id "o" :turn 3)))))
          (should (equal expected (summary (plist-get stashed :properties))))
          (should (equal expected (summary (plist-get closed :properties))))
          (should (equal expected (mapcar (lambda (key) (overlay-get live key)) keys))))
        ;; Turn 0 returns sent notes, open or closed, to unsent.
        (mevedel-notes-rewind-live workspace "s" 0)
        (should (eq 'unsent (overlay-get live 'mevedel-note-state)))
        (should (eq 'unsent (plist-get (plist-get stashed :properties) 'mevedel-note-state)))
        (should (eq 'unsent (plist-get closed :open-state)))
        (should (equal '((:session-id "o" :turn 3))
                       (overlay-get live 'mevedel-note-applied-changes)))))))

(mevedel-deftest mevedel-notes--label-string
  (:quiet t)
  ,test
  (test)
  :doc "collapsed labels fit indentation and window, keep identity and never change authored text"
  (mevedel-notes-test--with-source
    (insert "    ")
    (let* ((mevedel-note-labels 'collapsed)
           (fill-column 25)
           (text (concat "First\nSecond " (make-string 100 ?x)))
           (note (mevedel-note-create text)))
      (dolist (width '(38 120))
        (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) width)))
          (mevedel-notes--refit-labels (selected-window))
          (let* ((label (overlay-get (overlay-get note 'mevedel-label-overlay) 'after-string))
                 (lines (split-string label "\n" t)))
            (should (= 1 (length lines)))
            (should (string-prefix-p "    ▍ Note 1 · You  First Second " (car lines)))
            (should (string-suffix-p "…" label))
            (should (= (string-width (car lines)) (min 86 width))))))
      (should (equal text (overlay-get note 'mevedel-note-text))))))

(mevedel-deftest mevedel-notes--at-point
  (:quiet t)
  ,test
  (test)
  :doc "nested source tints prioritize the inner note and commands choose either identity"
  (mevedel-notes-test--with-source
    (let* ((child (mevedel-note-create "Child"))
           (parent (mevedel-note-make source (point-min) (overlay-end child)
                                           :text "Parent" :author 'agent)))
      (should (> (overlay-get child 'priority) (overlay-get parent 'priority)))
      (should (eq 'mevedel-note-you (get-char-property (overlay-start child) 'face)))
      (should (= (overlay-end (overlay-get child 'mevedel-label-overlay))
                 (overlay-end (overlay-get parent 'mevedel-label-overlay))))
      (dolist (note (list parent child))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (should (= 2 (length choices)))
                     (cl-find-if (lambda (choice)
                                   (string-prefix-p
                                    (format "Note %s ·" (overlay-get note 'mevedel-id)) choice))
                                 choices))))
          (should (eq note (mevedel-notes--at-point)))))
      (let (messages)
        (mevedel-test--with-captured-messages messages (mevedel-notes--entry-hint))
        (should (string-match-p "Note 1 · You.*Child.*Note 2 · AI.*Parent.*choose note" messages))))))

(mevedel-deftest mevedel-notes--stacked-newline-face
  (:quiet t :doc "only the first after-string tints the source newline in either priority order")
  (with-temp-buffer
    (insert "outer\ninner\n")
    (let* ((outer (mevedel-note-make (current-buffer) 1 (point-max)
                                    :span 'line :text "Outer"))
           (inner (mevedel-note-make (current-buffer) 7 (point-max)
                                    :span 'line :author 'buddy :text "Inner"))
           (outer-label (overlay-get outer 'mevedel-label-overlay))
           (inner-label (overlay-get inner 'mevedel-label-overlay)))
      (should (= (overlay-end outer-label) (overlay-end inner-label)))
      (should (> (overlay-get inner-label 'priority) (overlay-get outer-label 'priority)))
      (should (eq 'mevedel-note-buddy
                  (get-text-property 0 'face (overlay-get inner-label 'after-string))))
      (should (eq 'default
                  (get-text-property 0 'face (overlay-get outer-label 'after-string))))
      (overlay-put outer 'priority (1+ (overlay-get inner 'priority)))
      (mevedel-notes--render outer)
      (should (> (overlay-get outer-label 'priority) (overlay-get inner-label 'priority)))
      (should (eq 'mevedel-note-you
                  (get-text-property 0 'face (overlay-get outer-label 'after-string))))
      (should (eq 'default
                  (get-text-property 0 'face (overlay-get inner-label 'after-string))))
      (mevedel--delete-instruction-overlay outer)
      (should (eq 'mevedel-note-buddy
                  (get-text-property 0 'face (overlay-get inner-label 'after-string)))))))

(provide 'test-mevedel-notes)
;;; test-mevedel-notes.el ends here
