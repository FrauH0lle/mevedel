;;; test-mevedel-notes.el -- Source note command tests -*- lexical-binding: t -*-

;;; Commentary:

;; Drive note commands in real source files and observe their presentations.

;;; Code:

(require 'mevedel)
(require 'mevedel-persistence)
(require 'mevedel-session-artifacts)
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

(mevedel-deftest mevedel-note-create
  (:quiet t)
  ,test
  (test)
  :doc "current-line note shows author, optional text, unsent state and actions"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Check this")
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (let ((label (string-join (mevedel-notes-test--labels))))
      (should (string-match-p "Note 1.*you.*Check this.*not sent" label))
      (should (string-match-p "edit.*delete" label))))
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
  :doc "multiline and long authored text stays complete but the label stays bounded"
  (mevedel-notes-test--with-source
    (let* ((text (concat "First\nsecond " (make-string 500 ?x)))
           (note (mevedel-note-create text)))
      (should (= 1 (cl-count ?\n (car (mevedel-notes-test--labels)))))
      (should (< (length (car (mevedel-notes-test--labels))) 240))
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
                              0 'face (overlay-get (overlay-get child 'mevedel-label-overlay)
                                                   'after-string))))
      (call-interactively #'mevedel-cycle-instructions-at-point)
      (should (eq 'shadow (get-text-property
                           0 'face (overlay-get (overlay-get child 'mevedel-label-overlay)
                                                'after-string))))
      (should (eq 'highlight (get-text-property
                              0 'face (overlay-get (overlay-get parent 'mevedel-label-overlay)
                                                   'after-string))))
      (call-interactively #'mevedel-cycle-instructions-at-point)
      (should (eq 'shadow (get-text-property
                           0 'face (overlay-get (overlay-get parent 'mevedel-label-overlay)
                                                'after-string))))
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
          (should (mevedel-session-artifacts-load-instructions
                   session (current-buffer))))
        (should (= 1 (length (mevedel-notes-test--labels))))
        (should (string-match-p "Shared by sessions"
                                (car (mevedel-notes-test--labels))))))))

(mevedel-deftest mevedel-note-edit
  (:quiet t)
  ,test
  (test)
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
  :doc "modified-key menu edits note text while plain letters insert source text"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Before")
    (forward-char 2)
    (dolist (key '("e" "d" "s" "t" "x"))
      (should (eq #'self-insert-command (key-binding (kbd key)))))
    (let ((last-command-event ?e)) (call-interactively (key-binding "e")))
    (should (equal "before\nseelected code\nafter\n" (buffer-string)))
    ;; These stubs supply user input at the minibuffer boundary only.
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _)
                  (should (equal '(?s ?e ?x ?t ?d) (mapcar #'car choices)))
                 (assq ?e choices)))
              ((symbol-function 'read-string)
               (lambda (&rest _) "After")))
      (call-interactively (key-binding (kbd "M-m"))))
    (should (string-match-p "After" (car (mevedel-notes-test--labels))))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (should (string-match-p "After" (car (mevedel-notes-test--labels)))))
  :doc "menu deletion removes the note and label without changing source"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Delete me")
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _) (assq ?d choices))))
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
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _)
                 (or (assq ?x choices) (ert-fail "Missing done action")))))
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
      (should (string-match-p "Note 1.*Review the boundary.*M-m.*edit.*delete"
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
      (overlay-put note 'mevedel-note-prepared-change '(:test t))
      (overlay-put note 'mevedel-note-attention-state 'ready)
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
      (overlay-put note 'mevedel-note-prepared-change '(:test t))
      (overlay-put note 'mevedel-note-attention-state 'ready)
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
    (should (string-match-p "Note 1.*Finish this"
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
          (should-error (mevedel-note-restore 1 workspace) :type 'user-error))))))

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

(provide 'test-mevedel-notes)
;;; test-mevedel-notes.el ends here
