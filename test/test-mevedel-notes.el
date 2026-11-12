;;; test-mevedel-notes.el -- Source note command tests -*- lexical-binding: t -*-

;;; Commentary:

;; Drive note commands in real source files and observe their presentations.

;;; Code:

(require 'mevedel)
(require 'mevedel-persistence)
(require 'mevedel-session-artifacts)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

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
      (should-not (overlay-buffer note))))
  :doc "notes coexist with nested references and directives without sending them"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Not prompt context")
    (let ((reference (mevedel--create-reference-in source 8 12))
          (directive (mevedel--create-directive-in source 13 16 nil "Explain")))
      (should (eq 'reference (mevedel--instruction-type reference)))
      (should (eq 'directive (mevedel--instruction-type directive)))
      (should (string-match-p "REFERENCE" (overlay-get reference 'before-string)))
      (should (string-match-p "DIRECTIVE" (overlay-get directive 'before-string)))
      (should (= 1 (length (mevedel-notes-test--labels))))
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (should (= 1 (length (mevedel-notes-test--labels)))))))

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
                 (should (equal '(?e ?d) (mapcar #'car choices)))
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
    (should-not (mevedel-notes-test--labels))))

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

(mevedel-deftest mevedel-note-source-undo
  (:quiet t)
  ,test
  (test)
  :doc "source undo restores note geometry after deleting its entire marked code"
  (mevedel-notes-test--with-source
    (mevedel-note-create "Keep the anchor")
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (delete-region (line-beginning-position) (line-beginning-position 2))
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

(provide 'test-mevedel-notes)
;;; test-mevedel-notes.el ends here
