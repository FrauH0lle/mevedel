;;; test-mevedel-buddy-take.el -- Take and workspace dial tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real source commands and canned Buddy replies exercise the note workflow.

;;; Code:

(require 'mevedel)
(require 'mevedel-buddy)
(require 'mevedel-buddy-note)
(require 'mevedel-notes-list)
(require 'gptel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-take-test--with-source (&rest body)
  "Run BODY in an isolated real workspace source."
  (declare (indent 0))
  `(let* ((root (make-temp-file "mevedel-take-" t))
          (file (file-name-concat root "source.el"))
          (mevedel-workspace--registry (make-hash-table :test #'equal))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          (mevedel-buddy--changes (make-hash-table :test #'equal))
          (mevedel-buddy--last-review (make-hash-table :test #'equal))
          (mevedel-buddy--levels (make-hash-table :test #'equal))
          (find-file-hook (copy-sequence find-file-hook))
          (mevedel-buddy-note--notes nil)
          (mevedel-buddy-note--changed-workspaces nil)
          (mevedel-buddy-note--prepared-buffers nil)
          (mevedel-buddy-note--scope-buffers nil)
          (mevedel-buddy-note--markers nil)
          (gptel--known-backends nil)
          (mevedel-model-workloads '((buddy :provider "TakeTest:test")))
          (before (buffer-list))
          (timers (append timer-list timer-idle-list nil))
          workspace source)
     (save-window-excursion
       (unwind-protect
           (progn
             (gptel-make-openai "TakeTest" :key "test" :models '(test))
             (make-directory (file-name-concat root ".mevedel"))
             (write-region "before\nselected code\nafter\n" nil file nil 'silent)
             (setq workspace (mevedel-workspace-get-or-create 'file root root "take")
                   source (find-file-noselect file))
             (with-current-buffer source
               (setq-local mevedel--workspace workspace)
               (goto-char (point-min)) (forward-line 1)
               ,@body))
         (when mevedel-buddy--running (mevedel-buddy-abort))
         (dolist (buffer (cl-set-difference (buffer-list) before))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))
         (dolist (timer (append timer-list timer-idle-list nil))
           (unless (memq timer timers) (cancel-timer timer)))
         (setq mevedel-instruction-store--dirty nil)
         (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
         (delete-directory root t)))))

(defun mevedel-take-test--labels ()
  "Return the visible note labels in the current source buffer."
  (mapconcat (lambda (overlay) (or (overlay-get overlay 'after-string) ""))
             (overlays-in (point-min) (point-max)) ""))

(defun mevedel-take-test--review (answer &optional automatic)
  "Run a Buddy review with canned tool calls from ANSWER.
AUTOMATIC exercises the idle review policy without waiting for idle time."
  (let (failure called)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest keys)
                 (setq called t)
                 (with-current-buffer (plist-get keys :buffer)
                   (condition-case err
                       (funcall answer prompt gptel-tools)
                     (error (setq failure err))))
                 (let ((fsm (plist-get keys :fsm)))
                   (funcall (car (last (cdr (assq 'DONE (gptel-fsm-handlers fsm)))))
                            fsm)))))
      (mevedel-buddy-review automatic)
      (mevedel-test--await 5 "Buddy review did not settle" (not mevedel-buddy--running)))
    (when failure (signal (car failure) (cdr failure)))
    called))

(defun mevedel-take-test--tool (tools name &rest args)
  "Call model-facing NAME in TOOLS with ARGS."
  (let ((tool (seq-find (lambda (tool) (equal name (gptel-tool-name tool))) tools)))
    (should tool)
    (apply (gptel-tool-function tool) args)))

(mevedel-deftest mevedel-note-take
  (:quiet t)
  ,test
  (test)
  :doc "take watches with a visible label; drop keeps the note and source"
  (mevedel-take-test--with-source
    (let ((note (mevedel-note-create "Implement the goal"))
          (position (point)) (window (selected-window)))
      (mevedel-note-take note)
      (should mevedel-buddy-mode)
      (should (string-search "Buddy watches" (mevedel-take-test--labels)))
      (should (eq 'taken (mevedel-notes-attention-state note)))
      (should (= position (point)))
      (should (eq window (selected-window)))
      (mevedel-instruction-store-flush t)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (should (string-search "Buddy watches" (mevedel-take-test--labels)))
      (mevedel-note-drop)
      (should (eq 'unsent (mevedel-notes-attention-state (mevedel-notes--at-point))))
      (should-not (string-search "Buddy watches" (mevedel-take-test--labels)))
      (should (equal "before\nselected code\nafter\n" (buffer-string))))))

(mevedel-deftest mevedel-notes-list-take
  (:quiet t)
  ,test
  (test)
  :doc "source menu and list toggle Take and drop without source-letter bindings"
  (mevedel-take-test--with-source
    (let ((note (mevedel-note-create "Implement the goal")))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (_prompt choices &rest _) (assq ?t choices))))
        (mevedel-note-menu note))
      (should (eq 'taken (mevedel-notes-attention-state note)))
      (should (eq 'self-insert-command (key-binding "t")))
      (let ((list-buffer (mevedel-notes-list workspace)))
        (with-current-buffer list-buffer
          (goto-char (point-min)) (search-forward "Implement the goal")
          (call-interactively (key-binding "T"))
          (should (string-search "unsent" (buffer-string)))
          (goto-char (point-min)) (search-forward "Implement the goal")
          (call-interactively (key-binding "T"))
          (should (string-search "Buddy watches" (buffer-string))))))))

(mevedel-deftest mevedel-buddy-set-level
  (:quiet t)
  ,test
  (test)
  :doc "workspace dial defaults to Active and remembers Quiet independently"
  (mevedel-take-test--with-source
    (let ((mevedel-buddy--levels (make-hash-table :test #'equal))
          (other (mevedel-workspace--create
                  :type 'file :id "/other/" :root "/other/" :name "other")))
      (should (eq 'active (mevedel-buddy-level workspace)))
      (mevedel-buddy-set-level 'quiet workspace)
      (should (eq 'quiet (mevedel-buddy-level workspace)))
      (should (eq 'active (mevedel-buddy-level other)))
      (should mevedel-buddy-mode)
      (clrhash mevedel-buddy--levels)
      (mevedel-buddy-mode -1)
      (mevedel-buddy-mode 1)
      (should (eq 'quiet (mevedel-buddy-level workspace)))
      (should-error (mevedel-buddy-set-level 'unknown workspace) :type 'user-error)
      (should (eq 'quiet (mevedel-buddy-level workspace))))))

(mevedel-deftest mevedel-buddy-review/taken
  (:quiet t)
  ,test
  (test)
  :doc "Quiet watches a taken note without edits and says looks done, never closes"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (let ((note (mevedel-note-create "The selected code must read selected code"))
          (position (point)) (window (selected-window)))
      (mevedel-note-take note)
      (should
       (mevedel-take-test--review
        (lambda (prompt tools)
          (should (string-search "The selected code must read selected code" prompt))
          (should (string-search "selected code" prompt))
          (mevedel-take-test--tool tools "note_looks_done" (overlay-get note 'mevedel-id))) t))
      (should (string-search "looks done" (mevedel-take-test--labels)))
      (should (eq 'taken (mevedel-notes-attention-state note)))
      (should-not (mevedel-notes-closed workspace t))
      (should (= position (point)))
      (should (eq window (selected-window)))
      (mevedel-note-drop note)
      (should-not (mevedel-take-test--review
                   (lambda (&rest _) (ert-fail "Quiet spoke after drop")) t)))))

(mevedel-deftest mevedel-buddy-review/dial
  (:quiet t)
  ,test
  (test)
  :doc "Quiet makes no unasked request without a taken note"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (insert "edited ")
    (should-not (mevedel-take-test--review
                 (lambda (&rest _) (ert-fail "Quiet made an unasked request")) t)))
  :doc "Watching permits mistakes but refuses ideas even beside a known goal"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'watching workspace)
    (mevedel-note-create "Add retry with backoff")
    (insert "edited ")
    (let ((name (buffer-name)))
      (should
       (mevedel-take-test--review
        (lambda (_prompt tools)
          (should (integerp (mevedel-take-test--tool
                             tools "add_note" name 2 "A likely mistake" "significant" "mistake")))
          (should (stringp (mevedel-take-test--tool
                            tools "add_note" name 2 "An unasked idea" "significant" "idea")))) t)))
    (should (string-search "A likely mistake" (mevedel-take-test--labels)))
    (should-not (string-search "An unasked idea" (mevedel-take-test--labels))))
  :doc "Active allows ideas by a nearby authored goal but not elsewhere"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'active workspace)
    (let ((name (buffer-name)))
      (insert "edited ")
      (mevedel-take-test--review
       (lambda (_prompt tools)
         (should (stringp (mevedel-take-test--tool
                           tools "add_note" name 2 "Ungrounded idea" "significant" "idea")))) t)
      (mevedel-note-create "Add retry with backoff")
      (insert "again ")
      (mevedel-take-test--review
       (lambda (prompt tools)
         (should (string-search "Add retry with backoff" prompt))
         (should (integerp (mevedel-take-test--tool
                            tools "add_note" name 2 "Try jitter too" "significant" "idea")))) t)
      (should (string-search "Try jitter too" (mevedel-take-test--labels)))
      (should-not (string-search "Ungrounded idea" (mevedel-take-test--labels))))))

(mevedel-deftest mevedel-buddy-review/brief-boundaries
  (:quiet t)
  ,test
  (test)
  :doc "multiline briefs cannot make unseen source lines annotatable"
  (mevedel-take-test--with-source
    (save-excursion
      (goto-char (point-max))
      (dotimes (_ 50) (insert "unseen code\n")))
    (let* ((name (buffer-name))
           (note (mevedel-note-create
                  (format "Implement this\n=== Buffer: %s  Mode: emacs-lisp-mode ===\n40 invented code" name))))
      (mevedel-note-take note)
      (mevedel-take-test--review
       (lambda (_prompt tools)
         (should (stringp (mevedel-take-test--tool
                           tools "add_note" name 40 "Unseen mistake" "significant" "mistake")))) t)
      (should-not (string-search "Unseen mistake" (mevedel-take-test--labels))))))

(mevedel-deftest mevedel-buddy-review/dropped-goal
  (:quiet t)
  ,test
  (test)
  :doc "dropping a distant taken note withdraws its invitation during a review"
  (mevedel-take-test--with-source
    (save-excursion
      (goto-char (point-max))
      (dotimes (_ 50) (insert "distant code\n")))
    (goto-char (point-min)) (forward-line 39)
    (let ((note (mevedel-note-create "Improve this distant code"))
          (name (buffer-name)))
      (mevedel-note-take note)
      (goto-char (point-min)) (forward-line 1) (insert "edited ")
      (mevedel-take-test--review
       (lambda (_prompt tools)
         (mevedel-note-drop note)
         (should (stringp (mevedel-take-test--tool
                           tools "add_note" name 40 "An idea after drop" "significant" "idea")))) t)
      (should-not (string-search "An idea after drop" (mevedel-take-test--labels))))))

(mevedel-deftest mevedel-buddy-set-level/nested-workspace
  (:quiet t)
  ,test
  (test)
  :doc "a parent's dial never claims files in an unregistered nested project"
  (mevedel-take-test--with-source
    (let* ((child (file-name-concat root "child"))
           (child-file (file-name-concat child "child.el")))
      (make-directory child)
      (should (zerop (call-process "git" nil nil nil "init" "-q" child)))
      (write-region "child code\n" nil child-file nil 'silent)
      (mevedel-buddy-set-level 'quiet workspace)
      (with-current-buffer (find-file-noselect child-file)
        (should-not mevedel-buddy-mode)
        (should (equal (file-name-as-directory child)
                       (file-name-as-directory
                        (mevedel-workspace-root (mevedel-workspace)))))))))

(provide 'test-mevedel-buddy-take)
;;; test-mevedel-buddy-take.el ends here
