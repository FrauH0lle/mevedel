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
          (mevedel--instruction-current-state-key :global)
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          (mevedel-instruction-store--warned nil)
          (mevedel-buddy--changes (make-hash-table :test #'equal))
          (mevedel-buddy--last-review (make-hash-table :test #'equal))
          (mevedel-buddy--levels (make-hash-table :test #'equal))
          (mevedel-buddy--briefs (make-hash-table :test #'equal))
          (mevedel-notes--taken-watches (make-hash-table :test #'equal))
          (find-file-hook (copy-sequence find-file-hook))
          (post-command-hook (copy-sequence post-command-hook))
          (mevedel-buddy--running nil)
          (mevedel-buddy--running-automatic nil)
          (mevedel-buddy--generation 0)
          (mevedel-buddy--prepare-timer nil)
          (mevedel-buddy--timeout-timer nil)
          (mevedel-buddy--request-buffer nil)
          (mevedel-buddy-note--notes nil)
          (mevedel-buddy-note--changed-workspaces nil)
          (mevedel-buddy-note--prepared-buffers nil)
          (mevedel-buddy-note--scope-buffers nil)
          (mevedel-buddy-note--markers nil)
          (mevedel-buddy-note--taken nil)
          (mevedel-buddy-note--goals nil)
          (mevedel-buddy-note--level nil)
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
      (mevedel-take-test--review (lambda (&rest _) (ert-fail "Quiet spoke")) t)
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

(mevedel-deftest mevedel-buddy-review/settled-take
  (:quiet t)
  ,test
  (test)
  :doc "Quiet does not resend unchanged taken code after editing another file"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (mevedel-note-take (mevedel-note-create "Implement this goal"))
    (should (mevedel-take-test--review #'ignore t))
    (let ((other (file-name-concat root "other.el")))
      (write-region "unrelated code\n" nil other nil 'silent)
      (with-current-buffer (find-file-noselect other)
        (setq-local mevedel--workspace workspace)
        (mevedel-buddy-mode 1)
        (insert "edited ")
        (should-not (mevedel-take-test--review #'ignore t)))))
  :doc "edits made during a settled review still receive their own review"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (mevedel-note-take (mevedel-note-create "Implement this goal"))
    (should (mevedel-take-test--review
             (lambda (&rest _)
               (with-current-buffer source (insert "new edit "))) t))
    (should (mevedel-take-test--review #'ignore t))
    (should-not (mevedel-take-test--review #'ignore t)))
  :doc "an abandoned review does not mark taken code as reviewed"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (mevedel-note-take (mevedel-note-create "Implement this goal"))
    (should (mevedel-take-test--review
             (lambda (&rest _) (mevedel-buddy-abort)) t))
    (should (mevedel-take-test--review #'ignore t)))
  :doc "an edited goal receives a new review even when its code is unchanged"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (let ((note (mevedel-note-create "Implement this goal")))
      (mevedel-note-take note)
      (should (mevedel-take-test--review #'ignore t))
      (mevedel-note-edit "A revised goal" note)
      (should (mevedel-take-test--review
               (lambda (prompt _tools) (should (string-search "A revised goal" prompt))) t))))
  :doc "taking a note again renews its invitation after a settled review"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (let ((note (mevedel-note-create "Implement this goal")))
      (mevedel-note-take note)
      (should (mevedel-take-test--review #'ignore t))
      (mevedel-note-drop note)
      (mevedel-note-take note)
      (should (mevedel-take-test--review #'ignore t))))
  :doc "changing the dial renews review of unchanged taken code"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (mevedel-note-take (mevedel-note-create "Implement this goal"))
    (should (mevedel-take-test--review #'ignore t))
    (mevedel-buddy-set-level 'watching workspace)
    (should (mevedel-take-test--review #'ignore t)))
  :doc "explicit reviews are never suppressed by a settled take"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (mevedel-note-take (mevedel-note-create "Implement this goal"))
    (should (mevedel-take-test--review #'ignore t))
    (should (mevedel-take-test--review #'ignore))
    (should (mevedel-take-test--review #'ignore)))
  :doc "a changed workspace brief renews review of unchanged taken code"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (mevedel-note-take (mevedel-note-create "Implement this goal"))
    (should (mevedel-take-test--review #'ignore t))
    (mevedel-buddy-set-brief (file-name-as-directory (mevedel-workspace-root workspace))
                             "test" "Changed scope brief")
    (should (mevedel-take-test--review
             (lambda (prompt _tools) (should (string-search "Changed scope brief" prompt))) t))))

(mevedel-deftest mevedel-buddy-review/printer-limits
  (:quiet t)
  ,test
  (test)
  :doc "printer limits cannot hide changed taken code from automatic review"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (mevedel-note-take (mevedel-note-create "Implement this goal"))
    (let ((print-level 2) (print-length 2))
      (should (mevedel-take-test--review #'ignore t))
      (insert "changed code ")
      (should (mevedel-take-test--review #'ignore t)))))

(mevedel-deftest mevedel-buddy-review/large-take
  (:quiet t)
  ,test
  (test)
  :doc "a large take sends bounded code around the edit and can read the rest"
  (mevedel-take-test--with-source
    (erase-buffer)
    (dotimes (i 3000) (insert (format "source line %s\n" (1+ i))))
    (let ((transient-mark-mode t)
          (mevedel-buddy-note-read-limit 12)
          (name (buffer-name)))
      (goto-char (point-min)) (set-mark (point-max)) (activate-mark)
      (let ((note (mevedel-note-create "Review this large goal")))
        (mevedel-note-take note)
        (mevedel-buddy-set-level 'quiet workspace)
        (goto-char (point-min)) (forward-line 1499) (insert "edited ")
        (goto-char (point-min))
        (should
         (mevedel-take-test--review
          (lambda (prompt tools)
            (should (string-search "edited source line 1500" prompt))
            (should (= 12 (with-temp-buffer
                            (insert prompt)
                            (how-many "^ *[0-9]+  " (point-min) (point-max)))))
            (should (string-search "read_buffer" prompt))
            (should (stringp (mevedel-take-test--tool
                              tools "add_note" name 1 "Not shown" "significant")))
            (should (string-search "source line 1" (mevedel-take-test--tool
                                                    tools "read_buffer" name 1 2)))
            (should (integerp (mevedel-take-test--tool
                               tools "add_note" name 1 "Now shown" "significant")))) t))))))

(mevedel-deftest mevedel-buddy-review/paused-goal
  (:quiet t)
  ,test
  (test)
  :doc "Active shows a paused taken note's nearby goal before allowing ideas"
  (mevedel-take-test--with-source
    (let ((note (mevedel-note-create "Add retry with backoff"))
          (name (buffer-name)))
      (mevedel-note-take note)
      (mevedel-instruction-store-flush t)
      (mevedel--clear-instruction-state workspace)
      (clrhash mevedel-notes--taken-watches)
      (mevedel-instruction-store-observe workspace)
      (should (string-search "paused" (mevedel-take-test--labels)))
      (insert "edited ")
      (should
       (mevedel-take-test--review
        (lambda (prompt tools)
          (should (string-search "Nearby goal" prompt))
          (should (string-search "Add retry with backoff" prompt))
          (should (integerp (mevedel-take-test--tool
                             tools "add_note" name 2 "Try jitter" "significant" "idea")))) t)))))

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

(mevedel-deftest mevedel-note-take/restore
  (:quiet t)
  ,test
  (test)
  :doc "restored takes pause until taking again, without dial reads on restore"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (mevedel-note-take (mevedel-note-create "Keep watching after reload"))
    (mevedel-instruction-store-flush t)
    (mevedel-buddy-mode -1)
    (mevedel--clear-instruction-state workspace)
    (clrhash mevedel-buddy--levels)
    (clrhash mevedel-notes--taken-watches)
    (let* ((reads 0)
           (counter (lambda (file &rest _)
                      (when (string-suffix-p "/buddy-level" file) (cl-incf reads)))))
      (unwind-protect
          (progn
            (advice-add 'mevedel-session-control-fs-read-file :before counter)
            (mevedel-instruction-store-observe workspace)
            (should-not mevedel-buddy-mode)
            (should (zerop reads))
            (should (string-search "paused" (mevedel-take-test--labels)))
            (should-not (string-search "Buddy watches" (mevedel-take-test--labels)))
            (should (eq 'taken (mevedel-notes-attention-state (mevedel-notes--at-point))))
            (mevedel-note-take)
            (should mevedel-buddy-mode)
            (should-not (string-search "paused" (mevedel-take-test--labels)))
            (should (string-search "Buddy watches" (mevedel-take-test--labels)))
            (should (mevedel-take-test--review
                     (lambda (prompt _tools)
                       (should (string-search "Workspace dial: quiet" prompt))) t))
            (should (= 1 reads)))
        (advice-remove 'mevedel-session-control-fs-read-file counter))))
  :doc "the dial resumes paused takes, including later file visits in this session"
  (mevedel-take-test--with-source
    (let ((other-file (file-name-concat root "other.el")))
      (write-region "other code\n" nil other-file nil 'silent)
      (mevedel-note-take (mevedel-note-create "Current file goal"))
      (with-current-buffer (find-file-noselect other-file)
        (setq-local mevedel--workspace workspace)
        (mevedel-note-take (mevedel-note-create "Unvisited file goal"))
        (kill-buffer))
      (mevedel-instruction-store-flush t)
      (mevedel-buddy-mode -1)
      (mevedel--clear-instruction-state workspace)
      (clrhash mevedel-notes--taken-watches)
      (mevedel-instruction-store-observe workspace)
      (should (string-search "paused" (mevedel-take-test--labels)))
      (mevedel-buddy-set-level 'quiet workspace)
      (should mevedel-buddy-mode)
      (should (string-search "Buddy watches" (mevedel-take-test--labels)))
      (should-not (string-search "paused" (mevedel-take-test--labels)))
      (with-current-buffer (find-file-noselect other-file)
        (should mevedel-buddy-mode)
        (should (string-search "Buddy watches" (mevedel-take-test--labels)))
        (should-not (string-search "paused" (mevedel-take-test--labels)))))))

(mevedel-deftest mevedel-buddy-review/invalid-dial
  (:quiet t)
  ,test
  (test)
  :doc "an invalid saved dial falls back once without stranding a taken note"
  (mevedel-take-test--with-source
    (write-region "invalid" nil (file-name-concat root ".mevedel/buddy-level") nil 'silent)
    (let ((note (mevedel-note-create "Keep my taken note")))
      (mevedel-note-take note)
      (should (mevedel-take-test--review
               (lambda (prompt _tools)
                 (should (string-search "Workspace dial: active" prompt))) t))
      (should (eq 'taken (mevedel-notes-attention-state note)))
      (should (string-search "Buddy watches" (mevedel-take-test--labels)))
      (should (eq 'active (mevedel-buddy-level workspace))))))

(mevedel-deftest mevedel-buddy-review/workspace-scope
  (:quiet t)
  ,test
  (test)
  :doc "a parent's review cannot inherit a different pinned workspace's taken goal"
  (mevedel-take-test--with-source
    (should (zerop (call-process "git" nil nil nil "init" "-q" root)))
    (mevedel-buddy-set-level 'active workspace)
    (let* ((child (file-name-concat root "child"))
           (child-file (file-name-concat child "child.el")))
      (make-directory (file-name-concat child ".mevedel") t)
      (write-region "private child code\n" nil child-file nil 'silent)
      (with-current-buffer (find-file-noselect child-file)
        (setq-local mevedel--workspace
                    (mevedel-workspace-get-or-create 'project child child "child"))
        (mevedel-buddy-set-level 'quiet mevedel--workspace)
        (mevedel-note-take (mevedel-note-create "Private child goal")))
      (insert "parent edit ")
      (should (mevedel-take-test--review
               (lambda (prompt _tools)
                 (should-not (string-search "Private child goal" prompt))
                 (should-not (string-search "private child code" prompt))) t)))))

(mevedel-deftest mevedel-note-edit/completion-feedback
  (:quiet t)
  ,test
  (test)
  :doc "re-taking or changing a goal retires earlier looks-done feedback"
  (mevedel-take-test--with-source
    (let ((note (mevedel-note-create "Implement this goal")))
      (mevedel-note-take note)
      (mevedel-take-test--review
       (lambda (_prompt tools)
         (mevedel-take-test--tool tools "note_looks_done" (overlay-get note 'mevedel-id))) t)
      (should (string-search "looks done" (mevedel-take-test--labels)))
      (mevedel-note-take note)
      (should-not (string-search "looks done" (mevedel-take-test--labels)))
      (mevedel-take-test--review
       (lambda (_prompt tools)
         (mevedel-take-test--tool tools "note_looks_done" (overlay-get note 'mevedel-id))) t)
      (mevedel-note-edit "A different goal" note)
      (should-not (string-search "looks done" (mevedel-take-test--labels)))
      (should (eq 'taken (mevedel-notes-attention-state note))))))

(mevedel-deftest mevedel-buddy-review/unnoted-source
  (:quiet t)
  ,test
  (test)
  :doc "Quiet applies to workspace source files which have never had notes"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'quiet workspace)
    (let ((other-file (file-name-concat root "unnoted.el")))
      (write-region "unnoted code\n" nil other-file nil 'silent)
      (with-current-buffer (find-file-noselect other-file)
        (should mevedel-buddy-mode)
        (insert "edited ")
        (should-not (mevedel-take-test--review
                     (lambda (&rest _) (ert-fail "Quiet spoke in an unnoted source")) t))))))

(mevedel-deftest mevedel-note-take/buddy-ownership
  (:quiet t)
  ,test
  (test)
  :doc "taking a Buddy remark protects it from its review and retains ownership on reload"
  (mevedel-take-test--with-source
    (mevedel-buddy-set-level 'watching workspace)
    (insert "edited ")
    (let ((name (buffer-name)))
      (should
       (mevedel-take-test--review
        (lambda (_prompt tools)
          (let ((id (mevedel-take-test--tool
                     tools "add_note" name 2 "Implement this fix" "significant" "mistake")))
            (should (integerp id))
            (with-current-buffer source
              (mevedel-note-take (mevedel-notes--at-point)))
            (should (stringp (mevedel-take-test--tool tools "remove_note" id))))) t)))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (let ((note (mevedel-notes--at-point)))
      (should (eq 'user (overlay-get note 'mevedel-note-author)))
      (should (overlay-get note 'mevedel-note-kept-from-buddy))
      (should (eq 'taken (mevedel-notes-attention-state note))))))

(mevedel-deftest mevedel-buddy-review/project-brief
  (:quiet t)
  ,test
  (test)
  :doc "a project brief reaches pinned source notes even under the home directory"
  (let ((temporary-file-directory (expand-file-name "~/"))
        (mevedel-buddy--briefs (make-hash-table :test #'equal)))
    (mevedel-take-test--with-source
      (make-directory (file-name-concat root ".git"))
      (mevedel-buddy-set-brief (mevedel-buddy-directory-scope-key root)
                              "the project request" "SHARED-PROJECT-BRIEF")
      (mevedel-note-take (mevedel-note-create "Implement this goal"))
      (should
       (mevedel-take-test--review
        (lambda (prompt _tools)
          (should (string-search "SHARED-PROJECT-BRIEF" prompt))) t)))))

(mevedel-deftest mevedel-buddy-review/update-dial
  (:quiet t)
  ,test
  (test)
  :doc "automatic remark updates obey the same dial as new remarks"
  (dolist (level '(quiet watching))
    (mevedel-take-test--with-source
      (mevedel-buddy-mode 1)
      (goto-char (point-min))
      (insert "edited ")
      (let ((name (buffer-name)) id)
        (should
         (mevedel-take-test--review
          (lambda (_prompt tools)
            (setq id (mevedel-take-test--tool
                      tools "add_note" name 1 "Original remark" "significant" "mistake"))
            (should (integerp id)))))
        (goto-char (point-min)) (forward-line 1)
        (mevedel-note-take (mevedel-note-create "Implement the selected code"))
        (mevedel-buddy-set-level level workspace)
        (should
         (mevedel-take-test--review
          (lambda (_prompt tools)
            (should
             (string-match-p
              (if (eq level 'quiet) "Quiet" "Ideas")
              (if (eq level 'quiet)
                  (mevedel-take-test--tool tools "update_note" id "Uninvited update")
                (mevedel-take-test--tool tools "update_note" id "Uninvited update" "idea"))))
            (let ((invited (mevedel-take-test--tool
                            tools "add_note" name 2 "Taken concern" "significant" "mistake")))
              (should (integerp invited))
              (mevedel-take-test--tool tools "update_note" invited "Current taken concern"))) t))
        (should (string-search "Original remark" (mevedel-take-test--labels)))
        (should (string-search "Current taken concern" (mevedel-take-test--labels)))
        (should-not (string-search "Uninvited update" (mevedel-take-test--labels)))))))

(provide 'test-mevedel-buddy-take)
;;; test-mevedel-buddy-take.el ends here
