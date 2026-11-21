;;; test-niwa-buddy-take.el -- Take and workspace dial tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real source commands and canned Buddy replies exercise the note workflow.

;;; Code:

(require 'niwa)
(require 'niwa-buddy)
(require 'niwa-buddy-note)
(require 'niwa-notes-list)
(require 'gptel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro niwa-take-test--with-source (&rest body)
  "Run BODY in an isolated real workspace source."
  (declare (indent 0))
  `(let* ((root (make-temp-file "niwa-take-" t))
          (file (file-name-concat root "source.el"))
          (niwa-workspace--registry (make-hash-table :test #'equal))
          (niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)
          (niwa-instruction-store--dirty nil)
          (niwa-instruction-store--failed nil)
          (niwa-instruction-store--warned nil)
          (niwa-note-labels 'expanded)
          (niwa-buddy--changes (make-hash-table :test #'equal))
          (niwa-buddy--last-review (make-hash-table :test #'equal))
          (niwa-buddy--levels (make-hash-table :test #'equal))
          (niwa-buddy--briefs (make-hash-table :test #'equal))
          (niwa-notes--taken-watches (make-hash-table :test #'equal))
          (find-file-hook (copy-sequence find-file-hook))
          (post-command-hook (copy-sequence post-command-hook))
          (niwa-buddy--running nil)
          (niwa-buddy--running-automatic nil)
          (niwa-buddy--generation 0)
          (niwa-buddy--prepare-timer nil)
          (niwa-buddy--timeout-timer nil)
          (niwa-buddy--request-buffer nil)
          (niwa-buddy-note--notes nil)
          (niwa-buddy-note--run nil)
          (niwa-buddy-note--markers nil)
          (gptel--known-backends nil)
          (niwa-model-workloads '((buddy :provider "TakeTest:test")))
          (before (buffer-list))
          (timers (append timer-list timer-idle-list nil))
          workspace source)
     (save-window-excursion
       (unwind-protect
           (progn
             (gptel-make-openai "TakeTest" :key "test" :models '(test))
             (make-directory (file-name-concat root ".niwa"))
             (write-region "before\nselected code\nafter\n" nil file nil 'silent)
             (setq workspace (niwa-workspace-get-or-create 'file root root "take")
                   source (find-file-noselect file))
             (with-current-buffer source
               (setq-local niwa--workspace workspace)
               (goto-char (point-min)) (forward-line 1)
               ,@body))
         (when niwa-buddy--running (niwa-buddy-abort))
         (dolist (buffer (cl-set-difference (buffer-list) before))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))
         (dolist (timer (append timer-list timer-idle-list nil))
           (unless (memq timer timers) (cancel-timer timer)))
         (setq niwa-instruction-store--dirty nil)
         (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
         (delete-directory root t)))))

(defun niwa-take-test--labels ()
  "Return the visible note labels in the current source buffer."
  (mapconcat (lambda (overlay) (or (overlay-get overlay 'after-string) ""))
             (overlays-in (point-min) (point-max)) ""))

(defun niwa-take-test--review (answer &optional automatic)
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
      (niwa-buddy-review automatic)
      (niwa-test--await 5 "Buddy review did not settle" (not niwa-buddy--running)))
    (when failure (signal (car failure) (cdr failure)))
    called))

(defun niwa-take-test--tool (tools name &rest args)
  "Call model-facing NAME in TOOLS with ARGS."
  (let ((tool (seq-find (lambda (tool) (equal name (gptel-tool-name tool))) tools)))
    (should tool)
    (apply (gptel-tool-function tool) args)))

(niwa-deftest niwa-note-take
  (:quiet t)
  ,test
  (test)
  :doc "take watches with a visible label; drop keeps the note and source"
  (niwa-take-test--with-source
    (let ((note (niwa-note-create "Implement the goal"))
          (position (point)) (window (selected-window)))
      (niwa-note-take note)
      (should niwa-buddy-mode)
      (should (string-search "Buddy watches" (niwa-take-test--labels)))
      (should (eq 'taken (niwa-notes-attention-state note)))
      (should (= position (point)))
      (should (eq window (selected-window)))
      (niwa-instruction-store-flush t)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (should (string-search "Buddy watches" (niwa-take-test--labels)))
      (niwa-note-drop)
      (should (eq 'unsent (niwa-notes-attention-state (niwa-notes--at-point))))
      (should-not (string-search "Buddy watches" (niwa-take-test--labels)))
      (should (equal "before\nselected code\nafter\n" (buffer-string))))))

(niwa-deftest niwa-notes-list-take
  (:quiet t)
  ,test
  (test)
  :doc "source menu and list toggle Take and drop without source-letter bindings"
  (niwa-take-test--with-source
    (let ((note (niwa-note-create "Implement the goal")))
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?t)))
        (niwa-note-menu note))
      (should (eq 'taken (niwa-notes-attention-state note)))
      (should (eq 'self-insert-command (key-binding "t")))
      (let ((list-buffer (niwa-notes-list workspace)))
        (with-current-buffer list-buffer
          (goto-char (point-min)) (search-forward "Implement the goal")
          (call-interactively (key-binding "T"))
          (should (string-search "unsent" (buffer-string)))
          (goto-char (point-min)) (search-forward "Implement the goal")
          (call-interactively (key-binding "T"))
          (should (string-search "Buddy watches" (buffer-string))))))))

(niwa-deftest niwa-buddy-set-level
  (:quiet t)
  ,test
  (test)
  :doc "workspace dial defaults to Active and remembers Quiet independently"
  (niwa-take-test--with-source
    (let ((niwa-buddy--levels (make-hash-table :test #'equal))
          (other (niwa-workspace--create
                  :type 'file :id "/other/" :root "/other/" :name "other")))
      (should (eq 'active (niwa-buddy-level workspace)))
      (niwa-buddy-set-level 'quiet workspace)
      (should (eq 'quiet (niwa-buddy-level workspace)))
      (should (eq 'active (niwa-buddy-level other)))
      (should niwa-buddy-mode)
      (clrhash niwa-buddy--levels)
      (niwa-buddy-mode -1)
      (niwa-buddy-mode 1)
      (niwa-take-test--review (lambda (&rest _) (ert-fail "Quiet spoke")) t)
      (should (eq 'quiet (niwa-buddy-level workspace)))
      (should-error (niwa-buddy-set-level 'unknown workspace) :type 'user-error)
      (should (eq 'quiet (niwa-buddy-level workspace))))))

(niwa-deftest niwa-buddy-review/taken
  (:quiet t)
  ,test
  (test)
  :doc "Quiet watches a taken note without edits and says looks done, never closes"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (let ((note (niwa-note-create "The selected code must read selected code"))
          (position (point)) (window (selected-window)))
      (niwa-note-take note)
      (should
       (niwa-take-test--review
        (lambda (prompt tools)
          (should (string-search "The selected code must read selected code" prompt))
          (should (string-search "selected code" prompt))
          (niwa-take-test--tool tools "note_looks_done" (overlay-get note 'niwa-id))) t))
      (should (string-search "looks done" (niwa-take-test--labels)))
      (should (eq 'taken (niwa-notes-attention-state note)))
      (should-not (niwa-notes-closed workspace t))
      (should (= position (point)))
      (should (eq window (selected-window)))
      (niwa-note-drop note)
      (should-not (niwa-take-test--review
                   (lambda (&rest _) (ert-fail "Quiet spoke after drop")) t)))))

(niwa-deftest niwa-buddy-review/dial
  (:quiet t)
  ,test
  (test)
  :doc "Quiet makes no unasked request without a taken note"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (insert "edited ")
    (should-not (niwa-take-test--review
                 (lambda (&rest _) (ert-fail "Quiet made an unasked request")) t)))
  :doc "Watching permits mistakes but refuses ideas even beside a known goal"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'watching workspace)
    (niwa-note-create "Add retry with backoff")
    (insert "edited ")
    (let ((name (buffer-name)))
      (should
       (niwa-take-test--review
        (lambda (_prompt tools)
          (should (integerp (niwa-take-test--tool
                             tools "add_note" name 2 "A likely mistake" "significant" "mistake")))
          (should (stringp (niwa-take-test--tool
                            tools "add_note" name 2 "An unasked idea" "significant" "idea")))) t)))
    (should (string-search "A likely mistake" (niwa-take-test--labels)))
    (should-not (string-search "An unasked idea" (niwa-take-test--labels))))
  :doc "Active allows ideas by a nearby authored goal but not elsewhere"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'active workspace)
    (let ((name (buffer-name)))
      (insert "edited ")
      (niwa-take-test--review
       (lambda (_prompt tools)
         (should (stringp (niwa-take-test--tool
                           tools "add_note" name 2 "Ungrounded idea" "significant" "idea")))) t)
      (niwa-note-create "Add retry with backoff")
      (insert "again ")
      (niwa-take-test--review
       (lambda (prompt tools)
         (should (string-search "Add retry with backoff" prompt))
         (should (integerp (niwa-take-test--tool
                            tools "add_note" name 2 "Try jitter too" "significant" "idea")))) t)
      (should (string-search "Try jitter too" (niwa-take-test--labels)))
      (should-not (string-search "Ungrounded idea" (niwa-take-test--labels))))))

(niwa-deftest niwa-buddy-review/settled-take
  (:quiet t)
  ,test
  (test)
  :doc "Quiet does not resend unchanged taken code after editing another file"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (niwa-note-take (niwa-note-create "Implement this goal"))
    (should (niwa-take-test--review #'ignore t))
    (let ((other (file-name-concat root "other.el")))
      (write-region "unrelated code\n" nil other nil 'silent)
      (with-current-buffer (find-file-noselect other)
        (setq-local niwa--workspace workspace)
        (niwa-buddy-mode 1)
        (insert "edited ")
        (should-not (niwa-take-test--review #'ignore t)))))
  :doc "edits made during a settled review still receive their own review"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (niwa-note-take (niwa-note-create "Implement this goal"))
    (should (niwa-take-test--review
             (lambda (&rest _)
               (with-current-buffer source (insert "new edit "))) t))
    (should (niwa-take-test--review #'ignore t))
    (should-not (niwa-take-test--review #'ignore t)))
  :doc "an abandoned review does not mark taken code as reviewed"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (niwa-note-take (niwa-note-create "Implement this goal"))
    (should (niwa-take-test--review
             (lambda (&rest _) (niwa-buddy-abort)) t))
    (should (niwa-take-test--review #'ignore t)))
  :doc "an edited goal receives a new review even when its code is unchanged"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (let ((note (niwa-note-create "Implement this goal")))
      (niwa-note-take note)
      (should (niwa-take-test--review #'ignore t))
      (niwa-note-edit "A revised goal" note)
      (should (niwa-take-test--review
               (lambda (prompt _tools) (should (string-search "A revised goal" prompt))) t))))
  :doc "taking a note again renews its invitation after a settled review"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (let ((note (niwa-note-create "Implement this goal")))
      (niwa-note-take note)
      (should (niwa-take-test--review #'ignore t))
      (niwa-note-drop note)
      (niwa-note-take note)
      (should (niwa-take-test--review #'ignore t))))
  :doc "changing the dial renews review of unchanged taken code"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (niwa-note-take (niwa-note-create "Implement this goal"))
    (should (niwa-take-test--review #'ignore t))
    (niwa-buddy-set-level 'watching workspace)
    (should (niwa-take-test--review #'ignore t)))
  :doc "explicit reviews are never suppressed by a settled take"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (niwa-note-take (niwa-note-create "Implement this goal"))
    (should (niwa-take-test--review #'ignore t))
    (should (niwa-take-test--review #'ignore))
    (should (niwa-take-test--review #'ignore)))
  :doc "a changed workspace brief renews review of unchanged taken code"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (niwa-note-take (niwa-note-create "Implement this goal"))
    (should (niwa-take-test--review #'ignore t))
    (niwa-buddy-set-brief (file-name-as-directory (niwa-workspace-root workspace))
                             "test" "Changed scope brief")
    (should (niwa-take-test--review
             (lambda (prompt _tools) (should (string-search "Changed scope brief" prompt))) t))))

(niwa-deftest niwa-buddy-review/printer-limits
  (:quiet t)
  ,test
  (test)
  :doc "printer limits cannot hide changed taken code from automatic review"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (niwa-note-take (niwa-note-create "Implement this goal"))
    (let ((print-level 2) (print-length 2))
      (should (niwa-take-test--review #'ignore t))
      (insert "changed code ")
      (should (niwa-take-test--review #'ignore t)))))

(niwa-deftest niwa-buddy-review/large-take
  (:quiet t)
  ,test
  (test)
  :doc "a large take sends bounded code around the edit and can read the rest"
  (niwa-take-test--with-source
    (erase-buffer)
    (dotimes (i 3000) (insert (format "source line %s\n" (1+ i))))
    (let ((transient-mark-mode t)
          (niwa-buddy-note-read-limit 12)
          (name (buffer-name)))
      (goto-char (point-min)) (set-mark (point-max)) (activate-mark)
      (let ((note (niwa-note-create "Review this large goal")))
        (niwa-note-take note)
        (niwa-buddy-set-level 'quiet workspace)
        (goto-char (point-min)) (forward-line 1499) (insert "edited ")
        (goto-char (point-min))
        (should
         (niwa-take-test--review
          (lambda (prompt tools)
            (should (string-search "edited source line 1500" prompt))
            (should (= 12 (with-temp-buffer
                            (insert prompt)
                            (how-many "^ *[0-9]+  " (point-min) (point-max)))))
            (should (string-search "read_buffer" prompt))
            (should (stringp (niwa-take-test--tool
                              tools "add_note" name 1 "Not shown" "significant")))
            (should (string-search "source line 1" (niwa-take-test--tool
                                                    tools "read_buffer" name 1 2)))
            (should (integerp (niwa-take-test--tool
                               tools "add_note" name 1 "Now shown" "significant")))) t))))))

(niwa-deftest niwa-buddy-review/paused-goal
  (:quiet t)
  ,test
  (test)
  :doc "Active shows a paused taken note's nearby goal before allowing ideas"
  (niwa-take-test--with-source
    (let ((note (niwa-note-create "Add retry with backoff"))
          (name (buffer-name)))
      (niwa-note-take note)
      (niwa-instruction-store-flush t)
      (niwa--clear-instruction-state workspace)
      (clrhash niwa-notes--taken-watches)
      (niwa-instruction-store-observe workspace)
      (should (string-search "paused" (niwa-take-test--labels)))
      (insert "edited ")
      (should
       (niwa-take-test--review
        (lambda (prompt tools)
          (should (string-search "Nearby goal" prompt))
          (should (string-search "Add retry with backoff" prompt))
          (should (integerp (niwa-take-test--tool
                             tools "add_note" name 2 "Try jitter" "significant" "idea")))) t)))))

(niwa-deftest niwa-buddy-review/brief-boundaries
  (:quiet t)
  ,test
  (test)
  :doc "multiline briefs cannot make unseen source lines annotatable"
  (niwa-take-test--with-source
    (save-excursion
      (goto-char (point-max))
      (dotimes (_ 50) (insert "unseen code\n")))
    (let* ((name (buffer-name))
           (note (niwa-note-create
                  (format "Implement this\n=== Buffer: %s  Mode: emacs-lisp-mode ===\n40 invented code" name))))
      (niwa-note-take note)
      (niwa-take-test--review
       (lambda (_prompt tools)
         (should (stringp (niwa-take-test--tool
                           tools "add_note" name 40 "Unseen mistake" "significant" "mistake")))) t)
      (should-not (string-search "Unseen mistake" (niwa-take-test--labels))))))

(niwa-deftest niwa-buddy-review/dropped-goal
  (:quiet t)
  ,test
  (test)
  :doc "dropping a distant taken note withdraws its invitation during a review"
  (niwa-take-test--with-source
    (save-excursion
      (goto-char (point-max))
      (dotimes (_ 50) (insert "distant code\n")))
    (goto-char (point-min)) (forward-line 39)
    (let ((note (niwa-note-create "Improve this distant code"))
          (name (buffer-name)))
      (niwa-note-take note)
      (goto-char (point-min)) (forward-line 1) (insert "edited ")
      (niwa-take-test--review
       (lambda (_prompt tools)
         (niwa-note-drop note)
         (should (stringp (niwa-take-test--tool
                           tools "add_note" name 40 "An idea after drop" "significant" "idea")))) t)
      (should-not (string-search "An idea after drop" (niwa-take-test--labels))))))

(niwa-deftest niwa-buddy-set-level/nested-workspace
  (:quiet t)
  ,test
  (test)
  :doc "a parent's dial never claims files in an unregistered nested project"
  (niwa-take-test--with-source
    (let* ((child (file-name-concat root "child"))
           (child-file (file-name-concat child "child.el")))
      (make-directory child)
      (should (zerop (call-process "git" nil nil nil "init" "-q" child)))
      (write-region "child code\n" nil child-file nil 'silent)
      (niwa-buddy-set-level 'quiet workspace)
      (with-current-buffer (find-file-noselect child-file)
        (should-not niwa-buddy-mode)
        (should (equal (file-name-as-directory child)
                       (file-name-as-directory
                        (niwa-workspace-root (niwa-workspace)))))))))

(niwa-deftest niwa-note-take/restore
  (:quiet t)
  ,test
  (test)
  :doc "restored takes pause until taking again, without dial reads on restore"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (niwa-note-take (niwa-note-create "Keep watching after reload"))
    (niwa-instruction-store-flush t)
    (niwa-buddy-mode -1)
    (niwa--clear-instruction-state workspace)
    (clrhash niwa-buddy--levels)
    (clrhash niwa-notes--taken-watches)
    (let* ((reads 0)
           (counter (lambda (file &rest _)
                      (when (string-suffix-p "/buddy-level" file) (cl-incf reads)))))
      (unwind-protect
          (progn
            (advice-add 'niwa-session-control-fs-read-file :before counter)
            (niwa-instruction-store-observe workspace)
            (should-not niwa-buddy-mode)
            (should (zerop reads))
            (should (string-search "paused" (niwa-take-test--labels)))
            (should-not (string-search "Buddy watches" (niwa-take-test--labels)))
            (should (eq 'taken (niwa-notes-attention-state (niwa-notes--at-point))))
            (niwa-note-take)
            (should niwa-buddy-mode)
            (should-not (string-search "paused" (niwa-take-test--labels)))
            (should (string-search "Buddy watches" (niwa-take-test--labels)))
            (should (niwa-take-test--review
                     (lambda (prompt _tools)
                       (should (string-search "Workspace dial: quiet" prompt))) t))
            (should (= 1 reads)))
        (advice-remove 'niwa-session-control-fs-read-file counter))))
  :doc "the dial resumes paused takes, including later file visits in this session"
  (niwa-take-test--with-source
    (let ((other-file (file-name-concat root "other.el")))
      (write-region "other code\n" nil other-file nil 'silent)
      (niwa-note-take (niwa-note-create "Current file goal"))
      (with-current-buffer (find-file-noselect other-file)
        (setq-local niwa--workspace workspace)
        (niwa-note-take (niwa-note-create "Unvisited file goal"))
        (kill-buffer))
      (niwa-instruction-store-flush t)
      (niwa-buddy-mode -1)
      (niwa--clear-instruction-state workspace)
      (clrhash niwa-notes--taken-watches)
      (niwa-instruction-store-observe workspace)
      (should (string-search "paused" (niwa-take-test--labels)))
      (niwa-buddy-set-level 'quiet workspace)
      (should niwa-buddy-mode)
      (should (string-search "Buddy watches" (niwa-take-test--labels)))
      (should-not (string-search "paused" (niwa-take-test--labels)))
      (with-current-buffer (find-file-noselect other-file)
        (should niwa-buddy-mode)
        (should (string-search "Buddy watches" (niwa-take-test--labels)))
        (should-not (string-search "paused" (niwa-take-test--labels)))))))

(niwa-deftest niwa-buddy-review/invalid-dial
  (:quiet t)
  ,test
  (test)
  :doc "an invalid saved dial falls back once without stranding a taken note"
  (niwa-take-test--with-source
    (write-region "invalid" nil (file-name-concat root ".niwa/buddy-level") nil 'silent)
    (let ((note (niwa-note-create "Keep my taken note")))
      (niwa-note-take note)
      (should (niwa-take-test--review
               (lambda (prompt _tools)
                 (should (string-search "Workspace dial: active" prompt))) t))
      (should (eq 'taken (niwa-notes-attention-state note)))
      (should (string-search "Buddy watches" (niwa-take-test--labels)))
      (should (eq 'active (niwa-buddy-level workspace))))))

(niwa-deftest niwa-buddy-review/workspace-scope
  (:quiet t)
  ,test
  (test)
  :doc "a parent's review cannot inherit a different pinned workspace's taken goal"
  (niwa-take-test--with-source
    (should (zerop (call-process "git" nil nil nil "init" "-q" root)))
    (niwa-buddy-set-level 'active workspace)
    (let* ((child (file-name-concat root "child"))
           (child-file (file-name-concat child "child.el")))
      (make-directory (file-name-concat child ".niwa") t)
      (write-region "private child code\n" nil child-file nil 'silent)
      (with-current-buffer (find-file-noselect child-file)
        (setq-local niwa--workspace
                    (niwa-workspace-get-or-create 'project child child "child"))
        (niwa-buddy-set-level 'quiet niwa--workspace)
        (niwa-note-take (niwa-note-create "Private child goal")))
      (insert "parent edit ")
      (should (niwa-take-test--review
               (lambda (prompt _tools)
                 (should-not (string-search "Private child goal" prompt))
                 (should-not (string-search "private child code" prompt))) t)))))

(niwa-deftest niwa-note-edit/completion-feedback
  (:quiet t)
  ,test
  (test)
  :doc "re-taking or changing a goal retires earlier looks-done feedback"
  (niwa-take-test--with-source
    (let ((note (niwa-note-create "Implement this goal")))
      (niwa-note-take note)
      (niwa-take-test--review
       (lambda (_prompt tools)
         (niwa-take-test--tool tools "note_looks_done" (overlay-get note 'niwa-id))) t)
      (should (string-search "looks done" (niwa-take-test--labels)))
      (niwa-note-take note)
      (should-not (string-search "looks done" (niwa-take-test--labels)))
      (niwa-take-test--review
       (lambda (_prompt tools)
         (niwa-take-test--tool tools "note_looks_done" (overlay-get note 'niwa-id))) t)
      (niwa-note-edit "A different goal" note)
      (should-not (string-search "looks done" (niwa-take-test--labels)))
      (should (eq 'taken (niwa-notes-attention-state note))))))

(niwa-deftest niwa-buddy-review/unnoted-source
  (:quiet t)
  ,test
  (test)
  :doc "Quiet applies to workspace source files which have never had notes"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'quiet workspace)
    (let ((other-file (file-name-concat root "unnoted.el")))
      (write-region "unnoted code\n" nil other-file nil 'silent)
      (with-current-buffer (find-file-noselect other-file)
        (should niwa-buddy-mode)
        (insert "edited ")
        (should-not (niwa-take-test--review
                     (lambda (&rest _) (ert-fail "Quiet spoke in an unnoted source")) t))))))

(niwa-deftest niwa-note-take/buddy-ownership
  (:quiet t)
  ,test
  (test)
  :doc "taking a Buddy remark protects it from its review and retains ownership on reload"
  (niwa-take-test--with-source
    (niwa-buddy-set-level 'watching workspace)
    (insert "edited ")
    (let ((name (buffer-name)))
      (should
       (niwa-take-test--review
        (lambda (_prompt tools)
          (let ((id (niwa-take-test--tool
                     tools "add_note" name 2 "Implement this fix" "significant" "mistake")))
            (should (integerp id))
            (with-current-buffer source
              (niwa-note-take (niwa-notes--at-point)))
            (should (stringp (niwa-take-test--tool tools "remove_note" id))))) t)))
    (niwa-instruction-store-flush t)
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (let ((note (niwa-notes--at-point)))
      (should (eq 'user (overlay-get note 'niwa-note-author)))
      (should (overlay-get note 'niwa-note-kept-from-buddy))
      (should (eq 'taken (niwa-notes-attention-state note))))))

(niwa-deftest niwa-buddy-review/project-brief
  (:quiet t)
  ,test
  (test)
  :doc "a project brief reaches pinned source notes even under the home directory"
  (let ((temporary-file-directory (expand-file-name "~/"))
        (niwa-buddy--briefs (make-hash-table :test #'equal)))
    (niwa-take-test--with-source
      (make-directory (file-name-concat root ".git"))
      (niwa-buddy-set-brief (niwa-buddy-directory-scope-key root)
                              "the project request" "SHARED-PROJECT-BRIEF")
      (niwa-note-take (niwa-note-create "Implement this goal"))
      (should
       (niwa-take-test--review
        (lambda (prompt _tools)
          (should (string-search "SHARED-PROJECT-BRIEF" prompt))) t)))))

(niwa-deftest niwa-buddy-review/update-dial
  (:quiet t)
  ,test
  (test)
  :doc "automatic remark updates obey the same dial as new remarks"
  (dolist (level '(quiet watching))
    (niwa-take-test--with-source
      (niwa-buddy-mode 1)
      (goto-char (point-min))
      (insert "edited ")
      (let ((name (buffer-name)) id)
        (should
         (niwa-take-test--review
          (lambda (_prompt tools)
            (setq id (niwa-take-test--tool
                      tools "add_note" name 1 "Original remark" "significant" "mistake"))
            (should (integerp id)))))
        (goto-char (point-min)) (forward-line 1)
        (niwa-note-take (niwa-note-create "Implement the selected code"))
        (niwa-buddy-set-level level workspace)
        (should
         (niwa-take-test--review
          (lambda (_prompt tools)
            (should
             (string-match-p
              (if (eq level 'quiet) "Quiet" "Ideas")
              (if (eq level 'quiet)
                  (niwa-take-test--tool tools "update_note" id "Uninvited update")
                (niwa-take-test--tool tools "update_note" id "Uninvited update" "idea"))))
            (let ((invited (niwa-take-test--tool
                            tools "add_note" name 2 "Taken concern" "significant" "mistake")))
              (should (integerp invited))
              (niwa-take-test--tool tools "update_note" invited "Current taken concern"))) t))
        (should (string-search "Original remark" (niwa-take-test--labels)))
        (should (string-search "Current taken concern" (niwa-take-test--labels)))
        (should-not (string-search "Uninvited update" (niwa-take-test--labels)))))))

(provide 'test-niwa-buddy-take)
;;; test-niwa-buddy-take.el ends here
