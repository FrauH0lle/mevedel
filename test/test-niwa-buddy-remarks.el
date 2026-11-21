;;; test-niwa-buddy-remarks.el -- Durable Buddy review tests -*- lexical-binding: t -*-

;;; Commentary:

;; Drive Buddy commands with canned provider replies in real workspace files.

;;; Code:

(require 'niwa)
(require 'niwa-buddy)
(require 'niwa-buddy-note)
(require 'niwa-persistence)
(require 'niwa-session-control-fs)
(require 'gptel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro niwa-remarks-test--with-source (&rest body)
  "Run BODY in a private, file-backed workspace."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "niwa-remarks-" t))
          (file (file-name-concat directory "source.el"))
          (workspace (niwa-workspace--create
                      :type 'file :id directory :root directory :name "remarks"))
          (niwa--instruction-states (make-hash-table :test #'equal))
          (niwa-instruction-store--dirty nil)
          (niwa-instruction-store--failed nil)
          (niwa-buddy-note--notes nil)
          (niwa-buddy-note--run nil)
          (niwa-note-labels 'expanded)
          (gptel--known-backends nil)
          (niwa-model-workloads '((buddy :provider "BuddyTest:test")))
          source)
     (unwind-protect
         (progn
           (gptel-make-openai "BuddyTest" :key "test" :models '(test))
           (make-directory (file-name-concat directory ".niwa"))
           (write-region "before\nselected code\nafter\n" nil file nil 'silent)
           (setq source (find-file-noselect file))
           (with-current-buffer source
             (setq-local niwa--workspace workspace)
             (goto-char (point-min))
             ,@body))
       (when niwa-buddy--running (niwa-buddy-abort))
       (niwa-buddy-note-release-markers)
       (when (buffer-live-p source)
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source))
       (setq niwa-instruction-store--dirty nil)
       (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
       (delete-directory directory t))))

(defun niwa-remarks-test--review (answer &optional command)
  "Run COMMAND with ANSWER supplying tool calls at the provider boundary.
COMMAND defaults to guidance; it may also be the recorded-edit review command."
  (let (failure)
    (cl-letf (((symbol-function 'gptel-request)
             (lambda (prompt &rest keys)
               (with-current-buffer (plist-get keys :buffer)
                 (condition-case err
                     (funcall answer prompt gptel-tools)
                   (error (setq failure err))))
               (let ((fsm (plist-get keys :fsm)))
                 (funcall (car (last (cdr (assq 'DONE (gptel-fsm-handlers fsm)))))
                          fsm)))))
      (funcall (or command #'niwa-buddy-guide))
      (with-timeout (5 (ert-fail "Buddy review did not settle"))
        (while niwa-buddy--running (accept-process-output nil 0.01))))
    (when failure (signal (car failure) (cdr failure)))
    (should-not niwa-buddy--running)))

(defun niwa-remarks-test--tool (tools name &rest args)
  "Call the model-facing tool NAME from TOOLS with ARGS."
  (apply (gptel-tool-function
          (seq-find (lambda (tool) (equal name (gptel-tool-name tool))) tools))
         args))

(defun niwa-remarks-test--labels ()
  "Return visible note labels in the source buffer."
  (string-join
   (delq nil (mapcar (lambda (overlay) (overlay-get overlay 'after-string))
                    (overlays-in (point-min) (point-max))))))

(niwa-deftest niwa-buddy-remarks
  (:quiet t)
  ,test
  (test)
  :doc "review places a Buddy-authored note using the common label and actions"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)))
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (should (integerp (niwa-remarks-test--tool
                            tools "add_note" name 2 "Check this" "significant"))))))
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (should (string-match-p "┆ Note [0-9]+ · Buddy  Check this"
                            (niwa-remarks-test--labels)))
    (let ((label (niwa-remarks-test--labels)))
      (should (eq 'niwa-note-label-buddy
                  (get-text-property (string-search "┆" label) 'face label)))
      (should (eq 'default
                  (get-text-property (string-search "Check this" label) 'face label))))
    (goto-char (point-min))
    (forward-line 1)
    (cl-letf (((symbol-function 'read-key)
               (lambda (&rest _)
                 (let ((label (downcase (replace-regexp-in-string
                                         "[][]" "" (niwa-remarks-test--labels)))))
                   (should (string-search "edit" label))
                   (should (string-search "k dismiss" label))
                   (should (string-search "b keep" label)))
                 ?e)))
      (niwa-note-menu))
    (should (file-exists-p (niwa-instruction-store-path workspace))))
  :doc "a fresh review restores the remark and does not duplicate unchanged advice"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (niwa--clear-instruction-state workspace)
      (setq niwa-buddy-note--notes nil)
      (should (equal "" (niwa-remarks-test--labels)))
      (niwa-instruction-store-observe workspace)
      (niwa-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p (format "note_id %s \\[active\\]" id) prompt))
         (should (= id (niwa-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (= 1 (cl-count ?\n (niwa-remarks-test--labels))))))
  :doc "a later review retracts a fixed concern from labels and the durable store"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (goto-char (point-min))
      (forward-line 1)
      (insert "fixed ")
      (save-buffer)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (should (equal (format "Removed note %s" id)
                        (niwa-remarks-test--tool tools "remove_note" id)))))
      (should (equal "" (niwa-remarks-test--labels)))
      (should-not (niwa-notes-closed workspace t))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (should (equal "" (niwa-remarks-test--labels)))
      (should-not (niwa-notes-closed workspace t))
      (should (equal "before\nfixed selected code\nafter\n" (buffer-string)))))
  :doc "keep turns a remark into a durable user note beyond Buddy's ownership"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))
         (with-current-buffer source
           (goto-char (point-min)) (forward-line 1)
           (niwa-buddy-keep-note))
         (should (string-match-p "no longer owned"
                                 (niwa-remarks-test--tool tools "remove_note" id)))))
      (niwa-instruction-store-flush t)
      (niwa--clear-instruction-state workspace)
      (setq niwa-buddy-note--notes nil)
      (niwa-instruction-store-observe workspace)
      (niwa-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "\\[kept\\]" prompt))
         (should (string-match-p "no longer owned"
                                 (niwa-remarks-test--tool
                                  tools "update_note" id "Must not replace")))))
      (should (string-match-p "▍ Note [0-9]+ · You  Check this" (niwa-remarks-test--labels)))
      (should-not (string-match-p "Buddy\\|Must not replace"
                                  (niwa-remarks-test--labels)))))
  :doc "a multi-remark review writes once, including update and retraction"
  (niwa-remarks-test--with-source
    (let* ((name (buffer-name)) (writes 0)
          (count-write (lambda (&rest _) (cl-incf writes))))
      (advice-add 'niwa-instruction-store-sync :before count-write)
      (unwind-protect
          (progn
            (niwa-remarks-test--review
             (lambda (_prompt tools)
               (let (ids)
                 (dotimes (i 5)
                   (push (niwa-remarks-test--tool
                          tools "add_note" name 2 (format "Concern %s" i)
                          "significant") ids)
                   (run-hooks 'post-command-hook)
                   (should (= 0 writes)))
                 (niwa-remarks-test--tool tools "update_note" (car ids) "Updated")
                 (niwa-remarks-test--tool tools "remove_note" (cadr ids))
                 (run-hooks 'post-command-hook)
                 (should (= 0 writes)))))
            (should (= 1 writes))
            (niwa--clear-instruction-state workspace)
            (niwa-instruction-store-observe workspace)
            (should (= 4 (cl-count ?\n (niwa-remarks-test--labels))))
            (should (string-match-p "Updated" (niwa-remarks-test--labels)))
            (setq writes 0)
            (niwa-remarks-test--review (lambda (_prompt _tools)))
            (should (= 0 writes)))
        (advice-remove 'niwa-instruction-store-sync count-write))))
  :doc "single and all dismissal close only Buddy notes and survive a cold review"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (niwa-note-create "My own note")
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "First concern" "significant"))
         (niwa-remarks-test--tool tools "add_note" name 1 "Second concern" "critical")
         (niwa-remarks-test--tool tools "add_note" name 3 "Third concern" "trivial")))
      (goto-char (point-min)) (forward-line 1)
      (niwa-buddy-dismiss-note)
      (should-not (string-match-p "First concern" (niwa-remarks-test--labels)))
      (niwa-buddy-dismiss-notes)
      (should (string-match-p "My own note" (niwa-remarks-test--labels)))
      (should-not (string-match-p "Buddy" (niwa-remarks-test--labels)))
      (niwa-instruction-store-flush t)
      (should (= 3 (length (niwa-notes-closed workspace))))
      (dolist (entry (niwa-notes-closed workspace))
        (should (eq 'dismissed (plist-get (plist-get entry :instruction) :closed-reason))))
      (niwa--clear-instruction-state workspace)
      (setq niwa-buddy-note--notes nil)
      (niwa-instruction-store-observe workspace)
      (niwa-remarks-test--review
       (lambda (prompt tools)
         (should (= 3 (cl-count ?\[ prompt)))
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (= id (niwa-remarks-test--tool
                        tools "add_note" name 2 "First concern" "significant")))))
      (should-not (string-match-p "Buddy" (niwa-remarks-test--labels))))))

(niwa-deftest niwa-buddy-remarks/recheck
  (:quiet t)
  ,test
  (test)
  :doc "a restarted remark keeps its changed-source hint through the next review"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (goto-char (point-min)) (forward-line 1) (insert "fixed ")
      (save-buffer)
      (niwa--clear-instruction-state workspace)
      (setq niwa-buddy-note--notes nil)
      (niwa-instruction-store-observe workspace)
      (niwa-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "changed since this note" prompt))
         (niwa-remarks-test--tool tools "remove_note" id)))
      (should (equal "" (niwa-remarks-test--labels)))))
  :doc "repeat detection follows captured positions while source moves in flight"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (with-current-buffer source (goto-char (point-min)) (insert "new line\n"))
         (should (= id (niwa-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (= 1 (cl-count ?\n (niwa-remarks-test--labels))))))
  :doc "a recorded-edit review places remarks without changing code or focus"
  (niwa-remarks-test--with-source
    (niwa-buddy-mode 1)
    (goto-char (point-min)) (forward-line 1) (insert "edited ")
    (let ((name (buffer-name)) (code (buffer-string))
          (position (point)) (window (selected-window)) called)
      (niwa-remarks-test--review
       (lambda (prompt tools)
         (setq called t)
         (should (string-match-p "edited selected code" prompt))
         (niwa-remarks-test--tool tools "add_note" name 2 "Check this" "significant"))
       #'niwa-buddy-review)
      (should called)
      (should (equal code (buffer-string)))
      (should (= position (point)))
      (should (eq window (selected-window)))
      (should (string-match-p "┆ Note [0-9]+ · Buddy  Check this" (niwa-remarks-test--labels))))))

(niwa-deftest niwa-buddy-remarks/dismiss-in-flight
  (:quiet t)
  ,test
  (test)
  :doc "dismissing after source moves cannot re-raise the remark in that review"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (with-current-buffer source
           (goto-char (point-min)) (insert "new line\n")
           (forward-line 1)
           (niwa-buddy-dismiss-note))
         (should (= id (niwa-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (equal "" (niwa-remarks-test--labels)))
      (niwa-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (= id (niwa-remarks-test--tool
                        tools "add_note" name 3 "Check this" "significant")))))
      (should (equal "" (niwa-remarks-test--labels))))))

(niwa-deftest niwa-buddy-remarks/unsaved
  (:quiet t)
  ,test
  (test)
  :doc "unsaved guidance remains usable and remembers dismissal only in memory"
  (niwa-remarks-test--with-source
    (set-visited-file-name nil t)
    (let ((name (buffer-name)) id)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (should (string-match-p "┆ Note [0-9]+ · Buddy  Check this" (niwa-remarks-test--labels)))
      (goto-char (point-min)) (forward-line 1)
      (niwa-buddy-dismiss-note)
      (should (equal "" (niwa-remarks-test--labels)))
      (should-not (niwa-notes-closed workspace t))
      (niwa-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (= id (niwa-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (equal "" (niwa-remarks-test--labels)))
      (should-not (file-exists-p (niwa-instruction-store-path workspace)))
      (should (equal "before\nselected code\nafter\n" (buffer-string))))))

(niwa-deftest niwa-buddy-remarks/index
  (:quiet t)
  ,test
  (test)
  :doc "a warm unchanged review indexes loaded notes without hashing the store"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)))
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (niwa-remarks-test--tool tools "add_note" name 2 "Check this" "significant"))))
    (let* ((path (niwa-instruction-store-path workspace))
           (hashes 0)
           (count-hash (lambda (target)
                         (when (equal path target) (cl-incf hashes)))))
      (advice-add 'niwa-session-control-fs-file-hash :before count-hash)
      (unwind-protect
          (progn
            (niwa-remarks-test--review
             (lambda (prompt _tools) (should (string-match-p "Check this" prompt))))
            (should (= 0 hashes)))
        (advice-remove 'niwa-session-control-fs-file-hash count-hash))))
  :doc "dismissed advice stays visible when active and kept remarks fill the limit"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name))
          (niwa-buddy-note-serialize-limit 2))
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (niwa-remarks-test--tool tools "add_note" name 1 "Rejected concern" "significant")
         (niwa-remarks-test--tool tools "add_note" name 2 "Kept concern" "significant")
         (niwa-remarks-test--tool tools "add_note" name 3 "Open concern" "significant")))
      (goto-char (point-min)) (niwa-buddy-dismiss-note)
      (forward-line 1) (niwa-buddy-keep-note)
      (niwa-remarks-test--review
       (lambda (prompt _tools)
         (should (string-match-p "\\[active\\].*Open concern" prompt))
         (should (string-match-p "\\[dismissed\\].*Rejected concern" prompt))
         (should (< (string-match "\\[active\\].*Open concern" prompt)
                    (string-match "\\[dismissed\\].*Rejected concern" prompt)))
         (should (= 2 (with-temp-buffer
                        (insert prompt) (how-many "^- note_id" (point-min) (point-max))))))))))

(niwa-deftest niwa-buddy-remarks/open-before-dismissed
  (:quiet t :doc "a full dismissed group cannot crowd open remarks out of the review")
  (niwa-remarks-test--with-source
    (let ((name (buffer-name))
          (niwa-buddy-note-serialize-limit 2))
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (niwa-remarks-test--tool tools "add_note" name 1 "Rejected one" "significant")
         (niwa-remarks-test--tool tools "add_note" name 2 "Rejected two" "significant")
         (niwa-remarks-test--tool tools "add_note" name 3 "Open concern" "significant")))
      (goto-char (point-min)) (niwa-buddy-dismiss-note)
      (forward-line 1) (niwa-buddy-dismiss-note)
      (forward-line 1) (insert "edited ") (save-buffer)
      (niwa-remarks-test--review
       (lambda (prompt _tools)
         (should (string-match-p "\\[active\\].*Open concern.*annotated line changed" prompt))
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (< (string-match "\\[active\\]" prompt)
                    (string-match "\\[dismissed\\]" prompt)))
         (should (= 2 (with-temp-buffer
                        (insert prompt) (how-many "^- note_id" (point-min) (point-max))))))))))

(niwa-deftest niwa-buddy-remarks/dismissed-moving-line
  (:quiet t)
  ,test
  (test)
  :doc "dismissed advice stays dismissed after lines are inserted above it"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (goto-char (point-min)) (forward-line 1)
      (niwa-buddy-dismiss-note)
      (goto-char (point-min)) (insert "new line\n")
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (should (= id (niwa-remarks-test--tool
                        tools "add_note" name 3 "Check this" "significant")))))
      (should (equal "" (niwa-remarks-test--labels))))))

(niwa-deftest niwa-buddy-remarks/file-alias
  (:quiet t)
  ,test
  (test)
  :doc "a reopened file alias still receives its stored dismissed advice"
  (niwa-remarks-test--with-source
    (let ((name (buffer-name))
          (alias (file-name-concat directory "alias.el")) id)
      (make-symbolic-link file alias)
      (niwa-remarks-test--review
       (lambda (_prompt tools)
         (setq id (niwa-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (goto-char (point-min)) (forward-line 1)
      (niwa-buddy-dismiss-note)
      (niwa-instruction-store-flush t)
      (niwa--clear-instruction-state workspace)
      (setq niwa-buddy-note--notes nil)
      (set-visited-file-name alias t)
      (niwa-instruction-store-observe workspace)
      (setq name (buffer-name))
      (niwa-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (= id (niwa-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (equal "" (niwa-remarks-test--labels))))))

(niwa-deftest niwa-buddy-remarks/first-project-note
  (:quiet t)
  ,test
  (test)
  :doc "a project's first Buddy review creates its store and syncs the batch once"
  (niwa-remarks-test--with-source
    (should (zerop (process-file "git" nil nil nil "init" "-q" directory)))
    (delete-directory (file-name-concat directory ".niwa"))
    (let* ((name (buffer-name))
           (writes 0)
           (count-write (lambda (&rest _) (cl-incf writes))))
      (advice-add 'niwa-instruction-store-sync :before count-write)
      (unwind-protect
          (progn
            (niwa-remarks-test--review
             (lambda (_prompt tools)
               (dotimes (i 5)
                 (niwa-remarks-test--tool
                  tools "add_note" name 2 (format "Concern %s" i) "significant")
                 (run-hooks 'post-command-hook)
                 (should (= 0 writes)))))
            (should (= 1 writes))
            (setq workspace niwa--workspace)
            (should (file-exists-p (niwa-instruction-store-path workspace)))
            (niwa--clear-instruction-state workspace)
            (niwa-instruction-store-observe workspace)
            (should (= 5 (cl-count ?\n (niwa-remarks-test--labels))))
            (should (equal "before\nselected code\nafter\n" (buffer-string))))
        (advice-remove 'niwa-instruction-store-sync count-write)))))

(niwa-deftest niwa-buddy-remarks/cold-abort
  (:quiet t)
  ,test
  (test)
  :doc "abort releases a cold review's preparation slot before note tools load"
  (let* ((directory (make-temp-file "niwa-buddy-cold-" t))
         (file (file-name-concat directory "source.el")))
    (unwind-protect
        (with-temp-buffer
          (let ((process
                 (make-process
                  :name "buddy-cold-abort" :buffer (current-buffer) :noquery t
                  :connection-type 'pipe :sentinel #'ignore
                  :command
                  (list
                   (expand-file-name invocation-name invocation-directory)
                   "--batch" "-Q" "--eval"
                   (prin1-to-string
                    `(progn
                       (setq load-path ',load-path native-comp-jit-compilation nil)
                       (require 'niwa-buddy)
                       (write-region "before\nselected code\nafter\n"
                                     nil ,file nil 'silent)
                       (with-current-buffer (find-file-noselect ,file)
                         (niwa-buddy-mode 1)
                         (goto-char (point-min)) (insert "edited ")
                         (when (featurep 'niwa-buddy-note)
                           (error "Probe lost its cold first-use condition"))
                         (unless (niwa-buddy-review)
                           (error "Review was not scheduled"))
                         (niwa-buddy-abort)
                         (when niwa-buddy--running
                           (error "Abort did not release review slot"))
                         (set-buffer-modified-p nil))))))))
            (with-timeout (10 (delete-process process)
                              (ert-fail "Cold Buddy abort timed out"))
              (while (process-live-p process) (accept-process-output process 0.05)))
            (accept-process-output process 0)
            (ert-info ((buffer-string))
              (should (= 0 (process-exit-status process))))))
      (delete-directory directory t))))

(provide 'test-niwa-buddy-remarks)
;;; test-niwa-buddy-remarks.el ends here
