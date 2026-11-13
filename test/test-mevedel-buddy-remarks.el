;;; test-mevedel-buddy-remarks.el -- Durable Buddy review tests -*- lexical-binding: t -*-

;;; Commentary:

;; Drive Buddy commands with canned provider replies in real workspace files.

;;; Code:

(require 'mevedel)
(require 'mevedel-buddy)
(require 'mevedel-buddy-note)
(require 'mevedel-persistence)
(require 'mevedel-session-control-fs)
(require 'gptel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-remarks-test--with-source (&rest body)
  "Run BODY in a private, file-backed workspace."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "mevedel-remarks-" t))
          (file (file-name-concat directory "source.el"))
          (workspace (mevedel-workspace--create
                      :type 'file :id directory :root directory :name "remarks"))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          (mevedel-buddy-note--notes nil)
          (mevedel-buddy-note--run nil)
          (mevedel-note-labels 'expanded)
          (gptel--known-backends nil)
          (mevedel-model-workloads '((buddy :provider "BuddyTest:test")))
          source)
     (unwind-protect
         (progn
           (gptel-make-openai "BuddyTest" :key "test" :models '(test))
           (make-directory (file-name-concat directory ".mevedel"))
           (write-region "before\nselected code\nafter\n" nil file nil 'silent)
           (setq source (find-file-noselect file))
           (with-current-buffer source
             (setq-local mevedel--workspace workspace)
             (goto-char (point-min))
             ,@body))
       (when mevedel-buddy--running (mevedel-buddy-abort))
       (mevedel-buddy-note-release-markers)
       (when (buffer-live-p source)
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source))
       (setq mevedel-instruction-store--dirty nil)
       (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
       (delete-directory directory t))))

(defun mevedel-remarks-test--review (answer &optional command)
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
      (funcall (or command #'mevedel-buddy-guide))
      (with-timeout (5 (ert-fail "Buddy review did not settle"))
        (while mevedel-buddy--running (accept-process-output nil 0.01))))
    (when failure (signal (car failure) (cdr failure)))
    (should-not mevedel-buddy--running)))

(defun mevedel-remarks-test--tool (tools name &rest args)
  "Call the model-facing tool NAME from TOOLS with ARGS."
  (apply (gptel-tool-function
          (seq-find (lambda (tool) (equal name (gptel-tool-name tool))) tools))
         args))

(defun mevedel-remarks-test--labels ()
  "Return visible note labels in the source buffer."
  (string-join
   (delq nil (mapcar (lambda (overlay) (overlay-get overlay 'after-string))
                    (overlays-in (point-min) (point-max))))))

(mevedel-deftest mevedel-buddy-remarks
  (:quiet t)
  ,test
  (test)
  :doc "review places a Buddy-authored note using the common label and actions"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)))
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (should (integerp (mevedel-remarks-test--tool
                            tools "add_note" name 2 "Check this" "significant"))))))
    (should (equal "before\nselected code\nafter\n" (buffer-string)))
    (should (string-match-p "┆ Note [0-9]+ · Buddy  Check this"
                            (mevedel-remarks-test--labels)))
    (let ((label (mevedel-remarks-test--labels)))
      (should (eq 'mevedel-note-label-buddy
                  (get-text-property (string-search "┆" label) 'face label)))
      (should (eq 'default
                  (get-text-property (string-search "Check this" label) 'face label))))
    (goto-char (point-min))
    (forward-line 1)
    (cl-letf (((symbol-function 'read-key)
               (lambda (&rest _)
                 (let ((label (downcase (replace-regexp-in-string
                                         "[][]" "" (mevedel-remarks-test--labels)))))
                   (should (string-search "edit" label))
                   (should (string-search "k dismiss" label))
                   (should (string-search "b keep" label)))
                 ?e)))
      (mevedel-note-menu))
    (should (file-exists-p (mevedel-instruction-store-path workspace))))
  :doc "a fresh review restores the remark and does not duplicate unchanged advice"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (mevedel--clear-instruction-state workspace)
      (setq mevedel-buddy-note--notes nil)
      (should (equal "" (mevedel-remarks-test--labels)))
      (mevedel-instruction-store-observe workspace)
      (mevedel-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p (format "note_id %s \\[active\\]" id) prompt))
         (should (= id (mevedel-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (= 1 (cl-count ?\n (mevedel-remarks-test--labels))))))
  :doc "a later review retracts a fixed concern from labels and the durable store"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (goto-char (point-min))
      (forward-line 1)
      (insert "fixed ")
      (save-buffer)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (should (equal (format "Removed note %s" id)
                        (mevedel-remarks-test--tool tools "remove_note" id)))))
      (should (equal "" (mevedel-remarks-test--labels)))
      (should-not (mevedel-notes-closed workspace t))
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (should (equal "" (mevedel-remarks-test--labels)))
      (should-not (mevedel-notes-closed workspace t))
      (should (equal "before\nfixed selected code\nafter\n" (buffer-string)))))
  :doc "keep turns a remark into a durable user note beyond Buddy's ownership"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))
         (with-current-buffer source
           (goto-char (point-min)) (forward-line 1)
           (mevedel-buddy-keep-note))
         (should (string-match-p "no longer owned"
                                 (mevedel-remarks-test--tool tools "remove_note" id)))))
      (mevedel-instruction-store-flush t)
      (mevedel--clear-instruction-state workspace)
      (setq mevedel-buddy-note--notes nil)
      (mevedel-instruction-store-observe workspace)
      (mevedel-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "\\[kept\\]" prompt))
         (should (string-match-p "no longer owned"
                                 (mevedel-remarks-test--tool
                                  tools "update_note" id "Must not replace")))))
      (should (string-match-p "▍ Note [0-9]+ · You  Check this" (mevedel-remarks-test--labels)))
      (should-not (string-match-p "Buddy\\|Must not replace"
                                  (mevedel-remarks-test--labels)))))
  :doc "a multi-remark review writes once, including update and retraction"
  (mevedel-remarks-test--with-source
    (let* ((name (buffer-name)) (writes 0)
          (count-write (lambda (&rest _) (cl-incf writes))))
      (advice-add 'mevedel-instruction-store-sync :before count-write)
      (unwind-protect
          (progn
            (mevedel-remarks-test--review
             (lambda (_prompt tools)
               (let (ids)
                 (dotimes (i 5)
                   (push (mevedel-remarks-test--tool
                          tools "add_note" name 2 (format "Concern %s" i)
                          "significant") ids)
                   (run-hooks 'post-command-hook)
                   (should (= 0 writes)))
                 (mevedel-remarks-test--tool tools "update_note" (car ids) "Updated")
                 (mevedel-remarks-test--tool tools "remove_note" (cadr ids))
                 (run-hooks 'post-command-hook)
                 (should (= 0 writes)))))
            (should (= 1 writes))
            (mevedel--clear-instruction-state workspace)
            (mevedel-instruction-store-observe workspace)
            (should (= 4 (cl-count ?\n (mevedel-remarks-test--labels))))
            (should (string-match-p "Updated" (mevedel-remarks-test--labels)))
            (setq writes 0)
            (mevedel-remarks-test--review (lambda (_prompt _tools)))
            (should (= 0 writes)))
        (advice-remove 'mevedel-instruction-store-sync count-write))))
  :doc "single and all dismissal close only Buddy notes and survive a cold review"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (mevedel-note-create "My own note")
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "First concern" "significant"))
         (mevedel-remarks-test--tool tools "add_note" name 1 "Second concern" "critical")
         (mevedel-remarks-test--tool tools "add_note" name 3 "Third concern" "trivial")))
      (goto-char (point-min)) (forward-line 1)
      (mevedel-buddy-dismiss-note)
      (should-not (string-match-p "First concern" (mevedel-remarks-test--labels)))
      (mevedel-buddy-dismiss-notes)
      (should (string-match-p "My own note" (mevedel-remarks-test--labels)))
      (should-not (string-match-p "Buddy" (mevedel-remarks-test--labels)))
      (mevedel-instruction-store-flush t)
      (should (= 3 (length (mevedel-notes-closed workspace))))
      (dolist (entry (mevedel-notes-closed workspace))
        (should (eq 'dismissed (plist-get (plist-get entry :instruction) :closed-reason))))
      (mevedel--clear-instruction-state workspace)
      (setq mevedel-buddy-note--notes nil)
      (mevedel-instruction-store-observe workspace)
      (mevedel-remarks-test--review
       (lambda (prompt tools)
         (should (= 3 (cl-count ?\[ prompt)))
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (= id (mevedel-remarks-test--tool
                        tools "add_note" name 2 "First concern" "significant")))))
      (should-not (string-match-p "Buddy" (mevedel-remarks-test--labels))))))

(mevedel-deftest mevedel-buddy-remarks/recheck
  (:quiet t)
  ,test
  (test)
  :doc "a restarted remark keeps its changed-source hint through the next review"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (goto-char (point-min)) (forward-line 1) (insert "fixed ")
      (save-buffer)
      (mevedel--clear-instruction-state workspace)
      (setq mevedel-buddy-note--notes nil)
      (mevedel-instruction-store-observe workspace)
      (mevedel-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "changed since this note" prompt))
         (mevedel-remarks-test--tool tools "remove_note" id)))
      (should (equal "" (mevedel-remarks-test--labels)))))
  :doc "repeat detection follows captured positions while source moves in flight"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (with-current-buffer source (goto-char (point-min)) (insert "new line\n"))
         (should (= id (mevedel-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (= 1 (cl-count ?\n (mevedel-remarks-test--labels))))))
  :doc "a recorded-edit review places remarks without changing code or focus"
  (mevedel-remarks-test--with-source
    (mevedel-buddy-mode 1)
    (goto-char (point-min)) (forward-line 1) (insert "edited ")
    (let ((name (buffer-name)) (code (buffer-string))
          (position (point)) (window (selected-window)) called)
      (mevedel-remarks-test--review
       (lambda (prompt tools)
         (setq called t)
         (should (string-match-p "edited selected code" prompt))
         (mevedel-remarks-test--tool tools "add_note" name 2 "Check this" "significant"))
       #'mevedel-buddy-review)
      (should called)
      (should (equal code (buffer-string)))
      (should (= position (point)))
      (should (eq window (selected-window)))
      (should (string-match-p "┆ Note [0-9]+ · Buddy  Check this" (mevedel-remarks-test--labels))))))

(mevedel-deftest mevedel-buddy-remarks/dismiss-in-flight
  (:quiet t)
  ,test
  (test)
  :doc "dismissing after source moves cannot re-raise the remark in that review"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (with-current-buffer source
           (goto-char (point-min)) (insert "new line\n")
           (forward-line 1)
           (mevedel-buddy-dismiss-note))
         (should (= id (mevedel-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (equal "" (mevedel-remarks-test--labels)))
      (mevedel-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (= id (mevedel-remarks-test--tool
                        tools "add_note" name 3 "Check this" "significant")))))
      (should (equal "" (mevedel-remarks-test--labels))))))

(mevedel-deftest mevedel-buddy-remarks/unsaved
  (:quiet t)
  ,test
  (test)
  :doc "unsaved guidance remains usable and remembers dismissal only in memory"
  (mevedel-remarks-test--with-source
    (set-visited-file-name nil t)
    (let ((name (buffer-name)) id)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (should (string-match-p "┆ Note [0-9]+ · Buddy  Check this" (mevedel-remarks-test--labels)))
      (goto-char (point-min)) (forward-line 1)
      (mevedel-buddy-dismiss-note)
      (should (equal "" (mevedel-remarks-test--labels)))
      (should-not (mevedel-notes-closed workspace t))
      (mevedel-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (= id (mevedel-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (equal "" (mevedel-remarks-test--labels)))
      (should-not (file-exists-p (mevedel-instruction-store-path workspace)))
      (should (equal "before\nselected code\nafter\n" (buffer-string))))))

(mevedel-deftest mevedel-buddy-remarks/index
  (:quiet t)
  ,test
  (test)
  :doc "a warm unchanged review indexes loaded notes without hashing the store"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)))
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (mevedel-remarks-test--tool tools "add_note" name 2 "Check this" "significant"))))
    (let* ((path (mevedel-instruction-store-path workspace))
           (hashes 0)
           (count-hash (lambda (target)
                         (when (equal path target) (cl-incf hashes)))))
      (advice-add 'mevedel-session-control-fs-file-hash :before count-hash)
      (unwind-protect
          (progn
            (mevedel-remarks-test--review
             (lambda (prompt _tools) (should (string-match-p "Check this" prompt))))
            (should (= 0 hashes)))
        (advice-remove 'mevedel-session-control-fs-file-hash count-hash))))
  :doc "dismissed advice stays visible when active and kept remarks fill the limit"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name))
          (mevedel-buddy-note-serialize-limit 2))
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (mevedel-remarks-test--tool tools "add_note" name 1 "Rejected concern" "significant")
         (mevedel-remarks-test--tool tools "add_note" name 2 "Kept concern" "significant")
         (mevedel-remarks-test--tool tools "add_note" name 3 "Open concern" "significant")))
      (goto-char (point-min)) (mevedel-buddy-dismiss-note)
      (forward-line 1) (mevedel-buddy-keep-note)
      (mevedel-remarks-test--review
       (lambda (prompt _tools)
         (should (string-match-p "\\[active\\].*Open concern" prompt))
         (should (string-match-p "\\[dismissed\\].*Rejected concern" prompt))
         (should (< (string-match "\\[active\\].*Open concern" prompt)
                    (string-match "\\[dismissed\\].*Rejected concern" prompt)))
         (should (= 2 (with-temp-buffer
                        (insert prompt) (how-many "^- note_id" (point-min) (point-max))))))))))

(mevedel-deftest mevedel-buddy-remarks/open-before-dismissed
  (:quiet t :doc "a full dismissed group cannot crowd open remarks out of the review")
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name))
          (mevedel-buddy-note-serialize-limit 2))
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (mevedel-remarks-test--tool tools "add_note" name 1 "Rejected one" "significant")
         (mevedel-remarks-test--tool tools "add_note" name 2 "Rejected two" "significant")
         (mevedel-remarks-test--tool tools "add_note" name 3 "Open concern" "significant")))
      (goto-char (point-min)) (mevedel-buddy-dismiss-note)
      (forward-line 1) (mevedel-buddy-dismiss-note)
      (forward-line 1) (insert "edited ") (save-buffer)
      (mevedel-remarks-test--review
       (lambda (prompt _tools)
         (should (string-match-p "\\[active\\].*Open concern.*annotated line changed" prompt))
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (< (string-match "\\[active\\]" prompt)
                    (string-match "\\[dismissed\\]" prompt)))
         (should (= 2 (with-temp-buffer
                        (insert prompt) (how-many "^- note_id" (point-min) (point-max))))))))))

(mevedel-deftest mevedel-buddy-remarks/dismissed-moving-line
  (:quiet t)
  ,test
  (test)
  :doc "dismissed advice stays dismissed after lines are inserted above it"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name)) id)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (goto-char (point-min)) (forward-line 1)
      (mevedel-buddy-dismiss-note)
      (goto-char (point-min)) (insert "new line\n")
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (should (= id (mevedel-remarks-test--tool
                        tools "add_note" name 3 "Check this" "significant")))))
      (should (equal "" (mevedel-remarks-test--labels))))))

(mevedel-deftest mevedel-buddy-remarks/file-alias
  (:quiet t)
  ,test
  (test)
  :doc "a reopened file alias still receives its stored dismissed advice"
  (mevedel-remarks-test--with-source
    (let ((name (buffer-name))
          (alias (file-name-concat directory "alias.el")) id)
      (make-symbolic-link file alias)
      (mevedel-remarks-test--review
       (lambda (_prompt tools)
         (setq id (mevedel-remarks-test--tool
                   tools "add_note" name 2 "Check this" "significant"))))
      (goto-char (point-min)) (forward-line 1)
      (mevedel-buddy-dismiss-note)
      (mevedel-instruction-store-flush t)
      (mevedel--clear-instruction-state workspace)
      (setq mevedel-buddy-note--notes nil)
      (set-visited-file-name alias t)
      (mevedel-instruction-store-observe workspace)
      (setq name (buffer-name))
      (mevedel-remarks-test--review
       (lambda (prompt tools)
         (should (string-match-p "\\[dismissed\\]" prompt))
         (should (= id (mevedel-remarks-test--tool
                        tools "add_note" name 2 "Check this" "significant")))))
      (should (equal "" (mevedel-remarks-test--labels))))))

(mevedel-deftest mevedel-buddy-remarks/first-project-note
  (:quiet t)
  ,test
  (test)
  :doc "a project's first Buddy review creates its store and syncs the batch once"
  (mevedel-remarks-test--with-source
    (should (zerop (process-file "git" nil nil nil "init" "-q" directory)))
    (delete-directory (file-name-concat directory ".mevedel"))
    (let* ((name (buffer-name))
           (writes 0)
           (count-write (lambda (&rest _) (cl-incf writes))))
      (advice-add 'mevedel-instruction-store-sync :before count-write)
      (unwind-protect
          (progn
            (mevedel-remarks-test--review
             (lambda (_prompt tools)
               (dotimes (i 5)
                 (mevedel-remarks-test--tool
                  tools "add_note" name 2 (format "Concern %s" i) "significant")
                 (run-hooks 'post-command-hook)
                 (should (= 0 writes)))))
            (should (= 1 writes))
            (setq workspace mevedel--workspace)
            (should (file-exists-p (mevedel-instruction-store-path workspace)))
            (mevedel--clear-instruction-state workspace)
            (mevedel-instruction-store-observe workspace)
            (should (= 5 (cl-count ?\n (mevedel-remarks-test--labels))))
            (should (equal "before\nselected code\nafter\n" (buffer-string))))
        (advice-remove 'mevedel-instruction-store-sync count-write)))))

(mevedel-deftest mevedel-buddy-remarks/cold-abort
  (:quiet t)
  ,test
  (test)
  :doc "abort releases a cold review's preparation slot before note tools load"
  (let* ((directory (make-temp-file "mevedel-buddy-cold-" t))
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
                       (require 'mevedel-buddy)
                       (write-region "before\nselected code\nafter\n"
                                     nil ,file nil 'silent)
                       (with-current-buffer (find-file-noselect ,file)
                         (mevedel-buddy-mode 1)
                         (goto-char (point-min)) (insert "edited ")
                         (when (featurep 'mevedel-buddy-note)
                           (error "Probe lost its cold first-use condition"))
                         (unless (mevedel-buddy-review)
                           (error "Review was not scheduled"))
                         (mevedel-buddy-abort)
                         (when mevedel-buddy--running
                           (error "Abort did not release review slot"))
                         (set-buffer-modified-p nil))))))))
            (with-timeout (10 (delete-process process)
                              (ert-fail "Cold Buddy abort timed out"))
              (while (process-live-p process) (accept-process-output process 0.05)))
            (accept-process-output process 0)
            (ert-info ((buffer-string))
              (should (= 0 (process-exit-status process))))))
      (delete-directory directory t))))

(provide 'test-mevedel-buddy-remarks)
;;; test-mevedel-buddy-remarks.el ends here
