;;; test-niwa-session-rewind-reverts.el --- Rewind recorded changes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise recorded changes through session Rewind with real history and files.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-session-rewind)
(require 'niwa-session-rewind-merge)
(require 'niwa-session-artifacts)
(require 'niwa-pipeline)
(require 'niwa-tool-patch)
(require 'niwa-note-merge)
(require 'niwa-report)

(defun niwa-test-rewind-reverts--call (turns current check &optional patch)
  "Record TURNS, install CURRENT bytes, and call CHECK in a real session.
Each turn is (BEFORE AFTER).  CHECK receives the buffer, target and source path.
With PATCH, run that ApplyPatch hunk through the real pipeline for capture."
  (pcase-let* ((`(,workspace . ,directory)
                (test-niwa-session-persistence--make-tempdir-workspace))
               (session (niwa-session-create "main" workspace))
               (buffer (generate-new-buffer " *rewind recorded changes*"))
               (path (file-name-concat directory "source.txt"))
               (other (file-name-concat directory "other.txt")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (setq-local niwa--session session)
          (setq-local niwa--workspace workspace)
          (setf (niwa-session-permission-mode session) 'full-auto
                (niwa-session-root-buffer session) buffer)
          (setq-local gptel-backend
                      (test-niwa-session-persistence--agent-backend))
          (setq-local gptel-model 'test-model)
          (gptel-mode 1)
          (niwa-session-artifacts-save session buffer)
          (cl-loop for (before after) in turns
                   for turn from 1 do
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region before nil path nil 'silent))
                   (niwa-request-begin session)
                   (if patch
                       (progn
                         (unless (niwa-tool-get "ApplyPatch")
                           (niwa-tool-patch-register))
                         (let (reply)
                           (niwa-pipeline-run-tool-outcome
                            (niwa-tool-get "ApplyPatch")
                            (lambda (value) (setq reply value))
                            (list :patch
                                  (format "*** Begin Patch\n*** Update File: %s\n%s\n*** End Patch"
                                          path patch)))
                           (let ((deadline (+ (float-time) 10)))
                             (while (and (not reply) (< (float-time) deadline))
                               (accept-process-output nil 0.05)))
                           (should (eq 'success (plist-get reply :status)))))
                     (puthash path before
                              (niwa-request-file-snapshots
                               niwa--current-request))
                     (let ((coding-system-for-write 'no-conversion))
                       (write-region after nil path nil 'silent)))
                   (puthash other (format "clean-%d\n" (1- turn))
                            (niwa-request-file-snapshots
                             niwa--current-request))
                   (write-region (format "clean-%d\n" turn) nil other nil 'silent)
                   (goto-char (point-max))
                   (insert (format "Prompt %d\n" turn))
                   (insert (propertize (format "Reply %d\n" turn)
                                       'gptel 'response))
                   (setf (niwa-session-turn-count session) turn)
                   (niwa-session-artifacts-save session buffer t)
                   (niwa-request-end))
          (let ((coding-system-for-write 'no-conversion))
            (write-region current nil path nil 'silent))
          (let ((target (copy-sequence
                         (cdr (cl-find-if
                               (lambda (entry)
                                 (= 1 (plist-get (cdr entry) :cum-turn)))
                               (niwa-session-rewind--prompt-candidates
                                session))))))
            (should target)
            (cl-letf (((symbol-function 'display-buffer) #'ignore))
              (funcall check buffer target path))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (delete-directory directory t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-session-rewind-rewind (:quiet t)
  ,test
  (test)
  :doc "reverts discarded turns newest first and preserves unrelated later edits"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
   "UNO\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (niwa-session-rewind-rewind buffer target)))
     (should (equal "one\ntwo\nthree\nuser\n"
                    (niwa-session-artifacts-read-file-raw path)))
     (with-current-buffer buffer
       (should-not (string-match-p "Prompt" (buffer-string))))))
  :doc "one Rewind reads each immutable backup only once across repeated planning"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n") ("ONE\ntwo\n" "UNO\ntwo\n"))
   "UNO\ntwo\n"
   (lambda (buffer target _path)
     (let* ((reads (make-hash-table :test #'equal))
            (read-artifact (symbol-function 'niwa-session-artifacts-read-artifact)))
       ;; Count logical backups: equal bytes may share one published blob.
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'niwa-session-artifacts-read-artifact)
                  (lambda (session logical &rest args)
                    (when (string-prefix-p "file-history/" logical)
                      (cl-incf (gethash logical reads 0)))
                    (apply read-artifact session logical args))))
         (should (niwa-session-rewind-rewind buffer target)))
       (should (> (hash-table-count reads) 0))
       (maphash (lambda (_name count) (should (= 1 count))) reads))))
  :doc "conflict cancellation preserves all files, unsaved text and conversation"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\n" "one\nTWO\nthree\n"))
   "one\nuser\nthree\n"
   (lambda (buffer target path)
     (let ((transcript (with-current-buffer buffer (buffer-string)))
           (source (find-file-noselect path))
           asked)
       (unwind-protect
           (progn
             (with-current-buffer source
               (goto-char (point-max))
               (insert "unsaved\n"))
             (cl-letf (((symbol-function 'read-char-choice)
                        (lambda (prompt _choices &rest _)
                          (setq asked prompt)
                          (let ((report (with-current-buffer
                                            "*niwa-rewind-impact*"
                                          (buffer-string))))
                            (should (string-match-p (regexp-quote path) report))
                            (should (string-match-p "lines 2-2" report))
                            (should (string-match-p "Captured files restored: 1" report)))
                          (with-current-buffer "*niwa-rewind-impact*"
                            (goto-char (point-min))
                            (re-search-forward "^conflict +")
                            (should-error
                             (call-interactively (key-binding (kbd "d")))
                             :type 'user-error))
                          ?c))
                       ((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) (ert-fail "Conflict was not refused"))))
               (should-not (niwa-session-rewind-rewind buffer target)))
             (should-not (get-buffer "*niwa-rewind-impact*"))
             (should (string-match-p (regexp-quote "[c]ancel") asked))
             (should (equal transcript
                            (with-current-buffer buffer (buffer-string))))
             (should (equal "one\nuser\nthree\n"
                            (niwa-session-artifacts-read-file-raw path)))
             (should (equal "clean-1\n"
                            (niwa-session-artifacts-read-file-raw
                             (file-name-concat (file-name-directory path)
                                               "other.txt"))))
             (with-current-buffer source
               (should (buffer-modified-p))
               (should (equal "one\nuser\nthree\nunsaved\n" (buffer-string)))))
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source)))))
  :doc "force restores snapshots only after a separate overwrite confirmation"
  (dolist (confirmed '(nil t))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\nthree\n" "one\nTWO\nthree\n"))
     "one\nuser\nthree\n"
     (lambda (buffer target path)
       (let ((transcript (with-current-buffer buffer (buffer-string)))
             confirmation)
         (cl-letf (((symbol-function 'read-char-choice)
                    (lambda (_prompt choices &rest _)
                      (should (memq ?f choices))
                      ?f))
                   ((symbol-function 'yes-or-no-p)
                    (lambda (prompt)
                      (setq confirmation prompt)
                      confirmed)))
           (should (eq confirmed
                       (niwa-session-rewind-rewind buffer target))))
         (should (string-match-p "Force snapshot restore" confirmation))
         (should (string-match-p "overwrite" confirmation))
         (should (equal (if confirmed "one\ntwo\nthree\n" "one\nuser\nthree\n")
                        (niwa-session-artifacts-read-file-raw path)))
         (should (equal (if confirmed "clean-0\n" "clean-1\n")
                        (niwa-session-artifacts-read-file-raw
                         (file-name-concat (file-name-directory path) "other.txt"))))
         (unless confirmed
           (should (equal transcript
                          (with-current-buffer buffer (buffer-string)))))))))
  :doc "saved buffer edits are re-planned as reversals, not snapshot restores"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n"))
   "ONE\ntwo\nthree\nfour\n"
   (lambda (buffer target path)
     (let ((source (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer source
               (goto-char (point-min))
               (search-forward "four")
               (replace-match "user"))
             (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?s))
                       ((symbol-function 'save-some-buffers)
                        (lambda (&rest _) (with-current-buffer source (save-buffer))))
                       ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
               (should (niwa-session-rewind-rewind buffer target)))
             (should (equal "one\ntwo\nthree\nuser\n"
                            (niwa-session-artifacts-read-file-raw path)))
             (with-current-buffer source
               (should-not (buffer-modified-p))
               (should (equal "one\ntwo\nthree\nuser\n" (buffer-string)))))
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source)))))
  :doc "keeps intervening edits between turns and shifted unrelated later lines"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nother-session\nfour\n" "UNO\ntwo\nother-session\nfour\n"))
   "UNO\ntwo\nother-session\nextra\nfour\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (niwa-session-rewind-rewind buffer target)))
     (should (equal "one\ntwo\nother-session\nextra\nfour\n"
                    (niwa-session-artifacts-read-file-raw path)))))
  :doc "after-response boundary keeps that turn and reverses only later turns"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
   "UNO\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (niwa-session-rewind-rewind buffer target 'after)))
     (should (equal "ONE\ntwo\nthree\nuser\n"
                    (niwa-session-artifacts-read-file-raw path)))
     (with-current-buffer buffer
       (should (string-match-p "Reply 1" (buffer-string)))
       (should-not (string-match-p "Prompt 2" (buffer-string))))))
  :doc "a file changed during confirmation aborts without reverting other files"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\n" "ONE\ntwo\nthree\n"))
   "ONE\ntwo\nthree\n"
   (lambda (buffer target path)
     (let ((transcript (with-current-buffer buffer (buffer-string))))
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (&rest _)
                    (write-region "concurrent\ntwo\nthree\n" nil path nil 'silent)
                    t)))
         (should-error (niwa-session-rewind-rewind buffer target)))
       (should (equal transcript (with-current-buffer buffer (buffer-string))))
       (should (equal "concurrent\ntwo\nthree\n"
                      (niwa-session-artifacts-read-file-raw path)))
       (should (equal "clean-1\n"
                      (niwa-session-artifacts-read-file-raw
                       (file-name-concat (file-name-directory path) "other.txt")))))))
  :doc "rollback restores only written files and preserves the edit refusing a later write"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let ((apply-action (symbol-function 'niwa-session-rewind--apply-restore-action))
           (transcript (with-current-buffer buffer (buffer-string))))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'niwa-session-rewind--apply-restore-action)
                  (lambda (session entry)
                    (prog1 (funcall apply-action session entry)
                      (unless (equal path (plist-get entry :path))
                        (write-region "concurrent\ntwo\n" nil path nil 'silent))))))
         (should-error (niwa-session-rewind-rewind buffer target)))
       (should (equal "concurrent\ntwo\n" (niwa-test--read-file path)))
       (should (equal "clean-1\n"
                      (niwa-test--read-file
                       (file-name-concat (file-name-directory path) "other.txt"))))
       (should (equal transcript (with-current-buffer buffer (buffer-string)))))))
  :doc "quit after a written file rolls that file back before abandoning Rewind"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let ((apply-action (symbol-function 'niwa-session-rewind--apply-restore-action))
           quit)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'niwa-session-rewind--apply-restore-action)
                  (lambda (session entry)
                    (funcall apply-action session entry)
                    (signal 'quit nil))))
         (condition-case nil
             (niwa-session-rewind-rewind buffer target)
           (quit (setq quit t))))
       (should quit)
       (should (equal "ONE\ntwo\n" (niwa-test--read-file path)))
       (should (equal "clean-1\n"
                      (niwa-test--read-file
                       (file-name-concat (file-name-directory path) "other.txt")))))))
  :doc "a failed results render still rolls all completed file writes back"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let ((render (symbol-function 'niwa-report-render)) injected)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'niwa-report-render)
                  (lambda (report &rest args)
                    (if (equal "Restore results" (plist-get report :title))
                        (progn (setq injected t) (error "Injected results render failure"))
                      (apply render report args)))))
         (should-error (niwa-session-rewind-rewind buffer target)))
       (should injected)
       (should (equal "ONE\ntwo\n" (niwa-test--read-file path)))
       (should (equal "clean-1\n"
                      (niwa-test--read-file
                       (file-name-concat (file-name-directory path) "other.txt")))))))
  :doc "literal pipeline captures preserve CRLF when reversing a real ApplyPatch"
  (niwa-test-rewind-reverts--call
   '(("one\r\ntwo\r\nthree\r\nfour\r\n" "ONE\r\ntwo\r\nthree\r\nfour\r\n"))
   "ONE\r\ntwo\r\nthree\r\nuser\r\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
               ((symbol-function 'read-char-choice)
                (lambda (&rest _) (ert-fail "Unrelated CRLF edit conflicts"))))
       (should (niwa-session-rewind-rewind buffer target)))
     (should (equal "one\r\ntwo\r\nthree\r\nuser\r\n"
                    (niwa-session-artifacts-read-file-raw path))))
   "@@\n-one\n+ONE")
  :doc "reports a gap in a later discarded turn even after an earlier capture"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\n" "ONE\ntwo\nthree\n")
     ("ONE\ntwo\nthree\n" "ONE\ntwo\nthree\n"))
   "ONE\ntwo\nthree\n"
   (lambda (buffer target path)
     ;; Represent a later failed capture of an already tracked path.
     (with-current-buffer buffer
       (push (cons path '(:gap "later capture failed" :version 2))
             (cdr (assq 2 (niwa-session-file-snapshots niwa--session)))))
     (cl-letf (((symbol-function 'yes-or-no-p)
                (lambda (&rest _)
                  (with-current-buffer "*niwa-rewind-impact*"
                    (should (string-match-p "later capture failed" (buffer-string))))
                  nil)))
       (should-not (niwa-session-rewind-rewind buffer target)))))
  :doc "an already undone hunk preserves unrelated user edits without a conflict"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n"))
   "one\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
               ((symbol-function 'read-char-choice)
                (lambda (&rest _) (ert-fail "Already undone hunk conflicts"))))
       (should (niwa-session-rewind-rewind buffer target)))
     (should (equal "one\ntwo\nthree\nuser\n"
                    (niwa-test--read-file path)))))
  :doc "a change already undone by the user needs no file write or force"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "one\ntwo\n"
   (lambda (buffer target path)
     (let ((mtime (file-attribute-modification-time (file-attributes path))))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'read-char-choice)
                  (lambda (&rest _) (ert-fail "Already undone change refused"))))
         (should (niwa-session-rewind-rewind buffer target)))
       (should (equal "one\ntwo\n" (niwa-session-artifacts-read-file-raw path)))
       (should (equal mtime (file-attribute-modification-time
                            (file-attributes path)))))))
  :doc "merge resolves overlaps without writing until the complete Rewind commits"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n"))
   "mine\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (let ((transcript (with-current-buffer buffer (buffer-string))))
       (save-window-excursion
         (cl-letf (((symbol-function 'read-char-choice)
                    (lambda (_prompt choices &rest _)
                      (should (memq ?m choices)) ?m))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (should-not (niwa-session-rewind-rewind buffer target))
           (should (derived-mode-p 'niwa-note-merge-mode))
           (should-not (get-buffer "*niwa-rewind-impact*"))
           (should (equal transcript (with-current-buffer buffer (buffer-string))))
           (should (equal "mine\ntwo\nthree\nuser\n"
                          (niwa-session-artifacts-read-file-raw path)))
           (should-error (niwa-note-merge-confirm) :type 'user-error)
           (with-current-buffer buffer
             (put-text-property (point-min) (point-max) 'fontified t))
           (goto-char (point-min))
           (search-forward "<<<<<<<")
           (smerge-keep-lower)
           (niwa-note-merge-confirm)
           (should (equal "one\ntwo\nthree\nuser\n"
                          (niwa-session-artifacts-read-file-raw path)))
           (should (equal "clean-0\n"
                          (niwa-session-artifacts-read-file-raw
                           (file-name-concat (file-name-directory path) "other.txt"))))
           (with-current-buffer buffer
             (should-not (string-match-p "Prompt" (buffer-string)))))))))
  :doc "keeping a deleted current file in a Rewind merge leaves it deleted"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (delete-file path)
     (save-window-excursion
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (niwa-session-rewind-rewind buffer target)
         (goto-char (point-min)) (search-forward "<<<<<<<")
         (smerge-keep-upper)
         (niwa-note-merge-confirm)
         (should-not (file-exists-p path))))))
  :doc "merge chains conflicting turns newest first and writes neither round early"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
   "mine\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (save-window-excursion
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (niwa-session-rewind-rewind buffer target)
         (goto-char (point-min)) (search-forward "<<<<<<<")
         (smerge-keep-upper)
         (niwa-note-merge-confirm)
         (should (derived-mode-p 'niwa-note-merge-mode))
         (should (equal "mine\ntwo\nthree\nuser\n" (niwa-test--read-file path)))
         (should (equal "clean-2\n" (niwa-test--read-file
                                      (file-name-concat (file-name-directory path) "other.txt"))))
         (goto-char (point-min)) (search-forward "<<<<<<<")
         (smerge-keep-lower)
         (niwa-note-merge-confirm)
         (should (equal "one\ntwo\nthree\nuser\n" (niwa-test--read-file path)))
         (should (equal "clean-0\n" (niwa-test--read-file
                                      (file-name-concat (file-name-directory path) "other.txt"))))))))
  :doc "each older conflict offers cancel, force or merge again"
  (dolist (choice '(?c ?f ?m))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
       ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
     "mine\ntwo\nthree\nuser\n"
     (lambda (buffer target path)
       (let ((rounds 0)
             (transcript (with-current-buffer buffer (buffer-string))))
         (save-window-excursion
           (cl-letf (((symbol-function 'read-char-choice)
                      (lambda (_prompt choices &rest _)
                        (should (equal '(?c ?f ?m) choices))
                        (if (= 1 (cl-incf rounds)) ?m choice)))
                     ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             (niwa-session-rewind-rewind buffer target)
             (goto-char (point-min))
             (search-forward "<<<<<<<")
             (smerge-keep-upper)
             (niwa-note-merge-confirm)
             (should (= 2 rounds))
             (should-not (get-buffer "*niwa-rewind-impact*"))
             (when (eq choice ?m)
               (should (derived-mode-p 'niwa-note-merge-mode))
               (goto-char (point-min))
               (search-forward "<<<<<<<")
               (smerge-keep-lower)
               (niwa-note-merge-confirm))
             (should (equal (pcase choice
                              (?c "mine\ntwo\nthree\nuser\n")
                              (?f "one\ntwo\nthree\nfour\n")
                              (?m "one\ntwo\nthree\nuser\n"))
                            (niwa-test--read-file path)))
             (when (eq choice ?c)
               (should (equal transcript (with-current-buffer buffer (buffer-string)))))))))))
  :doc "merge starts from the intermediate text after a newer clean reversal"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("mine\ntwo\nthree\nfour\n" "mine\ntwo\nTHREE\nfour\n"))
   "mine\ntwo\nTHREE\nuser\n"
   (lambda (buffer target path)
     (save-window-excursion
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (niwa-session-rewind-rewind buffer target)
         (should-not (string-search "THREE" (buffer-string)))
         (goto-char (point-min)) (search-forward "<<<<<<<")
         (smerge-keep-lower)
         (niwa-note-merge-confirm)
         (should (equal "one\ntwo\nthree\nuser\n" (niwa-test--read-file path)))))))
  :doc "merge cancellation and source changes leave all files and history untouched"
  (dolist (action '(cancel source final))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\n" "ONE\ntwo\n")) "mine\ntwo\n"
     (lambda (buffer target path)
       (let ((transcript (with-current-buffer buffer (buffer-string))))
         (save-window-excursion
           (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                     ((symbol-function 'yes-or-no-p)
                      (lambda (&rest _)
                        (when (eq action 'final)
                          (write-region "later\ntwo\n" nil path nil 'silent))
                        t)))
             (niwa-session-rewind-rewind buffer target)
             (let ((merge (current-buffer)))
               (if (eq action 'cancel)
                   (niwa-note-merge-cancel)
                 (goto-char (point-min)) (search-forward "<<<<<<<")
                 (smerge-keep-lower)
                 (when (eq action 'source)
                   (write-region "later\ntwo\n" nil path nil 'silent))
                 (should-error (niwa-note-merge-confirm))
                 (when (buffer-live-p merge)
                   (with-current-buffer merge (niwa-note-merge-cancel))))
               (should (equal (if (eq action 'cancel) "mine\ntwo\n" "later\ntwo\n")
                              (niwa-test--read-file path)))
               (should (equal "clean-1\n" (niwa-test--read-file
                                            (file-name-concat (file-name-directory path) "other.txt"))))
               (should (equal transcript (with-current-buffer buffer (buffer-string)))))))))))
  :doc "a Rewind merge with two conflicting files resolves each against its own entry"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "mine\ntwo\n"
   (lambda (buffer target path)
     (let ((other (file-name-concat (file-name-directory path) "other.txt")))
       (write-region "mine-other\n" nil other nil 'silent)
       (save-window-excursion
         (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (niwa-session-rewind-rewind buffer target)
           (goto-char (point-min))
           (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
           (niwa-note-merge-confirm)
           (should (equal "one\ntwo\n" (niwa-test--read-file path)))
           (should (equal "clean-0\n" (niwa-test--read-file other)))))))))

(niwa-deftest niwa-note-merge-open (:quiet t)
  ,test
  (test)
  :doc "COMMIT pairs each change with its own section's data across several turns"
  (let* ((dir (make-temp-file "niwa-merge-pairs-" t))
         (turns '(3 1 2))
         (paths (mapcar (lambda (turn) (file-name-concat dir (format "f%d.txt" turn))) turns))
         merge got)
    (unwind-protect
        (save-window-excursion
          (cl-mapc (lambda (path turn)
                     (write-region (format "mine%d\n" turn) nil path nil 'silent))
                   paths turns)
          (setq merge
                (niwa-note-merge-open
                 "*pairs*"
                 (cl-mapcar (lambda (path turn)
                              (list :path path :disk (niwa-tool-patch--snapshot path)
                                    :coding 'utf-8-unix :yours (format "mine%d\n" turn)
                                    :seen "base\n" :proposed (format "ai%d\n" turn)
                                    :data (list :turn turn)))
                            paths turns)
                 (lambda (pairs) (setq got pairs))))
          (goto-char (point-min))
          (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
          (niwa-note-merge-confirm))
      (when (buffer-live-p merge)
        (with-current-buffer merge (set-buffer-modified-p nil))
        (kill-buffer merge))
      (delete-directory dir t))
    (should (equal turns (mapcar (lambda (pair) (plist-get (cdr pair) :turn)) got)))
    (should (equal paths (mapcar (lambda (pair) (plist-get (car pair) :path)) got)))
    (should (equal '("ai3\n" "ai1\n" "ai2\n")
                   (mapcar (lambda (pair) (plist-get (car pair) :content)) got)))
    ;; Negative control: a mispairing (sorted turns) must not equal the real result.
    (should-not (equal (sort (copy-sequence turns) #'<)
                       (mapcar (lambda (pair) (plist-get (cdr pair) :turn)) got)))))

(provide 'test-niwa-session-rewind-reverts)
;;; test-niwa-session-rewind-reverts.el ends here
