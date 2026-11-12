;;; test-mevedel-session-rewind-reverts.el --- Rewind recorded changes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise recorded changes through session Rewind with real history and files.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-session-rewind)
(require 'mevedel-pipeline)
(require 'mevedel-tool-patch)
(require 'mevedel-note-merge)

(defun mevedel-test-rewind-reverts--call (turns current check &optional patch)
  "Record TURNS, install CURRENT bytes, and call CHECK in a real session.
Each turn is (BEFORE AFTER).  CHECK receives the buffer, target and source path.
With PATCH, run that ApplyPatch hunk through the real pipeline for capture."
  (pcase-let* ((`(,workspace . ,directory)
                (test-mevedel-session-persistence--make-tempdir-workspace))
               (session (mevedel-session-create "main" workspace))
               (buffer (generate-new-buffer " *rewind recorded changes*"))
               (path (file-name-concat directory "source.txt"))
               (other (file-name-concat directory "other.txt")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (setq-local mevedel--session session)
          (setq-local mevedel--workspace workspace)
          (setf (mevedel-session-permission-mode session) 'full-auto
                (mevedel-session-root-buffer session) buffer)
          (setq-local gptel-backend
                      (test-mevedel-session-persistence--agent-backend))
          (setq-local gptel-model 'test-model)
          (gptel-mode 1)
          (mevedel-session-artifacts-save session buffer)
          (cl-loop for (before after) in turns
                   for turn from 1 do
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region before nil path nil 'silent))
                   (mevedel-request-begin session)
                   (if patch
                       (progn
                         (unless (mevedel-tool-get "ApplyPatch")
                           (mevedel-tool-patch-register))
                         (let (reply)
                           (mevedel-pipeline-run-tool-outcome
                            (mevedel-tool-get "ApplyPatch")
                            (lambda (value) (setq reply value))
                            (list :patch
                                  (format "*** Begin Patch\n*** Update File: %s\n%s\n*** End Patch"
                                          path patch)))
                           (let ((deadline (+ (float-time) 10)))
                             (while (and (not reply) (< (float-time) deadline))
                               (accept-process-output nil 0.05)))
                           (should (eq 'success (plist-get reply :status)))))
                     (puthash path before
                              (mevedel-request-file-snapshots
                               mevedel--current-request))
                     (let ((coding-system-for-write 'no-conversion))
                       (write-region after nil path nil 'silent)))
                   (puthash other (format "clean-%d\n" (1- turn))
                            (mevedel-request-file-snapshots
                             mevedel--current-request))
                   (write-region (format "clean-%d\n" turn) nil other nil 'silent)
                   (goto-char (point-max))
                   (insert (format "Prompt %d\n" turn))
                   (insert (propertize (format "Reply %d\n" turn)
                                       'gptel 'response))
                   (setf (mevedel-session-turn-count session) turn)
                   (mevedel-session-artifacts-save session buffer t)
                   (mevedel-request-end))
          (let ((coding-system-for-write 'no-conversion))
            (write-region current nil path nil 'silent))
          (let ((target (copy-sequence
                         (cdr (cl-find-if
                               (lambda (entry)
                                 (= 1 (plist-get (cdr entry) :cum-turn)))
                               (mevedel-session-rewind--prompt-candidates
                                session))))))
            (should target)
            (cl-letf (((symbol-function 'display-buffer) #'ignore))
              (funcall check buffer target path))))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (delete-directory directory t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-rewind-rewind (:quiet t)
  ,test
  (test)
  :doc "reverts discarded turns newest first and preserves unrelated later edits"
  (mevedel-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
   "UNO\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (mevedel-session-rewind-rewind buffer target)))
     (should (equal "one\ntwo\nthree\nuser\n"
                    (mevedel-session-artifacts-read-file-raw path)))
     (with-current-buffer buffer
       (should-not (string-match-p "Prompt" (buffer-string))))))
  :doc "conflict cancellation preserves all files, unsaved text and conversation"
  (mevedel-test-rewind-reverts--call
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
                                            "*mevedel-rewind-impact*"
                                          (buffer-string))))
                            (should (string-match-p (regexp-quote path) report))
                            (should (string-match-p "lines 2-2" report))
                            (should (string-match-p "Captured files restored: 1" report)))
                          (with-current-buffer "*mevedel-rewind-impact*"
                            (goto-char (point-min))
                            (re-search-forward "^conflict +")
                            (should-error
                             (call-interactively (key-binding (kbd "d")))
                             :type 'user-error))
                          ?c))
                       ((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) (ert-fail "Conflict was not refused"))))
               (should-not (mevedel-session-rewind-rewind buffer target)))
             (should (string-match-p (regexp-quote "[c]ancel") asked))
             (should (equal transcript
                            (with-current-buffer buffer (buffer-string))))
             (should (equal "one\nuser\nthree\n"
                            (mevedel-session-artifacts-read-file-raw path)))
             (should (equal "clean-1\n"
                            (mevedel-session-artifacts-read-file-raw
                             (file-name-concat (file-name-directory path)
                                               "other.txt"))))
             (with-current-buffer source
               (should (buffer-modified-p))
               (should (equal "one\nuser\nthree\nunsaved\n" (buffer-string)))))
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source)))))
  :doc "force restores snapshots only after a separate overwrite confirmation"
  (dolist (confirmed '(nil t))
    (mevedel-test-rewind-reverts--call
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
                       (mevedel-session-rewind-rewind buffer target))))
         (should (string-match-p "Force snapshot restore" confirmation))
         (should (string-match-p "overwrite" confirmation))
         (should (equal (if confirmed "one\ntwo\nthree\n" "one\nuser\nthree\n")
                        (mevedel-session-artifacts-read-file-raw path)))
         (should (equal (if confirmed "clean-0\n" "clean-1\n")
                        (mevedel-session-artifacts-read-file-raw
                         (file-name-concat (file-name-directory path) "other.txt"))))
         (unless confirmed
           (should (equal transcript
                          (with-current-buffer buffer (buffer-string)))))))))
  :doc "saved buffer edits are re-planned as reversals, not snapshot restores"
  (mevedel-test-rewind-reverts--call
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
               (should (mevedel-session-rewind-rewind buffer target)))
             (should (equal "one\ntwo\nthree\nuser\n"
                            (mevedel-session-artifacts-read-file-raw path)))
             (with-current-buffer source
               (should-not (buffer-modified-p))
               (should (equal "one\ntwo\nthree\nuser\n" (buffer-string)))))
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source)))))
  :doc "keeps intervening edits between turns and shifted unrelated later lines"
  (mevedel-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nother-session\nfour\n" "UNO\ntwo\nother-session\nfour\n"))
   "UNO\ntwo\nother-session\nextra\nfour\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (mevedel-session-rewind-rewind buffer target)))
     (should (equal "one\ntwo\nother-session\nextra\nfour\n"
                    (mevedel-session-artifacts-read-file-raw path)))))
  :doc "after-response boundary keeps that turn and reverses only later turns"
  (mevedel-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
   "UNO\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (mevedel-session-rewind-rewind buffer target 'after)))
     (should (equal "ONE\ntwo\nthree\nuser\n"
                    (mevedel-session-artifacts-read-file-raw path)))
     (with-current-buffer buffer
       (should (string-match-p "Reply 1" (buffer-string)))
       (should-not (string-match-p "Prompt 2" (buffer-string))))))
  :doc "a file changed during confirmation aborts without reverting other files"
  (mevedel-test-rewind-reverts--call
   '(("one\ntwo\nthree\n" "ONE\ntwo\nthree\n"))
   "ONE\ntwo\nthree\n"
   (lambda (buffer target path)
     (let ((transcript (with-current-buffer buffer (buffer-string))))
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (&rest _)
                    (write-region "concurrent\ntwo\nthree\n" nil path nil 'silent)
                    t)))
         (should-error (mevedel-session-rewind-rewind buffer target)))
       (should (equal transcript (with-current-buffer buffer (buffer-string))))
       (should (equal "concurrent\ntwo\nthree\n"
                      (mevedel-session-artifacts-read-file-raw path)))
       (should (equal "clean-1\n"
                      (mevedel-session-artifacts-read-file-raw
                       (file-name-concat (file-name-directory path) "other.txt")))))))
  :doc "literal pipeline captures preserve CRLF when reversing a real ApplyPatch"
  (mevedel-test-rewind-reverts--call
   '(("one\r\ntwo\r\nthree\r\nfour\r\n" "ONE\r\ntwo\r\nthree\r\nfour\r\n"))
   "ONE\r\ntwo\r\nthree\r\nuser\r\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
               ((symbol-function 'read-char-choice)
                (lambda (&rest _) (ert-fail "Unrelated CRLF edit conflicts"))))
       (should (mevedel-session-rewind-rewind buffer target)))
     (should (equal "one\r\ntwo\r\nthree\r\nuser\r\n"
                    (mevedel-session-artifacts-read-file-raw path))))
   "@@\n-one\n+ONE")
  :doc "reports a gap in a later discarded turn even after an earlier capture"
  (mevedel-test-rewind-reverts--call
   '(("one\ntwo\nthree\n" "ONE\ntwo\nthree\n")
     ("ONE\ntwo\nthree\n" "ONE\ntwo\nthree\n"))
   "ONE\ntwo\nthree\n"
   (lambda (buffer target path)
     ;; Represent a later failed capture of an already tracked path.
     (with-current-buffer buffer
       (push (cons path '(:gap "later capture failed" :version 2))
             (cdr (assq 2 (mevedel-session-file-snapshots mevedel--session)))))
     (cl-letf (((symbol-function 'yes-or-no-p)
                (lambda (&rest _)
                  (with-current-buffer "*mevedel-rewind-impact*"
                    (should (string-match-p "later capture failed" (buffer-string))))
                  nil)))
       (should-not (mevedel-session-rewind-rewind buffer target)))))
  :doc "a change already undone by the user needs no file write or force"
  (mevedel-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "one\ntwo\n"
   (lambda (buffer target path)
     (let ((mtime (file-attribute-modification-time (file-attributes path))))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'read-char-choice)
                  (lambda (&rest _) (ert-fail "Already undone change refused"))))
         (should (mevedel-session-rewind-rewind buffer target)))
       (should (equal "one\ntwo\n" (mevedel-session-artifacts-read-file-raw path)))
       (should (equal mtime (file-attribute-modification-time
                            (file-attributes path)))))))
  :doc "merge resolves overlaps without writing until the complete Rewind commits"
  (mevedel-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n"))
   "mine\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (let ((transcript (with-current-buffer buffer (buffer-string))))
       (save-window-excursion
         (cl-letf (((symbol-function 'read-char-choice)
                    (lambda (_prompt choices &rest _)
                      (should (memq ?m choices)) ?m))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (should-not (mevedel-session-rewind-rewind buffer target))
           (should (derived-mode-p 'mevedel-note-merge-mode))
           (should (equal transcript (with-current-buffer buffer (buffer-string))))
           (should (equal "mine\ntwo\nthree\nuser\n"
                          (mevedel-session-artifacts-read-file-raw path)))
           (should-error (mevedel-note-merge-confirm) :type 'user-error)
           (goto-char (point-min))
           (search-forward "<<<<<<<")
           (smerge-keep-lower)
           (mevedel-note-merge-confirm)
           (should (equal "one\ntwo\nthree\nuser\n"
                          (mevedel-session-artifacts-read-file-raw path)))
           (should (equal "clean-0\n"
                          (mevedel-session-artifacts-read-file-raw
                           (file-name-concat (file-name-directory path) "other.txt"))))
           (with-current-buffer buffer
             (should-not (string-match-p "Prompt" (buffer-string))))))))))

(provide 'test-mevedel-session-rewind-reverts)
;;; test-mevedel-session-rewind-reverts.el ends here
