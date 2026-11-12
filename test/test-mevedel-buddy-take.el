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

(provide 'test-mevedel-buddy-take)
;;; test-mevedel-buddy-take.el ends here
