;;; test-mevedel-note-change.el -- Prepared note changes -*- lexical-binding: t -*-

;;; Commentary:

;; Public note actions and ApplyPatch in real note sessions and source files.

;;; Code:

(require 'mevedel)
(require 'mevedel-notes)
(require 'mevedel-note-conversation)
(require 'mevedel-tool-patch)
(require 'mevedel-view)
(require 'mevedel-session-persistence)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-note-change-test--with-note (&rest body)
  "Run BODY with a sent note in a real source file and session."
  (declare (indent 0))
  `(mevedel-view-test--with-buffers
     (let* ((root (make-temp-file "mevedel-note-change-" t))
            (file (file-name-concat root "source.txt"))
            (mevedel-sessions-directory (file-name-concat root "sessions"))
            (mevedel--instruction-states (make-hash-table :test #'equal))
            (mevedel-instruction-store--dirty nil)
            (mevedel-instruction-store--failed nil)
            (mevedel-workspace--registry (make-hash-table :test #'equal))
            (post-command-hook (copy-sequence post-command-hook))
            (workspace (mevedel-workspace-get-or-create 'file root root "changes"))
            (session (mevedel-session-create "changes" workspace root))
            (old-buffers (buffer-list))
            source note result)
       (unwind-protect
           (progn
             (make-directory (file-name-concat root ".mevedel"))
             (write-region "before\nold\nafter\n" nil file nil 'silent)
             (setq source (find-file-noselect file))
             (with-current-buffer source
               (setq-local mevedel--workspace workspace)
               (goto-char (point-min))
               (forward-line 1)
               (setq note (mevedel-note-create "Improve this"))
               (overlay-put note 'mevedel-note-state 'sent)
               (overlay-put note 'mevedel-note-session-id (mevedel-session-session-id session)))
             (setf (mevedel-session-preset-name session) 'mevedel-note)
             (with-current-buffer data-buf
               (setq-local default-directory (file-name-as-directory root)
                           mevedel--workspace workspace
                           mevedel--session session))
             ,@body)
         (dolist (buffer (cl-set-difference (buffer-list) old-buffers))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil)
               (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
         (mevedel-workspace-clear-registry)
         (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
         (delete-directory root t)))))

(defun mevedel-note-change-test--propose (data patch)
  "Invoke ApplyPatch in DATA with PATCH and return its immediate result."
  (let (result)
    (with-current-buffer data
      (mevedel-tool-patch-handler (lambda (value) (setq result value))
                                (list :patch patch)))
    result))

(mevedel-deftest mevedel-note-prepare-change
  (:quiet t)
  ,test
  (test)
  :doc "ApplyPatch returns a prepared change without writing or awaiting review"
  (mevedel-note-change-test--with-note
    (setq result (mevedel-note-change-test--propose
                  data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"))
    (should result)
    (should (string-search "Prepared" (plist-get result :result)))
    (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))
    (should (eq 'ready (mevedel-notes-attention-state note)))
    (should (string-search "change ready"
                           (overlay-get (overlay-get note 'mevedel-label-overlay) 'after-string)))))

(mevedel-deftest mevedel-note-apply-change
  (:quiet t)
  ,test
  (test)
  :doc "apply writes the prepared change and one undo restores the source"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (mevedel-note-apply-change note)
      (should (equal "before\nnew\nafter\n" (buffer-string)))
      (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file)))
      (should-not (eq 'closed (overlay-get note 'mevedel-note-state)))
      (should-not (eq 'ready (mevedel-notes-attention-state note)))
      (undo-only 1)
      (should (equal "before\nold\nafter\n" (buffer-string)))))
  :doc "editing noted code immediately marks the waiting change stale and refuses apply"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my ")
      (should (eq 'stale (mevedel-notes-attention-state note)))
      (should-error (mevedel-note-apply-change note) :type 'user-error)
      (should (equal "before\nmy old\nafter\n" (buffer-string)))
      (should (equal "before\nold\nafter\n" (mevedel-test--read-file file))))))

(mevedel-deftest mevedel-note-merge-change
  (:quiet t)
  ,test
  (test)
  :doc "merge shows three versions and confirms only resolved text into unchanged source"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my "))
    (let ((merge (mevedel-note-merge-change note)))
      (with-current-buffer merge
        (should smerge-mode)
        (should (string-search "<<<<<<< yours now" (buffer-string)))
        (should (string-search "||||||| what the AI saw" (buffer-string)))
        (should (string-search ">>>>>>> AI" (buffer-string)))
        (should-error (mevedel-note-merge-confirm) :type 'user-error)
        (goto-char (point-min))
        (search-forward "<<<<<<<")
        (smerge-keep-lower)
        (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))
        (mevedel-note-merge-confirm))
      (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file)))
      (with-current-buffer source
        (should (equal "before\nnew\nafter\n" (buffer-string)))))))

(provide 'test-mevedel-note-change)
;;; test-mevedel-note-change.el ends here
