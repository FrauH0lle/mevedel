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

(provide 'test-mevedel-note-change)
;;; test-mevedel-note-change.el ends here
