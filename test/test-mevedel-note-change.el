;;; test-mevedel-note-change.el -- Prepared note changes -*- lexical-binding: t -*-

;;; Commentary:

;; Public note actions and ApplyPatch in real note sessions and source files.

;;; Code:

(require 'mevedel)
(require 'mevedel-notes)
(require 'mevedel-note-conversation)
(require 'mevedel-tool-patch)
(require 'mevedel-note-merge)
(require 'mevedel-patch-review)
(require 'mevedel-view)
(require 'mevedel-session-persistence)
(require 'mevedel-session-codec)
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
             (mevedel-session-artifacts-save session data-buf nil t)
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
        (should (equal "before\nnew\nafter\n" (buffer-string))))))
  :doc "multi-file merge regions never include another file's header or result"
  (mevedel-note-change-test--with-note
    (let ((two (file-name-concat root "two.txt")))
      (write-region "two old\n" nil two nil 'silent)
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** Update File: two.txt\n@@\n-two old\n+two new\n*** End Patch")
      (with-current-buffer (mevedel-note-merge-change note)
        (goto-char (point-min))
        (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
        (mevedel-note-merge-confirm))
      (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file)))
      (should (equal "two new\n" (mevedel-test--read-file two))))))

(mevedel-deftest mevedel-note-applied-history
  (:quiet t)
  ,test
  (test)
  :doc "apply durably records actual bytes under the preparing turn, preserving disjoint edits"
  (mevedel-note-change-test--with-note
    (setf (mevedel-session-turn-count session) 7)
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (setf (mevedel-session-turn-count session) 8)
    (with-current-buffer source
      (goto-char (point-max))
      (insert "user addition\n")
      (save-buffer))
    (mevedel-note-apply-change note)
    (should (equal "before\nnew\nafter\nuser addition\n" (mevedel-test--read-file file)))
    (let* ((saved (mevedel-session-codec-read
                   (mevedel-session-artifacts-sidecar-path (mevedel-session-save-path session))))
           (turns (plist-get saved :file-snapshots))
           (entry (cdr (assoc file (cdr (assq 7 turns))))))
      (should entry)
      (should-not (cdr (assq 8 turns)))
      (should (equal "before\nold\nafter\nuser addition\n"
                     (mevedel-test--read-file
                      (mevedel-session-artifacts-backup-path
                       (mevedel-session-save-path session) (plist-get entry :pre-backup-name)))))
      (should (equal "before\nnew\nafter\nuser addition\n"
                     (mevedel-test--read-file
                      (mevedel-session-artifacts-backup-path
                       (mevedel-session-save-path session) (plist-get entry :backup-name)))))))
  :doc "manual merge retains the actual unsaved source and confirmed result in file history"
  (mevedel-note-change-test--with-note
    (setf (mevedel-session-turn-count session) 3)
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my "))
    (with-current-buffer (mevedel-note-merge-change note)
      (goto-char (point-min))
      (search-forward "<<<<<<<")
      (smerge-keep-lower)
      (goto-char (point-min))
      (search-forward "new")
      (replace-match "manual resolution")
      (mevedel-note-merge-confirm))
    (let* ((saved (mevedel-session-codec-read
                   (mevedel-session-artifacts-sidecar-path (mevedel-session-save-path session))))
           (entry (cdr (assoc file (cdr (assq 3 (plist-get saved :file-snapshots)))))))
      (should entry)
      (should (equal "before\nmy old\nafter\n"
                     (mevedel-test--read-file
                      (mevedel-session-artifacts-backup-path
                       (mevedel-session-save-path session) (plist-get entry :pre-backup-name)))))
      (should (equal "before\nmanual resolution\nafter\n"
                     (mevedel-test--read-file
                      (mevedel-session-artifacts-backup-path
                       (mevedel-session-save-path session) (plist-get entry :backup-name))))))))

(mevedel-deftest mevedel-note-merge-creation
  (:quiet t)
  ,test
  (test)
  :doc "merging a new file retains absence, not an empty pre-existing file"
  (mevedel-note-change-test--with-note
    (let ((created (file-name-concat root "created.txt")))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Add File: created.txt\n+created\n*** End Patch")
      (with-current-buffer (mevedel-note-merge-change note)
        (goto-char (point-min))
        (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
        (mevedel-note-merge-confirm))
      (should (equal "created\n" (mevedel-test--read-file created)))
      (let* ((saved (mevedel-session-codec-read
                     (mevedel-session-artifacts-sidecar-path (mevedel-session-save-path session))))
             (entry (cdr (assoc created (cdr (assq 0 (plist-get saved :file-snapshots)))))))
        (should entry)
        (should-not (plist-get entry :pre-backup-name))))))

(mevedel-deftest mevedel-note-applied-history-gap
  (:quiet t)
  ,test
  (test)
  :doc "applying preserves an earlier capture gap instead of inventing file absence"
  (mevedel-note-change-test--with-note
    (setf (mevedel-session-file-snapshots session)
          (list (list 0 (cons file '(:version 1 :gap "pre-turn capture unavailable")))))
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (mevedel-note-apply-change note)
    (let* ((saved (mevedel-session-codec-read
                   (mevedel-session-artifacts-sidecar-path (mevedel-session-save-path session))))
           (entry (cdr (assoc file (cdr (assq 0 (plist-get saved :file-snapshots)))))))
      (should (equal "pre-turn capture unavailable" (plist-get entry :gap))))))

(mevedel-deftest mevedel-note-merge-disjoint
  (:quiet t)
  ,test
  (test)
  :doc "choosing AI for a conflicting hunk keeps disjoint user edits in the same file"
  (mevedel-note-change-test--with-note
    (with-current-buffer source
      (goto-char (point-max))
      (insert "middle1\nmiddle2\nmiddle3\ntail\n")
      (save-buffer))
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my ")
      (goto-char (point-max))
      (insert "user addition\n"))
    (with-current-buffer (mevedel-note-merge-change note)
      (goto-char (point-min))
      (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
      (mevedel-note-merge-confirm))
    (should (equal "before\nnew\nafter\nmiddle1\nmiddle2\nmiddle3\ntail\nuser addition\n"
                   (mevedel-test--read-file file)))))

(mevedel-deftest mevedel-note-merge-missing-git
  (:quiet t)
  ,test
  (test)
  :doc "merge refuses clearly without a local git executable and never changes source"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (let ((exec-path nil))
      (should (string-search "local git"
                             (error-message-string
                              (should-error (mevedel-note-merge-change note) :type 'user-error)))))
    (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))))

(mevedel-deftest mevedel-note-prepared-history
  (:quiet t)
  ,test
  (test)
  :doc "an unchanged preparation capture never records later user edits as an applied change"
  (mevedel-note-change-test--with-note
    (with-current-buffer data-buf
      (let* ((snapshots (make-hash-table :test #'equal))
             (mevedel--current-request (mevedel-request--create :file-snapshots snapshots)))
        (puthash file "before\nold\nafter\n" snapshots)
        (mevedel-note-change-test--propose
         data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
        (with-current-buffer source
          (goto-char (point-max))
          (insert "user edit\n")
          (save-buffer))
        (mevedel-session-artifacts-save session data-buf nil t)))
    (let ((saved (mevedel-session-codec-read
                  (mevedel-session-artifacts-sidecar-path (mevedel-session-save-path session)))))
      (should-not (cdr (assq 0 (plist-get saved :file-snapshots)))))))

(mevedel-deftest mevedel-note-merge-freshness
  (:quiet t)
  ,test
  (test)
  :doc "confirmation refuses source edits made after opening and cancel preserves source and focus"
  (dolist (save '(nil t))
    (mevedel-note-change-test--with-note
      (switch-to-buffer source)
      (goto-char (overlay-start note))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (let ((origin (point))
            (merge (mevedel-note-merge-change note)))
        (with-current-buffer source
          (goto-char (point-max))
          (insert "later user edit\n")
          (when save (save-buffer)))
        (with-current-buffer merge
          (goto-char (point-min))
          (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
          (should-error (mevedel-note-merge-confirm) :type 'user-error)
          (should (buffer-live-p merge))
          (mevedel-note-merge-cancel))
        (should (eq source (window-buffer (selected-window))))
        (should (= origin (window-point (selected-window))))
        (with-current-buffer source
          (should (equal "before\nold\nafter\nlater user edit\n" (buffer-string))))
        (should (equal (if save "before\nold\nafter\nlater user edit\n" "before\nold\nafter\n")
                       (mevedel-test--read-file file)))))))

(mevedel-deftest mevedel-note-reviewed-apply
  (:quiet t)
  ,test
  (test)
  :doc "prefix apply uses existing detailed review and records only the accepted selection"
  (mevedel-note-change-test--with-note
    (let ((two (file-name-concat root "two.txt")))
      (write-region "two old\n" nil two nil 'silent)
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** Update File: two.txt\n@@\n-two old\n+two new\n*** End Patch")
      (switch-to-buffer source)
      (goto-char (overlay-start note))
      (let ((current-prefix-arg '(4))) (call-interactively #'mevedel-note-apply-change))
      (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "M two.txt")
        (mevedel-patch-review-toggle-selection)
        (goto-char (point-min))
        (search-forward "ApplyPatch ·")
        (mevedel-patch-review-submit))
      (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file)))
      (should (equal "two old\n" (mevedel-test--read-file two)))
      (should-error (mevedel-note-apply-change note) :type 'user-error)
      (let* ((saved (mevedel-session-codec-read
                     (mevedel-session-artifacts-sidecar-path (mevedel-session-save-path session))))
             (entries (cdr (assq 0 (plist-get saved :file-snapshots)))))
        (should (assoc file entries))
        (should-not (assoc two entries))))))

(provide 'test-mevedel-note-change)
;;; test-mevedel-note-change.el ends here
