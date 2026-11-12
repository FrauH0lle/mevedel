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
(require 'mevedel-pipeline)
(require 'mevedel-view-disclosure)
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
             (save-window-excursion ,@body))
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

(mevedel-deftest mevedel-note-change-replacement
  (:quiet t)
  ,test
  (test)
  :doc "newer ApplyPatch replaces waiting work while both proposals stay readable in the conversation"
  (mevedel-note-change-test--with-note
    (setf (mevedel-session-permission-mode session) 'full-auto)
    (with-current-buffer data-buf
      (insert "*** Improve this note\n")
      (dolist (replacement '("first proposal" "second proposal"))
        (let* ((tool (mevedel-tool-ensure "ApplyPatch"))
               (provider (mevedel-tool-gptel-tool tool))
               (args (list :patch (format "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+%s\n*** End Patch" replacement)))
               (gptel-include-tool-results t)
               answer)
          (mevedel-pipeline-run-tool tool (lambda (value) (setq answer value)) args)
          (should (string-search "Prepared" answer))
          (goto-char (point-max))
          (gptel--display-tool-results
           (list (list provider args answer))
           (list :buffer data-buf :position (point-marker) :callback #'gptel--insert-response
                 :tools (list provider)
                 :tool-use (list (list :id replacement :name "ApplyPatch" :args args :result answer)))))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (while (search-forward "✓ ApplyPatch" nil t)
        (let ((row (match-beginning 0)))
          (when (get-text-property row 'mevedel-view-collapsed)
            (goto-char row)
            (mevedel-view-toggle-section))
          (forward-line 1)))
      (should (string-search "first proposal" (buffer-string)))
      (should (string-search "second proposal" (buffer-string))))
    (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))
    (mevedel-note-apply-change note)
    (should (equal "before\nsecond proposal\nafter\n" (mevedel-test--read-file file)))))

(mevedel-deftest mevedel-note-merge-literal-separator
  (:quiet t)
  ,test
  (test)
  :doc "literal heading separators in source are not mistaken for unresolved merge markers"
  (mevedel-note-change-test--with-note
    (with-current-buffer source
      (goto-char (point-max))
      (insert "Heading\n=======\n")
      (save-buffer))
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer (mevedel-note-merge-change note) (mevedel-note-merge-confirm))
    (should (equal "before\nnew\nafter\nHeading\n=======\n" (mevedel-test--read-file file)))))

(mevedel-deftest mevedel-note-merge-no-final-newline
  (:quiet t)
  ,test
  (test)
  :doc "keeping the unterminated source side never saves an artificial marker separator newline"
  (mevedel-note-change-test--with-note
    (with-current-buffer source
      (setq-local require-final-newline nil)
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (1- (point)) (point-max))
      (save-buffer))
    (should (equal "before\nold" (mevedel-test--read-file file)))
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my "))
    (with-current-buffer (mevedel-note-merge-change note)
      (goto-char (point-min))
      (search-forward "<<<<<<<")
      (smerge-keep-upper)
      (mevedel-note-merge-confirm))
    (should (equal "before\nmy old" (mevedel-test--read-file file)))))

(mevedel-deftest mevedel-note-revert-change
  (:quiet t)
  ,test
  (test)
  :doc "revert uses the actual applied pair and preserves later disjoint edits"
  (mevedel-note-change-test--with-note
    (setf (mevedel-session-turn-count session) 7)
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (point-max))
      (insert "before application\n")
      (save-buffer))
    (mevedel-note-apply-change note)
    (with-current-buffer source
      (goto-char (point-min))
      (insert "later user edit\n")
      (save-buffer)
      (mevedel-note-revert-change note 7)
      (should (equal "later user edit\nbefore\nold\nafter\nbefore application\n"
                     (buffer-string))))
    (should (equal "later user edit\nbefore\nold\nafter\nbefore application\n"
                   (mevedel-test--read-file file))))
  :doc "a done note reopens only after a successful reversal, including after store reload"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (mevedel-note-apply-change note)
    (let ((uuid (overlay-get note 'mevedel-uuid)))
      (mevedel-note-done note)
      (mevedel-instruction-store-flush t)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (with-current-buffer source
        (goto-char (point-min))
        (forward-line 1)
        (delete-region (line-beginning-position) (line-end-position))
        (insert "later replacement")
        (save-buffer))
      (should-error (mevedel-note-revert-change uuid 0 workspace) :type 'user-error)
      (should (= 1 (length (mevedel-notes-closed workspace))))
      (with-current-buffer source
        (goto-char (point-min))
        (forward-line 1)
        (delete-region (line-beginning-position) (line-end-position))
        (insert "new")
        (save-buffer))
      (mevedel-note-revert-change uuid 0 workspace)
      (should-not (mevedel-notes-closed workspace))
      (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))
      (let ((restored (mevedel-notes--resolve uuid workspace)))
        (should restored)
        (should (eq 'sent (overlay-get restored 'mevedel-note-state))))
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (should-not (mevedel-notes-closed workspace))
      (should (mevedel-notes--resolve uuid workspace))))
  :doc "the source key reverts the only applied turn without offering empty turns"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (mevedel-note-apply-change note)
    (mevedel-session-artifacts-snapshot-modified session 1 (make-hash-table :test #'equal))
    (with-current-buffer source
      (goto-char (overlay-start note))
      (call-interactively (lookup-key mevedel-note-actions-map (kbd "C-c C-b"))))
    (should (equal "before\nold\nafter\n" (mevedel-test--read-file file))))
  :doc "overlap reports shifted current lines and refuses every file in the change"
  (mevedel-note-change-test--with-note
    (let ((two (file-name-concat root "two.txt")))
      (write-region "two before\ntwo old\ntwo after\n" nil two nil 'silent)
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** Update File: two.txt\n@@\n-two old\n+two new\n*** End Patch")
      (mevedel-note-apply-change note)
      (write-region "user heading\ntwo before\nnewer user code\ntwo after\n" nil two nil 'silent)
      (let ((err (should-error (mevedel-note-revert-change note 0) :type 'user-error)))
        (should (string-search "two.txt:3-3" (error-message-string err))))
      (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file)))
      (should (equal "user heading\ntwo before\nnewer user code\ntwo after\n"
                     (mevedel-test--read-file two)))))
  :doc "deleting a note never undoes source, and explicit revert restores creation/deletion"
  (mevedel-note-change-test--with-note
    (let ((old (file-name-concat root "old.txt"))
          (new (file-name-concat root "new.txt"))
          (uuid (overlay-get note 'mevedel-uuid)))
      (write-region "removed file\n" nil old nil 'silent)
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Delete File: old.txt\n*** Add File: new.txt\n+created file\n*** End Patch")
      (mevedel-note-apply-change note)
      (mevedel-note-delete note)
      (should-not (file-exists-p old))
      (should (equal "created file\n" (mevedel-test--read-file new)))
      (mevedel-note-revert-change uuid 0 workspace)
      (should (equal "removed file\n" (mevedel-test--read-file old)))
      (should-not (file-exists-p new))
      (should (eq 'deleted (plist-get (plist-get (car (mevedel-notes-closed workspace))
                                                :instruction) :closed-reason)))))
  :doc "revert of a manual merge restores its actual unsaved pre-image"
  (mevedel-note-change-test--with-note
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
      (replace-match "resolved")
      (mevedel-note-merge-confirm))
    (should (equal "before\nresolved\nafter\n" (mevedel-test--read-file file)))
    (mevedel-note-revert-change note 0)
    (should (equal "before\nmy old\nafter\n" (mevedel-test--read-file file))))
  :doc "reverting an earlier turn leaves a later applied turn and the conversation intact"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (mevedel-note-apply-change note)
    (setf (mevedel-session-turn-count session) 1)
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-after\n+second turn\n*** End Patch")
    (mevedel-note-apply-change note)
    (let ((history (copy-tree (mevedel-session-file-snapshots session)))
          (transcript (with-current-buffer data-buf (buffer-string))))
      (mevedel-note-revert-change note 0)
      (should (equal "before\nold\nsecond turn\n" (mevedel-test--read-file file)))
      (should (equal history (mevedel-session-file-snapshots session)))
      (should (= 1 (mevedel-session-turn-count session)))
      (should (equal transcript (with-current-buffer data-buf (buffer-string))))))
  :doc "literal CRLF bytes survive session reload and later disjoint edits"
  (mevedel-note-change-test--with-note
    (with-current-buffer source
      (set-buffer-file-coding-system 'utf-8-dos)
      (save-buffer))
    (should (equal "before\r\nold\r\nafter\r\n"
                   (mevedel-session-artifacts-read-file-raw file)))
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (mevedel-note-apply-change note)
    (should (equal "before\r\nnew\r\nafter\r\n"
                   (mevedel-session-artifacts-read-file-raw file)))
    (mevedel-session-persistence-lock-release (mevedel-session-save-path session) session)
    (kill-buffer data-buf)
    (with-current-buffer source
      (goto-char (point-max))
      (insert "later\n")
      (save-buffer)
      (mevedel-note-revert-change note 0))
    (should (equal "before\r\nold\r\nafter\r\nlater\r\n"
                   (mevedel-session-artifacts-read-file-raw file))))
  :doc "unsaved visiting edits refuse without changing the buffer or disk"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (mevedel-note-apply-change note)
    (with-current-buffer source
      (goto-char (point-max))
      (insert "unsaved\n")
      (should-error (mevedel-note-revert-change note 0) :type 'user-error)
      (should (buffer-modified-p))
      (should (equal "before\nnew\nafter\nunsaved\n" (buffer-string))))
    (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file))))
  :doc "capture gaps refuse rather than treating an unknown pre-image as file absence"
  (mevedel-note-change-test--with-note
    (setf (mevedel-session-file-snapshots session)
          (list (list 0 (cons file '(:version 1 :gap "pre-turn capture unavailable")))))
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (mevedel-note-apply-change note)
    (let ((err (should-error (mevedel-note-revert-change note 0) :type 'user-error)))
      (should (string-search "pre-turn capture unavailable" (error-message-string err))))
    (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file)))))

(provide 'test-mevedel-note-change)
;;; test-mevedel-note-change.el ends here
