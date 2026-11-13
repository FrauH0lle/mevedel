;;; test-mevedel-note-change.el -- Prepared note changes -*- lexical-binding: t -*-

;;; Commentary:

;; Public note actions and ApplyPatch in real note sessions and source files.

;;; Code:

(require 'mevedel)
(require 'mevedel-notes)
(require 'mevedel-notes-list)
(require 'mevedel-buddy)
(require 'mevedel-note-conversation)
(require 'mevedel-note-write)
(require 'mevedel-tool-patch)
(require 'mevedel-note-merge)
(require 'mevedel-patch-review)
(require 'mevedel-view)
(require 'mevedel-session-persistence)
(require 'mevedel-session-codec)
(require 'mevedel-pipeline)
(require 'mevedel-presets)
(require 'mevedel-tools)
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
            (gptel--known-presets (copy-tree gptel--known-presets))
            (gptel--known-tools (copy-tree gptel--known-tools))
            (mevedel-preset--registry (copy-tree mevedel-preset--registry))
            (post-command-hook (copy-sequence post-command-hook))
            (workspace (mevedel-workspace-get-or-create 'file root root "changes"))
            (session (mevedel-session-create "changes" workspace root))
            (old-buffers (buffer-list))
            source note result)
       (unwind-protect
           (progn
             (make-directory (file-name-concat root ".mevedel"))
             (mevedel-tools-register)
             (mevedel--define-presets)
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
                           (overlay-get (overlay-get note 'mevedel-label-overlay) 'after-string))))
  :doc "persisted proposals retain one old and one new file image and still apply after reload"
  (mevedel-note-change-test--with-note
    (let ((tail (concat (make-string 65536 ?x) "\n"))
          (uuid (overlay-get note 'mevedel-uuid)))
      (with-current-buffer source
        (goto-char (point-max))
        (insert tail)
        (save-buffer))
      (mevedel-instruction-store-touch workspace)
      (mevedel-instruction-store-flush t)
      (let* ((store (mevedel-instruction-store-path workspace))
             (before-size (file-attribute-size (file-attributes store))))
        (should (string-search (substring tail 0 -1) (mevedel-test--read-file store)))
        (mevedel-note-change-test--propose
         data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
        (should (< (- (file-attribute-size (file-attributes store)) before-size)
                   (* 3 (file-attribute-size (file-attributes file))))))
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (setq note (mevedel-notes--resolve uuid workspace))
      (mevedel-note-apply-change note t)
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "ApplyPatch ·")
        (mevedel-patch-review-submit))
      (should (equal (concat "before\nnew\nafter\n" tail) (mevedel-test--read-file file)))
      (mevedel-note-revert-change note 0)
      (should (equal (concat "before\nold\nafter\n" tail) (mevedel-test--read-file file)))))
  :doc "batch proposals attach only to touched code and preserve a sibling's waiting change"
  (mevedel-note-change-test--with-note
    (let ((sibling (with-current-buffer source
                     (goto-char (point-min))
                     (let ((item (mevedel-note-create "Other request")))
                       (overlay-put item 'mevedel-note-state 'sent)
                       (overlay-put item 'mevedel-note-session-id
                                    (mevedel-session-session-id session))
                       item))))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (should (eq 'ready (mevedel-notes-attention-state note)))
      (should-not (eq 'ready (mevedel-notes-attention-state sibling)))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-before\n+BEFORE\n*** End Patch")
      (should (eq 'ready (mevedel-notes-attention-state note)))
      (should (eq 'ready (mevedel-notes-attention-state sibling)))
      (mevedel-note-apply-change note)
      (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file)))
      (should (eq 'ready (mevedel-notes-attention-state sibling)))
      (mevedel-note-apply-change sibling)
      (should (equal "BEFORE\nnew\nafter\n" (mevedel-test--read-file file)))))
  :doc "unsaved lines above notes do not attach a proposal to the wrong batch sibling"
  (mevedel-note-change-test--with-note
    (let ((sibling (with-current-buffer source
                     (goto-char (point-min))
                     (let ((item (mevedel-note-create "Other request")))
                       (overlay-put item 'mevedel-note-state 'sent)
                       (overlay-put item 'mevedel-note-session-id
                                    (mevedel-session-session-id session))
                       item))))
      (with-current-buffer source
        (goto-char (point-min))
        (insert "heading\n"))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (should (eq 'ready (mevedel-notes-attention-state note)))
      (should-not (eq 'ready (mevedel-notes-attention-state sibling)))
      (mevedel-note-apply-change note)
      (should (equal "heading\nbefore\nnew\nafter\n" (mevedel-test--read-file file)))))
  :doc "conflicting and disjoint hunks both notify their own batch notes"
  (mevedel-note-change-test--with-note
    (let ((sibling (with-current-buffer source
                     (goto-char (point-min))
                     (forward-line 2)
                     (let ((item (mevedel-note-create "Other request")))
                       (overlay-put item 'mevedel-note-state 'sent)
                       (overlay-put item 'mevedel-note-session-id
                                    (mevedel-session-session-id session))
                       (goto-char (overlay-start note))
                       (insert "my ")
                       item))))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n-after\n+AFTER\n*** End Patch")
      (should (eq 'ready (mevedel-notes-attention-state note)))
      (should (eq 'ready (mevedel-notes-attention-state sibling))))))

(mevedel-deftest mevedel-tool-patch-handler/note-resource-move
  (:quiet t :doc "a move to a resource uses ordinary patch application rather than a waiting change")
  (mevedel-note-change-test--with-note
    (let ((mevedel-resource-attempts-cell (list nil))
          (destination (file-name-concat root ".mevedel/shared/moved.txt")))
      (unwind-protect
          (progn
            (setf (mevedel-session-permission-mode session) 'full-auto)
            (setq result (mevedel-note-change-test--propose
                          data-buf "*** Begin Patch\n*** Update File: source.txt\n*** Move to: work://shared/moved.txt\n*** End Patch"))
            (should-not (overlay-get note 'mevedel-note-prepared-change))
            (should-not (file-exists-p file))
            (should (equal "before\nold\nafter\n" (mevedel-test--read-file destination))))
        (mevedel-resource-discard-attempts (car mevedel-resource-attempts-cell))))))

(mevedel-deftest mevedel-note-conversation/batch-change-actions
  (:quiet t :doc "conversation apply and merge choose the batch note with waiting work")
  (dolist (command '(mevedel-note-apply-change mevedel-note-merge-change))
    (mevedel-note-change-test--with-note
      (with-current-buffer source
        (goto-char (point-min))
        (let ((second (mevedel-note-create "Change only this note")))
          (overlay-put second 'mevedel-note-state 'sent)
          (overlay-put second 'mevedel-note-session-id (mevedel-session-session-id session))))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-before\n+BEFORE\n*** End Patch")
      (should-not (overlay-get note 'mevedel-note-prepared-change))
      (with-current-buffer view-buf
        (if (eq command 'mevedel-note-merge-change)
            (with-current-buffer (call-interactively command)
              (mevedel-note-merge-confirm))
          (call-interactively command)))
      (should (equal "BEFORE\nold\nafter\n" (mevedel-test--read-file file))))))

(mevedel-deftest mevedel-note-take/prepared-restore
  (:quiet t :doc "restored takes keep their prepared change and show paused in source and list")
  (mevedel-note-change-test--with-note
    (let ((mevedel-notes--taken-watches (make-hash-table :test #'equal))
          (mevedel-buddy--levels (make-hash-table :test #'equal)))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (with-current-buffer source
        (mevedel-note-take note)
        (mevedel-instruction-store-flush t)
        (mevedel-buddy-mode -1)
        (mevedel--clear-instruction-state workspace)
        (clrhash mevedel-notes--taken-watches)
        (mevedel-instruction-store-observe workspace)
        (setq note (mevedel-notes--at-point))
        (should (eq 'ready (mevedel-notes-attention-state note)))
        (should-not mevedel-buddy-mode)
        (should (string-search "paused"
                               (overlay-get (overlay-get note 'mevedel-label-overlay)
                                            'after-string))))
      (with-current-buffer (mevedel-notes-list workspace)
        (should (string-search "ready" (buffer-string)))
        (should (string-search "paused" (buffer-string)))))))

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
      (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))))
  :doc "apply refuses a saved nonoverlapping edit inside the noted code"
  (mevedel-note-change-test--with-note
    (with-current-buffer source
      (move-overlay note (point-min) (point-max)))
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (point-min))
      (insert "my ")
      (save-buffer)
      (should (eq 'stale (mevedel-notes-attention-state note)))
      (let ((err (should-error (mevedel-note-apply-change note) :type 'user-error)))
        (should (string-search "re-ask" (error-message-string err)))
        (should (string-search "merge" (error-message-string err))))
      (should (equal "my before\nold\nafter\n" (buffer-string))))
    (should (equal "my before\nold\nafter\n" (mevedel-test--read-file file))))
  :doc "a nonstale batch sibling cannot apply a proposal whose other note is stale"
  (mevedel-note-change-test--with-note
    (let ((sibling (with-current-buffer source
                     (goto-char (point-min))
                     (forward-line 2)
                     (let ((item (mevedel-note-create "Other request")))
                       (overlay-put item 'mevedel-note-state 'sent)
                       (overlay-put item 'mevedel-note-session-id
                                    (mevedel-session-session-id session))
                       (move-overlay note (point-min) (overlay-start item))
                       item))))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n-after\n+AFTER\n*** End Patch")
      (with-current-buffer source
        (goto-char (point-min))
        (insert "my ")
        (save-buffer))
      (should (eq 'stale (mevedel-notes-attention-state note)))
      (should (eq 'ready (mevedel-notes-attention-state sibling)))
      (should-error (mevedel-note-apply-change sibling) :type 'user-error)
      (should (equal "my before\nold\nafter\n" (mevedel-test--read-file file)))))
  :doc "apply preserves unsaved disjoint visiting text in the applied pair"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (point-max))
      (insert "unsaved\n")
      (mevedel-note-apply-change note)
      (should (equal "before\nnew\nafter\nunsaved\n" (buffer-string))))
    (should (equal "before\nnew\nafter\nunsaved\n" (mevedel-test--read-file file)))
    (mevedel-note-revert-change note 0)
    (should (equal "before\nold\nafter\nunsaved\n" (mevedel-test--read-file file))))
  :doc "apply preserves external disk edits and refuses unsaved buffers with outdated disk state"
  (dolist (modified '(nil t))
    (mevedel-note-change-test--with-note
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (when modified
        (with-current-buffer source
          (goto-char (point-max))
          (insert "unsaved\n")))
      (with-temp-file file (insert "before\nold\nexternal disk edit\n"))
      (if modified
          (progn
            (should-error (mevedel-note-apply-change note) :type 'user-error)
            (should (equal "before\nold\nexternal disk edit\n" (mevedel-test--read-file file)))
            (with-current-buffer source
              (should (equal "before\nold\nafter\nunsaved\n" (buffer-string)))))
        (mevedel-note-apply-change note)
        (should (equal "before\nnew\nexternal disk edit\n" (mevedel-test--read-file file))))))
  :doc "the notes list applies the selected note with the single a key"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer (mevedel-notes-list workspace)
      (goto-char (point-min))
      (search-forward "Improve this")
      (should (commandp (key-binding "a")))
      (call-interactively (key-binding "a"))
      (should-not (string-search "ready" (buffer-string))))
    (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file))))
  :doc "apply refuses a nonoverlapping external disk edit inside the noted code"
  (mevedel-note-change-test--with-note
    (with-current-buffer source
      (move-overlay note (point-min) (point-max)))
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-temp-file file (insert "external before\nold\nafter\n"))
    (should-error (mevedel-note-apply-change note) :type 'user-error)
    (should (eq 'stale (mevedel-notes-attention-state note)))
    (should (equal "external before\nold\nafter\n" (mevedel-test--read-file file))))
  :doc "NoteWrite prepared suggestions also refuse external edits inside their note"
  (mevedel-note-change-test--with-note
    (with-current-buffer data-buf
      (mevedel-tool-note-write
       (list :operations
             (vector (list :action "create" :file_path "source.txt"
                           :start_line 1 :end_line 3 :text "AI request"
                           :prepared_change
                           "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))))
    (with-temp-file file (insert "external before\nold\nafter\n"))
    (with-current-buffer (mevedel-notes-list workspace)
      (goto-char (point-min))
      (search-forward "AI request")
      (should-error (call-interactively (key-binding "a")) :type 'user-error))
    (should (equal "external before\nold\nafter\n" (mevedel-test--read-file file)))))

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
        (should-not (assoc two entries)))))

  :doc "detailed review merges saved disjoint edits made before or during review"
  (dolist (timing '(before during))
    (mevedel-note-change-test--with-note
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (when (eq timing 'during) (mevedel-note-apply-change note t))
      (with-current-buffer source
        (goto-char (point-max))
        (insert "saved user edit\n")
        (save-buffer))
      (when (eq timing 'before) (mevedel-note-apply-change note t))
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "ApplyPatch ·")
        (mevedel-patch-review-submit))
      (should (equal "before\nnew\nafter\nsaved user edit\n" (mevedel-test--read-file file)))
      (mevedel-note-revert-change note 0)
      (should (equal "before\nold\nafter\nsaved user edit\n" (mevedel-test--read-file file)))))
  :doc "detailed review preserves CRLF and UTF-8 bytes when merging saved edits"
  (mevedel-note-change-test--with-note
    (with-current-buffer source
      (set-buffer-file-coding-system 'utf-8-dos)
      (save-buffer))
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+n\u00e9w\n*** End Patch")
    (with-current-buffer source
      (goto-char (point-max))
      (insert "saved\n")
      (save-buffer))
    (mevedel-note-apply-change note t)
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "ApplyPatch ·")
      (mevedel-patch-review-submit))
    (should (equal (encode-coding-string "before\nn\u00e9w\nafter\nsaved\n" 'utf-8-dos)
                   (mevedel-session-artifacts-read-file-raw file))))
  :doc "detailed review preserves unsaved disjoint source text at confirmation"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (point-max))
      (insert "unsaved\n")
      (mevedel-note-apply-change note t))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "ApplyPatch ·")
      (mevedel-patch-review-submit))
    (should (equal "before\nnew\nafter\nunsaved\n" (mevedel-test--read-file file))))
  :doc "partial review refuses disk edits inside the owning note in an unselected file"
  (mevedel-note-change-test--with-note
    (let ((two (file-name-concat root "two.txt")))
      (write-region "two old\n" nil two nil 'silent)
      (with-current-buffer source
        (move-overlay note (point-min) (point-max)))
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** Update File: two.txt\n@@\n-two old\n+two new\n*** End Patch")
      (mevedel-note-apply-change note t)
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "M source.txt")
        (mevedel-patch-review-toggle-selection)
        (with-temp-file file (insert "external before\nold\nafter\n"))
        (goto-char (point-min))
        (search-forward "ApplyPatch ·")
        (mevedel-patch-review-submit))
      (should (equal "two old\n" (mevedel-test--read-file two)))
      (should (eq 'stale (mevedel-notes-attention-state note)))
      (should (equal "external before\nold\nafter\n" (mevedel-test--read-file file))))))

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
  :doc "revert preserves unsaved disjoint visiting edits"
  (mevedel-note-change-test--with-note
    (mevedel-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (mevedel-note-apply-change note)
    (with-current-buffer source
      (goto-char (point-max))
      (insert "unsaved\n")
      (mevedel-note-revert-change note 0)
      (should (equal "before\nold\nafter\nunsaved\n" (buffer-string))))
    (should (equal "before\nold\nafter\nunsaved\n" (mevedel-test--read-file file))))
  :doc "revert preserves external disk edits and refuses outdated unsaved visiting text"
  (dolist (modified '(nil t))
    (mevedel-note-change-test--with-note
      (mevedel-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (mevedel-note-apply-change note)
      (when modified
        (with-current-buffer source
          (goto-char (point-max))
          (insert "unsaved\n")))
      (with-temp-file file (insert "before\nnew\nexternal disk edit\n"))
      (if modified
          (progn
            (should-error (mevedel-note-revert-change note 0) :type 'user-error)
            (should (equal "before\nnew\nexternal disk edit\n" (mevedel-test--read-file file)))
            (with-current-buffer source
              (should (equal "before\nnew\nafter\nunsaved\n" (buffer-string)))))
        (mevedel-note-revert-change note 0)
        (should (equal "before\nold\nexternal disk edit\n" (mevedel-test--read-file file))))))
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

(mevedel-deftest mevedel-note-revert-change/note-write
  (:quiet t :doc "an applied NoteWrite suggestion remains reversible after closing and reloading")
  (mevedel-note-change-test--with-note
    (setf (mevedel-session-preset-name session) 'mevedel-implement)
    (with-current-buffer data-buf
      (setq result
            (mevedel-tool-note-write
             (list :operations
                   (vector (list :action "create" :file_path "source.txt" :start_line 2
                                 :text "AI suggestion" :prepared_change
                                 "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"))))))
    (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get result :result)))
    (let* ((uuid (match-string 1 (plist-get result :result)))
           (suggestion (mevedel-notes--resolve uuid workspace)))
      (should-not (overlay-get suggestion 'mevedel-note-session-id))
      (mevedel-note-apply-change suggestion)
      (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file)))
      (mevedel-note-done suggestion)
      (mevedel-instruction-store-flush t)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (mevedel-note-revert-change uuid nil workspace)
      (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))
      (should-not (mevedel-notes-closed workspace)))))

(mevedel-deftest mevedel-note-revert-change/deleted-origin
  (:quiet t :doc "a deleted originating main chat refuses revert without touching source or reopening the note")
  (mevedel-note-change-test--with-note
    (setf (mevedel-session-preset-name session) 'mevedel-implement)
    (with-current-buffer data-buf
      (setq result
            (mevedel-tool-note-write
             (list :operations
                   (vector (list :action "create" :file_path "source.txt" :start_line 2
                                 :text "AI suggestion" :prepared_change
                                 "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"))))))
    (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get result :result)))
    (let* ((uuid (match-string 1 (plist-get result :result)))
           (suggestion (mevedel-notes--resolve uuid workspace)))
      (mevedel-note-apply-change suggestion)
      (mevedel-note-done suggestion)
      (mevedel-session-persistence-lock-release (mevedel-session-save-path session) session)
      (kill-buffer data-buf)
      (delete-directory (mevedel-session-save-path session) t)
      (let ((before (mevedel-session-artifacts-read-file-raw file))
            (text (with-current-buffer source (buffer-string)))
            (err (should-error (mevedel-note-revert-change uuid nil workspace) :type 'user-error)))
        (should (string-search "conversation that made this change no longer exists"
                               (error-message-string err)))
        (should (equal before (mevedel-session-artifacts-read-file-raw file)))
        (should (equal text (with-current-buffer source (buffer-string))))
        (should (= 1 (length (mevedel-notes-closed workspace))))))))

(provide 'test-mevedel-note-change)
;;; test-mevedel-note-change.el ends here
