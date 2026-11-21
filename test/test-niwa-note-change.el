;;; test-niwa-note-change.el -- Prepared note changes -*- lexical-binding: t -*-

;;; Commentary:

;; Public note actions and ApplyPatch in real note sessions and source files.

;;; Code:

(require 'niwa)
(require 'niwa-notes)
(require 'niwa-notes-list)
(require 'niwa-buddy)
(require 'niwa-note-conversation)
(require 'niwa-note-write)
(require 'niwa-tool-patch)
(require 'niwa-note-merge)
(require 'niwa-patch-review)
(require 'niwa-view)
(require 'niwa-session-persistence)
(require 'niwa-session-codec)
(require 'niwa-session-fork)
(require 'niwa-session-rewind)
(require 'niwa-pipeline)
(require 'niwa-presets)
(require 'niwa-tools)
(require 'niwa-view-disclosure)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar niwa-note-change-test--workspace-type 'file
  "Workspace authority exercised by the note fixture.")

(defmacro niwa-note-change-test--with-note (&rest body)
  "Run BODY with a sent note in a real source file and session."
  (declare (indent 0))
  `(niwa-view-test--with-buffers
     (let* ((root (make-temp-file "niwa-note-change-" t))
            (file (file-name-concat root "source.txt"))
            (niwa-sessions-directory (file-name-concat root "sessions"))
            (niwa--instruction-states (make-hash-table :test #'equal))
            (niwa-instruction-store--dirty nil)
            (niwa-instruction-store--failed nil)
            (niwa-note-labels 'expanded)
            (niwa-workspace--registry (make-hash-table :test #'equal))
            (gptel--known-presets (copy-tree gptel--known-presets))
            (gptel--known-tools (copy-tree gptel--known-tools))
            (niwa-preset--registry (copy-tree niwa-preset--registry))
            (post-command-hook (copy-sequence post-command-hook))
            (workspace (niwa-workspace-get-or-create
                        niwa-note-change-test--workspace-type root root "changes"))
            (session (niwa-session-create "changes" workspace root))
            (old-buffers (buffer-list))
            source note result)
       (unwind-protect
           (progn
             (make-directory (file-name-concat root ".niwa"))
             (niwa-tools-register)
             (niwa--define-presets)
             (write-region "before\nold\nafter\n" nil file nil 'silent)
             (setq source (find-file-noselect file))
             (with-current-buffer source
               (setq-local niwa--workspace workspace)
               (goto-char (point-min))
               (forward-line 1)
               (setq note (niwa-note-create "Improve this"))
               (niwa-note-mark-sent (list note) (niwa-session-session-id session)))
             (setf (niwa-session-preset-name session) 'niwa-implement
                   (niwa-session-note-conversation session) t)
             (with-current-buffer data-buf
               (setq-local default-directory (file-name-as-directory root)
                           niwa--workspace workspace
                           niwa--session session))
             (niwa-session-artifacts-save session data-buf nil t)
             (save-window-excursion ,@body))
         (dolist (buffer (cl-set-difference (buffer-list) old-buffers))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil)
               (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
         (niwa-workspace-clear-registry)
         (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
         (delete-directory root t)))))

(defun niwa-note-change-test--propose (data patch)
  "Invoke ApplyPatch in DATA with PATCH and return its immediate result."
  (let (result)
    (with-current-buffer data
      (niwa-tool-patch-handler (lambda (value) (setq result value))
                                (list :patch patch)))
    result))

(niwa-deftest niwa-note-prepare-change
  (:quiet t)
  ,test
  (test)
  :doc "ApplyPatch returns a prepared change without writing or awaiting review"
  (niwa-note-change-test--with-note
    (setq result (niwa-note-change-test--propose
                  data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"))
    (should result)
    (should (string-search "Prepared" (plist-get result :result)))
    (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))
    (should (eq 'ready (niwa-notes-attention-state note)))
    (should (string-search "change ready"
                           (overlay-get (overlay-get note 'niwa-label-overlay) 'after-string))))
  :doc "persisted proposals retain one old and one new file image and still apply after reload"
  (niwa-note-change-test--with-note
    (let ((tail (concat (make-string 65536 ?x) "\n"))
          (uuid (overlay-get note 'niwa-uuid)))
      (with-current-buffer source
        (goto-char (point-max))
        (insert tail)
        (save-buffer))
      (niwa-instruction-store-touch workspace)
      (niwa-instruction-store-flush t)
      (let* ((store (niwa-instruction-store-path workspace))
             (before-size (file-attribute-size (file-attributes store))))
        (should (string-search (substring tail 0 -1) (niwa-test--read-file store)))
        (niwa-note-change-test--propose
         data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
        (should (< (- (file-attribute-size (file-attributes store)) before-size)
                   (* 3 (file-attribute-size (file-attributes file))))))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (setq note (niwa-note-resolve uuid workspace))
      (niwa-note-apply-change note t)
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "ApplyPatch ·")
        (niwa-patch-review-submit))
      (should (equal (concat "before\nnew\nafter\n" tail) (niwa-test--read-file file)))
      (niwa-note-revert-change note 0)
      (should (equal (concat "before\nold\nafter\n" tail) (niwa-test--read-file file)))))
  :doc "apply closes the whole batch, retaining a sibling's independent waiting change for restore"
  (niwa-note-change-test--with-note
    (let ((sibling (with-current-buffer source
                     (goto-char (point-min))
                     (let ((item (niwa-note-create "Other request")))
                       (niwa-note-mark-sent (list item) (niwa-session-session-id session))
                       item))))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (should (eq 'ready (niwa-notes-attention-state note)))
      (should-not (eq 'ready (niwa-notes-attention-state sibling)))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-before\n+BEFORE\n*** End Patch")
      (should (eq 'ready (niwa-notes-attention-state note)))
      (should (eq 'ready (niwa-notes-attention-state sibling)))
      (niwa-note-apply-change note)
      (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))
      (should-not (overlay-buffer note))
      (should-not (overlay-buffer sibling))
      (should (eq 'ready (niwa-notes-attention-state sibling)))
      (setq sibling (niwa-note-restore (overlay-get sibling 'niwa-uuid) workspace))
      (niwa-note-apply-change sibling)
      (should (equal "BEFORE\nnew\nafter\n" (niwa-test--read-file file)))))
  :doc "unsaved lines above notes do not attach a proposal to the wrong batch sibling"
  (niwa-note-change-test--with-note
    (let ((sibling (with-current-buffer source
                     (goto-char (point-min))
                     (let ((item (niwa-note-create "Other request")))
                       (niwa-note-mark-sent (list item) (niwa-session-session-id session))
                       item))))
      (with-current-buffer source
        (goto-char (point-min))
        (insert "heading\n"))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (should (eq 'ready (niwa-notes-attention-state note)))
      (should-not (eq 'ready (niwa-notes-attention-state sibling)))
      (niwa-note-apply-change note)
      (should (equal "heading\nbefore\nnew\nafter\n" (niwa-test--read-file file)))))
  :doc "conflicting and disjoint hunks both notify their own batch notes"
  (niwa-note-change-test--with-note
    (let ((sibling (with-current-buffer source
                     (goto-char (point-min))
                     (forward-line 2)
                     (let ((item (niwa-note-create "Other request")))
                       (niwa-note-mark-sent (list item) (niwa-session-session-id session))
                       (goto-char (overlay-start note))
                       (insert "my ")
                       item))))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n-after\n+AFTER\n*** End Patch")
      (should (eq 'ready (niwa-notes-attention-state note)))
      (should (eq 'ready (niwa-notes-attention-state sibling))))))

(niwa-deftest niwa-tool-patch-handler/note-resource-move
  (:quiet t :doc "a move to a resource uses ordinary patch application rather than a waiting change")
  (niwa-note-change-test--with-note
    (let ((niwa-resource-attempts-cell (list nil))
          (destination (file-name-concat root ".niwa/shared/moved.txt")))
      (unwind-protect
          (progn
            (setf (niwa-session-permission-mode session) 'full-auto)
            (setq result (niwa-note-change-test--propose
                          data-buf "*** Begin Patch\n*** Update File: source.txt\n*** Move to: work://shared/moved.txt\n*** End Patch"))
            (should-not (overlay-get note 'niwa-note-prepared-change))
            (should-not (file-exists-p file))
            (should (equal "before\nold\nafter\n" (niwa-test--read-file destination))))
        (niwa-resource-discard-attempts (car niwa-resource-attempts-cell))))))

(niwa-deftest niwa-note-conversation/batch-change-actions
  (:quiet t :doc "conversation apply and merge choose the batch note with waiting work")
  (dolist (command '(niwa-note-apply-change niwa-note-merge-change))
    (niwa-note-change-test--with-note
      (with-current-buffer source
        (goto-char (point-min))
        (let ((second (niwa-note-create "Change only this note")))
          (niwa-note-mark-sent (list second) (niwa-session-session-id session))))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-before\n+BEFORE\n*** End Patch")
      (should-not (overlay-get note 'niwa-note-prepared-change))
      (with-current-buffer view-buf
        (if (eq command 'niwa-note-merge-change)
            (with-current-buffer (call-interactively command)
              (niwa-note-merge-confirm))
          (call-interactively command)))
      (should (equal "BEFORE\nold\nafter\n" (niwa-test--read-file file))))))

(niwa-deftest niwa-note-take/prepared-restore
  (:quiet t :doc "restored takes keep their prepared change and show paused in source and list")
  (niwa-note-change-test--with-note
    (let ((niwa-notes--taken-watches (make-hash-table :test #'equal))
          (niwa-buddy--levels (make-hash-table :test #'equal)))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (with-current-buffer source
        (niwa-note-take note)
        (niwa-instruction-store-flush t)
        (niwa-buddy-mode -1)
        (niwa--clear-instruction-state workspace)
        (clrhash niwa-notes--taken-watches)
        (niwa-instruction-store-observe workspace)
        (setq note (niwa-notes--at-point))
        (should (eq 'ready (niwa-notes-attention-state note)))
        (should-not niwa-buddy-mode)
        (should (string-search "paused"
                               (overlay-get (overlay-get note 'niwa-label-overlay)
                                            'after-string))))
      (with-current-buffer (niwa-notes-list workspace)
        (should (string-search "ready" (buffer-string)))
        (should (string-search "paused" (buffer-string)))))))

(niwa-deftest niwa-note-apply-change
  (:quiet t)
  ,test
  (test)
  :doc "apply writes the prepared change and one undo restores the source"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (niwa-note-apply-change note)
      (should (equal "before\nnew\nafter\n" (buffer-string)))
      (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))
      (should-not (eq 'closed (overlay-get note 'niwa-note-state)))
      (should-not (eq 'ready (niwa-notes-attention-state note)))
      (let ((last-command nil)) (undo-only 1))
      (should (equal "before\nold\nafter\n" (buffer-string)))))
  :doc "editing noted code immediately marks the waiting change stale and refuses apply"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (1+ (overlay-start note)))
      (insert "my ")
      (should (eq 'stale (niwa-notes-attention-state note)))
      (should-error (niwa-note-apply-change note) :type 'user-error)
      (should (equal "before\nomy ld\nafter\n" (buffer-string)))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))))
  :doc "apply refuses a saved nonoverlapping edit inside the noted code"
  (niwa-note-change-test--with-note
    (with-current-buffer source
      (move-overlay note (point-min) (point-max)))
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (1+ (point-min)))
      (insert "my ")
      (save-buffer)
      (should (eq 'stale (niwa-notes-attention-state note)))
      (let ((err (should-error (niwa-note-apply-change note) :type 'user-error)))
        (should (string-search "ask in the conversation" (error-message-string err)))
        (should (string-search "merge" (error-message-string err))))
      (should (equal "bmy efore\nold\nafter\n" (buffer-string))))
    (should (equal "bmy efore\nold\nafter\n" (niwa-test--read-file file))))
  :doc "a nonstale batch sibling cannot apply a proposal whose other note is stale"
  (niwa-note-change-test--with-note
    (let ((sibling (with-current-buffer source
                     (goto-char (point-min))
                     (forward-line 2)
                     (let ((item (niwa-note-create "Other request")))
                       (niwa-note-mark-sent (list item) (niwa-session-session-id session))
                       (move-overlay note (point-min) (overlay-start item))
                       item))))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n-after\n+AFTER\n*** End Patch")
      (with-current-buffer source
        (goto-char (1+ (point-min)))
        (insert "my ")
        (save-buffer))
      (should (eq 'stale (niwa-notes-attention-state note)))
      (should (eq 'ready (niwa-notes-attention-state sibling)))
      (should-error (niwa-note-apply-change sibling) :type 'user-error)
      (should (equal "bmy efore\nold\nafter\n" (niwa-test--read-file file)))))
  :doc "apply preserves unsaved disjoint visiting text in the applied pair"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (point-max))
      (insert "unsaved\n")
      (niwa-note-apply-change note)
      (should (equal "before\nnew\nafter\nunsaved\n" (buffer-string))))
    (should (equal "before\nnew\nafter\nunsaved\n" (niwa-test--read-file file)))
    (niwa-note-revert-change note 0)
    (should (equal "before\nold\nafter\nunsaved\n" (niwa-test--read-file file))))
  :doc "apply preserves external disk edits and refuses unsaved buffers with outdated disk state"
  (dolist (modified '(nil t))
    (niwa-note-change-test--with-note
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (when modified
        (with-current-buffer source
          (goto-char (point-max))
          (insert "unsaved\n")))
      (with-temp-file file (insert "before\nold\nexternal disk edit\n"))
      (if modified
          (progn
            (should-error (niwa-note-apply-change note) :type 'user-error)
            (should (equal "before\nold\nexternal disk edit\n" (niwa-test--read-file file)))
            (with-current-buffer source
              (should (equal "before\nold\nafter\nunsaved\n" (buffer-string)))))
        (niwa-note-apply-change note)
        (should (equal "before\nnew\nexternal disk edit\n" (niwa-test--read-file file))))))
  :doc "the notes list applies the selected note with the single a key"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer (niwa-notes-list workspace)
      (goto-char (point-min))
      (search-forward "Improve this")
      (should (commandp (key-binding "a")))
      (call-interactively (key-binding "a"))
      (should-not (string-search "ready" (buffer-string))))
    (should (equal "before\nnew\nafter\n" (niwa-test--read-file file))))
  :doc "apply refuses a nonoverlapping external disk edit inside the noted code"
  (niwa-note-change-test--with-note
    (with-current-buffer source
      (move-overlay note (point-min) (point-max)))
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-temp-file file (insert "external before\nold\nafter\n"))
    (should-error (niwa-note-apply-change note) :type 'user-error)
    (should (eq 'stale (niwa-notes-attention-state note)))
    (should (equal "external before\nold\nafter\n" (niwa-test--read-file file))))
  :doc "NoteWrite prepared suggestions also refuse external edits inside their note"
  (niwa-note-change-test--with-note
    (with-current-buffer data-buf
      (niwa-tool-note-write
       (list :operations
             (vector (list :action "create" :file_path "source.txt"
                           :start_line 1 :end_line 3 :text "AI request"
                           :prepared_change
                           "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))))
    (with-temp-file file (insert "external before\nold\nafter\n"))
    (with-current-buffer (niwa-notes-list workspace)
      (goto-char (point-min))
      (search-forward "AI request")
      (should-error (call-interactively (key-binding "a")) :type 'user-error))
    (should (equal "external before\nold\nafter\n" (niwa-test--read-file file)))))

(niwa-deftest niwa-note-apply-change/missing-sibling
  (:quiet t :doc "a missing batch sibling does not interrupt apply or lose its durable revert record")
  (niwa-note-change-test--with-note
    (let* ((uuid (overlay-get note 'niwa-uuid))
           (other (file-name-concat root "other.txt"))
           (created (file-name-concat root "created.txt"))
           (sibling (progn
                      (write-region "other\n" nil other nil 'silent)
                      (find-file-noselect other))))
      (with-current-buffer sibling
        (setq-local niwa--workspace workspace)
        (let ((item (niwa-note-create "Batch sibling")))
          (niwa-note-mark-sent (list item) (niwa-session-session-id session))))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Add File: created.txt\n+created\n*** End Patch")
      (kill-buffer sibling)
      (delete-file other)
      (niwa-note-apply-change note)
      (should (equal "created\n" (niwa-test--read-file created)))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (should-not (niwa-note-resolve uuid workspace))
      (let* ((entry (cl-find uuid (niwa-notes-closed workspace)
                             :key (lambda (entry)
                                    (plist-get (plist-get (plist-get entry :instruction) :properties)
                                               'niwa-uuid)) :test #'equal))
             (properties (plist-get (plist-get entry :instruction) :properties)))
        (should-not (plist-get properties 'niwa-note-prepared-change))
        (should (plist-get properties 'niwa-note-applied-changes)))
      (niwa-note-revert-change uuid 0 workspace)
      (should-not (file-exists-p created)))))

(niwa-deftest niwa-note-apply-change/unencodable
  (:quiet t :doc "apply and revert refuse lossy encoding without changing disk or unsaved text")
  (dolist (revert '(nil t))
    (niwa-note-change-test--with-note
      (let* ((file (file-name-concat root "other.txt"))
             (source (progn
                       (write-region "before\nold\nafter\n" nil file nil 'silent)
                       (find-file-noselect file))))
        (niwa-note-change-test--propose
         data-buf "*** Begin Patch\n*** Update File: other.txt\n@@\n-old\n+new\n*** End Patch")
        (when revert (niwa-note-apply-change note))
        (let ((before (if revert "before\nnew\nafter\n" "before\nold\nafter\n")))
          (with-current-buffer source
            (set-buffer-file-coding-system 'iso-latin-1-unix)
            (goto-char (point-max))
            (insert "5\u20ac\n")
            (let ((err (should-error
                        (if revert (niwa-note-revert-change note 0)
                          (niwa-note-apply-change note))
                        :type 'user-error)))
              (should (string-search "coding cannot write" (error-message-string err))))
            (should (equal (concat before "5\u20ac\n") (buffer-string)))
            (should (buffer-modified-p)))
          (should (equal before (niwa-session-artifacts-read-file-raw file))))))))

(niwa-deftest niwa-note-apply-change/store-sync
  (:quiet t :doc "a stale client cannot reapply another client's reverted change")
  (dolist (resume '(t nil))
    (niwa-note-change-test--with-note
      (let ((uuid (overlay-get note 'niwa-uuid))
            (store (niwa-instruction-store-path workspace))
            waiting applied)
        (niwa-note-change-test--propose
         data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
        (setq waiting (niwa-session-artifacts-read-file-raw store))
        (niwa-note-apply-change note)
        (niwa-note-revert-change note 0)
        (setq applied (niwa-session-artifacts-read-file-raw store))
        ;; Replay A's last observation, then B's newer store bytes.
        (let ((coding-system-for-write 'no-conversion))
          (write-region waiting nil store nil 'silent)
          (niwa--clear-instruction-state workspace)
          (niwa-instruction-store-observe workspace)
          (write-region applied nil store nil 'silent))
        (setq note (niwa-note-resolve uuid workspace))
        (when resume
          (niwa-session-persistence-lock-release (niwa-session-save-path session) session)
          (kill-buffer data-buf))
        (should-error (niwa-note-apply-change note) :type 'user-error)
        (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))
        (setq note (niwa-note-resolve uuid workspace))
        (should (overlay-get note 'niwa-note-applied-changes))
        (should-not (overlay-get note 'niwa-note-prepared-change))))))

(niwa-deftest niwa-note-revert-change/store-sync
  (:quiet t :doc "revert observes another client's done note and reopens that UUID")
  (niwa-note-change-test--with-note
    (let ((uuid (overlay-get note 'niwa-uuid))
          (store (niwa-instruction-store-path workspace))
          applied closed)
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (niwa-note-apply-change note)
      (setq note (niwa-note-restore uuid workspace))
      (niwa-instruction-store-flush t)
      (setq applied (niwa-session-artifacts-read-file-raw store))
      (niwa-note-done note)
      (niwa-instruction-store-flush t)
      (setq closed (niwa-session-artifacts-read-file-raw store))
      (let ((coding-system-for-write 'no-conversion))
        (write-region applied nil store nil 'silent)
        (niwa--clear-instruction-state workspace)
        (niwa-instruction-store-observe workspace)
        (write-region closed nil store nil 'silent))
      (setq note (niwa-note-resolve uuid workspace))
      (niwa-note-revert-change note 0)
      (niwa-instruction-store-observe workspace)
      (should-not (niwa-notes-closed workspace))
      (should (niwa-note-resolve uuid workspace))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file))))))

(niwa-deftest niwa-note-revert-change/unseen-uuid
  (:quiet t :doc "revert observes the store before looking up a UUID first seen by another client")
  (niwa-note-change-test--with-note
    (let* ((store (niwa-instruction-store-path workspace))
           (before (niwa-session-artifacts-read-file-raw store))
           (sibling (with-current-buffer source
                      (goto-char (overlay-start note))
                      (let ((item (niwa-note-create "Other client's note")))
                        (niwa-note-mark-sent (list item) (niwa-session-session-id session))
                        item)))
           (uuid (overlay-get sibling 'niwa-uuid)))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (niwa-note-apply-change sibling)
      (let ((applied (niwa-session-artifacts-read-file-raw store))
            (coding-system-for-write 'no-conversion))
        (write-region before nil store nil 'silent)
        (niwa--clear-instruction-state workspace)
        (niwa-instruction-store-observe workspace)
        (write-region applied nil store nil 'silent))
      (niwa-note-revert-change uuid 0 workspace)
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file))))))

(niwa-deftest niwa-note-merge-change
  (:quiet t)
  ,test
  (test)
  :doc "merge shows three versions and confirms only resolved text into unchanged source"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my "))
    (let ((merge (niwa-note-merge-change note)))
      (with-current-buffer merge
        (should smerge-mode)
        (should (string-search "<<<<<<< yours now" (buffer-string)))
        (should (string-search "||||||| what the AI saw" (buffer-string)))
        (should (string-search ">>>>>>> AI" (buffer-string)))
        (should-error (niwa-note-merge-confirm) :type 'user-error)
        (goto-char (point-min))
        (search-forward "<<<<<<<")
        (smerge-keep-lower)
        (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))
        (niwa-note-merge-confirm))
      (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))
      (with-current-buffer source
        (should (equal "before\nnew\nafter\n" (buffer-string))))))
  :doc "multi-file merge regions never include another file's header or result"
  (niwa-note-change-test--with-note
    (let ((two (file-name-concat root "two.txt")))
      (write-region "two old\n" nil two nil 'silent)
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** Update File: two.txt\n@@\n-two old\n+two new\n*** End Patch")
      (with-current-buffer (niwa-note-merge-change note)
        (goto-char (point-min))
        (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
        (niwa-note-merge-confirm))
      (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))
      (should (equal "two new\n" (niwa-test--read-file two))))))

(niwa-deftest niwa-note-applied-history
  (:quiet t)
  ,test
  (test)
  :doc "apply durably records actual bytes under the preparing turn, preserving disjoint edits"
  (niwa-note-change-test--with-note
    (setf (niwa-session-turn-count session) 7)
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (setf (niwa-session-turn-count session) 8)
    (with-current-buffer source
      (goto-char (point-max))
      (insert "user addition\n")
      (save-buffer))
    (niwa-note-apply-change note)
    (should (equal "before\nnew\nafter\nuser addition\n" (niwa-test--read-file file)))
    (let* ((saved (niwa-session-codec-read
                   (niwa-session-artifacts-sidecar-path (niwa-session-save-path session))))
           (turns (plist-get saved :file-snapshots))
           (entry (cdr (assoc file (cdr (assq 7 turns))))))
      (should entry)
      (should-not (cdr (assq 8 turns)))
      (should (equal "before\nold\nafter\nuser addition\n"
                     (niwa-session-artifacts-read-artifact
                      session (file-name-concat "file-history" (plist-get entry :pre-backup-name)) t)))
      (should (equal "before\nnew\nafter\nuser addition\n"
                     (niwa-session-artifacts-read-artifact
                      session (file-name-concat "file-history" (plist-get entry :backup-name)) t)))))
  :doc "manual merge retains the actual unsaved source and confirmed result in file history"
  (niwa-note-change-test--with-note
    (setf (niwa-session-turn-count session) 3)
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my "))
    (with-current-buffer (niwa-note-merge-change note)
      (goto-char (point-min))
      (search-forward "<<<<<<<")
      (smerge-keep-lower)
      (goto-char (point-min))
      (search-forward "new")
      (replace-match "manual resolution")
      (niwa-note-merge-confirm))
    (let* ((saved (niwa-session-codec-read
                   (niwa-session-artifacts-sidecar-path (niwa-session-save-path session))))
           (entry (cdr (assoc file (cdr (assq 3 (plist-get saved :file-snapshots)))))))
      (should entry)
      (should (equal "before\nmy old\nafter\n"
                     (niwa-session-artifacts-read-artifact
                      session (file-name-concat "file-history" (plist-get entry :pre-backup-name)) t)))
      (should (equal "before\nmanual resolution\nafter\n"
                     (niwa-session-artifacts-read-artifact
                      session (file-name-concat "file-history" (plist-get entry :backup-name)) t))))))

(niwa-deftest niwa-note-merge-creation
  (:quiet t)
  ,test
  (test)
  :doc "merging a new file retains absence, not an empty pre-existing file"
  (niwa-note-change-test--with-note
    (let ((created (file-name-concat root "created.txt")))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Add File: created.txt\n+created\n*** End Patch")
      (with-current-buffer (niwa-note-merge-change note)
        (goto-char (point-min))
        (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
        (niwa-note-merge-confirm))
      (should (equal "created\n" (niwa-test--read-file created)))
      (let* ((saved (niwa-session-codec-read
                     (niwa-session-artifacts-sidecar-path (niwa-session-save-path session))))
             (entry (cdr (assoc created (cdr (assq 0 (plist-get saved :file-snapshots)))))))
        (should entry)
        (should-not (plist-get entry :pre-backup-name))))))

(niwa-deftest niwa-note-applied-history-gap
  (:quiet t)
  ,test
  (test)
  :doc "applying preserves an earlier capture gap instead of inventing file absence"
  (niwa-note-change-test--with-note
    (setf (niwa-session-file-snapshots session)
          (list (list 0 (cons file '(:version 1 :gap "pre-turn capture unavailable")))))
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (niwa-note-apply-change note)
    (let* ((saved (niwa-session-codec-read
                   (niwa-session-artifacts-sidecar-path (niwa-session-save-path session))))
           (entry (cdr (assoc file (cdr (assq 0 (plist-get saved :file-snapshots)))))))
      (should (equal "pre-turn capture unavailable" (plist-get entry :gap))))))

(niwa-deftest niwa-note-merge-disjoint
  (:quiet t)
  ,test
  (test)
  :doc "choosing AI for a conflicting hunk keeps disjoint user edits in the same file"
  (niwa-note-change-test--with-note
    (with-current-buffer source
      (goto-char (point-max))
      (insert "middle1\nmiddle2\nmiddle3\ntail\n")
      (save-buffer))
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my ")
      (goto-char (point-max))
      (insert "user addition\n"))
    (with-current-buffer (niwa-note-merge-change note)
      (goto-char (point-min))
      (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
      (niwa-note-merge-confirm))
    (should (equal "before\nnew\nafter\nmiddle1\nmiddle2\nmiddle3\ntail\nuser addition\n"
                   (niwa-test--read-file file)))))

(niwa-deftest niwa-note-merge-missing-git
  (:quiet t)
  ,test
  (test)
  :doc "merge refuses clearly without a local git executable and never changes source"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (let ((exec-path nil))
      (should (string-search "local git"
                             (error-message-string
                              (should-error (niwa-note-merge-change note) :type 'user-error)))))
    (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))))

(niwa-deftest niwa-note-prepared-history
  (:quiet t)
  ,test
  (test)
  :doc "an unchanged preparation capture never records later user edits as an applied change"
  (niwa-note-change-test--with-note
    (with-current-buffer data-buf
      (let* ((snapshots (make-hash-table :test #'equal))
             (niwa--current-request (niwa-request--create :file-snapshots snapshots)))
        (puthash file "before\nold\nafter\n" snapshots)
        (niwa-note-change-test--propose
         data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
        (with-current-buffer source
          (goto-char (point-max))
          (insert "user edit\n")
          (save-buffer))
        (niwa-session-artifacts-save session data-buf nil t)))
    (let ((saved (niwa-session-codec-read
                  (niwa-session-artifacts-sidecar-path (niwa-session-save-path session)))))
      (should-not (cdr (assq 0 (plist-get saved :file-snapshots)))))))

(niwa-deftest niwa-note-merge-freshness
  (:quiet t)
  ,test
  (test)
  :doc "confirmation refuses source edits made after opening and cancel preserves source and focus"
  (dolist (save '(nil t))
    (niwa-note-change-test--with-note
      (switch-to-buffer source)
      (goto-char (overlay-start note))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (let ((origin (point))
            (merge (niwa-note-merge-change note)))
        (with-current-buffer source
          (goto-char (point-max))
          (insert "later user edit\n")
          (when save (save-buffer)))
        (with-current-buffer merge
          (goto-char (point-min))
          (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
          (should-error (niwa-note-merge-confirm) :type 'user-error)
          (should (buffer-live-p merge))
          (niwa-note-merge-cancel))
        (should (eq source (window-buffer (selected-window))))
        (should (= origin (window-point (selected-window))))
        (with-current-buffer source
          (should (equal "before\nold\nafter\nlater user edit\n" (buffer-string))))
        (should (equal (if save "before\nold\nafter\nlater user edit\n" "before\nold\nafter\n")
                       (niwa-test--read-file file)))))))

(niwa-deftest niwa-note-reviewed-apply
  (:quiet t)
  ,test
  (test)
  :doc "prefix apply uses existing detailed review and records only the accepted selection"
  (niwa-note-change-test--with-note
    (let ((two (file-name-concat root "two.txt")))
      (write-region "two old\n" nil two nil 'silent)
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** Update File: two.txt\n@@\n-two old\n+two new\n*** End Patch")
      (switch-to-buffer source)
      (goto-char (overlay-start note))
      (let ((current-prefix-arg '(4))) (call-interactively #'niwa-note-apply-change))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "M two.txt")
        (niwa-patch-review-toggle-selection)
        (goto-char (point-min))
        (search-forward "ApplyPatch ·")
        (niwa-patch-review-submit))
      (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))
      (should (equal "two old\n" (niwa-test--read-file two)))
      (should-error (niwa-note-apply-change note) :type 'user-error)
      (let* ((saved (niwa-session-codec-read
                     (niwa-session-artifacts-sidecar-path (niwa-session-save-path session))))
             (entries (cdr (assq 0 (plist-get saved :file-snapshots)))))
        (should (assoc file entries))
        (should-not (assoc two entries)))))

  :doc "detailed review merges saved disjoint edits made before or during review"
  (dolist (timing '(before during))
    (niwa-note-change-test--with-note
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (when (eq timing 'during) (niwa-note-apply-change note t))
      (with-current-buffer source
        (goto-char (point-max))
        (insert "saved user edit\n")
        (save-buffer))
      (when (eq timing 'before) (niwa-note-apply-change note t))
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "ApplyPatch ·")
        (niwa-patch-review-submit))
      (should (equal "before\nnew\nafter\nsaved user edit\n" (niwa-test--read-file file)))
      (niwa-note-revert-change note 0)
      (should (equal "before\nold\nafter\nsaved user edit\n" (niwa-test--read-file file)))))
  :doc "detailed review preserves CRLF and UTF-8 bytes when merging saved edits"
  (niwa-note-change-test--with-note
    (with-current-buffer source
      (set-buffer-file-coding-system 'utf-8-dos)
      (save-buffer))
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+n\u00e9w\n*** End Patch")
    (with-current-buffer source
      (goto-char (point-max))
      (insert "saved\n")
      (save-buffer))
    (niwa-note-apply-change note t)
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "ApplyPatch ·")
      (niwa-patch-review-submit))
    (should (equal (encode-coding-string "before\nn\u00e9w\nafter\nsaved\n" 'utf-8-dos)
                   (niwa-session-artifacts-read-file-raw file))))
  :doc "detailed review preserves unsaved disjoint source text at confirmation"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (point-max))
      (insert "unsaved\n")
      (niwa-note-apply-change note t))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "ApplyPatch ·")
      (niwa-patch-review-submit))
    (should (equal "before\nnew\nafter\nunsaved\n" (niwa-test--read-file file))))
  :doc "partial review refuses disk edits inside the owning note in an unselected file"
  (niwa-note-change-test--with-note
    (let ((two (file-name-concat root "two.txt")))
      (write-region "two old\n" nil two nil 'silent)
      (with-current-buffer source
        (move-overlay note (point-min) (point-max)))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** Update File: two.txt\n@@\n-two old\n+two new\n*** End Patch")
      (niwa-note-apply-change note t)
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "M source.txt")
        (niwa-patch-review-toggle-selection)
        (with-temp-file file (insert "external before\nold\nafter\n"))
        (goto-char (point-min))
        (search-forward "ApplyPatch ·")
        (niwa-patch-review-submit))
      (should (equal "two old\n" (niwa-test--read-file two)))
      (should (eq 'stale (niwa-notes-attention-state note)))
      (should (equal "external before\nold\nafter\n" (niwa-test--read-file file))))))

(niwa-deftest niwa-note-change-replacement
  (:quiet t)
  ,test
  (test)
  :doc "newer ApplyPatch replaces waiting work while both proposals stay readable in the conversation"
  (niwa-note-change-test--with-note
    (setf (niwa-session-permission-mode session) 'full-auto)
    (with-current-buffer data-buf
      (insert "*** Improve this note\n")
      (dolist (replacement '("first proposal" "second proposal"))
        (let* ((tool (niwa-tool-ensure "ApplyPatch"))
               (provider (niwa-tool-gptel-tool tool))
               (args (list :patch (format "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+%s\n*** End Patch" replacement)))
               (gptel-include-tool-results t)
               answer)
          (niwa-pipeline-run-tool tool (lambda (value) (setq answer value)) args)
          (should (string-search "Prepared" answer))
          (goto-char (point-max))
          (gptel--display-tool-results
           (list (list provider args answer))
           (list :buffer data-buf :position (point-marker) :callback #'gptel--insert-response
                 :tools (list provider)
                 :tool-use (list (list :id replacement :name "ApplyPatch" :args args :result answer)))))))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (while (search-forward "✓ ApplyPatch" nil t)
        (let ((row (match-beginning 0)))
          (when (get-text-property row 'niwa-view-collapsed)
            (goto-char row)
            (niwa-view-toggle-section))
          (forward-line 1)))
      (should (string-search "first proposal" (buffer-string)))
      (should (string-search "second proposal" (buffer-string))))
    (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))
    (niwa-note-apply-change note)
    (should (equal "before\nsecond proposal\nafter\n" (niwa-test--read-file file)))))

(niwa-deftest niwa-note-merge-literal-separator
  (:quiet t)
  ,test
  (test)
  :doc "literal heading separators in source are not mistaken for unresolved merge markers"
  (niwa-note-change-test--with-note
    (with-current-buffer source
      (goto-char (point-max))
      (insert "Heading\n=======\n")
      (save-buffer))
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer (niwa-note-merge-change note) (niwa-note-merge-confirm))
    (should (equal "before\nnew\nafter\nHeading\n=======\n" (niwa-test--read-file file)))))

(niwa-deftest niwa-note-merge-no-final-newline
  (:quiet t)
  ,test
  (test)
  :doc "keeping the unterminated source side never saves an artificial marker separator newline"
  (niwa-note-change-test--with-note
    (with-current-buffer source
      (setq-local require-final-newline nil)
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (1- (point)) (point-max))
      (save-buffer))
    (should (equal "before\nold" (niwa-test--read-file file)))
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my "))
    (with-current-buffer (niwa-note-merge-change note)
      (goto-char (point-min))
      (search-forward "<<<<<<<")
      (smerge-keep-upper)
      (niwa-note-merge-confirm))
    (should (equal "before\nmy old" (niwa-test--read-file file)))))

(niwa-deftest niwa-note-revert-change
  (:quiet t)
  ,test
  (test)
  :doc "revert uses the actual applied pair and preserves later disjoint edits"
  (niwa-note-change-test--with-note
    (setf (niwa-session-turn-count session) 7)
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (point-max))
      (insert "before application\n")
      (save-buffer))
    (niwa-note-apply-change note)
    (with-current-buffer source
      (goto-char (point-min))
      (insert "later user edit\n")
      (save-buffer)
      (niwa-note-revert-change note 7)
      (should (equal "later user edit\nbefore\nold\nafter\nbefore application\n"
                     (buffer-string))))
    (should (equal "later user edit\nbefore\nold\nafter\nbefore application\n"
                   (niwa-test--read-file file))))
  :doc "a done note reopens only after a successful reversal, including after store reload"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (niwa-note-apply-change note)
    (let ((uuid (overlay-get note 'niwa-uuid)))
      (should-not (overlay-buffer note))
      (niwa-instruction-store-flush t)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (with-current-buffer source
        (goto-char (point-min))
        (forward-line 1)
        (delete-region (line-beginning-position) (line-end-position))
        (insert "later replacement")
        (save-buffer))
      (should-error (niwa-note-revert-change uuid 0 workspace) :type 'user-error)
      (should (= 1 (length (niwa-notes-closed workspace))))
      (with-current-buffer source
        (goto-char (point-min))
        (forward-line 1)
        (delete-region (line-beginning-position) (line-end-position))
        (insert "new")
        (save-buffer))
      (niwa-note-revert-change uuid 0 workspace)
      (should-not (niwa-notes-closed workspace))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))
      (let ((restored (niwa-note-resolve uuid workspace)))
        (should restored)
        (should (eq 'sent (overlay-get restored 'niwa-note-state))))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (should-not (niwa-notes-closed workspace))
      (should (niwa-note-resolve uuid workspace))))
  :doc "the restored source key reverts the only applied turn without offering empty turns"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (niwa-note-apply-change note)
    (niwa-session-artifacts-snapshot-modified session 1 (make-hash-table :test #'equal))
    (setq note (niwa-note-restore (overlay-get note 'niwa-uuid) workspace))
    (with-current-buffer source
      (goto-char (overlay-start note))
      (call-interactively (lookup-key niwa-note-actions-map (kbd "C-c C-b"))))
    (should (equal "before\nold\nafter\n" (niwa-test--read-file file))))
  :doc "overlap reports shifted current lines and refuses every file in the change"
  (niwa-note-change-test--with-note
    (let ((two (file-name-concat root "two.txt")))
      (write-region "two before\ntwo old\ntwo after\n" nil two nil 'silent)
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** Update File: two.txt\n@@\n-two old\n+two new\n*** End Patch")
      (niwa-note-apply-change note)
      (write-region "user heading\ntwo before\nnewer user code\ntwo after\n" nil two nil 'silent)
      (let ((err (should-error (niwa-note-revert-change note 0) :type 'user-error)))
        (should (string-search "two.txt:3-3" (error-message-string err))))
      (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))
      (should (equal "user heading\ntwo before\nnewer user code\ntwo after\n"
                     (niwa-test--read-file two)))))
  :doc "deleting a note never undoes source, and explicit revert restores creation/deletion"
  (niwa-note-change-test--with-note
    (let ((old (file-name-concat root "old.txt"))
          (new (file-name-concat root "new.txt"))
          (uuid (overlay-get note 'niwa-uuid)))
      (write-region "removed file\n" nil old nil 'silent)
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Delete File: old.txt\n*** Add File: new.txt\n+created file\n*** End Patch")
      (niwa-note-apply-change note)
      (niwa-note-delete (niwa-note-restore uuid workspace))
      (should-not (file-exists-p old))
      (should (equal "created file\n" (niwa-test--read-file new)))
      (niwa-note-revert-change uuid 0 workspace)
      (should (equal "removed file\n" (niwa-test--read-file old)))
      (should-not (file-exists-p new))
      (should (eq 'deleted (plist-get (plist-get (car (niwa-notes-closed workspace))
                                                :instruction) :closed-reason)))))
  :doc "revert of a manual merge restores its actual unsaved pre-image"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (with-current-buffer source
      (goto-char (overlay-start note))
      (insert "my "))
    (with-current-buffer (niwa-note-merge-change note)
      (goto-char (point-min))
      (search-forward "<<<<<<<")
      (smerge-keep-lower)
      (goto-char (point-min))
      (search-forward "new")
      (replace-match "resolved")
      (niwa-note-merge-confirm))
    (should (equal "before\nresolved\nafter\n" (niwa-test--read-file file)))
    (niwa-note-revert-change note 0)
    (should (equal "before\nmy old\nafter\n" (niwa-test--read-file file))))
  :doc "reverting an earlier turn leaves a later applied turn and the conversation intact"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (niwa-note-apply-change note)
    (setq note (niwa-note-restore (overlay-get note 'niwa-uuid) workspace))
    (setf (niwa-session-turn-count session) 1)
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-after\n+second turn\n*** End Patch")
    (niwa-note-apply-change note)
    (let ((history (copy-tree (niwa-session-file-snapshots session)))
          (transcript (with-current-buffer data-buf (buffer-string))))
      (niwa-note-revert-change note 0)
      (should (equal "before\nold\nsecond turn\n" (niwa-test--read-file file)))
      (should (equal history (niwa-session-file-snapshots session)))
      (should (= 1 (niwa-session-turn-count session)))
      (should (equal transcript (with-current-buffer data-buf (buffer-string))))))
  :doc "literal CRLF bytes survive session reload and later disjoint edits"
  (niwa-note-change-test--with-note
    (with-current-buffer source
      (set-buffer-file-coding-system 'utf-8-dos)
      (save-buffer))
    (should (equal "before\r\nold\r\nafter\r\n"
                   (niwa-session-artifacts-read-file-raw file)))
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (niwa-note-apply-change note)
    (should (equal "before\r\nnew\r\nafter\r\n"
                   (niwa-session-artifacts-read-file-raw file)))
    (niwa-session-persistence-lock-release (niwa-session-save-path session) session)
    (kill-buffer data-buf)
    (with-current-buffer source
      (goto-char (point-max))
      (insert "later\n")
      (save-buffer)
      (niwa-note-revert-change note 0))
    (should (equal "before\r\nold\r\nafter\r\nlater\r\n"
                   (niwa-session-artifacts-read-file-raw file))))
  :doc "revert preserves unsaved disjoint visiting edits"
  (niwa-note-change-test--with-note
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (niwa-note-apply-change note)
    (with-current-buffer source
      (goto-char (point-max))
      (insert "unsaved\n")
      (niwa-note-revert-change note 0)
      (should (equal "before\nold\nafter\nunsaved\n" (buffer-string))))
    (should (equal "before\nold\nafter\nunsaved\n" (niwa-test--read-file file))))
  :doc "revert preserves external disk edits and refuses outdated unsaved visiting text"
  (dolist (modified '(nil t))
    (niwa-note-change-test--with-note
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (niwa-note-apply-change note)
      (when modified
        (with-current-buffer source
          (goto-char (point-max))
          (insert "unsaved\n")))
      (with-temp-file file (insert "before\nnew\nexternal disk edit\n"))
      (if modified
          (progn
            (should-error (niwa-note-revert-change note 0) :type 'user-error)
            (should (equal "before\nnew\nexternal disk edit\n" (niwa-test--read-file file)))
            (with-current-buffer source
              (should (equal "before\nnew\nafter\nunsaved\n" (buffer-string)))))
        (niwa-note-revert-change note 0)
        (should (equal "before\nold\nexternal disk edit\n" (niwa-test--read-file file))))))
  :doc "capture gaps refuse rather than treating an unknown pre-image as file absence"
  (niwa-note-change-test--with-note
    (setf (niwa-session-file-snapshots session)
          (list (list 0 (cons file '(:version 1 :gap "pre-turn capture unavailable")))))
    (niwa-note-change-test--propose
     data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
    (niwa-note-apply-change note)
    (let ((err (should-error (niwa-note-revert-change note 0) :type 'user-error)))
      (should (string-search "pre-turn capture unavailable" (error-message-string err))))
    (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))))

(niwa-deftest niwa-note-revert-change/note-write
  (:quiet t :doc "an applied NoteWrite suggestion remains reversible after closing and reloading")
  (niwa-note-change-test--with-note
    (setf (niwa-session-preset-name session) 'niwa-implement)
    (with-current-buffer data-buf
      (setq result
            (niwa-tool-note-write
             (list :operations
                   (vector (list :action "create" :file_path "source.txt" :start_line 2
                                 :text "AI suggestion" :prepared_change
                                 "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"))))))
    (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get result :result)))
    (let* ((uuid (match-string 1 (plist-get result :result)))
           (suggestion (niwa-note-resolve uuid workspace)))
      (should-not (overlay-get suggestion 'niwa-note-session-id))
      (niwa-note-apply-change suggestion)
      (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))
      (should-not (overlay-buffer suggestion))
      (niwa-instruction-store-flush t)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (niwa-note-revert-change uuid nil workspace)
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))
      (should-not (niwa-notes-closed workspace)))))

(niwa-deftest niwa-note-revert-change/note-write-isolation
  (:quiet t)
  ,test
  (test)
  :doc "reverting a main-chat suggestion preserves that turn's other file edits"
  (niwa-note-change-test--with-note
    (let ((other (file-name-concat root "other.txt"))
          (snapshots (make-hash-table :test #'equal)))
      (write-region "agent-before\n" nil other nil 'silent)
      (puthash other "agent-before\n" snapshots)
      (write-region "agent-after\n" nil other nil 'silent)
      (niwa-session-artifacts-snapshot-modified session 0 snapshots)
      (setf (niwa-session-preset-name session) 'niwa-implement)
      (with-current-buffer data-buf
        (setq result
              (niwa-tool-note-write
               (list :operations
                     (vector (list :action "create" :file_path "source.txt" :start_line 2
                                   :text "AI suggestion" :prepared_change
                                   "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"))))))
      (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get result :result)))
      (let ((suggestion (niwa-note-resolve (match-string 1 (plist-get result :result)) workspace)))
        (niwa-note-apply-change suggestion)
        (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))
        (niwa-note-revert-change suggestion 0)
        (should (equal "agent-after\n" (niwa-test--read-file other)))
        (should (equal "before\nold\nafter\n" (niwa-test--read-file file))))))
  :doc "portable project revert preserves earlier edits, even after a failed store load"
  (let ((niwa-note-change-test--workspace-type 'project))
    (dolist (unreadable '(nil t))
      (niwa-note-change-test--with-note
        (should (niwa-session-codec-portable-authority-p session))
        (let ((snapshots (make-hash-table :test #'equal)))
          (puthash file "before\nold\nafter\n" snapshots)
          (with-current-buffer source
            (goto-char (point-max))
            (insert "agent addition\n")
            (save-buffer))
          (with-current-buffer data-buf
            (let ((niwa--current-request
                   (niwa-request--create :turn 0 :file-snapshots snapshots)))
              (niwa-session-artifacts-save session data-buf nil t)))
          (setf (niwa-session-preset-name session) 'niwa-implement)
          (with-current-buffer data-buf
            (setq result
                  (niwa-tool-note-write
                   (list :operations
                         (vector (list :action "create" :file_path "source.txt" :start_line 2
                                       :text "AI suggestion" :prepared_change
                                       "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"))))))
          (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get result :result)))
          (let* ((uuid (match-string 1 (plist-get result :result)))
                 (suggestion (niwa-note-resolve uuid workspace)))
            (niwa-note-apply-change suggestion)
            (should-not (overlay-buffer suggestion))
            (niwa-instruction-store-flush t)
            (let* ((store (niwa-instruction-store-path workspace))
                   (saved (niwa-test--read-file store)))
              (when unreadable
                (niwa--clear-instruction-state workspace)
                (write-region "Unreadable store" nil store nil 'silent)
                (niwa-session-artifacts-load-instructions session))
              (niwa-session-artifacts-save session data-buf nil t)
              (when unreadable
                (write-region saved nil store nil 'silent)))
            (niwa--clear-instruction-state workspace)
            (niwa-instruction-store-observe workspace)
            (niwa-note-revert-change uuid 0 workspace)
            (should (equal "before\nold\nafter\nagent addition\n" (niwa-test--read-file file)))
            (with-current-buffer source
              (should (equal "before\nold\nafter\nagent addition\n" (buffer-string))))))))))

(niwa-deftest niwa-note-apply-change/edited-capture
  (:quiet t :doc "Rewind leaves user edits made between the turn capture and suggestion apply")
  (niwa-note-change-test--with-note
    (setf (niwa-session-preset-name session) 'niwa-implement)
    (with-current-buffer data-buf
      (insert "*** Improve source\n")
      (niwa-request-begin session)
      (puthash file "before\nold\nafter\n"
               (niwa-request-file-snapshots niwa--current-request))
      (with-current-buffer source
        (goto-char (point-max))
        (insert "agent addition\n")
        (save-buffer))
      (setq result
            (niwa-tool-note-write
             (list :operations
                   (vector (list :action "create" :file_path "source.txt" :start_line 2
                                 :text "AI suggestion" :prepared_change
                                 "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))))
      (insert (propertize "Prepared.\n" 'gptel 'response))
      (setf (niwa-session-turn-count session) 1)
      (niwa-session-artifacts-save session data-buf t)
      (niwa-request-end))
    (with-current-buffer source
      (goto-char (point-min))
      (insert "USER LINE\n")
      (save-buffer))
    (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get result :result)))
    (niwa-note-apply-change
     (niwa-note-resolve (match-string 1 (plist-get result :result)) workspace))
    (let ((target (copy-sequence
                   (cdar (last (niwa-session-rewind--prompt-candidates session))))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (niwa-session-rewind-rewind data-buf target)))
    (should (equal "USER LINE\nbefore\nnew\nafter\nagent addition\n"
                   (niwa-test--read-file file)))))

(niwa-deftest niwa-note-revert-change/deleted-origin
  (:quiet t :doc "a deleted originating main chat refuses revert without touching source or reopening the note")
  (niwa-note-change-test--with-note
    (setf (niwa-session-preset-name session) 'niwa-implement)
    (with-current-buffer data-buf
      (setq result
            (niwa-tool-note-write
             (list :operations
                   (vector (list :action "create" :file_path "source.txt" :start_line 2
                                 :text "AI suggestion" :prepared_change
                                 "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"))))))
    (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get result :result)))
    (let* ((uuid (match-string 1 (plist-get result :result)))
           (suggestion (niwa-note-resolve uuid workspace)))
      (niwa-note-apply-change suggestion)
      (should-not (overlay-buffer suggestion))
      (niwa-session-persistence-lock-release (niwa-session-save-path session) session)
      (kill-buffer data-buf)
      (delete-directory (niwa-session-save-path session) t)
      (let ((before (niwa-session-artifacts-read-file-raw file))
            (text (with-current-buffer source (buffer-string)))
            (err (should-error (niwa-note-revert-change uuid nil workspace) :type 'user-error)))
        (should (string-search "conversation that made this change no longer exists"
                               (error-message-string err)))
        (should (equal before (niwa-session-artifacts-read-file-raw file)))
        (should (equal text (with-current-buffer source (buffer-string))))
        (should (= 1 (length (niwa-notes-closed workspace))))))))

(niwa-deftest niwa-note-menu/note-write
  (:quiet t :doc "the action menu offers and performs revert for an applied main-chat suggestion")
  (niwa-note-change-test--with-note
    (setf (niwa-session-preset-name session) 'niwa-implement)
    (with-current-buffer data-buf
      (setq result
            (niwa-tool-note-write
             (list :operations
                   (vector (list :action "create" :file_path "source.txt" :start_line 2
                                 :text "AI suggestion" :prepared_change
                                 "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"))))))
    (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get result :result)))
    (let ((suggestion (niwa-note-resolve (match-string 1 (plist-get result :result)) workspace)))
      (niwa-note-apply-change suggestion)
      (should-not (overlay-get suggestion 'niwa-note-session-id))
      (setq suggestion (niwa-note-restore (overlay-get suggestion 'niwa-uuid) workspace))
      (with-current-buffer source
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?r)))
          (niwa-note-menu suggestion)))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file))))))

(niwa-deftest niwa-note-menu/own-session
  (:quiet t :doc "an applied note conversation retains menu revert after unloading and store reload")
  (niwa-note-change-test--with-note
    (let ((uuid (overlay-get note 'niwa-uuid))
          (id (niwa-session-session-id session)))
      (niwa-note-change-test--propose
       data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
      (niwa-note-apply-change note)
      (niwa-session-persistence-lock-release (niwa-session-save-path session) session)
      (kill-buffer data-buf)
      (should-not (niwa-session-control-transfer-root-buffer-for-id id))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (setq note (niwa-note-restore uuid workspace))
      (with-current-buffer source
        (cl-letf (((symbol-function 'read-key)
                   (lambda (&rest _)
                     (let ((text (replace-regexp-in-string
                                  "[][]" "" (overlay-get
                                             (overlay-get note 'niwa-label-overlay)
                                             'after-string))))
                       (should (string-search "revert" (downcase text))))
                     ?r)))
          (niwa-note-menu note)))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file))))))

(niwa-deftest niwa-note-rewind-untracked-effects
  (:quiet t)
  ,test
  (test)
  :doc "a note turn that runs Bash rewinds with incomplete coverage, and a fork keeps the gap"
  (niwa-note-change-test--with-note
    (setf (niwa-session-permission-mode session) 'full-auto)
    (let (impact-text)
      (with-current-buffer data-buf
        (insert "*** Improve this note\n")
        (niwa-request-begin session)
        (let (result)
          (niwa-pipeline-run-tool-outcome
           (niwa-tool-ensure "Bash") (lambda (value) (setq result value))
           (list :command "sed -i s/old/shell/ source.txt" :description "Edit source"))
          (with-timeout (60 (ert-fail "Bash did not settle"))
            (while (null result) (accept-process-output nil 0.02))))
        (should (equal "before\nshell\nafter\n" (niwa-test--read-file file)))
        (insert (propertize "Edited.\n" 'gptel 'response))
        (setf (niwa-session-turn-count session) 1)
        (niwa-session-artifacts-save session data-buf t)
        (niwa-request-end)
        (let ((target (copy-sequence
                       (cdar (last (niwa-session-rewind--prompt-candidates session))))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _)
                       (setq impact-text (with-current-buffer "*niwa-rewind-impact*"
                                           (buffer-string)))
                       nil)))
            (niwa-session-rewind-rewind data-buf target))))
      (should (string-match-p "Checkpoint coverage: incomplete" impact-text))
      (should (string-match-p "Bash" impact-text))
      (should (equal "before\nshell\nafter\n" (niwa-test--read-file file)))
      (let* ((saved (niwa-session-codec-read
                     (niwa-session-artifacts-sidecar-path (niwa-session-save-path session))))
             (restored (plist-get (niwa-session-codec-deserialize saved workspace) :session))
             (child (niwa-session-fork-clone-session
                     restored 'fork :current-segment 1 :forked-from-turn 1)))
        (should (assoc "Bash" (cdr (assq 1 (niwa-session-untracked-effects child)))))))))

(niwa-deftest niwa-note-change/flush-points (:quiet t)
  ,test
  (test)
  :doc "preparing flushes once after the note is ready; applying flushes once after the pair is recorded"
  (niwa-note-change-test--with-note
    (let* (seen
           (spy (lambda (&rest _)
                 (push (list (niwa-notes-attention-state note)
                             (and (overlay-get note 'niwa-note-prepared-change) t)
                             (and (overlay-get note 'niwa-note-applied-changes) t))
                       seen))))
      (advice-add 'niwa-instruction-store-flush :before spy)
      (unwind-protect
          (progn
            (niwa-note-change-test--propose
             data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
            (should (equal '((ready t nil)) seen))
            (setq seen nil)
            (niwa-note-apply-change note)
            ;; Applying flushes once after the pair is recorded; the settled note
            ;; is back to its sent state and holds no prepared change.
            (should (equal '((sent nil t)) (last seen))))
        (advice-remove 'niwa-instruction-store-flush spy)))))

(niwa-deftest niwa-note-change--finish
  (:quiet t)
  ,test
  (test)
  :doc "apply closes included nested notes, leaving independent subtrees live and stale"
  (niwa-note-change-test--with-note
    (with-current-buffer source
      (move-overlay note (point-min) (point-max))
      (let* ((child (niwa-note-make source 1 8 :text "Included child"))
             (grandchild (niwa-note-make source 2 5 :text "Included grandchild" :span 'char))
             (excluded (niwa-note-make source 8 12 :text "Independent child"))
             (excluded-child (niwa-note-make source 9 10 :text "Excluded subtree" :span 'char)))
        (niwa-note-mark-sent (list child grandchild) (niwa-session-session-id session))
        (niwa-note-mark-sent (list excluded) "independent")
        (niwa-note-change-test--propose
         data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
        (let ((record (copy-tree (overlay-get note 'niwa-note-prepared-change))))
          (setq record (plist-put record :id "independent-change"))
          (setq record (plist-put record :session-id "independent"))
          (niwa-note-hold-change (list excluded) record))
        (niwa-note-apply-change note)
        (dolist (included (list note child grandchild))
          (should-not (overlay-buffer included)))
        (should (= 3 (length (niwa-notes-closed workspace))))
        (should (overlay-buffer excluded))
        (should (overlay-buffer excluded-child))
        (should (eq 'stale (niwa-notes-attention-state excluded)))
        (should (equal "independent" (overlay-get excluded 'niwa-note-session-id))))))
  :doc "applying closes a missing-source batch sibling without trying to visit it"
  (niwa-note-change-test--with-note
    (let* ((other-file (file-name-concat root "other.txt"))
           (id (niwa-session-session-id session))
           sibling uuid)
      (write-region "Other note\n" nil other-file nil 'silent)
      (with-current-buffer (find-file-noselect other-file)
        (setq-local niwa--workspace workspace)
        (setq sibling (niwa-note-create "Batch sibling")
              uuid (overlay-get sibling 'niwa-uuid))
        (niwa-note-mark-sent (list sibling) id)
        (niwa-note-change-test--propose
         data-buf "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
        (niwa-note-hold-change (list sibling) (overlay-get note 'niwa-note-prepared-change))
        (delete-file other-file)
        (niwa--stash-instructions-on-kill)
        (kill-buffer (current-buffer)))
      (should (plist-get (cl-find uuid (niwa-notes-open workspace)
                                 :key (lambda (entry) (plist-get entry :uuid)) :test #'equal)
                         :source-missing))
      (niwa-note-apply-change note)
      (should-not (niwa-notes-open workspace))
      (should-not (find-buffer-visiting other-file))
      (let* ((entry (cl-find other-file (niwa-notes-closed workspace)
                             :key (lambda (entry) (plist-get entry :file)) :test #'equal))
             (instruction (plist-get entry :instruction))
             (properties (plist-get instruction :properties)))
        (should (eq 'done (plist-get instruction :closed-reason)))
        (should-not (plist-get properties 'niwa-note-prepared-change))
        (should (equal id (plist-get (car (plist-get properties 'niwa-note-applied-changes))
                                     :session-id)))))))

(provide 'test-niwa-note-change)
;;; test-niwa-note-change.el ends here
