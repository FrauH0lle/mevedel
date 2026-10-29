;;; test-mevedel-tool-patch-notes.el --- Normalized note transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the actual ApplyPatch handler, native review and checked commit.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-tool-patch-registration)
(require 'mevedel-tool-patch)
(require 'mevedel-patch-review)
(require 'mevedel-pipeline)
(require 'mevedel-tool-permission)
(require 'mevedel-view)
(require 'mevedel-view-interaction)

(mevedel-deftest mevedel-tool-patch-handler/notes
  (:vars* ((root (file-name-as-directory (make-temp-file "mevedel-note-patch-" t)))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "notes"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session--create
                     :session-id "attributed-session" :workspace workspace
                     :working-directory root :permission-mode 'edits))
           (shared (mevedel-resource-work-shared-directory workspace))
           (path (file-name-concat shared "note.md"))
           (buffer (generate-new-buffer " *note-patch*"))
           result)
   :before-each
   (with-current-buffer buffer
     (setq-local default-directory root mevedel--workspace workspace
                 mevedel--session session
                 mevedel--current-request (mevedel-request--create
                                           :session session :origin "/root/worker")))
   :after-each
   (progn
     (when (buffer-live-p buffer) (kill-buffer buffer))
     (delete-directory root t)))
  ,test
  (test)
  :doc "Address and contained physical operands automatically create attributed Markdown"
  (progn
    (dolist (operand '("work://shared/note.md" ".mevedel/shared/physical.MD"))
      (with-current-buffer buffer
        (mevedel-tool-patch-handler
         (lambda (value) (setq result value))
         (list :patch (format "*** Begin Patch\n*** Add File: %s\n+# Body\n*** End Patch" operand))))
      (should-not (plist-get result :status))
      (let* ((file (if (string-prefix-p "work://" operand) path
                     (file-name-concat root operand)))
             (note (mevedel-working-note-classify file (mevedel-test--read-file file))))
        (should (eq 'managed (plist-get note :classification)))
        (should (eq 'scratch (plist-get note :kind)))
        (should (eq 'open (plist-get note :status)))
        (should (equal "attributed-session/root/worker" (plist-get note :owner)))
        (should (equal "# Body\n" (plist-get note :body)))
        (should (mevedel-working-note--timestamp-p (plist-get note :created)))
        (let ((file-data (car (plist-get (plist-get result :render-data) :files))))
          (should (string-search "+kind: scratch" (plist-get file-data :diff)))
          (should (= 7 (plist-get file-data :added)))
          (should (zerop (plist-get file-data :deleted))))))
    (should-not (mevedel-session-save-path session)))

  :doc "First unmanaged edit uses old mtime, subsequent edits preserve explicit attribution"
  (progn
    (make-directory shared t)
    (with-temp-file path (insert "old\n"))
    (set-file-times path (encode-time 0 0 0 1 1 2020 t))
    (with-current-buffer buffer
      (mevedel-tool-patch-handler
       (lambda (value) (setq result value))
       '(:patch "*** Begin Patch\n*** Update File: work://shared/note.md\n@@\n-old\n+new\n*** End Patch")))
    (should-not (plist-get result :status))
    (let ((note (mevedel-working-note-classify path (mevedel-test--read-file path))))
      (should (equal "2020-01-01T00:00:00Z" (plist-get note :created))))
    (with-current-buffer buffer
      (mevedel-tool-patch-handler
       (lambda (value) (setq result value))
       '(:patch "*** Begin Patch\n*** Update File: .mevedel/shared/note.md\n@@\n-kind: scratch\n+kind: handoff\n-status: open\n+status: done\n@@\n-new\n+final\n*** End Patch")))
    (should-not (plist-get result :status))
    (let ((note (mevedel-working-note-classify path (mevedel-test--read-file path))))
      (should (eq 'handoff (plist-get note :kind)))
      (should (eq 'done (plist-get note :status)))
      (should (equal "attributed-session/root/worker" (plist-get note :owner)))
      (should (equal "2020-01-01T00:00:00Z" (plist-get note :created)))))

  :doc "Moves into the tree normalize while moves out preserve unrelated data"
  (let ((source (file-name-concat root "outside.md"))
        (destination (file-name-concat root "left.md")))
    (with-temp-file source (insert "outside\n"))
    (with-current-buffer buffer
      (mevedel-tool-patch-handler
       (lambda (value) (setq result value))
       '(:patch "*** Begin Patch\n*** Update File: outside.md\n*** Move to: work://shared/note.md\n*** End Patch")))
    (should-not (plist-get result :status))
    (should-not (file-exists-p source))
    (should (eq 'managed (plist-get (mevedel-working-note-classify path (mevedel-test--read-file path)) :classification)))
    (should (= 6 (plist-get (car (plist-get (plist-get result :render-data) :files)) :added)))
    (let ((content (mevedel-test--read-file path)))
      (with-current-buffer buffer
        (mevedel-tool-patch-handler
         (lambda (value) (setq result value))
         '(:patch "*** Begin Patch\n*** Update File: work://shared/note.md\n*** Move to: left.md\n*** End Patch")))
      (should-not (plist-get result :status))
      (should (equal content (mevedel-test--read-file destination)))))

  :doc "Plans, memory, JSON, scripts, text and ordinary Markdown never gain metadata"
  (progn
    (setf (mevedel-session-save-path session) (file-name-concat root "session"))
    (let ((mevedel-memory-dirs '(".mevedel/memory")))
      (dolist (operand '("work://plans/current.md" "memory://local-mevedel/topic.md"
                         "work://shared/data.json" "work://shared/script.el"
                         "work://shared/plain.txt" "ordinary.md"))
        (with-current-buffer buffer
          (mevedel-tool-patch-handler
           (lambda (value) (setq result value))
           (list :patch (format "*** Begin Patch\n*** Add File: %s\n+untouched\n*** End Patch" operand))))
        (should-not (plist-get result :status))
        (should (equal "+untouched" (plist-get (car (plist-get (plist-get result :render-data) :files)) :diff))))))

  :doc "Malformed authored headers reject the entire transaction before any file write"
  (progn
    (with-current-buffer buffer
      (mevedel-tool-patch-handler
       (lambda (value) (setq result value))
       '(:patch "*** Begin Patch\n*** Add File: unrelated.txt\n+not committed\n*** Add File: work://shared/note.md\n+---\n+kind: unknown\n+---\n+Body\n*** End Patch")))
    (should (eq 'error (plist-get result :status)))
    (should (string-search "Invalid working-note kind" (plist-get result :result)))
    (should-not (file-exists-p path))
    (should-not (file-exists-p (file-name-concat root "unrelated.txt"))))

  :doc "A failed second write rolls normalized content back byte-exactly"
  (let* ((other (file-name-concat root "other.txt"))
         (original "old\n")
         (write (symbol-function 'mevedel-tool-patch--write-file))
         (attempts 0))
    (make-directory shared t)
    (with-temp-file path (insert original))
    (set-file-modes path #o640)
    (with-temp-file other (insert "before\n"))
    (cl-letf (((symbol-function 'mevedel-tool-patch--write-file)
               (lambda (&rest args)
                 (cl-incf attempts)
                 (if (= attempts 2) (error "Injected second write failure")
                   (apply write args)))))
      (with-current-buffer buffer
        (mevedel-tool-patch-handler
         (lambda (value) (setq result value))
         '(:patch "*** Begin Patch\n*** Update File: work://shared/note.md\n@@\n-old\n+new\n*** Update File: other.txt\n@@\n-before\n+after\n*** End Patch"))))
    (should (>= attempts 3))
    (should (eq 'error (plist-get result :status)))
    (should (string-search "Injected second write failure" (plist-get result :result)))
    (should (equal original (mevedel-test--read-file path)))
    (should (= #o640 (file-modes path)))
    (should (equal "before\n" (mevedel-test--read-file other)))))

(mevedel-deftest mevedel-tool-patch-normalized-preview ()
  ,test
  (test)
  :doc "Native partial review shows metadata and commits exactly its selected final bytes"
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory (make-temp-file "note-review-" t)))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "notes"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session--create :workspace workspace :permission-mode 'ask))
           (path (file-name-concat root ".mevedel/shared/note.md"))
           result proposal)
      (unwind-protect
          (progn
            (make-directory (file-name-directory path) t)
            (with-temp-file path (insert "old1\nseparator\nold2\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root mevedel--workspace workspace mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               '(:patch "*** Begin Patch\n*** Update File: work://shared/note.md\n@@\n-old1\n+new1\n@@\n-old2\n+new2\n*** End Patch")))
            (with-current-buffer view-buf
              (setq proposal (get-text-property (point-min) 'mevedel-patch-proposal))
              (unless proposal
                (setq proposal (get-text-property (next-single-property-change (point-min) 'mevedel-patch-proposal) 'mevedel-patch-proposal)))
              (should proposal)
              (should (string-search "+kind: scratch" (buffer-string)))
              (let* ((operation (car (plist-get proposal :operations)))
                     (hunks (plist-get operation :hunks)))
                (plist-put (cadr hunks) :selected nil)
                (mevedel-patch-review--render proposal)
                (let* ((body (mevedel-patch-review--body proposal))
                       (changes (mevedel-tool-patch-planned-changes proposal))
                       (expected (plist-get (car changes) :bytes)))
                  (should (string-search "Working-note final diff" body))
                  (should (string-search "+kind: scratch" body))
                  (should (string-search "+new1" body))
                  (should-not (string-search "+new2" (mevedel-tool-patch-normalized-preview proposal)))
                  (should (string-suffix-p "new1\nseparator\nold2\n" (plist-get operation :normalized-content)))
                  (mevedel-patch-review--submit proposal)
                  (should-not (plist-get result :status))
                  (should (equal expected (encode-coding-string (mevedel-test--read-file path) 'utf-8-unix)))))))
        (delete-directory root t)))))

(mevedel-deftest mevedel-tool-patch--prepare-notes ()
  ,test
  (test)
  :doc "Frozen defaults, native stale detection and unsaved edits protect normalization"
  (let* ((root (file-name-as-directory (make-temp-file "note-stale-" t)))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace))
         (path (file-name-concat root ".mevedel/shared/note.md"))
         visited)
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory root mevedel--session session)
          (make-directory (file-name-directory path) t)
          (with-temp-file path (insert "old\n"))
          (let ((proposal (mevedel-tool-patch-prepare-resources
                           (mevedel-tool-patch-parse "*** Begin Patch\n*** Update File: work://shared/note.md\n@@\n-old\n+new\n*** End Patch") t)))
            (mevedel-tool-patch--capture-baseline proposal)
            (mevedel-tool-patch--prepare-notes proposal)
            (let ((changes (mevedel-tool-patch-planned-changes proposal)))
              (setq visited (find-file-noselect path))
              (with-current-buffer visited (goto-char (point-max)) (insert "unsaved\n"))
              (should-error (mevedel-tool-patch-apply (current-buffer) changes #'ignore))
              (should (equal "old\n" (mevedel-test--read-file path)))
              (with-current-buffer visited (set-buffer-modified-p nil))
              (kill-buffer visited)
              (setq visited nil)
              (with-temp-file path (insert "external change\n"))
              (should-error (mevedel-tool-patch-assert-baseline proposal))
              (should (equal "external change\n" (mevedel-test--read-file path))))))
      (when (buffer-live-p visited)
        (with-current-buffer visited (set-buffer-modified-p nil)) (kill-buffer visited))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-work-shared-path-p ()
  ,test
  (test)
  :doc "Physical identity rejects siblings and symlink escapes without creating storage"
  (let* ((root (make-temp-file "note-identity-" t))
         (workspace (mevedel-workspace--create :root root))
         (shared (mevedel-resource-work-shared-directory workspace)))
    (unwind-protect
        (progn
          (should (mevedel-resource-work-shared-path-p workspace (file-name-concat shared "new.md")))
          (should-not (file-exists-p shared))
          (should-not (mevedel-resource-work-shared-path-p workspace shared))
          (should-not (mevedel-resource-work-shared-path-p workspace (file-name-concat root "other.md")))
          (make-directory shared t)
          (make-symbolic-link root (file-name-concat shared "escape"))
          (should-not (mevedel-resource-work-shared-path-p workspace (file-name-concat shared "escape/other.md"))))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-patch--normalize-note-content ()
  ,test
  (test)
  :doc "Valid to invalid to valid partial selection never republishes a stale browser diff"
  (let* ((root (file-name-as-directory (make-temp-file "note-invalid-selection-" t)))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace :permission-mode 'ask))
         (path (file-name-concat root ".mevedel/shared/note.md"))
         proposal)
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory root mevedel--session session)
          (make-directory (file-name-directory path) t)
          (with-temp-file path (insert "---\nkind: bad\nstatus: bad\n---\nold\n"))
          (cl-letf (((symbol-function 'mevedel-patch-review-start)
                     (lambda (prepared _callback _data-buffer) (setq proposal prepared))))
            (mevedel-tool-patch-handler
             (lambda (failure) (ert-fail failure))
             '(:patch "*** Begin Patch\n*** Update File: work://shared/note.md\n@@\n-kind: bad\n+kind: handoff\n@@\n-status: bad\n+status: done\n*** End Patch")))
          (should proposal)
          (let* ((operation (car (plist-get proposal :operations)))
                 (second (cadr (plist-get operation :hunks)))
                 (valid (plist-get operation :normalized-content))
                 (first-result (mevedel-tool-patch-result proposal nil)))
            (should (string-search "+kind: handoff" (plist-get (car (plist-get (plist-get first-result :render-data) :files)) :diff)))
            (should (= 4 (plist-get (mevedel-tool-patch-operation-stats operation) :added)))
            (should (= 2 (plist-get (mevedel-tool-patch-operation-stats operation) :deleted)))
            (plist-put second :selected nil)
            (let* ((invalid-result (mevedel-tool-patch-result proposal nil))
                   (render (plist-get invalid-result :render-data)))
              (should-not (plist-get operation :normalized-content))
              (should-not (plist-get operation :normalized-diff))
              (should (string-search "Invalid selected note metadata" (string-join (plist-get render :notes) "\n")))
              (should-not (string-search "+status: done" (plist-get (car (plist-get render :files)) :diff)))
              (should (string-search "Invalid selected note metadata" (mevedel-patch-review--body proposal)))
              (should-error (mevedel-tool-patch-planned-changes proposal)))
            (plist-put (car (plist-get operation :hunks)) :selected nil)
            (let ((deselected (mevedel-tool-patch-result proposal nil)))
              (should-not (plist-get operation :normalization-error))
              (should-not (plist-get (plist-get deselected :render-data) :files))
              (should-not (string-search "Invalid selected note metadata" (plist-get deselected :result))))
            (plist-put (car (plist-get operation :hunks)) :selected t)
            (plist-put second :selected t)
            (let ((final-result (mevedel-tool-patch-result proposal nil)))
              (should (equal valid (plist-get operation :normalized-content)))
              (should-not (plist-get (plist-get final-result :render-data) :notes))
              (should (equal (plist-get (car (plist-get (plist-get first-result :render-data) :files)) :diff)
                             (plist-get (car (plist-get (plist-get final-result :render-data) :files)) :diff))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-permission-step/note-plan-guard ()
  ,test
  (test)
  :doc "Plan rejects address and physical shared destinations before normalization or materialization"
  (let* ((root (file-name-as-directory (make-temp-file "note-plan-guard-" t)))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace :permission-mode 'edits))
         (request (mevedel-request--create :session session :origin "/root" :plan-read-only t))
         (mevedel-protected-paths nil)
         (mevedel-permission-rules nil))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory root mevedel--session session mevedel--current-request request)
          (dolist (operand '("work://shared/note.md" ".mevedel/shared/note.md"))
            (let* ((proposal (mevedel-tool-patch-prepare-resources
                              (mevedel-tool-patch-parse
                               (format "*** Begin Patch\n*** Add File: %s\n+body\n*** End Patch" operand))))
                   (context (list :tool (mevedel-tool-ensure "ApplyPatch")
                                  :patch-proposal proposal :session session
                                  :workspace workspace :request request))
                   denied)
              (mevedel-tool-permission-step
               context (lambda (_) (ert-fail "Plan unexpectedly authorized a shared write"))
               (lambda (reason &rest _) (setq denied reason)))
              (should denied)
              (should-not (plist-get (car (plist-get proposal :operations)) :working-note-p))
              (should-not (file-exists-p (file-name-concat root ".mevedel"))))))
      (delete-directory root t))))

(provide 'test-mevedel-tool-patch-notes)
;;; test-mevedel-tool-patch-notes.el ends here
