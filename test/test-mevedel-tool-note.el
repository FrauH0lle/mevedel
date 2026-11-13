;;; test-mevedel-tool-note.el -- Model note writes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the model tool with real source files and workspace sessions.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-note)
(require 'mevedel-note-write)
(require 'mevedel-session-artifacts)
(require 'mevedel-view)
(require 'mevedel-tool-fs-read)

(defun mevedel-tool-note-test--read (data address)
  "Read note ADDRESS in DATA through the model pipeline."
  (let (outcome)
    (with-current-buffer data
      (mevedel-pipeline-run-tool-outcome
       (mevedel-tool-ensure "Read") (lambda (value) (setq outcome value))
       (list :file_path address)))
    (with-timeout (10 (ert-fail "Read did not settle"))
      (while (not outcome) (accept-process-output nil 0.01)))
    (should (eq 'success (plist-get outcome :status)))
    (plist-get outcome :result)))

(defun mevedel-tool-note-test--create (data file &rest fields)
  "Create one note in FILE from DATA with FIELDS and return its UUID."
  (let ((outcome (mevedel-tool-note-test--write
                  data (vector (append (list :action "create" :file_path file
                                             :start_line 2 :text "Finding") fields)))))
    (ert-info ((format "%S" outcome))
      (should (eq 'success (plist-get outcome :status))))
    (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get outcome :result)))
    (match-string 1 (plist-get outcome :result))))

(defmacro mevedel-tool-note-test--workspace (&rest body)
  "Run BODY with a real source file and an isolated session."
  (declare (indent 0))
  `(mevedel-view-test--with-buffers
     (let* ((root (make-temp-file "note-write-" t))
            (file (file-name-concat root "source.txt"))
            (mevedel-sessions-directory (file-name-concat root "sessions"))
            (mevedel--instruction-states (make-hash-table :test #'equal))
            (mevedel-workspace--registry (make-hash-table :test #'equal))
            (mevedel-instruction-store--dirty nil)
            (mevedel-instruction-store--failed nil)
            (post-command-hook (copy-sequence post-command-hook))
            (workspace (mevedel-workspace-get-or-create 'file root root "notes"))
            (session (mevedel-session-create "notes" workspace root))
            (old-buffers (buffer-list)))
       (unwind-protect
           (progn
             (make-directory (file-name-concat root ".mevedel"))
             (write-region "before\nold\nafter\n" nil file nil 'silent)
             (with-current-buffer data-buf
               (setq-local default-directory (file-name-as-directory root)
                           mevedel--workspace workspace mevedel--session session))
             (mevedel-session-artifacts-save session data-buf nil t)
             (save-window-excursion ,@body))
         (dolist (buffer (cl-set-difference (buffer-list) old-buffers))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil)
               (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
         (mevedel-workspace-clear-registry)
         (delete-directory root t)))))

(defun mevedel-tool-note-test--write (data operations)
  "Call NoteWrite with OPERATIONS in DATA through the model pipeline."
  (let ((tool (mevedel-tool-ensure "NoteWrite")) result)
    (should tool)
    (with-current-buffer data
      (mevedel-pipeline-run-tool-outcome
       tool (lambda (value) (setq result value)) (list :operations operations)))
    (with-timeout (10 (ert-fail "NoteWrite did not settle"))
      (while (not result) (accept-process-output nil 0.01)))
    result))

(mevedel-deftest mevedel-tool-note-register ()
  ,test
  (test)
  :doc "the model contract reserves close, send and done for the user's request"
  (let ((prompt (mevedel-tool-prompt (mevedel-tool-ensure "NoteWrite"))))
    (should (string-match-p
             "Do not close notes, send them or mark them done unless the user asks" prompt)))
  :doc "the maintained NoteWrite contract reserves close for the user's request"
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat (file-name-directory (locate-library "mevedel-tool-note"))
                       "docs/tools.md"))
    (should (string-match-p "Closing, sending and marking[\n ]+done require the user's request"
                            (buffer-string)))))

(mevedel-deftest mevedel-tool-note-write (:quiet t)
  ,test
  (test)
  :doc "creates agent-authored tagged notes durably with one sync per batch"
  (mevedel-tool-note-test--workspace
    (let* ((syncs 0)
          (counter (lambda (&rest _) (cl-incf syncs))))
      (advice-add 'mevedel-instruction-store-sync :before counter)
      (unwind-protect
          (let ((outcome (mevedel-tool-note-test--write
                          data-buf
                          (vector (list :action "create" :file_path file :start_line 2
                                        :text "Review this" :tags ["review"])
                                  (list :action "create" :file_path file :start_line 3
                                        :text "Second finding")))))
            (ert-info ((format "%S" outcome))
              (should (eq 'success (plist-get outcome :status))))
            (should (string-search "notes://" (plist-get outcome :result)))
            (should (= 1 syncs)))
        (advice-remove 'mevedel-instruction-store-sync counter)))
    (kill-buffer (find-buffer-visiting file))
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (let* ((listing (mevedel-tool-note-test--read data-buf "notes://"))
           (document (progn
                       (should (string-match "notes://[a-z0-9-]+\\.md" listing))
                       (mevedel-tool-note-test--read data-buf (match-string 0 listing)))))
      (should (string-search "#2" listing))
      (dolist (expected '("Review this" "review" "agent" "unsent"))
        (should (string-search expected document)))))
  :doc "edits text and replaces tags, then retains done and deleted notes after reload"
  (mevedel-tool-note-test--workspace
    (let* ((id (mevedel-tool-note-test--create data-buf file))
           (address (concat "notes://" id ".md")))
      (should (eq 'success (plist-get
                           (mevedel-tool-note-test--write
                            data-buf (vector (list :action "edit" :id id
                                                   :text "Updated finding" :tags ["fix"]))) :status)))
      (let ((text (mevedel-tool-note-test--read data-buf address)))
        (should (string-search "Updated finding" text))
        (should (string-search "fix" text)))
      (dolist (action '("done" "delete" "close"))
        (should (eq 'success (plist-get
                             (mevedel-tool-note-test--write
                              data-buf (vector (list :action action :id id))) :status)))
        (kill-buffer (find-buffer-visiting file))
        (mevedel--clear-instruction-state workspace)
        (let ((text (mevedel-tool-note-test--read data-buf address)))
          (should (string-search "closed" text))
          (should (string-search (if (equal action "delete") "deleted" "done") text)))
        (mevedel-note-restore id workspace))
      (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))))
  :doc "read-only owners may create findings but cannot edit, send or close existing notes"
  (mevedel-tool-note-test--workspace
    (dolist (mode '(plan request invocation explorer))
      (with-current-buffer data-buf
        (setf (mevedel-session-plan-mode session) (eq mode 'plan)
              (mevedel-session-preset-name session) 'mevedel-implement)
        (setq-local mevedel--current-request
                    (and (eq mode 'request) (mevedel-request--create :plan-read-only t))
                    mevedel--agent-invocation
                    (cond ((eq mode 'invocation) (mevedel-agent-invocation--create
                                                 :plan-read-only t :path "/root/review" :parent-session session))
                          ((eq mode 'explorer) (mevedel-agent-invocation--create
                                               :path "/root/review" :parent-session session
                                               :agent (mevedel-agent-get 'explorer))))))
      (let* ((id (mevedel-tool-note-test--create data-buf file))
             (address (concat "notes://" id ".md")))
        (dolist (action '("edit" "done" "delete" "close" "send"))
          (let ((outcome (mevedel-tool-note-test--write
                          data-buf (vector (list :action action :id id :text "Forbidden")))))
            (should (eq 'error (plist-get outcome :status)))
            (should (string-match-p "[Rr]ead-only" (plist-get outcome :result)))))
        (should (string-search "Finding" (mevedel-tool-note-test--read data-buf address)))
        (should (string-search "unsent" (mevedel-tool-note-test--read data-buf address))))))
  :doc "a created note carries a durable prepared change that only the user applies"
  (mevedel-tool-note-test--workspace
    (setf (mevedel-session-plan-mode session) t)
    (let* ((id (mevedel-tool-note-test--create
                data-buf file :prepared_change
                "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))
      (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))
      (kill-buffer (find-buffer-visiting file))
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (let ((note (mevedel-notes--resolve id workspace)))
        (should (string-search "change ready"
                               (overlay-get (overlay-get note 'mevedel-label-overlay) 'after-string)))
        (mevedel-note-apply-change note)
        (should (equal "before\nnew\nafter\n" (mevedel-test--read-file file)))
        (should (string-search "unsent"
                               (mevedel-tool-note-test--read data-buf (concat "notes://" id ".md")))))))
  :doc "creation refuses to steal a source buffer from a different workspace"
  (mevedel-tool-note-test--workspace
    (let* ((nested (file-name-concat root "nested"))
           (other (mevedel-workspace-get-or-create 'file nested nested "other")))
      (with-current-buffer (find-file-noselect file)
        (setq-local mevedel--workspace other))
      (let ((outcome (mevedel-tool-note-test--write
                      data-buf (vector (list :action "create" :file_path file :start_line 2)))))
        (should (eq 'error (plist-get outcome :status)))
        (should (string-search "different workspace" (plist-get outcome :result))))
      (with-current-buffer (find-buffer-visiting file)
        (should (eq other mevedel--workspace)))))
  :doc "prepared suggestions respect ApplyPatch hard-deny path rules even in Plan"
  (mevedel-tool-note-test--workspace
    (setf (mevedel-session-plan-mode session) t
          (mevedel-session-permission-rules session)
          (list (list "ApplyPatch" :path file :action 'deny)))
    (let ((outcome (mevedel-tool-note-test--write
                    data-buf
                    (vector (list :action "create" :file_path file :start_line 2
                                  :prepared_change
                                  "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))))
      (should (eq 'error (plist-get outcome :status)))
      (should (string-search "Prepared change is denied" (plist-get outcome :result)))
      (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))))
  :doc "partial failure reports already-created UUIDs and persists those notes once"
  (mevedel-tool-note-test--workspace
    (let ((outcome (mevedel-tool-note-test--write
                    data-buf
                    (vector (list :action "create" :file_path file :start_line 2 :text "Kept finding")
                            (list :action "create" :file_path file :start_line 999)))))
      (should (eq 'error (plist-get outcome :status)))
      (should (string-search "Completed" (plist-get outcome :result)))
      (should (string-match "notes://[a-z0-9-]+\\.md" (plist-get outcome :result)))
      (let ((address (match-string 0 (plist-get outcome :result))))
        (kill-buffer (find-buffer-visiting file))
        (mevedel--clear-instruction-state workspace)
        (should (string-search "Kept finding" (mevedel-tool-note-test--read data-buf address)))))))

(mevedel-deftest mevedel-tool-note-write/guards (:quiet t)
  ,test
  (test)
  :doc "create refuses workspace escapes and absolute external paths without changes"
  (mevedel-tool-note-test--workspace
    (let ((outside (make-temp-file "note-outside-" nil ".txt" "outside\n")))
      (unwind-protect
          (dolist (path (list (file-relative-name outside root) outside "/etc/hosts"))
            (let ((before (mevedel-test--read-file (expand-file-name path root)))
                  (outcome (mevedel-tool-note-test--write
                            data-buf (vector (list :action "create" :file_path path
                                                   :start_line 1 :text "Forbidden")))))
              (should (eq 'error (plist-get outcome :status)))
              (should (equal before (mevedel-test--read-file (expand-file-name path root))))
              (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))))
        (delete-file outside))))
  :doc "prepared changes refuse protected source paths without writing them"
  (mevedel-tool-note-test--workspace
    (let* ((mevedel-protected-paths (list (cons file 'read-only)))
           (outcome (mevedel-tool-note-test--write
                     data-buf (vector (list :action "create" :file_path file :start_line 2
                                            :prepared_change
                                            "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))))
      (should (eq 'error (plist-get outcome :status)))
      (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))))
  :doc "prepared resource writes and moves refuse without changing source"
  (mevedel-tool-note-test--workspace
    (dolist (patch '("*** Begin Patch\n*** Add File: work://shared/refused.md\n+forbidden\n*** End Patch"
                     "*** Begin Patch\n*** Update File: source.txt\n*** Move to: work://shared/refused.md\n@@\n-old\n+new\n*** End Patch"))
      (let ((outcome (mevedel-tool-note-test--write
                      data-buf (vector (list :action "create" :file_path file
                                             :start_line 2 :prepared_change patch)))))
        (should (eq 'error (plist-get outcome :status)))
        (should (string-search "not resources" (plist-get outcome :result)))
        (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))))))

(mevedel-deftest mevedel-tool-note-write/line-bounds (:quiet t)
  ,test
  (test)
  :doc "a trailing newline does not create an addressable fourth source line"
  (mevedel-tool-note-test--workspace
    (dolist (range '((4 4) (3 4)))
      (let ((outcome (mevedel-tool-note-test--write
                      data-buf (vector (list :action "create" :file_path file
                                             :start_line (car range) :end_line (cadr range))))))
        (should (eq 'error (plist-get outcome :status)))
        (should (string-search "beyond source" (plist-get outcome :result)))
        (should (equal "before\nold\nafter\n" (mevedel-test--read-file file)))))))

(mevedel-deftest mevedel-tool-note-write/create-cost (:quiet t)
  ,test
  (test)
  :doc "creating text notes renders twice each without redundant edits, including tags"
  (mevedel-tool-note-test--workspace
    (let* ((renders 0) (edits 0)
           (count-render (lambda (&rest _) (cl-incf renders)))
           (count-edit (lambda (&rest _) (cl-incf edits))))
      (advice-add 'mevedel-notes--render :before count-render)
      (advice-add 'mevedel-note-edit :before count-edit)
      (unwind-protect
          (let ((outcome (mevedel-tool-note-test--write
                          data-buf (vector (list :action "create" :file_path file
                                                 :start_line 1 :text "Plain")
                                           (list :action "create" :file_path file
                                                 :start_line 2 :text "Tagged" :tags ["review"])))))
            (should (eq 'success (plist-get outcome :status)))
            (should (= 0 edits))
            (should (= 4 renders)))
        (advice-remove 'mevedel-notes--render count-render)
        (advice-remove 'mevedel-note-edit count-edit)))
    (with-current-buffer (find-buffer-visiting file)
      (dolist (label (cl-remove-if-not #'stringp
                                     (mapcar (lambda (overlay) (overlay-get overlay 'after-string))
                                             (overlays-in (point-min) (point-max)))))
        (should (string-match-p "AI" label))
        (should-not (string-match-p "you" label))))))

(mevedel-deftest mevedel-note-dismiss-agent-notes (:quiet t)
  ,test
  (test)
  :doc "single and workspace-wide dismissal preserve user notes and sync a batch once"
  (mevedel-tool-note-test--workspace
    (let ((first (mevedel-tool-note-test--create data-buf file))
          (second (mevedel-tool-note-test--create data-buf file)))
      (with-current-buffer (find-buffer-visiting file)
        (goto-char (point-min))
        (mevedel-note-create "Human note"))
      (mevedel-note-dismiss (mevedel-notes--resolve first workspace))
      (should (string-search "dismissed" (mevedel-tool-note-test--read
                                          data-buf (concat "notes://" first ".md"))))
      (mevedel-note-restore first workspace)
      (mevedel-instruction-store-flush t)
      (kill-buffer (find-buffer-visiting file))
      (mevedel--clear-instruction-state workspace)
      (let* ((syncs 0) (counter (lambda (&rest _) (cl-incf syncs))))
        (advice-add 'mevedel-instruction-store-sync :before counter)
        (unwind-protect
            (progn
              (should (= 2 (mevedel-note-dismiss-agent-notes workspace)))
              (should (= 1 syncs)))
          (advice-remove 'mevedel-instruction-store-sync counter)))
      (dolist (id (list first second))
        (should (string-search "dismissed" (mevedel-tool-note-test--read
                                            data-buf (concat "notes://" id ".md")))))
      (let ((listing (mevedel-tool-note-test--read data-buf "notes://")))
        (should (string-match-p "user unsent" listing)))))
  :doc "missing sources refuse dismiss-all before it can dismiss a different note at point"
  (mevedel-tool-note-test--workspace
    (mevedel-tool-note-test--create data-buf file)
    (let* ((other (file-name-concat root "other.txt"))
           (_ (write-region "before\nold\nafter\n" nil other nil 'silent))
           (id (mevedel-tool-note-test--create data-buf other)))
      (kill-buffer (find-buffer-visiting file))
      (delete-file file)
      (with-current-buffer (find-buffer-visiting other)
        (goto-char (point-min))
        (forward-line 1)
        (should-error (mevedel-note-dismiss-agent-notes workspace) :type 'user-error))
      (should (string-search "unsent" (mevedel-tool-note-test--read
                                       data-buf (concat "notes://" id ".md")))))))

(mevedel-deftest mevedel-note-dismiss-agent-notes/lookup-cost (:quiet t)
  ,test
  (test)
  :doc "bulk dismissal collects entries once and restores each source once"
  (mevedel-tool-note-test--workspace
    (mevedel-tool-note-test--create data-buf file)
    (mevedel-tool-note-test--create data-buf file)
    (kill-buffer (find-buffer-visiting file))
    (let* ((builds 0) (restores 0)
           (count-build (lambda (&rest _) (cl-incf builds)))
           (count-restore (lambda (&rest _) (cl-incf restores))))
      (advice-add 'mevedel-notes--entries :before count-build)
      (advice-add 'mevedel--restore-file-instructions :before count-restore)
      (unwind-protect
          (progn
            (should (= 2 (mevedel-note-dismiss-agent-notes workspace)))
            (should (= 1 builds))
            (should (= 1 restores)))
        (advice-remove 'mevedel-notes--entries count-build)
        (advice-remove 'mevedel--restore-file-instructions count-restore)))))

(provide 'test-mevedel-tool-note)
;;; test-mevedel-tool-note.el ends here
