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
    (dolist (mode '(plan request invocation discussion preset explorer))
      (with-current-buffer data-buf
        (setf (mevedel-session-plan-mode session) (eq mode 'plan)
              (mevedel-session-preset-name session) (if (eq mode 'preset) 'mevedel-discuss 'mevedel-implement))
        (setq-local mevedel--current-request
                    (and (eq mode 'request) (mevedel-request--create :plan-read-only t))
                    mevedel--agent-invocation
                    (cond ((eq mode 'invocation) (mevedel-agent-invocation--create
                                                 :plan-read-only t :path "/root/review" :parent-session session))
                          ((eq mode 'explorer) (mevedel-agent-invocation--create
                                               :path "/root/review" :parent-session session
                                               :agent (mevedel-agent-get 'explorer))))
                    mevedel--directive-read-only-request-p (eq mode 'discussion)))
      (let* ((id (mevedel-tool-note-test--create data-buf file))
             (address (concat "notes://" id ".md")))
        (dolist (action '("edit" "done" "delete" "close" "send"))
          (let ((outcome (mevedel-tool-note-test--write
                          data-buf (vector (list :action action :id id :text "Forbidden")))))
            (should (eq 'error (plist-get outcome :status)))
            (should (string-match-p "[Rr]ead-only" (plist-get outcome :result)))))
        (should (string-search "Finding" (mevedel-tool-note-test--read data-buf address)))
        (should (string-search "unsent" (mevedel-tool-note-test--read data-buf address)))))))

(provide 'test-mevedel-tool-note)
;;; test-mevedel-tool-note.el ends here
