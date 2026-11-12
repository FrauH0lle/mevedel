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
    (let ((entries (mevedel-notes--entries workspace)))
      (should (= 2 (length entries)))
      (let ((props (plist-get (car entries) :properties)))
        (should (equal "Review this" (plist-get props 'mevedel-note-text)))
        (should (equal '(review) (plist-get props 'mevedel-note-tags)))
        (should (eq 'agent (plist-get props 'mevedel-note-author)))
        (should (eq 'unsent (plist-get props 'mevedel-note-state)))
        (should-not (plist-get props 'mevedel-note-session-id))))))

(provide 'test-mevedel-tool-note)
;;; test-mevedel-tool-note.el ends here
