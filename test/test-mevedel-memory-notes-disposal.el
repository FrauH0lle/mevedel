;;; test-mevedel-memory-notes-disposal.el --- Permanent shared-file disposal -*- lexical-binding: t -*-

;;; Commentary:

;; Accepted housekeeping exercises the real decision/write/native deletion path.
;; Negative controls retain edited, referenced, active and unsaved files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-resource)
(require 'mevedel-session-control-fs)

(mevedel-deftest mevedel-memory-decision-apply/disposal
    (:quiet t
     :vars* ((root (file-name-as-directory (file-truename (make-temp-file "plain-disposal-" t))))
             (workspace (mevedel-workspace--create :root root))
             (shared (mevedel-resource-work-shared-directory workspace))
             (path (file-name-concat shared "folder" "capture.bin"))
             (bytes (unibyte-string 0 1 2 255))
             (mevedel-memory-dirs nil)
             claim pass proposal buffers)
     :before-each ((mevedel-workspace-identity-ensure root))
     :after-each ((dolist (buffer buffers)
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer)))
                  (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels
      ((put (file body)
         (make-directory (file-name-directory file) t)
         (let ((coding-system-for-write 'no-conversion)) (write-region body nil file nil 'silent))
         (set-file-times file (seconds-to-time (- (float-time) (* 15 86400)))))
       (prepare ()
         (put path bytes)
         (set-file-modes path #o640)
         (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
         (should claim)
         (let* ((scope (mevedel-memory-scope-capture workspace))
                (capture (mevedel-memory-notes-capture workspace nil :citation-scope scope))
                (row (plist-get capture :root)))
           (should (= 1 (plist-get capture :attachments)))
           (should-not (plist-member (cdr (assoc "folder/capture.bin" (plist-get (cdr row) :before))) :bytes))
           (setf (plist-get scope :roots) (append (plist-get scope :roots) (list row)))
           (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                  (accepted (mevedel-memory-store-accept workspace prepared nil nil "user" nil)))
             (setq pass (plist-get prepared :id) proposal (car (plist-get accepted :proposals))))))
       (apply-item () (mevedel-memory-decision-apply workspace pass (plist-get proposal :id)))
       (original ()
         (should (equal bytes (mevedel-session-control-fs-read-file path 'no-conversion)))
         (should (= #o640 (file-modes path))))
       (not-applied ()
         ;; Holds are returned statuses; nothing is decided or deleted.
         (should (eq 'held (plist-get (apply-item) :status)))
         (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
         (original)))
    ,test)
  (test)
  :doc "plain deletion prunes empty parents without blob storage, system trash, reversal or rollback"
  (let ((trash-calls 0))
    (prepare)
    (cl-letf (((symbol-function 'move-file-to-trash)
               (lambda (&rest _) (cl-incf trash-calls) (ert-fail "Unexpected system trash"))))
      (should (eq 'applied (plist-get (apply-item) :status))))
    (should (= 0 trash-calls))
    (should-not (file-exists-p path))
    (should-not (file-exists-p (file-name-directory path)))
    (should (file-directory-p shared))
    (should-not (file-exists-p (file-name-concat root ".mevedel" "state" "shared-notes" "blobs")))
    (should-error (mevedel-memory-decision-reverse workspace pass (plist-get proposal :id)) :type 'user-error)
    (let* ((write (car (mevedel-memory-write--list workspace)))
           (intent (mevedel-memory-write--read workspace (plist-get write :id) (plist-get write :hash))))
      (should intent)
      (should-error (mevedel-memory-write--rollback workspace claim nil intent) :type 'user-error))
    (should-not (file-exists-p path)))
  :doc "changed bytes after review remain untouched even with their old mtime restored"
  (progn
    (prepare)
    (let ((modified (file-attribute-modification-time (file-attributes path))))
      (write-region "changed" nil path nil 'silent)
      (set-file-times path modified))
    (should (eq 'stale (plist-get (apply-item) :status)))
    (should (equal "changed" (mevedel-session-control-fs-read-file path))))
  :doc "changed permissions after review remain untouched"
  (progn
    (prepare)
    (set-file-modes path #o600)
    (should (eq 'stale (plist-get (apply-item) :status)))
    (should (= #o600 (file-modes path))))
  :doc "active exact-file use holds deletion"
  (progn
    (prepare)
    (let ((reader (generate-new-buffer " *disposal-reader*")))
      (push reader buffers)
      (with-current-buffer reader
        (setq-local mevedel--session (mevedel-session--create :workspace workspace))
        (mevedel-resource-prepare 'read "work://shared/folder/capture.bin" (list :session mevedel--session))))
    (not-applied))
  :doc "unsaved visiting buffers hold deletion without inspecting their drafts"
  (progn
    (prepare)
    (let ((buffer (find-file-noselect path)))
      (push buffer buffers)
      (with-current-buffer buffer (goto-char (point-max)) (insert "draft")))
    (not-applied))
  :doc "a new surviving shared reference holds the reviewed attachment"
  (progn
    (prepare)
    (put (file-name-concat shared "new.json") "{\"source\": \"`work://shared/folder`\"}\n")
    (not-applied))
  :doc "an unrelated shared arrival does not hold deletion"
  (progn
    (prepare)
    (put (file-name-concat shared "new.json") "{\"source\": \"`work://shared/elsewhere`\"}\n")
    (should (eq 'applied (plist-get (apply-item) :status)))
    (should-not (file-exists-p path)))
  :doc "use beginning at the final write admission still holds deletion"
  (progn
    (prepare)
    (let ((commit (symbol-function 'mevedel-memory-notes-commit)) (entered 0))
      (cl-letf (((symbol-function 'mevedel-memory-notes-commit)
                 (lambda (&rest args)
                   (cl-incf entered)
                   (let ((reader (generate-new-buffer " *late-disposal-reader*")))
                     (push reader buffers)
                     (with-current-buffer reader
                       (setq-local mevedel--session (mevedel-session--create :workspace workspace))
                       (mevedel-resource-prepare 'read "work://shared/folder/capture.bin" (list :session mevedel--session))))
                   (apply commit args))))
        (not-applied))
      (should (= 1 entered)))))

(provide 'test-mevedel-memory-notes-disposal)
;;; test-mevedel-memory-notes-disposal.el ends here
