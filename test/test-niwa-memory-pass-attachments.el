;;; test-niwa-memory-pass-attachments.el --- Attachment-only production passes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise preparation, complete citation-scope admission, accepted housekeeping,
;; checked permanent disposal without any inference or live workspace data.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-journal-worker)
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-pass)
(require 'niwa-memory-review)
(require 'niwa-memory-scope)
(require 'niwa-memory-store)
(require 'niwa-session-control-fs)

(niwa-deftest niwa-memory-pass-start/attachments
    (:quiet t
     :vars* ((root (make-temp-file "niwa-attachment-pass-" t))
             (workspace (niwa-workspace--create :root root))
             (shared (file-name-concat root ".niwa" "shared"))
             (memory (file-name-concat root ".niwa" "memory"))
             (index (file-name-concat memory "MEMORY.md"))
             (path (file-name-concat shared "capture.bin"))
             (bytes (unibyte-string 0 1 2 255))
             (niwa-memory-dirs (list memory))
             (niwa-memory-consolidation-mode 'auto)
             (niwa-journal-worker--child-p t)
             (model-calls 0) state result)
     :before-each ((make-directory shared t) (make-directory memory t)
                   (write-region "No retained temporary references.\n" nil index nil 'silent)
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region bytes nil path nil 'silent))
                   (set-file-modes path #o640)
                   (set-file-times path (seconds-to-time (- (float-time) (* 15 86400)))))
     :after-each ((niwa-memory-pass-stop-all) (delete-directory root t)))
  (cl-letf (((symbol-function 'niwa-memory-review-request)
             (lambda (&rest _)
               (cl-incf model-calls)
               (ert-fail "Attachment housekeeping must not dispatch inference"))))
    (cl-labels ((start ()
                 (setq state (niwa-memory-pass-start workspace (lambda (value) (setq result value))))))
      ,test))
  (test)
  :doc "a complete attachment-only pass deletes permanently without inference or retained copies"
  (progn
    (start)
    (should (eq 'success (plist-get result :outcome)))
    (should-not (plist-get result :error))
    (should (= 0 model-calls))
    (should (= 1 (plist-get result :updated-files)))
    (should-not (file-exists-p path))
    (should (file-directory-p shared))
    (let* ((pass (plist-get (plist-get state :claim) :owner))
           (accepted (niwa-memory-store-accepted workspace pass))
           (proposal (car (plist-get accepted :proposals)))
           (before (cdr (assoc "capture.bin" (plist-get proposal :before)))))
      (should (= 1 (length (plist-get accepted :proposals))))
      (should (eq 'note-delete (plist-get proposal :action)))
      (should (equal "attachment" (plist-get proposal :type)))
      (should-not (plist-get accepted :admitted-notes))
      (should-not (plist-member before :trash-file))
      (should-error (niwa-memory-decision-reverse workspace pass (plist-get proposal :id)) :type 'user-error)
      (should-not (file-exists-p path))))
  :doc "an omitted permanent citation root cannot become empty flattened text and authorize housekeeping"
  (progn
    (write-region (make-string (1+ niwa-memory-scope--max-file-bytes) ?x) nil index nil 'silent)
    (should (plist-get (niwa-memory-scope-capture workspace) :omissions))
    (start)
    (should-not (eq 'success (plist-get result :outcome)))
    (should (= 0 model-calls))
    (should-not (plist-get state :prepared))
    (should-not (plist-get result :updated-files))
    (should (equal bytes (niwa-session-control-fs-read-file path 'no-conversion)))
    (should (= #o640 (file-modes path)))
    (let ((observation (niwa-memory-notes-observation workspace)))
      (should (> (plist-get observation :blocked-count) 0))
      (should (assoc "capture.bin" (plist-get observation :blocked))))))

(provide 'test-niwa-memory-pass-attachments)
;;; test-niwa-memory-pass-attachments.el ends here
