;;; test-mevedel-memory-pass-attachments.el --- Attachment-only production passes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise preparation, complete citation-scope admission, accepted housekeeping,
;; checked private disposal and undo without any inference or live workspace data.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-journal-worker)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-review)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-store)
(require 'mevedel-session-control-fs)

(mevedel-deftest mevedel-memory-pass-start/attachments
    (:quiet t
     :vars* ((root (make-temp-file "mevedel-attachment-pass-" t))
             (workspace (mevedel-workspace--create :root root))
             (shared (file-name-concat root ".mevedel" "shared"))
             (memory (file-name-concat root ".mevedel" "memory"))
             (index (file-name-concat memory "MEMORY.md"))
             (path (file-name-concat shared "capture.bin"))
             (bytes (unibyte-string 0 1 2 255))
             (mevedel-memory-dirs (list memory))
             (mevedel-memory-consolidation-mode 'auto)
             (mevedel-journal-worker--child-p t)
             (model-calls 0) state result)
     :before-each ((make-directory shared t) (make-directory memory t)
                   (write-region "No retained temporary references.\n" nil index nil 'silent)
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region bytes nil path nil 'silent))
                   (set-file-modes path #o640)
                   (set-file-times path (seconds-to-time (- (float-time) (* 15 86400)))))
     :after-each ((mevedel-memory-pass-stop-all) (delete-directory root t)))
  (cl-letf (((symbol-function 'mevedel-memory-review-request)
             (lambda (&rest _)
               (cl-incf model-calls)
               (ert-fail "Attachment housekeeping must not dispatch inference"))))
    (cl-labels ((start ()
                 (setq state (mevedel-memory-pass-start workspace (lambda (value) (setq result value))))))
      ,test))
  (test)
  :doc "a complete attachment-only pass deletes through private storage and restores exact bytes and mode"
  (progn
    (start)
    (should (eq 'success (plist-get result :outcome)))
    (should-not (plist-get result :error))
    (should (= 0 model-calls))
    (should (= 1 (plist-get result :updated-files)))
    (should-not (file-exists-p path))
    (should (file-directory-p shared))
    (let* ((pass (plist-get (plist-get state :claim) :owner))
           (accepted (mevedel-memory-store-accepted workspace pass))
           (proposal (car (plist-get accepted :proposals)))
           (before (cdr (assoc "capture.bin" (plist-get proposal :before)))))
      (should (= 1 (length (plist-get accepted :proposals))))
      (should (eq 'note-delete (plist-get proposal :action)))
      (should (equal "attachment" (plist-get proposal :type)))
      (should-not (plist-get accepted :admitted-notes))
      (should-not (plist-member before :trash-file))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get proposal :id)) :status)))
      (should (equal bytes (mevedel-session-control-fs-read-file path 'no-conversion)))
      (should (= #o640 (file-modes path)))))
  :doc "an omitted permanent citation root cannot become empty flattened text and authorize housekeeping"
  (progn
    (write-region (make-string (1+ mevedel-memory-scope--max-file-bytes) ?x) nil index nil 'silent)
    (should (plist-get (mevedel-memory-scope-capture workspace) :omissions))
    (start)
    (should-not (eq 'success (plist-get result :outcome)))
    (should (= 0 model-calls))
    (should-not (plist-get state :prepared))
    (should-not (plist-get result :updated-files))
    (should (equal bytes (mevedel-session-control-fs-read-file path 'no-conversion)))
    (should (= #o640 (file-modes path)))
    (let ((observation (mevedel-memory-notes-observation workspace)))
      (should (> (plist-get observation :blocked-count) 0))
      (should (assoc "capture.bin" (plist-get observation :blocked))))))

(provide 'test-mevedel-memory-pass-attachments)
;;; test-mevedel-memory-pass-attachments.el ends here
