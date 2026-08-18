;;; test-mevedel-buddy.el --- Tests for buddy change tracking and diff assembly -*- lexical-binding: t -*-

;;; Commentary:

;; Seam 1: recorded edits in, unified diff string out.  No model, no I/O.

;;; Code:

(require 'mevedel-buddy)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defvar mevedel-test--buddy-buffers nil
  "Buffers created by the buddy tests, killed in teardown.")

(defun mevedel-test--buddy-buffer (name content &optional mode)
  "Return a tracked buffer NAME holding CONTENT in MODE."
  (let ((buf (generate-new-buffer name)))
    (push buf mevedel-test--buddy-buffers)
    (with-current-buffer buf
      (funcall (or mode #'fundamental-mode))
      (insert content)
      (mevedel-buddy--track-buffer))
    buf))

(defun mevedel-test--buddy-cleanup ()
  "Kill buddy test buffers and clear recorded state."
  (dolist (buf mevedel-test--buddy-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (mevedel-buddy--untrack-buffer))
      (kill-buffer buf)))
  (setq mevedel-test--buddy-buffers nil)
  (mevedel-buddy-clear-changes))

(defun mevedel-test--buddy-edit (buffer fn)
  "Run FN in BUFFER so the change hooks record the edit."
  (with-current-buffer buffer
    (funcall fn)))

(defun mevedel-test--buddy-all-changes ()
  "Return every recorded change across scopes, most recent first."
  (let (changes)
    (maphash (lambda (_key records) (setq changes (append changes records)))
             mevedel-buddy--changes)
    changes))

(defun mevedel-test--buddy-diff (&optional buffer)
  "Return the assembled diff for BUFFER's scope."
  (with-current-buffer (or buffer (car mevedel-test--buddy-buffers))
    (mevedel-buddy--format-changes
     (mevedel-buddy--changes-for-scope (mevedel-buddy--scope-key)))))


;;
;;; Change recording and coalescing

(mevedel-deftest mevedel-buddy--record-change
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy--record-change' records one edit for its buffer"
  (let ((buf (mevedel-test--buddy-buffer "buddy-record" "alpha\nbeta\n")))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "gamma\n")))
    (should (= 1 (length (mevedel-buddy--changes-for-scope
                          (with-current-buffer buf
                            (mevedel-buddy--scope-key)))))))

  :doc "`mevedel-buddy--record-change' coalesces edits inside the window"
  (let ((buf (mevedel-test--buddy-buffer "buddy-coalesce" "alpha\n")))
    (mevedel-test--buddy-edit
     buf (lambda ()
           (goto-char (point-max))
           (insert "be")
           (insert "ta")
           (insert "\n")))
    (should (= 1 (length (mevedel-buddy--changes-for-scope
                          (with-current-buffer buf
                            (mevedel-buddy--scope-key)))))))

  :doc "`mevedel-buddy--record-change' starts a record outside the window"
  (let ((buf (mevedel-test--buddy-buffer "buddy-window" "alpha\nbeta\ngamma\n"))
        (mevedel-buddy-coalesce-window 0))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "delta\n")))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-min)) (insert "prelude\n")))
    (should (= 2 (length (mevedel-buddy--changes-for-scope
                          (with-current-buffer buf
                            (mevedel-buddy--scope-key)))))))

  :doc "`mevedel-buddy--record-change' ignores untracked buffers"
  (let ((buf (generate-new-buffer "buddy-untracked")))
    (push buf mevedel-test--buddy-buffers)
    (with-current-buffer buf
      (insert "alpha\n")
      (should (null (mevedel-buddy--changes-for-scope
                     (mevedel-buddy--scope-key)))))))


;;
;;; Region merging

(mevedel-deftest mevedel-buddy--merge-region
  (:doc "`mevedel-buddy--merge-region' merges adjacent and enclosed edits")
  ,test
  (test)

  :doc "`mevedel-buddy--merge-region' splices a change inside the region"
  (should (equal '(10 "old\n" "aXc\n")
                 (mevedel-buddy--merge-region
                  (list :beg 10 :end 14 :old-text "old\n" :new-text "abc\n")
                  11 "b" "X")))

  :doc "`mevedel-buddy--merge-region' extends the region to the right"
  (should (equal '(10 "old\nnext\n" "abc\nmore\n")
                 (mevedel-buddy--merge-region
                  (list :beg 10 :end 14 :old-text "old\n" :new-text "abc\n")
                  14 "next\n" "more\n")))

  :doc "`mevedel-buddy--merge-region' extends the region to the left"
  (should (equal '(5 "head\nold\n" "lead\nabc\n")
                 (mevedel-buddy--merge-region
                  (list :beg 10 :end 14 :old-text "old\n" :new-text "abc\n")
                  5 "head\n" "lead\n")))

  :doc "`mevedel-buddy--merge-region' refuses a disjoint change"
  (should-not (mevedel-buddy--merge-region
               (list :beg 10 :end 14 :old-text "old\n" :new-text "abc\n")
               40 "far\n" "away\n")))


;;
;;; Diff assembly

(mevedel-deftest mevedel-buddy--format-changes
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy--format-changes' names the added line and its number"
  (let ((buf (mevedel-test--buddy-buffer
              "buddy-added.el" "(defun a ())\n(defun b ())\n")))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "(defun c ())\n")))
    (let ((diff (mevedel-test--buddy-diff buf)))
      (should (string-match-p "(defun c ())" diff))
      (should (string-match-p "^ *3 \\+" diff))))

  :doc "`mevedel-buddy--format-changes' returns empty when edits cancel out"
  (let ((buf (mevedel-test--buddy-buffer "buddy-revert" "alpha\n")))
    (mevedel-test--buddy-edit
     buf (lambda ()
           (goto-char (point-max))
           (insert "typo\n")
           (delete-region (- (point-max) 5) (point-max))))
    (should (string-empty-p (mevedel-test--buddy-diff buf))))

  :doc "`mevedel-buddy--format-changes' labels removed lines as old"
  (let ((buf (mevedel-test--buddy-buffer "buddy-removed" "alpha\nbeta\ngamma\n")))
    (mevedel-test--buddy-edit
     buf (lambda ()
           (goto-char (point-min))
           (forward-line 1)
           (delete-region (point) (line-beginning-position 2))))
    (let ((diff (mevedel-test--buddy-diff buf)))
      (should (string-match-p "old -beta" diff))
      (should-not (string-match-p "[0-9] -beta" diff))))

  :doc "`mevedel-buddy--format-changes' carries a header with mode and cursor"
  (let ((buf (mevedel-test--buddy-buffer
              "buddy-header.el" "alpha\n" #'emacs-lisp-mode)))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "beta\n")))
    (let ((diff (mevedel-test--buddy-diff buf)))
      (should (string-match-p "Buffer: buddy-header.el" diff))
      (should (string-match-p "Mode: emacs-lisp-mode" diff))
      (should (string-match-p "Cursor: line 3" diff))))

  :doc "`mevedel-buddy--format-changes' sections every buffer it is given"
  (let ((one (mevedel-test--buddy-buffer "buddy-one" "alpha\n"))
        (two (mevedel-test--buddy-buffer "buddy-two" "gamma\n")))
    (mevedel-test--buddy-edit
     one (lambda () (goto-char (point-max)) (insert "beta\n")))
    (mevedel-test--buddy-edit
     two (lambda () (goto-char (point-max)) (insert "delta\n")))
    (let ((diff (mevedel-buddy--format-changes
                 (mevedel-test--buddy-all-changes))))
      (should (string-match-p "Buffer: buddy-one" diff))
      (should (string-match-p "Buffer: buddy-two" diff))))

  :doc "`mevedel-buddy--format-changes' skips a killed buffer"
  (let ((live (mevedel-test--buddy-buffer "buddy-live" "alpha\n"))
        (dead (mevedel-test--buddy-buffer "buddy-dead" "gamma\n")))
    (mevedel-test--buddy-edit
     live (lambda () (goto-char (point-max)) (insert "beta\n")))
    (mevedel-test--buddy-edit
     dead (lambda () (goto-char (point-max)) (insert "delta\n")))
    (kill-buffer dead)
    (let ((diff (mevedel-buddy--format-changes
                 (mevedel-test--buddy-all-changes))))
      (should (string-match-p "Buffer: buddy-live" diff))
      (should-not (string-match-p "Buffer: buddy-dead" diff)))))

(provide 'test-mevedel-buddy)
;;; test-mevedel-buddy.el ends here
