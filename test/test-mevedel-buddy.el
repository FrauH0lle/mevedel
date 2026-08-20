;;; test-mevedel-buddy.el --- Tests for buddy change tracking and diff assembly -*- lexical-binding: t -*-

;;; Commentary:

;; Seam 1: recorded edits in, unified diff string out.  No model, no I/O.

;;; Code:

(require 'mevedel-buddy)
(require 'mevedel-buddy-note)
(require 'mevedel-chat)
(require 'mevedel-models)
(require 'mevedel-system)
(require 'mevedel-workspace)
(require 'gptel)
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
  (when (timerp mevedel-buddy--timeout-timer)
    (cancel-timer mevedel-buddy--timeout-timer))
  (when (buffer-live-p mevedel-buddy--request-buffer)
    (kill-buffer mevedel-buddy--request-buffer))
  (dolist (buf mevedel-test--buddy-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (mevedel-buddy--untrack-buffer))
      (kill-buffer buf)))
  (setq mevedel-test--buddy-buffers nil
        mevedel-buddy--running nil
        mevedel-buddy--running-automatic nil
        mevedel-buddy--request-buffer nil
        mevedel-buddy--timeout-timer nil)
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

(defun mevedel-test--buddy-project (root name)
  "Create and return a real project NAME under ROOT."
  (let ((directory (file-name-concat root name)))
    (make-directory (file-name-concat directory ".git") t)
    directory))


;;
;;; Change recording and coalescing

(mevedel-deftest mevedel-buddy--scope-key
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "a visited buffer changing projects drops old records and resets its timer"
  (let* ((root (make-temp-file "mevedel-buddy-scope-" t))
         (project-a (mevedel-test--buddy-project root "a"))
         (project-b (mevedel-test--buddy-project root "b"))
         (file-a (file-name-concat project-a "source.el"))
         (file-b (file-name-concat project-b "source.el"))
         (buffer (mevedel-test--buddy-buffer "buddy-scope.el" "alpha\n")))
    (unwind-protect
        (progn
          (with-temp-file file-a (insert "alpha\n"))
          (with-temp-file file-b (insert "beta\n"))
          (with-current-buffer buffer
            (mevedel-buddy--untrack-buffer)
            (setq default-directory project-a)
            (set-visited-file-name file-a t)
            (mevedel-buddy--track-buffer)
            (let ((scope-a (mevedel-buddy--scope-key)))
              (goto-char (point-max))
              (insert "change\n")
              (should (mevedel-buddy--changes-for-scope scope-a))
              (let ((old-timer (run-at-time 60 nil #'ignore)))
                (setq mevedel-buddy--idle-timer old-timer
                      default-directory project-b)
                (set-visited-file-name file-b t)
                (should-not (equal scope-a (mevedel-buddy--scope-key)))
                (should-not (mevedel-buddy--changes-for-scope scope-a))
                (should mevedel-buddy--idle-timer)
                (should-not (eq old-timer mevedel-buddy--idle-timer))))))
      (delete-directory root t)))

  :doc "an unsaved buffer becoming visited drops records from its old identity"
  (let* ((root (make-temp-file "mevedel-buddy-unsaved-" t))
         (project (mevedel-test--buddy-project root "project"))
         (file (file-name-concat project "source.el"))
         (buffer (mevedel-test--buddy-buffer "buddy-unsaved" "alpha\n")))
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory project)
          (let ((scope (mevedel-buddy--scope-key)))
            (goto-char (point-max))
            (insert "change\n")
            (should (mevedel-buddy--changes-for-scope scope))
            (let ((old-timer (run-at-time 60 nil #'ignore)))
              (setq mevedel-buddy--idle-timer old-timer)
              (set-visited-file-name file t)
              (should-not (mevedel-buddy--changes-for-scope scope))
              (should-not mevedel-buddy--idle-timer)
              (should-not (memq old-timer timer-list)))))
      (delete-directory root t)))

  :doc "changing default-directory moves an unsaved buffer to the new project"
  (let* ((root (make-temp-file "mevedel-buddy-directory-" t))
         (project-a (mevedel-test--buddy-project root "a"))
         (project-b (mevedel-test--buddy-project root "b"))
         (buffer (mevedel-test--buddy-buffer "buddy-directory" "alpha\n")))
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory project-a)
          (let ((scope-a (mevedel-buddy--scope-key)))
            (goto-char (point-max))
            (insert "change\n")
            (should (mevedel-buddy--changes-for-scope scope-a))
            (setq default-directory project-b)
            (should-not (equal scope-a (mevedel-buddy--scope-key)))
            (should-not (mevedel-buddy--changes-for-scope scope-a))))
      (delete-directory root t))))

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

  :doc "`mevedel-buddy--format-changes' carries the region around the change"
  ;; A review comments on the region, not only the changed lines, so the
  ;; surrounding code has to reach the model with line numbers on it.
  (let ((buf (mevedel-test--buddy-buffer
              "buddy-region.el"
              (mapconcat (lambda (n) (format "(line %d)" n))
                         (number-sequence 1 20) "\n"))))
    (mevedel-test--buddy-edit
     buf (lambda ()
           (goto-char (point-min))
           (forward-line 9)
           (end-of-line)
           (insert " ; touched")))
    (let ((diff (mevedel-test--buddy-diff buf)))
      ;; The changed line, plus several unchanged lines either side of it.
      (should (string-match-p "touched" diff))
      (should (string-match-p "^ *5  (line 5)" diff))
      (should (string-match-p "^ *15  (line 15)" diff))
      ;; Still a region, not the whole buffer.
      (should-not (string-match-p "(line 1)$" diff))))

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

;;
;;; Review settlement

(mevedel-deftest mevedel-buddy--settle
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy--settle' retires the changes a settled review covered"
  (let ((buf (mevedel-test--buddy-buffer "settle-retire" "alpha\n")))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "beta\n")))
    (with-current-buffer buf
      (let ((scope (mevedel-buddy--scope-key)))
        (should (mevedel-buddy--changes-for-scope scope))
        (setq mevedel-buddy--running scope)
        (mevedel-buddy--settle scope (current-time))
        (should-not (mevedel-buddy--changes-for-scope scope))
        (should-not mevedel-buddy--running))))

  :doc "`mevedel-buddy--settle' keeps the changes of an abandoned review"
  (let ((buf (mevedel-test--buddy-buffer "settle-keep" "alpha\n")))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "beta\n")))
    (with-current-buffer buf
      (let ((scope (mevedel-buddy--scope-key)))
        (setq mevedel-buddy--running scope)
        (mevedel-buddy--settle scope nil)
        (should (mevedel-buddy--changes-for-scope scope))
        (should-not mevedel-buddy--running))))

  :doc "`mevedel-buddy--settle' keeps a change made while the review ran"
  (let ((buf (mevedel-test--buddy-buffer "settle-race" "alpha\n")))
    (with-current-buffer buf
      (let ((scope (mevedel-buddy--scope-key))
            (sent (current-time)))
        ;; The user keeps typing after the request went out; that edit was
        ;; never reviewed and must survive the settle.
        (mevedel-test--buddy-edit
         buf (lambda () (goto-char (point-max)) (insert "beta\n")))
        (setq mevedel-buddy--running scope)
        (mevedel-buddy--settle scope sent)
        (should (mevedel-buddy--changes-for-scope scope)))))

  :doc "`mevedel-buddy--settle' clears the annotatable buffer list"
  (progn
    (setq mevedel-buddy-note--scope-buffers '("some-buffer")
          mevedel-buddy--running "scope")
    (mevedel-buddy--settle "scope" nil)
    (should-not mevedel-buddy-note--scope-buffers)))

(mevedel-deftest mevedel-buddy-review
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-review' sends nothing when no edit was recorded"
  (let ((buf (mevedel-test--buddy-buffer "review-idle" "alpha\n")))
    (with-current-buffer buf
      (should-not (mevedel-buddy-review))))

  :doc "`mevedel-buddy-review' refuses while a review is already running"
  (let ((buf (mevedel-test--buddy-buffer "review-busy" "alpha\n")))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "beta\n")))
    (with-current-buffer buf
      (let ((mevedel-buddy--running "another-scope"))
        (should-not (mevedel-buddy-review)))))

  :doc "`mevedel-buddy-review' retires cancelling edits without a request"
  (let ((buf (mevedel-test--buddy-buffer "review-noop" "alpha\n")))
    (mevedel-test--buddy-edit
     buf (lambda ()
           (goto-char (point-max))
           (insert "typo\n")
           (delete-region (- (point-max) 5) (point-max))))
    (with-current-buffer buf
      (should-not (mevedel-buddy-review))
      (should-not (mevedel-buddy--changes-for-scope
                   (mevedel-buddy--scope-key))))))

(mevedel-deftest mevedel-buddy--severity-instruction
  (:doc "`mevedel-buddy--severity-instruction' states the configured floor")
  ,test
  (test)
  (let ((mevedel-buddy-severity-floor "critical"))
    (should (string-match-p
             "nothing below critical"
             (mevedel-buddy--severity-instruction)))))

;;
;;; Mode, tracking policy, and timers

(mevedel-deftest mevedel-buddy--tracked-buffer-p
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy--tracked-buffer-p' accepts a configured major mode"
  (let ((buf (mevedel-test--buddy-buffer "tracked.el" "" #'emacs-lisp-mode))
        (mevedel-buddy-tracked-modes '(prog-mode)))
    (should (mevedel-buddy--tracked-buffer-p buf)))

  :doc "`mevedel-buddy--tracked-buffer-p' rejects an unconfigured major mode"
  (let ((buf (mevedel-test--buddy-buffer "untracked" ""))
        (mevedel-buddy-tracked-modes '(prog-mode)))
    (should-not (mevedel-buddy--tracked-buffer-p buf)))

  :doc "`mevedel-buddy--tracked-buffer-p' rejects a mevedel session buffer"
  (let ((buf (mevedel-test--buddy-buffer "session.el" "" #'emacs-lisp-mode))
        (mevedel-buddy-tracked-modes '(prog-mode)))
    (with-current-buffer buf
      (setq-local mevedel--session 'pretend))
    (should-not (mevedel-buddy--tracked-buffer-p buf)))

  :doc "`mevedel-buddy--tracked-buffer-p' rejects an internal buffer"
  (let ((buf (mevedel-test--buddy-buffer " hidden.el" "" #'emacs-lisp-mode))
        (mevedel-buddy-tracked-modes '(prog-mode)))
    (should-not (mevedel-buddy--tracked-buffer-p buf))))

(mevedel-deftest mevedel-buddy-mode
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-mode' installs and removes the change hooks"
  (let ((buf (mevedel-test--buddy-buffer "mode-hooks.el" "" #'emacs-lisp-mode)))
    (with-current-buffer buf
      (mevedel-buddy--untrack-buffer)
      (mevedel-buddy-mode 1)
      (should (memq #'mevedel-buddy--after-change after-change-functions))
      (mevedel-buddy-mode -1)
      (should-not (memq #'mevedel-buddy--after-change after-change-functions))))

  :doc "`mevedel-buddy-mode' leaves no timer behind when disabled"
  (let ((buf (mevedel-test--buddy-buffer "mode-timer.el" "" #'emacs-lisp-mode)))
    (with-current-buffer buf
      (mevedel-buddy-mode 1)
      (mevedel-buddy--schedule)
      (should mevedel-buddy--idle-timer)
      (mevedel-buddy-mode -1)
      (should-not mevedel-buddy--idle-timer))))


;;
;;; Guidance channel

(mevedel-deftest mevedel-buddy--guide-payload
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy--guide-payload' describes the whole buffer with a header"
  (let ((buf (mevedel-test--buddy-buffer "guide-all.el" "alpha\nbeta\n")))
    (with-current-buffer buf
      (let ((payload (mevedel-buddy--guide-payload (point-min) (point-max))))
        (should (string-match-p "Buffer: guide-all.el" payload))
        (should (string-match-p "Cursor: line" payload))
        (should (string-match-p "alpha" payload))
        (should (string-match-p "beta" payload)))))

  :doc "`mevedel-buddy--guide-payload' describes only the given bounds"
  (let ((buf (mevedel-test--buddy-buffer
              "guide-region.el" "alpha\nbeta\ngamma\n")))
    (with-current-buffer buf
      (let ((payload (mevedel-buddy--guide-payload
                      (point-min)
                      (save-excursion (goto-char (point-min))
                                      (forward-line 2)
                                      (point)))))
        (should (string-match-p "alpha" payload))
        (should-not (string-match-p "gamma" payload)))))

  :doc "`mevedel-buddy--guide-payload' numbers lines from their real position"
  (let ((buf (mevedel-test--buddy-buffer
              "guide-lines.el" "alpha\nbeta\ngamma\n")))
    (with-current-buffer buf
      (let ((payload (mevedel-buddy--guide-payload
                      (save-excursion (goto-char (point-min))
                                      (forward-line 2)
                                      (point))
                      (point-max))))
        (should (string-match-p "^ *3  gamma" payload))
        (should-not (string-match-p "^ *1  gamma" payload))))))

(mevedel-deftest mevedel-buddy--preempt
  (:after-each (progn (mevedel-test--buddy-cleanup)
                      (setq mevedel-buddy--running nil
                            mevedel-buddy--running-automatic nil)))
  ,test
  (test)

  :doc "`mevedel-buddy--preempt' abandons an automatic review in flight"
  (let ((buf (mevedel-test--buddy-buffer "preempt.el" "alpha\n"))
        (request-buffer (generate-new-buffer " *buddy-preempt*")))
    (with-current-buffer buf
      (setq mevedel-buddy--running "scope"
            mevedel-buddy--running-automatic t
            mevedel-buddy--request-buffer request-buffer)
      (mevedel-buddy--preempt)
      (should-not mevedel-buddy--running)))

  :doc "`mevedel-buddy--preempt' leaves an explicit request alone"
  (let ((buf (mevedel-test--buddy-buffer "preempt-explicit.el" "alpha\n"))
        (request-buffer (generate-new-buffer " *buddy-explicit*")))
    (with-current-buffer buf
      (setq mevedel-buddy--running "scope"
            mevedel-buddy--running-automatic nil
            mevedel-buddy--request-buffer request-buffer)
      (mevedel-buddy--preempt)
      (should (equal "scope" mevedel-buddy--running))))

  :doc "`mevedel-buddy--preempt' does nothing when no review is running"
  (progn
    (setq mevedel-buddy--running nil)
    (mevedel-buddy--preempt)
    (should-not mevedel-buddy--running)))

;;
;;; Record hygiene

(mevedel-deftest mevedel-buddy-forget-buffer
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-forget-buffer' drops that buffer's records only"
  (let ((one (mevedel-test--buddy-buffer "forget-one" "alpha\n"))
        (two (mevedel-test--buddy-buffer "forget-two" "gamma\n")))
    (mevedel-test--buddy-edit
     one (lambda () (goto-char (point-max)) (insert "beta\n")))
    (mevedel-test--buddy-edit
     two (lambda () (goto-char (point-max)) (insert "delta\n")))
    (mevedel-buddy-forget-buffer "forget-one")
    (let ((names (mapcar (lambda (record) (plist-get record :buffer))
                         (mevedel-test--buddy-all-changes))))
      (should-not (member "forget-one" names))
      (should (member "forget-two" names))))

  :doc "killing a tracked buffer discards its records"
  (let ((buf (mevedel-test--buddy-buffer "forget-killed" "alpha\n")))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "beta\n")))
    (should (mevedel-test--buddy-all-changes))
    (kill-buffer buf)
    (should-not (mevedel-test--buddy-all-changes))))

(mevedel-deftest mevedel-buddy--after-change
  (:after-each (mevedel-test--buddy-cleanup)
   :doc "`mevedel-buddy--after-change' drops a change it cannot reconstruct")
  ;; Emacs does not guarantee before/after change notifications pair one
  ;; to one.  Recording a replacement with no replaced text would make
  ;; reconstruction delete real content and invent a diff.
  (let ((buf (mevedel-test--buddy-buffer "unpaired" "alpha\nbeta\n")))
    (with-current-buffer buf
      (setq mevedel-buddy--pending-beg nil
            mevedel-buddy--pending-old-text nil)
      (mevedel-buddy--after-change (point-min) (point-max) 5)
      (should-not (mevedel-buddy--changes-for-scope
                   (mevedel-buddy--scope-key))))))

(mevedel-deftest mevedel-buddy--payload-lines
  (:doc "`mevedel-buddy--payload-lines' returns the lines each buffer showed")
  ,test
  (test)
  (let ((payload (concat "=== Buffer: one.el  Mode: x  Scope: s  Cursor: line 1 ===\n"
                         "@@ -1 +1,2 @@\n"
                         "     1  alpha\n"
                         "     2 +beta\n"
                         "   old -gone\n"
                         "=== Buffer: two.el  Mode: x  Scope: s  Cursor: line 1 ===\n"
                         "    7 +gamma\n")))
    (should (equal '(("one.el" 1 2) ("two.el" 7))
                   (mevedel-buddy--payload-lines payload)))))

(mevedel-deftest mevedel-buddy--response-action
  (:doc "`mevedel-buddy--response-action' counts tool rounds and nothing else")
  (should (eq ,action (mevedel-buddy--response-action ,response)))
  (action response)
  'tool-round '(tool-result . ((tool args res)))
  ;; Nothing else may settle the review.  A streaming callback receives t
  ;; when one HTTP response ends, which with tool calls pending is not the
  ;; end of the request: settling there cleared the annotatable buffer list
  ;; before the model's own `add_note' ran, and every note was refused.
  'ignore     t
  'ignore     "prose, streamed or whole"
  'ignore     '(reasoning . "thinking")
  'ignore     '(tool-call . ((tool args cb)))
  'ignore     nil
  'ignore     'abort)

(mevedel-deftest mevedel-buddy--request-fsm
  (:doc "`mevedel-buddy--request-fsm' settles only on the terminal states")
  ,test
  (test)
  (let* ((settled 'unset)
         (fsm (mevedel-buddy--request-fsm (lambda (ok) (setq settled ok))))
         (handlers (gptel-fsm-handlers fsm)))
    ;; Every state gptel drives keeps its own handlers.
    (should (assq 'WAIT handlers))
    (should (assq 'TOOL handlers))
    ;; A non-terminal state must not settle the review, however often it
    ;; is entered while tool rounds continue.
    (dolist (handler (cdr (assq 'TOOL handlers)))
      (ignore-errors (funcall handler fsm)))
    (should (eq 'unset settled))
    ;; Success settles positively, failure and abort negatively.
    (funcall (car (last (cdr (assq 'DONE handlers)))) fsm)
    (should (eq t settled))
    (funcall (car (last (cdr (assq 'ERRS handlers)))) fsm)
    (should (eq nil settled))
    (setq settled 'unset)
    (funcall (car (last (cdr (assq 'ABRT handlers)))) fsm)
    (should (eq nil settled))))

(mevedel-deftest mevedel-buddy--request
  (:after-each (mevedel-test--buddy-cleanup)
   :quiet t)
  ,test
  (test)

  :doc "`mevedel-buddy--request' aborts only Buddy after its source dies"
  (let* ((source (mevedel-test--buddy-buffer "buddy-request.el" "alpha\n"))
         (source-directory
          (buffer-local-value 'default-directory source))
         (ordinary-fsm (gptel-make-fsm :info (list :buffer source)))
         (gptel--request-alist nil)
         request-buffer
         (ordinary-aborts 0)
         (buddy-aborts 0))
    (with-current-buffer source
      (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                 (lambda (&rest _) '(:backend mock :model mock)))
                ((symbol-function 'mevedel-buddy-note-capture-markers)
                 #'ignore)
                ((symbol-function 'mevedel-buddy-note-release-markers)
                 #'ignore)
                ((symbol-function 'mevedel-buddy-note-serialize)
                 (lambda () ""))
                ((symbol-function 'mevedel-buddy-note-tools)
                 (lambda (_) nil))
                ((symbol-function 'mevedel-buddy--telemetry) #'ignore)
                ((symbol-function 'mevedel-buddy--workspace) #'ignore)
                ((symbol-function 'mevedel-system-build-prompt)
                 (lambda (&rest _) "system"))
                ((symbol-function 'gptel-request)
                 (lambda (_prompt &rest keys)
                   (let ((fsm (plist-get keys :fsm)))
                     (setq request-buffer (plist-get keys :buffer))
                     (setf (gptel-fsm-info fsm)
                           (list :buffer request-buffer
                                 :callback (plist-get keys :callback)))
                     (setq gptel--request-alist
                           (list
                            (cons 'ordinary
                                  (cons ordinary-fsm
                                        (lambda ()
                                          (cl-incf ordinary-aborts))))
                            (cons 'buddy
                                  (cons fsm
                                        (lambda ()
                                          (cl-incf buddy-aborts))))))))))
        (mevedel-buddy--request
         "scope" 'buddy "payload" '("buddy-request.el")
         (current-time) nil)))
    (should (buffer-live-p request-buffer))
    (should-not (eq source request-buffer))
    (should (equal source-directory
                   (buffer-local-value 'default-directory request-buffer)))
    (kill-buffer source)
    (mevedel-buddy--abandon 'user)
    (should (= 0 ordinary-aborts))
    (should (= 1 buddy-aborts))
    (should (assq 'ordinary gptel--request-alist))
    (should-not (assq 'buddy gptel--request-alist))
    (should-not (buffer-live-p request-buffer)))

  :doc "the Buddy tool-round cap uses the same exact request identity"
  (let* ((source (mevedel-test--buddy-buffer "buddy-cap.el" "alpha\n"))
         (ordinary-fsm (gptel-make-fsm :info (list :buffer source)))
         (gptel--request-alist nil)
         (mevedel-buddy-max-iterations 0)
         request-buffer callback
         (ordinary-aborts 0)
         (buddy-aborts 0))
    (with-current-buffer source
      (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                 (lambda (&rest _) '(:backend mock :model mock)))
                ((symbol-function 'mevedel-buddy-note-capture-markers)
                 #'ignore)
                ((symbol-function 'mevedel-buddy-note-release-markers)
                 #'ignore)
                ((symbol-function 'mevedel-buddy-note-serialize)
                 (lambda () ""))
                ((symbol-function 'mevedel-buddy-note-tools)
                 (lambda (_) nil))
                ((symbol-function 'mevedel-buddy--telemetry) #'ignore)
                ((symbol-function 'mevedel-buddy--workspace) #'ignore)
                ((symbol-function 'mevedel-system-build-prompt)
                 (lambda (&rest _) "system"))
                ((symbol-function 'gptel-request)
                 (lambda (_prompt &rest keys)
                   (let ((fsm (plist-get keys :fsm)))
                     (setq request-buffer (plist-get keys :buffer)
                           callback (plist-get keys :callback))
                     (setf (gptel-fsm-info fsm)
                           (list :buffer request-buffer :callback callback))
                     (setq gptel--request-alist
                           (list
                            (cons 'ordinary
                                  (cons ordinary-fsm
                                        (lambda ()
                                          (cl-incf ordinary-aborts))))
                            (cons 'buddy
                                  (cons fsm
                                        (lambda ()
                                          (cl-incf buddy-aborts))))))))))
        (mevedel-buddy--request
         "scope" 'buddy "payload" '("buddy-cap.el")
         (current-time) nil)))
    (kill-buffer source)
    (funcall callback '(tool-result) nil)
    (should (= 0 ordinary-aborts))
    (should (= 1 buddy-aborts))
    (should (assq 'ordinary gptel--request-alist))
    (should-not (buffer-live-p request-buffer))))

(mevedel-deftest mevedel-buddy--current-generation-p
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "the review in flight is current"
  (let ((generation (cl-incf mevedel-buddy--generation)))
    (should (mevedel-buddy--current-generation-p generation)))

  :doc "a request outlived by its review is not"
  ;; A callback racing with cancellation must neither retire a newer
  ;; review's changes nor tear down its state.
  (let ((stale (cl-incf mevedel-buddy--generation)))
    (cl-incf mevedel-buddy--generation)
    (should-not (mevedel-buddy--current-generation-p stale))))

(mevedel-deftest mevedel-buddy--abandon
  (:after-each (mevedel-test--buddy-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy--abandon' retires the abandoned review's generation"
  (let ((buf (mevedel-test--buddy-buffer "abandon.el" "alpha\n"))
        (request-buffer (generate-new-buffer " *buddy-abandon*")))
    (with-current-buffer buf
      (setq mevedel-buddy--running "scope"
            mevedel-buddy--running-automatic t
            mevedel-buddy--request-buffer request-buffer)
      (let ((generation mevedel-buddy--generation))
        (mevedel-buddy--abandon 'timeout)
        (should-not (mevedel-buddy--current-generation-p generation)))))

  :doc "`mevedel-buddy--abandon' keeps the changes it abandoned"
  (let ((buf (mevedel-test--buddy-buffer "abandon-keep.el" "alpha\n"))
        (request-buffer (generate-new-buffer " *buddy-abandon-keep*")))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "beta\n")))
    (with-current-buffer buf
      (let ((scope (mevedel-buddy--scope-key)))
        (setq mevedel-buddy--running scope
              mevedel-buddy--running-automatic t
              mevedel-buddy--request-buffer request-buffer)
        (mevedel-buddy--abandon 'timeout)
        (should (mevedel-buddy--changes-for-scope scope))
        (should-not mevedel-buddy--running)))))

(mevedel-deftest mevedel-buddy--untrack-buffer
  (:after-each (mevedel-test--buddy-cleanup)
   :doc "`mevedel-buddy--untrack-buffer' discards the buffer's records")
  ;; Once the kill hook is gone nothing would drop them later, and a buffer
  ;; reusing the name would have those offsets replayed against unrelated
  ;; content.
  (let ((buf (mevedel-test--buddy-buffer "untrack.el" "alpha\n")))
    (mevedel-test--buddy-edit
     buf (lambda () (goto-char (point-max)) (insert "beta\n")))
    (should (mevedel-test--buddy-all-changes))
    (with-current-buffer buf (mevedel-buddy--untrack-buffer))
    (should-not (mevedel-test--buddy-all-changes))))

(provide 'test-mevedel-buddy)
;;; test-mevedel-buddy.el ends here
