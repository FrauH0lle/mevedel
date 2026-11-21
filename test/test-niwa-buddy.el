;;; test-niwa-buddy.el --- Tests for buddy change tracking and diff assembly -*- lexical-binding: t -*-

;;; Commentary:

;; Seam 1: recorded edits in, unified diff string out.  No model, no I/O.

;;; Code:

(require 'niwa-buddy)
(require 'niwa-buddy-note)
(require 'niwa-chat)
(require 'niwa-models)
(require 'niwa-persistence)
(require 'niwa-system)
(require 'niwa-workspace)
(require 'gptel)
(require 'niwa-claude-code)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-engine-test-support"))


;;
;;; Helpers

(defvar niwa-test--buddy-buffers nil
  "Buffers created by the buddy tests, killed in teardown.")

(niwa-deftest niwa-buddy-guide/claude
    (:quiet t
     :vars* ((source (niwa-test--buddy-buffer "buddy-claude.el" "alpha\n"))
             (foreign (niwa-test--buddy-buffer "buddy-foreign.el" "beta\n"))
             (gptel--known-backends nil)
             (niwa-model-workloads '((buddy :provider "Claude Code:sonnet" :effort nil)))
             (niwa-buddy-note--notes nil)
             (niwa-buddy-max-iterations 8)
             (niwa-claude-code-directory (make-temp-file "niwa-buddy-claude-" t))
             models batches)
     :after-each ((niwa-test--buddy-cleanup)
                  (delete-directory niwa-claude-code-directory t)))
  (progn
    (niwa-claude-code-register)
    (with-current-buffer source
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system _mcp model _effort &optional id _hook)
                    (should-not id) (push model models)
                    (list :meta `((toolBatches . ,batches)))))))
        (cl-labels ((note (id buffer text)
                      `((name . "add_note") (id . ,id)
                        (args . ((buffer . ,(buffer-name buffer)) (line_number . 1)
                                 (note . ,text) (severity . "significant")))))
                    (run (command)
                      (funcall command)
                      (with-timeout (5 (ert-fail "Subscription Buddy did not settle"))
                        (while niwa-buddy--running (accept-process-output nil 0.01)))
                      (should (equal '("sonnet") models))
                      (should-not niwa-buddy-note--run)
                      (should-not niwa-buddy--request-buffer)))
          ,test))))
  (test)
  :doc "Claude guidance can annotate only the captured source buffer"
  (progn
    (setq batches (vector (vector (note "note-owned" source "Check this expression")
                                  (note "note-foreign" foreign "Must not appear"))))
    (run #'niwa-buddy-guide)
    (should (= 1 (length niwa-buddy-note--notes)))
    (let ((note (car niwa-buddy-note--notes)))
      (should (equal "Check this expression" (plist-get note :note)))
      (should (eq source (overlay-buffer (plist-get note :overlay))))))
  :doc "the native batch boundary stops further notes and preserves unreviewed edits"
  (progn
    (goto-char (point-max)) (insert "beta\n")
    (should (niwa-test--buddy-all-changes))
    (setq niwa-buddy-max-iterations 0
          batches (vector (vector (note "first" source "First note"))
                          (vector (note "late" source "Must not appear"))))
    (run #'niwa-buddy-review)
    (should (= 1 (length niwa-buddy-note--notes)))
    (should (equal "First note" (plist-get (car niwa-buddy-note--notes) :note)))
    (should (niwa-test--buddy-all-changes))))

(defun niwa-test--buddy-buffer (name content &optional mode)
  "Return a tracked buffer NAME holding CONTENT in MODE."
  (let ((buf (generate-new-buffer name)))
    (push buf niwa-test--buddy-buffers)
    (with-current-buffer buf
      (funcall (or mode #'fundamental-mode))
      (insert content)
      (niwa-buddy--track-buffer))
    buf))

(defun niwa-test--buddy-cleanup ()
  "Kill buddy test buffers and clear recorded state."
  (when niwa-buddy--running (niwa-buddy--abandon 'test-cleanup))
  (when (timerp niwa-buddy--timeout-timer)
    (cancel-timer niwa-buddy--timeout-timer))
  (when (buffer-live-p niwa-buddy--request-buffer)
    (kill-buffer niwa-buddy--request-buffer))
  (dolist (buf niwa-test--buddy-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (niwa-buddy--untrack-buffer))
      (kill-buffer buf)))
  (setq niwa-test--buddy-buffers nil
        niwa-buddy--running nil
        niwa-buddy--running-automatic nil
        niwa-buddy--request-buffer nil
        niwa-buddy--timeout-timer nil)
  (niwa-buddy-clear-changes)
  (clrhash niwa-buddy--briefs))

(defun niwa-test--buddy-all-changes ()
  "Return every recorded change across scopes, most recent first."
  (let (changes)
    (maphash (lambda (_key records) (setq changes (append changes records)))
             niwa-buddy--changes)
    changes))

(defun niwa-test--buddy-diff (&optional buffer)
  "Return the assembled diff for BUFFER's scope."
  (with-current-buffer (or buffer (car niwa-test--buddy-buffers))
    (niwa-buddy--format-changes
     (niwa-buddy--changes-for-scope (niwa-buddy--scope-key)))))

(defun niwa-test--buddy-project (root name)
  "Create and return a real project NAME under ROOT."
  (let ((directory (file-name-concat root name)))
    (make-directory (file-name-concat directory ".git") t)
    directory))


;;
;;; Change recording and coalescing

(niwa-deftest niwa-buddy--scope-key
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "a visited buffer changing projects drops old records and resets its timer"
  (let* ((root (make-temp-file "niwa-buddy-scope-" t))
         (project-a (niwa-test--buddy-project root "a"))
         (project-b (niwa-test--buddy-project root "b"))
         (file-a (file-name-concat project-a "source.el"))
         (file-b (file-name-concat project-b "source.el"))
         (buffer (niwa-test--buddy-buffer "buddy-scope.el" "alpha\n")))
    (unwind-protect
        (progn
          (with-temp-file file-a (insert "alpha\n"))
          (with-temp-file file-b (insert "beta\n"))
          (with-current-buffer buffer
            (niwa-buddy--untrack-buffer)
            (setq default-directory project-a)
            (set-visited-file-name file-a t)
            (niwa-buddy--track-buffer)
            (let ((scope-a (niwa-buddy--scope-key)))
              (goto-char (point-max))
              (insert "change\n")
              (should (niwa-buddy--changes-for-scope scope-a))
              (let ((old-timer (run-at-time 60 nil #'ignore)))
                (setq niwa-buddy--idle-timer old-timer
                      default-directory project-b)
                (set-visited-file-name file-b t)
                (should-not (equal scope-a (niwa-buddy--scope-key)))
                (should-not (niwa-buddy--changes-for-scope scope-a))
                (should niwa-buddy--idle-timer)
                (should-not (eq old-timer niwa-buddy--idle-timer))))))
      (delete-directory root t)))

  :doc "an unsaved buffer becoming visited drops records from its old identity"
  (let* ((root (make-temp-file "niwa-buddy-unsaved-" t))
         (project (niwa-test--buddy-project root "project"))
         (file (file-name-concat project "source.el"))
         (buffer (niwa-test--buddy-buffer "buddy-unsaved" "alpha\n")))
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory project)
          (let ((scope (niwa-buddy--scope-key)))
            (goto-char (point-max))
            (insert "change\n")
            (should (niwa-buddy--changes-for-scope scope))
            (let ((old-timer (run-at-time 60 nil #'ignore)))
              (setq niwa-buddy--idle-timer old-timer)
              (set-visited-file-name file t)
              (should-not (niwa-buddy--changes-for-scope scope))
              (should-not niwa-buddy--idle-timer)
              (should-not (memq old-timer timer-list)))))
      (delete-directory root t)))

  :doc "changing default-directory moves an unsaved buffer to the new project"
  (let* ((root (make-temp-file "niwa-buddy-directory-" t))
         (project-a (niwa-test--buddy-project root "a"))
         (project-b (niwa-test--buddy-project root "b"))
         (buffer (niwa-test--buddy-buffer "buddy-directory" "alpha\n")))
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory project-a)
          (let ((scope-a (niwa-buddy--scope-key)))
            (goto-char (point-max))
            (insert "change\n")
            (should (niwa-buddy--changes-for-scope scope-a))
            (setq default-directory project-b)
            (should-not (equal scope-a (niwa-buddy--scope-key)))
            (should-not (niwa-buddy--changes-for-scope scope-a))))
      (delete-directory root t))))

(niwa-deftest niwa-buddy-directory-scope-key
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)
  :doc "a session directory and a source buffer in one project share a key"
  (let* ((root (make-temp-file "niwa-buddy-dirkey-" t))
         (project (niwa-test--buddy-project root "project"))
         (nested (file-name-concat project "lisp"))
         (buffer (niwa-test--buddy-buffer "buddy-dirkey.el" "alpha\n")))
    (unwind-protect
        (progn
          (make-directory nested)
          (with-current-buffer buffer
            (setq default-directory (file-name-as-directory nested))
            (should (equal (niwa-buddy--scope-key)
                           (niwa-buddy-directory-scope-key project))))
          (should-not (niwa-buddy-directory-scope-key root)))
      (delete-directory root t))))

(niwa-deftest niwa-buddy--record-change
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy--record-change' records one edit for its buffer"
  (let ((buf (niwa-test--buddy-buffer "buddy-record" "alpha\nbeta\n")))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "gamma\n"))
    (should (= 1 (length (niwa-buddy--changes-for-scope
                          (with-current-buffer buf
                            (niwa-buddy--scope-key)))))))

  :doc "`niwa-buddy--record-change' coalesces edits inside the window"
  (let ((buf (niwa-test--buddy-buffer "buddy-coalesce" "alpha\n")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "be")
      (insert "ta")
      (insert "\n"))
    (should (= 1 (length (niwa-buddy--changes-for-scope
                          (with-current-buffer buf
                            (niwa-buddy--scope-key)))))))

  :doc "`niwa-buddy--record-change' starts a record outside the window"
  (let ((buf (niwa-test--buddy-buffer "buddy-window" "alpha\nbeta\ngamma\n"))
        (niwa-buddy-coalesce-window 0))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "delta\n"))
    (with-current-buffer buf
      (goto-char (point-min)) (insert "prelude\n"))
    (should (= 2 (length (niwa-buddy--changes-for-scope
                          (with-current-buffer buf
                            (niwa-buddy--scope-key)))))))

  :doc "`niwa-buddy--record-change' ignores untracked buffers"
  (let ((buf (generate-new-buffer "buddy-untracked")))
    (push buf niwa-test--buddy-buffers)
    (with-current-buffer buf
      (insert "alpha\n")
      (should (null (niwa-buddy--changes-for-scope
                     (niwa-buddy--scope-key)))))))


;;
;;; Region merging

(niwa-deftest niwa-buddy--merge-region
  (:doc "`niwa-buddy--merge-region' merges adjacent and enclosed edits")
  ,test
  (test)

  :doc "`niwa-buddy--merge-region' splices a change inside the region"
  (should (equal '(10 "old\n" "aXc\n")
                 (niwa-buddy--merge-region
                  (list :beg 10 :end 14 :old-text "old\n" :new-text "abc\n")
                  11 "b" "X")))

  :doc "`niwa-buddy--merge-region' extends the region to the right"
  (should (equal '(10 "old\nnext\n" "abc\nmore\n")
                 (niwa-buddy--merge-region
                  (list :beg 10 :end 14 :old-text "old\n" :new-text "abc\n")
                  14 "next\n" "more\n")))

  :doc "`niwa-buddy--merge-region' extends the region to the left"
  (should (equal '(5 "head\nold\n" "lead\nabc\n")
                 (niwa-buddy--merge-region
                  (list :beg 10 :end 14 :old-text "old\n" :new-text "abc\n")
                  5 "head\n" "lead\n")))

  :doc "`niwa-buddy--merge-region' refuses a disjoint change"
  (should-not (niwa-buddy--merge-region
               (list :beg 10 :end 14 :old-text "old\n" :new-text "abc\n")
               40 "far\n" "away\n")))


;;
;;; Diff assembly

(niwa-deftest niwa-buddy--format-changes
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy--format-changes' names the added line and its number"
  (let ((buf (niwa-test--buddy-buffer
              "buddy-added.el" "(defun a ())\n(defun b ())\n")))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "(defun c ())\n"))
    (let ((diff (niwa-test--buddy-diff buf)))
      (should (string-match-p "(defun c ())" diff))
      (should (string-match-p "^ *3 \\+" diff))))

  :doc "`niwa-buddy--format-changes' carries the region around the change"
  ;; A review comments on the region, not only the changed lines, so the
  ;; surrounding code has to reach the model with line numbers on it.
  (let ((buf (niwa-test--buddy-buffer
              "buddy-region.el"
              (mapconcat (lambda (n) (format "(line %d)" n))
                         (number-sequence 1 20) "\n"))))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 9)
      (end-of-line)
      (insert " ; touched"))
    (let ((diff (niwa-test--buddy-diff buf)))
      ;; The changed line, plus several unchanged lines either side of it.
      (should (string-match-p "touched" diff))
      (should (string-match-p "^ *5  (line 5)" diff))
      (should (string-match-p "^ *15  (line 15)" diff))
      ;; Still a region, not the whole buffer.
      (should-not (string-match-p "(line 1)$" diff))))

  :doc "`niwa-buddy--format-changes' returns empty when edits cancel out"
  (let ((buf (niwa-test--buddy-buffer "buddy-revert" "alpha\n")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "typo\n")
      (delete-region (- (point-max) 5) (point-max)))
    (should (string-empty-p (niwa-test--buddy-diff buf))))

  :doc "`niwa-buddy--format-changes' labels removed lines as old"
  (let ((buf (niwa-test--buddy-buffer "buddy-removed" "alpha\nbeta\ngamma\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point) (line-beginning-position 2)))
    (let ((diff (niwa-test--buddy-diff buf)))
      (should (string-match-p "old -beta" diff))
      (should-not (string-match-p "[0-9] -beta" diff))))

  :doc "`niwa-buddy--format-changes' carries a header with mode and cursor"
  (let ((buf (niwa-test--buddy-buffer
              "buddy-header.el" "alpha\n" #'emacs-lisp-mode)))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "beta\n"))
    (let ((diff (niwa-test--buddy-diff buf)))
      (should (string-match-p "Buffer: buddy-header.el" diff))
      (should (string-match-p "Mode: emacs-lisp-mode" diff))
      (should (string-match-p "Cursor: line 3" diff))))

  :doc "`niwa-buddy--format-changes' sections every buffer it is given"
  (let ((one (niwa-test--buddy-buffer "buddy-one" "alpha\n"))
        (two (niwa-test--buddy-buffer "buddy-two" "gamma\n")))
    (with-current-buffer one
      (goto-char (point-max)) (insert "beta\n"))
    (with-current-buffer two
      (goto-char (point-max)) (insert "delta\n"))
    (let ((diff (niwa-buddy--format-changes
                 (niwa-test--buddy-all-changes))))
      (should (string-match-p "Buffer: buddy-one" diff))
      (should (string-match-p "Buffer: buddy-two" diff))))

  :doc "`niwa-buddy--format-changes' skips a killed buffer"
  (let ((live (niwa-test--buddy-buffer "buddy-live" "alpha\n"))
        (dead (niwa-test--buddy-buffer "buddy-dead" "gamma\n")))
    (with-current-buffer live
      (goto-char (point-max)) (insert "beta\n"))
    (with-current-buffer dead
      (goto-char (point-max)) (insert "delta\n"))
    (kill-buffer dead)
    (let ((diff (niwa-buddy--format-changes
                 (niwa-test--buddy-all-changes))))
      (should (string-match-p "Buffer: buddy-live" diff))
      (should-not (string-match-p "Buffer: buddy-dead" diff)))))

;;
;;; Review settlement

(niwa-deftest niwa-buddy--settle
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy--settle' retires the changes a settled review covered"
  (let ((buf (niwa-test--buddy-buffer "settle-retire" "alpha\n")))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "beta\n"))
    (with-current-buffer buf
      (let ((scope (niwa-buddy--scope-key)))
        (should (niwa-buddy--changes-for-scope scope))
        (setq niwa-buddy--running scope)
        (niwa-buddy--settle scope (current-time))
        (should-not (niwa-buddy--changes-for-scope scope))
        (should-not niwa-buddy--running))))

  :doc "`niwa-buddy--settle' keeps the changes of an abandoned review"
  (let ((buf (niwa-test--buddy-buffer "settle-keep" "alpha\n")))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "beta\n"))
    (with-current-buffer buf
      (let ((scope (niwa-buddy--scope-key)))
        (setq niwa-buddy--running scope)
        (niwa-buddy--settle scope nil)
        (should (niwa-buddy--changes-for-scope scope))
        (should-not niwa-buddy--running))))

  :doc "`niwa-buddy--settle' keeps a change made while the review ran"
  (let ((buf (niwa-test--buddy-buffer "settle-race" "alpha\n")))
    (with-current-buffer buf
      (let ((scope (niwa-buddy--scope-key))
            (sent (current-time)))
        ;; The user keeps typing after the request went out; that edit was
        ;; never reviewed and must survive the settle.
        (with-current-buffer buf
          (goto-char (point-max)) (insert "beta\n"))
        (setq niwa-buddy--running scope)
        (niwa-buddy--settle scope sent)
        (should (niwa-buddy--changes-for-scope scope)))))

  :doc "`niwa-buddy--settle' clears the annotatable buffer list"
  (progn
    (setq niwa-buddy-note--run
          (niwa-buddy-note--make-run
           :scope (list (cons "some-buffer" (current-buffer))))
          niwa-buddy--running "scope")
    (niwa-buddy--settle "scope" nil)
    (should-not niwa-buddy-note--run))

  :doc "`niwa-buddy--settle' cleans up and re-signals a store flush error"
  (let* ((buf (current-buffer))
         (marker (copy-marker 1))
         (niwa-buddy-note--markers (list (list "b" (list 1 marker marker))))
         (niwa-buddy--request-buffer nil))
    (setq niwa-buddy-note--run
          (niwa-buddy-note--make-run
           :scope (list (cons (buffer-name buf) buf)) :changed '(ws))
          niwa-buddy--running "scope"
          niwa-buddy--running-automatic t)
    (let* ((generation niwa-buddy--generation)
           (read-tool
            (seq-find (lambda (tool) (equal (gptel-tool-name tool) "read_buffer"))
                      (niwa-buddy-note-tools
                       (lambda ()
                         (niwa-buddy--current-generation-p generation))))))
     (cl-letf (((symbol-function 'niwa-instruction-store-touch) #'ignore)
              ((symbol-function 'niwa-instruction-store-flush)
               (lambda () (error "Flush failed"))))
      (should-error (niwa-buddy--settle "scope" nil)))
     (should-not (niwa-buddy--current-generation-p generation))
     ;; The settled review's own tool call must be refused.
     (should (string-match-p
              "has ended"
              (funcall (gptel-tool-function read-tool) (buffer-name buf) 1 1))))
    (should-not niwa-buddy-note--run)
    (should-not niwa-buddy--running)
    (should-not niwa-buddy--running-automatic)
    (should-not niwa-buddy-note--markers)
    (should-not (marker-buffer marker)))

  :doc "`niwa-buddy--settle' persists remarks when killing the request buffer fails"
  (let ((request (generate-new-buffer " *buddy-request*"))
        (finished nil))
    (with-current-buffer request
      (add-hook 'kill-buffer-hook (lambda () (error "Kill failed")) nil t))
    (setq niwa-buddy--request-buffer request
          niwa-buddy--running "scope")
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-buddy-note-end-review)
                   (lambda () (setq finished t))))
          (should-error (niwa-buddy--settle "scope" nil))
          (should finished)
          (should-not niwa-buddy--running))
      (with-current-buffer request (setq-local kill-buffer-hook nil))
      (kill-buffer request)))

  :doc "`niwa-buddy--settle' leaves a newer run untouched when an older call arrives"
  (let* ((buf (niwa-test--buddy-buffer "newer-run" "alpha\nbeta\n"))
         (niwa-buddy--request-buffer nil)
         (old-generation (cl-incf niwa-buddy--generation))
         (old-tools (niwa-buddy-note-tools
                     (lambda ()
                       (niwa-buddy--current-generation-p old-generation))))
         (add (seq-find (lambda (tool) (equal "add_note" (gptel-tool-name tool)))
                        old-tools))
         newer)
    (setq niwa-buddy--running "scope")
    (niwa-buddy--settle "scope" nil)
    ;; A newer review starts, as `niwa-buddy--request' does.
    (cl-incf niwa-buddy--generation)
    (setq niwa-buddy--running "newer")
    (with-current-buffer buf
      (niwa-buddy-note-begin-review
       (list "newer-run") (niwa-buddy-guide-payload (point-min) (point-max))
       nil '("newer") nil))
    (setq newer niwa-buddy-note--run)
    (should (string-match-p
             "has ended"
             (funcall (gptel-tool-function add) "newer-run" 1 "late" "trivial")))
    (should (eq newer niwa-buddy-note--run))
    (should (assoc "newer-run" (niwa-buddy-note--run-scope newer)))
    (should-not (niwa-buddy-note--run-changed newer))
    (should (equal "newer" niwa-buddy--running))
    (niwa-buddy--settle "newer" nil)))
(niwa-deftest niwa-buddy-review
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-review' sends nothing when no edit was recorded"
  (let ((buf (niwa-test--buddy-buffer "review-idle" "alpha\n")))
    (with-current-buffer buf
      (should-not (niwa-buddy-review))))

  :doc "`niwa-buddy-review' refuses while a review is already running"
  (let ((buf (niwa-test--buddy-buffer "review-busy" "alpha\n")))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "beta\n"))
    (with-current-buffer buf
      (let ((niwa-buddy--running "another-scope"))
        (should-not (niwa-buddy-review)))))

  :doc "prepares one changed buffer per callback and lets input run before dispatch"
  (let* ((root (make-temp-file "buddy-preparation-" t))
         (project (niwa-test--buddy-project root "project"))
         (default-directory project)
         (first (niwa-test--buddy-buffer "buddy-first" "alpha\n"))
         (second (niwa-test--buddy-buffer "buddy-second" "beta\n"))
         (calls 0) input-ran sent timer
         (count-section (lambda (&rest _) (cl-incf calls))))
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer (goto-char (point-max)) (insert "changed\n")))
          (advice-add 'niwa-buddy--buffer-section :before count-section)
          (cl-letf (((symbol-function 'niwa-buddy--request)
                     (lambda (scope _profile payload _buffers through _automatic &optional _taken-only)
                       (should input-ran)
                       (should (= calls 2))
                       (should (string-search "buddy-first" payload))
                       (should (string-search "buddy-second" payload))
                       (setq sent t)
                       (niwa-buddy--settle scope through))))
            (with-current-buffer first (should (niwa-buddy-review t)))
            (should (= calls 0))
            (setq timer (run-at-time 0 nil (lambda () (setq input-ran t))))
            (let ((deadline (+ (float-time) 2)))
              (while (and (not sent) (< (float-time) deadline)) (sleep-for .002)))
            (should sent)
            (should-not (niwa-test--buddy-all-changes))))
      (when timer (cancel-timer timer))
      (advice-remove 'niwa-buddy--buffer-section count-section)
      (delete-directory root t)))

  :doc "editing during preparation abandons stale evidence without retiring the new edit"
  (let ((buffer (niwa-test--buddy-buffer "buddy-preparing-edit" "alpha\n"))
        sent)
    (with-current-buffer buffer
      (goto-char (point-max)) (insert "first edit\n")
      (cl-letf (((symbol-function 'niwa-buddy--request)
                 (lambda (&rest _) (setq sent t))))
        (should (niwa-buddy-review t))
        (insert "new edit\n")
        (let ((deadline (+ (float-time) 2)))
          (while (and niwa-buddy--running (< (float-time) deadline)) (sleep-for .002)))
        (should-not sent)
        (should-not niwa-buddy--running)
        (should (string-search "new edit" (niwa-test--buddy-diff buffer))))))

  :doc "a preempted preparation callback cannot replace its successor's timer"
  (let ((buffer (niwa-test--buddy-buffer "buddy-preparation-preempt" "alpha\n")))
    (with-current-buffer buffer
      (goto-char (point-max)) (insert "changed\n")
      (should (niwa-buddy-review t))
      (let ((old niwa-buddy--prepare-timer))
        (niwa-buddy--preempt)
        (should (niwa-buddy-review t))
        (let ((successor niwa-buddy--prepare-timer))
          (apply (timer--function old) (timer--args old))
          (should (eq successor niwa-buddy--prepare-timer))
          (should niwa-buddy--running)))))

  :doc "source buffer death cancels preparation before a request can be sent"
  (let ((buffer (niwa-test--buddy-buffer "buddy-preparation-death" "alpha\n")) sent)
    (cl-letf (((symbol-function 'niwa-buddy--request)
               (lambda (&rest _) (setq sent t))))
      (with-current-buffer buffer
        (goto-char (point-max)) (insert "changed\n")
        (should (niwa-buddy-review t)))
      (kill-buffer buffer)
      (let ((deadline (+ (float-time) 2)))
        (while (and niwa-buddy--running (< (float-time) deadline)) (sleep-for .002)))
      (should-not sent)
      (should-not niwa-buddy--running)))

  :doc "discarding tracked changes cancels preparation before provider dispatch"
  (let ((buffer (niwa-test--buddy-buffer "buddy-preparation-untrack" "alpha\n")) sent)
    (cl-letf (((symbol-function 'niwa-buddy--request)
               (lambda (&rest _) (setq sent t))))
      (with-current-buffer buffer
        (goto-char (point-max)) (insert "changed\n")
        (should (niwa-buddy-review t))
        (niwa-buddy--untrack-buffer))
      (let ((deadline (+ (float-time) 2)))
        (while (and niwa-buddy--running (< (float-time) deadline)) (sleep-for .002)))
      (should-not sent)
      (should-not niwa-buddy--running)))

  :doc "a diff failure after preemption cannot abandon the successor"
  (let ((buffer (niwa-test--buddy-buffer "buddy-preparation-error" "alpha\n")) successor)
    (with-current-buffer buffer
      (goto-char (point-max)) (insert "changed\n")
      (should (niwa-buddy-review t))
      (let ((old niwa-buddy--prepare-timer))
        (cancel-timer old)
        (cl-letf (((symbol-function 'niwa-buddy--format-changes)
                   (lambda (&rest _)
                     (niwa-buddy--preempt)
                     (should (niwa-buddy-review t))
                     (setq successor niwa-buddy--prepare-timer)
                     (error "Interrupted diff"))))
          (apply (timer--function old) (timer--args old))))
      (should successor)
      (should (eq successor niwa-buddy--prepare-timer))
      (should niwa-buddy--running)))

  :doc "`niwa-buddy-review' retires cancelling edits without a request"
  (let ((buf (niwa-test--buddy-buffer "review-noop" "alpha\n")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "typo\n")
      (delete-region (- (point-max) 5) (point-max)))
    (with-current-buffer buf
      (should (niwa-buddy-review))
      (let ((deadline (+ (float-time) 2)))
        (while (and niwa-buddy--running (< (float-time) deadline)) (sleep-for .002)))
      (should-not (niwa-buddy--changes-for-scope
                   (niwa-buddy--scope-key))))))

(niwa-deftest niwa-buddy--severity-instruction
  (:doc "`niwa-buddy--severity-instruction' states the configured floor")
  ,test
  (test)
  (let ((niwa-buddy-severity-floor "critical"))
    (should (string-match-p
             "nothing below critical"
             (niwa-buddy--severity-instruction)))))

;;
;;; Mode, tracking policy, and timers

(niwa-deftest niwa-buddy--tracked-buffer-p
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy--tracked-buffer-p' accepts a configured major mode"
  (let ((buf (niwa-test--buddy-buffer "tracked.el" "" #'emacs-lisp-mode))
        (niwa-buddy-tracked-modes '(prog-mode)))
    (should (niwa-buddy--tracked-buffer-p buf)))

  :doc "`niwa-buddy--tracked-buffer-p' rejects an unconfigured major mode"
  (let ((buf (niwa-test--buddy-buffer "untracked" ""))
        (niwa-buddy-tracked-modes '(prog-mode)))
    (should-not (niwa-buddy--tracked-buffer-p buf)))

  :doc "`niwa-buddy--tracked-buffer-p' rejects a niwa session buffer"
  (let ((buf (niwa-test--buddy-buffer "session.el" "" #'emacs-lisp-mode))
        (niwa-buddy-tracked-modes '(prog-mode)))
    (with-current-buffer buf
      (setq-local niwa--session 'pretend))
    (should-not (niwa-buddy--tracked-buffer-p buf)))

  :doc "`niwa-buddy--tracked-buffer-p' rejects an internal buffer"
  (let ((buf (niwa-test--buddy-buffer " hidden.el" "" #'emacs-lisp-mode))
        (niwa-buddy-tracked-modes '(prog-mode)))
    (should-not (niwa-buddy--tracked-buffer-p buf))))

(niwa-deftest niwa-buddy-mode
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-mode' installs and removes the change hooks"
  (let ((buf (niwa-test--buddy-buffer "mode-hooks.el" "" #'emacs-lisp-mode)))
    (with-current-buffer buf
      (niwa-buddy--untrack-buffer)
      (niwa-buddy-mode 1)
      (should (memq #'niwa-buddy--after-change after-change-functions))
      (niwa-buddy-mode -1)
      (should-not (memq #'niwa-buddy--after-change after-change-functions))))

  :doc "`niwa-buddy-mode' leaves no timer behind when disabled"
  (let ((buf (niwa-test--buddy-buffer "mode-timer.el" "" #'emacs-lisp-mode)))
    (with-current-buffer buf
      (niwa-buddy-mode 1)
      (niwa-buddy--schedule)
      (should niwa-buddy--idle-timer)
      (niwa-buddy-mode -1)
      (should-not niwa-buddy--idle-timer))))


;;
;;; Scheduling

(niwa-deftest niwa-buddy--interval-remaining
  ()
  ,test
  (test)

  :doc "`niwa-buddy--interval-remaining' is zero for a scope never reviewed"
  (let ((niwa-buddy--last-review (make-hash-table :test #'equal)))
    (should (= 0 (niwa-buddy--interval-remaining "scope"))))

  :doc "`niwa-buddy--interval-remaining' counts down from the last review"
  (let ((niwa-buddy--last-review (make-hash-table :test #'equal))
        (niwa-buddy-min-interval 60))
    (puthash "scope" (time-subtract nil 20) niwa-buddy--last-review)
    (should (< 39 (niwa-buddy--interval-remaining "scope") 41)))

  :doc "`niwa-buddy--interval-remaining' is zero once the interval passed"
  (let ((niwa-buddy--last-review (make-hash-table :test #'equal))
        (niwa-buddy-min-interval 60))
    (puthash "scope" (time-subtract nil 90) niwa-buddy--last-review)
    (should (= 0 (niwa-buddy--interval-remaining "scope")))))

(niwa-deftest niwa-buddy--run-scheduled
  (:after-each (progn
                 (dolist (buf niwa-test--buddy-buffers)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf (niwa-buddy--cancel-timer))))
                 (niwa-test--buddy-cleanup)))
  ,test
  (test)

  :doc "`niwa-buddy--run-scheduled' defers a review inside the minimum interval"
  (let ((buf (niwa-test--buddy-buffer "interval.el" "alpha\n"))
        (niwa-buddy--last-review (make-hash-table :test #'equal))
        (niwa-buddy-min-interval 60)
        (reviews 0))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "beta\n")
      (puthash (niwa-buddy--scope-key) (time-subtract nil 20)
               niwa-buddy--last-review)
      (cl-letf (((symbol-function 'niwa-buddy-review)
                 (lambda (&rest _) (cl-incf reviews) t)))
        (niwa-buddy--run-scheduled buf))
      (should (= 0 reviews))
      (should (timerp niwa-buddy--idle-timer))
      ;; A plain timer for the rest of the interval, not an idle timer.
      (should-not (timer--idle-delay niwa-buddy--idle-timer))
      (let ((due (float-time (time-subtract
                              (timer--time niwa-buddy--idle-timer) nil))))
        (should (< 38 due 41)))))

  :doc "`niwa-buddy--run-scheduled' reviews a due scope and records it"
  (let ((buf (niwa-test--buddy-buffer "due.el" "alpha\n"))
        (niwa-buddy--last-review (make-hash-table :test #'equal))
        (reviews nil))
    (with-current-buffer buf
      (cl-letf (((symbol-function 'niwa-buddy-review)
                 (lambda (&optional automatic) (push automatic reviews) t)))
        (niwa-buddy--run-scheduled buf))
      (should (equal '(t) reviews))
      (should-not niwa-buddy--idle-timer)
      (should (gethash (niwa-buddy--scope-key)
                       niwa-buddy--last-review))))

  :doc "`niwa-buddy--run-scheduled' does not start the interval without a review"
  (let ((buf (niwa-test--buddy-buffer "nothing.el" "alpha\n"))
        (niwa-buddy--last-review (make-hash-table :test #'equal)))
    (with-current-buffer buf
      (cl-letf (((symbol-function 'niwa-buddy-review) #'ignore))
        (niwa-buddy--run-scheduled buf))
      (should-not (gethash (niwa-buddy--scope-key)
                           niwa-buddy--last-review))))

  :doc "`niwa-buddy--run-scheduled' retries while another review runs"
  (let ((buf (niwa-test--buddy-buffer "busy.el" "alpha\n"))
        (niwa-buddy--last-review (make-hash-table :test #'equal))
        (niwa-buddy-idle-delay 10))
    (with-current-buffer buf
      (setq niwa-buddy--running "other")
      (cl-letf (((symbol-function 'niwa-buddy-review)
                 (lambda (&rest _) (error "Must not review"))))
        (niwa-buddy--run-scheduled buf))
      (setq niwa-buddy--running nil)
      (should (timerp niwa-buddy--idle-timer))
      (should-not (timer--idle-delay niwa-buddy--idle-timer)))))


;;
;;; Guidance channel

(niwa-deftest niwa-buddy-guide-payload
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-guide-payload' describes the whole buffer with a header"
  (let ((buf (niwa-test--buddy-buffer "guide-all.el" "alpha\nbeta\n")))
    (with-current-buffer buf
      (let ((payload (niwa-buddy-guide-payload (point-min) (point-max))))
        (should (string-match-p "Buffer: guide-all.el" payload))
        (should (string-match-p "Cursor: line" payload))
        (should (string-match-p "alpha" payload))
        (should (string-match-p "beta" payload)))))

  :doc "`niwa-buddy-guide-payload' describes only the given bounds"
  (let ((buf (niwa-test--buddy-buffer
              "guide-region.el" "alpha\nbeta\ngamma\n")))
    (with-current-buffer buf
      (let ((payload (niwa-buddy-guide-payload
                      (point-min)
                      (save-excursion (goto-char (point-min))
                                      (forward-line 2)
                                      (point)))))
        (should (string-match-p "alpha" payload))
        (should-not (string-match-p "gamma" payload)))))

  :doc "`niwa-buddy-guide-payload' numbers lines from their real position"
  (let ((buf (niwa-test--buddy-buffer
              "guide-lines.el" "alpha\nbeta\ngamma\n")))
    (with-current-buffer buf
      (let ((payload (niwa-buddy-guide-payload
                      (save-excursion (goto-char (point-min))
                                      (forward-line 2)
                                      (point))
                      (point-max))))
        (should (string-match-p "^ *3  gamma" payload))
        (should-not (string-match-p "^ *1  gamma" payload))))))

(niwa-deftest niwa-buddy--preempt
  (:after-each (progn (niwa-test--buddy-cleanup)
                      (setq niwa-buddy--running nil
                            niwa-buddy--running-automatic nil)))
  ,test
  (test)

  :doc "`niwa-buddy--preempt' abandons an automatic review in flight"
  (let ((buf (niwa-test--buddy-buffer "preempt.el" "alpha\n"))
        (request-buffer (generate-new-buffer " *buddy-preempt*")))
    (with-current-buffer buf
      (setq niwa-buddy--running "scope"
            niwa-buddy--running-automatic t
            niwa-buddy--request-buffer request-buffer)
      (niwa-buddy--preempt)
      (should-not niwa-buddy--running)))

  :doc "`niwa-buddy--preempt' leaves an explicit request alone"
  (let ((buf (niwa-test--buddy-buffer "preempt-explicit.el" "alpha\n"))
        (request-buffer (generate-new-buffer " *buddy-explicit*")))
    (with-current-buffer buf
      (setq niwa-buddy--running "scope"
            niwa-buddy--running-automatic nil
            niwa-buddy--request-buffer request-buffer)
      (niwa-buddy--preempt)
      (should (equal "scope" niwa-buddy--running))))

  :doc "`niwa-buddy--preempt' does nothing when no review is running"
  (progn
    (setq niwa-buddy--running nil)
    (niwa-buddy--preempt)
    (should-not niwa-buddy--running)))

(niwa-deftest niwa-buddy-guide
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-guide' cancels the queued automatic review"
  (let ((buf (niwa-test--buddy-buffer "guide-timer.el" "alpha\n")))
    (with-current-buffer buf
      (niwa-buddy--schedule)
      (should niwa-buddy--idle-timer)
      (cl-letf (((symbol-function 'niwa-buddy--request) #'ignore))
        (niwa-buddy-guide))
      (should-not niwa-buddy--idle-timer))))

;;
;;; Record hygiene

(niwa-deftest niwa-buddy-forget-buffer
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-forget-buffer' drops that buffer's records only"
  (let ((one (niwa-test--buddy-buffer "forget-one" "alpha\n"))
        (two (niwa-test--buddy-buffer "forget-two" "gamma\n")))
    (with-current-buffer one
      (goto-char (point-max)) (insert "beta\n"))
    (with-current-buffer two
      (goto-char (point-max)) (insert "delta\n"))
    (niwa-buddy-forget-buffer "forget-one")
    (let ((names (mapcar (lambda (record) (plist-get record :buffer))
                         (niwa-test--buddy-all-changes))))
      (should-not (member "forget-one" names))
      (should (member "forget-two" names))))

  :doc "killing a tracked buffer discards its records"
  (let ((buf (niwa-test--buddy-buffer "forget-killed" "alpha\n")))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "beta\n"))
    (should (niwa-test--buddy-all-changes))
    (kill-buffer buf)
    (should-not (niwa-test--buddy-all-changes))))

(niwa-deftest niwa-buddy--after-change
  (:after-each (niwa-test--buddy-cleanup)
   :doc "`niwa-buddy--after-change' drops a change it cannot reconstruct")
  ;; Emacs does not guarantee before/after change notifications pair one
  ;; to one.  Recording a replacement with no replaced text would make
  ;; reconstruction delete real content and invent a diff.
  (let ((buf (niwa-test--buddy-buffer "unpaired" "alpha\nbeta\n")))
    (with-current-buffer buf
      (setq niwa-buddy--pending-beg nil
            niwa-buddy--pending-old-text nil)
      (niwa-buddy--after-change (point-min) (point-max) 5)
      (should-not (niwa-buddy--changes-for-scope
                   (niwa-buddy--scope-key))))))

(niwa-deftest niwa-buddy-payload-lines
  (:doc "`niwa-buddy-payload-lines' returns the lines each buffer showed")
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
                   (niwa-buddy-payload-lines payload)))))

(niwa-deftest niwa-buddy--response-action
  (:doc "`niwa-buddy--response-action' counts tool rounds and nothing else")
  (should (eq ,action (niwa-buddy--response-action ,response)))
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

(niwa-deftest niwa-buddy--request-fsm
  (:doc "`niwa-buddy--request-fsm' settles only on the terminal states")
  ,test
  (test)
  (let* ((settled 'unset)
         (fsm (niwa-buddy--request-fsm (lambda (ok) (setq settled ok))))
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

(niwa-deftest niwa-buddy--request
  (:after-each (niwa-test--buddy-cleanup)
   :quiet t)
  ,test
  (test)

  :doc "`niwa-buddy--request' aborts only Buddy after its source dies"
  (let* ((source (niwa-test--buddy-buffer "buddy-request.el" "alpha\n"))
         (source-directory
          (buffer-local-value 'default-directory source))
         (ordinary-fsm (gptel-make-fsm :info (list :buffer source)))
         (gptel--request-alist nil)
         request-buffer
         (ordinary-aborts 0)
         (buddy-aborts 0))
    (with-current-buffer source
      (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                 (lambda (&rest _) '(:backend mock :model mock)))
                ((symbol-function 'niwa-buddy-note-capture-markers)
                 #'ignore)
                ((symbol-function 'niwa-buddy-note-release-markers)
                 #'ignore)
                ((symbol-function 'niwa-buddy-note-serialize)
                 (lambda () ""))
                ((symbol-function 'niwa-buddy-note-tools)
                 (lambda (_) nil))
                ((symbol-function 'niwa-buddy--telemetry) #'ignore)
                ((symbol-function 'niwa-buddy--workspace) #'ignore)
                ((symbol-function 'niwa-system-build-prompt)
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
        (niwa-buddy--request
         "scope" 'buddy "payload" '("buddy-request.el")
         (current-time) nil)))
    (should (buffer-live-p request-buffer))
    (should-not (eq source request-buffer))
    (should (equal source-directory
                   (buffer-local-value 'default-directory request-buffer)))
    (kill-buffer source)
    (niwa-buddy--abandon 'user)
    (should (= 0 ordinary-aborts))
    (should (= 1 buddy-aborts))
    (should (assq 'ordinary gptel--request-alist))
    (should-not (assq 'buddy gptel--request-alist))
    (should-not (buffer-live-p request-buffer)))

  :doc "the request buffer owns the complete Buddy request policy"
  (let* ((source (niwa-test--buddy-buffer "buddy-policy.el" "alpha\n"))
         (gptel-stream nil)
         captured)
    (with-current-buffer source
      (setq-local gptel-stream t
                  gptel-system-prompt "source-system")
      (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                 (lambda (&rest _)
                   '(:backend buddy-backend :model buddy-model
                     :effort high)))
                ((symbol-function 'niwa-buddy-note-capture-markers)
                 #'ignore)
                ((symbol-function 'niwa-buddy-note-serialize)
                 (lambda () ""))
                ((symbol-function 'niwa-buddy-note-tools)
                 (lambda (_) '(buddy-tool)))
                ((symbol-function 'niwa-buddy--telemetry) #'ignore)
                ((symbol-function 'niwa-buddy--workspace) #'ignore)
                ((symbol-function 'niwa-system-build-prompt)
                 (lambda (&rest _) "buddy-system"))
                ((symbol-function 'gptel-request)
                 (lambda (_prompt &rest keys)
                   (let ((buffer (plist-get keys :buffer)))
                     (should (eq (current-buffer) buffer))
                     (setq captured
                           (list
                            :backend
                            (buffer-local-value 'gptel-backend buffer)
                            :model (buffer-local-value 'gptel-model buffer)
                            :effort
                            (buffer-local-value 'gptel-reasoning-effort buffer)
                            :system
                            (buffer-local-value 'gptel-system-prompt buffer)
                            :use-context
                            (buffer-local-value 'gptel-use-context buffer)
                            :use-tools
                            (buffer-local-value 'gptel-use-tools buffer)
                            :tools (buffer-local-value 'gptel-tools buffer)
                            :stream (buffer-local-value 'gptel-stream buffer)
                            :stream-arg (plist-get keys :stream)
                            :system-arg (plist-get keys :system)))))))
        (niwa-buddy--request
         "scope" 'buddy "payload" '("buddy-policy.el")
         (current-time) nil)))
    (should (eq (plist-get captured :backend) 'buddy-backend))
    (should (eq (plist-get captured :model) 'buddy-model))
    (should (eq (plist-get captured :effort) 'high))
    (should (equal (plist-get captured :system) "buddy-system"))
    (should-not (plist-get captured :use-context))
    (should (plist-get captured :use-tools))
    (should (equal (plist-get captured :tools) '(buddy-tool)))
    (should (plist-get captured :stream))
    (should (plist-get captured :stream-arg))
    (should (equal (plist-get captured :system-arg) "buddy-system")))

  :doc "the Buddy tool-round cap uses the same exact request identity"
  (let* ((source (niwa-test--buddy-buffer "buddy-cap.el" "alpha\n"))
         (ordinary-fsm (gptel-make-fsm :info (list :buffer source)))
         (gptel--request-alist nil)
         (niwa-buddy-max-iterations 0)
         request-buffer callback
         (ordinary-aborts 0)
         (buddy-aborts 0))
    (with-current-buffer source
      (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                 (lambda (&rest _) '(:backend mock :model mock)))
                ((symbol-function 'niwa-buddy-note-capture-markers)
                 #'ignore)
                ((symbol-function 'niwa-buddy-note-release-markers)
                 #'ignore)
                ((symbol-function 'niwa-buddy-note-serialize)
                 (lambda () ""))
                ((symbol-function 'niwa-buddy-note-tools)
                 (lambda (_) nil))
                ((symbol-function 'niwa-buddy--telemetry) #'ignore)
                ((symbol-function 'niwa-buddy--workspace) #'ignore)
                ((symbol-function 'niwa-system-build-prompt)
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
        (niwa-buddy--request
         "scope" 'buddy "payload" '("buddy-cap.el")
         (current-time) nil)))
    (kill-buffer source)
    (funcall callback '(tool-result) nil)
    (should (= 0 ordinary-aborts))
    (should (= 1 buddy-aborts))
    (should (assq 'ordinary gptel--request-alist))
    (should-not (buffer-live-p request-buffer)))

  :doc "a scope's brief precedes the payload and never gains note markers"
  (let* ((source (niwa-test--buddy-buffer "buddy-brief.el" "alpha\n"))
         (payload (concat "=== Buffer: buddy-brief.el  Mode: x  Scope: s"
                          "  Cursor: line 1 ===\n     1  alpha"))
         prompt marked)
    (niwa-buddy-set-brief
     "scope" "the current plan"
     "=== Buffer: buddy-brief.el  Mode: x\n     7  not a buffer line")
    (with-current-buffer source
      (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                 (lambda (&rest _) '(:backend mock :model mock)))
                ((symbol-function 'niwa-buddy-note-capture-markers)
                 (lambda (lines) (setq marked lines)))
                ((symbol-function 'niwa-buddy-note-serialize)
                 (lambda () ""))
                ((symbol-function 'niwa-buddy-note-tools)
                 (lambda (_) nil))
                ((symbol-function 'niwa-buddy--telemetry) #'ignore)
                ((symbol-function 'niwa-buddy--workspace) #'ignore)
                ((symbol-function 'niwa-system-build-prompt)
                 (lambda (&rest _) "system"))
                ((symbol-function 'gptel-request)
                 (lambda (text &rest _) (setq prompt text))))
        (niwa-buddy--request
         "scope" 'buddy-guide payload '("buddy-brief.el") nil nil)))
    (should (equal '(("buddy-brief.el" 1)) marked))
    (should (string-prefix-p
             "=== Brief: what the programmer is building, from the current plan"
             prompt))
    (should (string-suffix-p (concat "=== End of brief ===\n\n" payload)
                             prompt))))

(niwa-deftest niwa-buddy--brief-section
  (:after-each (clrhash niwa-buddy--briefs))
  ,test
  (test)
  :doc "a scope without a brief adds nothing to its requests"
  (should (equal "" (niwa-buddy--brief-section "scope")))
  :doc "a long brief is cut to the limit and says so"
  (let ((niwa-buddy--brief-limit 5))
    (niwa-buddy-set-brief "scope" "the session's Goal" "  abcdefgh  ")
    (should (equal (concat "=== Brief: what the programmer is building, from"
                           " the session's Goal in their niwa session ===\n"
                           "abcde\n[brief truncated]\n=== End of brief ===\n\n")
                   (niwa-buddy--brief-section "scope"))))
  :doc "a nil text removes the brief"
  (progn
    (niwa-buddy-set-brief "scope" "the /buddy request" "x")
    (niwa-buddy-set-brief "scope" nil nil)
    (should (equal "" (niwa-buddy--brief-section "scope")))))

(niwa-deftest niwa-buddy--current-generation-p
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "the review in flight is current"
  (let ((generation (cl-incf niwa-buddy--generation)))
    (should (niwa-buddy--current-generation-p generation)))

  :doc "a request outlived by its review is not"
  ;; A callback racing with cancellation must neither retire a newer
  ;; review's changes nor tear down its state.
  (let ((stale (cl-incf niwa-buddy--generation)))
    (cl-incf niwa-buddy--generation)
    (should-not (niwa-buddy--current-generation-p stale))))

(niwa-deftest niwa-buddy--abandon
  (:after-each (niwa-test--buddy-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy--abandon' retires the abandoned review's generation"
  (let ((buf (niwa-test--buddy-buffer "abandon.el" "alpha\n"))
        (request-buffer (generate-new-buffer " *buddy-abandon*")))
    (with-current-buffer buf
      (setq niwa-buddy--running "scope"
            niwa-buddy--running-automatic t
            niwa-buddy--request-buffer request-buffer)
      (let ((generation niwa-buddy--generation))
        (niwa-buddy--abandon 'timeout)
        (should-not (niwa-buddy--current-generation-p generation)))))

  :doc "`niwa-buddy--abandon' keeps the changes it abandoned"
  (let ((buf (niwa-test--buddy-buffer "abandon-keep.el" "alpha\n"))
        (request-buffer (generate-new-buffer " *buddy-abandon-keep*")))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "beta\n"))
    (with-current-buffer buf
      (let ((scope (niwa-buddy--scope-key)))
        (setq niwa-buddy--running scope
              niwa-buddy--running-automatic t
              niwa-buddy--request-buffer request-buffer)
        (niwa-buddy--abandon 'timeout)
        (should (niwa-buddy--changes-for-scope scope))
        (should-not niwa-buddy--running)))))

(niwa-deftest niwa-buddy--untrack-buffer
  (:after-each (niwa-test--buddy-cleanup)
   :doc "`niwa-buddy--untrack-buffer' discards the buffer's records")
  ;; Once the kill hook is gone nothing would drop them later, and a buffer
  ;; reusing the name would have those offsets replayed against unrelated
  ;; content.
  (let ((buf (niwa-test--buddy-buffer "untrack.el" "alpha\n")))
    (with-current-buffer buf
      (goto-char (point-max)) (insert "beta\n"))
    (should (niwa-test--buddy-all-changes))
    (with-current-buffer buf (niwa-buddy--untrack-buffer))
    (should-not (niwa-test--buddy-all-changes))))

(provide 'test-niwa-buddy)
;;; test-niwa-buddy.el ends here
