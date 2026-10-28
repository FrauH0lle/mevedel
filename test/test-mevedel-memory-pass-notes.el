;;; test-mevedel-memory-pass-notes.el -- Shared notes through consolidation -*- lexical-binding: t -*-

;;; Commentary:

;; Real shared folders, journal records and checked writes exercise note
;; extraction, disposal, housekeeping, undo and eligibility through the
;; consolidation pass without network calls.  The review request is replaced
;; by a scripted reply.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-pass)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-review)
(require 'mevedel-resource)
(require 'mevedel-session-control-fs)
(require 'mevedel-system)
(require 'gptel-openai)

(defmacro mevedel-test-memory-pass-notes--without-prompts (&rest body)
  "Run BODY failing on any interactive question."
  `(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (ert-fail "Prompted")))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Prompted")))
             ((symbol-function 'read-from-minibuffer) (lambda (&rest _) (ert-fail "Prompted")))
             ((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Prompted"))))
     ,@body))

(mevedel-deftest mevedel-memory-pass-start/notes
                 (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "mevedel-notes-" t))))
                          (workspace (mevedel-workspace--create :root root))
                          (identity (mevedel-workspace-identity-ensure root))
                          (shared (file-name-concat root ".mevedel" "shared"))
                          (memory (file-name-concat root "memory"))
                          (mevedel-memory-dirs (list (file-name-concat root "memory")))
                          (mevedel-journal-max-age-days 14)
                          (mevedel-memory-consolidation-mode 'auto)
                          (mevedel-memory-consolidation-min-hours 24)
                          (mevedel-memory-consolidation-min-digests 5)
                          (mevedel-memory-notes-quiet-hours 24)
                          (mevedel-memory-notes-review-days 7)
                          (mevedel-memory-notes-undo-quota (* 1024 1024))
                          (mevedel-memory-pass--inhibit-scheduling nil)
                          (gptel--known-backends nil)
                          (backend (gptel-make-openai "notes-test" :key "test-only" :models '(test-model)))
                          (now 1800000000)
                          (calls 0) callback scope-seen entries-seen state result reply buffers)
                         :before-each ((make-directory shared t) (make-directory memory t))
                         :after-each ((mevedel-memory-pass-stop-all)
                                      (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) buffers)
                                      (delete-directory root t)))
                 (cl-letf (((symbol-value 'mevedel-journal-worker--child-p) t)
                           ((symbol-function 'mevedel-session-control-fs-target-time) (lambda (_) now))
                           ((symbol-function 'mevedel-memory-review-request)
                           (lambda (scope entries cb &rest _)
                              (cl-incf calls)
                             (setq callback cb scope-seen scope entries-seen entries)
                              (list :cancel #'ignore))))
                   (cl-labels ((note (relative text &optional age-hours)
                                 (let ((path (file-name-concat shared relative)))
                                   (make-directory (file-name-directory path) t)
                                   (let ((coding-system-for-write 'no-conversion))
                                     (write-region text nil path nil 'silent))
                                   (set-file-times path (seconds-to-time (- now (* 3600 (or age-hours 48)))))
                                   path))
                               (bytes (path)
                                 (with-temp-buffer
                                   (set-buffer-multibyte nil)
                                   (insert-file-contents-literally path)
                                   (buffer-string)))
                               (address (relative) (mevedel-memory-notes-address relative))
                               (session-buffer (&optional request)
                                 ;; An open conversation in this workspace, running a turn
                                 ;; when REQUEST is non-nil.
                                 (let ((buffer (generate-new-buffer " *notes-session*")))
                                   (push buffer buffers)
                                   (with-current-buffer buffer
                                     (setq-local mevedel--session (mevedel-session--create :workspace workspace))
                                     (setq-local mevedel--current-request request))
                                   buffer))
                               (use (buffer relative)
                                 ;; The production resource path records a session's tool access.
                                 (mevedel-resource-prepare
                                  'read (address relative)
                                  (list :session (buffer-local-value 'mevedel--session buffer))))
                               (memory-id ()
                                 (car (seq-find (lambda (row) (eq 'memory (plist-get (cdr row) :kind)))
                                                (plist-get (mevedel-memory-scope-capture workspace) :roots))))
                               (text (promotes dispositions)
                                 (concat
                                  "## Promote\n"
                                  (if promotes
                                      (mapconcat
                                       (lambda (promote)
                                         (format (concat "````proposal\nroot: %S\nfile: %S\ntype: \"project\"\ntitle: %S\n"
                                                         "hook: \"Lesson\"\nreason: \"Supported\"\nevidence: %s\n---\n%s\n````\n")
                                                 (memory-id) (nth 0 promote) (nth 0 promote)
                                                 (json-serialize (vconcat (nth 2 promote))) (nth 1 promote)))
                                       promotes "")
                                    "- none\n")
                                  "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n"
                                  (when dispositions
                                    (concat "## Notes\n"
                                            (mapconcat
                                             (lambda (disposition)
                                               (format "````proposal\nnote: %S\ndisposition: %S\nreason: \"Reviewed\"\n%s---\n%s````\n"
                                                       (nth 0 disposition) (nth 1 disposition)
                                                       (cond ((nth 3 disposition))
                                                             ((equal (nth 1 disposition) "delete")
                                                              "open-items: []\nresolved-by: []\n")
                                                             (t ""))
                                                       (if (nth 2 disposition) (concat (nth 2 disposition) "\n") "")))
                                             dispositions "")))
                                  "## No action\n- none"))
                               (start (&rest args)
                                 (setq result nil state (apply #'mevedel-memory-pass-start workspace
                                                               (lambda (value) (setq result value)) args)))
                               (finish (&optional inspected)
                                 (funcall callback
                                          (list :outcome 'success :entries nil :reply reply
                                                :notes (mapcar (lambda (document) (plist-get document :id))
                                                               (mevedel-memory-notes-documents scope-seen))
                                                :inspected inspected
                                                :policy (list :backend backend :model 'test-model))))
                               (pass () (plist-get (plist-get state :claim) :owner))
                               (proposals () (plist-get (mevedel-memory-store-accepted workspace (pass)) :proposals))
                               (proposal (action &optional file)
                                 (seq-find (lambda (item) (and (eq action (plist-get item :action))
                                                               (or (null file) (equal file (plist-get item :file)))))
                                           (proposals)))
                               (status (item) (plist-get (mevedel-memory-decision-status workspace (plist-get item :id)) :status))
                               (capture () (mevedel-memory-notes-capture workspace (mevedel-journal-store-entries root))))
                     ,test))
                 (test)
                 :doc "a notes-only workspace without a journal extracts and deletes automatically without prompts"
                 (mevedel-test-memory-pass-notes--without-prompts
                  (let ((path (note "plan.md" "The parser must reject duplicate index destinations.\n")))
                    (should-not (file-exists-p (mevedel-journal-store-directory root)))
                    (should (start :automatic t))
                    (should (= 1 calls))
                    (should (equal (list (address "plan.md"))
                                   (mapcar (lambda (document) (plist-get document :id))
                                           (mevedel-memory-notes-documents scope-seen))))
                    (setq reply (text (list (list "parser.md" "Reject duplicate index destinations."
                                                  (list (address "plan.md"))))
                                      (list (list (address "plan.md") "delete"))))
                    (let ((original (bytes path)))
                      (finish)
                      (should (eq 'success (plist-get result :outcome)))
                      (should (= 1 (plist-get result :notes-reviewed)))
                      (should (file-exists-p (file-name-concat memory "parser.md")))
                      (should-not (file-exists-p path))
                      (should (file-directory-p shared))
                      (let ((extraction (proposal 'promote)) (disposal (proposal 'note-delete)))
                        (should (equal (list (plist-get extraction :id)) (plist-get disposal :requires)))
                        (should (eq 'applied (status extraction)))
                        (should (eq 'applied (status disposal)))
                        (should (file-exists-p (file-name-concat root ".mevedel" "state" "shared-notes" "blobs"
                                                                 (secure-hash 'sha256 original))))
                        ;; Reversing the extraction first restores the note it depended on.
                        (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse
                                                          workspace (pass) (plist-get extraction :id))
                                                         :status)))
                        (should (eq 'reversed (status disposal)))
                        (should (equal original (bytes path)))
                        (should-not (file-exists-p (file-name-concat memory "parser.md")))))))
                 :doc "propose mode leaves decisions pending and a disposal waits for its extraction"
                 (let ((mevedel-memory-consolidation-mode 'propose)
                       (path (note "handoff.md" "Lesson: run clean elc before tests.\n")))
                   (should (start :automatic t))
                   (setq reply (text (list (list "clean.md" "Run clean elc first." (list (address "handoff.md"))))
                                     (list (list (address "handoff.md") "delete"))))
                   (finish)
                   (should (file-exists-p path))
                   (let ((extraction (proposal 'promote)) (disposal (proposal 'note-delete)))
                     (should-not (status extraction))
                     (should-error (mevedel-memory-decision-apply workspace (pass) (plist-get disposal :id))
                                   :type 'user-error)
                     (should-not (status disposal))
                     ;; A pending disposal suppresses another review of the same version.
                     (should (= 0 (plist-get (capture) :notes)))
                     (mevedel-memory-decision-reject workspace (pass) (plist-get extraction :id) "Not durable")
                     (should (eq 'stale (plist-get (mevedel-memory-decision-apply workspace (pass) (plist-get disposal :id))
                                                   :status)))
                     (should (file-exists-p path))
                     ;; The unextracted note returns to review.
                     (should (= 1 (plist-get (capture) :notes)))))
                 :doc "capture authenticates only present-path decisions and rechecks recreated sources"
                 (let ((gone (note "gone.md" "Completed investigation.\n"))
                       (kept (note "kept.md" "Open handoff.\n")))
                   (should (start :automatic t))
                   (setq reply (text nil (list (list (address "gone.md") "delete")
                                               (list (address "kept.md") "keep"))))
                   (finish)
                   (should-not (file-exists-p gone))
                   (should (file-exists-p kept))
                   (let ((reader (symbol-function 'mevedel-memory-decision-status)) observed)
                     (cl-letf (((symbol-function 'mevedel-memory-decision-status)
                                (lambda (ws id &rest args)
                                  (push id observed)
                                  (apply reader ws id args))))
                       (should (= 0 (plist-get (capture) :notes)))
                       (should (equal (list (plist-get (proposal 'note-keep) :id)) observed))
                       ;; Both identical and changed recreations must reenter
                       ;; history authentication; absence is not cached.
                       (dolist (body '("Completed investigation.\n" "New investigation.\n"))
                         (note "gone.md" body)
                         (setq observed nil)
                         (should (= 1 (plist-get (capture) :notes)))
                         (should (member (plist-get (proposal 'note-delete) :id) observed))
                         (should (member (plist-get (proposal 'note-keep) :id) observed))))))
                 :doc "keep and condense retain their versions until reconsideration or an edit"
                 (let ((kept (note "kept.md" "Open handoff: continue the parser fix tomorrow.\n"))
                       (condensed (note "long.md" "Finding: obsolete detail.\nOpen: verify the remote case.\n")))
                   (should (start :automatic t))
                   (setq reply (text nil (list (list (address "kept.md") "keep")
                                               (list (address "long.md") "condense" "Open: verify the remote case."))))
                   (finish)
                   (should (equal "Open handoff: continue the parser fix tomorrow.\n" (bytes kept)))
                   (should (equal "Open: verify the remote case.\n" (bytes condensed)))
                   (set-file-times condensed (seconds-to-time (- now (* 3600 48))))
                   (should (= 0 (plist-get (capture) :notes)))
                   (cl-incf now (* 8 86400))
                   (should (= 2 (plist-get (capture) :notes)))
                   (cl-decf now (* 8 86400))
                   ;; An edit is a new version, eligible once quiet again.
                   (note "kept.md" "Open handoff: parser fix moved to Friday.\n" 1)
                   (should (= 0 (plist-get (capture) :notes)))
                   (set-file-times kept (seconds-to-time (- now (* 3600 25))))
                   (should (= 1 (plist-get (capture) :notes))))
                 :doc "housekeeping removes quiet unreferenced attachments and empty folders without inference"
                 (mevedel-test-memory-pass-notes--without-prompts
                  (let* ((binary (apply #'unibyte-string (number-sequence 0 255)))
                         (orphan (note "x/y/orphan.bin" binary))
                         (kept (note "kept.bin" binary))
                         (young (note "young.bin" (concat binary "young") 1))
                         (empty (file-name-concat shared "old" "empty")))
                    (set-file-modes orphan #o640)
                    (note "draft.md" "Uses kept.bin for the screenshot.\n" 1)
                    (make-directory empty t)
                    (set-file-times empty (seconds-to-time (- now (* 3600 48))))
                    (should (start :automatic t))
                    (should (= 0 calls))
                    (should (eq 'success (plist-get result :outcome)))
                    (should-not (file-exists-p orphan))
                    (should-not (file-exists-p (file-name-concat shared "x")))
                    (should-not (file-exists-p empty))
                    (should (file-exists-p kept))
                    (should (file-exists-p young))
                    (should (file-directory-p shared))
                    (should (eq 'applied (status (proposal 'note-prune))))
                    ;; Undo restores exact bytes, mode and removed parents.
                    (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse
                                                      workspace (pass) (plist-get (proposal 'note-delete "x/y/orphan.bin") :id))
                                                     :status)))
                    (should (equal binary (bytes orphan)))
                    (should (= #o640 (file-modes orphan)))))
                 :doc "quota refusal, concurrent edits and unsaved buffers leave notes untouched"
                 (let* ((mevedel-memory-consolidation-mode 'propose)
                        (attachment (note "big.bin" (make-string 600 0)))
                        (edited (note "edited.md" "Original finding.\n"))
                        (visited (note "visited.md" "Visited finding.\n"))
                        buffer)
                   (unwind-protect
                       (progn
                         (should (start :automatic t))
                         (setq reply (text nil (mapcar (lambda (name) (list (address name) "delete"))
                                                       '("edited.md" "visited.md"))))
                         (finish)
                         (let ((mevedel-memory-notes-undo-quota 100))
                           (should (eq 'unavailable
                                       (plist-get (mevedel-memory-decision-apply
                                                   workspace (pass) (plist-get (proposal 'note-delete "big.bin") :id))
                                                  :status))))
                         (should (file-exists-p attachment))
                         (write-region "Concurrent edit.\n" nil edited nil 'silent)
                         (should (eq 'stale (plist-get (mevedel-memory-decision-apply
                                                        workspace (pass) (plist-get (proposal 'note-delete "edited.md") :id))
                                                       :status)))
                         (should (equal "Concurrent edit.\n" (bytes edited)))
                         (setq buffer (find-file-noselect visited))
                         (with-current-buffer buffer (goto-char (point-max)) (insert "Unsaved.\n"))
                         (should-not (eq 'applied (plist-get (condition-case nil
                                                                 (mevedel-memory-decision-apply
                                                                  workspace (pass) (plist-get (proposal 'note-delete "visited.md") :id))
                                                               (error nil))
                                                             :status)))
                         (should (file-exists-p visited)))
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer (set-buffer-modified-p nil))
                       (kill-buffer buffer))))
                 :doc "oversized or unreadable text stays visibly blocked and protects attachment cleanup"
                 (let ((mevedel-memory-notes-max-review-bytes mevedel-memory-notes--max-note-bytes)
                       (large (note "large.md" (make-string (* 40 1024) ?x))))
                   (note "plain.bin" (unibyte-string 0 1 2))
                   (make-symbolic-link large (file-name-concat shared "linked.md"))
                   (let ((observation (capture)))
                     (should (= 0 (plist-get observation :notes)))
                     (should (= 0 (plist-get observation :attachments)))
                     (should (assoc "large.md" (plist-get observation :blocked)))
                     (should (assoc "linked.md" (plist-get observation :blocked)))
                     (should (assoc "plain.bin" (plist-get observation :blocked))))
                   (should (assoc "large.md" (plist-get (mevedel-memory-notes-observation workspace) :blocked))))
                 :doc "files cited by memory are never auto-deleted and a cited note is kept without an update"
                 (let ((mevedel-journal-enabled nil))
                   (write-region "Measurements live in `work://shared/lab/`.\n" nil
                                 (file-name-concat memory "report.md") nil 'silent)
                   (let ((image (note "lab/result.png" (unibyte-string 0 1 2 3)))
                         (report (note "lab/report.md" "Measured 40 ms redraws.\n")))
                     (should (start :automatic t))
                     (should (plist-get (car (mevedel-memory-notes-documents scope-seen)) :cited))
                     (should-not (plist-get (cdr (mevedel-memory-notes-root scope-seen)) :attachments))
                     (setq reply (text nil (list (list (address "lab/report.md") "delete"))))
                     (finish)
                     (should (eq 'note-keep (plist-get (proposal 'note-keep "lab/report.md") :action)))
                     (should (string-prefix-p "Kept: a memory topic" (plist-get (proposal 'note-keep "lab/report.md") :reason)))
                     (should (file-exists-p report))
                     (should (file-exists-p image))))
                 :doc "a delete listing open items applies only with quoted read workspace evidence for each"
                 ;; Real failure (run08, 2026-10-28): a reviewer with no tool calls
                 ;; proposed deleting an audit while naming two unresolved findings;
                 ;; with tools, a reviewer cited a file it read that did not record them.
                 (let ((paths (list (note "unread.md" "Findings 2 and 1B belong to the partner.\n")
                                    (note "claimed.md" "Acceptance limit: text-scale without resize.\n")
                                    (note "elsewhere.md" "Finding 7 not re-measured.\n")
                                    (note "fewer.md" "Findings 1B and 7 remain open.\n")
                                    (note "resolved.md" "Finding 1B remains open.\n")
                                    (note "done.md" "Fixed and verified.\n")))
                       (anchor "./docs/adr/0126.md: One viewport anchor per window (2026-10-18). Logical window-start restoration"))
                   (should (start :automatic t))
                   (setq reply
                         (text nil (list (list (address "unread.md") "delete" nil
                                               "open-items: [\"Finding 2\", \"Finding 1B\"]\nresolved-by: []\n")
                                         ;; Read, but the quote is not there: over-claiming.
                                         (list (address "claimed.md") "delete" nil
                                               "open-items: [\"Text-scale limit\"]\nresolved-by: [\"docs/adr/0126.md: the text-scale limit is resolved\"]\n")
                                         ;; The quote names a file the review never read.
                                         (list (address "elsewhere.md") "delete" nil
                                               "open-items: [\"Finding 7\"]\nresolved-by: [\"docs/backlog.md: Finding 7 was re-measured\"]\n")
                                         (list (address "fewer.md") "delete" nil
                                               (format "open-items: [\"Finding 1B\", \"Finding 7\"]\nresolved-by: [%S]\n" anchor))
                                         ;; Quotes span a line break and ignore line numbers.
                                         (list (address "resolved.md") "delete" nil
                                               (format "open-items: [\"Finding 1B\"]\nresolved-by: [%S]\n" anchor))
                                         (list (address "done.md") "delete"))))
                   (finish (list (cons "docs/adr/0126.md"
                                       (concat "  352\t- **One viewport anchor per window (2026-10-18).** Logical window-start\n"
                                               "  353\t  restoration previously overrode the composer's row\n"))
                                 (cons "mevedel-view-zone.el" "  1\t;;; mevedel-view-zone.el\n")))
                   (should (eq 'success (plist-get result :outcome)))
                   (should (string-prefix-p
                            "Kept: the review did not quote read workspace evidence resolving or recording each of these open items: Finding 2; Finding 1B. Reviewer: "
                            (plist-get (proposal 'note-keep "unread.md") :reason)))
                   (should (proposal 'note-keep "claimed.md"))
                   (should (proposal 'note-keep "elsewhere.md"))
                   (should (proposal 'note-keep "fewer.md"))
                   (should (proposal 'note-delete "resolved.md"))
                   (should (proposal 'note-delete "done.md"))
                   (should-not (proposal 'note-delete "unread.md"))
                   (dotimes (index 4) (should (file-exists-p (nth index paths)))))
                 :doc "an attachment referenced by a note written after capture is not removed"
                 (let ((mevedel-memory-consolidation-mode 'propose)
                       (image (note "old.png" (unibyte-string 0 1 2 3))))
                   (should (start :automatic t))
                   (should (eq 'success (plist-get result :outcome)))
                   (let ((late (note "late.md" "See old.png for the regression." 0)))
                     (set-file-times late (seconds-to-time (+ now 60))))
                   (should (eq 'stale (plist-get (mevedel-memory-decision-apply
                                                  workspace (pass) (plist-get (proposal 'note-delete "old.png") :id))
                                                 :status)))
                   (should (file-exists-p image)))
                 :doc "digests and ready notes are reviewed together and notes alone admit a review"
                 (progn
                   (dotimes (number 2)
                     (mevedel-journal-store-publish-digest
                      root (list :capture-id (format "%064x" (1+ number)) :session "closed" :session-name "Closed"
                                 :workspace identity :trigger 'session-end :segment 1
                                 :source-revision (make-string 64 ?c) :turns '(1)
                                 :turn-ids (list (format "%064x" (1+ number)))
                                 :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" (- now 3600) t) :model "test:model")
                      "## Done\n- Observed: Tests passed.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"))
                   (note "finding.md" "A finding.\n")
                   ;; Two digests stay below the digest threshold; the ready note admits the review.
                   (should (start :automatic t))
                   (should (= 2 (length entries-seen)))
                   (should (= 1 (length (mevedel-memory-notes-documents scope-seen)))))
                 :doc "an old handoff an open session uses waits while an unrelated note applies during another turn"
                 (let ((handoff (note "handoff.md" "Open handoff: port the fix.\n" 240))
                       (quiet (note "quiet.md" "Nothing durable.\n"))
                       (reader (session-buffer)))
                   (session-buffer (list :request 'live))
                   (use reader "handoff.md")
                   ;; A folder search is not use of every note in it.
                   (mevedel-resource-prepare 'glob "work://shared/"
                                             (list :session (buffer-local-value 'mevedel--session reader)))
                   (should (equal '("handoff.md") (mevedel-memory-notes-in-use workspace)))
                   (should (start :automatic t))
                   (should (equal (list (address "quiet.md"))
                                  (mapcar (lambda (document) (plist-get document :id))
                                          (mevedel-memory-notes-documents scope-seen))))
                   (setq reply (text nil (list (list (address "quiet.md") "delete"))))
                   (finish)
                   (should (eq 'success (plist-get result :outcome)))
                   (should-not (file-exists-p quiet))
                   (should (eq 'applied (status (proposal 'note-delete "quiet.md"))))
                   (should (file-exists-p handoff))
                   ;; Closing the reading session ends the use.
                   (kill-buffer reader)
                   (should-not (mevedel-memory-notes-in-use workspace)))
                 :doc "a note decision waits while an open session uses its file and applies once that use ends"
                 (let ((path (note "busy.md" "Nothing durable.\n"))
                       (other (note "other.md" "Nothing durable either.\n"))
                       (reader (session-buffer)))
                   (should (start :automatic t))
                   (setq reply (text nil (list (list (address "busy.md") "delete")
                                               (list (address "other.md") "delete"))))
                   ;; Use begins after admission; application rechecks it.
                   (use reader "busy.md")
                   (finish)
                   (should (= 1 (plist-get result :held-count)))
                   (should (file-exists-p path))
                   (should-not (file-exists-p other))
                   (should (gethash workspace mevedel-memory-pass--deferred))
                   (mevedel-memory-pass--retry-notes workspace)
                   (should (file-exists-p path))
                   (kill-buffer reader)
                   (mevedel-memory-pass--retry-notes workspace)
                   (should-not (gethash workspace mevedel-memory-pass--deferred))
                   (let ((deadline (+ (float-time) 30)))
                     (while (and (mevedel-memory-pass-running workspace) (< (float-time) deadline))
                       (accept-process-output nil .01)))
                   (should-not (mevedel-memory-pass-running workspace))
                   (should-not (file-exists-p path))
                   (should (eq 'applied (status (proposal 'note-delete "busy.md")))))
                 :doc "a later review supersedes an earlier undecided disposition of the same file"
                 (let ((mevedel-memory-consolidation-mode 'propose)
                       (path (note "draft.md" "First draft.\n")))
                   (should (start :automatic t))
                   (setq reply (text nil (list (list (address "draft.md") "delete"))))
                   (finish)
                   (let ((first-pass (pass)) (first (proposal 'note-delete "draft.md")))
                     (note "draft.md" "Second draft with an open handoff.\n")
                     (cl-incf now (* 2 86400))
                     (should (start :automatic t))
                     (setq reply (text nil (list (list (address "draft.md") "keep"))))
                     (finish)
                     (let ((decision (mevedel-memory-decision-status workspace (plist-get first :id))))
                       (should (eq 'rejected (plist-get decision :status)))
                       (should (equal mevedel-memory-notes-superseded-reason (plist-get decision :reason))))
                     (should-not (equal first-pass (pass)))
                     (should-not (status (proposal 'note-keep "draft.md")))
                     (should (file-exists-p path))))
                 :doc "an extraction applies before its note is condensed to the open part"
                 (let ((path (note "mixed.md" "Finding: index entries must be unique.\nOpen: port the fix.\n")))
                   (should (start :automatic t))
                   (setq reply (text (list (list "unique.md" "Index entries must be unique." (list (address "mixed.md"))))
                                     (list (list (address "mixed.md") "condense" "Open: port the fix."))))
                   (finish)
                   (should (file-exists-p (file-name-concat memory "unique.md")))
                   (should (equal "Open: port the fix.\n" (bytes path)))
                   (should (equal (list (plist-get (proposal 'promote) :id))
                                  (plist-get (proposal 'note-condense) :requires))))
                 :doc "manual mode reviews notes only on request"
                 (let ((mevedel-memory-consolidation-mode 'manual))
                   (note "manual.md" "A finding.\n")
                   (should-not (start :automatic t))
                   (should (= 0 calls))
                   (should (start :memory-only t))
                   (should (= 1 (length (mevedel-memory-notes-documents scope-seen)))))
                 :doc "a cancelled review decides nothing and leaves notes untouched"
                 (let ((path (note "cancel.md" "A finding.\n")))
                   (should (start :automatic t))
                   (mevedel-memory-pass-cancel workspace)
                   (should (eq 'aborted (plist-get result :outcome)))
                   (should (file-exists-p path))
                   (should-not (mevedel-memory-notes-passes workspace (mevedel-journal-store-entries root)))
                   (should (= 1 (plist-get (capture) :notes))))
                 :doc "an interrupted mutation is recorded from the observed state"
                 (let ((mevedel-memory-consolidation-mode 'propose)
                       (kept (note "kept.md" "Nothing durable.\n"))
                       (gone (note "gone.md" "Nothing durable either.\n"))
                       (real (symbol-function 'mevedel-memory-notes-commit)))
                   (should (start :automatic t))
                   (setq reply (text nil (list (list (address "kept.md") "delete") (list (address "gone.md") "delete"))))
                   (finish)
                   ;; Failing before any write leaves the note and an unavailable attempt.
                   (cl-letf (((symbol-function 'mevedel-memory-notes-commit)
                              (lambda (&rest _) (error "Interrupted"))))
                     (should (eq 'unavailable (plist-get (mevedel-memory-decision-apply
                                                          workspace (pass) (plist-get (proposal 'note-delete "kept.md") :id))
                                                         :status))))
                   (should (file-exists-p kept))
                   (should (= 1 (plist-get (capture) :notes)))
                   ;; Failing after the write still records the completed state.
                   (cl-letf (((symbol-function 'mevedel-memory-notes-commit)
                              (lambda (&rest args) (apply real args) (error "Interrupted"))))
                     (should (eq 'applied (plist-get (mevedel-memory-decision-apply
                                                      workspace (pass) (plist-get (proposal 'note-delete "gone.md") :id))
                                                     :status))))
                   (should-not (file-exists-p gone)))
                 :doc "large binary attachments round trip byte for byte through undo"
                 (let* ((mevedel-memory-notes-undo-quota (* 16 1024 1024))
                        (large (let ((bytes (make-string (* 3 1024 1024) 0)))
                                 (dotimes (index (length bytes) bytes)
                                   (aset bytes index (% (* index 7) 256)))))
                        (path (note "recording.bin" large)))
                   (should (start :automatic t))
                   (should-not (file-exists-p path))
                   (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse
                                                     workspace (pass) (plist-get (proposal 'note-delete "recording.bin") :id))
                                                    :status)))
                   (should (equal large (bytes path))))
                 :doc "oversized text keeps complete checked deletion and condensation undo"
                 (let* ((large (concat (make-string (* 40 1024) ?x) "\nOpen: verify remote.\n"))
                        (path (note "large.md" large)))
                   (set-file-modes path #o640)
                   (should (start :automatic t))
                   (should (plist-get (car (mevedel-memory-notes-documents scope-seen)) :chunked))
                   (setq reply (text nil (list (list (address "large.md") "condense" "Open: verify remote."))))
                   (finish)
                   (should (equal "Open: verify remote.\n" (bytes path)))
                   (should (eq 'applied (status (proposal 'note-condense))))
                   (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse
                                                     workspace (pass) (plist-get (proposal 'note-condense) :id)) :status)))
                   (should (equal large (bytes path)))
                   (should (= #o640 (file-modes path))))
                 :doc "worst-case escaped 512 KiB notes fit durable writes and checked undo"
                 (let* ((large (encode-coding-string (make-string (* 256 1024) #x00e9) 'utf-8-unix))
                        (path (note "escaped.md" large)))
                   ;; Two snapshots of 512 KiB with fourfold escaping exceed
                   ;; the old 4 MiB record bound once metadata is included.
                   (should (= mevedel-memory-notes-max-review-bytes (length large)))
                   (should (start :automatic t))
                   (setq reply (text nil (list (list (address "escaped.md") "condense" "Open handoff."))))
                   (finish)
                   (should (eq 'applied (status (proposal 'note-condense))))
                   (should (equal "Open handoff.\n" (bytes path)))
                   (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse
                                                     workspace (pass) (plist-get (proposal 'note-condense) :id)) :status)))
                   (should (equal large (bytes path))))
                 :doc "over-quota WAV housekeeping uses real freedesktop trash and remains undoable"
                 (let* ((trash-directory nil)
                        (data (file-name-concat root "xdg-data"))
                        (process-environment (cons (concat "XDG_DATA_HOME=" data) process-environment))
                        (large (make-string (* 5 1024 1024) 0))
                        (hash (secure-hash 'sha256 large))
                        (path (note "recording.wav" large)))
                   (set-file-modes path #o640)
                   (should (start :automatic t))
                   (should (= 0 calls))
                   (should (eq 'success (plist-get result :outcome)))
                   (should (eq 'applied (status (proposal 'note-delete))))
                   (should-not (file-exists-p path))
                   (should (equal large (bytes (file-name-concat data "Trash" "files" hash))))
                   (should (file-exists-p (file-name-concat data "Trash" "info" (concat hash ".trashinfo"))))
                   (should-not (file-exists-p (file-name-concat root ".mevedel" "state" "shared-notes" "blobs" hash)))
                   (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse
                                                     workspace (pass) (plist-get (proposal 'note-delete) :id)) :status)))
                   (should (equal large (bytes path)))
                   (should (= #o640 (file-modes path))))
                 :doc "a trash failure cannot delete the original or fabricate a successful housekeeping decision"
                 (let* ((trash-directory (file-name-concat root "trash"))
                        (mevedel-memory-notes-undo-quota 1)
                        (path (note "recording.wav" (unibyte-string 0 1 2))))
                   (cl-letf (((symbol-function 'move-file-to-trash) (lambda (file) (delete-file file))))
                     (should (start :automatic t)))
                   (should (eq 'unavailable (status (proposal 'note-delete))))
                   (should (equal (unibyte-string 0 1 2) (bytes path))))
                 :doc "a folder populated after capture is not pruned"
                 (let ((mevedel-memory-consolidation-mode 'propose)
                       (folder (file-name-concat shared "later")))
                   (make-directory folder)
                   (set-file-times folder (seconds-to-time (- now (* 3600 48))))
                   (should (start :automatic t))
                   (write-region "New work.\n" nil (file-name-concat folder "new.md") nil 'silent)
                   (should (eq 'stale (plist-get (mevedel-memory-decision-apply
                                                  workspace (pass) (plist-get (proposal 'note-prune "later") :id))
                                                 :status)))
                   (should (file-exists-p (file-name-concat folder "new.md")))))

(provide 'test-mevedel-memory-pass-notes)
;;; test-mevedel-memory-pass-notes.el ends here
