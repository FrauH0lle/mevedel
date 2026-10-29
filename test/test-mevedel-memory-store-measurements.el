;;; test-mevedel-memory-store-measurements.el -- Numeric evidence guards -*- lexical-binding: t -*-

;;; Commentary:

;; Conservative paragraph recognition supplements the full pass path tests.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-journal-claim)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-workspace-identity)

(defconst mevedel-test-memory-store-measurements--known-units
  "Occupancy: 1.26%.\nElapsed interval: 457ms."
  "Observation paragraph that the old unit vocabulary already recognized.")

(defconst mevedel-test-memory-store-measurements--observation
  (concat "Scenario: local snapshot with the same observer and input.\n"
          "Matched entries: 584.\n"
          "Retained entries: 67.\n"
          "Relative allocation: 2.491.\n\n"
          mevedel-test-memory-store-measurements--known-units)
  "Separate domain-neutral numeric and already-recognized unit paragraphs.")

(mevedel-deftest mevedel-memory-store--measurements ()
  ,test
  (test)
  :doc "protects operation, values, scenario and corrections rather than bare numbers"
  (let ((paragraph "Scenario: 64 decisions, one sample.\n- **Read:** 4,294 programs in 1 sample.\n- Existence: 680 programs."))
    (should (equal (list paragraph "Correction: the 2,736-program leaf is not the separate 64/192 case.")
                   (mevedel-memory-store--measurements
                    (concat "# Counts 2026-10-27\n\n" paragraph
                            "\n\nCorrection: the 2,736-program leaf is not the separate 64/192 case.\n\nOpen: batch probes.")))))
  :doc "source lines, dates, commit hashes and nonnumeric progress are not measurements"
  (should-not (mevedel-memory-store--measurements
               "# Note 2026-10-27\n\nSource: code.el:123-456, commit 5ba2d6e4.\n\nOpen: investigate the remote case."))
  :doc "raw tab-separated counts are measurements, not Read line numbers"
  (should (equal '("512\treads on target A.")
                 (mevedel-memory-store--measurements "512\treads on target A.")))
  :doc "standalone counts, decimals, percentages and durations need no known unit vocabulary"
  (dolist (paragraph '("Matched entries: 584." "Retained entries: 67."
                       "Matched entries: 20." "Matched entries: 0020."
                       "Matched entries: 584674293756."
                       "Relative allocation: 2.491." "Relative allocation: 1.26." "Relative allocation: 0.5."
                       "Relative allocation: .5."
                       "Occupancy: 1.26%." "Occupancy: 7%." "Occupancy: 5%."
                       "Elapsed interval: 457ms." "Elapsed interval: 7ms." "Elapsed interval: 1ms."
                       "Time: 457ms/read." "Time: 2.491ms/op."
                       "Observed \"584 reads from src/foo.el\"."
                       "Observed \"584 records in data.log\"."))
    (ert-info ((format "Measurement paragraph: %s" paragraph))
      (should (equal (list paragraph) (mevedel-memory-store--measurements paragraph)))))
  :doc "a generic observation retains its operation, conditions and every numeric result"
  (should (equal (split-string mevedel-test-memory-store-measurements--observation "\n\n")
                 (mevedel-memory-store--measurements mevedel-test-memory-store-measurements--observation)))
  :doc "dates alone are not measurements"
  (dolist (paragraph '("Recorded on 2026-10-29." "Recorded on 2026/10/29."
                       "Recorded at 2026-10-29T12:34:56Z."))
    (should-not (mevedel-memory-store--measurements paragraph)))
  :doc "times of day alone are not measurements"
  (dolist (paragraph '("Recorded at 12:34:56 UTC." "Recorded at 12:34 UTC."))
    (should-not (mevedel-memory-store--measurements paragraph)))
  :doc "commit hashes alone are not measurements"
  (dolist (paragraph '("Commit: 584a67bcdef0123456789abcdef0123456789abcd."
                       "Commit: 5ba2d6e4." "Commit: 584674293756." "Hash: 584674293756."))
    (should-not (mevedel-memory-store--measurements paragraph)))
  :doc "numbers embedded in paths alone are not measurements"
  (dolist (paragraph '("Record: /tmp/run-584/results67.md."
                       "Record: reports/584/67.csv." "Record: ./67/foo.el."
                       "Record: ../foo-584.txt." "Record: ~/runs/67."
                       "Record: C:\\runs\\584\\foo.el." "Record: work://shared/run-584.md."
                       "Record: \"/tmp/run 584 67.log\"."))
    (should-not (mevedel-memory-store--measurements paragraph)))
  :doc "file line references, ranges and columns alone are not measurements"
  (dolist (paragraph '("Location: code.el:123." "Location: code.el:123-456."
                       "Location: src/worker.el:123:67." "Location: code.el:L123-L456."))
    (should-not (mevedel-memory-store--measurements paragraph)))
  :doc "versions alone are not measurements"
  (dolist (paragraph '("Version: v31.1.2." "Version: v1.26."
                       "Release version: 31.1." "Release: 31.1.0."
                       "Installed: package-1.26.0."))
    (should-not (mevedel-memory-store--measurements paragraph)))
  :doc "numbered list ordinals alone are not measurements"
  (dolist (paragraph '("12. Retain the context." "67) Retain the context."
                       "  67. Retain the context."))
    (should-not (mevedel-memory-store--measurements paragraph)))
  :doc "ordinary single digits and digits inside identifiers are not measurements"
  (dolist (paragraph '("The observer retained 7 entries." "The observer retained 0007 entries."
                       "Identifier: batch584." "Identifier: issue-584." "Identifier: case-67."))
    (should-not (mevedel-memory-store--measurements paragraph)))
  :doc "excluded metadata does not hide a true measurement in the same paragraph"
  (dolist (paragraph '("Recorded 2026-10-29 at 12:34, version v31.1.2, source code.el:123. Matched entries: 584."
                       "Record: /tmp/run-584/report.txt. Matched entries: 584."
                       "Record: \"/tmp/run 584 67.log\". Matched entries: 584."))
    (should (equal (list paragraph) (mevedel-memory-store--measurements paragraph))))
  :doc "major-mode syntax tables do not change numeric or excluded-metadata recognition"
  (dolist (mode '(fundamental-mode text-mode emacs-lisp-mode))
    (with-temp-buffer
      (funcall mode)
      (ert-info ((format "Calling major mode: %s" mode))
        (dolist (paragraph '("Duration: 4ms." "Time: 2.491ms/op."
                             "Relative allocation: .5." "Matched entries: 584."))
          (should (equal (list paragraph) (mevedel-memory-store--measurements paragraph))))
        (dolist (paragraph '("Location: code.el:123." "Record: \"/tmp/run 584 67.log\"."
                             "67. Retain context."))
          (should-not (mevedel-memory-store--measurements paragraph))))))
  :doc "huge alphanumeric tokens finish in a bounded child instead of quadratic filename backtracking"
  (with-temp-buffer
    (let ((child
           (make-process
            :name "measurement-large-token" :buffer (current-buffer)
            :sentinel #'ignore :noquery t :connection-type 'pipe
            :command
            (list (expand-file-name invocation-name invocation-directory) "--batch" "-Q" "--eval"
                  (prin1-to-string
                   `(progn
                      (setq load-path ',load-path native-comp-jit-compilation nil)
                      (require 'mevedel-memory-store)
                      (let ((large (make-string (* 256 1024) #x00e9)))
                        (dolist (token (list large (concat large ".")))
                          (when (mevedel-memory-store--measurements token)
                            (error "Alphanumeric token became a measurement"))))))))))
      (unwind-protect
          (progn
            (with-timeout (10 (ert-fail "Measurement child exceeded its bounded deadline"))
              (while (process-live-p child) (accept-process-output child 0.05)))
            (should (= 0 (process-exit-status child)))
            (should (equal "" (buffer-string))))
        (when (process-live-p child) (delete-process child))))))

(mevedel-deftest mevedel-memory-store-accept/structural-measurements ()
  ,test
  (test)
  :doc "production acceptance and application retain lost observations but allow intact condensation"
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "memory-structural-" t))))
         (workspace (mevedel-workspace--create :root root))
         (mevedel-memory-dirs nil)
         (shared (file-name-concat root ".mevedel" "shared"))
         (observation mevedel-test-memory-store-measurements--observation)
         (original (concat "Completed introduction.\n\n" observation "\n"))
         ;; Preserve the already-recognized paragraph so only structural
         ;; recognition prevents loss of the bare counts and their conditions.
         (cases (list (list "lost.md" 'note-condense mevedel-test-memory-store-measurements--known-units)
                      (list "prefix.md" 'note-condense (string-replace "584" "1584" observation))
                      (list "scenario.md" 'note-condense (string-replace "same observer" "different observer" observation))
                      (list "preserved.md" 'note-condense observation)
                      (list "delete.md" 'note-condense nil)))
         claim)
    (unwind-protect
        (progn
          (mevedel-workspace-identity-ensure root)
          (make-directory shared t)
          (dolist (case cases)
            (let ((path (file-name-concat shared (car case))))
              (write-region original nil path nil 'silent)
              (set-file-times path (seconds-to-time (- (float-time) 172800)))))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (capture (mevedel-memory-notes-capture workspace nil :quiet-hours 0))
                 (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                                "## Remove\n- none\n## Instructions\n- none\n## Notes\n"
                                (mapconcat
                                 (lambda (case)
                                   (format "````proposal\nnote: %S\ndisposition: %S\nreason: \"Completed review\"\n%s---\n%s````\n"
                                           (mevedel-memory-notes-address (car case))
                                           (if (nth 2 case) "condense" "delete")
                                           (if (nth 2 case) "" "open-items: []\n")
                                           (if (nth 2 case) (concat (nth 2 case) "\n") "")))
                                 cases "")
                                "## No action\n- none")))
            (should (= (length cases) (plist-get capture :notes)))
            (push (plist-get capture :root) (plist-get scope :roots))
            (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
            (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                   (pass (plist-get prepared :id))
                   (accepted (mevedel-memory-store-accept
                              workspace prepared reply nil "test:model" nil
                              (mapcar #'car (mevedel-memory-notes-index scope))))
                   (proposals (plist-get accepted :proposals)))
              (should (= (length cases) (length proposals)))
              (dolist (case cases)
                (let* ((proposal (seq-find (lambda (item) (equal (car case) (plist-get item :file))) proposals))
                       (guarded (not (equal "preserved.md" (car case))))
                       (path (file-name-concat shared (car case))))
                  (ert-info ((format "Accepted measurement note: %s" (car case)))
                    (should proposal)
                    (should (eq (nth 1 case) (plist-get proposal :action)))
                    (should (eq guarded (not (null (plist-get proposal :guard-preserved)))))
                    (should-not (plist-get proposal :guard-converted))
                    (when guarded
                      (should (string-search "retained verbatim" (plist-get proposal :reason))))
                    (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass (plist-get proposal :id))
                                                  :status)))
                    (with-temp-buffer
                      (insert-file-contents-literally path)
                      (let ((written (buffer-string)))
                        (should-not (string-search "Completed introduction." written))
                        (dolist (paragraph (split-string observation "\n\n"))
                          (should (string-search paragraph written)))
                        (unless guarded (should (equal (concat observation "\n") written)))))
                    (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get proposal :id))
                                                    :status)))
                    (with-temp-buffer
                      (insert-file-contents-literally path)
                      (should (equal original (buffer-string))))))))))
      (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t)))
  :doc "a large escaped preimage survives checked preservation, recovery reads and exact undo"
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "memory-preserved-large-" t))))
         (workspace (mevedel-workspace--create :root root))
         (mevedel-memory-dirs nil)
         (shared (file-name-concat root ".mevedel" "shared"))
         (path (file-name-concat shared "large.md"))
         (measurement (concat "Measured 584 results under identical conditions.\n"
                              (make-string (* 255 1024) #x00e9)))
         (original (encode-coding-string (concat measurement "\n") 'utf-8-unix))
         claim)
    (unwind-protect
        (progn
          (mevedel-workspace-identity-ensure root)
          (make-directory shared t)
          (let ((coding-system-for-write 'no-conversion)) (write-region original nil path nil 'silent))
          (set-file-times path (seconds-to-time (- (float-time) 172800)))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (capture (mevedel-memory-notes-capture workspace nil :quiet-hours 0)))
            (should (= 1 (plist-get capture :notes)))
            (push (plist-get capture :root) (plist-get scope :roots))
            (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
            (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                   (pass (plist-get prepared :id))
                   (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                                  "## Remove\n- none\n## Instructions\n- none\n## Notes\n"
                                  "````proposal\nnote: \"work://shared/large.md\"\ndisposition: \"delete\"\n"
                                  "reason: \"Completed review\"\nopen-items: []\n---\n````\n## No action\n- none"))
                   (accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil
                                                          (mapcar #'car (mevedel-memory-notes-index scope))))
                   (item (car (plist-get accepted :proposals))))
              (should (plist-get item :guard-preserved))
              (should (eq 'note-condense (plist-get item :action)))
              (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :status)))
              (should (cl-some (lambda (file) (> (file-attribute-size (file-attributes file)) (* 5 1024 1024)))
                               (directory-files (mevedel-memory-write--directory workspace) t "\\.el\\'")))
              (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get item :id)) :status)))
              (with-temp-buffer
                (insert-file-contents path)
                (should (equal (concat "Measurements retained from work://shared/large.md.\n\n" measurement "\n") (buffer-string))))
              (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status)))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally path)
                (should (equal original (buffer-string)))))))
      (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t))))

(mevedel-deftest mevedel-memory-store--preserved ()
  (let* ((paragraphs '("  Scenario: 584 results.\nBounded cold run." "Allocation: 2.491 with the same observer."))
         (proposal '(:action note-condense :note "work://shared/observation.md" :body "Still useful.\n" :reason "Reviewed"))
         (kept (mevedel-memory-store--preserved proposal paragraphs)))
    (should (equal "Still useful.\n\n## Preserved measurements\n\n  Scenario: 584 results.\nBounded cold run.\n\nAllocation: 2.491 with the same observer."
                   (plist-get kept :body)))
    (should (eq 'note-condense (plist-get kept :action)))
    (should (plist-get kept :guard-preserved))
    (should-not (plist-get kept :guard-converted))
    (should (equal "Still useful.\n" (plist-get proposal :body)))
    (should-not (plist-member proposal :guard-preserved))
    (should (equal (concat "Measurements retained from work://shared/observation.md.\n\n"
                           (string-join paragraphs "\n\n"))
                   (plist-get (mevedel-memory-store--preserved
                               '(:action note-delete :note "work://shared/observation.md" :body "" :reason "Reviewed")
                               paragraphs) :body)))))

(mevedel-deftest mevedel-memory-store--contains-paragraph-p ()
  ,test
  (test)
  (progn
  (should (mevedel-memory-store--contains-paragraph-p
           "512 reads in 2 samples on target A." "**512 reads**\nin 2 samples on target A."))
  (should (mevedel-memory-store--contains-paragraph-p
           "512 reads in 2 samples on target A." "\"512 reads in 2 samples on target A.\""))
  (should (mevedel-memory-store--contains-paragraph-p
           "512 reads in 2 samples on target A." "\u201c512 reads in 2 samples on target A.\u201d"))
  (should-not (mevedel-memory-store--contains-paragraph-p
               "512 reads in 2 samples on target A." "1512 reads in 2 samples on target A."))
  (should-not (mevedel-memory-store--contains-paragraph-p
               "512 reads in 2 samples on target A." "-512 reads in 2 samples on target A."))
  (should-not (mevedel-memory-store--contains-paragraph-p
               "512 reads in 2 samples on target A." "512 reads in 2 samples on target B."))
  (should-not (mevedel-memory-store--contains-paragraph-p
               "512\treads in 2 samples on target A." "1512\treads in 2 samples on target A."))
  (should (mevedel-memory-store--contains-paragraph-p
           "512\treads on target A." "  352\t512\treads on target A." t)))
  :doc "literal matching continues after invalid hits and preserves case-fold-search"
  (let ((paragraph "512 reads on target A."))
    (should (mevedel-memory-store--contains-paragraph-p paragraph "1512 reads on target A. 512 reads on target A."))
    (let ((case-fold-search t))
      (should (mevedel-memory-store--contains-paragraph-p paragraph "512 READS ON TARGET A.")))
    (let ((case-fold-search nil))
      (should-not (mevedel-memory-store--contains-paragraph-p paragraph "512 READS ON TARGET A.")))))

(mevedel-deftest mevedel-memory-store--plain ()
  (progn
    (should (equal "512 reads" (mevedel-memory-store--plain "512\treads")))
    (should (equal "512 reads" (mevedel-memory-store--plain "  352\t512\treads" t)))))

(mevedel-deftest mevedel-memory-store--anchor-p ()
  (progn
    (should (mevedel-memory-store--anchor-p "Runtime  ACTIVATION" "The runtime\nactivation completed."))
    (should-not (mevedel-memory-store--anchor-p "activation" "The deactivation completed."))
    (should-not (mevedel-memory-store--anchor-p "" "The check completed."))
    (should-not (mevedel-memory-store--anchor-p "***" "The check completed."))))

(mevedel-deftest mevedel-memory-store--resolution-anchor-p ()
  (progn
    (should (mevedel-memory-store--resolution-anchor-p "activation" "\"The activation completed.\" (lines 3-5)"))
    (should-not (mevedel-memory-store--resolution-anchor-p "activation" "\"The merge completed.\" activation"))))

(provide 'test-mevedel-memory-store-measurements)
;;; test-mevedel-memory-store-measurements.el ends here
