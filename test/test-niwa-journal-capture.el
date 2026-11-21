;;; test-niwa-journal-capture.el -- Durable journal capture -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises completed-turn capture through the real save boundary.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-journal-process)
;; Agent compaction cases name their target directly; production loads this
;; lazily inside a run, which an isolated case may not have started yet.
(require 'niwa-compact-target)

(niwa-deftest niwa-journal-capture-checkpoint ()
  ,test
  (test)
  :doc "freezes a completed saved turn and survives source-buffer death"
  (cl-destructuring-bind (workspace . directory)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let ((buffer (generate-new-buffer " *journal-capture-test*"))
          (session (niwa-session-create "capture" workspace))
          (niwa-journal-enabled t))
      (unwind-protect
          (niwa-skills-test--with-model-backends
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--session session
                          gptel-backend (gptel-get-backend "Fast")
                          gptel-model 'fast-model)
              (gptel-mode 1)
              (insert "Use the local runner.\n")
              (insert (propertize "The local runner passed.\n" 'gptel 'response))
              (setf (niwa-session-turn-count session) 1)
              (niwa-session-artifacts-save session buffer t)
              (let ((notes (file-name-concat (niwa-session-save-path session)
                                             "local" "notes.md")))
                (make-directory (file-name-directory notes) t)
                (write-region "User correction: local runner.\n" nil notes nil 'silent))
              (let* ((capture (niwa-journal-capture-checkpoint session buffer))
                     (id (plist-get capture :id)))
                (should id)
                (should (equal '(1) (plist-get (plist-get capture :metadata) :turns)))
                (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
                (should (equal id (plist-get
                                   (niwa-journal-capture-checkpoint session buffer) :id)))
                (should (= 1 (length (niwa-journal-capture-list workspace))))
                (let ((source (niwa-journal-capture-evidence workspace capture)))
                  (should (string-match-p "local runner passed" source))
                  (should (string-match-p "User correction" source))
                  (should-not (string-match-p "fork-point.*:type" source)))
                (set-buffer-modified-p nil)
                (kill-buffer buffer)
                (should (equal id (plist-get (car (niwa-journal-capture-list workspace)) :id)))
                (should (string-match-p
                         "local runner passed"
                         (niwa-journal-capture-evidence workspace capture))))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (when (niwa-session-save-path session)
          (niwa-session-persistence-lock-release (niwa-session-save-path session) session))
        (delete-directory directory t)
        (niwa-workspace-clear-registry))))

  :doc "tool volume cannot displace middle dialogue and the frozen capture stays bounded"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn
      session buffer "Opening user request: preserve local state." "Initial assistant response.")
     (with-current-buffer buffer
       (goto-char (point-max))
       (let ((start (point)))
         (insert "\nMiddle user correction: batch operations first.\n")
         (remove-text-properties start (point) '(gptel nil)))
       (insert (propertize "Middle assistant decision: permit a helper when batching is insufficient.\n"
                           'gptel 'response))
       (dotimes (index 64)
         (insert "#+begin_tool\n")
         (let ((start (point)))
           (insert "(:name \"Read\" :args (:file_path \"fixture.txt\"))\n\n"
                   "Observed tool output.\n" (make-string 4000 ?x))
           (put-text-property start (point) 'gptel (cons 'tool (format "read-%d" index))))
         (insert "\n#+end_tool\n"))
       (insert (propertize "Final assistant conclusion: keep all user capabilities.\n" 'gptel 'response))
       (setf (niwa-session-turn-count session) 2)
       (niwa-session-artifacts-save session buffer t)
       (let* ((capture (niwa-journal-capture-checkpoint session buffer))
              (source (niwa-journal-capture-evidence (niwa-session-workspace session) capture)))
         (should (> (string-bytes (niwa-session-artifacts-read-artifact
                                  session "segment-0001.chat.org" t)) (* 128 1024)))
         (should (<= (string-bytes source) (* 96 1024)))
         (should (string-match-p "Opening user request" source))
         (should (string-match-p "Middle user correction: batch operations first" source))
         (should (string-match-p "Middle assistant decision: permit a helper" source))
         (should (string-match-p "Final assistant conclusion" source))
         (should (string-match-p "Observed tool output" source))
         (should (string-match-p "remaining supporting evidence omitted" source))))))

  :doc "rejects an oversized descriptor before publication or pinning, preserving later capture eligibility"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Completed work" "Observed success"))
     (let ((niwa-journal-store--entry-max-bytes 256))
       (should-error (niwa-journal-capture-checkpoint session buffer)))
     (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
     (should-not (niwa-journal-capture-list (niwa-session-workspace session) t))
     (should (plist-get (niwa-journal-capture-checkpoint session buffer) :id))))

  :doc "public digest removal never reopens old turns when the saved session continues"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Old requirement" "Old observed result")
     (let* ((workspace (niwa-session-workspace session))
            (root (niwa-workspace-root workspace))
            (capture (car (niwa-journal-capture-seal session buffer 'compaction)))
            (metadata (plist-put (niwa-journal-capture--metadata capture) :trigger 'compaction))
            (entry (niwa-journal-store-publish-digest
                    root metadata "## Done\n- Observed: completed (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (&rest _) (ert-fail "Published capture must retire without inference"))))
         (niwa-journal-process-next workspace))
       (should-not (niwa-journal-capture-list workspace))
       (delete-file (file-name-concat (niwa-journal-store-directory root) (plist-get entry :file)))
       (should-not (niwa-journal-capture-checkpoint session buffer))
       (niwa-test-journal-capture--turn session buffer "New requirement" "New observed result")
       (let ((next (car (niwa-journal-capture-list workspace))))
         (should (equal '(2) (plist-get (plist-get next :metadata) :turns)))
         (should-not (equal (plist-get capture :id) (plist-get next :id)))))))

  :doc "auto-save refreshes one checkpoint and preserves only the newest pin"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((workspace (niwa-session-workspace session)))
       (niwa-test-journal-capture--turn session buffer "Use local runner" "Runner passed")
       (let* ((first (car (niwa-journal-capture-list workspace)))
              (first-id (plist-get first :id)))
         (should first-id)
         (niwa-test-journal-capture--turn session buffer "Check formatting" "Formatting passed")
         (let* ((captures (niwa-journal-capture-list workspace))
                (second (car captures)))
           (should (= 1 (length captures)))
           (should-not (equal first-id (plist-get second :id)))
           (should (equal '(1 2) (plist-get (plist-get second :metadata) :turns)))
           (should (string-match-p "Runner passed" (niwa-journal-capture-evidence workspace second)))
           (should (string-match-p "Formatting passed" (niwa-journal-capture-evidence workspace second)))
           (should-not (file-exists-p
                        (file-name-concat (niwa-session-save-path session) ".journal-pins"
                                          (concat first-id ".json"))))
           (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))))

  :doc "disabling capture preserves its pending snapshot and evidence pin"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((workspace (niwa-session-workspace session)))
       (niwa-test-journal-capture--turn session buffer "First request" "First result")
       (let ((first (car (niwa-journal-capture-list workspace)))
             (niwa-journal-enabled nil))
         (niwa-test-journal-capture--turn session buffer "Second request" "Second result")
         (should (equal (list first) (niwa-journal-capture-list workspace)))
         (should-not (string-match-p "Second result" (niwa-journal-capture-evidence workspace first)))
         (should (niwa-journal-pins-present-p (niwa-session-save-path session)))))))

  :doc "storage failure reports capture failure without failing the completed save"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let* ((root (niwa-workspace-root (niwa-session-workspace session)))
            (niwa--warn-once-table (make-hash-table :test #'equal))
            diagnostics)
       (make-directory (file-name-concat root ".niwa") t)
       (write-region "blocked private state directory" nil
                     (file-name-concat root ".niwa" "state") nil 'silent)
       (niwa-test--with-captured-diagnostics diagnostics
         (niwa-test-journal-capture--turn session buffer "Completed request" "Completed result"))
       (should (string-match-p "Journal capture checkpoint failed" diagnostics))
       (should-not (buffer-local-value 'niwa-session--save-failed buffer))
       (should (= 1 (plist-get (niwa-session-codec-read
                                (niwa-session-artifacts-sidecar-path
                                 (niwa-session-save-path session))) :total-turn-count))))))

  :doc "published stable turn identities stay covered when later work is captured"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let* ((workspace (niwa-session-workspace session))
            (root (niwa-workspace-root workspace)))
       (niwa-test-journal-capture--turn session buffer "First request" "First result")
       (let* ((first (car (niwa-journal-capture-list workspace)))
              (metadata (copy-tree (plist-get first :metadata))))
         (plist-put metadata :trigger 'session-end)
         (niwa-journal-store-publish-digest
          root metadata "## Done\n- First result (turn 1).\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none")
         (should-not (niwa-journal-capture-checkpoint session buffer))
         (niwa-test-journal-capture--turn session buffer "Second request" "Second result")
         (let ((second (cl-find-if
                        (lambda (record) (equal '(2) (plist-get (plist-get record :metadata) :turns)))
                        (niwa-journal-capture-list workspace))))
           (should second)
           (should (string-match-p "Second result" (niwa-journal-capture-evidence workspace second)))
           (should-not (string-match-p "First result" (niwa-journal-capture-evidence workspace second))))))))

  :doc "a different saved branch with the same turn number has a distinct identity"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((workspace (niwa-session-workspace session)))
       (niwa-test-journal-capture--turn session buffer "First request" "First branch result")
       (let ((first (car (niwa-journal-capture-list workspace))))
         (with-current-buffer buffer
           (let ((inhibit-read-only t)) (erase-buffer)))
         (setf (niwa-session-turn-count session) 0
               (niwa-session-prompt-index session) nil)
         (niwa-test-journal-capture--turn session buffer "Replacement request" "Second branch result")
         (let* ((records (niwa-journal-capture-list workspace))
                (second (cl-find-if (lambda (record) (not (equal (plist-get first :id) (plist-get record :id)))) records)))
           (should (= 2 (length records)))
           (should second)
           (should (equal '(1) (plist-get (plist-get second :metadata) :turns)))
           (should-not (equal (plist-get (plist-get first :metadata) :turn-ids)
                              (plist-get (plist-get second :metadata) :turn-ids))))))))

  :doc "portable capture pins the saved target publication under its lease"
  (let ((local-root (file-name-as-directory (make-temp-file "niwa-journal-portable-" t)))
        (buffer (generate-new-buffer " *journal-portable*"))
        (niwa-journal-enabled t))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("journal-portable")
          (cl-destructuring-bind (workspace session session-dir _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               "journal-portable" local-root "")
            (let ((niwa-session-durability--disclosed-targets (make-hash-table :test #'equal)))
              (puthash (niwa-execution-target-identity (niwa-session-execution-target session))
                       t niwa-session-durability--disclosed-targets)
              (should (niwa-session-durability-lease-acquire session-dir "*journal-portable*" session))
              (unwind-protect
                  (niwa-skills-test--with-model-backends
                    (with-current-buffer buffer
                      (org-mode)
                      (setq-local niwa--session session
                                  default-directory (niwa-workspace-root workspace)
                                  gptel-backend (gptel-get-backend "Fast")
                                  gptel-model 'fast-model)
                      (gptel-mode 1)
                      (niwa-session-set-root-buffer session buffer)
                      (setf (niwa-session-publication session)
                            (niwa-session-publication-read session-dir))
                      (niwa-test-journal-capture--turn session buffer "Check target" "Target passed")
                      (let* ((capture (car (niwa-journal-capture-list workspace)))
                             (head (plist-get (niwa-session-publication session) :head)))
                        (should capture)
                        (should head)
                        (should (equal head (plist-get capture :head)))
                        (should (member head (niwa-journal-pins-heads session-dir)))
                        (should (equal "target" (plist-get capture :source-kind)))
                        (should (equal (file-local-name session-dir) (plist-get capture :source-directory)))
                        (should (string-match-p "Target passed"
                                                (niwa-journal-capture-evidence workspace capture)))
                        (niwa-session-durability-lease-release session-dir session)
                        (should-not (niwa-journal-capture-seal session buffer 'session-end))
                        (should-not (niwa-journal-capture-trigger workspace capture))
                        (should (niwa-session-durability-lease-acquire session-dir "*journal-portable*" session))
                        (should (niwa-journal-capture-seal session buffer 'session-end))
                        (should (eq 'session-end (niwa-journal-capture-trigger workspace capture)))
                        (cl-letf (((symbol-function 'gptel-request)
                                   (lambda (prompt &rest args)
                                     (should (string-match-p "Target passed" prompt))
                                     (funcall (plist-get args :callback)
                                              "## Done\n- Observed: Target passed (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"
                                              nil))))
                          (should (niwa-journal-process-next workspace)))
                        (let ((entry (car (niwa-journal-store-entries (niwa-workspace-root workspace)))))
                          (should (equal (plist-get capture :id) (plist-get entry :capture-id)))
                          (should (string-match-p "Target passed" (plist-get entry :body))))
                        (should-not (niwa-journal-pins-present-p session-dir))
                        (should-not (niwa-journal-capture-list workspace)))))
                (let ((niwa-journal-enabled nil))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer (set-buffer-modified-p nil))
                    (kill-buffer buffer)))
                (niwa-session-durability-lease-release session-dir session)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory local-root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-journal-capture-seal ()
  ,test
  (test)
  :doc "clear sealing preserves selected evidence, title, first trigger, and turn coverage"
  (dolist (first-trigger '(clear compaction session-end idle))
    (niwa-test-journal-capture--with-session
     (lambda (session buffer)
       (niwa-test-journal-capture--turn session buffer "First request" "Completed first result")
       (let* ((workspace (niwa-session-workspace session))
              (captures (niwa-journal-capture-list workspace))
              (capture (car captures))
              (metadata (copy-tree (plist-get capture :metadata)))
              (evidence (niwa-journal-capture-evidence workspace capture))
              (completed-end (with-current-buffer buffer (point-max))))
         (setf (niwa-session-name session) "Later title")
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert "Pending prompt\n" (propertize "Incomplete streaming response" 'gptel 'response)))
         (should (equal captures (niwa-journal-capture-seal session buffer first-trigger captures)))
         (dolist (trigger '(clear clear compaction session-end))
           (should (equal captures (niwa-journal-capture-seal session buffer trigger captures)))
           (should (eq first-trigger (niwa-journal-capture-trigger workspace capture))))
         (should-not (niwa-journal-capture-checkpoint session buffer))
         (should (equal captures (niwa-journal-capture-list workspace)))
         ;; Discard the unfinished output before completing a different turn.
         (with-current-buffer buffer
           (delete-region completed-end (point-max)))
         (niwa-test-journal-capture--turn session buffer "Second request" "Completed second result")
         (let* ((records (niwa-journal-capture-list workspace))
                (old (cl-find (plist-get capture :id) records
                              :key (lambda (record) (plist-get record :id)) :test #'equal))
                (new (cl-find-if (lambda (record) (not (equal capture record))) records)))
           (should (= 2 (length records)))
           (should (equal metadata (plist-get old :metadata)))
           (should (equal '(2) (plist-get (plist-get new :metadata) :turns)))
           (should (equal evidence (niwa-journal-capture-evidence workspace old)))
           (should-not (string-match-p "Incomplete streaming response" evidence))
           (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))))

  :doc "sealing freezes the trigger and reserves exactly the captured turns"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((workspace (niwa-session-workspace session)))
       (niwa-test-journal-capture--turn session buffer "First request" "Completed first result")
       (let* ((capture (car (niwa-journal-capture-list workspace)))
              (evidence (niwa-journal-capture-evidence workspace capture)))
         (should-not (niwa-journal-capture-trigger workspace capture))
         (should (equal (list capture) (niwa-journal-capture-seal session buffer 'compaction)))
         (should (eq 'compaction (niwa-journal-capture-trigger workspace capture)))
         (niwa-journal-capture-seal session buffer 'session-end)
         (should (eq 'compaction (niwa-journal-capture-trigger workspace capture)))
         (niwa-test-journal-capture--turn session buffer "Second request" "Completed second result")
         (let* ((captures (niwa-journal-capture-list workspace))
                (new (cl-find-if (lambda (entry) (not (equal (plist-get capture :id) (plist-get entry :id)))) captures)))
           (should (= 2 (length captures)))
           (should (equal '(2) (plist-get (plist-get new :metadata) :turns)))
           (should (equal evidence (niwa-journal-capture-evidence workspace capture)))
           (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))))

  :doc "sealing uses the checkpoint without capturing unfinished buffer output"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((workspace (niwa-session-workspace session)))
       (niwa-test-journal-capture--turn session buffer "Completed request" "Completed result")
       (with-current-buffer buffer
         (goto-char (point-max))
         (insert "Pending user prompt\n" (propertize "Incomplete streaming response" 'gptel 'response)))
       (let ((capture (car (niwa-journal-capture-seal session buffer 'session-end))))
         (should capture)
         (should (eq 'session-end (niwa-journal-capture-trigger workspace capture)))
         (should-not (string-match-p "Incomplete streaming response"
                                     (niwa-journal-capture-evidence workspace capture)))))))

  :doc "disabled journaling leaves the checkpoint unsealed"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (niwa-journal-enabled nil))
       (should-not (niwa-journal-capture-seal session buffer 'session-end))
       (should-not (niwa-journal-capture-trigger workspace capture))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "root buffer close seals its completed checkpoint before releasing authority"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace))))
       (with-current-buffer buffer
         (add-hook 'kill-buffer-hook #'niwa-session-persistence-release-on-kill nil t)
         (set-buffer-modified-p nil))
       (kill-buffer buffer)
       (should (eq 'session-end (niwa-journal-capture-trigger workspace capture)))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "exit seals completed work without starting inference"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            requested)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (&rest _) (setq requested t)))
                 ((symbol-function 'buffer-list)
                  (lambda (&optional _frame) (list buffer))))
         (niwa-session-persistence--kill-emacs-hook))
       (should-not requested)
       (should (eq 'session-end (niwa-journal-capture-trigger workspace capture)))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "successful root compaction seals the frozen pre-compaction checkpoint"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Check local runner" "Local runner passed")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (evidence (niwa-journal-capture-evidence workspace capture))
            (result 'unfinished))
       (niwa-test--with-captured-diagnostics nil
         (with-current-buffer buffer
           (niwa-compact-run-start
            :aggressive t :prepared-summary test-niwa-compact--valid-summary
            :callback (lambda (err) (setq result err)))))
       (should-not result)
       (should (= 2 (niwa-session-current-segment session)))
       (should (eq 'compaction (niwa-journal-capture-trigger workspace capture)))
       (should (equal evidence (niwa-journal-capture-evidence workspace capture)))
       (should (string-match-p "Local runner passed" evidence)))))

  :doc "failed compaction leaves its checkpoint unsealed and pinned"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            result)
       (niwa-test--with-captured-diagnostics nil
         (with-current-buffer buffer
           (niwa-compact-run-start
            :aggressive t :prepared-summary test-niwa-compact--valid-summary
            :summary-ready (lambda (_) (error "Cannot prepare summary"))
            :callback (lambda (err) (setq result err)))))
       (should (string-match-p "Cannot prepare summary" result))
       (should (= 1 (niwa-session-current-segment session)))
       (should-not (niwa-journal-capture-trigger workspace capture))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "successful agent compaction does not create root journal work"
  (let ((niwa-journal-enabled t))
    (test-niwa-compact--with-persisted-agent
        (agent-buffer invocation session canonical-path parent-buffer)
      (niwa-skills-test--with-model-backends
        (setq-local gptel-backend (gptel-get-backend "Fast")
                    gptel-model 'fast-model)
        (test-niwa-compact--insert-agent-task invocation "inspect" "Check the source")
        (insert (propertize "Source inspected\n" 'gptel 'response))
        (let ((result 'unfinished))
          (niwa-test--with-captured-diagnostics nil
            (should (niwa-agent-conversation-save invocation))
            (niwa-compact-run-start
             :aggressive t :target (niwa-compact-target-agent-target invocation)
             :prepared-summary test-niwa-compact--valid-summary
             :callback (lambda (err) (setq result err))))
          (should-not result)
          (should (string-match-p "begin_summary" (buffer-string)))
          (should-not (niwa-journal-capture-list (niwa-session-workspace session)))
          (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))))))

  :doc "closing either half of the view pair seals its completed checkpoint"
  (dolist (close-view '(t nil))
    (niwa-test-journal-capture--with-session
     (lambda (session buffer)
       (niwa-test-journal-capture--turn session buffer "Request" "Result")
       (let* ((workspace (niwa-session-workspace session))
              (capture (car (niwa-journal-capture-list workspace)))
              (view (generate-new-buffer " *journal-close-view*")))
         (unwind-protect
             (progn
               (with-current-buffer buffer
                 (add-hook 'kill-buffer-hook #'niwa-session-persistence-release-on-kill nil t)
                 (set-buffer-modified-p nil))
               (niwa-view--setup view buffer)
               (kill-buffer (if close-view view buffer))
               (should-not (buffer-live-p buffer))
               (should (eq 'session-end (niwa-journal-capture-trigger workspace capture)))
               (should (niwa-journal-pins-present-p (niwa-session-save-path session))))
           (when (buffer-live-p view) (kill-buffer view)))))))

  :doc "read-only roots do not seal another owner's pending checkpoint"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace))))
       (with-current-buffer buffer
         (setq-local niwa-session--read-only-mode t))
       (should-not (niwa-journal-capture-seal session buffer 'session-end))
       (should-not (niwa-journal-capture-trigger workspace capture))))))

(niwa-deftest niwa-journal-capture--turns ()
  ,test
  (test)
  :doc "reads a turn continued past auto-compaction from its later segment"
  (let ((session (niwa-session--create
                  :turn-count 4
                  :prompt-index
                  '((2)
                    (1 (:turn 1 :file-turn 1 :cum-turn 3 :pos 1
                        :fork-point-id "fp-3" :transcript-cutoff 90)
                       (:turn 2 :file-turn 2 :cum-turn 4 :pos 95
                        :continuation (:segment 2 :start 40
                                       :fork-point-id "fp-4"
                                       :transcript-cutoff 700)))))))
    (should (equal '((3 1 1 90 "fp-3") (4 2 40 700 "fp-4"))
                   (mapcar (lambda (turn)
                             (list (plist-get turn :number)
                                   (plist-get turn :segment)
                                   (plist-get turn :start)
                                   (plist-get turn :end)
                                   (plist-get turn :fork-point)))
                           (niwa-journal-capture--turns session)))))

  :doc "skips turns without a fork point or continuation"
  (should-not (niwa-journal-capture--turns
               (niwa-session--create
                :turn-count 1
                :prompt-index '((1 (:turn 1 :file-turn 1 :cum-turn 1 :pos 1)))))))

(niwa-deftest niwa-journal-capture--freeze-policy ()
  ,test
  (test)
  :doc "freezes explicit effort and output limits without inventing defaults"
  (niwa-skills-test--with-model-backends
    (let ((model (make-symbol "quality-model"))
          (gptel-stream t))
      (put model :reasoning-effort '(member disabled high max))
      (dolist (gptel-max-tokens '(nil 700 16000))
        (dolist (effort '(nil high disabled))
          (cl-letf (((symbol-function 'niwa-model-resolve-workload)
                     (lambda (workload &rest _)
                       (should (eq workload 'journal))
                       (list :backend (gptel-get-backend "Fast") :model model :effort effort))))
            (let ((policy (niwa-journal-capture--freeze-policy)))
              (should (equal (and effort (symbol-name effort)) (plist-get policy :effort)))
              (should (equal gptel-max-tokens (plist-get policy :max-tokens)))
              (should (eq t (plist-get policy :stream))))))))))

(niwa-deftest niwa-journal-capture--read ()
  ,test
  (test)
  :doc "capture metadata admits clear and idle while rejecting triggers outside the closed vocabulary"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Completed result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (path (niwa-journal-capture--file workspace id "capture.json"))
            (original (niwa-session-control-fs-read-file path)))
       (unwind-protect
           (dolist (trigger '("clear" "compaction" "session-end" "idle" "manual" "CLEAR"))
             (let ((record (json-parse-string original)))
               (puthash "trigger" trigger (gethash "metadata" record))
               (write-region (json-serialize record) nil path nil 'silent)
               (if (member trigger '("clear" "compaction" "session-end" "idle"))
                   (should (eq (intern trigger)
                               (plist-get (niwa-journal-capture--metadata
                                           (niwa-journal-capture--read workspace id)) :trigger)))
                 (should-error (niwa-journal-capture--read workspace id)))))
         (write-region original nil path nil 'silent)))))

  :doc "capture decoding preserves absent and large limits and rejects malformed limits"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Preference" "Acknowledged")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (path (niwa-journal-capture--file workspace id "capture.json"))
            (original (niwa-session-control-fs-read-file path)))
       (unwind-protect
           (dolist (limit '(:null 700 16000 0 -1 "4000"))
             (let ((record (json-parse-string original)))
               (puthash "max-tokens" limit (gethash "policy" record))
               (write-region (json-serialize record) nil path nil 'silent)
               (if (memq limit '(:null 700 16000))
                   (should (equal (unless (eq limit :null) limit)
                                  (plist-get (plist-get (niwa-journal-capture--read workspace id)
                                                        :policy) :max-tokens)))
                 (should-error (niwa-journal-capture--read workspace id)))))
         (write-region original nil path nil 'silent)))))

  :doc "rejects malformed provenance and missing or unknown descriptor/policy fields"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Check the result" "The result passed")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (path (file-name-concat (niwa-journal-capture--directory workspace id) "capture.json"))
            (original (niwa-session-control-fs-read-file path)))
       (unwind-protect
           (dolist (mutation
                    (list
                     (lambda (record) (remhash "head" record))
                     (lambda (record)
                       (remhash "head" record)
                       (puthash "unexpected" nil record))
                     (lambda (record) (puthash "sources" [] record))
                     (lambda (record)
                       (puthash "logical" "../secret" (aref (gethash "sources" record) 0)))
                     (lambda (record)
                       (puthash "sha256" "invalid" (aref (gethash "sources" record) 0)))
                     (lambda (record)
                       (puthash "extra" t (aref (gethash "sources" record) 0)))
                     (lambda (record)
                       (puthash "sources" (vconcat (gethash "sources" record) (gethash "sources" record)) record))
                     (lambda (record)
                       (puthash "source-revision" (make-string 64 ?0) (gethash "metadata" record)))
                     (lambda (record)
                       (let ((policy (gethash "policy" record)))
                         (remhash "stream" policy)
                         (puthash "unexpected" t policy)))))
             (let ((record (json-parse-string original)))
               (funcall mutation record)
               (write-region (json-serialize record) nil path nil 'silent)
               (should-error (niwa-journal-capture--read workspace id))
               (should (niwa-journal-pins-present-p (niwa-session-save-path session)))))
         (write-region original nil path nil 'silent))
       (should (equal id (plist-get (niwa-journal-capture--read workspace id) :id)))))))

(niwa-deftest niwa-journal-capture-seal-and-schedule ()
  ,test
  (test)
  :doc "schedules processing after a successful seal"
  (let* ((root (make-temp-file "niwa-seal-schedule-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace))
         scheduled)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-journal-capture-seal) (lambda (&rest _) '(sealed)))
                  ((symbol-function 'niwa-journal-process-schedule)
                   (lambda (ws &rest _) (setq scheduled ws))))
          (with-temp-buffer
            (niwa-journal-capture-seal-and-schedule session (current-buffer) 'session-end))
          (should (eq workspace scheduled)))
      (delete-directory root t)))
  :doc "reports a sealing failure as a warning without scheduling or signalling"
  (let* ((root (make-temp-file "niwa-seal-schedule-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace))
         scheduled captured)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-journal-capture-seal)
                   (lambda (&rest _) (error "Frozen evidence is unreadable")))
                  ((symbol-function 'niwa-journal-process-schedule)
                   (lambda (&rest _) (setq scheduled t))))
          (niwa-test--with-captured-diagnostics captured
            (with-temp-buffer
              (niwa-journal-capture-seal-and-schedule session (current-buffer) 'session-end)))
          (should-not scheduled)
          (should (string-match-p "Journal capture sealing failed: Frozen evidence is unreadable" captured)))
      (delete-directory root t))))

(niwa-deftest niwa-journal-capture-list ()
  ,test
  (test)
  :doc "batches retired marker inspection and retains interrupted recovery"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (run (symbol-function 'niwa-session-control-fs-run-program))
            (calls 0))
       (dotimes (n 40)
         (let ((directory (niwa-journal-capture--directory workspace (secure-hash 'sha256 (format "retired-%s" n)))))
           (make-directory directory t)
           (with-temp-file (file-name-concat directory "retired") (insert "published\n"))))
       (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (&rest args) (cl-incf calls) (apply run args))))
         (should (= 1 (length (niwa-journal-capture-list workspace t))))
         (should (< calls 10)))
       ;; A retirement with retained bytes still needs pin-release recovery.
       (niwa-session-control-fs-create-file (niwa-journal-capture--file workspace id "retired") "interrupted")
       (should-not (niwa-journal-capture-list workspace))
       (should (= 1 (length (niwa-journal-capture-list workspace t)))))))

  :doc "reads selected descriptors together, keeping damaged ones visible"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((workspace (niwa-session-workspace session))
            (damaged (secure-hash 'sha256 "damaged"))
            (directory (niwa-journal-capture--directory workspace damaged))
            (run (symbol-function 'niwa-session-control-fs-run-program))
            (calls 0))
       (make-directory directory t)
       (with-temp-file (file-name-concat directory "ready") (insert "ready\n"))
       (with-temp-file (file-name-concat directory "capture.json") (insert "{"))
       (let ((captures
              (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                         (lambda (&rest args) (cl-incf calls) (apply run args))))
                (niwa-journal-capture-list workspace))))
         ;; Listing the captures, their markers, and both descriptors.
         (should (= calls 3))
         (should (= 2 (length captures)))
         (should (plist-get (seq-find (lambda (capture)
                                        (equal damaged (plist-get capture :id)))
                                      captures)
                            :unreadable))
         (should (seq-find (lambda (capture) (plist-get capture :metadata))
                           captures)))))))

(niwa-deftest niwa-journal-capture--markers ()
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((workspace (niwa-session-workspace session))
            (id (plist-get (car (niwa-journal-capture-list workspace)) :id))
            (run (symbol-function 'niwa-session-control-fs-run-program))
            (calls 0))
       (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (&rest args) (cl-incf calls) (apply run args))))
         (should (equal '(t nil nil)
                        (niwa-journal-capture--markers
                         workspace id '("ready" "retired" "discard.json"))))
         (should (= calls 1))
         (should (niwa-journal-capture--pending-p workspace id))
         (should-not (niwa-journal-capture--closed-p workspace id)))
       (niwa-session-control-fs-create-file
        (niwa-journal-capture--file workspace id "discard.json") "{}")
       (should (niwa-journal-capture--closed-p workspace id))
       (niwa-session-control-fs-create-file
        (niwa-journal-capture--file workspace id "retired") "done")
       (should-not (niwa-journal-capture--pending-p workspace id))))))

(provide 'test-niwa-journal-capture)
;;; test-niwa-journal-capture.el ends here
