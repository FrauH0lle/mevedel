;;; test-niwa-session-rewind.el --- Transactional Rewind tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `niwa-session-rewind'.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-chat)
(require 'niwa-execution)
(require 'niwa-notes)
(require 'niwa-session-rewind)
(require 'niwa-session-artifacts)
(require 'niwa-persistence)
(require 'niwa-schedule)
(require 'niwa-session-publication)
(require 'niwa-session-recovery)
(require 'niwa-session-durability)


(defun test-niwa-rewind--publish-backups (session &rest pairs)
  "Publish and commit file-history backups for SESSION.
PAIRS alternate backup name and bytes.  Call after the snapshots referencing
them are set, so the committing save keeps them."
  (let ((niwa-session-artifacts--collecting-critical-artifacts-p t)
        (niwa-session-artifacts--critical-artifacts nil))
    (while pairs
      (niwa-session-artifacts--file-history-write-backup
       (niwa-session-save-path session) (pop pairs) (pop pairs)))
    (niwa-session-publication-publish
     session (nreverse niwa-session-artifacts--critical-artifacts) t))
  (niwa-session-artifacts-save
   session (or (get-buffer "*test-data-buf*") (current-buffer)) nil t))

(niwa-deftest niwa-session-rewind--refresh-restored-buffers ()
  ,test
  (test)
  :doc "reverts visiting buffers after file restore, including confirmed local edits"
  (dolist (modified '(nil t))
    (let* ((tempdir (make-temp-file "niwa-refresh-" t))
           (file (file-name-concat tempdir "source.el"))
           (buf nil))
      (unwind-protect
          (progn
            (write-region "old\n" nil file nil 'silent)
            (setq buf (find-file-noselect file))
            (when modified
              (with-current-buffer buf
                (goto-char (point-max))
                (insert "local\n")))
            (write-region "new\n" nil file nil 'silent)
            (niwa-session-rewind--refresh-restored-buffers
             (list (list :action 'restore :path file))
             (list :succeeded 1))
            (with-current-buffer buf
              (should-not (buffer-modified-p))
              (should (equal "new\n"
                             (buffer-substring-no-properties
                              (point-min) (point-max))))))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-directory tempdir t))))
  :doc "kills visiting buffers for deleted restored files"
  (let* ((tempdir (make-temp-file "niwa-refresh-" t))
         (file (file-name-concat tempdir "source.el"))
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (delete-file file)
          (niwa-session-rewind--refresh-restored-buffers
           (list (list :action 'delete :path file))
           (list :succeeded 1))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t))))


(niwa-deftest niwa-session-rewind--prepare-buffers-for-restore ()
  ,test
  (test)
  :doc "discard reverts affected modified buffers before restore"
  (let* ((tempdir (make-temp-file "niwa-prepare-" t))
         (file (file-name-concat tempdir "source.el"))
         (plan nil)
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (setq plan (list (list :action 'restore :path file)))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "local\n"))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?d)))
            (should (equal plan
                           (niwa-session-rewind--prepare-buffers-for-restore
                            nil 1 plan))))
          (with-current-buffer buf
            (should-not (buffer-modified-p))
            (should (equal "old\n" (buffer-string)))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t)))
  :doc "abort returns abort sentinel when affected buffers are modified"
  (let* ((tempdir (make-temp-file "niwa-prepare-" t))
         (file (file-name-concat tempdir "source.el"))
         (plan nil)
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (setq plan (list (list :action 'restore :path file)))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "local\n"))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?a)))
            (should (eq :abort
                        (niwa-session-rewind--prepare-buffers-for-restore
                         nil 1 plan))))
          (with-current-buffer buf
            (should (buffer-modified-p))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t))))


(niwa-deftest niwa-session-rewind--prompt-candidates ()
  ,test
  (test)
  :doc "returns flat alist with unique display strings across segments"
  (let ((session (niwa-session-create
                  "main" (niwa-workspace-get-or-create
                          'project "x" "/tmp" "x"))))
    (setf (niwa-session-prompt-index session)
          '((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                   :pos 0 :preview "alpha" :fork-point-id "a")
                  (:turn 2 :file-turn 2 :cum-turn 2
                   :pos 100 :preview "beta" :fork-point-id "b")))
            (2 . ((:turn 1 :file-turn 1 :cum-turn 3
                   :pos 0 :preview "alpha" :fork-point-id "c")
                  (:turn 2 :file-turn 2 :cum-turn 4
                   :pos 50 :preview "gamma" :fork-point-id "d")))))
    (let ((candidates
           (niwa-session-rewind--prompt-candidates session)))
      (should (= 4 (length candidates)))
      ;; All display strings unique (segment + turn folded in).
      (should (= 4 (length (cl-delete-duplicates
                            (mapcar #'car candidates) :test #'equal))))
      ;; Newest prompt in the newest segment first.
      (let* ((first (car candidates))
             (plist (cdr first)))
        (should (= 2 (plist-get plist :segment)))
        (should (= 2 (plist-get plist :turn)))))
    (niwa-workspace-clear-registry))
  :doc "preserves raw file turn for compacted segments with copied tail"
  (let ((session (niwa-session-create
                  "main" (niwa-workspace-get-or-create
                          'project "x" "/tmp" "x"))))
    (setf (niwa-session-prompt-index session)
          '((2 . ((:turn 1 :file-turn 3 :cum-turn 11
                   :pos 100 :preview "after tail"
                   :fork-point-id "after-tail")))))
    (let* ((candidate
            (car (niwa-session-rewind--prompt-candidates session)))
           (plist (cdr candidate)))
      (should (= 1 (plist-get plist :turn)))
      (should (= 3 (plist-get plist :file-turn))))
    (niwa-workspace-clear-registry))
  :doc "old directive index records appear as ordinary prompt previews"
  (let ((session (niwa-session--create
                  :prompt-index
                  '((1 . ((:turn 2 :file-turn 2 :cum-turn 2
                           :kind directive :directive-id "abcdef123456"
                           :action request-changes
                           :preview "Old request"
                           :fork-point-id "directive")))))))
    (should (string-match-p
             "S1 T2  Old request"
             (caar (niwa-session-rewind--prompt-candidates
                    session))))))


(niwa-deftest niwa-session-rewind--find-turn-cutoff ()
  ,test
  (test)
  :doc "returns position of next user prompt"
  (with-temp-buffer
    (insert "First prompt\n")                               ; pos 1, turn 1
    (let ((next-prompt-pos
           (progn
             (insert (propertize "Response 1.\n" 'gptel 'response))
             (point))))
      (insert "Second prompt\n")                            ; turn 2 starts here
      (insert (propertize "Response 2.\n" 'gptel 'response))
      ;; Cutoff for turn 1 is the start of turn 2's prompt.
      (should (= next-prompt-pos
                 (niwa-session-rewind--find-turn-cutoff 1)))))
  :doc "returns point-max when turn-n is the last"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Response.\n" 'gptel 'response))
    (insert "Last prompt\n")
    (should (= (point-max)
               (niwa-session-rewind--find-turn-cutoff 2))))
  :doc "skips unpropertized gptel org tool and reasoning scaffolding"
  (with-temp-buffer
    (insert "Fetch a page\n")
    (insert (propertize "Initial answer text.\n" 'gptel 'response))
    (insert "#+begin_reasoning\nThinking text.\n")
    (insert "#+begin_tool (WebFetch :url \"https://example.com\")\n")
    (insert (propertize
             "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\nbody\n"
             'gptel '(tool . "call_1")))
    (insert "#+end_tool\nMore thinking.\n#+end_reasoning\n")
    (let ((next-prompt-pos (point)))
      (insert "Search for docs\n")
      (insert (propertize "Second answer.\n" 'gptel 'response))
      (should (= next-prompt-pos
                 (niwa-session-rewind--find-turn-cutoff 1)))))
  :doc "stays consistent with transcript-repaired assistant fragments"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Initial answer.\n" 'gptel 'response))
    (insert (propertize "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nbody\n"
                        'gptel '(tool . "call_1")))
    (insert "Conti")
    (insert (propertize "nuing the answer.\n" 'gptel 'response))
    (let ((next-prompt-pos (point)))
      (insert "Second prompt\n")
      (insert (propertize "Second answer.\n" 'gptel 'response))
      (let ((prompts (niwa-session-artifacts-collect-prompts
                      (current-buffer))))
        (should (= 2 (length prompts)))
        (should (equal "Second prompt"
                       (plist-get (nth 1 prompts) :preview)))
        (should (= next-prompt-pos
                   (niwa-session-rewind--find-turn-cutoff 1)))))))


(niwa-deftest niwa-session-rewind--staged-file-p ()
  ,test
  (test)
  :doc "checks the remote Git index with target-native command arguments"
  (let* ((host "staged-file-host")
         (root (file-name-as-directory
                (make-temp-file "niwa-staged-file-" t)))
         (local-file (file-name-concat root "staged.el"))
         (remote-file (format "/niwamock:%s:%s" host local-file)))
    (unwind-protect
        (progn
          (test-niwa-session-persistence--git root "init")
          (write-region "staged\n" nil local-file nil 'silent)
          (test-niwa-session-persistence--git root "add" "staged.el")
          (niwa-test--with-local-shell-tramp (list host)
            (should
             (niwa-session-rewind--staged-file-p remote-file)))
          (test-niwa-session-persistence--git
           root "reset" "--" "staged.el")
          (niwa-test--with-local-shell-tramp (list host)
            (should-not
             (niwa-session-rewind--staged-file-p remote-file))))
      (delete-directory root t)))

  :doc "does not expose client environment variables to target Git"
  (let* ((host "staged-file-environment-host")
         (root (file-name-as-directory
                (make-temp-file "niwa-staged-environment-" t)))
         (local-file (file-name-concat root "staged.el"))
         (remote-file (format "/niwamock:%s:%s" host local-file))
         (process-environment
          (cons "NIWA_CLIENT_SECRET=do-not-forward" process-environment)))
    (write-region "staged\n" nil local-file nil 'silent)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (&rest _)
                       (should-not (getenv "NIWA_CLIENT_SECRET"))
                       "/usr/bin/git"))
                    ((symbol-function 'process-file)
                     (lambda (&rest _)
                       (should-not (getenv "NIWA_CLIENT_SECRET"))
                       1)))
            (should
             (niwa-session-rewind--staged-file-p remote-file))))
      (delete-directory root t))))


(niwa-deftest niwa-session-rewind--rewind-impact
  (:doc "lists the complete discarded prompt suffix in chronological order")
  (let* ((session
          (niwa-session--create
           :name "rewind" :turn-count 3
           :prompt-index
           '((1 . ((:turn 1 :cum-turn 1 :preview "Directive one"
                    :fork-point-id "one")
                   (:turn 2 :cum-turn 2 :preview "Ordinary chat"
                    :fork-point-id "two")
                   (:turn 3 :cum-turn 3 :preview "Directive two"
                    :kind directive :directive-id "directive-two"
                    :action discuss :fork-point-id "three"))))))
         (target '(:segment 1 :turn 2 :cum-turn 2 :fork-point-id "two"))
         impact)
    (cl-letf (((symbol-function
                'niwa-session-rewind--detached-child-count)
               (lambda (&rest _) 0))
              ((symbol-function 'display-buffer) #'ignore))
      (setq impact
            (niwa-session-rewind--rewind-impact session target nil))
      (should
       (equal '("Ordinary chat" "Directive two")
              (mapcar (lambda (entry) (plist-get entry :preview))
                      (plist-get impact :discarded-prompts))))
      (niwa-session-rewind--render-rewind-impact session impact)
      (with-current-buffer "*niwa-rewind-impact*"
        (let ((text (buffer-string)))
          (should (string-match-p "Discarded session events" text))
          (should (< (string-match "Ordinary chat" text)
                     (string-match "Directive two" text))))))))


(niwa-deftest niwa-session-rewind-rewind-checkpoint
  (:doc "resumes a cold session and rewinds before the checkpoint turn")
  (let* ((workspace (niwa-workspace--create
                     :type 'file :id "checkpoint" :root "/tmp"
                     :name "checkpoint"))
         (session (niwa-session--create :session-id "cold-session"))
         (buffer (generate-new-buffer " *checkpoint-rewind*"))
         resumed rewound)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--session session))
          (cl-letf
              (((symbol-function 'niwa-session-persistence-resume-id)
                (lambda (owner session-id)
                  (setq resumed (list owner session-id))
                  buffer))
               ((symbol-function
                 'niwa-session-rewind--prompt-candidates)
                (lambda (_)
                  '(("S1 T4" . (:segment 1 :turn 4 :cum-turn 4
                                  :fork-point-id "point")))))
               ((symbol-function 'niwa-session-rewind-rewind)
                (lambda (selected target &optional boundary)
                  (setq rewound (list selected target boundary))
                  t)))
            (should
             (niwa-session-rewind-rewind-checkpoint
              workspace '(:session-id "cold-session" :turn 4))))
          (should (equal (list workspace "cold-session") resumed))
          (should (eq buffer (car rewound)))
          (should (= 4 (plist-get (cadr rewound) :cum-turn)))
          ;; Rewinding before an implementation discards that attempt.
          (should (eq 'before (nth 2 rewound))))
      (kill-buffer buffer))))


(niwa-deftest niwa-rewind (:quiet t)
  (cl-letf (((symbol-function 'read-multiple-choice)
             (lambda (&rest _) '(?f "conversation and files"))))
    ,test)
  (test)
  :doc "errors when no current session"
  (with-temp-buffer
    (let ((niwa--session nil))
      (should-error (niwa-rewind) :type 'user-error)))
  :doc "refuses both pending-input categories before the picker"
  (dolist (category '(steering follow-up))
    (with-temp-buffer
      (let ((session (niwa-session--create :name "rewind"))
            picked)
        (setq-local niwa--session session)
        (niwa-session-enqueue-pending-input
         session category '(:input "keep me"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (setq picked t))))
          (let ((err (should-error (niwa-rewind) :type 'user-error)))
            (should (string-match-p "Pending Inputs"
                                    (error-message-string err)))
            (should (string-match-p "C-c C-q"
                                    (error-message-string err)))
            (should-not picked))))))
  :doc "refuses before the picker while executions remain live"
  (let ((buffer (generate-new-buffer " *execution-rewind*"))
        (session (niwa-session--create :name "rewind")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--session session)
          (cl-letf (((symbol-function 'niwa-execution-session-live-p)
                     (lambda (_session) t)))
            (let ((err (should-error (niwa-rewind) :type 'user-error)))
              (should (string-match-p
                       "/ps or /stop" (error-message-string err))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))))
  :doc "refuses without changing a tree that has an active agent turn"
  (with-temp-buffer
    (let* ((session (niwa-session--create :name "rewind"))
           (record
            (niwa-agent-record--create :activity 'running)))
      (setq-local niwa--session session)
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (let ((err (should-error (niwa-rewind) :type 'user-error)))
        (should (string-match-p
                 "Interrupt active agent turns"
                 (error-message-string err))))
      (should (eq record
                  (cdr (assoc
                        "/root/worker"
                        (niwa-session-agent-registry session)))))))
  :doc "errors when request in flight"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (setq-local niwa--session session)
                (let ((niwa--current-request 'placeholder))
                  (should-error (niwa-rewind) :type 'user-error)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "errors when no recorded prompts"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (setq-local niwa--session session)
                (let ((niwa--current-request nil))
                  (should-error (niwa-rewind) :type 'user-error)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "selecting the latest turn still confirms discarding that turn"
  (let* ((session
          (niwa-session--create
           :name "rewind" :turn-count 1
           :prompt-index
           '((1 . ((:turn 1 :cum-turn 1 :fork-point-id "point"))))))
         (buffer (generate-new-buffer " *empty-rewind*"))
         (target '(:segment 1 :turn 1 :cum-turn 1
                   :fork-point-id "point"))
         confirmed committed started)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--session session)
          (cl-letf
              (((symbol-function
                 'niwa-session-rewind-assert-stable-source)
                #'ignore)
               ((symbol-function
                 'niwa-session-rewind-restore-plan)
                (lambda (&rest _) nil))
               ((symbol-function
                 'niwa-session-rewind--detached-child-count)
                (lambda (&rest _) 0))
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _) (setq confirmed t)))
               ((symbol-function
                 'niwa-session-rewind--commit-rewind)
                (lambda (&rest _) (setq committed t)))
               ((symbol-function 'niwa--run-session-start-hooks)
                (lambda (&rest _) (setq started t))))
            (niwa-session-rewind-rewind buffer target))
          (should confirmed)
          (should committed)
          (should started))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "rechecks remote mutation authority after impact confirmation"
  (let* ((target '(:segment 1 :turn 1 :cum-turn 1
                   :fork-point-id "point"))
         (session
          (niwa-session--create
           :name "rewind" :turn-count 1
           :prompt-index `((1 . (,target)))))
         (buffer (generate-new-buffer " *remote-authority-rewind*"))
         (authority-checks 0)
         confirmed)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--session session)
          (cl-letf
              (((symbol-function
                 'niwa-session-rewind-assert-stable-source)
                #'ignore)
               ((symbol-function
                 'niwa-session-rewind--prompt-candidates)
                (lambda (_) (list (cons "target" target))))
               ((symbol-function 'completing-read)
                (lambda (&rest _) "target"))
               ((symbol-function
                 'niwa-session-artifacts-assert-mutation-authority)
                (lambda (&rest _)
                  (cl-incf authority-checks)
                  (when confirmed
                    (user-error "Injected lost remote lease"))))
               ((symbol-function
                 'niwa-session-rewind-restore-plan)
                (lambda (&rest _) nil))
               ((symbol-function
                 'niwa-session-rewind--detached-child-count)
                (lambda (&rest _) 0))
               ((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _)
                  (setq confirmed t)))
               ((symbol-function
                 'niwa-session-rewind--commit-rewind)
                (lambda (&rest _)
                  (ert-fail "Rewind committed without mutation authority"))))
            (should-error (niwa-rewind) :type 'user-error))
          (should (= 2 authority-checks)))
      (when-let* ((impact (get-buffer "*niwa-rewind-impact*")))
        (kill-buffer impact))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "rewinds first-turn modifications, creations, and deletions to pre-turn state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buffer (generate-new-buffer "*test-first-turn-rewind*"))
               (modified (file-name-concat tempdir "modified.el"))
               (created (file-name-concat tempdir "created.el"))
               (deleted (file-name-concat tempdir "deleted.el")))
          (unwind-protect
              (with-current-buffer buffer
                (org-mode)
                (setq-local niwa--session session)
                (write-region "before-modify" nil modified nil 'silent)
                (write-region "before-delete" nil deleted nil 'silent)
                (niwa-request-begin session)
                (let ((checkpoint
                       (niwa-request-file-snapshots
                        niwa--current-request)))
                  (puthash modified "before-modify" checkpoint)
                  (puthash created nil checkpoint)
                  (puthash deleted "before-delete" checkpoint))
                (write-region "after-modify" nil modified nil 'silent)
                (write-region "after-create" nil created nil 'silent)
                (delete-file deleted)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (niwa-session-turn-count session) 1)
                (niwa-session-artifacts-save session buffer t)
                (let ((target
                       (copy-sequence
                        (cdar
                         (niwa-session-rewind--prompt-candidates
                          session)))))
                  (niwa-request-end)
                  (cl-letf (((symbol-function 'display-buffer) #'ignore)
                            ((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) t))
                            ((symbol-function
                              'niwa--run-session-start-hooks)
                             #'ignore))
                    (should
                     (niwa-session-rewind-rewind buffer target))))
                (should (= 0 (niwa-session-turn-count session)))
                (should-not (string-match-p "First prompt" (buffer-string)))
                (should
                 (equal "before-modify"
                        (niwa-session-artifacts--file-text modified)))
                (should-not (file-exists-p created))
                (should
                 (equal "before-delete"
                        (niwa-session-artifacts--file-text deleted))))
            (test-niwa-session-persistence--release-and-kill
             buffer session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "a rolled-back Rewind keeps another session's note activity and deletions"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer "*test-rollback-other-session*"))
           (source-file (file-name-concat tempdir "source.el"))
           (change '(:id "other-2" :session-id "other-session" :turn 2))
           (restore (symbol-function 'niwa-session-rewind-execute-restore))
           (stored (lambda (uuid key)
                     (catch 'found
                       (dolist (file (plist-get (plist-get (niwa-instruction-store--read
                                                            workspace)
                                                           :data)
                                                :files))
                         (dolist (instruction (plist-get (cdr file) :instructions))
                           (let ((properties (plist-get instruction :properties)))
                             (when (equal uuid (plist-get properties 'niwa-uuid))
                               (throw 'found (plist-get properties key)))))))))
           source notes theirs doomed)
      (unwind-protect
          (progn
            (test-niwa-session-persistence--reset-instructions)
            (write-region "AAAAA\nBBBBB\nCCCCC\n" nil source-file nil 'silent)
            (setq source (find-file-noselect source-file))
            (with-current-buffer source
              (setq-local niwa--workspace workspace)
              (dolist (line '(1 2 3))
                (goto-char (point-min))
                (forward-line (1- line))
                (push (niwa-note-create (format "line %d" line)) notes)))
            (setq theirs (overlay-get (nth 1 notes) 'niwa-uuid)
                  doomed (overlay-get (nth 0 notes) 'niwa-uuid))
            (dolist (note (list (nth 1 notes) (nth 0 notes)))
              (niwa-note-mark-sent (list note) "other-session"))
            (niwa-instruction-store-sync workspace)
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--workspace workspace)
              (setq-local niwa--session session)
              (niwa-request-begin session)
              (insert "First prompt\n")
              (insert (propertize "First reply.\n" 'gptel 'response))
              (setf (niwa-session-turn-count session) 1)
              (niwa-session-artifacts-save session buffer t)
              (let ((target (copy-sequence
                             (cdar (niwa-session-rewind--prompt-candidates
                                    session)))))
                (niwa-request-end)
                (cl-letf (((symbol-function 'display-buffer) #'ignore)
                          ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                          ((symbol-function 'niwa--run-session-start-hooks)
                           #'ignore)
                          ((symbol-function 'niwa-session-rewind-execute-restore)
                           (lambda (&rest args)
                             (prog1 (apply restore args)
                               ;; Another session prepares a change and deletes
                               ;; a note meanwhile; then this Rewind fails and
                               ;; rolls back.
                               (let ((note (niwa--instruction-with-uuid theirs workspace)))
                                 (overlay-put note 'niwa-note-prepared-change change)
                                 (niwa-instruction-store-touch workspace))
                               (niwa-note-delete
                                (niwa--instruction-with-uuid doomed workspace))
                               (error "Injected Rewind failure")))))
                  (should-error (niwa-session-rewind-rewind buffer target)))))
            (should (= 1 (niwa-session-turn-count session)))
            (should (equal change
                           (overlay-get (niwa--instruction-with-uuid theirs workspace)
                                        'niwa-note-prepared-change)))
            (should-not (niwa--instruction-with-uuid doomed workspace))
            ;; The held-back writes reach the store once the Rewind is over.
            (niwa-instruction-store-flush)
            (should (equal change (funcall stored theirs 'niwa-note-prepared-change)))
            (should (eq 'closed (funcall stored doomed 'niwa-note-state))))
        (when-let* ((impact (get-buffer "*niwa-rewind-impact*")))
          (kill-buffer impact))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (test-niwa-session-persistence--reset-instructions)
        (niwa-workspace-clear-registry))))
  :doc "keeps the selected turn and discards only what follows"
  ;; The view's Rewind names a response the user is looking at, so that
  ;; turn is the last one kept and only later turns go.
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (data-buf (generate-new-buffer " *test-keep-turn-data*")))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (org-mode)
              (setq-local niwa--session session)
              (setq-local niwa--workspace workspace)
              (setq-local gptel-backend
                          (test-niwa-session-persistence--agent-backend))
              (setq-local gptel-model 'test-model)
              (gptel-mode 1)
              (niwa-session-artifacts-save session data-buf)
              (goto-char (point-max))
              (insert "First prompt\n")
              (let ((start (point)))
                (insert "First reply.\n")
                (put-text-property start (point) 'gptel 'response))
              (setf (niwa-session-turn-count session) 1)
              (niwa-session-artifacts-save session data-buf t)
              (goto-char (point-max))
              (insert "Second prompt\n")
              (let ((start (point)))
                (insert "Second reply.\n")
                (put-text-property start (point) 'gptel 'response))
              (setf (niwa-session-turn-count session) 2)
              (niwa-session-artifacts-save session data-buf t))
            (let* ((target
                    (copy-sequence
                     (cdr (cl-find-if
                           (lambda (entry)
                             (= 1 (plist-get (cdr entry) :cum-turn)))
                           (niwa-session-rewind--prompt-candidates
                            session))))))
              (should target)
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'niwa--run-session-start-hooks)
                         #'ignore))
                (should (niwa-session-rewind-rewind
                         data-buf target 'after)))
              ;; Turn 1 survives with its reply; turn 2 is gone.
              (should (= 1 (niwa-session-turn-count session)))
              (with-current-buffer data-buf
                (should (string-match-p "First prompt" (buffer-string)))
                (should (string-match-p "First reply" (buffer-string)))
                (should-not
                 (string-match-p "Second prompt" (buffer-string)))
                (should-not
                 (string-match-p "Second reply" (buffer-string))))))
        (test-niwa-session-persistence--release-and-kill
         data-buf session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))

  :doc "rewinds before a saved first gptel turn without exposing metadata"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (data-buf (generate-new-buffer " *test-gptel-rewind-data*"))
           (view-buf (generate-new-buffer " *test-gptel-rewind-view*"))
           (draft "> keep this draft\nsecond line"))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (org-mode)
              (setq-local niwa--session session)
              (setq-local niwa--workspace workspace)
              (setq-local gptel-backend
                          (test-niwa-session-persistence--agent-backend))
              (setq-local gptel-model 'test-model)
              (gptel-mode 1)
              (niwa-session-artifacts-save session data-buf)
              (goto-char (point-max))
              (insert "Implement hello world\n")
              (dotimes (index 40)
                (let ((start (point)))
                  (insert (format "model span %02d\n" index))
                  (put-text-property
                   start (point) 'gptel
                   (if (= (% index 2) 0) 'response 'ignore))))
              (setf (niwa-session-turn-count session) 1)
              (niwa-session-artifacts-save session data-buf t))
            (niwa-view--setup view-buf data-buf)
            (with-current-buffer view-buf
              (niwa-view--full-rerender)
              (niwa-view-test--insert-composer-draft draft 4))
            (let ((target
                   (copy-sequence
                    (cdar
                     (niwa-session-rewind--prompt-candidates
                      session)))))
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'niwa--run-session-start-hooks)
                         #'ignore))
                (should
                 (niwa-session-rewind-rewind data-buf target))))
            (should (= 0 (niwa-session-turn-count session)))
            (should-not
             (cdr (assoc 1 (niwa-session-prompt-index session))))
            ;; The sidecar owns request config; the rewound transcript
            ;; must carry no gptel config properties.  Whether an empty
            ;; drawer or GPTEL_BOUNDS remains after the cutoff is not
            ;; part of the contract -- the next save recomputes both.
            (with-current-buffer data-buf
              (should-not
               (string-match-p
                ":GPTEL_\\(?:BACKEND\\|MODEL\\|PRESET\\|SYSTEM\\|TOOLS\\):"
                (buffer-string))))
            (let ((sidecar
                   (niwa-session-codec-read
                    (niwa-session-artifacts-sidecar-path
                     (niwa-session-save-path session)))))
              (should-not (cdr (assoc 1 (plist-get sidecar :prompt-index)))))
            ;; The rewind schedules the view's rebuild.
            (niwa-view--flush-scheduled-render view-buf t)
            (with-current-buffer view-buf
              (should (equal draft (niwa-view-composer-draft-text)))
              (should-not (string-match-p ":PROPERTIES:" (buffer-string)))
              (should-not (string-match-p "^You$" (buffer-string))))
            (should-not (get-buffer "*niwa-rewind-impact*")))
        (when-let* ((impact (get-buffer "*niwa-rewind-impact*")))
          (kill-buffer impact))
        (when (buffer-live-p view-buf)
          (kill-buffer view-buf))
        (test-niwa-session-persistence--release-and-kill
         data-buf session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))
  :doc "leaves no transcript residue when rewinding before the first turn"
  ;; The transcript's leading newlines carry the discarded turns'
  ;; properties, and the folded property drawer starts at point-min.  Org
  ;; walks one character back from a folded region on every change, so a
  ;; wholesale buffer replacement there used to signal `beginning-of-buffer'
  ;; and abort the Rewind; what survived it projected a turn with no
  ;; content and hid the retained lines behind an invisibility ellipsis.
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (data-buf (generate-new-buffer " *test-rewind-residue-data*"))
           (view-buf (generate-new-buffer " *test-rewind-residue-view*")))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (org-mode)
              (setq-local niwa--session session)
              (setq-local niwa--workspace workspace)
              (setq-local gptel-backend
                          (test-niwa-session-persistence--agent-backend))
              (setq-local gptel-model 'test-model)
              (gptel-mode 1)
              (niwa-session-artifacts-save session data-buf)
              (goto-char (point-max))
              (let ((start (point)))
                (insert "\n")
                (put-text-property start (point) 'gptel
                                   (cons 'tool "call-residue")))
              (let ((start (point)))
                (insert "\n")
                (put-text-property start (point) 'gptel 'niwa-hook-audit)
                (put-text-property start (point) 'niwa-hook-audit t)
                (put-text-property start (point) 'invisible t))
              (insert "First prompt\n")
              (let ((start (point)))
                (insert "assistant text\n")
                (put-text-property start (point) 'gptel 'response))
              (setf (niwa-session-turn-count session) 1)
              (niwa-session-artifacts-save session data-buf t))
            (niwa-view--setup view-buf data-buf)
            (with-current-buffer view-buf
              (niwa-view--full-rerender))
            (let ((target
                   (copy-sequence
                    (cdar (niwa-session-rewind--prompt-candidates
                           session)))))
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'niwa--run-session-start-hooks)
                         #'ignore))
                (should (niwa-session-rewind-rewind data-buf target))))
            (with-current-buffer data-buf
              (should (string-blank-p
                       (buffer-substring-no-properties
                        (point-min) (point-max))))
              (should-not (buffer-modified-p))
              (should-not
               (niwa-transcript-restore-properties-present-p
                (point-min) (point-max)))
              (should-not (text-property-not-all (point-min) (point-max)
                                                 'invisible nil))
              (should-not
               (niwa-view--group-into-turns
                (cl-remove-if-not
                 (lambda (segment) (memq (car segment) '(ignored tool)))
                 (niwa-transcript-segments (point-min) (point-max)))
                data-buf)))
            (niwa-view--flush-scheduled-render view-buf t)
            (with-current-buffer view-buf
              (should-not (string-match-p "^Assistant$" (buffer-string)))))
        (when-let* ((impact (get-buffer "*niwa-rewind-impact*")))
          (kill-buffer impact))
        (when (buffer-live-p view-buf)
          (kill-buffer view-buf))
        (test-niwa-session-persistence--release-and-kill data-buf session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))

  :doc "rewinds a later turn while preserving the preceding turn"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buffer (generate-new-buffer "*test-later-turn-rewind*"))
               (path (file-name-concat tempdir "serial.el")))
          (unwind-protect
              (with-current-buffer buffer
                (org-mode)
                (setq-local niwa--session session)
                (write-region "zero" nil path nil 'silent)
                (niwa-request-begin session)
                (puthash path "zero"
                         (niwa-request-file-snapshots
                          niwa--current-request))
                (write-region "one" nil path nil 'silent)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (niwa-session-turn-count session) 1)
                (niwa-session-artifacts-save session buffer t)
                (niwa-request-end)
                (niwa-request-begin session)
                (puthash path "one"
                         (niwa-request-file-snapshots
                          niwa--current-request))
                (write-region "two" nil path nil 'silent)
                (insert "Second prompt\n")
                (insert (propertize "Second reply.\n" 'gptel 'response))
                (setf (niwa-session-turn-count session) 2)
                (niwa-session-artifacts-save session buffer t)
                (let* ((candidates
                        (niwa-session-rewind--prompt-candidates
                         session))
                       (target (copy-sequence (cdar (last candidates)))))
                  (niwa-request-end)
                  (cl-letf (((symbol-function 'display-buffer) #'ignore)
                            ((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) t))
                            ((symbol-function
                              'niwa--run-session-start-hooks)
                             #'ignore))
                    (should
                     (niwa-session-rewind-rewind buffer target))))
                (should (= 1 (niwa-session-turn-count session)))
                (should (string-match-p "First reply" (buffer-string)))
                (should-not (string-match-p "Second prompt"
                                            (buffer-string)))
                (should
                 (equal "one"
                        (niwa-session-artifacts--file-text path))))
            (test-niwa-session-persistence--release-and-kill
             buffer session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "a Rewind keeps the current pending wake-up armed"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer "*test-rewind-wake-up*"))
           (niwa-user-dir (file-name-as-directory (file-name-concat tempdir "user")))
           (now 1795176000)
           (niwa-schedule-clock (lambda () now)))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session)
            (setf (niwa-session-root-buffer session) buffer)
            (niwa-request-begin session)
            (insert "First prompt\n")
            (insert (propertize "First reply.\n" 'gptel 'response))
            (setf (niwa-session-turn-count session) 1)
            (niwa-session-artifacts-save session buffer t)
            (niwa-request-end)
            (niwa-request-begin session)
            (insert "Second prompt\n")
            (insert (propertize "Second reply.\n" 'gptel 'response))
            (niwa-schedule-create session buffer "20m" "Check CI")
            (setf (niwa-session-turn-count session) 2)
            (niwa-session-artifacts-save session buffer t)
            (let ((target (copy-sequence
                           (cdar (last (niwa-session-rewind--prompt-candidates
                                        session))))))
              (niwa-request-end)
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'niwa--run-session-start-hooks) #'ignore))
                (should (niwa-session-rewind-rewind buffer target))))
            (should (= 1 (niwa-session-turn-count session)))
            (should (= 1 (length (niwa-schedule-pending session))))
            (should (memq buffer niwa-schedule--buffers))
            (should niwa-schedule--timer)
            (setq now (+ now 1200))
            (niwa-schedule-run-due)
            (should-not (niwa-schedule-pending session)))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (should-not niwa-schedule--timer)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))
  :doc "keeps Rewind reachable and discloses known checkpoint gaps"
  (let* ((session
          (niwa-session--create
           :name "rewind" :turn-count 1
           :prompt-index
           '((1 . ((:turn 1 :cum-turn 1 :fork-point-id "point"))))
           :file-snapshots
           '((1 . (("/unreadable" . (:gap "capture failed"
                                      :version 1)))))))
         (buffer (generate-new-buffer " *gap-rewind*"))
         (target '(:segment 1 :turn 1 :cum-turn 1
                   :fork-point-id "point"))
         confirmed impact-text)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--session session)
          (cl-letf
              (((symbol-function
                 'niwa-session-rewind-assert-stable-source)
                #'ignore)
               ((symbol-function
                 'niwa-session-rewind--detached-child-count)
                (lambda (&rest _) 0))
               ((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _)
                  (setq confirmed t
                        impact-text
                        (with-current-buffer "*niwa-rewind-impact*"
                          (buffer-string)))
                  nil)))
            (niwa-session-rewind-rewind buffer target))
          (should confirmed)
          (should (string-match-p "Checkpoint coverage: incomplete"
                                  impact-text))
          (should (string-match-p "/unreadable" impact-text))
          (should (string-match-p "capture failed" impact-text))
          (should-not (get-buffer "*niwa-rewind-impact*")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "main-chat coverage groups repeated tools and leaves real file gaps visible"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *grouped-coverage*"))
           (file (file-name-concat tempdir "source.txt"))
           impact)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session niwa--workspace workspace)
            (write-region "unchanged\n" nil file nil 'silent)
            (dotimes (index 30)
              (let* ((turn (1+ index))
                     (effects (append '(("Bash" . "shell changes are not captured"))
                                      (when (memq turn '(5 25))
                                        '(("Eval" . "evaluation effects are not captured"))))))
                (niwa-request-begin session)
                (setf (niwa-request-untracked-effects niwa--current-request) effects)
                (when (= turn 15)
                  (puthash file '(:gap "capture failed")
                           (niwa-request-file-snapshots niwa--current-request)))
                (goto-char (point-max))
                (let ((start (point)))
                  (insert (format "Prompt %d\n" turn))
                  (put-text-property start (point) 'gptel nil))
                (insert (propertize "Reply.\n" 'gptel 'response))
                (setf (niwa-session-turn-count session) turn)
                (niwa-session-artifacts-save session buffer t)
                (niwa-request-end)))
            (cl-letf (((symbol-function 'display-buffer) #'ignore)
                      ((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _)
                         (or (seq-find (lambda (choice) (string-match-p "T1 " choice))
                                       (all-completions "" choices))
                             (ert-fail (format "First turn missing: %S" (all-completions "" choices))))))
                      ((symbol-function 'yes-or-no-p)
                       (lambda (&rest _)
                         (setq impact (with-current-buffer "*niwa-rewind-impact*" (buffer-string)))
                         nil)))
              (niwa-rewind))
            (should (string-search "incomplete (3 known gaps)" impact))
            (should (string-match-p "gap +Bash (30 turns: shell changes are not captured)" impact))
            (should (string-match-p "gap +Eval (2 turns: evaluation effects are not captured)" impact))
            (should (string-search (concat file " (capture failed)") impact))
            (with-temp-buffer
              (insert impact)
              (should (= 1 (how-many "^gap +Bash " (point-min) (point-max))))
              (should (= 1 (how-many "^gap +Eval " (point-min) (point-max)))))
            (should (= 30 (niwa-session-turn-count session)))
            (should (equal "unchanged\n" (niwa-test--read-file file))))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))
  :doc "retains the impact buffer when the commit fails"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *failed-rewind*"))
           (missing (file-name-concat tempdir "missing.el")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session)
            (insert "First prompt\n")
            (insert (propertize "First reply.\n" 'gptel 'response))
            (setf (niwa-session-turn-count session) 1)
            (niwa-session-artifacts-save session buffer t)
            (let ((target
                   (copy-sequence
                    (cdar
                     (niwa-session-rewind--prompt-candidates
                      session)))))
              (setf (niwa-session-file-snapshots session)
                    `((1 . ((,missing . (:backup-name nil
                                         :pre-backup-name "missing"
                                         :version 1))))))
              (test-niwa-rewind--publish-backups session "missing" "original")
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _)
                           ;; Lose the committed pre-image behind the plan.
                           (delete-file
                            (plist-get
                             (cdr (assoc "file-history/missing"
                                         (plist-get
                                          (niwa-session-publication session)
                                          :artifacts)))
                             :published))
                           t)))
                (should-error
                 (niwa-session-rewind-rewind buffer target)))
              (should (get-buffer "*niwa-rewind-impact*"))
              (should (= 1 (niwa-session-turn-count session)))
              (should (string-match-p "First prompt" (buffer-string)))))
        (when-let* ((impact (get-buffer "*niwa-rewind-impact*")))
          (kill-buffer impact))
        (test-niwa-session-persistence--release-and-kill
         buffer session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))
  :doc "restores a deleted source and holds back concurrent instruction-store writes"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let ((session nil)
          (buffer nil)
          (source-buffer nil)
          (source-file (file-name-concat tempdir "source.el"))
          (stored-notes
           (lambda (owner)
             (sort (cl-loop
                    for file in (plist-get
                                 (plist-get (niwa-instruction-store--read owner)
                                            :data)
                                 :files)
                    append (cl-loop
                            for instruction in (plist-get (cdr file) :instructions)
                            collect (plist-get (plist-get instruction :properties)
                                               'niwa-note-text)))
                   #'string<))))
      (unwind-protect
          (progn
            (test-niwa-session-persistence--reset-instructions)
            (write-region "EARLY\nLATER\n" nil source-file nil 'silent)
            (setq source-buffer (find-file-noselect source-file))
            (with-current-buffer source-buffer
              (setq-local niwa--workspace workspace)
              (goto-char (point-min))
              (niwa-note-create "Early"))
            (setq session (niwa-session-create "main" workspace)
                  buffer (generate-new-buffer "*test-note-rewind*"))
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--workspace workspace)
              (setq-local niwa--session session)
              (insert "First note turn\n")
              (insert (propertize "First reply.\n" 'gptel 'response))
              (setf (niwa-session-turn-count session) 1)
              (niwa-session-artifacts-save session buffer t))
            (let* ((notes (funcall stored-notes workspace))
                   target)
              (should (equal '("Early") notes))
              (with-current-buffer buffer
                (niwa-request-begin session)
                (puthash source-file "EARLY\nLATER\n"
                         (niwa-request-file-snapshots
                          niwa--current-request))
                (delete-file source-file)
                (goto-char (point-max))
                (insert "Second note turn\n")
                (insert (propertize "Second reply.\n" 'gptel 'response))
                (setf (niwa-session-turn-count session) 2)
                (niwa-session-artifacts-save session buffer t)
                (setq target
                      (copy-sequence
                       (cdr
                        (cl-find-if
                         (lambda (entry)
                           (= 2 (plist-get (cdr entry) :cum-turn)))
                         (niwa-session-rewind--prompt-candidates
                          session)))))
                (niwa-request-end)
                (goto-char (point-max))
                (insert "Ordinary later chat\n")
                (insert (propertize "Later reply.\n" 'gptel 'response))
                (setf (niwa-session-turn-count session) 3)
                (niwa-session-artifacts-save session buffer t))
              (when (buffer-live-p source-buffer)
                (kill-buffer source-buffer)
                (setq source-buffer nil))
              ;; Meanwhile another workspace and another session of this
              ;; workspace each write a note; this session's Rewind must
              ;; keep neither from its store.
              (let* ((other-root (file-name-as-directory
                                  (make-temp-file "niwa-other-ws-" t)))
                     (other (niwa-workspace-get-or-create
                             'file "other-ws" other-root "other-ws"))
                     (other-file (file-name-concat other-root "other.el"))
                     (same-file (file-name-concat tempdir "same.el"))
                     (restore (symbol-function
                               'niwa-session-rewind-execute-restore))
                     other-buffer same-buffer held-back)
                (niwa-workspace-identity-ensure other-root)
                (write-region "(defun other () t)\n" nil other-file nil 'silent)
                (write-region "(defun same () t)\n" nil same-file nil 'silent)
                (unwind-protect
                    (progn
                      (cl-letf (((symbol-function 'display-buffer) #'ignore)
                                ((symbol-function 'yes-or-no-p)
                                 (lambda (&rest _) t))
                                ((symbol-function 'niwa--run-session-start-hooks)
                                 #'ignore)
                                ((symbol-function
                                  'niwa-session-rewind-execute-restore)
                                 (lambda (&rest args)
                                   (prog1 (apply restore args)
                                     (setq other-buffer (find-file-noselect other-file))
                                     (with-current-buffer other-buffer
                                       (setq-local niwa--workspace other)
                                       (goto-char (point-min))
                                       (niwa-note-create "elsewhere"))
                                     ;; Another session of this workspace
                                     ;; writes a note and syncs the store.
                                     (setq same-buffer (find-file-noselect same-file))
                                     (with-current-buffer same-buffer
                                       (setq-local niwa--workspace workspace)
                                       (goto-char (point-min))
                                       (niwa-note-create "same workspace"))
                                     (niwa-instruction-store-sync workspace)
                                     ;; Control: the Rewind really held it back.
                                     (setq held-back
                                           (not (member "same workspace"
                                                        (funcall stored-notes
                                                                 workspace))))))))
                        (should
                         (niwa-session-rewind-rewind buffer target)))
                      (should other-buffer)
                      (should held-back)
                      (should (memq workspace niwa-instruction-store--dirty))
                      (niwa-instruction-store-sync other)
                      (should
                       (equal '("elsewhere")
                              (funcall stored-notes other))))
                  (when (buffer-live-p other-buffer)
                    (with-current-buffer other-buffer (set-buffer-modified-p nil))
                    (kill-buffer other-buffer))
                  (niwa--clear-instruction-state other)
                  (delete-directory other-root t)))
              (should (= 1 (niwa-session-turn-count session)))
              (with-current-buffer buffer
                (should (string-match-p "First reply" (buffer-string)))
                (should-not (string-match-p "Second note turn"
                                            (buffer-string)))
                (should-not (string-match-p "Ordinary later chat"
                                            (buffer-string))))
              (should (file-exists-p source-file))
              ;; The Rewind neither drops nor duplicates this workspace's note.
              (should (equal notes (funcall stored-notes workspace)))
              ;; The workspace instruction store's Rewind intent is settled.
              (should-not
               (plist-get (plist-get (niwa-instruction-store--read workspace)
                                     :data)
                          :rewinds))
              ;; The other session's activity, held back during the
              ;; Rewind, reaches the store once the Rewind is over.
              (niwa-instruction-store-flush)
              (should (equal '("Early" "same workspace")
                             (funcall stored-notes workspace)))
              (when-let* ((same-buffer (find-buffer-visiting
                                        (file-name-concat tempdir "same.el"))))
                (with-current-buffer same-buffer (set-buffer-modified-p nil))
                (kill-buffer same-buffer))))
        (when (and buffer (buffer-live-p buffer))
          (test-niwa-session-persistence--release-and-kill buffer session))
        (setq source-buffer (or source-buffer
                                (find-buffer-visiting source-file)))
        (when (buffer-live-p source-buffer)
          (with-current-buffer source-buffer (set-buffer-modified-p nil))
          (kill-buffer source-buffer))
        (delete-directory tempdir t)
        (test-niwa-session-persistence--reset-instructions)
        (niwa-workspace-clear-registry))))
  :doc "cancelling the impact confirmation changes no transcript or file state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf (generate-new-buffer "*test-rewind-cancel*"))
               (path (file-name-concat tempdir "tracked.el"))
               target)
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local niwa--session session)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (niwa-session-turn-count session) 1)
                (niwa-session-artifacts-save session buf t)
                (setq target
                      (copy-sequence
                       (cdar
                         (niwa-session-rewind--prompt-candidates
                         session))))
                (let ((backup
                       (niwa-session-artifacts--file-history-backup-name "first")))
                  (setf (niwa-session-file-snapshots session)
                        `((1 . ((,path . (:backup-name ,backup
                                        :pre-backup-name ,backup
                                        :version 1))))))
                  (test-niwa-rewind--publish-backups
                   session backup "first"))
                (goto-char (point-max))
                (insert "Second prompt\n")
                (insert (propertize "Second reply.\n" 'gptel 'response))
                (setf (niwa-session-turn-count session) 2)
                (niwa-session-artifacts-save session buf t)
                (let ((backup
                       (niwa-session-artifacts--file-history-backup-name "second")))
                  (setf (niwa-session-file-snapshots session)
                        (append
                         (niwa-session-file-snapshots session)
                         `((2 . ((,path . (:backup-name ,backup
                                         :pre-backup-name
                                         ,(niwa-session-artifacts--file-history-backup-name
                                           "first")
                                         :version 2)))))))
                  (test-niwa-rewind--publish-backups
                   session backup "second"))
                (write-region "second" nil path nil 'silent)
                (let* ((before-buffer (buffer-string))
                       (session-id (niwa-session-session-id session))
                       (save-path (niwa-session-save-path session))
                       starts ended
                       (before-sidecar
                        (niwa-session-artifacts-read-artifact
                         session "session.meta.el" t)))
                  (cl-letf (((symbol-function 'display-buffer) #'ignore)
                            ((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) nil))
                            ((symbol-function
                              'niwa--run-session-start-hooks)
                             (lambda (source)
                               (push source starts))))
                    (niwa-session-rewind-rewind buf target))
                  (should-not starts)
                  (should-not (get-buffer "*niwa-rewind-impact*"))
                  (should (equal before-buffer (buffer-string)))
                  (should
                   (equal
                    before-sidecar
                    (niwa-session-artifacts-read-artifact
                     session "session.meta.el" t)))
                  (should
                   (equal "second"
                          (with-temp-buffer
                            (insert-file-contents path)
                            (buffer-string))))
                  (should (= 2 (niwa-session-turn-count session)))
                  (should (equal buffer-file-name
                                 (niwa-session-artifacts-segment-path
                                  (niwa-session-save-path session) 1)))
                  (let (confirmation)
                    (cl-letf (((symbol-function 'display-buffer) #'ignore)
                              ((symbol-function 'yes-or-no-p)
                               (lambda (prompt)
                                 (setq confirmation prompt)
                                 t))
                              ((symbol-function
                                'niwa--run-session-start-hooks)
                               (lambda (source)
                                 (push source starts)
                                 (setf
                                  (niwa-session-hook-context-pending session)
                                  `((:event SessionStart
                                     :body ,source)))))
                              ((symbol-function
                                'niwa--run-session-end-hooks)
                               (lambda ()
                                 (setq ended t))))
                      (niwa-session-rewind-rewind buf target))
                    ;; Lease-owned sessions keep both redo layers.
                    (should (string-match-p "conversation and captured-file redo"
                                            confirmation)))
                  (should (equal '("rewind") starts))
                  (should-not ended)
                  (let* ((context
                          (niwa-session-hook-context-pending session))
                         (submission
                          (niwa-prompt-submission-create
                           :session session :context-entries context)))
                    (niwa-prompt-submission-commit submission)
                    (should-not
                     (niwa-session-hook-context-pending session)))
                  (should (equal session-id
                                 (niwa-session-session-id session)))
                  (should (equal save-path
                                 (niwa-session-save-path session)))
                  (should (= 0 (niwa-session-turn-count session)))
                  (should-not (string-match-p "First prompt"
                                              (buffer-string)))
                  (should-not (string-match-p "Second prompt"
                                              (buffer-string)))
                  (should
                   (equal "first"
                          (with-temp-buffer
                            (insert-file-contents path)
                            (buffer-string))))
                  (let ((sidecar
                         (car (read-from-string
                               (niwa-session-artifacts-read-artifact
                                session "session.meta.el" t)))))
                    (should (= 0 (plist-get sidecar :total-turn-count)))
                    (should-not (plist-get sidecar
                                           :forked-from-session-id)))))
            (test-niwa-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-rewind-materialize-publication ()
  ,test
  (test)
  :doc "writes committed artifacts into a staging root that does not exist yet"
  (let* ((tempdir (make-temp-file "niwa-materialize-" t))
         (published (file-name-concat tempdir "published-0001"))
         (staging (file-name-concat tempdir "temporary" "staging"))
         (logical "tool-results/ToolCall-aA1b2c.txt")
         (content "returned value\n")
         (publication
          (list :head ".publications/0001/manifest.el"
                :artifacts
                (list (cons logical
                            (list :published published
                                  :sha256 (secure-hash 'sha256 content))))))
         (session (niwa-session--create
                   :authority-mode 'portable
                   :save-path (file-name-as-directory tempdir)
                   :publication publication)))
    (unwind-protect
        (progn
          (write-region content nil published nil 'silent)
          ;; Rewind hands over `<temporary-root>/staging', which nothing
          ;; creates before materialization.
          (should-not (file-directory-p staging))
          (niwa-session-rewind-materialize-publication
           session publication staging)
          (should (equal content
                         (with-temp-buffer
                           (insert-file-contents
                            (file-name-concat staging logical))
                           (buffer-string)))))
      (delete-directory tempdir t)))
  :doc "refuses an artifact whose logical path leaves the staging root"
  (let* ((tempdir (make-temp-file "niwa-materialize-escape-" t))
         (staging (file-name-concat tempdir "staging"))
         (publication
          (list :head ".publications/0001/manifest.el"
                :artifacts (list (cons "../outside.txt"
                                       (list :published (file-name-concat tempdir "x")
                                             :sha256 "0")))))
         (session (niwa-session--create
                   :authority-mode 'portable
                   :save-path (file-name-as-directory tempdir)
                   :publication publication)))
    (unwind-protect
        (progn
          (should-error (niwa-session-rewind-materialize-publication
                         session publication staging))
          (should-not (file-exists-p (file-name-concat tempdir "outside.txt"))))
      (delete-directory tempdir t))))

(niwa-deftest niwa-session-rewind--commit-rewind ()
  ,test
  (test)
  :doc "commits one replacement head without moving remote control state"
  (let* ((host "remote-rewind-commit")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-rewind-" t)))
         buffer stopped)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let* ((niwa-session-durability--client-id
                    (make-string 64 ?b))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (target '(:segment 1 :turn 1 :cum-turn 1
                             :fork-point-id "remote-point")))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*remote-rewind*" session))
              (setf (niwa-session-publication session)
                    (niwa-session-publication-read session-dir)
                    (niwa-session-turn-count session) 1
                    (niwa-session-prompt-index session)
                    `((1 . (,target))))
              (setq buffer (generate-new-buffer " *remote-rewind-live*"))
              (with-current-buffer buffer
                (org-mode)
                (insert "Original transcript\n")
                (setq-local niwa--session session)
                (setq buffer-file-name segment))
              (let ((generation
                     (plist-get (niwa-session-lease session) :generation))
                    (old-head
                     (plist-get (niwa-session-publication session) :head)))
                (cl-letf
                    (((symbol-function 'niwa-execution-stop-services)
                      (lambda (owner)
                        (should (eq owner session))
                        (should (= 1 (niwa-session-turn-count owner)))
                        (setq stopped t)))
                     ((symbol-function
                       'niwa-session-rewind--stage-rewind)
                      (lambda (_session candidate actual-target staging-path
                               staging-buffer &rest _)
                        (make-directory staging-path t)
                        (with-current-buffer staging-buffer
                          (erase-buffer)
                          (insert "Rewound transcript\n")
                          (setq buffer-file-name
                                (niwa-session-artifacts-segment-path
                                 staging-path
                                 (plist-get actual-target :segment)))
                          (write-region (point-min) (point-max)
                                        buffer-file-name nil 'silent))
                        candidate))
                     ((symbol-function
                       'niwa-session-rewind-restore-plan)
                      (lambda (&rest _) nil))
                     ((symbol-function
                       'niwa-session-rewind--refresh-restored-buffers)
                      #'ignore))
                  (should
                   (niwa-session-rewind--commit-rewind
                    session buffer target nil)))
                (should stopped)
                (should
                 (equal generation
                        (plist-get (niwa-session-lease session)
                                   :generation)))
                (should-not
                 (equal old-head
                        (plist-get (niwa-session-publication session)
                                   :head)))
                (should
                 (file-exists-p (file-name-concat session-dir old-head)))
                (should
                 (file-directory-p
                  (file-name-concat session-dir ".publications")))
                (should
                 (equal "Rewound transcript\n"
                        (niwa-session-artifacts-read-artifact
                         session "segment-0001.chat.org" t)))
                (should (= 0 (niwa-session-turn-count session)))
                (with-current-buffer buffer
                  (should (equal "Rewound transcript\n" (buffer-string))))))))
      (when (buffer-live-p buffer)
        (let ((session (buffer-local-value 'niwa--session buffer)))
          (when (and session
                     (niwa-session-durability-lease-owned-p session))
            (niwa-session-durability-lease-release
             (niwa-session-save-path session) session)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "keeps a committed head and refreshes on when the buffer install fails"
  (let* ((host "remote-rewind-install-fail")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-rewind-" t)))
         buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let* ((niwa-session-durability--client-id
                    (make-string 64 ?d))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (target '(:segment 1 :turn 1 :cum-turn 1
                             :fork-point-id "remote-point")))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*remote-rewind-install*" session))
              (setf (niwa-session-publication session)
                    (niwa-session-publication-read session-dir)
                    (niwa-session-turn-count session) 1
                    (niwa-session-prompt-index session)
                    `((1 . (,target))))
              (setq buffer (generate-new-buffer " *remote-rewind-live*"))
              (with-current-buffer buffer
                (org-mode)
                (insert "Original transcript\n")
                (setq-local niwa--session session)
                (setq buffer-file-name segment))
              (let ((generation
                     (plist-get (niwa-session-lease session) :generation))
                    (old-head
                     (plist-get (niwa-session-publication session) :head))
                    (followed nil)
                    (warning nil))
                (cl-letf
                    (((symbol-function
                       'niwa-session-rewind--stage-rewind)
                      (lambda (_session candidate actual-target staging-path
                               staging-buffer &rest _)
                        (make-directory staging-path t)
                        (with-current-buffer staging-buffer
                          (erase-buffer)
                          (insert "Rewound transcript\n")
                          (setq buffer-file-name
                                (niwa-session-artifacts-segment-path
                                 staging-path
                                 (plist-get actual-target :segment)))
                          (write-region (point-min) (point-max)
                                        buffer-file-name nil 'silent))
                        candidate))
                     ((symbol-function
                       'niwa-session-rewind-restore-plan)
                      (lambda (&rest _) nil))
                     ((symbol-function
                       'niwa-session-rewind--install-rewind-buffer)
                      (lambda (&rest _)
                        (signal 'beginning-of-buffer nil)))
                     ((symbol-function
                       'niwa-instruction-store-rewind-end)
                      (lambda (&rest _) (push 'instructions followed) t))
                     ((symbol-function
                       'niwa-session-rewind--refresh-restored-buffers)
                      (lambda (&rest _) (push 'buffers followed))))
                  (niwa-test--with-captured-diagnostics warning
                    (should
                     (niwa-session-rewind--commit-rewind
                      session buffer target nil))))
                ;; The head is committed, so the failed install degrades to
                ;; a warning that names its own frame, and every later
                ;; local refresh still runs.
                (should-not
                 (equal old-head
                        (plist-get (niwa-session-publication session)
                                   :head)))
                (should (equal generation
                               (plist-get (niwa-session-lease session)
                                          :generation)))
                (should (equal '(instructions buffers)
                               (nreverse followed)))
                (should (string-match-p "did not install" warning))
                (should (string-match-p "Beginning of buffer" warning))
                (should
                 (string-match-p
                  "niwa-session-rewind--install-rewind-buffer" warning))))))
      (when (buffer-live-p buffer)
        (let ((session (buffer-local-value 'niwa--session buffer)))
          (when (and session
                     (niwa-session-durability-lease-owned-p session))
            (niwa-session-durability-lease-release
             (niwa-session-save-path session) session)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "rolls project files back under a fresh authority reservation"
  (let* ((host "remote-rewind-rollback")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-rewind-rollback-" t)))
         buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let* ((niwa-session-durability--client-id
                    (make-string 64 ?c))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (tracked (file-name-concat
                             (file-name-directory session-dir) "tracked.el"))
                   (target '(:segment 1 :turn 1 :cum-turn 1
                             :fork-point-id "remote-point"))
                   (plan (list (list :path tracked :action 'overwrite
                                     :bytes "rewound project bytes\n")))
                   (reserve-function
                    (symbol-function
                     'niwa-session-durability-call-with-reserved-lease))
                   reservations)
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*remote-rewind-rollback*" session))
              (setf (niwa-session-publication session)
                    (niwa-session-publication-read session-dir)
                    (niwa-session-turn-count session) 1)
              (write-region "current project bytes\n" nil tracked nil 'silent)
              (setq buffer (generate-new-buffer " *remote-rewind-rollback*"))
              (with-current-buffer buffer
                (org-mode)
                (insert "Original transcript\n")
                (setq-local niwa--session session)
                (setq buffer-file-name segment))
              (let ((old-head
                     (plist-get (niwa-session-publication session) :head)))
                (cl-letf
                    (((symbol-function
                       'niwa-session-rewind--stage-rewind)
                      (lambda (_session candidate actual-target staging-path
                               staging-buffer &rest _)
                        (make-directory staging-path t)
                        (with-current-buffer staging-buffer
                          (erase-buffer)
                          (insert "Rewound transcript\n")
                          (setq buffer-file-name
                                (niwa-session-artifacts-segment-path
                                 staging-path
                                 (plist-get actual-target :segment)))
                          (write-region (point-min) (point-max)
                                        buffer-file-name nil 'silent))
                        candidate))
                     ((symbol-function
                       'niwa-session-rewind-restore-plan)
                      (lambda (&rest _) plan))
                     ((symbol-function 'niwa-session-publication-publish)
                      (lambda (actual-session _artifacts)
                        (setf (niwa-session-pending-publication
                               actual-session)
                              '(:batches nil))
                        (error "Injected pre-CAS failure")))
                     ((symbol-function
                       'niwa-session-durability-call-with-reserved-lease)
                      (lambda (actual-session function)
                        (push (niwa-session-save-path actual-session)
                              reservations)
                        (funcall reserve-function actual-session function))))
                  (should-error
                   (niwa-session-rewind--commit-rewind
                    session buffer target plan)))
                (should (= 2 (length reservations)))
                (should-not (niwa-session-pending-publication session))
                (should
                 (equal old-head
                        (plist-get (niwa-session-publication session)
                                   :head)))
                (should
                 (equal "current project bytes\n"
                        (niwa-session-artifacts--file-text tracked)))
                (with-current-buffer buffer
                  (should (equal "Original transcript\n"
                                 (buffer-string))))))))
      (when (buffer-live-p buffer)
        (let ((session (buffer-local-value 'niwa--session buffer)))
          (when (and session
                     (niwa-session-durability-lease-owned-p session))
            (niwa-session-durability-lease-release
             (niwa-session-save-path session) session)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "pre-restore failure preserves a concurrent edit only after staging is reached"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *rewind-pre-restore*"))
           (path (file-name-concat tempdir "tracked.el")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session)
            (insert "First prompt\n")
            (insert (propertize "First reply.\n" 'gptel 'response))
            (setf (niwa-session-turn-count session) 1)
            (niwa-session-artifacts-save session buffer t)
            (let* ((target (copy-sequence
                            (cdar (niwa-session-rewind--prompt-candidates
                                   session))))
                   (plan (list (list :path path :action 'overwrite)))
                   (head (plist-get (niwa-session-publication session) :head))
                   (text (buffer-string))
                   (state (niwa-session-codec-serialize session))
                   (publication (niwa-session-publication session))
                   (read-publication
                    (symbol-function 'niwa-session-publication-read))
                   staging-reached unpublished)
              (cl-letf
                  (((symbol-function 'niwa-session-publication-read)
                    (lambda (&rest args)
                      (unless unpublished (apply read-publication args))))
                   ((symbol-function 'niwa-session-rewind--stage-rewind)
                    (lambda (&rest _)
                      (setq staging-reached t)
                      (write-region "concurrent" nil path nil 'silent)
                      (error "Injected staging failure"))))
                ;; An unpublished fixture's early error is not this regression.
                (setq unpublished t)
                (setf (niwa-session-publication session) nil)
                (write-region "before" nil path nil 'silent)
                (let ((err (should-error
                            (niwa-session-rewind--commit-rewind
                             session buffer target plan))))
                  (should (string-match-p "requires a committed publication"
                                          (error-message-string err))))
                (should-not staging-reached)
                (should (equal "before" (niwa-session-artifacts--file-text path)))
                (setq unpublished nil)
                (setf (niwa-session-publication session) publication)
                (let ((err (should-error
                            (niwa-session-rewind--commit-rewind
                             session buffer target plan))))
                  (should (equal "Injected staging failure"
                                 (error-message-string err)))))
              (should staging-reached)
              (should (equal "concurrent" (niwa-session-artifacts--file-text path)))
              (should (equal head (plist-get (niwa-session-publication session)
                                             :head)))
              (should (equal text (buffer-string)))
              (should (equal state (niwa-session-codec-serialize session)))))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))
  :doc "boundary after rechecks the published post-turn plan instead of the pre-turn plan"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *rewind-boundary-after*"))
           (path (file-name-concat tempdir "tracked.el"))
           (pre (niwa-session-artifacts--file-history-backup-name "pre-turn"))
           (post (niwa-session-artifacts--file-history-backup-name "post-turn")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session
                        gptel-backend (test-niwa-session-persistence--agent-backend)
                        gptel-model 'test-model)
            (gptel-mode 1)
            (insert "First prompt\n")
            (insert (propertize "First reply.\n" 'gptel 'response))
            (setf (niwa-session-turn-count session) 1)
            (niwa-session-artifacts-save session buffer t)
            (setf (niwa-session-file-snapshots session)
                  `((1 . ((,path . (:backup-name ,post :pre-backup-name ,pre
                                   :version 1))))))
            (test-niwa-rewind--publish-backups
             session pre "pre-turn" post "post-turn")
            (write-region "external edit" nil path nil 'silent)
            (let ((target (copy-sequence
                           (cdar (niwa-session-rewind--prompt-candidates
                                  session))))
                  (plan (niwa-session-rewind-restore-plan session 1))
                  (head (plist-get (niwa-session-publication session) :head)))
              (should (plist-get target :fork-point-id))
              (should (= 1 (length plan)))
              (should (niwa-session-rewind--commit-rewind
                       session buffer target plan 'after))
              (should (equal "post-turn" (niwa-session-artifacts--file-text path)))
              (should (= 1 (niwa-session-turn-count session)))
              (should-not
               (equal head (plist-get (niwa-session-publication session) :head)))))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry)))))


(niwa-deftest niwa-session-rewind--rewind-confirmation ()
  ,test
  (test)
  :doc "names the boundary and both sides of it"
  (let ((session (niwa-session--create :name "main"
                                          :authority-mode 'portable))
        (target '(:segment 1 :turn 2 :cum-turn 2)))
    (should
     (equal
      "Rewind main to before S1 T2: keep 1 turn, discard 3 (0 files; conversation and captured-file redo)? "
      (niwa-session-rewind--rewind-confirmation
       session (list :target target :boundary 'before
                     :surviving-turns 1 :discarded-turns 3 :file-plan nil))))
    (should
     (equal
      "Rewind main keeping S1 T2: keep 2 turns, discard 2 (1 file; conversation and captured-file redo)? "
      (niwa-session-rewind--rewind-confirmation
       session (list :target target :boundary 'after
                     :surviving-turns 2 :discarded-turns 2
                     :file-plan (list (list :action 'restore
                                            :path "/tmp/a"))))))))

(niwa-deftest niwa-session-rewind--restore-confirmation ()
  ,test
  (test)
  :doc "names the turns and captured files a restore returns"
  (let ((session (niwa-session--create :name "main"))
        (entry (list :head ".publications/generation-abc/manifest.el"
                     :time '(0 0) :turn-count 4 :transcript-bytes 2048)))
    (should
     (string-match-p
      "4 turns, 2 captured files, 1 externally changed; uncaptured effects remain"
      (niwa-session-rewind--restore-confirmation
       session entry
       (list :restored-turns 4
             :file-plan (list (list :action 'restore :path "/tmp/a")
                              (list :action 'overwrite :path "/tmp/b"))
             :external-overwrites 1))))
    ;; A restore with no captured files promises nothing about files.
    (let ((prompt (niwa-session-rewind--restore-confirmation
                   session entry
                   (list :restored-turns 1 :file-plan nil
                         :external-overwrites 0))))
      (should (string-match-p "1 turns, 0 captured files" prompt))
      (should-not (string-match-p "uncaptured" prompt)))))

(niwa-deftest niwa-redo ()
  ,test
  (test)
  :doc "selects and restores one published head, then starts a restore epoch"
  (with-temp-buffer
    (let* ((session (niwa-session--create
                     :name "main" :authority-mode 'portable))
           (entry '(:head ".publications/generation-abc/manifest.el"))
           restored hook-source)
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-session-rewind-published-heads)
                 (lambda (_session) (list entry)))
                ((symbol-function
                  'niwa-session-rewind--published-head-label)
                 (lambda (_entry) "published state"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "published state"))
                ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) '(?f "conversation and files")))
                ((symbol-function 'niwa-session-rewind-restore-head)
                 (lambda (seen-session seen-buffer head confirm conversation-only complete)
                   (should-not conversation-only)
                   (setq restored (list seen-session seen-buffer head))
                   (funcall confirm
                            '(:restored-turns 1 :file-plan nil
                              :external-overwrites 0))
                   (funcall complete t nil)
                   t))
                ((symbol-function 'niwa--run-session-start-hooks)
                 (lambda (source) (setq hook-source source)))
                ((symbol-function 'message) #'ignore))
        (should (niwa-redo)))
      (should (equal (list session (current-buffer) (plist-get entry :head))
                     restored))
      (should (equal "restore" hook-source)))))

(niwa-deftest niwa-session-rewind-rewind/conversation (:quiet t)
  ,test
  (test)
  :doc "conversation rewind and redo preserve changed disk bytes and unsaved buffers"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "conversation-only" workspace))
           (buffer (generate-new-buffer " *conversation-rewind*"))
           (path (file-name-concat tempdir "source.txt"))
           visiting head)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session niwa--workspace workspace)
            (write-region "before\n" nil path nil 'silent)
            (niwa-request-begin session)
            (puthash path "before\n"
                     (niwa-request-file-snapshots niwa--current-request))
            (write-region "after\n" nil path nil 'silent)
            (insert "Prompt\n" (propertize "Reply.\n" 'gptel 'response))
            (setf (niwa-session-turn-count session) 1)
            (niwa-session-artifacts-save session buffer t)
            (niwa-request-end)
            (setq head (plist-get (niwa-session-publication session) :head))
            (write-region "external\n" nil path nil 'silent)
            (setq visiting (find-file-noselect path))
            (with-current-buffer visiting (goto-char (point-max)) (insert "draft\n"))
            (let ((target (cdar (niwa-session-rewind--prompt-candidates session))))
              (cl-letf (((symbol-function 'niwa-session-rewind--prepare-rewind-files)
                         (lambda (&rest _) (ert-fail "Read working files")))
                        ((symbol-function 'niwa-session-rewind--restore-file-plan)
                         (lambda (&rest _) (ert-fail "Read working files during Redo")))
                        ((symbol-function 'niwa-session-rewind--render-rewind-impact)
                         (lambda (&rest _) (ert-fail "Opened file impact UI")))
                        ((symbol-function 'niwa--run-session-start-hooks) #'ignore))
                (should (niwa-session-rewind-rewind
                         buffer target 'before 'conversation (lambda (_) t)))
                (should (= 0 (niwa-session-turn-count session)))
                (should-not (string-search "Reply." (buffer-string)))
                (should (niwa-session-rewind-restore-head session buffer head nil t))
                (should (= 1 (niwa-session-turn-count session)))
                (should (string-search "Reply." (buffer-string)))))
            (should (equal "external\n" (niwa-test--read-file path)))
            (with-current-buffer visiting
              (should (buffer-modified-p))
              (should (equal "external\ndraft\n" (buffer-string)))))
        (when (buffer-live-p visiting)
          (with-current-buffer visiting (set-buffer-modified-p nil))
          (kill-buffer visiting))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-session-rewind--choose-scope ()
  ,test
  (test)
  :doc "maps explicit scope choices and cancellation without reading files"
  (dolist (choice '((?c . conversation) (?f . files) (?q . abort)))
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (&rest _) (list (car choice)))))
      (should (eq (cdr choice)
                  (niwa-session-rewind--choose-scope "Rewind" "before T2"))))))

(niwa-deftest niwa-session-rewind--redo-availability ()
  ,test
  (test)
  :doc "promises captured-state redo exactly where heads stay published"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (should (equal "conversation and captured-file redo"
                       (niwa-session-rewind--redo-availability
                        (niwa-session-create "redo" workspace))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "promises no redo for a workspace-free session"
  (should (equal "no redo"
                 (niwa-session-rewind--redo-availability
                  (niwa-session--create)))))

(niwa-deftest niwa-session-rewind-published-heads ()
  ,test
  (test)
  :doc "lists one head per settled turn state, newest first"
  (let* ((host "published-heads")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-published-heads-" t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((niwa-session-durability--client-id (make-string 64 ?e))
                  (niwa-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*published-heads*" session))
              (unwind-protect
                  (cl-labels
                      ((publish (transcript turns)
                         (setf (niwa-session-turn-count session) turns)
                         (niwa-session-publication-publish
                          session
                          (list
                           (list :path segment :content transcript)
                           (list :path
                                 (niwa-session-artifacts-sidecar-path
                                  session-dir)
                                 :content
                                 (niwa-session-artifacts-printed-value
                                  (niwa-session-artifacts--live-sidecar
                                   session (current-buffer)))
                                 :commit-marker t
                                 :replace t)))
                         (plist-get (niwa-session-publication session)
                                    :head))
                       (heads ()
                         (mapcar (lambda (entry) (plist-get entry :head))
                                 (niwa-session-rewind-published-heads
                                  session))))
                    (setf (niwa-session-publication session)
                          (niwa-session-publication-read session-dir))
                    (let ((settled (publish "Turn one transcript\n" 1)))
                      ;; The state the session is already in is not a
                      ;; restore target.
                      (should-not (member settled (heads)))
                      ;; A later save publishes another generation for the
                      ;; same settled state; it stands for the earlier one
                      ;; rather than adding a second row.
                      (let ((mid (publish "Turn one transcript, more\n" 1)))
                        (should-not (member mid (heads)))
                        (should-not (member settled (heads)))
                        ;; Rewinding moves the turn count, so the state it
                        ;; left becomes restorable -- once.  Which of its
                        ;; generations stands for it is not the contract:
                        ;; they restore the same conversation.
                        (let ((rewound (publish "Rewound transcript\n" 0)))
                          (should-not (member rewound (heads)))
                          (should
                           (= 1 (length (seq-filter
                                         (lambda (head) (member head (heads)))
                                         (list settled mid)))))
                          (should
                           (= 1 (cl-count
                                 1
                                 (mapcar
                                  (lambda (entry)
                                    (plist-get entry :turn-count))
                                  (niwa-session-rewind-published-heads
                                   session)))))))))
                (niwa-session-durability-lease-release
                 session-dir session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))

  :doc "returns nothing for a workspace-free session"
  (should-not
   (niwa-session-rewind-published-heads
    (niwa-session--create))))

(niwa-deftest niwa-session-rewind-restore-head ()
  ,test
  (test)
  :doc "republishes a superseded head as the live conversation"
  (let* ((host "restore-head")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-restore-head-" t)))
         buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((niwa-session-durability--client-id (make-string 64 ?f))
                  (niwa-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*restore-head*" session))
              (unwind-protect
                  (cl-labels
                      ((publish (transcript turns)
                         (setf (niwa-session-turn-count session) turns)
                         (niwa-session-publication-publish
                          session
                          (list
                           (list :path segment :content transcript)
                           (list :path
                                 (niwa-session-artifacts-sidecar-path
                                  session-dir)
                                 :content
                                 (niwa-session-artifacts-printed-value
                                  (niwa-session-artifacts--live-sidecar
                                   session (current-buffer)))
                                 :commit-marker t
                                 :replace t)))
                         (plist-get (niwa-session-publication session)
                                    :head)))
                    (setf (niwa-session-publication session)
                          (niwa-session-publication-read session-dir))
                    ;; The fixture's head is also a turn-0 state.  Heads are
                    ;; ordered by whole-second manifest times, so make it
                    ;; strictly older: the newest turn-0 head, the rewound
                    ;; one, must be the one that stands for that state.
                    (set-file-times
                     (file-name-concat
                      session-dir
                      (plist-get (niwa-session-publication session) :head))
                     (time-subtract nil 60))
                    (let* ((fired (list :id "w-000001" :note "Fired already"
                                        :timing '(:kind once :at 1795177200)
                                        :created-at 1795176000 :due 1795177200
                                        :expires-at nil :status 'pending))
                           (live (list :id "w-000002" :note "Still pending"
                                       :timing '(:kind once :at 1795180800)
                                       :created-at 1795176000 :due 1795180800
                                       :expires-at nil :status 'pending))
                           (restorable
                            (progn (setf (niwa-session-schedules session) (list fired))
                                   (publish "Original transcript\n" 1)))
                           (rewound
                            (progn (setf (niwa-session-schedules session) (list live))
                                   (publish "Rewound transcript\n" 0))))
                      (setq buffer (generate-new-buffer " *restore-live*"))
                      (with-current-buffer buffer
                        (org-mode)
                        (insert "Rewound transcript\n")
                        (setq-local niwa--session session)
                        (setq buffer-file-name segment))
                      (cl-letf
                          (((symbol-function
                             'niwa-session-artifacts-load-instructions)
                            (lambda (&rest _) t))
                           ((symbol-function
                             'niwa-session-recovery-refresh-session-buffers)
                            #'ignore))
                        (should
                         (niwa-session-rewind-restore-head
                          session buffer restorable)))
                      ;; The restore is itself a new committed head, and
                      ;; the superseded transcript and turn count are live
                      ;; again.
                      (let ((head (plist-get
                                   (niwa-session-publication session)
                                   :head)))
                        (should-not (equal head rewound))
                        (should-not (equal head restorable)))
                      (should (= 1 (niwa-session-turn-count session)))
                      ;; Wake-ups belong to the live session: a restored head
                      ;; neither brings back a fired one nor drops a pending one.
                      (should (equal (list live) (niwa-session-schedules session)))
                      (should (equal (list live)
                                     (plist-get
                                      (niwa-session-codec-read
                                       (plist-get (niwa-session-publication-read session-dir)
                                                  :sidecar))
                                      :schedules)))
                      (should (= 1 (plist-get
                                    (niwa-session-checkpoint-project session nil)
                                    :total-turn-count)))
                      (should
                       (equal "Original transcript\n"
                              (niwa-session-artifacts-read-artifact
                               session "segment-0001.chat.org" t)))
                      (with-current-buffer buffer
                        (should (equal "Original transcript\n"
                                       (buffer-string))))
                      ;; Restoring consumes nothing: the state it moved
                      ;; away from is restorable in turn.
                      (should
                       (member rewound
                               (mapcar (lambda (entry) (plist-get entry :head))
                                       (niwa-session-rewind-published-heads
                                        session))))))
                (niwa-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))

  :doc "restores captured files from the published head's file history"
  ;; The pre-Rewind file bytes stay published under `file-history', and
  ;; the restored sidecar reinstates the snapshot index that names them,
  ;; so a restore returns captured working-tree state as well.
  (let* ((host "restore-head-files")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-restore-files-" t)))
         buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((niwa-session-durability--client-id (make-string 64 ?a))
                  (niwa-session-durability--disclosed-targets
                   (make-hash-table :test #'equal))
                  (tracked (file-name-concat local-root "tracked.el")))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*restore-files*" session))
              (unwind-protect
                  (progn
                    (write-region "captured bytes\n" nil tracked nil 'silent)
                    (setf (niwa-session-publication session)
                          (niwa-session-publication-read session-dir)
                          (niwa-session-turn-count session) 1
                          (niwa-session-file-snapshots session)
                          `((1 . ((,tracked . (:backup-name "000001"
                                               :pre-backup-name "000001"
                                               :version 1))))))
                    ;; Publish the head that owns both the transcript and
                    ;; the captured file bytes.
                    (niwa-session-publication-publish
                     session
                     (list
                      (list :path (file-name-concat
                                   session-dir "file-history" "000001")
                            :content "captured bytes\n")
                      (list :path segment :content "Original transcript\n")
                      (list :path (niwa-session-artifacts-sidecar-path
                                   session-dir)
                            :content
                            (niwa-session-artifacts-printed-value
                             (niwa-session-artifacts--live-sidecar
                              session (current-buffer)))
                            :commit-marker t
                            :replace t)))
                    (let ((restorable
                           (plist-get (niwa-session-publication session)
                                      :head)))
                      ;; Supersede it the way a Rewind does: no turns, no
                      ;; snapshots, and the working tree moved on.
                      (setf (niwa-session-turn-count session) 0
                            (niwa-session-file-snapshots session) nil)
                      (write-region "rewound bytes\n" nil tracked nil 'silent)
                      (niwa-session-publication-publish
                       session
                       (list
                        (list :path segment :content "Rewound transcript\n")
                        (list :path (niwa-session-artifacts-sidecar-path
                                     session-dir)
                              :content
                              (niwa-session-artifacts-printed-value
                               (niwa-session-artifacts--live-sidecar
                                session (current-buffer)))
                              :commit-marker t
                              :replace t)))
                      (setq buffer (generate-new-buffer " *restore-files*"))
                      (with-current-buffer buffer
                        (org-mode)
                        (insert "Rewound transcript\n")
                        (setq-local niwa--session session)
                        (setq buffer-file-name segment))
                      (let (impact)
                        (cl-letf
                            (((symbol-function
                               'niwa-session-artifacts-load-instructions)
                              (lambda (&rest _) t))
                             ((symbol-function
                               'niwa-session-recovery-refresh-session-buffers)
                              #'ignore))
                          (should
                           (niwa-session-rewind-restore-head
                            session buffer restorable
                            (lambda (actual) (setq impact actual) t))))
                        ;; The impact named the captured file before any
                        ;; mutation, and the file came back with the turns.
                        (should (= 1 (plist-get impact :restored-turns)))
                        (should (= 1 (length (plist-get impact :file-plan))))
                        (should (= 1 (plist-get impact
                                                :external-overwrites))))
                      (should
                       (equal "captured bytes\n"
                              (niwa-session-artifacts--file-text tracked)))
                      (should (= 1 (niwa-session-turn-count session)))
                      (with-current-buffer buffer
                        (should (equal "Original transcript\n"
                                       (buffer-string))))))
                (niwa-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))

  :doc "abandons everything when the confirmation declines"
  (let* ((host "restore-head-declined")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-restore-declined-" t)))
         buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((niwa-session-durability--client-id (make-string 64 ?c))
                  (niwa-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*restore-declined*" session))
              (unwind-protect
                  (progn
                    (setf (niwa-session-publication session)
                          (niwa-session-publication-read session-dir))
                    (let ((restorable
                           (plist-get (niwa-session-publication session)
                                      :head)))
                      (niwa-session-publication-publish
                       session
                       (list
                        (list :path segment :content "Rewound transcript\n")
                        (list :path (niwa-session-artifacts-sidecar-path
                                     session-dir)
                              :content
                              (niwa-session-artifacts-read-file-raw
                               (niwa-session-artifacts-sidecar-path
                                session-dir))
                              :commit-marker t
                              :replace t)))
                      (setq buffer (generate-new-buffer " *restore-declined*"))
                      (with-current-buffer buffer
                        (org-mode)
                        (insert "Rewound transcript\n")
                        (setq-local niwa--session session)
                        (setq buffer-file-name segment))
                      (let ((rewound-head
                             (plist-get (niwa-session-publication session)
                                        :head)))
                        (should-not
                         (niwa-session-rewind-restore-head
                          session buffer restorable (lambda (_impact) nil)))
                        (should
                         (equal rewound-head
                                (plist-get
                                 (niwa-session-publication session)
                                 :head)))
                        (should-not
                         (niwa-session-pending-publication session))
                        (with-current-buffer buffer
                          (should (equal "Rewound transcript\n"
                                         (buffer-string)))))))
                (niwa-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))

  :doc "rolls a pre-CAS file restore back under a fresh lease reservation"
  (let* ((root (make-temp-file "niwa-restore-rollback-" t))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "restore"))
         (session (niwa-session--create
                   :workspace workspace :authority-mode 'portable
                   :save-path root
                   :publication '(:head "current")))
         (candidate (niwa-session--create :workspace workspace))
         (buffer (generate-new-buffer " *restore-rollback*"))
         (plan '((:path "/tmp/captured" :action overwrite)))
         (backups '((:path "/tmp/captured" :existed nil)))
         (reservations 0)
         rolled-back)
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-rewind-assert-stable-source)
              #'ignore)
             ((symbol-function 'niwa-session-publication-read)
              (lambda (_path &optional head)
                (if head '(:head "target") '(:head "current"))))
             ((symbol-function
               'niwa-session-rewind-materialize-publication)
              #'ignore)
             ((symbol-function 'niwa-session-codec-read)
              (lambda (_path) '(:sidecar t)))
             ((symbol-function 'niwa-session-codec-deserialize)
              (lambda (_raw _workspace) (list :session candidate)))
             ((symbol-function 'niwa-session-set-execution-target)
              #'ignore)
             ((symbol-function
               'niwa-session-rewind--load-restored-transcript)
              #'ignore)
             ((symbol-function 'niwa-session-rewind--restore-file-plan)
              (lambda (_candidate) plan))
             ((symbol-function
               'niwa-session-rewind--prepare-buffers-for-restore)
              (lambda (_candidate _turn actual-plan &rest _) actual-plan))
             ((symbol-function
               'niwa-session-rewind--backup-restore-files)
              (lambda (_plan _directory) backups))
             ((symbol-function 'niwa-session-rewind-execute-restore)
              (lambda (_candidate _plan) '(:succeeded 1)))
             ((symbol-function
               'niwa-session-rewind-rewind-publication-artifacts)
              (lambda (&rest _) nil))
             ((symbol-function 'niwa-session-publication-publish)
              (lambda (actual-session &rest _)
                (setf (niwa-session-pending-publication actual-session)
                      '(:batches nil))
                (error "Injected pre-CAS failure")))
             ((symbol-function
               'niwa-session-durability-call-with-reserved-lease)
              (lambda (_session function)
                (cl-incf reservations)
                (funcall function)))
             ((symbol-function
               'niwa-session-rewind--rollback-restore-files)
              (lambda (actual-backups _written)
                (setq rolled-back actual-backups)
                nil))
             ((symbol-function
               'niwa-session-publication-discard-rolled-back)
              (lambda (actual-session)
                (setf (niwa-session-pending-publication actual-session)
                      nil))))
          (let ((raised
                 (should-error
                  (niwa-session-rewind-restore-head
                   session buffer "target" (lambda (_impact) t)))))
            (should (string-match-p "Injected pre-CAS failure"
                                    (error-message-string raised)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-directory-p root) (delete-directory root t)))
    (should (= 2 reservations))
    (should (equal backups rolled-back))
    (should-not (niwa-session-pending-publication session)))

  :doc "refuses a workspace-free session"
  (let ((session (niwa-session--create)))
    (should-error
     (niwa-session-rewind-restore-head
      session (current-buffer) ".publications/generation-x/manifest.el")
     :type 'user-error)))


(niwa-deftest niwa-session-rewind--rollback-restore-files ()
  ,test
  (test)
  :doc "reports every file whose rollback fails"
  (let* ((directory (make-temp-file "rewind-rollback-" t))
         (path-a (file-name-concat directory "a"))
         (path-b (file-name-concat directory "b"))
         (backup (file-name-concat directory "backup")))
    (unwind-protect
        (progn
          (write-region "before" nil backup nil 'silent)
          (dolist (path (list path-a path-b)) (write-region "after" nil path nil 'silent))
          (let ((failures
                 (cl-letf (((symbol-function 'copy-file)
                            (lambda (_source target &rest _)
                              (error "Cannot restore %s" target))))
                   (niwa-session-rewind--rollback-restore-files
                    (mapcar (lambda (path) (list :path path :existed t :backup backup))
                            (list path-a path-b))
                    (mapcar #'niwa-session-rewind--file-state (list path-a path-b))))))
            (should (= 2 (length failures)))
            (should (string-match-p (regexp-quote path-a) (nth 0 failures)))
            (should (string-match-p (regexp-quote path-b) (nth 1 failures)))))
      (delete-directory directory t))))


;;
;;; Phase 8: file restore plan

(niwa-deftest niwa-session-rewind-reduce-agent-transcripts ()
  ,test
  (test)
  :doc "keeps historical transcripts at or before the picked turn"
  (let ((entries
         '(("agent--early" :agent-path "/root/early"
            :parent-turn 2 :status completed)
           ("agent--unknown" :agent-path "/root/unknown"
            :status incomplete)
           ("agent--late" :agent-path "/root/late"
            :parent-turn 6 :status completed))))
    (should
     (equal '("agent--early" "agent--unknown")
            (mapcar
             #'car
             (niwa-session-rewind-reduce-agent-transcripts entries 5))))))

(niwa-deftest niwa-session-rewind--rewind-candidate ()
  ,test
  (test)
  :doc "keeps only chats closed before the target segment, so a later /clear stays ordered"
  (let* ((chapters '((:segment 1 :title "A" :closed-at "t1")
                     (:segment 3 :title "C" :closed-at "t3")))
         (session (niwa-session--create
                   :name "main" :current-segment 4 :turn-count 6
                   :chapters (copy-tree chapters)))
         (candidate (niwa-session-rewind--rewind-candidate
                     session '(:segment 2 :turn 1 :cum-turn 3))))
    (should (= 2 (niwa-session-current-segment candidate)))
    (should (equal '((:segment 1 :title "A" :closed-at "t1"))
                   (niwa-session-chapters candidate)))
    (should (equal chapters (niwa-session-chapters session)))
    ;; Two later clears close segments 2 and 3 under their own titles.
    (setf (niwa-session-chapters candidate)
          (append (niwa-session-chapters candidate)
                  '((:segment 2 :title "B" :closed-at "t5")
                    (:segment 3 :title "D" :closed-at "t6"))))
    (should (equal '(("A" 1 1) ("B" 2 2) ("D" 3 3) ("Now" 4 4))
                   (mapcar (lambda (chat)
                             (list (plist-get chat :title)
                                   (plist-get chat :first-segment)
                                   (plist-get chat :last-segment)))
                           (niwa-session-artifacts-chats
                            (niwa-session-chapters candidate) nil 4 "Now"))))))

(niwa-deftest niwa-session-rewind-reduce-prompt-index ()
  ,test
  (test)
  (let ((index '((1 . ((:cum-turn 1) (:cum-turn 2)))
                 (2 . ((:cum-turn 3) (:cum-turn 4)))
                 (3 . ((:cum-turn 5))))))
    (should
     (equal '((1 . ((:cum-turn 1) (:cum-turn 2)))
              (2 . ((:cum-turn 3) (:cum-turn 4))))
            (niwa-session-rewind-reduce-prompt-index index 2 4)))
    (should
     (equal '((1 . ((:cum-turn 1) (:cum-turn 2)))
              (2 . ((:cum-turn 3))))
            (niwa-session-rewind-reduce-prompt-index index 2 4 t)))))


(niwa-deftest niwa-session-rewind-reduce-file-snapshots ()
  ,test
  (test)
  (let ((snapshots '((1 . first) (3 . third) (4 . fourth) (5 . fifth))))
    (should
     (equal '((1 . first) (3 . third) (4 . fourth))
            (niwa-session-rewind-reduce-file-snapshots snapshots 4)))
    (should
     (equal '((1 . first) (3 . third))
            (niwa-session-rewind-reduce-file-snapshots snapshots 4 t)))
    (should
     (eq snapshots
         (niwa-session-rewind-reduce-file-snapshots snapshots nil)))))


(niwa-deftest niwa-session-rewind-state-at-turn ()
  ,test
  (test)
  :doc "picks each path's earliest pre-turn checkpoint in the discarded suffix"
  (let ((session (niwa-session-create
                  "x" (niwa-workspace-get-or-create
                       'project "id" "/tmp" "x"))))
    (setf (niwa-session-file-snapshots session)
          '((1 . (("/abs/foo" . (:backup-name "fooA-post"
                                  :pre-backup-name "fooA" :version 1))))
            (3 . (("/abs/foo" . (:backup-name "fooC-post"
                                  :pre-backup-name "fooC" :version 3))
                  ("/abs/bar" . (:backup-name "barB-post"
                                  :pre-backup-name "barB" :version 2))))
            (5 . (("/abs/foo" . (:backup-name "fooE-post"
                                  :pre-backup-name "fooE" :version 5))))))
    ;; Rewind before turn 2: foo and bar first change at turn 3.
    (let ((state (niwa-session-rewind-state-at-turn session 2 t)))
      (should (= 2 (length state)))
      (should (equal "fooC"
                     (plist-get (cdr (assoc "/abs/foo" state))
                                :pre-backup-name)))
      (should (equal "barB"
                     (plist-get (cdr (assoc "/abs/bar" state))
                                :pre-backup-name))))
    ;; Rewind before turn 1 selects foo's turn-1 checkpoint and bar's turn-3
    ;; checkpoint because bar was not changed before then.
    (let ((state (niwa-session-rewind-state-at-turn session 1 t)))
      (should (= 2 (length state)))
      (should (equal "fooA"
                     (plist-get (cdr (assoc "/abs/foo" state))
                                :pre-backup-name)))
      (should (equal "barB"
                     (plist-get (cdr (assoc "/abs/bar" state))
                                :pre-backup-name))))
    (niwa-workspace-clear-registry)))


(niwa-deftest niwa-session-rewind--latest-snapshot-entry ()
  ,test
  (test)
  :doc "returns highest-version entry for the path"
  (let ((session (niwa-session-create
                  "x" (niwa-workspace-get-or-create
                       'project "id2" "/tmp" "x"))))
    (setf (niwa-session-file-snapshots session)
          '((1 . (("/abs/foo" . (:backup-name "v1" :version 1))))
            (5 . (("/abs/foo" . (:backup-name "v3" :version 3))))
            (3 . (("/abs/foo" . (:backup-name "v2" :version 2))))))
    (let ((latest (niwa-session-rewind--latest-snapshot-entry
                   session "/abs/foo")))
      (should (equal "v3" (plist-get latest :backup-name))))
    (niwa-workspace-clear-registry)))


(niwa-deftest niwa-session-rewind--plan-row-diff ()
  ,test
  (test)
  :doc "materializes resolver-owned bytes instead of diffing a fixed cache"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-rewind-row-diff-" t)))
         (current (file-name-concat root "current.el"))
         (backup-name "current@v1")
         (fixed-backup
          (niwa-session-artifacts-backup-path root backup-name))
         (session (niwa-session--create :save-path root))
         resolver-called
         diff-current
         diff-target
         diff-target-content)
    (unwind-protect
        (progn
          (make-directory (file-name-directory fixed-backup) t)
          (write-region "current" nil current nil 'silent)
          (write-region "poison" nil fixed-backup nil 'silent)
          (with-temp-buffer
            (setq-local niwa-session-rewind--plan-buffer-session
                        session)
            (insert "row\n")
            (put-text-property
             (point-min) (point-max) 'niwa-plan-entry
             (list :action 'restore :path current
                   :backup-name backup-name))
            (goto-char (point-min))
            (cl-letf
                (((symbol-function 'niwa-session-artifacts-read-backup)
                  (lambda (seen-session seen-name)
                    (should (eq session seen-session))
                    (should (equal backup-name seen-name))
                    (setq resolver-called t)
                    "published"))
                 ((symbol-function 'diff)
                  (lambda (old new &optional _switches _no-async)
                    (setq diff-current old
                          diff-target new
                          diff-target-content
                          (niwa-session-artifacts-read-file-raw new)))))
              (niwa-session-rewind--plan-row-diff)))
          (should resolver-called)
          (should (equal current diff-current))
          (should (equal "published" diff-target-content))
          (should-not (file-exists-p diff-target))
          (should (equal "poison"
                         (niwa-session-artifacts-read-file-raw fixed-backup))))
      (delete-directory root t))))


(niwa-deftest niwa-session-rewind-restore-plan ()
  ,test
  (test)
  :doc "noop when current content matches target snapshot"
  (cl-destructuring-bind (session . tempdir)
      (test-niwa-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (backup-name (niwa-session-artifacts--file-history-backup-name "v1")))
          (write-region "v1" nil path nil 'silent)
          (setf (niwa-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,backup-name :version 1
                                  :backup-time "..." :file-mtime "..."))))))
          (test-niwa-rewind--publish-backups session backup-name "v1")
          (let ((plan (niwa-session-rewind-restore-plan session 1)))
            (should (null plan))))   ; noop entries filtered
      (test-niwa-session-persistence--cleanup tempdir)))
  :doc "create when target has content but file currently absent"
  (cl-destructuring-bind (session . tempdir)
      (test-niwa-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (backup-name (niwa-session-artifacts--file-history-backup-name "content")))
          ;; File doesn't currently exist.
          (setf (niwa-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,backup-name :version 1
                                  :backup-time "..." :file-mtime "..."))))))
          (test-niwa-rewind--publish-backups session backup-name "content")
          (let ((plan (niwa-session-rewind-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'create (plist-get (car plan) :action)))))
      (test-niwa-session-persistence--cleanup tempdir)))
  :doc "delete when target is absent but file exists"
  (cl-destructuring-bind (session . tempdir)
      (test-niwa-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "stale.el")))
          (write-region "stale content" nil path nil 'silent)
          (setf (niwa-session-file-snapshots session)
                `((1 . ((,path . (:backup-name nil :version 1
                                  :backup-time "..." :file-mtime nil))))))
          (let ((plan (niwa-session-rewind-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'delete (plist-get (car plan) :action)))))
      (test-niwa-session-persistence--cleanup tempdir)))
  :doc "overwrite when current content diverges from latest snapshot"
  (cl-destructuring-bind (session . tempdir)
      (test-niwa-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (b1   (niwa-session-artifacts--file-history-backup-name "v1"))
               (b2   (niwa-session-artifacts--file-history-backup-name "v2")))
          ;; Current file content is something the snapshots have never seen.
          (write-region "external edits" nil path nil 'silent)
          (setf (niwa-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,b1 :version 1
                                  :backup-time "..." :file-mtime "..."))))
                  (2 . ((,path . (:backup-name ,b2 :version 2
                                  :backup-time "..." :file-mtime "..."))))))
          (test-niwa-rewind--publish-backups session b1 "v1" b2 "v2")
          (let ((plan (niwa-session-rewind-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'overwrite (plist-get (car plan) :action)))
            (should (plist-get (car plan) :diverged))))
      (test-niwa-session-persistence--cleanup tempdir))))


(niwa-deftest niwa-session-rewind-execute-restore ()
  ,test
  (test)
  :doc "applies create / delete / restore actions correctly"
  (cl-destructuring-bind (session . tempdir)
      (test-niwa-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((create-path (file-name-concat tempdir "new.el"))
               (delete-path (file-name-concat tempdir "old.el"))
               (restore-path (file-name-concat tempdir "modified.el"))
               (backup-name-create
                (niwa-session-artifacts--file-history-backup-name "newly created"))
               (backup-name-restore
                (niwa-session-artifacts--file-history-backup-name "original")))
          (setf (niwa-session-file-snapshots session)
                `((1 . ((,create-path . (:backup-name ,backup-name-create :version 1))
                        (,restore-path . (:backup-name ,backup-name-restore :version 1))))))
          (test-niwa-rewind--publish-backups
           session backup-name-create "newly created"
           backup-name-restore "original")
          ;; Set up current state: delete-path exists, restore-path has different content
          (write-region "to be deleted" nil delete-path nil 'silent)
          (write-region "diverged" nil restore-path nil 'silent)
          (let* ((plan
                  (list (list :action 'create  :path create-path
                              :backup-name backup-name-create)
                        (list :action 'delete  :path delete-path)
                        (list :action 'overwrite :path restore-path
                              :backup-name backup-name-restore
                              :diverged t)))
                 (result (niwa-session-rewind-execute-restore
                          session plan)))
            (should (= 3 (plist-get result :succeeded)))
            (should (null (plist-get result :failed)))
            (should (file-exists-p create-path))
            (should-not (file-exists-p delete-path))
            (with-temp-buffer
              (insert-file-contents create-path)
              (should (equal "newly created" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents restore-path)
              (should (equal "original" (buffer-string))))))
      (test-niwa-session-persistence--cleanup tempdir)))
  :doc "stops on first failure"
  (cl-destructuring-bind (session . tempdir)
      (test-niwa-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "fine.el"))
               (bn   (niwa-session-artifacts--file-history-backup-name "ok")))
          (setf (niwa-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,bn :version 1))))))
          (test-niwa-rewind--publish-backups session bn "ok")
          (let* ((plan
                  (list (list :action 'create :path path :backup-name bn)
                        ;; Bogus backup name; reading the backup will fail.
                        (list :action 'create
                              :path (file-name-concat tempdir "two.el")
                              :backup-name "nonexistent@v1")
                        ;; Should not be reached.
                        (list :action 'create
                              :path (file-name-concat tempdir "three.el")
                              :backup-name bn)))
                 (result (niwa-session-rewind-execute-restore
                          session plan)))
            (should (= 1 (plist-get result :succeeded)))
            (should (plist-get result :failed))
            (should-not (file-exists-p
                         (file-name-concat tempdir "three.el")))))
      (test-niwa-session-persistence--cleanup tempdir))))


(niwa-deftest niwa-session-rewind-assert-stable-source ()
  ,test
  (test)
  :doc "blocks every live owner and permits stable Goal and Plan states"
  (let ((session (niwa-session--create :name "source"))
        (buffer (generate-new-buffer " *stable-source*"))
        execution-live
        agent-live)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--current-request nil))
          (cl-letf
              (((symbol-function 'niwa-execution-session-live-p)
                (lambda (_) execution-live))
               ((symbol-function 'niwa-agent-control-active-turn-p)
                (lambda (_) agent-live)))
            (setf (niwa-session-pending-follow-ups session)
                  '((:input "later")))
            (should-error
             (niwa-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setf (niwa-session-pending-follow-ups session) nil)
            (with-current-buffer buffer
              (setq-local niwa--current-request t))
            (should-error
             (niwa-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (with-current-buffer buffer
              (setq-local niwa--current-request nil))
            (setq execution-live t)
            (should-error
             (niwa-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setq execution-live nil
                  agent-live t)
            (should-error
             (niwa-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setq agent-live nil)
            (setf (niwa-session-goal session)
                  (niwa-goal--create :status 'active))
            (should-error
             (niwa-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setf (niwa-goal-status (niwa-session-goal session))
                  'paused
                  (niwa-session-pending-plan-approval session)
                  '(:proposal stable))
            (should-not
             (niwa-session-rewind-assert-stable-source
              session buffer "forking"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


(provide 'test-niwa-session-rewind)
;;; test-niwa-session-rewind.el ends here
