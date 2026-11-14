;;; test-mevedel-session-rewind.el --- Transactional Rewind tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-rewind'.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-chat)
(require 'mevedel-notes)
(require 'mevedel-session-rewind)
(require 'mevedel-session-artifacts)
(require 'mevedel-persistence)
(require 'mevedel-session-publication)
(require 'mevedel-session-recovery)
(require 'mevedel-session-durability)


(defun test-mevedel-rewind--publish-backups (session &rest pairs)
  "Publish and commit file-history backups for SESSION.
PAIRS alternate backup name and bytes.  Call after the snapshots referencing
them are set, so the committing save keeps them."
  (let ((mevedel-session-artifacts--collecting-critical-artifacts-p t)
        (mevedel-session-artifacts--critical-artifacts nil))
    (while pairs
      (mevedel-session-artifacts--file-history-write-backup
       (mevedel-session-save-path session) (pop pairs) (pop pairs)))
    (mevedel-session-publication-publish
     session (nreverse mevedel-session-artifacts--critical-artifacts) t))
  (mevedel-session-artifacts-save
   session (or (get-buffer "*test-data-buf*") (current-buffer)) nil t))

(mevedel-deftest mevedel-session-rewind--refresh-restored-buffers ()
  ,test
  (test)
  :doc "reverts visiting buffers after file restore, including confirmed local edits"
  (dolist (modified '(nil t))
    (let* ((tempdir (make-temp-file "mevedel-refresh-" t))
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
            (mevedel-session-rewind--refresh-restored-buffers
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
  (let* ((tempdir (make-temp-file "mevedel-refresh-" t))
         (file (file-name-concat tempdir "source.el"))
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (delete-file file)
          (mevedel-session-rewind--refresh-restored-buffers
           (list (list :action 'delete :path file))
           (list :succeeded 1))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t))))


(mevedel-deftest mevedel-session-rewind--prepare-buffers-for-restore ()
  ,test
  (test)
  :doc "discard reverts affected modified buffers before restore"
  (let* ((tempdir (make-temp-file "mevedel-prepare-" t))
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
                           (mevedel-session-rewind--prepare-buffers-for-restore
                            nil 1 plan))))
          (with-current-buffer buf
            (should-not (buffer-modified-p))
            (should (equal "old\n" (buffer-string)))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t)))
  :doc "abort returns abort sentinel when affected buffers are modified"
  (let* ((tempdir (make-temp-file "mevedel-prepare-" t))
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
                        (mevedel-session-rewind--prepare-buffers-for-restore
                         nil 1 plan))))
          (with-current-buffer buf
            (should (buffer-modified-p))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t))))


(mevedel-deftest mevedel-session-rewind--prompt-candidates ()
  ,test
  (test)
  :doc "returns flat alist with unique display strings across segments"
  (let ((session (mevedel-session-create
                  "main" (mevedel-workspace-get-or-create
                          'project "x" "/tmp" "x"))))
    (setf (mevedel-session-prompt-index session)
          '((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                   :pos 0 :preview "alpha" :fork-point-id "a")
                  (:turn 2 :file-turn 2 :cum-turn 2
                   :pos 100 :preview "beta" :fork-point-id "b")))
            (2 . ((:turn 1 :file-turn 1 :cum-turn 3
                   :pos 0 :preview "alpha" :fork-point-id "c")
                  (:turn 2 :file-turn 2 :cum-turn 4
                   :pos 50 :preview "gamma" :fork-point-id "d")))))
    (let ((candidates
           (mevedel-session-rewind--prompt-candidates session)))
      (should (= 4 (length candidates)))
      ;; All display strings unique (segment + turn folded in).
      (should (= 4 (length (cl-delete-duplicates
                            (mapcar #'car candidates) :test #'equal))))
      ;; Newest prompt in the newest segment first.
      (let* ((first (car candidates))
             (plist (cdr first)))
        (should (= 2 (plist-get plist :segment)))
        (should (= 2 (plist-get plist :turn)))))
    (mevedel-workspace-clear-registry))
  :doc "preserves raw file turn for compacted segments with copied tail"
  (let ((session (mevedel-session-create
                  "main" (mevedel-workspace-get-or-create
                          'project "x" "/tmp" "x"))))
    (setf (mevedel-session-prompt-index session)
          '((2 . ((:turn 1 :file-turn 3 :cum-turn 11
                   :pos 100 :preview "after tail"
                   :fork-point-id "after-tail")))))
    (let* ((candidate
            (car (mevedel-session-rewind--prompt-candidates session)))
           (plist (cdr candidate)))
      (should (= 1 (plist-get plist :turn)))
      (should (= 3 (plist-get plist :file-turn))))
    (mevedel-workspace-clear-registry))
  :doc "old directive index records appear as ordinary prompt previews"
  (let ((session (mevedel-session--create
                  :prompt-index
                  '((1 . ((:turn 2 :file-turn 2 :cum-turn 2
                           :kind directive :directive-id "abcdef123456"
                           :action request-changes
                           :preview "Old request"
                           :fork-point-id "directive")))))))
    (should (string-match-p
             "S1 T2  Old request"
             (caar (mevedel-session-rewind--prompt-candidates
                    session))))))


(mevedel-deftest mevedel-session-rewind--find-turn-cutoff ()
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
                 (mevedel-session-rewind--find-turn-cutoff 1)))))
  :doc "returns point-max when turn-n is the last"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Response.\n" 'gptel 'response))
    (insert "Last prompt\n")
    (should (= (point-max)
               (mevedel-session-rewind--find-turn-cutoff 2))))
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
                 (mevedel-session-rewind--find-turn-cutoff 1)))))
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
      (let ((prompts (mevedel-session-artifacts-collect-prompts
                      (current-buffer))))
        (should (= 2 (length prompts)))
        (should (equal "Second prompt"
                       (plist-get (nth 1 prompts) :preview)))
        (should (= next-prompt-pos
                   (mevedel-session-rewind--find-turn-cutoff 1)))))))


(mevedel-deftest mevedel-session-rewind--staged-file-p ()
  ,test
  (test)
  :doc "checks the remote Git index with target-native command arguments"
  (let* ((host "staged-file-host")
         (root (file-name-as-directory
                (make-temp-file "mevedel-staged-file-" t)))
         (local-file (file-name-concat root "staged.el"))
         (remote-file (format "/mevedelmock:%s:%s" host local-file)))
    (unwind-protect
        (progn
          (test-mevedel-session-persistence--git root "init")
          (write-region "staged\n" nil local-file nil 'silent)
          (test-mevedel-session-persistence--git root "add" "staged.el")
          (mevedel-test--with-local-shell-tramp (list host)
            (should
             (mevedel-session-rewind--staged-file-p remote-file)))
          (test-mevedel-session-persistence--git
           root "reset" "--" "staged.el")
          (mevedel-test--with-local-shell-tramp (list host)
            (should-not
             (mevedel-session-rewind--staged-file-p remote-file))))
      (delete-directory root t)))

  :doc "does not expose client environment variables to target Git"
  (let* ((host "staged-file-environment-host")
         (root (file-name-as-directory
                (make-temp-file "mevedel-staged-environment-" t)))
         (local-file (file-name-concat root "staged.el"))
         (remote-file (format "/mevedelmock:%s:%s" host local-file))
         (process-environment
          (cons "MEVEDEL_CLIENT_SECRET=do-not-forward" process-environment)))
    (write-region "staged\n" nil local-file nil 'silent)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (&rest _)
                       (should-not (getenv "MEVEDEL_CLIENT_SECRET"))
                       "/usr/bin/git"))
                    ((symbol-function 'process-file)
                     (lambda (&rest _)
                       (should-not (getenv "MEVEDEL_CLIENT_SECRET"))
                       1)))
            (should
             (mevedel-session-rewind--staged-file-p remote-file))))
      (delete-directory root t))))


(mevedel-deftest mevedel-session-rewind--rewind-impact
  (:doc "lists the complete discarded prompt suffix in chronological order")
  (let* ((session
          (mevedel-session--create
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
                'mevedel-session-rewind--detached-child-count)
               (lambda (&rest _) 0))
              ((symbol-function 'display-buffer) #'ignore))
      (setq impact
            (mevedel-session-rewind--rewind-impact session target nil))
      (should
       (equal '("Ordinary chat" "Directive two")
              (mapcar (lambda (entry) (plist-get entry :preview))
                      (plist-get impact :discarded-prompts))))
      (mevedel-session-rewind--render-rewind-impact session impact)
      (with-current-buffer "*mevedel-rewind-impact*"
        (let ((text (buffer-string)))
          (should (string-match-p "Discarded session events" text))
          (should (< (string-match "Ordinary chat" text)
                     (string-match "Directive two" text))))))))


(mevedel-deftest mevedel-session-rewind-rewind-checkpoint
  (:doc "resumes a cold session and rewinds before the checkpoint turn")
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "checkpoint" :root "/tmp"
                     :name "checkpoint"))
         (session (mevedel-session--create :session-id "cold-session"))
         (buffer (generate-new-buffer " *checkpoint-rewind*"))
         resumed rewound)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (cl-letf
              (((symbol-function 'mevedel-session-persistence-resume-id)
                (lambda (owner session-id)
                  (setq resumed (list owner session-id))
                  buffer))
               ((symbol-function
                 'mevedel-session-rewind--prompt-candidates)
                (lambda (_)
                  '(("S1 T4" . (:segment 1 :turn 4 :cum-turn 4
                                  :fork-point-id "point")))))
               ((symbol-function 'mevedel-session-rewind-rewind)
                (lambda (selected target &optional boundary)
                  (setq rewound (list selected target boundary))
                  t)))
            (should
             (mevedel-session-rewind-rewind-checkpoint
              workspace '(:session-id "cold-session" :turn 4))))
          (should (equal (list workspace "cold-session") resumed))
          (should (eq buffer (car rewound)))
          (should (= 4 (plist-get (cadr rewound) :cum-turn)))
          ;; Rewinding before an implementation discards that attempt.
          (should (eq 'before (nth 2 rewound))))
      (kill-buffer buffer))))


(mevedel-deftest mevedel-rewind (:quiet t)
  ,test
  (test)
  :doc "errors when no current session"
  (with-temp-buffer
    (let ((mevedel--session nil))
      (should-error (mevedel-rewind) :type 'user-error)))
  :doc "refuses both pending-input categories before the picker"
  (dolist (category '(steering follow-up))
    (with-temp-buffer
      (let ((session (mevedel-session--create :name "rewind"))
            picked)
        (setq-local mevedel--session session)
        (mevedel-session-enqueue-pending-input
         session category '(:input "keep me"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (setq picked t))))
          (let ((err (should-error (mevedel-rewind) :type 'user-error)))
            (should (string-match-p "Pending Inputs"
                                    (error-message-string err)))
            (should (string-match-p "C-c C-q"
                                    (error-message-string err)))
            (should-not picked))))))
  :doc "refuses before the picker while executions remain live"
  (let ((buffer (generate-new-buffer " *execution-rewind*"))
        (session (mevedel-session--create :name "rewind")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (cl-letf (((symbol-function 'mevedel-execution-session-live-p)
                     (lambda (_session) t)))
            (let ((err (should-error (mevedel-rewind) :type 'user-error)))
              (should (string-match-p
                       "/ps or /stop" (error-message-string err))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))))
  :doc "refuses without changing a tree that has an active agent turn"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :name "rewind"))
           (record
            (mevedel-agent-record--create :activity 'running)))
      (setq-local mevedel--session session)
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (let ((err (should-error (mevedel-rewind) :type 'user-error)))
        (should (string-match-p
                 "Interrupt active agent turns"
                 (error-message-string err))))
      (should (eq record
                  (cdr (assoc
                        "/root/worker"
                        (mevedel-session-agent-registry session)))))))
  :doc "errors when request in flight"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (setq-local mevedel--session session)
                (let ((mevedel--current-request 'placeholder))
                  (should-error (mevedel-rewind) :type 'user-error)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "errors when no recorded prompts"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (setq-local mevedel--session session)
                (let ((mevedel--current-request nil))
                  (should-error (mevedel-rewind) :type 'user-error)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "selecting the latest turn still confirms discarding that turn"
  (let* ((session
          (mevedel-session--create
           :name "rewind" :turn-count 1
           :prompt-index
           '((1 . ((:turn 1 :cum-turn 1 :fork-point-id "point"))))))
         (buffer (generate-new-buffer " *empty-rewind*"))
         (target '(:segment 1 :turn 1 :cum-turn 1
                   :fork-point-id "point"))
         confirmed committed started)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (cl-letf
              (((symbol-function
                 'mevedel-session-rewind-assert-stable-source)
                #'ignore)
               ((symbol-function
                 'mevedel-session-rewind-restore-plan)
                (lambda (&rest _) nil))
               ((symbol-function
                 'mevedel-session-rewind--detached-child-count)
                (lambda (&rest _) 0))
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _) (setq confirmed t)))
               ((symbol-function
                 'mevedel-session-rewind--commit-rewind)
                (lambda (&rest _) (setq committed t)))
               ((symbol-function 'mevedel--run-session-start-hooks)
                (lambda (&rest _) (setq started t))))
            (mevedel-session-rewind-rewind buffer target))
          (should confirmed)
          (should committed)
          (should started))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "rechecks remote mutation authority after impact confirmation"
  (let* ((target '(:segment 1 :turn 1 :cum-turn 1
                   :fork-point-id "point"))
         (session
          (mevedel-session--create
           :name "rewind" :turn-count 1
           :prompt-index `((1 . (,target)))))
         (buffer (generate-new-buffer " *remote-authority-rewind*"))
         (authority-checks 0)
         confirmed)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (cl-letf
              (((symbol-function
                 'mevedel-session-rewind-assert-stable-source)
                #'ignore)
               ((symbol-function
                 'mevedel-session-rewind--prompt-candidates)
                (lambda (_) (list (cons "target" target))))
               ((symbol-function 'completing-read)
                (lambda (&rest _) "target"))
               ((symbol-function
                 'mevedel-session-artifacts-assert-mutation-authority)
                (lambda (&rest _)
                  (cl-incf authority-checks)
                  (when confirmed
                    (user-error "Injected lost remote lease"))))
               ((symbol-function
                 'mevedel-session-rewind-restore-plan)
                (lambda (&rest _) nil))
               ((symbol-function
                 'mevedel-session-rewind--detached-child-count)
                (lambda (&rest _) 0))
               ((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _)
                  (setq confirmed t)))
               ((symbol-function
                 'mevedel-session-rewind--commit-rewind)
                (lambda (&rest _)
                  (ert-fail "Rewind committed without mutation authority"))))
            (should-error (mevedel-rewind) :type 'user-error))
          (should (= 2 authority-checks)))
      (when-let* ((impact (get-buffer "*mevedel-rewind-impact*")))
        (kill-buffer impact))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "rewinds first-turn modifications, creations, and deletions to pre-turn state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buffer (generate-new-buffer "*test-first-turn-rewind*"))
               (modified (file-name-concat tempdir "modified.el"))
               (created (file-name-concat tempdir "created.el"))
               (deleted (file-name-concat tempdir "deleted.el")))
          (unwind-protect
              (with-current-buffer buffer
                (org-mode)
                (setq-local mevedel--session session)
                (write-region "before-modify" nil modified nil 'silent)
                (write-region "before-delete" nil deleted nil 'silent)
                (mevedel-request-begin session)
                (let ((checkpoint
                       (mevedel-request-file-snapshots
                        mevedel--current-request)))
                  (puthash modified "before-modify" checkpoint)
                  (puthash created nil checkpoint)
                  (puthash deleted "before-delete" checkpoint))
                (write-region "after-modify" nil modified nil 'silent)
                (write-region "after-create" nil created nil 'silent)
                (delete-file deleted)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 1)
                (mevedel-session-artifacts-save session buffer t)
                (let ((target
                       (copy-sequence
                        (cdar
                         (mevedel-session-rewind--prompt-candidates
                          session)))))
                  (mevedel-request-end)
                  (cl-letf (((symbol-function 'display-buffer) #'ignore)
                            ((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) t))
                            ((symbol-function
                              'mevedel--run-session-start-hooks)
                             #'ignore))
                    (should
                     (mevedel-session-rewind-rewind buffer target))))
                (should (= 0 (mevedel-session-turn-count session)))
                (should-not (string-match-p "First prompt" (buffer-string)))
                (should
                 (equal "before-modify"
                        (mevedel-session-artifacts--file-text modified)))
                (should-not (file-exists-p created))
                (should
                 (equal "before-delete"
                        (mevedel-session-artifacts--file-text deleted))))
            (test-mevedel-session-persistence--release-and-kill
             buffer session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "a rolled-back Rewind keeps another session's note activity and deletions"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer "*test-rollback-other-session*"))
           (source-file (file-name-concat tempdir "source.el"))
           (change '(:id "other-2" :session-id "other-session" :turn 2))
           (restore (symbol-function 'mevedel-session-rewind-execute-restore))
           (stored (lambda (uuid key)
                     (catch 'found
                       (dolist (file (plist-get (plist-get (mevedel-instruction-store--read
                                                            workspace)
                                                           :data)
                                                :files))
                         (dolist (instruction (plist-get (cdr file) :instructions))
                           (let ((properties (plist-get instruction :properties)))
                             (when (equal uuid (plist-get properties 'mevedel-uuid))
                               (throw 'found (plist-get properties key)))))))))
           source notes theirs doomed)
      (unwind-protect
          (progn
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "AAAAA\nBBBBB\nCCCCC\n" nil source-file nil 'silent)
            (setq source (find-file-noselect source-file))
            (with-current-buffer source
              (setq-local mevedel--workspace workspace)
              (dolist (line '(1 2 3))
                (goto-char (point-min))
                (forward-line (1- line))
                (push (mevedel-note-create (format "line %d" line)) notes)))
            (setq theirs (overlay-get (nth 1 notes) 'mevedel-uuid)
                  doomed (overlay-get (nth 0 notes) 'mevedel-uuid))
            (dolist (note (list (nth 1 notes) (nth 0 notes)))
              (mevedel-note-mark-sent (list note) "other-session"))
            (mevedel-instruction-store-sync workspace)
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (mevedel-request-begin session)
              (insert "First prompt\n")
              (insert (propertize "First reply.\n" 'gptel 'response))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-artifacts-save session buffer t)
              (let ((target (copy-sequence
                             (cdar (mevedel-session-rewind--prompt-candidates
                                    session)))))
                (mevedel-request-end)
                (cl-letf (((symbol-function 'display-buffer) #'ignore)
                          ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                          ((symbol-function 'mevedel--run-session-start-hooks)
                           #'ignore)
                          ((symbol-function 'mevedel-session-rewind-execute-restore)
                           (lambda (&rest args)
                             (prog1 (apply restore args)
                               ;; Another session prepares a change and deletes
                               ;; a note meanwhile; then this Rewind fails and
                               ;; rolls back.
                               (let ((note (mevedel--instruction-with-uuid theirs workspace)))
                                 (overlay-put note 'mevedel-note-prepared-change change)
                                 (mevedel-instruction-store-touch workspace))
                               (mevedel-note-delete
                                (mevedel--instruction-with-uuid doomed workspace))
                               (error "Injected Rewind failure")))))
                  (should-error (mevedel-session-rewind-rewind buffer target)))))
            (should (= 1 (mevedel-session-turn-count session)))
            (should (equal change
                           (overlay-get (mevedel--instruction-with-uuid theirs workspace)
                                        'mevedel-note-prepared-change)))
            (should-not (mevedel--instruction-with-uuid doomed workspace))
            ;; The held-back writes reach the store once the Rewind is over.
            (mevedel-instruction-store-flush)
            (should (equal change (funcall stored theirs 'mevedel-note-prepared-change)))
            (should (eq 'closed (funcall stored doomed 'mevedel-note-state))))
        (when-let* ((impact (get-buffer "*mevedel-rewind-impact*")))
          (kill-buffer impact))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "keeps the selected turn and discards only what follows"
  ;; The view's Rewind names a response the user is looking at, so that
  ;; turn is the last one kept and only later turns go.
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (data-buf (generate-new-buffer " *test-keep-turn-data*")))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local mevedel--workspace workspace)
              (setq-local gptel-backend
                          (test-mevedel-session-persistence--agent-backend))
              (setq-local gptel-model 'test-model)
              (gptel-mode 1)
              (mevedel-session-artifacts-save session data-buf)
              (goto-char (point-max))
              (insert "First prompt\n")
              (let ((start (point)))
                (insert "First reply.\n")
                (put-text-property start (point) 'gptel 'response))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-artifacts-save session data-buf t)
              (goto-char (point-max))
              (insert "Second prompt\n")
              (let ((start (point)))
                (insert "Second reply.\n")
                (put-text-property start (point) 'gptel 'response))
              (setf (mevedel-session-turn-count session) 2)
              (mevedel-session-artifacts-save session data-buf t))
            (let* ((target
                    (copy-sequence
                     (cdr (cl-find-if
                           (lambda (entry)
                             (= 1 (plist-get (cdr entry) :cum-turn)))
                           (mevedel-session-rewind--prompt-candidates
                            session))))))
              (should target)
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'mevedel--run-session-start-hooks)
                         #'ignore))
                (should (mevedel-session-rewind-rewind
                         data-buf target 'after)))
              ;; Turn 1 survives with its reply; turn 2 is gone.
              (should (= 1 (mevedel-session-turn-count session)))
              (with-current-buffer data-buf
                (should (string-match-p "First prompt" (buffer-string)))
                (should (string-match-p "First reply" (buffer-string)))
                (should-not
                 (string-match-p "Second prompt" (buffer-string)))
                (should-not
                 (string-match-p "Second reply" (buffer-string))))))
        (test-mevedel-session-persistence--release-and-kill
         data-buf session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))

  :doc "rewinds before a saved first gptel turn without exposing metadata"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (data-buf (generate-new-buffer " *test-gptel-rewind-data*"))
           (view-buf (generate-new-buffer " *test-gptel-rewind-view*"))
           (draft "> keep this draft\nsecond line"))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local mevedel--workspace workspace)
              (setq-local gptel-backend
                          (test-mevedel-session-persistence--agent-backend))
              (setq-local gptel-model 'test-model)
              (gptel-mode 1)
              (mevedel-session-artifacts-save session data-buf)
              (goto-char (point-max))
              (insert "Implement hello world\n")
              (dotimes (index 40)
                (let ((start (point)))
                  (insert (format "model span %02d\n" index))
                  (put-text-property
                   start (point) 'gptel
                   (if (= (% index 2) 0) 'response 'ignore))))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-artifacts-save session data-buf t))
            (mevedel-view--setup view-buf data-buf)
            (with-current-buffer view-buf
              (mevedel-view--full-rerender)
              (mevedel-view-test--insert-composer-draft draft 4))
            (let ((target
                   (copy-sequence
                    (cdar
                     (mevedel-session-rewind--prompt-candidates
                      session)))))
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'mevedel--run-session-start-hooks)
                         #'ignore))
                (should
                 (mevedel-session-rewind-rewind data-buf target))))
            (should (= 0 (mevedel-session-turn-count session)))
            (should-not
             (cdr (assoc 1 (mevedel-session-prompt-index session))))
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
                   (mevedel-session-codec-read
                    (mevedel-session-artifacts-sidecar-path
                     (mevedel-session-save-path session)))))
              (should-not (cdr (assoc 1 (plist-get sidecar :prompt-index)))))
            ;; The rewind schedules the view's rebuild.
            (mevedel-view--flush-scheduled-render view-buf t)
            (with-current-buffer view-buf
              (should (equal draft (mevedel-view--input-text)))
              (should-not (string-match-p ":PROPERTIES:" (buffer-string)))
              (should-not (string-match-p "^You$" (buffer-string))))
            (should-not (get-buffer "*mevedel-rewind-impact*")))
        (when-let* ((impact (get-buffer "*mevedel-rewind-impact*")))
          (kill-buffer impact))
        (when (buffer-live-p view-buf)
          (kill-buffer view-buf))
        (test-mevedel-session-persistence--release-and-kill
         data-buf session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))
  :doc "leaves no transcript residue when rewinding before the first turn"
  ;; The transcript's leading newlines carry the discarded turns'
  ;; properties, and the folded property drawer starts at point-min.  Org
  ;; walks one character back from a folded region on every change, so a
  ;; wholesale buffer replacement there used to signal `beginning-of-buffer'
  ;; and abort the Rewind; what survived it projected a turn with no
  ;; content and hid the retained lines behind an invisibility ellipsis.
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (data-buf (generate-new-buffer " *test-rewind-residue-data*"))
           (view-buf (generate-new-buffer " *test-rewind-residue-view*")))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local mevedel--workspace workspace)
              (setq-local gptel-backend
                          (test-mevedel-session-persistence--agent-backend))
              (setq-local gptel-model 'test-model)
              (gptel-mode 1)
              (mevedel-session-artifacts-save session data-buf)
              (goto-char (point-max))
              (let ((start (point)))
                (insert "\n")
                (put-text-property start (point) 'gptel
                                   (cons 'tool "call-residue")))
              (let ((start (point)))
                (insert "\n")
                (put-text-property start (point) 'gptel 'mevedel-hook-audit)
                (put-text-property start (point) 'mevedel-hook-audit t)
                (put-text-property start (point) 'invisible t))
              (insert "First prompt\n")
              (let ((start (point)))
                (insert "assistant text\n")
                (put-text-property start (point) 'gptel 'response))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-artifacts-save session data-buf t))
            (mevedel-view--setup view-buf data-buf)
            (with-current-buffer view-buf
              (mevedel-view--full-rerender))
            (let ((target
                   (copy-sequence
                    (cdar (mevedel-session-rewind--prompt-candidates
                           session)))))
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                        ((symbol-function 'mevedel--run-session-start-hooks)
                         #'ignore))
                (should (mevedel-session-rewind-rewind data-buf target))))
            (with-current-buffer data-buf
              (should (string-blank-p
                       (buffer-substring-no-properties
                        (point-min) (point-max))))
              (should-not (buffer-modified-p))
              (should-not
               (mevedel-transcript-restore-properties-present-p
                (point-min) (point-max)))
              (should-not (text-property-not-all (point-min) (point-max)
                                                 'invisible nil))
              (should-not
               (mevedel-view--group-into-turns
                (cl-remove-if-not
                 (lambda (segment) (memq (car segment) '(ignored tool)))
                 (mevedel-transcript-segments (point-min) (point-max)))
                data-buf)))
            (mevedel-view--flush-scheduled-render view-buf t)
            (with-current-buffer view-buf
              (should-not (string-match-p "^Assistant$" (buffer-string)))))
        (when-let* ((impact (get-buffer "*mevedel-rewind-impact*")))
          (kill-buffer impact))
        (when (buffer-live-p view-buf)
          (kill-buffer view-buf))
        (test-mevedel-session-persistence--release-and-kill data-buf session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))

  :doc "rewinds a later turn while preserving the preceding turn"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buffer (generate-new-buffer "*test-later-turn-rewind*"))
               (path (file-name-concat tempdir "serial.el")))
          (unwind-protect
              (with-current-buffer buffer
                (org-mode)
                (setq-local mevedel--session session)
                (write-region "zero" nil path nil 'silent)
                (mevedel-request-begin session)
                (puthash path "zero"
                         (mevedel-request-file-snapshots
                          mevedel--current-request))
                (write-region "one" nil path nil 'silent)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 1)
                (mevedel-session-artifacts-save session buffer t)
                (mevedel-request-end)
                (mevedel-request-begin session)
                (puthash path "one"
                         (mevedel-request-file-snapshots
                          mevedel--current-request))
                (write-region "two" nil path nil 'silent)
                (insert "Second prompt\n")
                (insert (propertize "Second reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 2)
                (mevedel-session-artifacts-save session buffer t)
                (let* ((candidates
                        (mevedel-session-rewind--prompt-candidates
                         session))
                       (target (copy-sequence (cdar (last candidates)))))
                  (mevedel-request-end)
                  (cl-letf (((symbol-function 'display-buffer) #'ignore)
                            ((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) t))
                            ((symbol-function
                              'mevedel--run-session-start-hooks)
                             #'ignore))
                    (should
                     (mevedel-session-rewind-rewind buffer target))))
                (should (= 1 (mevedel-session-turn-count session)))
                (should (string-match-p "First reply" (buffer-string)))
                (should-not (string-match-p "Second prompt"
                                            (buffer-string)))
                (should
                 (equal "one"
                        (mevedel-session-artifacts--file-text path))))
            (test-mevedel-session-persistence--release-and-kill
             buffer session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "keeps Rewind reachable and discloses known checkpoint gaps"
  (let* ((session
          (mevedel-session--create
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
          (setq-local mevedel--session session)
          (cl-letf
              (((symbol-function
                 'mevedel-session-rewind-assert-stable-source)
                #'ignore)
               ((symbol-function
                 'mevedel-session-rewind--detached-child-count)
                (lambda (&rest _) 0))
               ((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _)
                  (setq confirmed t
                        impact-text
                        (with-current-buffer "*mevedel-rewind-impact*"
                          (buffer-string)))
                  nil)))
            (mevedel-session-rewind-rewind buffer target))
          (should confirmed)
          (should (string-match-p "Checkpoint coverage: incomplete"
                                  impact-text))
          (should (string-match-p "/unreadable" impact-text))
          (should (string-match-p "capture failed" impact-text))
          (should-not (get-buffer "*mevedel-rewind-impact*")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "main-chat coverage groups repeated tools and leaves real file gaps visible"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *grouped-coverage*"))
           (file (file-name-concat tempdir "source.txt"))
           impact)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session mevedel--workspace workspace)
            (write-region "unchanged\n" nil file nil 'silent)
            (dotimes (index 30)
              (let* ((turn (1+ index))
                     (effects (append '(("Bash" . "shell changes are not captured"))
                                      (when (memq turn '(5 25))
                                        '(("Eval" . "evaluation effects are not captured"))))))
                (mevedel-request-begin session)
                (setf (mevedel-request-untracked-effects mevedel--current-request) effects)
                (when (= turn 15)
                  (puthash file '(:gap "capture failed")
                           (mevedel-request-file-snapshots mevedel--current-request)))
                (goto-char (point-max))
                (let ((start (point)))
                  (insert (format "Prompt %d\n" turn))
                  (put-text-property start (point) 'gptel nil))
                (insert (propertize "Reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) turn)
                (mevedel-session-artifacts-save session buffer t)
                (mevedel-request-end)))
            (cl-letf (((symbol-function 'display-buffer) #'ignore)
                      ((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _)
                         (or (seq-find (lambda (choice) (string-match-p "T1 " choice))
                                       (all-completions "" choices))
                             (ert-fail (format "First turn missing: %S" (all-completions "" choices))))))
                      ((symbol-function 'yes-or-no-p)
                       (lambda (&rest _)
                         (setq impact (with-current-buffer "*mevedel-rewind-impact*" (buffer-string)))
                         nil)))
              (mevedel-rewind))
            (should (string-search "incomplete (3 known gaps)" impact))
            (should (string-match-p "gap +Bash (30 turns: shell changes are not captured)" impact))
            (should (string-match-p "gap +Eval (2 turns: evaluation effects are not captured)" impact))
            (should (string-search (concat file " (capture failed)") impact))
            (with-temp-buffer
              (insert impact)
              (should (= 1 (how-many "^gap +Bash " (point-min) (point-max))))
              (should (= 1 (how-many "^gap +Eval " (point-min) (point-max)))))
            (should (= 30 (mevedel-session-turn-count session)))
            (should (equal "unchanged\n" (mevedel-test--read-file file))))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))
  :doc "retains the impact buffer when the commit fails"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *failed-rewind*"))
           (missing (file-name-concat tempdir "missing.el")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "First prompt\n")
            (insert (propertize "First reply.\n" 'gptel 'response))
            (setf (mevedel-session-turn-count session) 1)
            (mevedel-session-artifacts-save session buffer t)
            (let ((target
                   (copy-sequence
                    (cdar
                     (mevedel-session-rewind--prompt-candidates
                      session)))))
              (setf (mevedel-session-file-snapshots session)
                    `((1 . ((,missing . (:backup-name nil
                                         :pre-backup-name "missing"
                                         :version 1))))))
              (test-mevedel-rewind--publish-backups session "missing" "original")
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _)
                           ;; Lose the committed pre-image behind the plan.
                           (delete-file
                            (plist-get
                             (cdr (assoc "file-history/missing"
                                         (plist-get
                                          (mevedel-session-publication session)
                                          :artifacts)))
                             :published))
                           t)))
                (should-error
                 (mevedel-session-rewind-rewind buffer target)))
              (should (get-buffer "*mevedel-rewind-impact*"))
              (should (= 1 (mevedel-session-turn-count session)))
              (should (string-match-p "First prompt" (buffer-string)))))
        (when-let* ((impact (get-buffer "*mevedel-rewind-impact*")))
          (kill-buffer impact))
        (test-mevedel-session-persistence--release-and-kill
         buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))
  :doc "restores a deleted source and holds back concurrent instruction-store writes"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((session nil)
          (buffer nil)
          (source-buffer nil)
          (source-file (file-name-concat tempdir "source.el"))
          (stored-notes
           (lambda (owner)
             (sort (cl-loop
                    for file in (plist-get
                                 (plist-get (mevedel-instruction-store--read owner)
                                            :data)
                                 :files)
                    append (cl-loop
                            for instruction in (plist-get (cdr file) :instructions)
                            collect (plist-get (plist-get instruction :properties)
                                               'mevedel-note-text)))
                   #'string<))))
      (unwind-protect
          (progn
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "EARLY\nLATER\n" nil source-file nil 'silent)
            (setq source-buffer (find-file-noselect source-file))
            (with-current-buffer source-buffer
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (mevedel-note-create "Early"))
            (setq session (mevedel-session-create "main" workspace)
                  buffer (generate-new-buffer "*test-note-rewind*"))
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (insert "First note turn\n")
              (insert (propertize "First reply.\n" 'gptel 'response))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-artifacts-save session buffer t))
            (let* ((notes (funcall stored-notes workspace))
                   target)
              (should (equal '("Early") notes))
              (with-current-buffer buffer
                (mevedel-request-begin session)
                (puthash source-file "EARLY\nLATER\n"
                         (mevedel-request-file-snapshots
                          mevedel--current-request))
                (delete-file source-file)
                (goto-char (point-max))
                (insert "Second note turn\n")
                (insert (propertize "Second reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 2)
                (mevedel-session-artifacts-save session buffer t)
                (setq target
                      (copy-sequence
                       (cdr
                        (cl-find-if
                         (lambda (entry)
                           (= 2 (plist-get (cdr entry) :cum-turn)))
                         (mevedel-session-rewind--prompt-candidates
                          session)))))
                (mevedel-request-end)
                (goto-char (point-max))
                (insert "Ordinary later chat\n")
                (insert (propertize "Later reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 3)
                (mevedel-session-artifacts-save session buffer t))
              (when (buffer-live-p source-buffer)
                (kill-buffer source-buffer)
                (setq source-buffer nil))
              ;; Meanwhile another workspace and another session of this
              ;; workspace each write a note; this session's Rewind must
              ;; keep neither from its store.
              (let* ((other-root (file-name-as-directory
                                  (make-temp-file "mevedel-other-ws-" t)))
                     (other (mevedel-workspace-get-or-create
                             'file "other-ws" other-root "other-ws"))
                     (other-file (file-name-concat other-root "other.el"))
                     (same-file (file-name-concat tempdir "same.el"))
                     (restore (symbol-function
                               'mevedel-session-rewind-execute-restore))
                     other-buffer same-buffer held-back)
                (mevedel-workspace-identity-ensure other-root)
                (write-region "(defun other () t)\n" nil other-file nil 'silent)
                (write-region "(defun same () t)\n" nil same-file nil 'silent)
                (unwind-protect
                    (progn
                      (cl-letf (((symbol-function 'display-buffer) #'ignore)
                                ((symbol-function 'yes-or-no-p)
                                 (lambda (&rest _) t))
                                ((symbol-function 'mevedel--run-session-start-hooks)
                                 #'ignore)
                                ((symbol-function
                                  'mevedel-session-rewind-execute-restore)
                                 (lambda (&rest args)
                                   (prog1 (apply restore args)
                                     (setq other-buffer (find-file-noselect other-file))
                                     (with-current-buffer other-buffer
                                       (setq-local mevedel--workspace other)
                                       (goto-char (point-min))
                                       (mevedel-note-create "elsewhere"))
                                     ;; Another session of this workspace
                                     ;; writes a note and syncs the store.
                                     (setq same-buffer (find-file-noselect same-file))
                                     (with-current-buffer same-buffer
                                       (setq-local mevedel--workspace workspace)
                                       (goto-char (point-min))
                                       (mevedel-note-create "same workspace"))
                                     (mevedel-instruction-store-sync workspace)
                                     ;; Control: the Rewind really held it back.
                                     (setq held-back
                                           (not (member "same workspace"
                                                        (funcall stored-notes
                                                                 workspace))))))))
                        (should
                         (mevedel-session-rewind-rewind buffer target)))
                      (should other-buffer)
                      (should held-back)
                      (should (memq workspace mevedel-instruction-store--dirty))
                      (mevedel-instruction-store-sync other)
                      (should
                       (equal '("elsewhere")
                              (funcall stored-notes other))))
                  (when (buffer-live-p other-buffer)
                    (with-current-buffer other-buffer (set-buffer-modified-p nil))
                    (kill-buffer other-buffer))
                  (mevedel--clear-instruction-state other)
                  (delete-directory other-root t)))
              (should (= 1 (mevedel-session-turn-count session)))
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
               (plist-get (plist-get (mevedel-instruction-store--read workspace)
                                     :data)
                          :rewinds))
              ;; The other session's activity, held back during the
              ;; Rewind, reaches the store once the Rewind is over.
              (mevedel-instruction-store-flush)
              (should (equal '("Early" "same workspace")
                             (funcall stored-notes workspace)))
              (when-let* ((same-buffer (find-buffer-visiting
                                        (file-name-concat tempdir "same.el"))))
                (with-current-buffer same-buffer (set-buffer-modified-p nil))
                (kill-buffer same-buffer))))
        (when (and buffer (buffer-live-p buffer))
          (test-mevedel-session-persistence--release-and-kill buffer session))
        (setq source-buffer (or source-buffer
                                (find-buffer-visiting source-file)))
        (when (buffer-live-p source-buffer)
          (with-current-buffer source-buffer (set-buffer-modified-p nil))
          (kill-buffer source-buffer))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "cancelling the impact confirmation changes no transcript or file state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-rewind-cancel*"))
               (path (file-name-concat tempdir "tracked.el"))
               target)
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 1)
                (mevedel-session-artifacts-save session buf t)
                (setq target
                      (copy-sequence
                       (cdar
                         (mevedel-session-rewind--prompt-candidates
                         session))))
                (let ((backup
                       (mevedel-session-artifacts--file-history-backup-name "first")))
                  (setf (mevedel-session-file-snapshots session)
                        `((1 . ((,path . (:backup-name ,backup
                                        :pre-backup-name ,backup
                                        :version 1))))))
                  (test-mevedel-rewind--publish-backups
                   session backup "first"))
                (goto-char (point-max))
                (insert "Second prompt\n")
                (insert (propertize "Second reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 2)
                (mevedel-session-artifacts-save session buf t)
                (let ((backup
                       (mevedel-session-artifacts--file-history-backup-name "second")))
                  (setf (mevedel-session-file-snapshots session)
                        (append
                         (mevedel-session-file-snapshots session)
                         `((2 . ((,path . (:backup-name ,backup
                                         :pre-backup-name
                                         ,(mevedel-session-artifacts--file-history-backup-name
                                           "first")
                                         :version 2)))))))
                  (test-mevedel-rewind--publish-backups
                   session backup "second"))
                (write-region "second" nil path nil 'silent)
                (let* ((before-buffer (buffer-string))
                       (session-id (mevedel-session-session-id session))
                       (save-path (mevedel-session-save-path session))
                       starts ended
                       (before-sidecar
                        (mevedel-session-artifacts-read-artifact
                         session "session.meta.el" t)))
                  (cl-letf (((symbol-function 'display-buffer) #'ignore)
                            ((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) nil))
                            ((symbol-function
                              'mevedel--run-session-start-hooks)
                             (lambda (source)
                               (push source starts))))
                    (mevedel-session-rewind-rewind buf target))
                  (should-not starts)
                  (should-not (get-buffer "*mevedel-rewind-impact*"))
                  (should (equal before-buffer (buffer-string)))
                  (should
                   (equal
                    before-sidecar
                    (mevedel-session-artifacts-read-artifact
                     session "session.meta.el" t)))
                  (should
                   (equal "second"
                          (with-temp-buffer
                            (insert-file-contents path)
                            (buffer-string))))
                  (should (= 2 (mevedel-session-turn-count session)))
                  (should (equal buffer-file-name
                                 (mevedel-session-artifacts-segment-path
                                  (mevedel-session-save-path session) 1)))
                  (let (confirmation)
                    (cl-letf (((symbol-function 'display-buffer) #'ignore)
                              ((symbol-function 'yes-or-no-p)
                               (lambda (prompt)
                                 (setq confirmation prompt)
                                 t))
                              ((symbol-function
                                'mevedel--run-session-start-hooks)
                               (lambda (source)
                                 (push source starts)
                                 (setf
                                  (mevedel-session-hook-context-pending session)
                                  `((:event SessionStart
                                     :body ,source)))))
                              ((symbol-function
                                'mevedel--run-session-end-hooks)
                               (lambda ()
                                 (setq ended t))))
                      (mevedel-session-rewind-rewind buf target))
                    ;; Lease-owned sessions keep both redo layers.
                    (should (string-match-p "conversation and captured-file redo"
                                            confirmation)))
                  (should (equal '("rewind") starts))
                  (should-not ended)
                  (let* ((context
                          (mevedel-session-hook-context-pending session))
                         (submission
                          (mevedel-prompt-submission-create
                           :session session :context-entries context)))
                    (mevedel-prompt-submission-commit submission)
                    (should-not
                     (mevedel-session-hook-context-pending session)))
                  (should (equal session-id
                                 (mevedel-session-session-id session)))
                  (should (equal save-path
                                 (mevedel-session-save-path session)))
                  (should (= 0 (mevedel-session-turn-count session)))
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
                               (mevedel-session-artifacts-read-artifact
                                session "session.meta.el" t)))))
                    (should (= 0 (plist-get sidecar :total-turn-count)))
                    (should-not (plist-get sidecar
                                           :forked-from-session-id)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-rewind-materialize-publication ()
  ,test
  (test)
  :doc "writes committed artifacts into a staging root that does not exist yet"
  (let* ((tempdir (make-temp-file "mevedel-materialize-" t))
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
         (session (mevedel-session--create
                   :authority-mode 'portable
                   :save-path (file-name-as-directory tempdir)
                   :publication publication)))
    (unwind-protect
        (progn
          (write-region content nil published nil 'silent)
          ;; Rewind hands over `<temporary-root>/staging', which nothing
          ;; creates before materialization.
          (should-not (file-directory-p staging))
          (mevedel-session-rewind-materialize-publication
           session publication staging)
          (should (equal content
                         (with-temp-buffer
                           (insert-file-contents
                            (file-name-concat staging logical))
                           (buffer-string)))))
      (delete-directory tempdir t)))
  :doc "refuses an artifact whose logical path leaves the staging root"
  (let* ((tempdir (make-temp-file "mevedel-materialize-escape-" t))
         (staging (file-name-concat tempdir "staging"))
         (publication
          (list :head ".publications/0001/manifest.el"
                :artifacts (list (cons "../outside.txt"
                                       (list :published (file-name-concat tempdir "x")
                                             :sha256 "0")))))
         (session (mevedel-session--create
                   :authority-mode 'portable
                   :save-path (file-name-as-directory tempdir)
                   :publication publication)))
    (unwind-protect
        (progn
          (should-error (mevedel-session-rewind-materialize-publication
                         session publication staging))
          (should-not (file-exists-p (file-name-concat tempdir "outside.txt"))))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-rewind--commit-rewind ()
  ,test
  (test)
  :doc "commits one replacement head without moving remote control state"
  (let* ((host "remote-rewind-commit")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-rewind-" t)))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?b))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (target '(:segment 1 :turn 1 :cum-turn 1
                             :fork-point-id "remote-point")))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*remote-rewind*" session))
              (setf (mevedel-session-publication session)
                    (mevedel-session-publication-read session-dir)
                    (mevedel-session-turn-count session) 1
                    (mevedel-session-prompt-index session)
                    `((1 . (,target))))
              (setq buffer (generate-new-buffer " *remote-rewind-live*"))
              (with-current-buffer buffer
                (org-mode)
                (insert "Original transcript\n")
                (setq-local mevedel--session session)
                (setq buffer-file-name segment))
              (let ((generation
                     (plist-get (mevedel-session-lease session) :generation))
                    (old-head
                     (plist-get (mevedel-session-publication session) :head)))
                (cl-letf
                    (((symbol-function
                       'mevedel-session-rewind--stage-rewind)
                      (lambda (_session candidate actual-target staging-path
                               staging-buffer &rest _)
                        (make-directory staging-path t)
                        (with-current-buffer staging-buffer
                          (erase-buffer)
                          (insert "Rewound transcript\n")
                          (setq buffer-file-name
                                (mevedel-session-artifacts-segment-path
                                 staging-path
                                 (plist-get actual-target :segment)))
                          (write-region (point-min) (point-max)
                                        buffer-file-name nil 'silent))
                        candidate))
                     ((symbol-function
                       'mevedel-session-rewind-restore-plan)
                      (lambda (&rest _) nil))
                     ((symbol-function
                       'mevedel-session-rewind--refresh-restored-buffers)
                      #'ignore))
                  (should
                   (mevedel-session-rewind--commit-rewind
                    session buffer target nil)))
                (should
                 (equal generation
                        (plist-get (mevedel-session-lease session)
                                   :generation)))
                (should-not
                 (equal old-head
                        (plist-get (mevedel-session-publication session)
                                   :head)))
                (should
                 (file-exists-p (file-name-concat session-dir old-head)))
                (should
                 (file-directory-p
                  (file-name-concat session-dir ".publications")))
                (should
                 (equal "Rewound transcript\n"
                        (mevedel-session-artifacts-read-artifact
                         session "segment-0001.chat.org" t)))
                (should (= 0 (mevedel-session-turn-count session)))
                (with-current-buffer buffer
                  (should (equal "Rewound transcript\n" (buffer-string))))))))
      (when (buffer-live-p buffer)
        (let ((session (buffer-local-value 'mevedel--session buffer)))
          (when (and session
                     (mevedel-session-durability-lease-owned-p session))
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "keeps a committed head and refreshes on when the buffer install fails"
  (let* ((host "remote-rewind-install-fail")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-rewind-" t)))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?d))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (target '(:segment 1 :turn 1 :cum-turn 1
                             :fork-point-id "remote-point")))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*remote-rewind-install*" session))
              (setf (mevedel-session-publication session)
                    (mevedel-session-publication-read session-dir)
                    (mevedel-session-turn-count session) 1
                    (mevedel-session-prompt-index session)
                    `((1 . (,target))))
              (setq buffer (generate-new-buffer " *remote-rewind-live*"))
              (with-current-buffer buffer
                (org-mode)
                (insert "Original transcript\n")
                (setq-local mevedel--session session)
                (setq buffer-file-name segment))
              (let ((generation
                     (plist-get (mevedel-session-lease session) :generation))
                    (old-head
                     (plist-get (mevedel-session-publication session) :head))
                    (followed nil)
                    (warning nil))
                (cl-letf
                    (((symbol-function
                       'mevedel-session-rewind--stage-rewind)
                      (lambda (_session candidate actual-target staging-path
                               staging-buffer &rest _)
                        (make-directory staging-path t)
                        (with-current-buffer staging-buffer
                          (erase-buffer)
                          (insert "Rewound transcript\n")
                          (setq buffer-file-name
                                (mevedel-session-artifacts-segment-path
                                 staging-path
                                 (plist-get actual-target :segment)))
                          (write-region (point-min) (point-max)
                                        buffer-file-name nil 'silent))
                        candidate))
                     ((symbol-function
                       'mevedel-session-rewind-restore-plan)
                      (lambda (&rest _) nil))
                     ((symbol-function
                       'mevedel-session-rewind--install-rewind-buffer)
                      (lambda (&rest _)
                        (signal 'beginning-of-buffer nil)))
                     ((symbol-function
                       'mevedel-instruction-store-rewind-end)
                      (lambda (&rest _) (push 'instructions followed) t))
                     ((symbol-function
                       'mevedel-session-rewind--refresh-restored-buffers)
                      (lambda (&rest _) (push 'buffers followed))))
                  (mevedel-test--with-captured-diagnostics warning
                    (should
                     (mevedel-session-rewind--commit-rewind
                      session buffer target nil))))
                ;; The head is committed, so the failed install degrades to
                ;; a warning that names its own frame, and every later
                ;; local refresh still runs.
                (should-not
                 (equal old-head
                        (plist-get (mevedel-session-publication session)
                                   :head)))
                (should (equal generation
                               (plist-get (mevedel-session-lease session)
                                          :generation)))
                (should (equal '(instructions buffers)
                               (nreverse followed)))
                (should (string-match-p "did not install" warning))
                (should (string-match-p "Beginning of buffer" warning))
                (should
                 (string-match-p
                  "mevedel-session-rewind--install-rewind-buffer" warning))))))
      (when (buffer-live-p buffer)
        (let ((session (buffer-local-value 'mevedel--session buffer)))
          (when (and session
                     (mevedel-session-durability-lease-owned-p session))
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "rolls project files back under a fresh authority reservation"
  (let* ((host "remote-rewind-rollback")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-rewind-rollback-" t)))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?c))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (tracked (file-name-concat
                             (file-name-directory session-dir) "tracked.el"))
                   (target '(:segment 1 :turn 1 :cum-turn 1
                             :fork-point-id "remote-point"))
                   (plan (list (list :path tracked :action 'overwrite
                                     :bytes "rewound project bytes\n")))
                   (reserve-function
                    (symbol-function
                     'mevedel-session-durability-call-with-reserved-lease))
                   reservations)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*remote-rewind-rollback*" session))
              (setf (mevedel-session-publication session)
                    (mevedel-session-publication-read session-dir)
                    (mevedel-session-turn-count session) 1)
              (write-region "current project bytes\n" nil tracked nil 'silent)
              (setq buffer (generate-new-buffer " *remote-rewind-rollback*"))
              (with-current-buffer buffer
                (org-mode)
                (insert "Original transcript\n")
                (setq-local mevedel--session session)
                (setq buffer-file-name segment))
              (let ((old-head
                     (plist-get (mevedel-session-publication session) :head)))
                (cl-letf
                    (((symbol-function
                       'mevedel-session-rewind--stage-rewind)
                      (lambda (_session candidate actual-target staging-path
                               staging-buffer &rest _)
                        (make-directory staging-path t)
                        (with-current-buffer staging-buffer
                          (erase-buffer)
                          (insert "Rewound transcript\n")
                          (setq buffer-file-name
                                (mevedel-session-artifacts-segment-path
                                 staging-path
                                 (plist-get actual-target :segment)))
                          (write-region (point-min) (point-max)
                                        buffer-file-name nil 'silent))
                        candidate))
                     ((symbol-function
                       'mevedel-session-rewind-restore-plan)
                      (lambda (&rest _) plan))
                     ((symbol-function 'mevedel-session-publication-publish)
                      (lambda (actual-session _artifacts)
                        (setf (mevedel-session-pending-publication
                               actual-session)
                              '(:batches nil))
                        (error "Injected pre-CAS failure")))
                     ((symbol-function
                       'mevedel-session-durability-call-with-reserved-lease)
                      (lambda (actual-session function)
                        (push (mevedel-session-save-path actual-session)
                              reservations)
                        (funcall reserve-function actual-session function))))
                  (should-error
                   (mevedel-session-rewind--commit-rewind
                    session buffer target plan)))
                (should (= 2 (length reservations)))
                (should-not (mevedel-session-pending-publication session))
                (should
                 (equal old-head
                        (plist-get (mevedel-session-publication session)
                                   :head)))
                (should
                 (equal "current project bytes\n"
                        (mevedel-session-artifacts--file-text tracked)))
                (with-current-buffer buffer
                  (should (equal "Original transcript\n"
                                 (buffer-string))))))))
      (when (buffer-live-p buffer)
        (let ((session (buffer-local-value 'mevedel--session buffer)))
          (when (and session
                     (mevedel-session-durability-lease-owned-p session))
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "pre-restore failure preserves a concurrent edit only after staging is reached"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *rewind-pre-restore*"))
           (path (file-name-concat tempdir "tracked.el")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "First prompt\n")
            (insert (propertize "First reply.\n" 'gptel 'response))
            (setf (mevedel-session-turn-count session) 1)
            (mevedel-session-artifacts-save session buffer t)
            (let* ((target (copy-sequence
                            (cdar (mevedel-session-rewind--prompt-candidates
                                   session))))
                   (plan (list (list :path path :action 'overwrite)))
                   (head (plist-get (mevedel-session-publication session) :head))
                   (text (buffer-string))
                   (state (mevedel-session-codec-serialize session))
                   (publication (mevedel-session-publication session))
                   (read-publication
                    (symbol-function 'mevedel-session-publication-read))
                   staging-reached unpublished)
              (cl-letf
                  (((symbol-function 'mevedel-session-publication-read)
                    (lambda (&rest args)
                      (unless unpublished (apply read-publication args))))
                   ((symbol-function 'mevedel-session-rewind--stage-rewind)
                    (lambda (&rest _)
                      (setq staging-reached t)
                      (write-region "concurrent" nil path nil 'silent)
                      (error "Injected staging failure"))))
                ;; An unpublished fixture's early error is not this regression.
                (setq unpublished t)
                (setf (mevedel-session-publication session) nil)
                (write-region "before" nil path nil 'silent)
                (let ((err (should-error
                            (mevedel-session-rewind--commit-rewind
                             session buffer target plan))))
                  (should (string-match-p "requires a committed publication"
                                          (error-message-string err))))
                (should-not staging-reached)
                (should (equal "before" (mevedel-session-artifacts--file-text path)))
                (setq unpublished nil)
                (setf (mevedel-session-publication session) publication)
                (let ((err (should-error
                            (mevedel-session-rewind--commit-rewind
                             session buffer target plan))))
                  (should (equal "Injected staging failure"
                                 (error-message-string err)))))
              (should staging-reached)
              (should (equal "concurrent" (mevedel-session-artifacts--file-text path)))
              (should (equal head (plist-get (mevedel-session-publication session)
                                             :head)))
              (should (equal text (buffer-string)))
              (should (equal state (mevedel-session-codec-serialize session)))))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))
  :doc "boundary after rechecks the published post-turn plan instead of the pre-turn plan"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *rewind-boundary-after*"))
           (path (file-name-concat tempdir "tracked.el"))
           (pre (mevedel-session-artifacts--file-history-backup-name "pre-turn"))
           (post (mevedel-session-artifacts--file-history-backup-name "post-turn")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session
                        gptel-backend (test-mevedel-session-persistence--agent-backend)
                        gptel-model 'test-model)
            (gptel-mode 1)
            (insert "First prompt\n")
            (insert (propertize "First reply.\n" 'gptel 'response))
            (setf (mevedel-session-turn-count session) 1)
            (mevedel-session-artifacts-save session buffer t)
            (setf (mevedel-session-file-snapshots session)
                  `((1 . ((,path . (:backup-name ,post :pre-backup-name ,pre
                                   :version 1))))))
            (test-mevedel-rewind--publish-backups
             session pre "pre-turn" post "post-turn")
            (write-region "external edit" nil path nil 'silent)
            (let ((target (copy-sequence
                           (cdar (mevedel-session-rewind--prompt-candidates
                                  session))))
                  (plan (mevedel-session-rewind-restore-plan session 1))
                  (head (plist-get (mevedel-session-publication session) :head)))
              (should (plist-get target :fork-point-id))
              (should (= 1 (length plan)))
              (should (mevedel-session-rewind--commit-rewind
                       session buffer target plan 'after))
              (should (equal "post-turn" (mevedel-session-artifacts--file-text path)))
              (should (= 1 (mevedel-session-turn-count session)))
              (should-not
               (equal head (plist-get (mevedel-session-publication session) :head)))))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry)))))


(mevedel-deftest mevedel-session-rewind--rewind-confirmation ()
  ,test
  (test)
  :doc "names the boundary and both sides of it"
  (let ((session (mevedel-session--create :name "main"
                                          :authority-mode 'portable))
        (target '(:segment 1 :turn 2 :cum-turn 2)))
    (should
     (equal
      "Rewind main to before S1 T2: keep 1 turn, discard 3 (0 files; conversation and captured-file redo)? "
      (mevedel-session-rewind--rewind-confirmation
       session (list :target target :boundary 'before
                     :surviving-turns 1 :discarded-turns 3 :file-plan nil))))
    (should
     (equal
      "Rewind main keeping S1 T2: keep 2 turns, discard 2 (1 file; conversation and captured-file redo)? "
      (mevedel-session-rewind--rewind-confirmation
       session (list :target target :boundary 'after
                     :surviving-turns 2 :discarded-turns 2
                     :file-plan (list (list :action 'restore
                                            :path "/tmp/a"))))))))

(mevedel-deftest mevedel-session-rewind--restore-confirmation ()
  ,test
  (test)
  :doc "names the turns and captured files a restore returns"
  (let ((session (mevedel-session--create :name "main"))
        (entry (list :head ".publications/generation-abc/manifest.el"
                     :time '(0 0) :turn-count 4 :transcript-bytes 2048)))
    (should
     (string-match-p
      "4 turns, 2 captured files, 1 externally changed; uncaptured effects remain"
      (mevedel-session-rewind--restore-confirmation
       session entry
       (list :restored-turns 4
             :file-plan (list (list :action 'restore :path "/tmp/a")
                              (list :action 'overwrite :path "/tmp/b"))
             :external-overwrites 1))))
    ;; A restore with no captured files promises nothing about files.
    (let ((prompt (mevedel-session-rewind--restore-confirmation
                   session entry
                   (list :restored-turns 1 :file-plan nil
                         :external-overwrites 0))))
      (should (string-match-p "1 turns, 0 captured files" prompt))
      (should-not (string-match-p "uncaptured" prompt)))))

(mevedel-deftest mevedel-redo ()
  ,test
  (test)
  :doc "selects and restores one published head, then starts a restore epoch"
  (with-temp-buffer
    (let* ((session (mevedel-session--create
                     :name "main" :authority-mode 'portable))
           (entry '(:head ".publications/generation-abc/manifest.el"))
           restored hook-source)
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-session-rewind-published-heads)
                 (lambda (_session) (list entry)))
                ((symbol-function
                  'mevedel-session-rewind--published-head-label)
                 (lambda (_entry) "published state"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "published state"))
                ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'mevedel-session-rewind-restore-head)
                 (lambda (seen-session seen-buffer head confirm)
                   (setq restored (list seen-session seen-buffer head))
                   (funcall confirm
                            '(:restored-turns 1 :file-plan nil
                              :external-overwrites 0))))
                ((symbol-function 'mevedel--run-session-start-hooks)
                 (lambda (source) (setq hook-source source)))
                ((symbol-function 'message) #'ignore))
        (should (mevedel-redo)))
      (should (equal (list session (current-buffer) (plist-get entry :head))
                     restored))
      (should (equal "restore" hook-source)))))

(mevedel-deftest mevedel-session-rewind--redo-availability ()
  ,test
  (test)
  :doc "promises captured-state redo exactly where heads stay published"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (should (equal "conversation and captured-file redo"
                       (mevedel-session-rewind--redo-availability
                        (mevedel-session-create "redo" workspace))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "promises no redo for a workspace-free session"
  (should (equal "no redo"
                 (mevedel-session-rewind--redo-availability
                  (mevedel-session--create)))))

(mevedel-deftest mevedel-session-rewind-published-heads ()
  ,test
  (test)
  :doc "lists one head per settled turn state, newest first"
  (let* ((host "published-heads")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-published-heads-" t))))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((mevedel-session-durability--client-id (make-string 64 ?e))
                  (mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*published-heads*" session))
              (unwind-protect
                  (cl-labels
                      ((publish (transcript turns)
                         (setf (mevedel-session-turn-count session) turns)
                         (mevedel-session-publication-publish
                          session
                          (list
                           (list :path segment :content transcript)
                           (list :path
                                 (mevedel-session-artifacts-sidecar-path
                                  session-dir)
                                 :content
                                 (mevedel-session-artifacts-printed-value
                                  (mevedel-session-artifacts--live-sidecar
                                   session (current-buffer)))
                                 :commit-marker t
                                 :replace t)))
                         (plist-get (mevedel-session-publication session)
                                    :head))
                       (heads ()
                         (mapcar (lambda (entry) (plist-get entry :head))
                                 (mevedel-session-rewind-published-heads
                                  session))))
                    (setf (mevedel-session-publication session)
                          (mevedel-session-publication-read session-dir))
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
                                  (mevedel-session-rewind-published-heads
                                   session)))))))))
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))

  :doc "returns nothing for a workspace-free session"
  (should-not
   (mevedel-session-rewind-published-heads
    (mevedel-session--create))))

(mevedel-deftest mevedel-session-rewind-restore-head ()
  ,test
  (test)
  :doc "republishes a superseded head as the live conversation"
  (let* ((host "restore-head")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-restore-head-" t)))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((mevedel-session-durability--client-id (make-string 64 ?f))
                  (mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*restore-head*" session))
              (unwind-protect
                  (cl-labels
                      ((publish (transcript turns)
                         (setf (mevedel-session-turn-count session) turns)
                         (mevedel-session-publication-publish
                          session
                          (list
                           (list :path segment :content transcript)
                           (list :path
                                 (mevedel-session-artifacts-sidecar-path
                                  session-dir)
                                 :content
                                 (mevedel-session-artifacts-printed-value
                                  (mevedel-session-artifacts--live-sidecar
                                   session (current-buffer)))
                                 :commit-marker t
                                 :replace t)))
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    (setf (mevedel-session-publication session)
                          (mevedel-session-publication-read session-dir))
                    ;; The fixture's head is also a turn-0 state.  Heads are
                    ;; ordered by whole-second manifest times, so make it
                    ;; strictly older: the newest turn-0 head, the rewound
                    ;; one, must be the one that stands for that state.
                    (set-file-times
                     (file-name-concat
                      session-dir
                      (plist-get (mevedel-session-publication session) :head))
                     (time-subtract nil 60))
                    (let* ((restorable (publish "Original transcript\n" 1))
                           (rewound (publish "Rewound transcript\n" 0)))
                      (setq buffer (generate-new-buffer " *restore-live*"))
                      (with-current-buffer buffer
                        (org-mode)
                        (insert "Rewound transcript\n")
                        (setq-local mevedel--session session)
                        (setq buffer-file-name segment))
                      (cl-letf
                          (((symbol-function
                             'mevedel-session-artifacts-load-instructions)
                            (lambda (&rest _) t))
                           ((symbol-function
                             'mevedel-session-recovery-refresh-session-buffers)
                            #'ignore))
                        (should
                         (mevedel-session-rewind-restore-head
                          session buffer restorable)))
                      ;; The restore is itself a new committed head, and
                      ;; the superseded transcript and turn count are live
                      ;; again.
                      (let ((head (plist-get
                                   (mevedel-session-publication session)
                                   :head)))
                        (should-not (equal head rewound))
                        (should-not (equal head restorable)))
                      (should (= 1 (mevedel-session-turn-count session)))
                      (should (= 1 (plist-get
                                    (mevedel-session-checkpoint-project session nil)
                                    :total-turn-count)))
                      (should
                       (equal "Original transcript\n"
                              (mevedel-session-artifacts-read-artifact
                               session "segment-0001.chat.org" t)))
                      (with-current-buffer buffer
                        (should (equal "Original transcript\n"
                                       (buffer-string))))
                      ;; Restoring consumes nothing: the state it moved
                      ;; away from is restorable in turn.
                      (should
                       (member rewound
                               (mapcar (lambda (entry) (plist-get entry :head))
                                       (mevedel-session-rewind-published-heads
                                        session))))))
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))

  :doc "restores captured files from the published head's file history"
  ;; The pre-Rewind file bytes stay published under `file-history', and
  ;; the restored sidecar reinstates the snapshot index that names them,
  ;; so a restore returns captured working-tree state as well.
  (let* ((host "restore-head-files")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-restore-files-" t)))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((mevedel-session-durability--client-id (make-string 64 ?a))
                  (mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal))
                  (tracked (file-name-concat local-root "tracked.el")))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*restore-files*" session))
              (unwind-protect
                  (progn
                    (write-region "captured bytes\n" nil tracked nil 'silent)
                    (setf (mevedel-session-publication session)
                          (mevedel-session-publication-read session-dir)
                          (mevedel-session-turn-count session) 1
                          (mevedel-session-file-snapshots session)
                          `((1 . ((,tracked . (:backup-name "000001"
                                               :pre-backup-name "000001"
                                               :version 1))))))
                    ;; Publish the head that owns both the transcript and
                    ;; the captured file bytes.
                    (mevedel-session-publication-publish
                     session
                     (list
                      (list :path (file-name-concat
                                   session-dir "file-history" "000001")
                            :content "captured bytes\n")
                      (list :path segment :content "Original transcript\n")
                      (list :path (mevedel-session-artifacts-sidecar-path
                                   session-dir)
                            :content
                            (mevedel-session-artifacts-printed-value
                             (mevedel-session-artifacts--live-sidecar
                              session (current-buffer)))
                            :commit-marker t
                            :replace t)))
                    (let ((restorable
                           (plist-get (mevedel-session-publication session)
                                      :head)))
                      ;; Supersede it the way a Rewind does: no turns, no
                      ;; snapshots, and the working tree moved on.
                      (setf (mevedel-session-turn-count session) 0
                            (mevedel-session-file-snapshots session) nil)
                      (write-region "rewound bytes\n" nil tracked nil 'silent)
                      (mevedel-session-publication-publish
                       session
                       (list
                        (list :path segment :content "Rewound transcript\n")
                        (list :path (mevedel-session-artifacts-sidecar-path
                                     session-dir)
                              :content
                              (mevedel-session-artifacts-printed-value
                               (mevedel-session-artifacts--live-sidecar
                                session (current-buffer)))
                              :commit-marker t
                              :replace t)))
                      (setq buffer (generate-new-buffer " *restore-files*"))
                      (with-current-buffer buffer
                        (org-mode)
                        (insert "Rewound transcript\n")
                        (setq-local mevedel--session session)
                        (setq buffer-file-name segment))
                      (let (impact)
                        (cl-letf
                            (((symbol-function
                               'mevedel-session-artifacts-load-instructions)
                              (lambda (&rest _) t))
                             ((symbol-function
                               'mevedel-session-recovery-refresh-session-buffers)
                              #'ignore))
                          (should
                           (mevedel-session-rewind-restore-head
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
                              (mevedel-session-artifacts--file-text tracked)))
                      (should (= 1 (mevedel-session-turn-count session)))
                      (with-current-buffer buffer
                        (should (equal "Original transcript\n"
                                       (buffer-string))))))
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))

  :doc "abandons everything when the confirmation declines"
  (let* ((host "restore-head-declined")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-restore-declined-" t)))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let ((mevedel-session-durability--client-id (make-string 64 ?c))
                  (mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*restore-declined*" session))
              (unwind-protect
                  (progn
                    (setf (mevedel-session-publication session)
                          (mevedel-session-publication-read session-dir))
                    (let ((restorable
                           (plist-get (mevedel-session-publication session)
                                      :head)))
                      (mevedel-session-publication-publish
                       session
                       (list
                        (list :path segment :content "Rewound transcript\n")
                        (list :path (mevedel-session-artifacts-sidecar-path
                                     session-dir)
                              :content
                              (mevedel-session-artifacts-read-file-raw
                               (mevedel-session-artifacts-sidecar-path
                                session-dir))
                              :commit-marker t
                              :replace t)))
                      (setq buffer (generate-new-buffer " *restore-declined*"))
                      (with-current-buffer buffer
                        (org-mode)
                        (insert "Rewound transcript\n")
                        (setq-local mevedel--session session)
                        (setq buffer-file-name segment))
                      (let ((rewound-head
                             (plist-get (mevedel-session-publication session)
                                        :head)))
                        (should-not
                         (mevedel-session-rewind-restore-head
                          session buffer restorable (lambda (_impact) nil)))
                        (should
                         (equal rewound-head
                                (plist-get
                                 (mevedel-session-publication session)
                                 :head)))
                        (should-not
                         (mevedel-session-pending-publication session))
                        (with-current-buffer buffer
                          (should (equal "Rewound transcript\n"
                                         (buffer-string)))))))
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))

  :doc "rolls a pre-CAS file restore back under a fresh lease reservation"
  (let* ((root (make-temp-file "mevedel-restore-rollback-" t))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "restore"))
         (session (mevedel-session--create
                   :workspace workspace :authority-mode 'portable
                   :save-path root
                   :publication '(:head "current")))
         (candidate (mevedel-session--create :workspace workspace))
         (buffer (generate-new-buffer " *restore-rollback*"))
         (plan '((:path "/tmp/captured" :action overwrite)))
         (backups '((:path "/tmp/captured" :existed nil)))
         (reservations 0)
         rolled-back)
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-rewind-assert-stable-source)
              #'ignore)
             ((symbol-function 'mevedel-session-publication-read)
              (lambda (_path &optional head)
                (if head '(:head "target") '(:head "current"))))
             ((symbol-function
               'mevedel-session-rewind-materialize-publication)
              #'ignore)
             ((symbol-function 'mevedel-session-codec-read)
              (lambda (_path) '(:sidecar t)))
             ((symbol-function 'mevedel-session-codec-deserialize)
              (lambda (_raw _workspace) (list :session candidate)))
             ((symbol-function 'mevedel-session-set-execution-target)
              #'ignore)
             ((symbol-function
               'mevedel-session-rewind--load-restored-transcript)
              #'ignore)
             ((symbol-function 'mevedel-session-rewind--restore-file-plan)
              (lambda (_candidate) plan))
             ((symbol-function
               'mevedel-session-rewind--prepare-buffers-for-restore)
              (lambda (_candidate _turn actual-plan &rest _) actual-plan))
             ((symbol-function
               'mevedel-session-rewind--backup-restore-files)
              (lambda (_plan _directory) backups))
             ((symbol-function 'mevedel-session-rewind-execute-restore)
              (lambda (_candidate _plan) '(:succeeded 1)))
             ((symbol-function
               'mevedel-session-rewind-rewind-publication-artifacts)
              (lambda (&rest _) nil))
             ((symbol-function 'mevedel-session-publication-publish)
              (lambda (actual-session &rest _)
                (setf (mevedel-session-pending-publication actual-session)
                      '(:batches nil))
                (error "Injected pre-CAS failure")))
             ((symbol-function
               'mevedel-session-durability-call-with-reserved-lease)
              (lambda (_session function)
                (cl-incf reservations)
                (funcall function)))
             ((symbol-function
               'mevedel-session-rewind--rollback-restore-files)
              (lambda (actual-backups _written)
                (setq rolled-back actual-backups)
                nil))
             ((symbol-function
               'mevedel-session-publication-discard-rolled-back)
              (lambda (actual-session)
                (setf (mevedel-session-pending-publication actual-session)
                      nil))))
          (let ((raised
                 (should-error
                  (mevedel-session-rewind-restore-head
                   session buffer "target" (lambda (_impact) t)))))
            (should (string-match-p "Injected pre-CAS failure"
                                    (error-message-string raised)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-directory-p root) (delete-directory root t)))
    (should (= 2 reservations))
    (should (equal backups rolled-back))
    (should-not (mevedel-session-pending-publication session)))

  :doc "refuses a workspace-free session"
  (let ((session (mevedel-session--create)))
    (should-error
     (mevedel-session-rewind-restore-head
      session (current-buffer) ".publications/generation-x/manifest.el")
     :type 'user-error)))


(mevedel-deftest mevedel-session-rewind--rollback-restore-files ()
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
                   (mevedel-session-rewind--rollback-restore-files
                    (mapcar (lambda (path) (list :path path :existed t :backup backup))
                            (list path-a path-b))
                    (mapcar #'mevedel-session-rewind--file-state (list path-a path-b))))))
            (should (= 2 (length failures)))
            (should (string-match-p (regexp-quote path-a) (nth 0 failures)))
            (should (string-match-p (regexp-quote path-b) (nth 1 failures)))))
      (delete-directory directory t))))


;;
;;; Phase 8: file restore plan

(mevedel-deftest mevedel-session-rewind-reduce-agent-transcripts ()
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
             (mevedel-session-rewind-reduce-agent-transcripts entries 5))))))

(mevedel-deftest mevedel-session-rewind--rewind-candidate ()
  ,test
  (test)
  :doc "keeps only chats closed before the target segment, so a later /clear stays ordered"
  (let* ((chapters '((:segment 1 :title "A" :closed-at "t1")
                     (:segment 3 :title "C" :closed-at "t3")))
         (session (mevedel-session--create
                   :name "main" :current-segment 4 :turn-count 6
                   :chapters (copy-tree chapters)))
         (candidate (mevedel-session-rewind--rewind-candidate
                     session '(:segment 2 :turn 1 :cum-turn 3))))
    (should (= 2 (mevedel-session-current-segment candidate)))
    (should (equal '((:segment 1 :title "A" :closed-at "t1"))
                   (mevedel-session-chapters candidate)))
    (should (equal chapters (mevedel-session-chapters session)))
    ;; Two later clears close segments 2 and 3 under their own titles.
    (setf (mevedel-session-chapters candidate)
          (append (mevedel-session-chapters candidate)
                  '((:segment 2 :title "B" :closed-at "t5")
                    (:segment 3 :title "D" :closed-at "t6"))))
    (should (equal '(("A" 1 1) ("B" 2 2) ("D" 3 3) ("Now" 4 4))
                   (mapcar (lambda (chat)
                             (list (plist-get chat :title)
                                   (plist-get chat :first-segment)
                                   (plist-get chat :last-segment)))
                           (mevedel-session-artifacts-chats
                            (mevedel-session-chapters candidate) nil 4 "Now"))))))

(mevedel-deftest mevedel-session-rewind-reduce-prompt-index ()
  ,test
  (test)
  (let ((index '((1 . ((:cum-turn 1) (:cum-turn 2)))
                 (2 . ((:cum-turn 3) (:cum-turn 4)))
                 (3 . ((:cum-turn 5))))))
    (should
     (equal '((1 . ((:cum-turn 1) (:cum-turn 2)))
              (2 . ((:cum-turn 3) (:cum-turn 4))))
            (mevedel-session-rewind-reduce-prompt-index index 2 4)))
    (should
     (equal '((1 . ((:cum-turn 1) (:cum-turn 2)))
              (2 . ((:cum-turn 3))))
            (mevedel-session-rewind-reduce-prompt-index index 2 4 t)))))


(mevedel-deftest mevedel-session-rewind-reduce-file-snapshots ()
  ,test
  (test)
  (let ((snapshots '((1 . first) (3 . third) (4 . fourth) (5 . fifth))))
    (should
     (equal '((1 . first) (3 . third) (4 . fourth))
            (mevedel-session-rewind-reduce-file-snapshots snapshots 4)))
    (should
     (equal '((1 . first) (3 . third))
            (mevedel-session-rewind-reduce-file-snapshots snapshots 4 t)))
    (should
     (eq snapshots
         (mevedel-session-rewind-reduce-file-snapshots snapshots nil)))))


(mevedel-deftest mevedel-session-rewind-state-at-turn ()
  ,test
  (test)
  :doc "picks each path's earliest pre-turn checkpoint in the discarded suffix"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/abs/foo" . (:backup-name "fooA-post"
                                  :pre-backup-name "fooA" :version 1))))
            (3 . (("/abs/foo" . (:backup-name "fooC-post"
                                  :pre-backup-name "fooC" :version 3))
                  ("/abs/bar" . (:backup-name "barB-post"
                                  :pre-backup-name "barB" :version 2))))
            (5 . (("/abs/foo" . (:backup-name "fooE-post"
                                  :pre-backup-name "fooE" :version 5))))))
    ;; Rewind before turn 2: foo and bar first change at turn 3.
    (let ((state (mevedel-session-rewind-state-at-turn session 2 t)))
      (should (= 2 (length state)))
      (should (equal "fooC"
                     (plist-get (cdr (assoc "/abs/foo" state))
                                :pre-backup-name)))
      (should (equal "barB"
                     (plist-get (cdr (assoc "/abs/bar" state))
                                :pre-backup-name))))
    ;; Rewind before turn 1 selects foo's turn-1 checkpoint and bar's turn-3
    ;; checkpoint because bar was not changed before then.
    (let ((state (mevedel-session-rewind-state-at-turn session 1 t)))
      (should (= 2 (length state)))
      (should (equal "fooA"
                     (plist-get (cdr (assoc "/abs/foo" state))
                                :pre-backup-name)))
      (should (equal "barB"
                     (plist-get (cdr (assoc "/abs/bar" state))
                                :pre-backup-name))))
    (mevedel-workspace-clear-registry)))


(mevedel-deftest mevedel-session-rewind--latest-snapshot-entry ()
  ,test
  (test)
  :doc "returns highest-version entry for the path"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id2" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/abs/foo" . (:backup-name "v1" :version 1))))
            (5 . (("/abs/foo" . (:backup-name "v3" :version 3))))
            (3 . (("/abs/foo" . (:backup-name "v2" :version 2))))))
    (let ((latest (mevedel-session-rewind--latest-snapshot-entry
                   session "/abs/foo")))
      (should (equal "v3" (plist-get latest :backup-name))))
    (mevedel-workspace-clear-registry)))


(mevedel-deftest mevedel-session-rewind--plan-row-diff ()
  ,test
  (test)
  :doc "materializes resolver-owned bytes instead of diffing a fixed cache"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-rewind-row-diff-" t)))
         (current (file-name-concat root "current.el"))
         (backup-name "current@v1")
         (fixed-backup
          (mevedel-session-artifacts-backup-path root backup-name))
         (session (mevedel-session--create :save-path root))
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
            (setq-local mevedel-session-rewind--plan-buffer-session
                        session)
            (insert "row\n")
            (put-text-property
             (point-min) (point-max) 'mevedel-plan-entry
             (list :action 'restore :path current
                   :backup-name backup-name))
            (goto-char (point-min))
            (cl-letf
                (((symbol-function 'mevedel-session-artifacts-read-backup)
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
                          (mevedel-session-artifacts-read-file-raw new)))))
              (mevedel-session-rewind--plan-row-diff)))
          (should resolver-called)
          (should (equal current diff-current))
          (should (equal "published" diff-target-content))
          (should-not (file-exists-p diff-target))
          (should (equal "poison"
                         (mevedel-session-artifacts-read-file-raw fixed-backup))))
      (delete-directory root t))))


(mevedel-deftest mevedel-session-rewind-restore-plan ()
  ,test
  (test)
  :doc "noop when current content matches target snapshot"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (backup-name (mevedel-session-artifacts--file-history-backup-name "v1")))
          (write-region "v1" nil path nil 'silent)
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,backup-name :version 1
                                  :backup-time "..." :file-mtime "..."))))))
          (test-mevedel-rewind--publish-backups session backup-name "v1")
          (let ((plan (mevedel-session-rewind-restore-plan session 1)))
            (should (null plan))))   ; noop entries filtered
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "create when target has content but file currently absent"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (backup-name (mevedel-session-artifacts--file-history-backup-name "content")))
          ;; File doesn't currently exist.
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,backup-name :version 1
                                  :backup-time "..." :file-mtime "..."))))))
          (test-mevedel-rewind--publish-backups session backup-name "content")
          (let ((plan (mevedel-session-rewind-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'create (plist-get (car plan) :action)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "delete when target is absent but file exists"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "stale.el")))
          (write-region "stale content" nil path nil 'silent)
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name nil :version 1
                                  :backup-time "..." :file-mtime nil))))))
          (let ((plan (mevedel-session-rewind-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'delete (plist-get (car plan) :action)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "overwrite when current content diverges from latest snapshot"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (b1   (mevedel-session-artifacts--file-history-backup-name "v1"))
               (b2   (mevedel-session-artifacts--file-history-backup-name "v2")))
          ;; Current file content is something the snapshots have never seen.
          (write-region "external edits" nil path nil 'silent)
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,b1 :version 1
                                  :backup-time "..." :file-mtime "..."))))
                  (2 . ((,path . (:backup-name ,b2 :version 2
                                  :backup-time "..." :file-mtime "..."))))))
          (test-mevedel-rewind--publish-backups session b1 "v1" b2 "v2")
          (let ((plan (mevedel-session-rewind-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'overwrite (plist-get (car plan) :action)))
            (should (plist-get (car plan) :diverged))))
      (test-mevedel-session-persistence--cleanup tempdir))))


(mevedel-deftest mevedel-session-rewind-execute-restore ()
  ,test
  (test)
  :doc "applies create / delete / restore actions correctly"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((create-path (file-name-concat tempdir "new.el"))
               (delete-path (file-name-concat tempdir "old.el"))
               (restore-path (file-name-concat tempdir "modified.el"))
               (backup-name-create
                (mevedel-session-artifacts--file-history-backup-name "newly created"))
               (backup-name-restore
                (mevedel-session-artifacts--file-history-backup-name "original")))
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,create-path . (:backup-name ,backup-name-create :version 1))
                        (,restore-path . (:backup-name ,backup-name-restore :version 1))))))
          (test-mevedel-rewind--publish-backups
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
                 (result (mevedel-session-rewind-execute-restore
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
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "stops on first failure"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "fine.el"))
               (bn   (mevedel-session-artifacts--file-history-backup-name "ok")))
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,bn :version 1))))))
          (test-mevedel-rewind--publish-backups session bn "ok")
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
                 (result (mevedel-session-rewind-execute-restore
                          session plan)))
            (should (= 1 (plist-get result :succeeded)))
            (should (plist-get result :failed))
            (should-not (file-exists-p
                         (file-name-concat tempdir "three.el")))))
      (test-mevedel-session-persistence--cleanup tempdir))))


(mevedel-deftest mevedel-session-rewind-assert-stable-source ()
  ,test
  (test)
  :doc "blocks every live owner and permits stable Goal and Plan states"
  (let ((session (mevedel-session--create :name "source"))
        (buffer (generate-new-buffer " *stable-source*"))
        execution-live
        agent-live)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--current-request nil))
          (cl-letf
              (((symbol-function 'mevedel-execution-session-live-p)
                (lambda (_) execution-live))
               ((symbol-function 'mevedel-agent-control-active-turn-p)
                (lambda (_) agent-live)))
            (setf (mevedel-session-pending-follow-ups session)
                  '((:input "later")))
            (should-error
             (mevedel-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setf (mevedel-session-pending-follow-ups session) nil)
            (with-current-buffer buffer
              (setq-local mevedel--current-request t))
            (should-error
             (mevedel-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (with-current-buffer buffer
              (setq-local mevedel--current-request nil))
            (setq execution-live t)
            (should-error
             (mevedel-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setq execution-live nil
                  agent-live t)
            (should-error
             (mevedel-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setq agent-live nil)
            (setf (mevedel-session-goal session)
                  (mevedel-goal--create :status 'active))
            (should-error
             (mevedel-session-rewind-assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setf (mevedel-goal-status (mevedel-session-goal session))
                  'paused
                  (mevedel-session-pending-plan-approval session)
                  '(:proposal stable))
            (should-not
             (mevedel-session-rewind-assert-stable-source
              session buffer "forking"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


(provide 'test-mevedel-session-rewind)
;;; test-mevedel-session-rewind.el ends here
