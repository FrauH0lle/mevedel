;;; test-mevedel-execution-transcript.el -- Execution transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests durable execution render data, compaction archives, and terminal
;; reconciliation independently from live View streaming.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'mevedel-agents)
(require 'mevedel-execution-target)
(require 'mevedel-execution-transcript)
(require 'mevedel-pipeline)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-session-publication)
(require 'mevedel-structs)
(require 'mevedel-transcript-audit)
(require 'mevedel-transcript-restore)
(require 'mevedel-tool-render-data)
(require 'mevedel-workspace)
(require 'mevedel-workspace-identity)

;;
;;; Archive projection and settlement

(defun mevedel-execution-transcript-test--persisted-audit-transcript (text)
  "Return a persisted Org transcript containing trusted audit TEXT."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n" text)
    (mevedel-session-artifacts-stabilize-gptel-bounds)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mevedel-execution-transcript-test--restored-audit-records (text type)
  "Return trusted audit records of TYPE restored from persisted TEXT."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (org-mode))
    (mevedel-transcript-restore-properties)
    (mevedel-transcript-audit-records (buffer-string) type)))

(defun mevedel-execution-transcript-test--audit-records-in-file (path type)
  "Return trusted audit records of TYPE restored from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (mevedel-execution-transcript-test--restored-audit-records
     (buffer-string) type)))

(mevedel-deftest mevedel-execution-transcript-terminal-render-data ()
  ,test
  (test)
  :doc "omits the default sandbox boundary"
  (should-not
   (plist-member
    (mevedel-execution-transcript-terminal-render-data
     '(:facts (:outcome success)
              :observation
              (:sandbox-summary
               (:attempt-count 1 :started-count 1 :refused-count 0
                               :sandbox bubblewrap :filesystem workspace-write
                               :network isolated :proc fresh
                               :additional-read-count 0 :additional-write-count 0))))
    :sandbox-summary))
  :doc "omits additional read-only access"
  (should-not
   (plist-member
    (mevedel-execution-transcript-terminal-render-data
     '(:facts (:outcome success)
              :observation
              (:sandbox-summary
               (:attempt-count 1 :started-count 1 :refused-count 0
                               :sandbox bubblewrap :filesystem workspace-write
                               :network isolated :proc fresh
                               :additional-read-count 2 :additional-write-count 0))))
    :sandbox-summary))
  :doc "keeps a material sandbox boundary"
  (should
   (plist-get
    (mevedel-execution-transcript-terminal-render-data
     '(:facts (:outcome success)
              :observation
              (:sandbox-summary
               (:attempt-count 1 :started-count 1 :refused-count 0
                               :sandbox bubblewrap :filesystem workspace-write
                               :network isolated :proc fresh
                               :additional-read-count 0 :additional-write-count 1))))
    :sandbox-summary)))
(mevedel-deftest mevedel-execution-transcript-handle-event ()
  ,test
  (test)
  :doc "persists terminal output without live View streaming"
  (with-temp-buffer
    (insert "#+begin_tool (Bash :command \"true\")\n")
    (let ((start (point)))
      (insert
       "(:name \"Bash\" :args (:command \"true\"))\n\nrunning"
       (mevedel-tool-render-data-format
        '(:execution-id "exec-terminal" :state running
                        :live-execution-p t)
        "terminal-call"))
      (put-text-property start (point) 'gptel '(tool . "terminal-call")))
    (insert "#+end_tool\n")
    (should-not
     (mevedel-execution-transcript-handle-event
      (list :type 'terminal :data-buffer (current-buffer)
            :tool-use-id "terminal-call" :whole-output "finished"
            :facts '(:execution-id "exec-terminal" :state completed
                                   :outcome success))))
    (let ((render-data
           (mevedel-tool-render-data-for-tool
            (current-buffer) "terminal-call")))
      (should (equal "finished"
                     (plist-get render-data :execution-output)))
      (should-not (plist-get render-data :live-execution-p)))))

(mevedel-deftest mevedel-execution-transcript-prepare-archive ()
  ,test
  (test)
  :doc "separates live and already-completed execution rows"
  (with-temp-buffer
    (insert
     (propertize
      (mevedel-tool-render-data-format
       '(:execution-id "exec-live" :state running :live-execution-p t)
       "live-call")
      'gptel '(tool . "live-call")))
    (insert
     (propertize
      (mevedel-tool-render-data-format
       '(:execution-id "exec-done" :state completed
                       :live-execution-p nil)
       "done-call")
      'gptel '(tool . "done-call")))
    (let ((plan
           (mevedel-execution-transcript-prepare-archive
            (current-buffer) '("live-call" "done-call" "missing-call"))))
      (should (equal "live-call" (caar (plist-get plan :live))))
      (should (eq 'running
                  (plist-get (cdar (plist-get plan :live)) :state)))
      (should (equal "done-call"
                     (caar (plist-get plan :completed)))))))

(mevedel-deftest mevedel-execution-transcript-commit-archive ()
  ,test
  (test)
  :doc "marks live rows and persists already-completed rows after compaction"
  (with-temp-buffer
    (let ((plan
           '(:live (("live-call" :execution-id "exec-live"
                     :state running :live-execution-p t))
                   :completed (("done-call" :execution-id "exec-done"
                                :state completed)))))
      (insert (mevedel-execution-transcript-archive-text plan))
      (mevedel-execution-transcript-commit-archive
       (current-buffer) plan))
    (should (gethash "live-call"
                     mevedel-execution-transcript--archived-rows))
    (should (= 1
               (length
                (mevedel-transcript-audit-records
                 (buffer-string) 'execution-completion))))
    (should (= 1
               (length
                (mevedel-transcript-audit-records
                 (buffer-string) 'execution-archive))))))

(mevedel-deftest mevedel-execution-transcript-pending-render-data ()
  ,test
  (test)
  :doc "returns a copy of pending terminal render data"
  (with-temp-buffer
    (mevedel-execution-transcript-store-pending-terminal
     (current-buffer) '(:tool-use-id "pending-1")
     '(:execution-output "done"))
    (let ((render-data
           (mevedel-execution-transcript-pending-render-data
            (current-buffer) "pending-1")))
      (should (equal '(:execution-output "done") render-data))
      (plist-put render-data :execution-output "changed")
      (should
       (equal
        "done"
        (plist-get
         (mevedel-execution-transcript-pending-render-data
          (current-buffer) "pending-1")
         :execution-output))))))

(mevedel-deftest mevedel-execution-transcript--record-archived-terminal ()
  ,test
  (test)
  :doc "publishes completion transactionally and permits a later save"
  (let* ((path (make-temp-file "mevedel-execution-archive-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :name "archive")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (let ((archive
                   (mevedel-execution-transcript-test--persisted-audit-transcript
                    (mevedel-execution-transcript-archive-text
                     '(:live (("old-call" :execution-id "exec-old"
                               :state running :live-execution-p t)))))))
              (insert archive))
            (mevedel-transcript-restore-properties)
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)
            (insert "pending prompt\n")
            (set-buffer-modified-p nil))
          (mevedel-execution-transcript-commit-archive
           buffer '(:live (("old-call" :execution-id "exec-old"
                            :state running :live-execution-p t))))
          (should (= 1
                     (length
                      (mevedel-execution-transcript-test--audit-records-in-file
                       path 'execution-archive))))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence-write-current-buffer-atomically)
                     (lambda (&rest _) (error "Publication failed")))
                    ((symbol-function 'display-warning) #'ignore))
            (mevedel-execution-transcript-handle-event
             (list :type 'terminal :session session :data-buffer buffer
                   :owner "main" :tool-use-id "old-call"
                   :facts '(:state completed :outcome success :exit-code 0)
                   :whole-output "done")))
          (with-current-buffer buffer
            (should (gethash "old-call"
                             mevedel-execution-transcript--archived-rows))
            (should (gethash "old-call"
                             mevedel-execution-transcript--pending-terminals)))
          (should (= 1
                     (length
                      (mevedel-execution-transcript-test--audit-records-in-file
                       path 'execution-archive))))
          (with-current-buffer buffer
            (should-not (buffer-modified-p)))
          (mevedel-execution-transcript-retry-pending-terminals buffer)
          (with-current-buffer buffer
            (should-not (buffer-modified-p))
            (should (verify-visited-file-modtime buffer))
            (let ((records
                   (mevedel-transcript-audit-records
                    (buffer-string) 'execution-completion)))
              (should (= 1 (length records)))
              (should (equal "old-call"
                             (plist-get (car records) :tool-use-id))))
            (goto-char (point-max))
            (insert "assistant done\n")
            (save-buffer))
          (with-temp-buffer
            (insert-file-contents path)
            (should (string-search "pending prompt" (buffer-string)))
            (should (string-search "assistant done" (buffer-string))))
          (should (= 1
                     (length
                      (mevedel-execution-transcript-test--audit-records-in-file
                       path 'execution-completion)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p path) (delete-file path))))
  :doc "retries a disk-first partial commit from a narrowed live buffer"
  (let* ((path (make-temp-file "mevedel-execution-partial-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :name "partial"))
         (event
          (list :type 'terminal :session session :data-buffer buffer
                :owner "main" :tool-use-id "partial-call"
                :facts '(:state completed :outcome success :exit-code 0)
                :whole-output "done")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert
             (mevedel-execution-transcript-test--persisted-audit-transcript
              (mevedel-execution-transcript-archive-text
               '(:live (("partial-call" :execution-id "exec-partial"
                         :state running :live-execution-p t))))))
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-restore-properties)
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          (mevedel-execution-transcript-commit-archive
           buffer '(:live (("partial-call" :execution-id "exec-partial"
                            :state running :live-execution-p t))))
          (let ((replace
                 (symbol-function
                  'mevedel-execution-transcript--replace-archived-record)))
            (cl-letf
                (((symbol-function
                   'mevedel-execution-transcript--replace-archived-record)
                  (lambda (&rest args)
                    (if (eq (current-buffer) buffer)
                        (error "Live publication failed")
                      (apply replace args))))
                 ((symbol-function 'display-warning) #'ignore))
              (mevedel-execution-transcript-handle-event event)))
          (should (= 1
                     (length
                      (mevedel-execution-transcript-test--audit-records-in-file
                       path 'execution-completion))))
          (with-current-buffer buffer
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-archive))))
            (narrow-to-region (point-max) (point-max)))
          (mevedel-execution-transcript-retry-pending-terminals buffer)
          (with-current-buffer buffer
            (widen)
            (should-not
             (gethash "partial-call"
                      mevedel-execution-transcript--pending-terminals))
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-completion))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (widen)
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p path) (delete-file path))))
  :doc "reroutes a terminal queued before its archive marker commits"
  (let* ((path (make-temp-file "mevedel-execution-reroute-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :name "reroute"))
         (event
          (list :type 'terminal :session session :data-buffer buffer
                :owner "main" :tool-use-id "reroute-call"
                :facts '(:state completed :outcome success :exit-code 0)
                :whole-output "done")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode)))
          (mevedel-execution-transcript-handle-event event)
          (with-current-buffer buffer
            (should
             (gethash "reroute-call"
                      mevedel-execution-transcript--pending-terminals))
            (insert
             (mevedel-execution-transcript-test--persisted-audit-transcript
              (mevedel-execution-transcript-archive-text
               '(:live (("reroute-call" :execution-id "exec-reroute"
                         :state running :live-execution-p t))))))
            (mevedel-transcript-restore-properties)
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          (mevedel-execution-transcript-commit-archive
           buffer '(:live (("reroute-call" :execution-id "exec-reroute"
                            :state running :live-execution-p t))))
          (mevedel-execution-transcript-retry-pending-terminals buffer)
          (with-current-buffer buffer
            (should-not
             (gethash "reroute-call"
                      mevedel-execution-transcript--pending-terminals))
            (should (= 1
                       (length
                        (mevedel-transcript-audit-records
                         (buffer-string) 'execution-completion))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p path) (delete-file path))))
  :doc "updates the committed remote transcript instead of its fixed cache"
  (let* ((host "archived-terminal-publication")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-archived-terminal-" t)))
         (remote-root
          (format "/mevedelmock:%s:%s/"
                  host (directory-file-name local-root)))
         (session-dir (concat remote-root "session/"))
         (root-segment (concat session-dir "segment-0001.chat.org"))
         (transcript (concat session-dir "agents/remote-call.chat.org"))
         (sidecar (concat session-dir "session.meta.el"))
         (archive
          (mevedel-execution-transcript-test--persisted-audit-transcript
           (mevedel-execution-transcript-archive-text
            '(:live (("remote-call" :execution-id "exec-remote"
                      :state running :live-execution-p t))))))
         (render-data
          '(:execution-id "exec-remote" :state completed
                          :status success :live-execution-p nil))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (mevedel-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         buffer event root-buffer session)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
                                              (let ((workspace
                                                     (mevedel-workspace--create
                                                      :type 'project :id remote-root :root remote-root
                                                      :name "remote")))
                                                (setq session
                                                      (mevedel-session-create "main" workspace remote-root)))
                                              (setq event
                                                    (list :type 'terminal :session session :owner "main"
                                                          :tool-use-id "remote-call"))
                                              (mevedel-workspace-identity-ensure remote-root)
                                              (mevedel-execution-target-seed-incarnation
                                               (mevedel-session-execution-target session) "mock-incarnation")
                                              (setf (mevedel-session-session-id session) "archived-terminal"
                                                    (mevedel-session-save-path session) session-dir
                                                    (mevedel-session-current-segment session) 1)
                                              (make-directory session-dir t)
                                              (puthash
                                               (mevedel-execution-target-identity
                                                (mevedel-session-execution-target session))
                                               t mevedel-session-durability--disclosed-targets)
                                              (should
                                               (mevedel-session-durability-lease-acquire
                                                session-dir "*archived terminal*" session))
                                              (setq root-buffer (generate-new-buffer " *archived root*"))
                                              (with-current-buffer root-buffer
                                                (setq-local mevedel--session session)
                                                (setq buffer-file-name root-segment)
                                                (insert "* Root\n"))
                                              (setq buffer (generate-new-buffer " *archived terminal*"))
                                              (with-current-buffer buffer
                                                (setq-local mevedel--session session)
                                                (setq-local
                                                 mevedel--agent-invocation
                                                 (mevedel-agent-invocation--create
                                                  :parent-data-buffer root-buffer))
                                                (setq buffer-file-name transcript)
                                                (insert archive)
                                                (delay-mode-hooks (org-mode))
                                                (mevedel-transcript-restore-properties))
                                              (should
                                               (mevedel-session-publication-publish
                                                session
                                                (list
                                                 (list :path transcript :content archive)
                                                 (list
                                                  :path sidecar
                                                  :content
                                                  (mevedel-session-artifacts-printed-value
                                                   (mevedel-session-artifacts-build-sidecar
                                                    session root-buffer))
                                                  :commit-marker t))))
                                              (write-region "poisoned fixed cache" nil transcript nil 'silent)
                                              (with-current-buffer buffer
                                                (goto-char (point-max))
                                                (insert "pending prompt\n")
                                                (set-buffer-modified-p nil))
                                              (mevedel-execution-transcript-commit-archive
                                               buffer
                                               '(:live (("remote-call" :execution-id "exec-remote"
                                                         :state running :live-execution-p t))))
                                              (mevedel-execution-transcript--record-archived-terminal
                                               buffer event render-data)
                                              (let ((published
                                                     (decode-coding-string
                                                      (mevedel-session-artifacts-read-artifact
                                                       session "agents/remote-call.chat.org" t)
                                                      'utf-8-unix)))
                                                (should-not (string-search "poisoned" published))
                                                (should-not (string-search "pending prompt" published))
                                                (should (= 1
                                                           (length
                                                            (mevedel-execution-transcript-test--restored-audit-records
                                                             published 'execution-completion)))))
                                              (with-current-buffer buffer
                                                (should (string-search "pending prompt" (buffer-string)))
                                                (should (= 1
                                                           (length
                                                            (mevedel-transcript-audit-records
                                                             (buffer-string) 'execution-completion))))))
      (when (and session (mevedel-session-lease session))
        (ignore-errors
          (mevedel-session-durability-lease-release session-dir session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (buffer-live-p root-buffer)
        (with-current-buffer root-buffer (set-buffer-modified-p nil))
        (kill-buffer root-buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(provide 'test-mevedel-execution-transcript)

;;; test-mevedel-execution-transcript.el ends here
