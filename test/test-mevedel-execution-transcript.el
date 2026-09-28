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
(require 'mevedel-execution)
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
      (should-not (plist-get render-data :live-execution-p))))
  :doc "records exactly one yielded completion, not foreground settlement"
  (with-temp-buffer
    (let ((event (list :type 'terminal :data-buffer (current-buffer)
                       :tool-use-id "tool-one" :owner "/root"
                       :whole-output "ok"
                       :facts '(:execution-id "exec-one" :command "true"
                               :state completed :outcome success))))
      (mevedel-execution-transcript-handle-event event)
      (mevedel-execution-transcript-handle-event event)
      (should (= 1 (length (mevedel-transcript-audit-records
                            (buffer-string) 'execution-breadcrumb))))
      (should (equal "exec-one"
                     (plist-get
                      (car (mevedel-transcript-audit-records
                            (buffer-string) 'execution-breadcrumb))
                      :execution-id)))
      (mevedel-execution-transcript-handle-event
       (list :type 'terminal :data-buffer (current-buffer)
             :tool-use-id "tool-foreground" :owner "/root"
             :facts '(:execution-id nil :command "true" :state completed
                     :outcome success)))
      (should (= 1 (length (mevedel-transcript-audit-records
                            (buffer-string) 'execution-breadcrumb))))))
  :doc "distinguishes simultaneous identical commands by execution identity"
  (with-temp-buffer
    (set-buffer-modified-p nil)
    (dolist (id '("exec-a" "exec-b"))
      (mevedel-execution-transcript-handle-event
       (list :type 'terminal :data-buffer (current-buffer)
             :tool-use-id id :owner "/root"
             :facts (list :execution-id id :command "sleep 1"
                          :state 'completed :outcome 'success))))
    (should (= 2 (length (mevedel-transcript-audit-records
                          (buffer-string) 'execution-breadcrumb))))
    (should (buffer-modified-p)))
  :doc "retains a new completion after reload when the execution counter restarts"
  (let (saved old-id new-id)
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
      (setq old-id (mevedel-execution--next-id
                    (mevedel-execution--new-state)))
      (mevedel-execution-transcript-handle-event
       (list :type 'terminal :data-buffer (current-buffer)
             :owner "/root" :tool-use-id "before-reload"
             :facts (list :execution-id old-id :command "echo BEFORE"
                          :state 'completed :outcome 'success)))
      (mevedel-session-artifacts-stabilize-gptel-bounds)
      (setq saved (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (org-mode)
      (insert saved)
      (mevedel-transcript-restore-properties)
      (setq new-id (mevedel-execution--next-id
                    (mevedel-execution--new-state)))
      (mevedel-execution-transcript-handle-event
       (list :type 'terminal :data-buffer (current-buffer)
             :owner "/root" :tool-use-id "after-reload"
             :facts (list :execution-id new-id :command "echo AFTER"
                          :state 'completed :outcome 'success)))
      (should-not (equal old-id new-id))
      (should (equal '("before-reload" "after-reload")
                     (mapcar (lambda (record) (plist-get record :tool-use-id))
                             (mevedel-transcript-audit-records
                              (buffer-string) 'execution-breadcrumb)))))))

(mevedel-deftest mevedel-execution-transcript--record-breadcrumb ()
  ,test
  (test)
  :doc "persists one completion without writing an unsent in-memory draft"
  (let* ((path (make-temp-file "mevedel-execution-breadcrumb-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :authority-mode 'pid-lock
                                           :name "breadcrumb")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)
            (insert "draft not yet sent\n"))
          (let ((event
                 (list :type 'terminal :data-buffer buffer :owner "/root"
                       :tool-use-id "call-b" :emitted-at 17.0
                       :facts '(:execution-id "exec-b" :command "exit 1"
                               :state completed :outcome failure :exit-code 1))))
            (mevedel-execution-transcript--record-breadcrumb event)
            (mevedel-execution-transcript--record-breadcrumb event))
          (with-current-buffer buffer
            (should (buffer-modified-p))
            (should (= 1 (length (mevedel-transcript-audit-records
                                  (buffer-string) 'execution-breadcrumb)))))
          (should (= 1 (length
                        (mevedel-execution-transcript-test--audit-records-in-file
                         path 'execution-breadcrumb))))
          (with-temp-buffer
            (insert-file-contents path)
            (should-not (string-search "draft not yet sent" (buffer-string)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file path)))
  :doc "keeps a dirty in-memory completion when publication fails"
  (with-temp-buffer
    (let (warning)
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'mevedel-execution-transcript--persist-terminal-record)
                 (lambda (_record) (error "simulated publication failure")))
                ((symbol-function 'display-warning)
                 (lambda (_type message &rest _args) (setq warning message))))
        (mevedel-execution-transcript--record-breadcrumb
         (list :data-buffer (current-buffer) :owner "/root" :tool-use-id "call-x"
               :facts '(:execution-id "exec-x" :command "true"
                        :state completed :outcome success))))
      (should (string-match-p "simulated publication failure" warning))
      (should (buffer-modified-p))
      (should (= 1 (length (mevedel-transcript-audit-records
                            (buffer-string) 'execution-breadcrumb)))))))

(mevedel-deftest mevedel-execution-transcript--breadcrumb-archived-retry ()
  ,test
  (test)
  :doc "a retry after compaction does not append a second completion breadcrumb"
  (let* ((root (make-temp-file "mevedel-breadcrumb-archives-" t))
         (archive (mevedel-session-artifacts-segment-path root 1))
         (session (mevedel-session--create :save-path root
                                           :authority-mode 'pid-lock
                                           :current-segment 3)))
    (unwind-protect
        (with-temp-buffer
          (let* ((current (current-buffer))
                 (event (list :type 'terminal :data-buffer current
                              :owner "/root" :tool-use-id "original"
                              :facts '(:execution-id "exec-1" :command "true"
                                      :state completed :outcome success))))
            (mevedel-execution-transcript--record-breadcrumb event)
            (let ((text (mevedel-execution-transcript-test--persisted-audit-transcript
                         (buffer-string))))
              (with-temp-file archive (insert text)))
            (erase-buffer)
            (setq buffer-file-name
                  (mevedel-session-artifacts-segment-path root 3))
            (setq-local mevedel--session session)
            (should (equal '(readable missing readable)
                           (mapcar (lambda (descriptor)
                                     (plist-get descriptor :status))
                                   (mevedel-session-artifacts-transcript-segments
                                    session current))))
            (let ((before (buffer-list)))
              (cl-letf (((symbol-function
                          'mevedel-execution-transcript--persist-terminal-record)
                         (lambda (&rest _) (ert-fail "Duplicate completion"))))
                (mevedel-execution-transcript--record-breadcrumb event))
              (should-not (cl-set-difference (buffer-list) before)))
            (should-not (mevedel-transcript-audit-records
                         (buffer-string) 'execution-breadcrumb))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-transcript--record-nested-terminal ()
  ,test
  (test)
  :doc "retains a ToolCall child result independently of the ephemeral pending table"
  (let* ((path (make-temp-file "mevedel-execution-nested-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :authority-mode 'pid-lock
                                           :name "nested completion"))
         (event (list :type 'terminal :data-buffer buffer :owner "/root"
                      :tool-use-id "outer/1" :whole-output "whole output"
                      :facts '(:execution-id "exec-nested" :command "printf ok"
                              :state completed :outcome success))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          (mevedel-execution-transcript-handle-event event)
          (mevedel-execution-transcript-handle-event event)
          (with-current-buffer buffer
            (should (equal "whole output"
                           (plist-get
                            (mevedel-execution-transcript-pending-render-data
                             buffer "outer/1")
                            :execution-output)))
            (should (= 1 (length (mevedel-transcript-audit-records
                                  (buffer-string) 'execution-completion)))))
          (should (= 1 (length
                        (mevedel-execution-transcript-test--audit-records-in-file
                         path 'execution-completion))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-restore-properties)
            (let ((mevedel-execution-transcript--pending-terminals nil))
              (should (equal "whole output"
                             (plist-get
                              (mevedel-execution-transcript-pending-render-data
                               buffer "outer/1")
                              :execution-output))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file path))))

(mevedel-deftest mevedel-execution-transcript--persist-terminal-record ()
  ,test
  (test)
  :doc "publishes the terminal Bash row and breadcrumb together, not an unsent draft"
  (let* ((path (make-temp-file "mevedel-execution-row-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :authority-mode 'pid-lock
                                           :name "terminal row")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
                    "#+begin_tool (Bash :command \"printf ok\")\n")
            (let ((start (point)))
              (insert "(:name \"Bash\" :args (:command \"printf ok\"))\n\nrunning"
                      (mevedel-tool-render-data-format
                       '(:execution-id "exec-row" :state running
                         :live-execution-p t) "call-row"))
              (put-text-property start (point) 'gptel '(tool . "call-row")))
            (insert "#+end_tool\n")
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime)
            (should (eq 'running
                        (plist-get
                         (mevedel-tool-render-data-for-tool buffer "call-row")
                         :state)))
            (insert "unsent composer draft\n"))
          (mevedel-execution-transcript-handle-event
           (list :type 'terminal :data-buffer buffer :owner "/root"
                 :tool-use-id "call-row" :whole-output "ok"
                 :facts '(:execution-id "exec-row" :command "printf ok"
                         :state completed :outcome success :exit-code 0)))
          (with-temp-buffer
            (insert-file-contents path)
            (should-not (string-search "unsent composer draft" (buffer-string)))
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-restore-properties)
            (let ((data (mevedel-tool-render-data-for-tool
                         (current-buffer) "call-row")))
              (should (eq 'completed (plist-get data :state)))
              (should (equal "ok" (plist-get data :execution-output)))
              (should-not (plist-get data :live-execution-p)))
            (should (= 1 (length (mevedel-transcript-audit-records
                                  (buffer-string) 'execution-breadcrumb))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file path))))

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
                     (caar (plist-get plan :completed))))))

  :doc "resolves multiple archive misses with one transcript inspection"
  (with-temp-buffer
    (insert
     (mevedel--format-hook-audit-record
      '(:type execution-archive :tool-use-id "archived"
              :render-data (:execution-id "exec-archived" :state running
                            :live-execution-p t))))
    (let ((read-records (symbol-function 'mevedel-transcript-audit-records))
          (reads 0)
          (before (buffer-string)))
      (cl-letf (((symbol-function 'mevedel-transcript-audit-records)
                 (lambda (&rest args)
                   (cl-incf reads)
                   (apply read-records args))))
        (should
         (equal
          '(:live (("archived" :execution-id "exec-archived" :state running
                     :live-execution-p t)) :completed nil)
          (mevedel-execution-transcript-prepare-archive
           (current-buffer) '("missing-first" "archived" "missing-last")))))
      (should (equal-including-properties before (buffer-string)))
      (should (= 1 reads))))

  :doc "keeps live precedence and observes later archive changes without narrowing edits"
  (with-temp-buffer
    (insert
     (mevedel--format-hook-audit-record
      '(:type execution-archive :tool-use-id "row"
              :render-data (:execution-id "old" :state running
                            :live-execution-p t)))
     (mevedel--format-hook-audit-record
      '(:type execution-archive :tool-use-id "row"
              :render-data (:execution-id "later" :state running
                            :live-execution-p t))))
    (save-restriction
      (narrow-to-region (point-max) (point-max))
      (let ((begin (point-min)))
        (should (equal "old"
                       (plist-get
                        (cdar (plist-get
                               (mevedel-execution-transcript-prepare-archive
                                (current-buffer) '("row")) :live))
                        :execution-id)))
        (should (= begin (point-min) (point-max)))))
    (erase-buffer)
    (insert
     (mevedel--format-hook-audit-record
      '(:type execution-archive :tool-use-id "row"
              :render-data (:execution-id "new" :state completed))))
    (should (equal "new"
                   (plist-get
                    (cdar (plist-get
                           (mevedel-execution-transcript-prepare-archive
                            (current-buffer) '("row")) :completed))
                    :execution-id)))
    (insert
     (propertize
      (mevedel-tool-render-data-format
       '(:execution-id "live" :state running :live-execution-p t) "row")
      'gptel '(tool . "row")))
    (cl-letf (((symbol-function 'mevedel-transcript-audit-records)
               (lambda (&rest _) (ert-fail "Live rows should not inspect archives"))))
      (should (equal "live"
                     (plist-get
                      (cdar (plist-get
                             (mevedel-execution-transcript-prepare-archive
                              (current-buffer) '("row")) :live))
                      :execution-id))))))

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
         :execution-output)))))
  :doc "restores archived execution output without a live pending table"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record
             '(:type execution-completion :tool-use-id "archived-call"
               :render-data (:state completed :execution-output "archive output"))))
    (should (equal "archive output"
                   (plist-get
                    (mevedel-execution-transcript-pending-render-data
                     (current-buffer) "archived-call")
                    :execution-output)))))

(mevedel-deftest mevedel-execution-transcript--record-archived-terminal ()
  ,test
  (test)
  :doc "retains failed archived publication through ordinary save and reload"
  (let* ((path (make-temp-file "mevedel-execution-archive-fallback-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :authority-mode 'pid-lock
                                           :name "archive fallback")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (setq-local mevedel--session session)
            (insert (mevedel-execution-transcript-test--persisted-audit-transcript
                     (mevedel-execution-transcript-archive-text
                      '(:live (("failed-call" :execution-id "exec-failed"
                                :state running :live-execution-p t))))))
            (mevedel-transcript-restore-properties)
            (mevedel-transcript-enable-gptel-mode)
            (write-region (point-min) (point-max) path nil 'silent)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          (mevedel-execution-transcript-commit-archive
           buffer '(:live (("failed-call" :execution-id "exec-failed"
                            :state running :live-execution-p t))))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence-write-current-buffer-atomically)
                     (lambda (&rest _) (error "Publication failed")))
                    ((symbol-function 'display-warning) #'ignore))
            (mevedel-execution-transcript-handle-event
             (list :type 'terminal :session session :data-buffer buffer
                   :owner "/root" :tool-use-id "failed-call"
                   :facts '(:execution-id "exec-failed" :command "false"
                           :state completed :outcome failure :exit-code 1)
                   :whole-output "failed output")))
          (with-current-buffer buffer
            (should (buffer-modified-p))
            (should (= 1 (length (mevedel-transcript-audit-records
                                  (buffer-string) 'execution-completion))))
            (save-buffer)
            (should (= 1 (length (mevedel-execution-transcript-test--audit-records-in-file
                                  path 'execution-completion))))
            (kill-buffer buffer))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (delay-mode-hooks (org-mode))
            (mevedel-transcript-restore-properties)
            (should (= 1 (length (mevedel-transcript-audit-records
                                  (buffer-string) 'execution-completion))))
            (should (= 1 (length (mevedel-transcript-audit-records
                                  (buffer-string) 'execution-breadcrumb))))
            (let ((data (cdar (plist-get
                               (mevedel-execution-transcript-prepare-archive
                                buffer '("failed-call")) :completed))))
              (should (eq 'completed (plist-get data :state)))
              (should (equal "failed output"
                             (plist-get data :execution-output))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p path) (delete-file path))))
  :doc "publishes completion transactionally and permits a later save"
  (let* ((path (make-temp-file "mevedel-execution-archive-"))
         (buffer (find-file-noselect path))
         (session (mevedel-session--create :authority-mode 'pid-lock :name "archive")))
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
            (should (buffer-modified-p))
            (should (= 1 (length (mevedel-transcript-audit-records
                                  (buffer-string) 'execution-completion)))))
          (mevedel-execution-transcript-retry-pending-terminals buffer)
          (with-current-buffer buffer
            (should (buffer-modified-p))
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
         (session (mevedel-session--create :authority-mode 'pid-lock :name "partial"))
         (event
          (list :type 'terminal :session session :data-buffer buffer
                :owner "main" :tool-use-id "partial-call"
                :facts '(:state completed :outcome success :exit-code 0)
                :whole-output "done")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session)
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
         (session (mevedel-session--create :authority-mode 'pid-lock :name "reroute"))
         (event
          (list :type 'terminal :session session :data-buffer buffer
                :owner "main" :tool-use-id "reroute-call"
                :facts '(:state completed :outcome success :exit-code 0)
                :whole-output "done")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session)
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
  :doc "updates committed project transcripts through local and remote access"
  (dolist (remote '(nil t))
    (let* ((host "archived-terminal-publication")
           (local-root
            (file-name-as-directory
             (make-temp-file "mevedel-archived-terminal-" t)))
           (remote-root
            (if remote
                (format "/mevedelmock:%s:%s/"
                        host (directory-file-name local-root))
              local-root))
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
                                                (setf (mevedel-execution-target-incarnation
                                                       (mevedel-session-execution-target session))
                                                      "mock-incarnation")
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
                                                (make-directory (file-name-directory transcript) t)
                                                (write-region "poisoned fixed cache" nil transcript nil 'silent)
                                                (with-current-buffer buffer
                                                  (goto-char (point-max))
                                                  (insert "pending prompt\n")
                                                  (set-buffer-modified-p nil))
                                                (mevedel-execution-transcript-commit-archive
                                                 buffer
                                                 '(:live (("remote-call" :execution-id "exec-remote"
                                                           :state running :live-execution-p t))))
                                                (let ((before
                                                       (with-current-buffer buffer
                                                         (buffer-string)))
                                                      (committed
                                                       (mevedel-session-artifacts-read-artifact
                                                        session "agents/remote-call.chat.org" t)))
                                                  (cl-letf
                                                   (((symbol-function
                                                      'mevedel-session-artifacts-publish-transcript-state)
                                                     (lambda (&rest _)
                                                       (error "Publication failed"))))
                                                   (should
                                                    (equal
                                                     '(error "Publication failed")
                                                     (should-error
                                                      (mevedel-execution-transcript--record-archived-terminal
                                                       buffer event render-data)))))
                                                  (with-current-buffer buffer
                                                    (should (equal-including-properties
                                                             before (buffer-string)))
                                                    (should-not (buffer-modified-p))
                                                    (should (gethash
                                                             "remote-call"
                                                             mevedel-execution-transcript--archived-rows)))
                                                  (should (equal
                                                           committed
                                                           (mevedel-session-artifacts-read-artifact
                                                            session "agents/remote-call.chat.org" t))))
                                                (dotimes (_ 2)
                                                  (mevedel-execution-transcript--record-archived-terminal
                                                   buffer event render-data))
                                                (with-current-buffer buffer
                                                  (should-not (gethash
                                                               "remote-call"
                                                               mevedel-execution-transcript--archived-rows)))
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
          (delete-directory local-root t))))))

(provide 'test-mevedel-execution-transcript)

;;; test-mevedel-execution-transcript.el ends here
