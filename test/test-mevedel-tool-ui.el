;;; test-mevedel-tool-ui.el --- Interaction tool assembly tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the small user-interaction tool assembly boundary.  Focused Ask and
;; RequestAccess behavior lives in their mirrored test modules.

;;; Code:

(require 'gptel)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-ui)
(require 'mevedel-view)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-ui--deliver-result ()
  ,test
  (test)
  :doc "wraps raw results in the canonical envelope"
  (let (delivered)
    (mevedel-tool-ui--deliver-result
     (lambda (value) (setq delivered value))
     "done")
    (should (equal '(:result "done") delivered)))

  :doc "preserves result metadata envelopes"
  (let ((envelope '(:result "done" :render-data (:kind card)
                    :media ((:type image))))
        delivered)
    (mevedel-tool-ui--deliver-result
     (lambda (value) (setq delivered value))
     envelope)
    (should (eq envelope delivered))))

(mevedel-deftest mevedel-tool-ui--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "assembles the unchanged user-interaction tool surface"
  (progn
    (mevedel-tool-ui--register)
    (dolist (name '("Ask" "RequestAccess" "Agent" "StopAgent"
                    "ToolSearch" "SendMessage"))
      (should (mevedel-tool-get name))))

  :doc "keeps RequestAccess.directory as a semantic path"
  (progn
    (mevedel-tool-ui--register)
    (should
     (eq 'path
         (cadr (assq 'directory
                     (mevedel-tool-args
                      (mevedel-tool-get "RequestAccess"))))))))


;;; Expand/collapse

(mevedel-deftest mevedel-tool-ui--render-agent-body
  (:doc "selects the correct Agent expanded body for foreground and background rows")
  ,test
  (test)

  :doc "agent description truncation never exceeds narrow widths"
  (progn
    (should (equal "" (mevedel-tool-ui--compact-agent-description
                       "long task" 0)))
    (should (equal "." (mevedel-tool-ui--compact-agent-description
                       "long task" 1)))
    (should (equal ".." (mevedel-tool-ui--compact-agent-description
                        "long task" 2)))
    (should (equal "..." (mevedel-tool-ui--compact-agent-description
                         "long task" 3)))
    (should (<= (string-width
                 (mevedel-tool-ui--compact-agent-description
                  "long task" 2))
                2)))

  :doc "foreground Agent rows still render the final response"
  (mevedel-view-test--with-buffers
    (let* ((args '(:subagent_type "explorer" :description "Task"))
           (rd '(:kind agent-transcript
                 :agent-id "explorer--fg"
                 :status completed
                 :activity ((:type tool-start :tool-name "Read"))))
           (rendering (mevedel-tool-ui--render-agent
                       "Agent" args "final response body" rd)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--render-expanded-body rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "final response body" text))
          (should-not (string-match-p "Read" text)))))))

  :doc "Agent rows render SubagentStart hook context as a compact audit note"
  (mevedel-view-test--with-buffers
    (let* ((args '(:subagent_type "explorer" :description "Task"))
           (rd '(:kind agent-transcript
                 :agent-id "explorer--hook"
                 :status running
                 :hook-audits
                 ((:type subagent-context
                   :event "SubagentStart"))))
           (rendering (mevedel-tool-ui--render-agent
                       "Agent" args "launch body" rd)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "hook added sub-agent context" text))
          (should-not (string-match-p "extra start context" text))))))

  :doc "Agent handle transcript click target is the visible type label"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abcdef1234567890")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "agent-target"
                       :root temporary-file-directory
                       :name "agent-target"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-agent-target-session")))
           (args '(:subagent_type "explorer" :description "Task"))
           (rd `(:kind agent-transcript
                 :agent-id ,agent-id
                 :status completed))
           rendering)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abcdef12.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq rendering (mevedel-tool-ui--render-agent
                         "Agent" args "final response body" rd))
        (let ((inhibit-read-only t)
              start)
          (goto-char mevedel-view--input-marker)
          (setq start (point))
          (mevedel-view--insert-rendered-tool rendering (cons 1 1))
          (mevedel-view--add-display-region-properties
           start (point) 'agent-handle)
          (should
           (string-search
            "Agent: explorer -- Task"
            (buffer-substring-no-properties start (point))))
          (goto-char start)
          (search-forward "Agent: explorer")
          (search-backward "explorer")
          (should (eq (get-text-property (point) 'keymap)
                      mevedel-view--agent-label-map))
          (should (equal agent-id
                         (get-text-property
                          (point) 'mevedel-view-agent-id)))
          (should
           (lookup-key (get-text-property (point) 'keymap) [mouse-1])))))

  :doc "Agent handle header normalizes long task text to one line"
  (let* ((args '(:subagent_type "coordinator"
                 :description "Run validation.\nThen repeat until green."))
         (rd '(:kind agent-transcript
               :agent-id "coordinator--abcdef123456"
               :status running
               :calls 9))
         (mevedel-tool-ui-agent-description-width 30)
         (rendering (mevedel-tool-ui--render-agent
                     "Agent" args "launch status" rd))
         (header (plist-get rendering :header)))
    (should (string-match-p
             "Agent: coordinator -- Run validation\\. Then repeat\\.\\.\\."
             header))
    (should-not (string-match-p "\n" header))
    (should (string-match-p "\\[running · 9 calls\\]" header))))

(provide 'test-mevedel-tool-ui)

;;; test-mevedel-tool-ui.el ends here
