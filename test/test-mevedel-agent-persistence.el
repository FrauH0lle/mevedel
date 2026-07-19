;;; test-mevedel-agent-persistence.el --- Durable agent storage tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests retained-agent registry codecs, validation, and cold hydration.

;;; Code:

(require 'mevedel-agent-control)
(require 'mevedel-agent-persistence)
(require 'mevedel-agents)
(require 'mevedel-reminders)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-tools)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `gptel'
(defvar gptel-backend)

;; `gptel-request'
(declare-function gptel--make-backend "ext:gptel-request" (&rest slots))

(mevedel-tools-register)

(defun mevedel-agent-persistence-test--backend ()
  "Return the registered backend used by persistence codec tests."
  (let ((name "Agent Persistence Test"))
    (condition-case nil
        (gptel-get-backend name)
      (user-error
       (let ((backend (gptel--make-backend :name name)))
         (setf (gptel-get-backend name) backend)
         backend)))))

(defun mevedel-agent-persistence-test--configuration (&optional context)
  "Return a small frozen configuration with optional gptel CONTEXT."
  (mevedel-agent-configuration--create
   :agent
   (mevedel-agent--create
    :name "default"
    :description "Persisted default agent"
    :tools '((:tool "Read"))
    :system-prompt "Frozen instructions"
    :max-turns 12
    :hook-rules nil
    :frozen-p t)
   :request-locals
   (list (cons 'gptel-backend
               (or gptel-backend
                   (mevedel-agent-persistence-test--backend)))
         (cons 'gptel-model 'test-model)
         (cons 'gptel-tools
               (list (gptel-get-tool '("mevedel" "Read"))))
         (cons 'gptel-context context))))

(defun mevedel-agent-persistence-test--session (&optional root)
  "Return a fresh session rooted at optional ROOT."
  (mevedel-session-create
   "main"
   (mevedel-workspace--create
    :type 'project
    :id "agent-persistence"
    :root (or root temporary-file-directory)
    :name "agent-persistence")))

(defun mevedel-agent-persistence-test--record
    (&optional path parent id activity)
  "Return one retained fixture record with optional identity fields."
  (mevedel-agent-record--create
   :id (or id "opaque-id")
   :path (or path "/root/task")
   :parent-path (or parent "/root")
   :role "default"
   :configuration (mevedel-agent-persistence-test--configuration)
   :activity (or activity 'idle)
   :conversation-location "agents/task.chat.org"))

(mevedel-deftest mevedel-agent-persistence--invalid ()
  ,test
  (test)
  :doc "classifies malformed sidecar data with the dedicated condition"
  (should-error
   (mevedel-agent-persistence--invalid "Bad field: %s" 'value)
   :type 'mevedel-agent-persistence-invalid-data))

(mevedel-deftest mevedel-agent-persistence--encode-local ()
  ,test
  (test)
  :doc "encodes backends, tools, contexts, and ordinary plain values"
  (let ((backend (mevedel-agent-persistence-test--backend))
        (tool (gptel-get-tool '("mevedel" "Read"))))
    (should (stringp
             (mevedel-agent-persistence--encode-local
              'gptel-backend backend)))
    (should
     (equal '(("mevedel" "Read"))
            (mevedel-agent-persistence--encode-local
             'gptel-tools (list tool))))
    (should
     (equal '("/tmp/plain.el" ("/tmp/region.el" 1 2))
            (mevedel-agent-persistence--encode-local
             'gptel-context
             '("/tmp/plain.el" ("/tmp/region.el" 1 2)))))
    (should (eq 'read
                (mevedel-agent-persistence--encode-local
                 'gptel-model 'read))))
  :doc "rejects non-plain request-local values"
  (should-error
   (mevedel-agent-persistence--encode-local
    'gptel-model (lambda () t))))

(mevedel-deftest mevedel-agent-persistence--encode-configuration ()
  ,test
  (test)
  :doc "encodes the frozen role and request-local contracts"
  (let ((encoded
         (mevedel-agent-persistence--encode-configuration
          (mevedel-agent-persistence-test--configuration))))
    (should (equal "default"
                   (plist-get (plist-get encoded :agent) :name)))
    (should (assq 'gptel-backend
                  (plist-get encoded :request-locals))))
  :doc "rejects a missing frozen configuration"
  (should-error (mevedel-agent-persistence--encode-configuration nil)))

(mevedel-deftest mevedel-agent-persistence--serialize-record ()
  ,test
  (test)
  :doc "encodes one valid identity and reserves its path and opaque ID"
  (let* ((record (mevedel-agent-persistence-test--record))
         (entry (cons "/root/task" record))
         (paths (make-hash-table :test #'equal))
         (ids (make-hash-table :test #'equal))
         (encoded
          (mevedel-agent-persistence--serialize-record entry paths ids)))
    (should (equal "/root/task" (plist-get encoded :path)))
    (should (gethash "/root/task" paths))
    (should (gethash "opaque-id" ids))
    (should-error
     (mevedel-agent-persistence--serialize-record entry paths ids))))

(mevedel-deftest mevedel-agent-persistence--identity-p ()
  ,test
  (test)
  :doc "accepts one rooted, path-consistent, previously unseen identity"
  (let ((paths (make-hash-table :test #'equal))
        (ids (make-hash-table :test #'equal)))
    (should
     (mevedel-agent-persistence--identity-p
      "opaque" "/root/task" "/root" "default" 'running
      "agents/task.chat.org" paths ids))
    (puthash "/root/task" t paths)
    (should-not
     (mevedel-agent-persistence--identity-p
      "opaque" "/root/task" "/root" "default" 'running
      "agents/task.chat.org" paths ids))
    (should-not
     (mevedel-agent-persistence--identity-p
      "other" "/root/task/child" "/root/wrong" "default" 'idle
      "../escape.chat.org" (make-hash-table :test #'equal) ids))))

(mevedel-deftest mevedel-agent-persistence--decode-local ()
  ,test
  (test)
  :doc "restores registered backends, tools, contexts, and plain symbols"
  (let* ((backend (mevedel-agent-persistence-test--backend))
         (backend-name (gptel-backend-name backend)))
    (should (eq backend
                (mevedel-agent-persistence--decode-local
                 'gptel-backend backend-name)))
    (should
     (gptel-tool-p
      (car (mevedel-agent-persistence--decode-local
            'gptel-tools '(("mevedel" "Read"))))))
    (should
     (equal '("/tmp/plain.el")
            (mevedel-agent-persistence--decode-local
             'gptel-context '("/tmp/plain.el"))))
    (should (eq 'read
                (mevedel-agent-persistence--decode-local
                 'gptel-model 'read))))
  :doc "classifies unknown registered values as invalid persisted data"
  (should-error
   (mevedel-agent-persistence--decode-local
    'gptel-backend "Missing Backend")
   :type 'mevedel-agent-persistence-invalid-data))

(mevedel-deftest mevedel-agent-persistence--decode-configuration ()
  ,test
  (test)
  :doc "restores one encoded frozen configuration"
  (let* ((configuration
          (mevedel-agent-persistence-test--configuration))
         (encoded
          (mevedel-agent-persistence--encode-configuration configuration))
         (decoded
          (mevedel-agent-persistence--decode-configuration
           encoded "default")))
    (should (mevedel-agent-configuration-p decoded))
    (should
     (equal "default"
            (mevedel-agent-name
             (mevedel-agent-configuration-agent decoded))))
    (should-error
     (mevedel-agent-persistence--decode-configuration encoded "other")
     :type 'mevedel-agent-persistence-invalid-data)))

(mevedel-deftest mevedel-agent-persistence--deserialize-record ()
  ,test
  (test)
  :doc "decodes one identity and rejects a duplicate in the same registry"
  (let* ((record (mevedel-agent-persistence-test--record))
         (encoded
          (mevedel-agent-persistence--serialize-record
           (cons "/root/task" record)
           (make-hash-table :test #'equal)
           (make-hash-table :test #'equal)))
         (paths (make-hash-table :test #'equal))
         (ids (make-hash-table :test #'equal))
         (decoded
          (mevedel-agent-persistence--deserialize-record encoded paths ids)))
    (should (equal "/root/task" (car decoded)))
    (should (mevedel-agent-record-p (cdr decoded)))
    (should-error
     (mevedel-agent-persistence--deserialize-record encoded paths ids)
     :type 'mevedel-agent-persistence-invalid-data)))

(mevedel-deftest mevedel-agent-persistence--drop-orphans ()
  ,test
  (test)
  :doc "removes an absent-parent branch while retaining rooted identities"
  (let ((valid (mevedel-agent-record--create :parent-path "/root"))
        (orphan (mevedel-agent-record--create :parent-path "/root/missing"))
        (descendant
         (mevedel-agent-record--create :parent-path "/root/orphan")))
    (should
     (equal '("/root/valid")
            (mapcar
             #'car
             (mevedel-agent-persistence--drop-orphans
              (list (cons "/root/valid" valid)
                    (cons "/root/orphan" orphan)
                    (cons "/root/orphan/child" descendant))))))))

(mevedel-deftest mevedel-agent-persistence-transcript-path-p ()
  ,test
  (test)
  :doc "accepts well-formed relative paths under agents/"
  (let ((tmp (file-name-as-directory (make-temp-file "agent-path-" t))))
    (unwind-protect
        (should (mevedel-agent-persistence-transcript-path-p
                 "agents/task.chat.org" tmp))
      (delete-directory tmp t)))
  :doc "rejects absolute paths"
  (let ((tmp (file-name-as-directory (make-temp-file "agent-path-" t))))
    (unwind-protect
        (should-not
         (mevedel-agent-persistence-transcript-path-p
          "/etc/passwd.chat.org" tmp))
      (delete-directory tmp t)))
  :doc "rejects paths with parent segments"
  (let ((tmp (file-name-as-directory (make-temp-file "agent-path-" t))))
    (unwind-protect
        (should-not
         (mevedel-agent-persistence-transcript-path-p
          "agents/../escape.chat.org" tmp))
      (delete-directory tmp t)))
  :doc "rejects non-transcript suffixes"
  (let ((tmp (file-name-as-directory (make-temp-file "agent-path-" t))))
    (unwind-protect
        (should-not
         (mevedel-agent-persistence-transcript-path-p
          "agents/transcript.txt" tmp))
      (delete-directory tmp t)))
  :doc "rejects paths outside the agents directory"
  (let ((tmp (file-name-as-directory (make-temp-file "agent-path-" t))))
    (unwind-protect
        (should-not
         (mevedel-agent-persistence-transcript-path-p
          "outside/file.chat.org" tmp))
      (delete-directory tmp t)))
  :doc "rejects an existing transcript symlink that escapes agents/"
  (let* ((tmp (file-name-as-directory (make-temp-file "agent-path-" t)))
         (agents (file-name-concat tmp "agents"))
         (outside (make-temp-file "agent-path-outside-" nil ".chat.org"))
         (link (file-name-concat agents "linked.chat.org")))
    (unwind-protect
        (progn
          (make-directory agents t)
          (make-symbolic-link outside link)
          (should-not
           (mevedel-agent-persistence-transcript-path-p
            "agents/linked.chat.org" tmp)))
      (when (file-exists-p link) (delete-file link))
      (when (file-exists-p outside) (delete-file outside))
      (delete-directory tmp t)))
  :doc "rejects nil and empty paths"
  (should-not (mevedel-agent-persistence-transcript-path-p nil "/tmp/"))
  (should-not (mevedel-agent-persistence-transcript-path-p "" "/tmp/")))

(mevedel-deftest mevedel-agent-persistence-sanitize-mailbox ()
  ,test
  (test)
  :doc "keeps canonical MAIL, RESULT, and USER records in stored queue order"
  (let* ((newer (list 2 0 0 0))
         (older (list 1 0 0 0))
         (raw
          (list
           (list :type 'RESULT :sender "/root/child" :recipient "/root"
                 :outcome 'completed :payload "done" :timestamp newer)
           (list :type 'MAIL :sender "/root/child" :recipient "/root"
                 :payload "hello" :timestamp older)
           (list :type 'MAIL :sender "bad" :recipient "/root"
                 :payload "drop" :timestamp older)))
         (clean (mevedel-agent-persistence-sanitize-mailbox raw "/root")))
    (should (= 2 (length clean)))
    (should (eq 'RESULT (plist-get (car clean) :type)))
    (should (equal "hello" (plist-get (cadr clean) :payload))))
  :doc "drops wrong recipients, malformed outcomes, payloads, and timestamps"
  (dolist (entry
           (list
            (list :type 'MAIL :sender "/root/a" :recipient "/root/b"
                  :payload "x" :timestamp '(1 0 0 0))
            (list :type 'RESULT :sender "/root/a" :recipient "/root"
                  :outcome 'future :payload "x" :timestamp '(1 0 0 0))
            (list :type 'MAIL :sender "/root/a" :recipient "/root"
                  :payload 4 :timestamp '(1 0 0 0))
            (list :type 'MAIL :sender "/root/a" :recipient "/root"
                  :payload "x" :timestamp 'bad)))
    (should-not
     (mevedel-agent-persistence-sanitize-mailbox (list entry) "/root"))))

(mevedel-deftest mevedel-agent-persistence-serialize-registry ()
  ,test
  (test)
  :doc "encodes identity, activity, configuration, file context, and mailbox"
  (let* ((session (mevedel-agent-persistence-test--session))
         (live-context (generate-new-buffer " *agent-live-context*"))
         (configuration
          (mevedel-agent-persistence-test--configuration
           (list "/tmp/plain.el" '("/tmp/durable.el")
                 (list live-context 1 2))))
         (record
          (mevedel-agent-record--create
           :id "opaque-id"
           :path "/root/task"
           :parent-path "/root"
           :role "default"
           :configuration configuration
           :activity 'waiting
           :conversation-location "agents/task.chat.org"
           :mailbox
           (list (list :type 'MAIL :sender "/root"
                       :recipient "/root/task" :payload "news"
                       :timestamp '(1 0 0 0))))))
    (unwind-protect
        (progn
          (setf (mevedel-session-agent-registry session)
                (list (cons "/root/task" record)))
          (let* ((raw (mevedel-agent-persistence-serialize-registry session))
                 (saved (car raw))
                 (locals
                  (plist-get (plist-get saved :configuration)
                             :request-locals)))
            (should (equal "opaque-id" (plist-get saved :id)))
            (should (eq 'waiting (plist-get saved :activity)))
            (should (equal '("/tmp/plain.el" ("/tmp/durable.el"))
                           (cdr (assq 'gptel-context locals))))
            (should
             (equal '(("mevedel" "Read"))
                    (cdr (assq 'gptel-tools locals))))
            (should (equal "news"
                           (plist-get (car (plist-get saved :mailbox))
                                      :payload)))))
      (kill-buffer live-context)))

  :doc "fails a save instead of silently omitting invalid live state"
  (let* ((session (mevedel-agent-persistence-test--session))
         (record
          (mevedel-agent-record--create
           :id "broken" :path "/root/broken" :parent-path "/root"
           :role "default" :configuration nil :activity 'idle
           :conversation-location "agents/broken.chat.org")))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/broken" record)))
    (should-error (mevedel-agent-persistence-serialize-registry session))))

(mevedel-deftest mevedel-agent-persistence-deserialize-registry ()
  ,test
  (test)
  :doc "round-trips frozen configuration and retained identity data"
  (let* ((source (mevedel-agent-persistence-test--session))
         (configuration (mevedel-agent-persistence-test--configuration
                         '("/tmp/plain.el" ("/tmp/durable.el"))))
         (_ (setf
             (mevedel-agent-reminders
              (mevedel-agent-configuration-agent configuration))
             (list (mevedel-reminders-make-verifier-read-only))))
         (record
          (mevedel-agent-record--create
           :id "opaque-id" :path "/root/task" :parent-path "/root"
           :role "default" :configuration configuration :activity 'idle
           :conversation-location "agents/task.chat.org")))
    (setf (mevedel-session-agent-registry source)
          (list (cons "/root/task" record)))
    (let* ((raw (mevedel-agent-persistence-serialize-registry source))
           (mevedel-agent--default
            (mevedel-agent--create
             :name "default"
             :description "Redefined role"
             :reminders
             (list (mevedel-reminders-make-reviewer-read-only))))
           (restored
            (mevedel-agent-persistence-deserialize-registry raw))
           (result (cdar restored))
           (restored-agent
            (mevedel-agent-configuration-agent
             (mevedel-agent-record-configuration result)))
           (locals
            (mevedel-agent-configuration-request-locals
             (mevedel-agent-record-configuration result))))
      (should (equal "opaque-id" (mevedel-agent-record-id result)))
      (should (gptel-backend-p (alist-get 'gptel-backend locals)))
      (should (gptel-tool-p (car (alist-get 'gptel-tools locals))))
      (should (equal '("/tmp/plain.el" ("/tmp/durable.el"))
                     (alist-get 'gptel-context locals)))
      (should
       (equal '(verifier-read-only)
              (mapcar #'mevedel-reminder-type
                      (mevedel-agent-reminders restored-agent))))))

  :doc "drops identities whose frozen backend or tool is no longer known"
  (let* ((source (mevedel-agent-persistence-test--session))
         (record
          (mevedel-agent-record--create
           :id "valid" :path "/root/valid" :parent-path "/root"
           :role "default"
           :configuration (mevedel-agent-persistence-test--configuration)
           :activity 'idle
           :conversation-location "agents/valid.chat.org")))
    (setf (mevedel-session-agent-registry source)
          (list (cons "/root/valid" record)))
    (let* ((raw (mevedel-agent-persistence-serialize-registry source))
           (locals
            (plist-get (plist-get (car raw) :configuration)
                       :request-locals)))
      (setcdr (assq 'gptel-backend locals) "Missing Backend")
      (should-not (mevedel-agent-persistence-deserialize-registry raw)))
    (let* ((raw (mevedel-agent-persistence-serialize-registry source))
           (locals
            (plist-get (plist-get (car raw) :configuration)
                       :request-locals)))
      (setcdr (assq 'gptel-tools locals) '(("missing" "tool")))
      (should-not (mevedel-agent-persistence-deserialize-registry raw))))

  :doc "drops an identity whose trusted reminder key has invalid arguments"
  (let* ((source (mevedel-agent-persistence-test--session))
         (record (mevedel-agent-persistence-test--record))
         (_ (setf (mevedel-session-agent-registry source)
                  (list (cons "/root/task" record))))
         (raw (mevedel-agent-persistence-serialize-registry source))
         (agent-data
          (plist-get (plist-get (car raw) :configuration) :agent)))
    (setf (plist-get agent-data :reminders) '((verifier-read-only 1)))
    (should-not (mevedel-agent-persistence-deserialize-registry raw)))

  :doc "drops malformed identities and every descendant of a rejected parent"
  (let* ((source (mevedel-agent-persistence-test--session))
         (configuration (mevedel-agent-persistence-test--configuration))
         (valid
          (mevedel-agent-record--create
           :id "valid" :path "/root/valid" :parent-path "/root"
           :role "default" :configuration configuration :activity 'idle
           :conversation-location "agents/valid.chat.org"))
         (bad-parent (copy-mevedel-agent-record valid))
         (child (copy-mevedel-agent-record valid)))
    (setf (mevedel-agent-record-id bad-parent) "bad"
          (mevedel-agent-record-path bad-parent) "/root/bad"
          (mevedel-agent-record-conversation-location bad-parent)
          "agents/bad.chat.org"
          (mevedel-agent-record-id child) "child"
          (mevedel-agent-record-path child) "/root/bad/child"
          (mevedel-agent-record-parent-path child) "/root/bad"
          (mevedel-agent-record-conversation-location child)
          "agents/child.chat.org")
    (setf (mevedel-session-agent-registry source)
          (list (cons "/root/valid" valid)
                (cons "/root/bad" bad-parent)
                (cons "/root/bad/child" child)))
    (let ((raw (mevedel-agent-persistence-serialize-registry source)))
      (setf (plist-get (cadr raw) :conversation-location)
            "../escape.chat.org")
      (should
       (equal '("/root/valid")
              (mapcar #'car
                      (mevedel-agent-persistence-deserialize-registry raw))))))

  :doc "propagates unexpected decoder bugs instead of dropping identities"
  (let* ((source (mevedel-agent-persistence-test--session))
         (record
          (mevedel-agent-record--create
           :id "valid" :path "/root/valid" :parent-path "/root"
           :role "default"
           :configuration (mevedel-agent-persistence-test--configuration)
           :activity 'idle
           :conversation-location "agents/valid.chat.org")))
    (setf (mevedel-session-agent-registry source)
          (list (cons "/root/valid" record)))
    (let ((raw (mevedel-agent-persistence-serialize-registry source)))
      (cl-letf (((symbol-function
                  'mevedel-reminders-restore-agent-templates)
                 (lambda (_recipes) (error "Decoder regression"))))
        (should-error
         (mevedel-agent-persistence-deserialize-registry raw))))))

(mevedel-deftest mevedel-agent-persistence-restore-tree ()
  ,test
  (test)
  :doc "hydrates a retained conversation and drops only an escaping identity"
  (let* ((root-dir (make-temp-file "mevedel-agent-tree-" t))
         (session (mevedel-agent-persistence-test--session root-dir))
         (root-buffer (generate-new-buffer " *agent-tree-root*"))
         (agents-dir (file-name-concat root-dir "agents"))
         (relative "agents/valid.chat.org")
         (conversation (expand-file-name relative root-dir))
         (configuration (mevedel-agent-persistence-test--configuration))
         (valid
          (mevedel-agent-record--create
           :id "valid" :path "/root/valid" :parent-path "/root"
           :role "default" :configuration configuration :activity 'idle
           :conversation-location relative))
         (escaping
          (mevedel-agent-record--create
           :id "escaping" :path "/root/escaping" :parent-path "/root"
           :role "default" :configuration configuration :activity 'idle
           :conversation-location "../escape.chat.org")))
    (unwind-protect
        (progn
          (make-directory agents-dir t)
          (write-region "* Agent Task: valid\nDurable context.\n"
                        nil conversation nil 'silent)
          (setf (mevedel-session-save-path session) root-dir
                (mevedel-session-agent-registry session)
                (list (cons "/root/valid" valid)
                      (cons "/root/escaping" escaping)))
          (with-current-buffer root-buffer
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace
                        (mevedel-session-workspace session)))
          (should (= 1 (mevedel-agent-persistence-restore-tree
                        session root-buffer nil)))
          (should (equal '("/root/valid")
                         (mapcar #'car
                                 (mevedel-session-agent-registry session))))
          (should
           (buffer-live-p
            (mevedel-agent-record-conversation-buffer valid))))
      (mevedel-agent-control-teardown-session session)
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (delete-directory root-dir t)))

  :doc "normalizes read-only persisted activity without a live invocation"
  (let* ((root-dir (make-temp-file "mevedel-agent-readonly-tree-" t))
         (session (mevedel-agent-persistence-test--session root-dir))
         (root-buffer (generate-new-buffer " *agent-readonly-root*"))
         (relative "agents/active.chat.org")
         (conversation (expand-file-name relative root-dir))
         (record
          (mevedel-agent-record--create
           :id "active" :path "/root/active" :parent-path "/root"
           :role "default"
           :configuration (mevedel-agent-persistence-test--configuration)
           :activity 'running
           :blockers (list 'persisted)
           :conversation-location relative)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory conversation) t)
          (write-region "* Agent Task: active\nDurable context.\n"
                        nil conversation nil 'silent)
          (setf (mevedel-session-save-path session) root-dir
                (mevedel-session-agent-registry session)
                (list (cons "/root/active" record)))
          (with-current-buffer root-buffer
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace
                        (mevedel-session-workspace session)))
          (should (= 0 (mevedel-agent-persistence-restore-tree
                        session root-buffer t)))
          (should (eq 'idle (mevedel-agent-record-activity record)))
          (should-not (mevedel-agent-record-invocation record))
          (should-not (mevedel-agent-record-blockers record))
          (should-not (mevedel-agent-control-active-turn-p session))
          (should (equal '(:path "/root/active" :previous-activity idle)
                         (mevedel-agent-control-interrupt
                          session "/root/active"))))
      (mevedel-agent-control-teardown-session session)
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (delete-directory root-dir t))))

(provide 'test-mevedel-agent-persistence)

;;; test-mevedel-agent-persistence.el ends here
