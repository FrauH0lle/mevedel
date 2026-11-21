;;; test-niwa-agent-persistence.el --- Durable agent storage tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests retained-agent registry codecs, validation, and cold hydration.

;;; Code:

(require 'niwa-agent-control)
(require 'niwa-agent-conversation)
(require 'niwa-agent-persistence)
(require 'niwa-agent-runtime)
(require 'niwa-agents)
(require 'niwa-execution-target)
(require 'niwa-reminders)
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-structs)
(require 'niwa-tools)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `gptel'
(defvar gptel-backend)

;; `gptel-request'
(declare-function gptel--make-backend "ext:gptel-request" (&rest slots))

(niwa-tools-register)

(defun niwa-agent-persistence-test--backend ()
  "Return the registered backend used by persistence codec tests."
  (let ((name "Agent Persistence Test"))
    (condition-case nil
        (gptel-get-backend name)
      (user-error
       (let ((backend (gptel--make-backend :name name)))
         (setf (gptel-get-backend name) backend)
         backend)))))

(defun niwa-agent-persistence-test--configuration (&optional context)
  "Return a small frozen configuration with optional gptel CONTEXT."
  (niwa-agent-configuration--create
   :agent
   (niwa-agent--create
    :name "default"
    :description "Persisted default agent"
    :tools '((:tool "Read"))
    :system-prompt "Frozen instructions"
    :context-components '(environment skills)
    :max-turns 12
    :hook-rules nil
    :frozen-p t)
   :request-locals
   (mapcar
    (lambda (symbol)
      (cons symbol
            (pcase symbol
              ('gptel-backend
               (or gptel-backend
                   (niwa-agent-persistence-test--backend)))
              ('gptel-model 'test-model)
              ('gptel-tools
               (list (gptel-get-tool '("niwa" "Read"))))
              ('gptel-context context))))
    niwa-agent-request-local-symbols)))

(defun niwa-agent-persistence-test--session (&optional root)
  "Return a fresh session rooted at optional ROOT."
  (niwa-session-create
   "main"
   (niwa-workspace--create
    ;; File sessions are portable: fixtures publish conversations through
    ;; `niwa-agent-persistence-test--publish' instead of staging paths.
    :type 'file
    :id "agent-persistence"
    :root (or root temporary-file-directory)
    :name "agent-persistence")))

(defun niwa-agent-persistence-test--publish (session &rest entries)
  "Publish ENTRIES to SESSION's committed storage under its lease.
Each entry is (RELATIVE . CONTENT), or (RELATIVE . :delete) to remove it."
  (let ((root (niwa-session-save-path session)))
    (niwa-workspace-identity-ensure
     (niwa-workspace-root (niwa-session-workspace session)))
    (unless (niwa-session-session-id session)
      (setf (niwa-session-session-id session) "agent-persistence-test"))
    (unless (niwa-session-durability-lease-owned-p session)
      (should (niwa-session-durability-lease-acquire
               root "*agent-persistence*" session)))
    (should
     (niwa-session-publication-publish
      session
      (append
       (mapcar (lambda (entry)
                 (if (eq :delete (cdr entry))
                     (list :path (expand-file-name (car entry) root) :delete t)
                   (list :path (expand-file-name (car entry) root)
                         :content (cdr entry))))
               entries)
       (list (list :path (expand-file-name "session.meta.el" root)
                   ;; The fixtures' registries are not live-valid; the
                   ;; commit marker only needs a well-formed sidecar.
                   :content (let ((registry
                                   (niwa-session-agent-registry session)))
                              (setf (niwa-session-agent-registry session) nil)
                              (unwind-protect
                                  (with-temp-buffer
                                    (niwa-session-artifacts-printed-value
                                     (niwa-session-artifacts--live-sidecar
                                      session (current-buffer))))
                                (setf (niwa-session-agent-registry session)
                                      registry)))
                   :commit-marker t :replace t)))))))

(defun niwa-agent-persistence-test--release (session)
  "Release SESSION's lease taken by `niwa-agent-persistence-test--publish'."
  (when (niwa-session-durability-lease-owned-p session)
    (niwa-session-durability-lease-release
     (niwa-session-save-path session) session)))

(defun niwa-agent-persistence-test--record
    (&optional path parent id activity)
  "Return one retained fixture record with optional identity fields."
  (niwa-agent-record--create
   :id (or id "opaque-id")
   :path (or path "/root/task")
   :parent-path (or parent "/root")
   :role "default"
   :configuration (niwa-agent-persistence-test--configuration)
   :activity (or activity 'idle)
   :conversation-location "agents/task.chat.org"))

(niwa-deftest niwa-agent-persistence--invalid ()
  ,test
  (test)
  :doc "classifies malformed sidecar data with the dedicated condition"
  (should-error
   (niwa-agent-persistence--invalid "Bad field: %s" 'value)
   :type 'niwa-agent-persistence-invalid-data))

(niwa-deftest niwa-agent-persistence--encode-local ()
  ,test
  (test)
  :doc "encodes backends, tools, contexts, and ordinary plain values"
  (let ((backend (niwa-agent-persistence-test--backend))
        (tool (gptel-get-tool '("niwa" "Read"))))
    (should (stringp
             (niwa-agent-persistence--encode-local
              'gptel-backend backend)))
    (should
     (equal '(("niwa" "Read"))
            (niwa-agent-persistence--encode-local
             'gptel-tools (list tool))))
    (should
     (equal '("/tmp/plain.el" ("/tmp/region.el" 1 2))
            (niwa-agent-persistence--encode-local
             'gptel-context
             '("/tmp/plain.el" ("/tmp/region.el" 1 2)))))
    (should (eq 'read
                (niwa-agent-persistence--encode-local
                 'gptel-model 'read))))
  :doc "rejects non-plain request-local values"
  (should-error
   (niwa-agent-persistence--encode-local
    'gptel-model (lambda () t))))

(niwa-deftest niwa-agent-persistence--encode-configuration ()
  ,test
  (test)
  :doc "encodes the frozen role and request-local contracts"
  (let ((encoded
         (niwa-agent-persistence--encode-configuration
          (niwa-agent-persistence-test--configuration))))
    (should (equal "default"
                   (plist-get (plist-get encoded :agent) :name)))
    (should (equal '(environment skills)
                   (plist-get (plist-get encoded :agent) :context-components)))
    (should (assq 'gptel-backend
                  (plist-get encoded :request-locals))))
  :doc "rejects a missing frozen configuration"
  (should-error (niwa-agent-persistence--encode-configuration nil))
  :doc "rejects incomplete and malformed durable request-local configurations"
  (let* ((configuration (niwa-agent-persistence-test--configuration))
         (locals (niwa-agent-configuration-request-locals configuration)))
    (dolist (invalid (list (cdr locals) (cons 'bad locals)
                          (cons '("gptel-model" . bad) locals)))
      (setf (niwa-agent-configuration-request-locals configuration) invalid)
      (should-error
       (niwa-agent-persistence--encode-configuration configuration)))))

(niwa-deftest niwa-agent-persistence--serialize-record
  (:vars ((target (niwa-execution-target-create "/"))))
  ,test
  (test)
  :doc "encodes one valid identity and reserves its path and opaque ID"
  (let* ((record (niwa-agent-persistence-test--record))
         (entry (cons "/root/task" record))
         (paths (make-hash-table :test #'equal))
         (ids (make-hash-table :test #'equal))
         (encoded
          (niwa-agent-persistence--serialize-record entry paths ids target)))
    (should (equal "/root/task" (plist-get encoded :path)))
    (should (plist-member encoded :working-directory))
    (should-not (plist-get encoded :working-directory))
    (should (gethash "/root/task" paths))
    (should (gethash "opaque-id" ids))
    (should-error
     (niwa-agent-persistence--serialize-record entry paths ids target)))

  :doc "round-trips explicit nil, local and remote working directories without probing them"
  (dolist (directory '(nil "/tmp/agent-persisted-cwd/"
                          "/ssh:example.invalid:/srv/agent/"))
    (let* ((target (niwa-execution-target-create (or directory "/")))
           (record (niwa-agent-persistence-test--record))
           (_ (setf (niwa-agent-record-working-directory record) directory))
           (encoded (niwa-agent-persistence--serialize-record
                     (cons "/root/task" record)
                     (make-hash-table :test #'equal)
                     (make-hash-table :test #'equal) target))
           (restored (cdr (niwa-agent-persistence--deserialize-record
                           (car (read-from-string (prin1-to-string encoded)))
                           (make-hash-table :test #'equal)
                           (make-hash-table :test #'equal) target))))
      (should (equal (and directory (niwa-execution-target-native-path target directory))
                     (plist-get encoded :working-directory)))
      (should (equal directory (niwa-agent-record-working-directory restored)))))

  :doc "rejects malformed and other-target live working directory overrides"
  (dolist (directory '("relative/" "" 17 t "/ssh:other-route:/srv/agent/"))
    (let ((record (niwa-agent-persistence-test--record)))
      (setf (niwa-agent-record-working-directory record) directory)
      (should-error
       (niwa-agent-persistence--serialize-record
        (cons "/root/task" record)
        (make-hash-table :test #'equal) (make-hash-table :test #'equal) target))))

  :doc "persists a running turn's acknowledged provider without changing its frozen identity"
  (let* ((record (niwa-agent-persistence-test--record
                  nil nil nil 'running))
         (frozen (niwa-agent-record-configuration record))
         (effective (copy-niwa-agent-configuration frozen))
         (locals (copy-tree (niwa-agent-configuration-request-locals frozen)))
         launch)
    (setf (alist-get 'gptel-reasoning-effort
                    (niwa-agent-configuration-request-locals frozen)) 'high)
    (setq launch (niwa-agent-configuration-model-metadata frozen))
    (setf (alist-get 'gptel-model locals) 'served-model
          (alist-get 'gptel-reasoning-effort locals) nil
          (niwa-agent-configuration-request-locals effective) locals
          (niwa-agent-record-model-metadata record) launch
          (niwa-agent-record-invocation record)
          (niwa-agent-invocation--create :frozen-configuration effective))
    (let* ((encoded (niwa-agent-persistence--serialize-record
                     (cons "/root/task" record)
                     (make-hash-table :test #'equal)
                     (make-hash-table :test #'equal) target))
           (restored (cdr (niwa-agent-persistence--deserialize-record
                           (car (read-from-string (prin1-to-string encoded)))
                           (make-hash-table :test #'equal)
                           (make-hash-table :test #'equal) target))))
      (should (equal (niwa-agent-configuration-model-metadata effective)
                     (niwa-agent-control-record-model-metadata restored)))
      (should (equal launch (niwa-agent-record-model-metadata record)))
      (should (eq 'test-model
                  (alist-get 'gptel-model
                             (plist-get (plist-get encoded :configuration)
                                        :request-locals))))))

  :doc "retains the last served provider metadata across a restore"
  (let* ((record (niwa-agent-persistence-test--record))
         (metadata '(:backend "Served" :model "served-model" :effort nil)))
    (setf (niwa-agent-record-model-metadata record) metadata)
    (let* ((encoded (niwa-agent-persistence--serialize-record
                     (cons "/root/task" record)
                     (make-hash-table :test #'equal)
                     (make-hash-table :test #'equal) target))
           (restored (cdr (niwa-agent-persistence--deserialize-record
                           (car (read-from-string (prin1-to-string encoded)))
                           (make-hash-table :test #'equal)
                           (make-hash-table :test #'equal) target))))
      (should (equal metadata (plist-get encoded :model-metadata)))
      (should (equal metadata (niwa-agent-record-model-metadata restored)))
      (should-error
       (niwa-agent-persistence--deserialize-record
        (plist-put (copy-sequence encoded) :model-metadata '(:model "m"))
        (make-hash-table :test #'equal) (make-hash-table :test #'equal) target)
       :type 'niwa-agent-persistence-invalid-data)
      (setf (niwa-agent-record-model-metadata record) '(:model "m"))
      (should-error
       (niwa-agent-persistence--serialize-record
        (cons "/root/task" record)
        (make-hash-table :test #'equal) (make-hash-table :test #'equal) target)))))

(niwa-deftest niwa-agent-persistence--identity-p ()
  ,test
  (test)
  :doc "accepts one rooted, path-consistent, previously unseen identity"
  (let ((paths (make-hash-table :test #'equal))
        (ids (make-hash-table :test #'equal)))
    (should
     (niwa-agent-persistence--identity-p
      "opaque" "/root/task" "/root" "default" 'running
      "agents/task.chat.org" paths ids))
    (puthash "/root/task" t paths)
    (should-not
     (niwa-agent-persistence--identity-p
      "opaque" "/root/task" "/root" "default" 'running
      "agents/task.chat.org" paths ids))
    (should-not
     (niwa-agent-persistence--identity-p
      "other" "/root/task/child" "/root/wrong" "default" 'idle
      "../escape.chat.org" (make-hash-table :test #'equal) ids))))

(niwa-deftest niwa-agent-persistence--decode-local ()
  ,test
  (test)
  :doc "restores registered backends, tools, contexts, and plain symbols"
  (let* ((backend (niwa-agent-persistence-test--backend))
         (backend-name (gptel-backend-name backend)))
    (should (eq backend
                (niwa-agent-persistence--decode-local
                 'gptel-backend backend-name)))
    (should
     (gptel-tool-p
      (car (niwa-agent-persistence--decode-local
            'gptel-tools '(("niwa" "Read"))))))
    (should
     (equal '("/tmp/plain.el")
            (niwa-agent-persistence--decode-local
             'gptel-context '("/tmp/plain.el"))))
    (should (eq 'read
                (niwa-agent-persistence--decode-local
                 'gptel-model 'read))))
  :doc "classifies unknown registered values as invalid persisted data"
		 (should (equal "Missing Backend"
				(gptel-backend-name (niwa-agent-persistence--decode-local
						     'gptel-backend "Missing Backend"))))
  (dolist (path '(("unknown-category" "Read") ("niwa" "MissingBuiltinTool")))
    (should-error (niwa-agent-persistence--decode-local 'gptel-tools (list path))
                  :type 'niwa-agent-persistence-invalid-data)))

(niwa-deftest niwa-agent-persistence--decode-configuration ()
  ,test
  (test)
  :doc "restores one encoded frozen configuration"
  (let* ((configuration
          (niwa-agent-persistence-test--configuration))
         (encoded
          (niwa-agent-persistence--encode-configuration configuration))
         (decoded
          (niwa-agent-persistence--decode-configuration
           encoded "default")))
    (should (niwa-agent-configuration-p decoded))
    (should (equal '(environment skills)
                   (niwa-agent-context-components
                    (niwa-agent-configuration-agent decoded))))
    (should
     (equal "default"
            (niwa-agent-name
             (niwa-agent-configuration-agent decoded))))
    (should-error
     (niwa-agent-persistence--decode-configuration encoded "other")
     :type 'niwa-agent-persistence-invalid-data)
    (dolist (locals
             (list
              (cons 'bad (copy-tree (plist-get encoded :request-locals)))
              (cons '("gptel-model" . bad)
                    (copy-tree (plist-get encoded :request-locals)))
              (cons '(kill-buffer-hook . (ignore))
                    (copy-tree (plist-get encoded :request-locals)))
              (append (copy-tree (plist-get encoded :request-locals))
                      '((gptel-model . duplicate)))
              (assq-delete-all
               'gptel-model
               (copy-tree (plist-get encoded :request-locals)))))
      (let ((invalid (copy-tree encoded)))
        (setf (plist-get invalid :request-locals) locals)
        (should-error
         (niwa-agent-persistence--decode-configuration invalid "default")
         :type 'niwa-agent-persistence-invalid-data)))))

(niwa-deftest niwa-agent-persistence--deserialize-record
  (:vars ((target (niwa-execution-target-create "/"))))
  ,test
  (test)
  :doc "decodes one identity and rejects a duplicate in the same registry"
  (let* ((record (niwa-agent-persistence-test--record))
         (encoded
          (niwa-agent-persistence--serialize-record
           (cons "/root/task" record)
           (make-hash-table :test #'equal)
           (make-hash-table :test #'equal) target))
         (paths (make-hash-table :test #'equal))
         (ids (make-hash-table :test #'equal))
         (decoded
          (niwa-agent-persistence--deserialize-record encoded paths ids target)))
    (should (equal "/root/task" (car decoded)))
    (should (niwa-agent-record-p (cdr decoded)))
    (should-error
     (niwa-agent-persistence--deserialize-record encoded paths ids target)
     :type 'niwa-agent-persistence-invalid-data))

  :doc "rejects missing and malformed persisted working directories without a legacy default"
  (let ((encoded (niwa-agent-persistence--serialize-record
                  (cons "/root/task" (niwa-agent-persistence-test--record))
                  (make-hash-table :test #'equal) (make-hash-table :test #'equal) target)))
    (dolist (directory '("relative/" "" 17 t missing "/ssh:old-route:/srv/agent/"))
      (let ((invalid (copy-tree encoded)))
        (if (eq directory 'missing)
            (cl-remf invalid :working-directory)
          (setf (plist-get invalid :working-directory) directory))
        (should-error
         (niwa-agent-persistence--deserialize-record
          invalid (make-hash-table :test #'equal) (make-hash-table :test #'equal) target)
         :type 'niwa-agent-persistence-invalid-data)))))

(niwa-deftest niwa-agent-persistence--drop-orphans ()
  ,test
  (test)
  :doc "removes an absent-parent branch while retaining rooted identities"
  (let ((valid (niwa-agent-record--create :parent-path "/root"))
        (orphan (niwa-agent-record--create :parent-path "/root/missing"))
        (descendant
         (niwa-agent-record--create :parent-path "/root/orphan")))
    (should
     (equal '("/root/valid")
            (mapcar
             #'car
             (niwa-agent-persistence--drop-orphans
              (list (cons "/root/valid" valid)
                    (cons "/root/orphan" orphan)
                    (cons "/root/orphan/child" descendant))))))))

(niwa-deftest niwa-agent-persistence-transcript-path-p ()
  ,test
  (test)
  :doc "accepts relative agent transcripts and rejects other path shapes"
  (let ((tmp (file-name-as-directory (make-temp-file "agent-path-" t))))
    (unwind-protect
        (progn
          (should (niwa-agent-persistence-transcript-path-p
                   "agents/task.chat.org" tmp))
          (dolist (path '("/etc/passwd.chat.org" "agents/../escape.chat.org"
                          "agents/transcript.txt" "outside/file.chat.org"))
            (should-not (niwa-agent-persistence-transcript-path-p path tmp))))
      (delete-directory tmp t)))
  :doc "checks local symlinks but leaves remote cache validation to publication"
  (let* ((tmp (file-name-as-directory (make-temp-file "agent-path-" t)))
         (agents (file-name-concat tmp "agents"))
         (outside (make-temp-file "agent-path-outside-" nil ".chat.org"))
         (link (file-name-concat agents "linked.chat.org")))
    (unwind-protect
        (progn
          (make-directory agents t)
          (make-symbolic-link outside link)
          (should-not
           (niwa-agent-persistence-transcript-path-p
            "agents/linked.chat.org" tmp))
          (cl-letf (((symbol-function 'file-remote-p)
                     (lambda (path &optional _identification _connected)
                       (and (equal path tmp) "/mock:"))))
            (should
             (niwa-agent-persistence-transcript-path-p
              "agents/linked.chat.org" tmp))))
      (when (file-exists-p link) (delete-file link))
      (when (file-exists-p outside) (delete-file outside))
      (delete-directory tmp t)))
  :doc "rejects nil and empty paths"
  (should-not (niwa-agent-persistence-transcript-path-p nil "/tmp/"))
  (should-not (niwa-agent-persistence-transcript-path-p "" "/tmp/")))

(niwa-deftest niwa-agent-persistence-sanitize-mailbox ()
  ,test
  (test)
  :doc "rejects absent or malformed durable identities without synthesizing IDs"
  (dolist (id '(nil "" 12))
    (should-not
     (niwa-agent-persistence-sanitize-mailbox
      (list (list :id id :type 'MAIL :sender "/root/a" :recipient "/root"
                  :payload "mail" :timestamp '(1 0 0 0)))
      "/root")))
  :doc "keeps canonical MAIL, EXECUTION, RESULT, and USER records in stored order"
  (let* ((newer (list 2 0 0 0))
         (older (list 1 0 0 0))
         (raw
          (list
           (list :id "result" :type 'RESULT :sender "/root/child" :recipient "/root"
                 :outcome 'completed :payload "done" :timestamp newer)
           (list :id "execution" :type 'EXECUTION :sender "/root" :recipient "/root"
                 :payload "execution" :timestamp newer)
           (list :id "mail" :type 'MAIL :sender "/root/child" :recipient "/root"
                 :payload "hello" :timestamp older)
           (list :id "bad" :type 'MAIL :sender "bad" :recipient "/root"
                 :payload "drop" :timestamp older)))
         (clean (niwa-agent-persistence-sanitize-mailbox raw "/root")))
    (should (= 3 (length clean)))
    (should (eq 'RESULT (plist-get (car clean) :type)))
    (should (eq 'EXECUTION (plist-get (cadr clean) :type)))
    (should (equal "hello" (plist-get (caddr clean) :payload))))
  :doc "retains child execution deliveries to root and nested spawn parents"
  (dolist (parent '("/root" "/root/parent"))
    (let ((raw (list (list :id "child-execution" :type 'EXECUTION :sender (concat parent "/child")
                           :recipient parent :payload "Bash output"
                           :timestamp '(1 0 0 0)))))
      (should (equal raw (niwa-agent-persistence-sanitize-mailbox
                         raw parent)))))
  :doc "retains root agent MAIL and RESULT without persisting their wake marks"
  (let* ((raw (list (list :id "mail" :type 'MAIL :sender "/root/child" :recipient "/root"
                          :payload "report" :timestamp '(1 0 0 0) :wake t)
                    (list :id "result" :type 'RESULT :sender "/root/child" :recipient "/root"
                          :payload "done" :outcome 'completed
                          :timestamp '(2 0 0 0) :wake t)))
         (clean (niwa-agent-persistence-sanitize-mailbox raw "/root")))
    (should (equal '("report" "done")
                   (mapcar (lambda (record) (plist-get record :payload)) clean)))
    (should (eq 'completed (plist-get (cadr clean) :outcome)))
    (should-not (cl-some (lambda (record) (plist-member record :wake)) clean))
    (should (cl-every (lambda (record) (plist-get record :wake)) raw)))
  :doc "keeps root MONITOR records and drops the runtime wake mark"
  (let* ((raw (list (list :id "monitor" :type 'MONITOR :sender "/root" :recipient "/root"
                          :execution-id "exec-1" :description "CI"
                          :payload "line\n" :timestamp '(1 0 0 0) :wake t)
                    (list :id "execution" :type 'EXECUTION :sender "/root" :recipient "/root"
                          :execution-id "exec-2" :payload "done"
                          :timestamp '(1 0 0 0) :wake t)))
         (clean (niwa-agent-persistence-sanitize-mailbox raw "/root")))
    (should (equal (list :id "monitor" :type 'MONITOR :sender "/root" :recipient "/root"
                         :payload "line\n" :timestamp '(1 0 0 0)
                         :execution-id "exec-1" :description "CI")
                   (car clean)))
    (should-not (cl-some (lambda (record) (plist-member record :wake)) clean)))
  :doc "keeps structural metadata and the terminal record type"
  (let* ((raw (list (list :id "terminal" :type 'MONITOR_TERMINAL :sender "/root" :recipient "/root"
                          :execution-id "e" :description "CI"
                          :meta '(:event-count 2 :termination "stopped")
                          :payload "done" :timestamp '(1 0 0 0))
                    (list :id "execution" :type 'EXECUTION :sender "/root" :recipient "/root"
                          :meta '(:execution-id "x" :exit-code 1)
                          :payload "p" :timestamp '(1 0 0 0))))
         (clean (niwa-agent-persistence-sanitize-mailbox raw "/root")))
    (should (= 2 (length clean)))
    (should (equal '(:event-count 2 :termination "stopped")
                   (plist-get (car clean) :meta)))
    (should (equal '(:execution-id "x" :exit-code 1)
                   (plist-get (cadr clean) :meta))))
  :doc "drops MONITOR records without identity or outside the root"
  (dolist (entry
           (list (list :id "monitor" :type 'MONITOR :sender "/root" :recipient "/root"
                       :description "CI" :payload "x" :timestamp '(1 0 0 0))
                 (list :id "monitor" :type 'MONITOR :sender "/root" :recipient "/root"
                       :execution-id "e" :payload "x" :timestamp '(1 0 0 0))
                 (list :id "monitor" :type 'MONITOR :sender "/root/a" :recipient "/root"
                       :execution-id "e" :description "CI" :payload "x"
                       :timestamp '(1 0 0 0))))
    (should-not (niwa-agent-persistence-sanitize-mailbox (list entry) "/root")))
  :doc "drops wrong recipients, malformed outcomes, payloads, and timestamps"
  (dolist (entry
           (list
            (list :id "mail" :type 'MAIL :sender "/root/a" :recipient "/root/b"
                  :payload "x" :timestamp '(1 0 0 0))
            (list :id "result" :type 'RESULT :sender "/root/a" :recipient "/root"
                  :outcome 'future :payload "x" :timestamp '(1 0 0 0))
            (list :id "execution" :type 'EXECUTION :sender "/root/a/b" :recipient "/root"
                  :payload "x" :timestamp '(1 0 0 0))
            (list :id "mail" :type 'MAIL :sender "/root/a" :recipient "/root"
                  :payload 4 :timestamp '(1 0 0 0))
            (list :id "mail" :type 'MAIL :sender "/root/a" :recipient "/root"
                  :payload "x" :timestamp 'bad)))
    (should-not
     (niwa-agent-persistence-sanitize-mailbox (list entry) "/root"))))

(niwa-deftest niwa-agent-persistence-serialize-registry ()
  ,test
  (test)
  :doc "encodes identity, activity, configuration, file context, and mailbox"
  (let* ((session (niwa-agent-persistence-test--session))
         (live-context (generate-new-buffer " *agent-live-context*"))
         (configuration
          (niwa-agent-persistence-test--configuration
           (list "/tmp/plain.el" '("/tmp/durable.el")
                 (list live-context 1 2))))
         (record
          (niwa-agent-record--create
           :id "opaque-id"
           :path "/root/task"
           :parent-path "/root"
           :role "default"
           :configuration configuration
           :activity 'waiting
           :conversation-location "agents/task.chat.org"
           :hook-context-pending
           '((:event UserPromptSubmit :body "pending guidance"))
           :mailbox
           (list (list :id "mail" :type 'MAIL :sender "/root"
                       :recipient "/root/task" :payload "news"
                       :timestamp '(1 0 0 0))))))
    (unwind-protect
        (progn
          (setf (niwa-session-agent-registry session)
                (list (cons "/root/task" record)))
          (let* ((raw (niwa-agent-persistence-serialize-registry session))
                 (saved (car raw))
                 (locals
                  (plist-get (plist-get saved :configuration)
                             :request-locals)))
            (should (equal "opaque-id" (plist-get saved :id)))
            (should (eq 'waiting (plist-get saved :activity)))
            (should
             (equal '((:event UserPromptSubmit :body "pending guidance"))
                    (plist-get saved :hook-context-pending)))
            (should (equal '("/tmp/plain.el" ("/tmp/durable.el"))
                           (cdr (assq 'gptel-context locals))))
            (should
             (equal '(("niwa" "Read"))
                    (cdr (assq 'gptel-tools locals))))
            (should (equal "news"
                           (plist-get (car (plist-get saved :mailbox))
                                      :payload)))))
      (kill-buffer live-context)))

  :doc "fails a save instead of silently omitting invalid live state"
  (let* ((session (niwa-agent-persistence-test--session))
         (record
          (niwa-agent-record--create
           :id "broken" :path "/root/broken" :parent-path "/root"
           :role "default" :configuration nil :activity 'idle
           :conversation-location "agents/broken.chat.org")))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/broken" record)))
    (should-error (niwa-agent-persistence-serialize-registry session)))

  :doc "writes one shared copy of equal configurations that round-trips"
  (let* ((session (niwa-agent-persistence-test--session))
         (paths '("/root/one" "/root/two" "/root/three")))
    (setf (niwa-session-agent-registry session)
          (mapcar (lambda (path)
                    (cons path (niwa-agent-persistence-test--record
                                path "/root" (concat "id-" (substring path 6)))))
                  paths))
    (let* ((raw (niwa-agent-persistence-serialize-registry session))
           (configurations (mapcar (lambda (entry) (plist-get entry :configuration))
                                   raw))
           (printed (with-temp-buffer
                      (let ((print-circle t) (print-length nil) (print-level nil))
                        (prin1 raw (current-buffer)))
                      (buffer-string))))
      ;; Distinct live configurations print once: equal values are one object.
      (should (eq (nth 0 configurations) (nth 1 configurations)))
      (should (eq (nth 1 configurations) (nth 2 configurations)))
      (should (= 1 (with-temp-buffer
                     (insert printed)
                     (how-many ":system-prompt" (point-min) (point-max)))))
      ;; The printed sidecar still decodes into three independent records.
      (let ((restored (niwa-agent-persistence-deserialize-registry
                       (car (read-from-string printed))
                       (niwa-session-execution-target session))))
        (should (equal paths (mapcar #'car restored)))
        (should-not (eq (niwa-agent-record-configuration (cdr (nth 0 restored)))
                        (niwa-agent-record-configuration (cdr (nth 1 restored)))))))))

(niwa-deftest niwa-agent-persistence-deserialize-registry ()
  ,test
  (test)
  :doc "stores remote cwd natively and rebinds it through another client route or locally"
  (let* ((old-target (niwa-execution-target-create "/ssh:old-route:/srv/project/"))
         (source (niwa-session--create :execution-target old-target))
         (record (niwa-agent-persistence-test--record))
         (original "/ssh:old-route:/srv/agent/"))
    (setf (niwa-agent-record-working-directory record) original
          (niwa-session-agent-registry source) (list (cons "/root/task" record)))
    (let* ((raw (niwa-agent-persistence-serialize-registry source))
           (stored (plist-get (car raw) :working-directory))
           (readback (car (read-from-string (prin1-to-string raw)))))
      (should (equal "/srv/agent/" stored))
      (should-not (file-remote-p stored))
      (should (equal original (niwa-agent-record-working-directory record)))
      (dolist (root '("/ssh:new-route:/srv/project/" "/srv/project/"))
        (let* ((target (niwa-execution-target-create root))
               (restored (niwa-agent-persistence-deserialize-registry readback target))
               (directory (niwa-agent-record-working-directory (cdar restored))))
          (should (equal (if (file-remote-p root)
                             "/ssh:new-route:/srv/agent/"
                           "/srv/agent/")
                         directory))
          (should (equal "/srv/agent/"
                         (niwa-execution-target-native-path target directory)))))))

  :doc "round-trips frozen configuration and retained identity data"
  (let* ((source (niwa-agent-persistence-test--session))
         (configuration (niwa-agent-persistence-test--configuration
                         '("/tmp/plain.el" ("/tmp/durable.el"))))
         (_ (setf
             (niwa-agent-reminders
              (niwa-agent-configuration-agent configuration))
             (list (niwa-reminders-make-pending-events))))
         (record
          (niwa-agent-record--create
           :id "opaque-id" :path "/root/task" :parent-path "/root"
           :role "default" :configuration configuration :activity 'idle
           :conversation-location "agents/task.chat.org"
           :hook-context-pending
           '((:event UserPromptSubmit :body "persist me"))
           :settled-result "persist me too"
           :settled-outcome 'completed)))
    (setf (niwa-session-agent-registry source)
          (list (cons "/root/task" record)))
    (let* ((raw (niwa-agent-persistence-serialize-registry source))
           (niwa-agent--default
            (niwa-agent--create
             :name "default"
             :description "Redefined role"
             :reminders
             (list (niwa-reminders-make-date-change))))
           (restored
            (niwa-agent-persistence-deserialize-registry
             raw (niwa-session-execution-target source)))
           (result (cdar restored))
           (restored-agent
            (niwa-agent-configuration-agent
             (niwa-agent-record-configuration result)))
           (locals
            (niwa-agent-configuration-request-locals
             (niwa-agent-record-configuration result))))
      (should (equal "opaque-id" (niwa-agent-record-id result)))
      (should
       (equal '((:event UserPromptSubmit :body "persist me"))
              (niwa-agent-record-hook-context-pending result)))
      (should (equal "persist me too"
                     (niwa-agent-record-settled-result result)))
      (should (eq 'completed
                  (niwa-agent-record-settled-outcome result)))
      (should (gptel-backend-p (alist-get 'gptel-backend locals)))
      (should (gptel-tool-p (car (alist-get 'gptel-tools locals))))
      (should (equal '("/tmp/plain.el" ("/tmp/durable.el"))
                     (alist-get 'gptel-context locals)))
      (should
       (equal '(pending-events)
              (mapcar #'niwa-reminder-type
                      (niwa-agent-reminders restored-agent))))))

		 :doc "preserves missing provider identities while rejecting missing tools"
  (let* ((source (niwa-agent-persistence-test--session))
         (record
          (niwa-agent-record--create
           :id "valid" :path "/root/valid" :parent-path "/root"
           :role "default"
           :configuration (niwa-agent-persistence-test--configuration)
           :activity 'idle
           :conversation-location "agents/valid.chat.org")))
    (setf (niwa-session-agent-registry source)
          (list (cons "/root/valid" record)))
    (let* ((raw (niwa-agent-persistence-serialize-registry source))
           (locals
            (plist-get (plist-get (car raw) :configuration)
                       :request-locals)))
      (setcdr (assq 'gptel-backend locals) "Missing Backend")
      (niwa-test--with-captured-diagnostics nil
							      (should
         (niwa-agent-persistence-deserialize-registry
          raw (niwa-session-execution-target source)))))
    (let* ((raw (niwa-agent-persistence-serialize-registry source))
           (locals
            (plist-get (plist-get (car raw) :configuration)
                       :request-locals)))
      (setcdr (assq 'gptel-tools locals) '(("missing" "tool")))
      (niwa-test--with-captured-diagnostics nil
        (should-not
         (niwa-agent-persistence-deserialize-registry
          raw (niwa-session-execution-target source))))))

  :doc "drops an identity whose trusted reminder key has invalid arguments"
  (let* ((source (niwa-agent-persistence-test--session))
         (record (niwa-agent-persistence-test--record))
         (_ (setf (niwa-session-agent-registry source)
                  (list (cons "/root/task" record))))
         (raw (niwa-agent-persistence-serialize-registry source))
         (agent-data
          (plist-get (plist-get (car raw) :configuration) :agent)))
    (setf (plist-get agent-data :reminders) '((pending-events 1)))
    (niwa-test--with-captured-diagnostics nil
      (should-not
       (niwa-agent-persistence-deserialize-registry
        raw (niwa-session-execution-target source)))))

  :doc "drops malformed identities and every descendant of a rejected parent"
  (let* ((source (niwa-agent-persistence-test--session))
         (configuration (niwa-agent-persistence-test--configuration))
         (valid
          (niwa-agent-record--create
           :id "valid" :path "/root/valid" :parent-path "/root"
           :role "default" :configuration configuration :activity 'idle
           :conversation-location "agents/valid.chat.org"))
         (bad-parent (copy-niwa-agent-record valid))
         (child (copy-niwa-agent-record valid)))
    (setf (niwa-agent-record-id bad-parent) "bad"
          (niwa-agent-record-path bad-parent) "/root/bad"
          (niwa-agent-record-conversation-location bad-parent)
          "agents/bad.chat.org"
          (niwa-agent-record-id child) "child"
          (niwa-agent-record-path child) "/root/bad/child"
          (niwa-agent-record-parent-path child) "/root/bad"
          (niwa-agent-record-conversation-location child)
          "agents/child.chat.org")
    (setf (niwa-session-agent-registry source)
          (list (cons "/root/valid" valid)
                (cons "/root/bad" bad-parent)
                (cons "/root/bad/child" child)))
    (let ((raw (niwa-agent-persistence-serialize-registry source)))
      (setf (plist-get (cadr raw) :conversation-location)
            "../escape.chat.org")
      (niwa-test--with-captured-diagnostics nil
        (should
         (equal '("/root/valid")
                (mapcar #'car
                        (niwa-agent-persistence-deserialize-registry
                         raw (niwa-session-execution-target source))))))))

  :doc "propagates unexpected decoder bugs instead of dropping identities"
  (let* ((source (niwa-agent-persistence-test--session))
         (record
          (niwa-agent-record--create
           :id "valid" :path "/root/valid" :parent-path "/root"
           :role "default"
           :configuration (niwa-agent-persistence-test--configuration)
           :activity 'idle
           :conversation-location "agents/valid.chat.org")))
    (setf (niwa-session-agent-registry source)
          (list (cons "/root/valid" record)))
    (let ((raw (niwa-agent-persistence-serialize-registry source)))
      (cl-letf (((symbol-function
                  'niwa-reminders-restore-agent-templates)
                 (lambda (_recipes) (error "Decoder regression"))))
        (should-error
         (niwa-agent-persistence-deserialize-registry
          raw (niwa-session-execution-target source)))))))

(niwa-deftest niwa-agent-persistence-restore-tree ()
  ,test
  (test)
  :doc "retains an idle identity without loading its conversation and drops an escaping identity"
  (let* ((root-dir (make-temp-file "niwa-agent-tree-" t))
         (session (niwa-agent-persistence-test--session root-dir))
         (root-buffer (generate-new-buffer " *agent-tree-root*"))
         (relative "agents/valid.chat.org")
         (configuration (niwa-agent-persistence-test--configuration))
         (valid
          (niwa-agent-record--create
           :id "valid" :path "/root/valid" :parent-path "/root"
           :role "default" :configuration configuration :activity 'idle
           :conversation-location relative))
         (escaping
          (niwa-agent-record--create
           :id "escaping" :path "/root/escaping" :parent-path "/root"
           :role "default" :configuration configuration :activity 'idle
           :conversation-location "../escape.chat.org")))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) root-dir
                (niwa-session-agent-registry session)
                (list (cons "/root/valid" valid)
                      (cons "/root/escaping" escaping)))
          (niwa-agent-persistence-test--publish
           session (cons relative "* Agent Task: valid\nDurable context.\n"))
          (with-current-buffer root-buffer
            (org-mode)
            (setq-local niwa--session session)
            (setq-local niwa--workspace
                        (niwa-session-workspace session)))
          ;; The escaping identity is dropped with a warning; the
          ;; surviving registry is what this case asserts.
          (niwa-test--with-captured-diagnostics nil
            (should (= 1 (niwa-agent-persistence-restore-tree
                          session root-buffer nil))))
          (should (equal '("/root/valid")
                         (mapcar #'car
                                 (niwa-session-agent-registry session))))
          (should-not (niwa-agent-record-conversation-buffer valid))
          (should (eq configuration (niwa-agent-record-configuration valid))))
      (niwa-agent-persistence-test--release session)
      (niwa-agent-control-teardown-session session)
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (delete-directory root-dir t)))

  :doc "hydrates a settled outcome as the transcript status it reported"
  (let* ((root-dir (make-temp-file "niwa-agent-outcome-" t))
         (session (niwa-agent-persistence-test--session root-dir))
         (root-buffer (generate-new-buffer " *agent-outcome-root*"))
         (configuration (niwa-agent-persistence-test--configuration))
         (records
          (list (cons 'errored "failed")
                (cons 'interrupted "stopped")
                (cons 'completed "finished")
                (cons nil "unknown"))))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) root-dir)
          (apply #'niwa-agent-persistence-test--publish session
                 (mapcar (lambda (entry)
                           (cons (format "agents/%s.chat.org" (cdr entry))
                                 (format "* Agent Task: %s\n" (cdr entry))))
                         records))
          (setf (niwa-session-agent-registry session)
                (mapcar
                 (lambda (entry)
                   (let ((relative (format "agents/%s.chat.org" (cdr entry))))
                     (cons (format "/root/%s" (cdr entry))
                           (niwa-agent-record--create
                            :id (cdr entry)
                            :path (format "/root/%s" (cdr entry))
                            :parent-path "/root" :role "default"
                            :configuration configuration :activity 'idle
                            :settled-outcome (car entry)
                            :conversation-location relative))))
                 records))
          (with-current-buffer root-buffer
            (org-mode)
            (setq-local niwa--session session)
            (setq-local niwa--workspace
                        (niwa-session-workspace session)))
          ;; The count is of dropped records; all three are valid.
          (should (= 0 (niwa-agent-persistence-restore-tree
                        session root-buffer nil)))
          ;; A resumed agent that failed or was interrupted must not read as
          ;; one that finished: the transcript badge is derived from this.
          (dolist (expected '(("/root/failed" . error)
                              ("/root/stopped" . aborted)
                              ("/root/finished" . completed)
                              ("/root/unknown" . incomplete)))
            (let* ((record (cdr (assoc (car expected)
                                      (niwa-session-agent-registry
                                       session))))
                   (buffer (niwa-agent-persistence-ensure-conversation
                            session record root-buffer)))
              (should (buffer-live-p buffer))
              (should
               (eq (cdr expected)
                   (niwa-agent-invocation-transcript-status
                    (buffer-local-value 'niwa--agent-invocation
                                        buffer)))))))
      (niwa-agent-persistence-test--release session)
      (niwa-agent-control-teardown-session session)
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (delete-directory root-dir t)))

  :doc "hydrates a published remote transcript when the fixed cache is missing"
  (let* ((root-dir (make-temp-file "niwa-agent-remote-tree-" t))
         (session (niwa-agent-persistence-test--session root-dir))
         (root-buffer (generate-new-buffer " *agent-remote-root*"))
         (relative "agents/published.chat.org")
         (record
          (niwa-agent-record--create
           :id "published" :path "/root/published" :parent-path "/root"
           :role "default"
           :configuration (niwa-agent-persistence-test--configuration)
           :activity 'idle :conversation-location relative))
         hydrated-buffer
         seen)
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) root-dir
                (niwa-session-agent-registry session)
                (list (cons "/root/published" record)))
          (with-current-buffer root-buffer
            (org-mode)
            (setq-local niwa--session session)
            (setq-local niwa--workspace
                        (niwa-session-workspace session)))
          (cl-letf (((symbol-function 'file-remote-p)
                     (lambda (path &optional _identification _connected)
                       (and (equal path root-dir) "/mock:")))
                    ((symbol-function
                      'niwa-session-artifacts-artifact-present-p)
                     (lambda (seen-session logical)
                       (should (eq session seen-session))
                       (should (equal relative logical))
                       t))
                    ((symbol-function 'niwa-agent-conversation-hydrate)
                     (lambda (invocation parent logical &optional inspection)
                       (setq seen
                             (list invocation parent logical inspection)
                             hydrated-buffer
                             (generate-new-buffer " *agent-published*"))
                       hydrated-buffer)))
            (niwa-test--with-captured-messages nil
              (should (= 0 (niwa-agent-persistence-restore-tree
                            session root-buffer nil)))
              (should-not seen)
              (niwa-agent-persistence-ensure-conversation session record root-buffer)))
          (should (equal relative (nth 2 seen)))
          (should-not (nth 3 seen))
          (should (eq hydrated-buffer
                      (niwa-agent-record-conversation-buffer record))))
      (when (buffer-live-p hydrated-buffer)
        (kill-buffer hydrated-buffer))
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (delete-directory root-dir t)))

  :doc "normalizes read-only persisted activity without a live invocation"
  (let* ((root-dir (make-temp-file "niwa-agent-readonly-tree-" t))
         (session (niwa-agent-persistence-test--session root-dir))
         (root-buffer (generate-new-buffer " *agent-readonly-root*"))
         (relative "agents/active.chat.org")
         (record
          (niwa-agent-record--create
           :id "active" :path "/root/active" :parent-path "/root"
           :role "default"
           :configuration (niwa-agent-persistence-test--configuration)
           :activity 'running
           :blockers (list 'persisted)
           :conversation-location relative)))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) root-dir
                (niwa-session-agent-registry session)
                (list (cons "/root/active" record)))
          (niwa-agent-persistence-test--publish
           session (cons relative "* Agent Task: active\nDurable context.\n"))
          (with-current-buffer root-buffer
            (org-mode)
            (setq-local niwa--session session)
            (setq-local niwa--workspace
                        (niwa-session-workspace session)))
          (niwa-test--with-captured-messages nil
            (should (= 0 (niwa-agent-persistence-restore-tree
                          session root-buffer t))))
          (should (eq 'idle (niwa-agent-record-activity record)))
          (should-not (niwa-agent-record-invocation record))
          (should-not (niwa-agent-record-blockers record))
          (should-not (niwa-agent-control-active-turn-p session))
          (should (equal '(:path "/root/active" :previous-activity idle)
                         (niwa-agent-control-interrupt
                          session "/root/active"))))
      (niwa-agent-persistence-test--release session)
      (niwa-agent-control-teardown-session session)
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (delete-directory root-dir t))))

(niwa-deftest niwa-agent-persistence-ensure-conversation ()
  ,test
  (test)
  :doc "hydrates a local override for inspection and retained follow-up"
  (let* ((root (make-temp-file "niwa-agent-lazy-" t))
         (session (niwa-agent-persistence-test--session root))
         (parent (generate-new-buffer " *lazy-agent-root*"))
         (configuration (niwa-agent-persistence-test--configuration))
         (relative "agents/lazy.chat.org")
         (record (niwa-agent-record--create
                  :id "lazy" :path "/root/lazy" :parent-path "/root"
                  :role "default" :activity 'idle :settled-outcome 'completed
                  :working-directory (file-name-as-directory root)
                  :configuration configuration :conversation-location relative))
         (sibling (niwa-agent-record--create :path "/root/sibling" :activity 'running))
         inspected resident dispatched)
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) root
                (niwa-session-root-buffer session) parent
                (niwa-session-agent-registry session)
                (list (cons "/root/lazy" record) (cons "/root/sibling" sibling)))
          (niwa-agent-persistence-test--publish
           session (cons relative "* Agent Task: original\nRetained decision.\n"))
          (with-current-buffer parent
            (setq-local niwa--session session
                        niwa--workspace (niwa-session-workspace session)))
          ;; Read-only observation can be reused, and does not touch another agent.
          (setq inspected (niwa-agent-persistence-ensure-conversation
                           session record parent t))
          (should (equal (file-name-as-directory root)
                         (buffer-local-value 'default-directory inspected)))
          (should (equal (niwa-agent-record-working-directory record)
                         (niwa-agent-invocation-working-directory
                          (buffer-local-value 'niwa--agent-invocation inspected))))
          (should (buffer-local-value 'buffer-read-only inspected))
          (should (buffer-local-value 'niwa-session--inspection-buffer-p inspected))
          (should-not (buffer-local-value 'buffer-offer-save inspected))
          (should (eq inspected (niwa-agent-persistence-ensure-conversation
                                 session record parent t)))
          (should (eq 'running (niwa-agent-record-activity sibling)))
          (should-not (niwa-agent-record-conversation-buffer sibling))
          ;; A follow-up needs owned writable bytes, not the inspection buffer.
          (with-current-buffer parent
            (let ((niwa-agent-control-suppress-persistence t))
              (cl-letf (((symbol-function 'niwa-agent-runtime-dispatch)
                         (lambda (_agent _description _message &rest options)
                           (setq resident (plist-get options :retained-buffer)
                                 dispatched t)
                           (should-not (buffer-local-value 'buffer-read-only resident))
                           (should (with-current-buffer resident
                                     (string-search "Retained decision." (buffer-string))))
                           (should (eq configuration (plist-get options :frozen-configuration)))
                           t)))
                (niwa-agent-control-send-message session "/root/lazy" "Continue"))))
          (should dispatched)
          (should-not (eq inspected resident))
          (should (equal (file-name-as-directory root)
                         (buffer-local-value 'default-directory resident)))
          (should (eq resident (niwa-agent-persistence-ensure-conversation
                                session record parent)))
          (setf (niwa-agent-record-activity record) 'idle)
          (kill-buffer resident)
          (setf (niwa-agent-record-conversation-buffer record) nil)
          ;; Missing durable bytes fail before any provider dispatch and can retry.
          (niwa-agent-persistence-test--publish
           session (cons relative :delete))
          (should-error (niwa-agent-persistence-ensure-conversation session record parent))
          (should-not (niwa-agent-record-conversation-buffer record))
          (niwa-agent-persistence-test--publish
           session (cons relative "Recovered transcript\n"))
          (setq resident (niwa-agent-persistence-ensure-conversation session record parent))
          (should (with-current-buffer resident (string-search "Recovered transcript" (buffer-string)))))
      (niwa-agent-persistence-test--release session)
      (niwa-agent-control-teardown-session session)
      (dolist (buffer (list inspected resident parent))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t)))

  :doc "passes explicit nil and remote overrides unchanged into lazy hydration"
  (dolist (directory '(nil "/ssh:example.invalid:/srv/retained/"))
    (let* ((session (niwa-agent-persistence-test--session))
           (record (niwa-agent-persistence-test--record))
           (parent (generate-new-buffer " *cwd-hydrate-root*"))
           (conversation (generate-new-buffer " *cwd-hydrate-agent*"))
           hydrated)
      (unwind-protect
          (progn
            (setf (niwa-session-save-path session) temporary-file-directory
                  (niwa-agent-record-working-directory record) directory)
            (with-current-buffer parent (setq-local niwa--session session))
            (cl-letf (((symbol-function 'niwa-agent-conversation-hydrate)
                       (lambda (invocation root relative inspection)
                         (setq hydrated t)
                         (should (eq root parent))
                         (should (equal "agents/task.chat.org" relative))
                         (should inspection)
                         (should (equal directory
                                        (niwa-agent-invocation-working-directory
                                         invocation)))
                         conversation)))
              (should (eq conversation
                          (niwa-agent-persistence-ensure-conversation
                           session record parent t))))
            (should hydrated))
        (kill-buffer conversation)
        (kill-buffer parent)))))

(provide 'test-niwa-agent-persistence)

;;; test-niwa-agent-persistence.el ends here
