;;; room.el --- Isolated real-host browser acceptance fixture -*- lexical-binding: t; -*-
(require 'gptel-openai)
(require 'helpers)
(require 'mevedel)
(require 'mevedel-chat)
(require 'mevedel-tools)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-tool-editing)
(require 'mevedel-shared-editing)
(require 'mevedel-view)
(require 'mevedel-pending-inputs)
(require 'mevedel-pipeline)
(require 'tramp-sh)

(defvar editing-test-root (getenv "MEVEDEL_EDITING_TEST_ROOT"))
(when-let* ((config (getenv "MEVEDEL_TEST_SSH_CONFIG")))
  (setq tramp-use-connection-share t
        tramp-ssh-controlmaster-options (format "-F %s" (shell-quote-argument config))))
(defvar editing-test-workspace-root
  (if-let* ((remote (getenv "MEVEDEL_TEST_SSH_ROOT")))
      (make-temp-file (file-name-concat remote "shared-editing-") t)
    editing-test-root))
(defvar editing-test-buffer (get-buffer-create " *shared editing acceptance*"))
(defvar editing-test-node mevedel-shared-editing-node-program)
(defun editing-test-store-modes (mode)
  "Set the artifact store and each of its item directories to MODE."
  (let ((store (mevedel-artifact-store-directory
                (mevedel-session-workspace editing-test-session))))
    (when (file-directory-p store)
      (dolist (directory (cons store (directory-files store t "\\`[^.]")))
        (when (file-directory-p directory) (set-file-modes directory mode))))))
(defvar editing-test-session
  (mevedel-session-create
   "editing" (mevedel-workspace--create :type 'project :root editing-test-workspace-root)
   editing-test-workspace-root))
(unless (mevedel-session-codec-portable-authority-p editing-test-session)
  (error "Browser acceptance requires the normal project lease"))
(setf (mevedel-session-permission-mode editing-test-session) 'full-auto)
(puthash (mevedel-execution-target-identity (mevedel-session-execution-target editing-test-session))
         t mevedel-session-durability--disclosed-targets)
(setq mevedel-collaboration-relay-url (getenv "MEVEDEL_EDITING_RELAY"))
(with-current-buffer editing-test-buffer
  (mevedel-chat-prepare-transcript-buffer)
  (setq-local mevedel--session editing-test-session)
  (setq default-directory editing-test-workspace-root)
  (mevedel-session-set-root-buffer editing-test-session editing-test-buffer)
  (mevedel-session-set-pending-input-paused editing-test-session t)
  (let ((view (mevedel-view--ensure editing-test-buffer)))
    (with-current-buffer view
      (goto-char (point-max))
      (insert "> Host draft\nsecond line")))
  (mevedel-tool-editing--register)
  (let ((room (mevedel-collaboration--start editing-test-session editing-test-buffer)))
    (write-region (mevedel-shared-editing--json
                   (list :full (plist-get room :link-full) :view (plist-get room :link-view)
                         :owner (plist-get room :link-owner)))
                  nil (file-name-concat editing-test-root "links.json") nil 'silent)))

;; The test process supplies deterministic model calls through real tools.
;; This file is not packaged or loaded by the product.
(while (not (file-exists-p (file-name-concat editing-test-root "stop")))
  (accept-process-output nil 0.03)
  (let ((input (file-name-concat editing-test-root "request.json")))
    (when (file-exists-p input)
      (let ((command (mevedel-shared-editing--parse
                      (with-temp-buffer (insert-file-contents input) (buffer-string)))))
        (delete-file input)
        (with-current-buffer editing-test-buffer
          (condition-case err
              (pcase (plist-get command :tool)
                ("RequestState"
                 (if (eq (plist-get (plist-get command :args) :busy) t)
                     (mevedel-request-begin editing-test-session)
                   (mevedel-request-end))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("CompactHistory"
                 (goto-char (point-max))
                 (mevedel--insert-user-turn "Earlier room question")
                 (insert (propertize "A preserved earlier answer.\n" 'gptel 'response))
                 (mevedel-session-artifacts-rotate-segment
                  editing-test-session editing-test-buffer "Earlier work was summarized.")
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("EndShare"
                 (mevedel-collaboration--stop-internal
                  (mevedel-collaboration--room-for-session editing-test-session) 'test)
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("FenceHost"
                 ;; Model a paused former host waking after another Emacs
                 ;; took its items over: its remembered leases are stale.
                 (let ((workspace (mevedel-session-workspace editing-test-session))
                       (mevedel-session-durability--client-id (make-string 64 ?f)))
                   (dolist (id (mevedel-shared-editing-ids workspace))
                     (let ((directory (mevedel-artifact-lease-directory workspace id)))
                       (unless (mevedel-session-durability--claim-next
                                directory (mevedel-session-durability--lease-head directory)
                                "*successor*")
                         (error "Successor could not claim %s" id)))))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("RuntimeAvailable"
                 (setq mevedel-shared-editing-node-program
                       (if (eq (plist-get (plist-get command :args) :available) t)
                           editing-test-node
                         (file-name-concat editing-test-root "missing-node")))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("ImportShared"
                 (mevedel-shared-editing-call
                  (mevedel-session-workspace editing-test-session)
                  (append (list :action "import" :format "excalidraw" :opId "import" :actor "Guest: Test"
                                :id (substring (secure-hash 'sha256 (format "%s" (random t))) 0 32))
                          (plist-get command :args))
                  (lambda (reply)
                    (write-region (mevedel-shared-editing--json
                                   (if (plist-get reply :error) (list :error (plist-get reply :error))
                                     (list :status "success"
                                           :result (mevedel-shared-editing--json (plist-get reply :result)))))
                                  nil (file-name-concat editing-test-root "reply.json") nil 'silent))))
                ;; The test's own view of an item: its full state, which the
                ;; model reads in parts through shared:// addresses instead.
                ("ReadShared"
                 (let ((id (plist-get (plist-get command :args) :id))
                       (reply-file (file-name-concat editing-test-root "reply.json")))
                   (if (not id)
                       (write-region (mevedel-shared-editing--json
                                      (list :status "success"
                                            :result (mevedel-shared-editing--json
                                                     (vconcat (mevedel-shared-editing-list
                                                               (mevedel-session-workspace editing-test-session))))))
                                     nil reply-file nil 'silent)
                     (mevedel-shared-editing-call
                      (mevedel-session-workspace editing-test-session) (list :action "read" :id id)
                      (lambda (reply)
                        (write-region (mevedel-shared-editing--json
                                       (if (plist-get reply :error) (list :status "error" :result (plist-get reply :error))
                                         (list :status "success"
                                               :result (mevedel-shared-editing--json (plist-get reply :result)))))
                                      nil reply-file nil 'silent))))))
                ("RestartHelper"
                 (mevedel-shared-editing-stop)
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("StorageWritable"
                 (editing-test-store-modes
                  (if (eq (plist-get (plist-get command :args) :writable) t) #o700 #o500))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("InspectTest"
                 (write-region
                  (mevedel-shared-editing--json
                   (list :draft (with-current-buffer mevedel--view-buffer (mevedel-view--visible-draft))
                         :attachments (vconcat (mapcar (lambda (entry) (length (plist-get entry :guest-paths)))
                                                       (mevedel-session-pending-inputs editing-test-session 'follow-up)))
                         :queue (vconcat (mapcar (lambda (entry) (plist-get entry :input))
						 (mevedel-session-pending-inputs editing-test-session 'follow-up)))))
                  nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                (_
                 (mevedel-pipeline-run-tool-outcome
                  (mevedel-tool-get (plist-get command :tool))
                  (lambda (reply)
                    (write-region (mevedel-shared-editing--json
                                   (list :status (symbol-name (plist-get reply :status))
                                         :result (plist-get reply :result))) nil
					 (file-name-concat editing-test-root "reply.json") nil 'silent))
                  (let ((args (plist-get command :args)))
                    ;; gptel's empty top-level argument object is a nil plist.
                    (if (hash-table-p args) nil args)))))
            (error
             (write-region (mevedel-shared-editing--json (list :error (error-message-string err))) nil
                           (file-name-concat editing-test-root "reply.json") nil 'silent))))))))
(mevedel-shared-editing-stop)
(mevedel-artifact-lease-release-all)
(mevedel-collaboration--stop-for-emacs)
(editing-test-store-modes #o700)
(when (mevedel-session-save-path editing-test-session)
  (mevedel-session-persistence-lock-release
   (mevedel-session-save-path editing-test-session) editing-test-session))
(when (file-remote-p editing-test-workspace-root)
  (delete-directory editing-test-workspace-root t))
(kill-emacs 0)
