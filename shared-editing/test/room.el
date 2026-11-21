;;; room.el --- Isolated real-host browser acceptance fixture -*- lexical-binding: t; -*-
(require 'gptel-openai)
(require 'helpers)
(require 'niwa)
(require 'niwa-chat)
(require 'niwa-tools)
(require 'niwa-collaboration)
(require 'niwa-collaboration-projection)
(require 'niwa-collaboration-transport)
(require 'niwa-tool-editing)
(require 'niwa-shared-editing)
(require 'niwa-view)
(require 'niwa-pending-inputs)
(require 'niwa-pipeline)
(require 'tramp-sh)

;; Package setup registers the tools and defines the presets a new session,
;; such as an artifact's own conversation, starts with.
(niwa-tools-register)
(niwa--define-presets)

(defvar editing-test-root (getenv "NIWA_EDITING_TEST_ROOT"))
(when-let* ((config (getenv "NIWA_TEST_SSH_CONFIG")))
  (setq tramp-use-connection-share t
        tramp-ssh-controlmaster-options (format "-F %s" (shell-quote-argument config))))
(defvar editing-test-workspace-root
  (if-let* ((remote (getenv "NIWA_TEST_SSH_ROOT")))
      (make-temp-file (file-name-concat remote "shared-editing-") t)
    editing-test-root))
(defvar editing-test-buffer (get-buffer-create " *shared editing acceptance*"))
(defvar editing-test-node niwa-shared-editing-node-program)
(defun editing-test-store-modes (mode)
  "Set authored and protected artifact storage directories to MODE."
  (let* ((workspace (niwa-session-workspace editing-test-session))
         (store (niwa-artifact-store-directory workspace)))
    (when (file-directory-p store)
      (dolist (directory
               (append (list store (file-name-concat store ".state"))
                       (cl-loop for id in (niwa-artifact-store-ids workspace)
                                append (list (niwa-artifact-store-artifact-directory workspace id)
                                             (niwa-artifact-store-bookkeeping-directory workspace id)))))
        (when (file-directory-p directory) (set-file-modes directory mode))))))
(defvar editing-test-session
  (niwa-session-create
   "editing" (niwa-workspace--create :type 'project :root editing-test-workspace-root)
   editing-test-workspace-root))
(unless (niwa-session-codec-portable-authority-p editing-test-session)
  (error "Browser acceptance requires the normal project lease"))
(setf (niwa-session-permission-mode editing-test-session) 'full-auto)
(puthash (niwa-execution-target-identity (niwa-session-execution-target editing-test-session))
         t niwa-session-durability--disclosed-targets)
(setq niwa-collaboration-relay-url (getenv "NIWA_EDITING_RELAY"))
(with-current-buffer editing-test-buffer
  (niwa-chat-prepare-transcript-buffer)
  (setq-local niwa--session editing-test-session)
  (setq default-directory editing-test-workspace-root)
  (niwa-session-set-root-buffer editing-test-session editing-test-buffer)
  (niwa-session-set-pending-input-paused editing-test-session t)
  (let ((view (niwa-view--ensure editing-test-buffer)))
    (with-current-buffer view
      (goto-char (point-max))
      (insert "> Host draft\nsecond line")))
  (niwa-tool-editing--register)
  (let ((room (niwa-collaboration--start editing-test-session editing-test-buffer)))
    (write-region (niwa-shared-editing--json
                   (list :full (plist-get room :link-full) :view (plist-get room :link-view)
                         :owner (plist-get room :link-owner)))
                  nil (file-name-concat editing-test-root "links.json") nil 'silent)))

;; The test process supplies deterministic model calls through real tools.
;; This file is not packaged or loaded by the product.
(while (not (file-exists-p (file-name-concat editing-test-root "stop")))
  (accept-process-output nil 0.03)
  (let ((input (file-name-concat editing-test-root "request.json")))
    (when (file-exists-p input)
      (let ((command (niwa-shared-editing--parse
                      (with-temp-buffer (insert-file-contents input) (buffer-string)))))
        (delete-file input)
        (with-current-buffer editing-test-buffer
          (condition-case err
              (pcase (plist-get command :tool)
                ("RequestState"
                 (if (eq (plist-get (plist-get command :args) :busy) t)
                     (niwa-request-begin editing-test-session)
                   (niwa-request-end))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("CompactHistory"
                 (goto-char (point-max))
                 (niwa--insert-user-turn "Earlier room question")
                 (insert (propertize "A preserved earlier answer.\n" 'gptel 'response))
                 (niwa-session-artifacts-rotate-segment
                  editing-test-session editing-test-buffer "Earlier work was summarized.")
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("EndShare"
                 (niwa-collaboration--stop-internal
                  (niwa-collaboration--room-for-session editing-test-session) 'test)
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ;; The host operation behind sharing an artifact through a
                ;; link: `share', `revoke' or `rotate' one of the session's items.
                ("ArtifactSharing"
                 (require 'niwa-collaboration-artifact-room)
                 (let* ((args (plist-get command :args))
                        (links (niwa-collaboration-artifact-room-sharing
                                (niwa-session-workspace editing-test-session)
                                (plist-get args :id) (intern (plist-get args :operation)))))
                   (write-region (if links
                                     (niwa-shared-editing--json
                                      (list :view (plist-get links :link-view)
                                            :edit (plist-get links :link-edit)))
                                   "{}")
                                 nil (file-name-concat editing-test-root "reply.json") nil 'silent)))
                ;; An HTML artifact's file as the origin's own editor sees
                ;; it: with `content', write it first, creating the artifact
                ;; (that file its primary one, titled `title') if needed.
                ;; Reply with the file's text, null when it is absent.
                ("HtmlFile"
                 (let* ((args (plist-get command :args))
                        (workspace (niwa-session-workspace editing-test-session))
                        (id (plist-get args :id))
                        (file (plist-get args :file))
                        (content (plist-get args :content)))
                   (when (stringp content)
                     (unless (niwa-artifact-store-meta workspace id)
                       (niwa-artifact-store-create-meta workspace id file 'html
                                                           (plist-get args :title)))
                     (let ((path (niwa-artifact-store-file-path workspace id file))
                           (coding-system-for-write 'utf-8-unix))
                       (make-directory (file-name-directory path) t)
                       (write-region content nil path nil 'silent)))
                   (let ((bytes (niwa-artifact-store-read-file workspace id file)))
                     (write-region (niwa-shared-editing--json
                                    (list :text (and bytes (decode-coding-string bytes 'utf-8-unix))))
                                   nil (file-name-concat editing-test-root "reply.json") nil 'silent))))
                ("FenceHost"
                 ;; Model a paused former host waking after another Emacs
                 ;; took its items over: its remembered leases are stale.
                 (let ((workspace (niwa-session-workspace editing-test-session))
                       (niwa-session-durability--client-id (make-string 64 ?f)))
                   (dolist (id (mapcar (lambda (item) (plist-get item :id)) (niwa-shared-editing-list workspace)))
                     (let ((directory (niwa-artifact-lease-directory workspace id)))
                       (unless (niwa-artifact-lease--claim
                                directory (niwa-session-durability--lease-head directory)
                                id)
                         (error "Successor could not claim %s" id)))))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("RuntimeAvailable"
                 (setq niwa-shared-editing-node-program
                       (if (eq (plist-get (plist-get command :args) :available) t)
                           editing-test-node
                         (file-name-concat editing-test-root "missing-node")))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("ImportShared"
                 (niwa-shared-editing-call
                  (niwa-session-workspace editing-test-session)
                  (append (list :action "import" :format "excalidraw" :opId "import" :actor "Guest: Test"
                                :id (substring (secure-hash 'sha256 (format "%s" (random t))) 0 32))
                          (plist-get command :args))
                  (lambda (reply)
                    (write-region (niwa-shared-editing--json
                                   (if (plist-get reply :error) (list :error (plist-get reply :error))
                                     (list :status "success"
                                           :result (niwa-shared-editing--json (plist-get reply :result)))))
                                  nil (file-name-concat editing-test-root "reply.json") nil 'silent))))
                ;; The test's own view of an item: its full state, which the
                ;; model reads in parts through shared:// addresses instead.
                ("ReadShared"
                 (let ((id (plist-get (plist-get command :args) :id))
                       (reply-file (file-name-concat editing-test-root "reply.json")))
                   (if (not id)
                       (write-region (niwa-shared-editing--json
                                      (list :status "success"
                                            :result (niwa-shared-editing--json
                                                     (vconcat (niwa-shared-editing-list
                                                               (niwa-session-workspace editing-test-session))))))
                                     nil reply-file nil 'silent)
                     (niwa-shared-editing-call
                      (niwa-session-workspace editing-test-session) (list :action "read" :id id)
                      (lambda (reply)
                        (write-region (niwa-shared-editing--json
                                       (if (plist-get reply :error) (list :status "error" :result (plist-get reply :error))
                                         (list :status "success"
                                               :result (niwa-shared-editing--json (plist-get reply :result)))))
                                      nil reply-file nil 'silent))))))
                ("RestartHelper"
                 (niwa-shared-editing-stop)
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("StorageWritable"
                 (editing-test-store-modes
                  (if (eq (plist-get (plist-get command :args) :writable) t) #o700 #o500))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("InspectTest"
                 (write-region
                  (niwa-shared-editing--json
                   (list :draft (with-current-buffer niwa--view-buffer (niwa-view--visible-draft))
                         :attachments (vconcat (mapcar (lambda (entry) (length (plist-get entry :guest-paths)))
                                                       (niwa-session-pending-inputs editing-test-session 'follow-up)))
                         :queue (vconcat (mapcar (lambda (entry) (plist-get entry :input))
						 (niwa-session-pending-inputs editing-test-session 'follow-up)))))
                  nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                (_
                 (niwa-pipeline-run-tool-outcome
                  (niwa-tool-get (plist-get command :tool))
                  (lambda (reply)
                    (write-region (niwa-shared-editing--json
                                   (list :status (symbol-name (plist-get reply :status))
                                         :result (plist-get reply :result))) nil
					 (file-name-concat editing-test-root "reply.json") nil 'silent))
                  (let ((args (plist-get command :args)))
                    ;; gptel's empty top-level argument object is a nil plist.
                    (if (hash-table-p args) nil args)))))
            (error
             (write-region (niwa-shared-editing--json (list :error (error-message-string err))) nil
                           (file-name-concat editing-test-root "reply.json") nil 'silent))))))))
(niwa-shared-editing-stop)
(niwa-artifact-lease-release-all)
(niwa-collaboration--stop-for-emacs)
(editing-test-store-modes #o700)
(when (niwa-session-save-path editing-test-session)
  (niwa-session-persistence-lock-release
   (niwa-session-save-path editing-test-session) editing-test-session))
(when (file-remote-p editing-test-workspace-root)
  (delete-directory editing-test-workspace-root t))
(kill-emacs 0)
