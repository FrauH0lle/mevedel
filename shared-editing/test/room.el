;;; room.el --- Isolated real-host browser acceptance fixture -*- lexical-binding: t; -*-
(require 'gptel-openai)
(require 'helpers)
(require 'mevedel)
(require 'mevedel-tools)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-tool-editing)
(require 'mevedel-view)
(require 'mevedel-pending-inputs)
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
(defvar editing-test-successor nil)
(defvar editing-test-node mevedel-shared-editing-node-program)
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
                ("EndShare"
                 (mevedel-collaboration--stop-internal
                  (mevedel-collaboration--room-for-session editing-test-session) 'test)
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("FenceHost"
                 (let ((lease (copy-tree (mevedel-session-lease editing-test-session)))
                       (path (mevedel-session-save-path editing-test-session)))
                   (mevedel-session-durability-lease-release path editing-test-session)
                   (setq editing-test-successor (copy-mevedel-session editing-test-session))
                   (let ((mevedel-session-durability--client-id (make-string 64 ?f)))
                     (unless (mevedel-session-durability-lease-acquire
                              path "*successor*" editing-test-successor)
                       (error "Successor could not acquire the released lease")))
                   ;; Model a paused former host waking with its old epoch.
                   (setf (mevedel-session-lease editing-test-session) lease))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("RuntimeAvailable"
                 (setq mevedel-shared-editing-node-program
                       (if (eq (plist-get (plist-get command :args) :available) t)
                           editing-test-node
                         (file-name-concat editing-test-root "missing-node")))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("RestartHelper"
                 (mevedel-shared-editing-stop)
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("StorageWritable"
                 (set-file-modes
                  (file-name-concat (mevedel-session-save-path editing-test-session) ".publications")
                  (if (eq (plist-get (plist-get command :args) :writable) t) #o700 #o500))
                 (write-region "{}" nil (file-name-concat editing-test-root "reply.json") nil 'silent))
                ("RetryPublication"
                 (mevedel-session-publication-retry editing-test-session)
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
(with-current-buffer editing-test-buffer (mevedel-shared-editing-stop))
(mevedel-collaboration--stop-for-emacs)
(when editing-test-successor
  (let ((mevedel-session-durability--client-id (make-string 64 ?f)))
    (mevedel-session-durability-lease-release
     (mevedel-session-save-path editing-test-successor) editing-test-successor)))
(when (mevedel-session-save-path editing-test-session)
  (set-file-modes (file-name-concat (mevedel-session-save-path editing-test-session) ".publications") #o700)
  (mevedel-session-persistence-lock-release
   (mevedel-session-save-path editing-test-session) editing-test-session))
(when (file-remote-p editing-test-workspace-root)
  (delete-directory editing-test-workspace-root t))
(kill-emacs 0)
