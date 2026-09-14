;;; test-mevedel-session-naming.el --- Session title tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise naming through real session metadata and controlled model replies.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name
                                  byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-session-naming)

(defmacro test-mevedel-session-naming--with-session (&rest body)
  "Run BODY with an unnamed SESSION and its root BUFFER in a temp workspace."
  (declare (indent 0))
  `(pcase-let* ((`(,workspace . ,directory)
                 (test-mevedel-session-persistence--make-tempdir-workspace))
                (session (mevedel-session-create nil workspace))
                (buffer (generate-new-buffer " *test-session-naming*")))
     (unwind-protect
         (with-current-buffer buffer
           (org-mode)
           (setq-local mevedel--session session
                       mevedel--workspace workspace)
           (setf (mevedel-session-root-buffer session) buffer)
           (insert "Fix the session chooser\n")
           ,@body)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (mevedel-session-naming-cancel)))
       (test-mevedel-session-persistence--release-and-kill buffer session)
       (delete-directory directory t)
       (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-naming-normalize ()
  ,test (test)
  :doc "preserves readable Unicode and collapses whitespace"
  (should (equal "Ändere die Sitzung"
                 (mevedel-session-naming-normalize "  Ändere\n die\tSitzung  ")))
  :doc "rejects a blank display name"
  (should-error (mevedel-session-naming-normalize " \n\t") :type 'user-error))

(mevedel-deftest mevedel-session-naming-consider (:quiet t)
  ,test (test)
  :doc "isolates the naming workload and persists a bounded title without moving files"
  (test-mevedel-session-naming--with-session
    (let ((gptel-request-function (symbol-function 'gptel-request))
          (resolve (symbol-function 'mevedel-model-resolve-workload))
          (original-id (mevedel-session-session-id session))
          (authored (concat "Fix the session chooser " (make-string 2200 ?x)))
          reply request-buffer captured workload)
      (setq-local gptel-model 'gpt-4o-mini
                  gptel-reasoning-effort nil)
      (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                 (lambda (value &rest args)
                   (setq workload value)
                   (apply resolve value args)))
                ((symbol-function 'gptel-request)
                 (lambda (prompt &rest options)
                   (setq reply (plist-get options :callback)
                         request-buffer (current-buffer)
                         captured (list prompt options))
                   ;; Use the real gptel serializer without sending a request.
                   (let ((fsm (apply gptel-request-function prompt
                                     :dry-run t options)))
                     (should-not (plist-get (gptel-fsm-info fsm) :tools)))
                   (should-not gptel-use-context)
                   (should-not gptel-track-response)
                   (should-not (plist-get options :transforms)))))
        (mevedel-session-naming-consider session authored)
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (should (eq workload 'naming))
        (should (= 2000 (length (car captured))))
        (should-not (eq buffer request-buffer))
        (let ((old-path (mevedel-session-save-path session))
              (file buffer-file-name)
              (transcript (buffer-string)))
          (funcall reply "\"Fix the session chooser\"" nil)
          (should (equal "Fix the session chooser" (mevedel-session-name session)))
          (should (equal original-id (mevedel-session-session-id session)))
          (should (equal old-path (mevedel-session-save-path session)))
          (should (equal file buffer-file-name))
          (should (equal transcript (buffer-string)))
          (should-not (buffer-live-p request-buffer))
          (should-not mevedel-session-naming--cancel)
          (let* ((sidecar (mevedel-session-codec-read
                           (mevedel-session-artifacts-sidecar-path old-path)))
                 (restored (plist-get (mevedel-session-codec-deserialize sidecar workspace) :session)))
            (should (equal "Fix the session chooser" (mevedel-session-name restored)))
            (should-not (mevedel-session-auto-name-pending restored)))))))
  :doc "failed, cancelled and stale replies retain the fallback or manual title"
  (dolist (mode '(failure blank oversized manual cancelled killed timeout))
    (test-mevedel-session-naming--with-session
      (let ((original-id (mevedel-session-session-id session))
            (timer-function (symbol-function 'run-at-time))
            reply request-buffer timeout calls)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (time repeat function &rest args)
                     (if (equal time 30)
                         (progn (setq timeout function) nil)
                       (apply timer-function time repeat function args))))
                  ((symbol-function 'gptel-request)
                   (lambda (_prompt &rest options)
                     (setq calls (1+ (or calls 0))
                           request-buffer (current-buffer)
                           reply (plist-get options :callback)))))
          (mevedel-session-naming-consider session "Fix this")
          (with-timeout (3 (ert-fail "Title request did not start"))
            (while (null reply) (accept-process-output nil 0.01)))
          (pcase mode
            ('failure (funcall reply nil nil))
            ('blank (funcall reply " \n " nil))
            ('oversized (funcall reply (make-string 4097 ?x) nil))
            ('manual (mevedel-rename-session "My title"))
            ('cancelled (mevedel-session-naming-cancel))
            ('killed (set-buffer-modified-p nil) (kill-buffer buffer))
            ('timeout (funcall timeout)))
          (funcall reply "Stale title" nil)
          (should (equal (if (eq mode 'manual) "My title" original-id)
                         (mevedel-session-name session)))
          (should-not (buffer-live-p request-buffer))
          (mevedel-session-naming-consider session "Second attempt")
          (should (= 1 calls))
          (should-not (mevedel-session-auto-name-pending session))))))
  :doc "cancellation during a save prevents an orphan inference request"
  (test-mevedel-session-naming--with-session
    (let ((save (symbol-function 'mevedel-session-artifacts-save))
          saved sent)
      (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
                 (lambda (&rest args)
                   (prog1 (apply save args)
                     (setq saved t)
                     (mevedel-session-naming-cancel))))
                ((symbol-function 'gptel-request)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Naming was not scheduled"))
          (while (not saved) (accept-process-output nil 0.01)))
        (should-not sent)
        (should-not mevedel-session-naming--cancel))))
  :doc "manual rename during authority I/O wins over an automatic title"
  (test-mevedel-session-naming--with-session
    (let ((assert-authority (symbol-function 'mevedel-session-artifacts-assert-mutation-authority))
          reply injected)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options) (setq reply (plist-get options :callback)))))
        (mevedel-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (cl-letf (((symbol-function 'mevedel-session-artifacts-assert-mutation-authority)
                   (lambda (&rest args)
                     (prog1 (apply assert-authority args)
                       (unless injected
                         (setq injected t)
                         (mevedel-rename-session "Manual wins"))))))
          (funcall reply "Automatic loses" nil))
        (should injected)
        (should (equal "Manual wins" (mevedel-session-name session))))))
  :doc "waits for publication before applying a valid title"
  (test-mevedel-session-naming--with-session
    (let ((mevedel-transport-retry-seconds 0.01) reply)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest options) (setq reply (plist-get options :callback)))))
        (mevedel-session-naming-consider session "Fix this")
        (with-timeout (3 (ert-fail "Title request did not start"))
          (while (null reply) (accept-process-output nil 0.01)))
        (unwind-protect
            (progn
              (setf (mevedel-session-publication-active-p session) t)
              (funcall reply "Deferred title" nil)
              (should mevedel-session-naming--cancel)
              (should (equal (mevedel-session-session-id session)
                             (mevedel-session-name session))))
          (setf (mevedel-session-publication-active-p session) nil))
        (with-timeout (3 (ert-fail "Title did not apply after publication"))
          (while mevedel-session-naming--cancel (accept-process-output nil 0.01)))
        (should (equal "Deferred title" (mevedel-session-name session))))))
  :doc "explicit names and transient conversations do not request inference"
  (test-mevedel-session-naming--with-session
    (setf (mevedel-session-auto-name-pending session) nil)
    (mevedel-session-naming-consider session "Fix this")
    (should-not mevedel-session-naming--cancel)
    (setf (mevedel-session-auto-name-pending session) t
          (mevedel-session-audit-session session) (mevedel-session-create "parent" workspace))
    (mevedel-session-naming-consider session "Fix this")
    (should-not mevedel-session-naming--cancel)))

(mevedel-deftest mevedel-session-persistence--save-as (:quiet t)
  ,test (test)
  :doc "an explicit Save As name wins before and during title inference"
  (dolist (in-flight '(nil t))
    (test-mevedel-session-naming--with-session
      (let ((old-id (mevedel-session-session-id session)) reply request-buffer)
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (_prompt &rest options)
                     (setq reply (plist-get options :callback)
                           request-buffer (current-buffer))))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "  My saved session  ")))
          (when in-flight
            (mevedel-session-naming-consider session "Fix this")
            (with-timeout (3 (ert-fail "Title request did not start"))
              (while (null reply) (accept-process-output nil 0.01))))
          (mevedel-save-session t)
          (when reply (funcall reply "Stale title" nil))
          (should (equal "My saved session" (mevedel-session-name session)))
          (should-not (equal old-id (mevedel-session-session-id session)))
          (should-not (mevedel-session-auto-name-pending session))
          (should-not mevedel-session-naming--cancel)
          (should-not (buffer-live-p request-buffer))
          (let ((sidecar (mevedel-session-codec-read
                          (mevedel-session-artifacts-sidecar-path
                           (mevedel-session-save-path session)))))
            (should (equal "My saved session" (plist-get sidecar :session-name)))
            (should-not (plist-get sidecar :auto-name-pending))))))))

(mevedel-deftest mevedel-session-naming-cancel (:quiet t)
  ,test (test)
  :doc "cancels before dispatch without creating a request"
  (test-mevedel-session-naming--with-session
    (let (sent)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-session-naming-consider session "Fix this")
        (should mevedel-session-naming--cancel)
        (mevedel-session-naming-cancel)
        (accept-process-output nil 0.02)
        (should-not sent)
        (should-not mevedel-session-naming--cancel)))))

(mevedel-deftest mevedel-session-naming-rename-view (:quiet t)
  ,test (test)
  :doc "renaming from the view preserves a multiline composer draft"
  (test-mevedel-session-naming--with-session
    (let ((view (mevedel-view--ensure buffer))
          (draft "> Continue here\nwith another line"))
      (unwind-protect
          (with-current-buffer view
            (mevedel-view-test--insert-composer-draft draft)
            (mevedel-rename-session "Ändere die Sitzung")
            (should (equal draft (mevedel-view--visible-draft)))
            (should (equal "Ändere die Sitzung" (mevedel-session-name session))))
        (when (buffer-live-p view) (kill-buffer view))))))

(mevedel-deftest mevedel--pick-session (:quiet t)
  ,test (test)
  :doc "equal titles select distinct session buffers by their IDs"
  (test-mevedel-session-naming--with-session
    (let* ((other (mevedel--chat-buffer "other" t workspace directory))
           (other-session (buffer-local-value 'mevedel--session other)))
      (unwind-protect
          (progn
            (mevedel-session-naming-rename session buffer "Same title")
            (mevedel-session-naming-rename other-session other "Same title")
            (should-error (mevedel--chat-buffer "Same title" nil workspace)
                          :type 'user-error)
            (dolist (selected (list buffer other))
              (let ((selected-id (mevedel-session-session-id
                                  (buffer-local-value 'mevedel--session selected))))
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (_prompt collection &rest _)
                             (car (cl-find-if
                                   (lambda (entry)
                                     (string-suffix-p (concat "[" selected-id "]") (car entry)))
                                   collection)))))
                  (should (eq selected (mevedel--pick-session
                                        (mevedel--workspace-sessions workspace) nil)))))))
        (test-mevedel-session-persistence--release-and-kill other other-session)))))

(mevedel-deftest mevedel-session-naming-rename (:quiet t)
  ,test
  (test)
  :doc "keeps the owned lease and commits renamed metadata through one head"
  (let* ((host "rename-publication")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-rename-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?e))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host local-root "rename transcript\n"))
                       (parent
                        (file-name-directory
                         (directory-file-name session-dir)))
                       (new-id (mevedel-session-session-id session))
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat parent new-id)))
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (mevedel-execution-target-identity
              (mevedel-session-execution-target session))
             t mevedel-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*remote-rename*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer " *remote-rename-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "rename transcript\n"))
                  (mevedel-session-control-transfer-register-root-buffer session buffer)
                  (let ((generation
                         (plist-get (mevedel-session-lease session)
                                    :generation))
                        (head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    (mevedel-session-naming-rename session buffer "renamed")
                    (should (file-directory-p session-dir))
                    (should (file-directory-p new-save-path))
                    (should
                     (= generation
                        (plist-get (mevedel-session-lease session)
                                   :generation)))
                    (should-not
                     (equal head-before
                            (plist-get (mevedel-session-publication session)
                                       :head)))
                    (should
                     (equal "rename transcript\n"
                            (mevedel-session-artifacts-read-artifact
                             session "segment-0001.chat.org" t)))
                    (let ((sidecar
                           (with-temp-buffer
                             (insert
                              (mevedel-session-artifacts-read-artifact
                               session "session.meta.el" t))
                             (goto-char (point-min))
                             (read (current-buffer)))))
                      (should (equal "renamed"
                                     (plist-get sidecar :session-name)))
                      (should (equal new-id
                                     (plist-get sidecar :session-id))))))
              (when session
                (mevedel-session-durability-lease-release
                 (mevedel-session-save-path session) session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "restores the display name after a pre-CAS failure"
  (let* ((host "rename-rollback")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-rename-rollback-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?f))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host local-root "rollback transcript\n"))
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat
                          (file-name-directory
                           (directory-file-name session-dir))
                          "renamed-rollback")))
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (mevedel-execution-target-identity
              (mevedel-session-execution-target session))
             t mevedel-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*remote-rename-rollback*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer
                         " *remote-rename-rollback-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "rollback transcript\n"))
                  (mevedel-session-control-transfer-register-root-buffer session buffer)
                  (let ((head-before
                         (plist-get (mevedel-session-publication session)
                                    :head))
                        (publish-artifact
                         (symbol-function
                          'mevedel-session-publication--publish-artifact)))
                    (cl-letf
                        (((symbol-function
                           'mevedel-session-publication--publish-artifact)
                          (lambda (artifact)
                            (ignore artifact publish-artifact)
                            (signal 'file-error
                                    '("Injected Rename publication failure")))))
                      (should-error
                       (mevedel-session-naming-rename session buffer "renamed")
                       :type 'file-error))
                    (should (file-directory-p session-dir))
                    (should-not (file-directory-p new-save-path))
                    (should (equal session-dir
                                   (mevedel-session-save-path session)))
                    (should (equal "main" (mevedel-session-name session)))
                    (should (equal segment
                                   (buffer-local-value
                                    'buffer-file-name buffer)))
                    (should (equal head-before
                                   (plist-get
                                    (mevedel-session-publication session)
                                    :head)))
                    (should-not
                     (mevedel-session-pending-publication session))
                    (should
                     (mevedel-session-durability-lease-owned-p session))))
              (when session
                (mevedel-session-durability-lease-release
                 (mevedel-session-save-path session) session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-rename-session (:quiet t)
  ,test
  (test)
  :doc "renames the session-name field and the buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (insert "Hi\n")
                (mevedel-session-artifacts-save session buf)
                (let* ((old-save-path (mevedel-session-save-path session))
                       (artifact-directory
                        (file-name-concat old-save-path "tool-results"))
                       (mevedel-sandbox-mode 'off)
                       initial terminal execution-id)
                  (mevedel-execution-start-bash
                   (lambda (value) (setq initial value))
                   :session session :data-buffer buf :owner "agent-a"
                   :owner-context session
                   :command
                   '("sh" "-c" "printf before; sleep 1; printf after")
                   :workdir tempdir :writable-roots (list tempdir)
                   :artifact-directory artifact-directory
                   :yield-time-ms 10)
                  (with-timeout (2 (error "Execution did not yield"))
                    (while (null initial)
                      (accept-process-output nil 0.02)))
                  (setq execution-id
                        (plist-get (plist-get initial :facts) :execution-id))
                  (mevedel-rename-session "alt-permissions")
                  (should (equal "alt-permissions"
                                 (mevedel-session-name session)))
                  (should (equal old-save-path (mevedel-session-save-path session)))
                  (should (file-directory-p old-save-path))
                  ;; Buffer renamed per convention.
                  (should (string-match-p
                           "\\`\\*mevedel:alt-permissions@"
                           (buffer-name buf)))
                  (mevedel-execution-observe
                   session "agent-a" execution-id
                   (lambda (value) (setq terminal value))
                   :wait-ms 5000)
                  (with-timeout (6 (error "Renamed execution did not finish"))
                    (while (null terminal)
                      (accept-process-output nil 0.02)))
                  (should (= 0
                             (plist-get (plist-get terminal :facts)
                                        :exit-code)))
                  (let ((artifact
                         (plist-get (plist-get terminal :facts) :output-path)))
                    (should (string-prefix-p "artifact://" artifact))
                    (should
                     (equal "beforeafter"
                            (mevedel-resource-execute
                             (mevedel-resource-prepare
                              'read artifact (list :session session))
                             (lambda (path _address)
                               (with-temp-buffer
                                 (insert-file-contents path)
                                 (buffer-string)))))))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "publishes the renamed sidecar through the critical seam"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *rename-publication*"))
           (publish-function
            (symbol-function 'mevedel-session-artifacts-publish-text))
           published)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "transcript\n")
            (mevedel-session-artifacts-save session buffer)
            (cl-letf
                (((symbol-function
                   'mevedel-session-artifacts-publish-text)
                  (lambda (actual-session path content &optional coding)
                    (push path published)
                    (funcall publish-function
                             actual-session path content coding))))
              (mevedel-rename-session "renamed"))
            (should
             (equal
              (list
               (mevedel-session-artifacts-sidecar-path
                (mevedel-session-save-path session)))
              published)))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry)))))


(provide 'test-mevedel-session-naming)
;;; test-mevedel-session-naming.el ends here
