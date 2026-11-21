;;; test-niwa-permission-queue-profile.el -- Complete execution profiles -*- lexical-binding: t -*-

;;; Commentary:

;; Real prompt selection, profile reuse, and confined filesystem effects.

;;; Code:

(require 'niwa-tool-exec-registration)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution)
(require 'niwa-permission-prompt)
(require 'niwa-permission-queue)
(require 'niwa-pipeline)
(require 'niwa-sandbox)
(require 'niwa-tool-exec)
(require 'niwa-tool-fs)
(require 'niwa-tools)
(require 'niwa-view)

(niwa-deftest niwa-permission-queue--complete-profile
  (:quiet t)
  (let ((niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (plist-get availability :reason))))
    (let* ((tool-name ,tool-name)
           (narrow-p ,narrow-p)
           (parent (make-temp-file "niwa-pq-profile-" t))
           (root (file-name-concat parent "workspace"))
           (source (file-name-concat parent "source.txt"))
           (cache (file-name-concat parent "cache"))
           (counter (file-name-concat root "counter"))
           (script (file-name-concat root "validate"))
           (expression
            (prin1-to-string
             `(let ((n (1+ (if (file-exists-p ,counter)
                               (with-temp-buffer
                                 (insert-file-contents ,counter)
                                 (string-to-number (buffer-string)))
                             0))))
                (with-temp-file ,counter (insert (number-to-string n)))
                (let ((directory (expand-file-name (format "run-%d" n) ,cache)))
                  (make-directory directory t)
                  (with-temp-buffer
                    (insert-file-contents ,source)
                    (write-region (point-min) (point-max)
                                  (expand-file-name "value" directory))))
                "profile execution complete")))
           (operation (if (equal tool-name "Bash") script expression))
           (success-output (if (equal tool-name "Bash")
                               "profile execution complete"
                             "Result:\n\"profile execution complete\""))
           (args (if (equal tool-name "Bash")
                     (list :command script :description "Probe Bash authority profile")
                   (list :expression expression :mode "fresh")))
           (data (generate-new-buffer " *test-pq-profile-data*"))
           (view (generate-new-buffer " *test-pq-profile-view*"))
           (workspace (niwa-workspace-get-or-create 'project root root "profile"))
           (session (niwa-session-create "main" workspace))
           (niwa-permission-rules nil)
           (niwa-permission-reviewer 'user)
           (niwa-protected-paths (list (cons source 'inaccessible)))
           (draft "> keep this draft\nsecond line")
           result)
      (unwind-protect
          (progn
            (make-directory root)
            (make-directory cache)
            (with-temp-file source (insert "protected input contents"))
            (with-temp-file script
              (insert "#!/bin/sh\nset -eu\n"
                      (format "n=$(cat %s 2>/dev/null || printf 0)\n"
                              (shell-quote-argument counter))
                      "n=$((n + 1))\n"
                      (format "printf '%%s' \"$n\" > %s\n"
                              (shell-quote-argument counter))
                      (format "mkdir -p %s/run-\"$n\"\n" (shell-quote-argument cache))
                      (format "cat %s > %s/run-\"$n\"/value\n"
                              (shell-quote-argument source) (shell-quote-argument cache))
                      "printf 'profile execution complete'\n"))
            (set-file-modes script #o700)
            (niwa-tool-fs--register)
            (niwa-tool-exec--register)
            (setf (niwa-session-permission-mode session) 'edits
                  (niwa-session-sandbox-mode session) 'required)
            (with-current-buffer data
              (org-mode)
              (setq-local niwa--session session)
              (setq-local temporary-file-directory root))
            (niwa-view--setup view data)
            (with-current-buffer view
              (niwa-view-test--insert-composer-draft draft))
            (cl-letf (((symbol-function 'niwa--prompt-block-face) (lambda () 'ask)))
              (cl-labels
                  ((press (key)
                     (with-current-buffer view
                       (let* ((entry (car (niwa-session-permission-queue session)))
                              (id (niwa-queue--entry-metadata-get entry :interaction-id))
                              (overlay (gethash id niwa-view--interaction-overlays))
                              (last-command-event (aref key 0)))
                         (goto-char (overlay-start overlay))
                         (let ((action (lookup-key (overlay-get overlay 'keymap) key)))
                           (should (commandp action))
                           (call-interactively action)))))
                   (run (tool input)
                     (setq result nil)
                     (with-current-buffer data
                       (niwa-pipeline-run-tool
                        (niwa-tool-get tool)
                        (lambda (value) (should-not result) (setq result value)) input)))
                   (await-result ()
                     (let ((deadline (+ (float-time) 20)))
                       (while (and (not result) (< (float-time) deadline))
                         (accept-process-output nil 0.01)))
                     (should result)))
                ;; A prior native approval is available to the first execution.
                (run "Read" (list :file_path source))
                (should-not result)
                (press "s")
                (should (string-match-p "protected input contents" result))
                (run tool-name
                     (append args
                             (list :sandbox_permissions "with_additional_permissions"
                                   :additional_permissions
                                   (list :network t :file_system
                                         (list :read (vector source) :write (vector cache)))
                                   :justification "Read input and write validation cache")))
                (should-not result)
                (should (= 1 (length (niwa-session-permission-queue session))))
                (when (equal tool-name "Eval")
                  (let ((event (niwa-permission-queue--log-props
                                (car (niwa-session-permission-queue session)))))
                    (should (equal "fresh" (plist-get event :eval-mode)))
                    (should (eq 'edits (plist-get event :permission-mode-effective)))))
                (let ((choices (list (format "Write %s (exact)" cache)
                                     (format "Write %s (recursive)" cache))))
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (_prompt collection &rest _)
                               (let ((choice (pop choices)))
                                 (should (assoc choice collection))
                                 choice))))
                    (press "g"))
                  (should-not choices))
                (when narrow-p
                  ;; Omit the already granted read and network from remembering.
                  (press "n")
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (prompt collection &rest _)
                               (let ((choice (if (equal prompt "Remember capability: ")
                                                 (format "Read %s (exact)" source)
                                               "Do not remember")))
                                 (should (assoc choice collection))
                                 choice))))
                    (press "p")))
                (press "s")
                (await-result)
                (should (string-match-p "sandbox: bubblewrap" result))
                (should (string-search success-output result))
                (with-temp-buffer
                  (insert-file-contents (file-name-concat cache "run-1" "value"))
                  (should (equal "protected input contents" (buffer-string))))
                ;; Execution approval must not create independent cache access.
                (should-not
                 (niwa-permission-rules-resource-granted-p
                  cache 'write (niwa-session-resource-grants session) t))
                ;; Revoking the native input grant cannot revoke command access.
                (setf (niwa-session-resource-grants session) nil)
                (setf (niwa-session-permission-mode session) 'edits)
                (with-current-buffer data
                  (let* ((request
                          (niwa-tool-exec-permission-effective-sandbox-request
                           args tool-name operation (and (equal tool-name "Eval") 'fresh)))
                         (profile (plist-get request :additional-permissions)))
                    (should (eq (not narrow-p) (plist-get profile :network)))))
                (run tool-name args)
                (await-result)
                (should-not (niwa-session-permission-queue session))
                (should (string-match-p "sandbox: bubblewrap" result))
                (if narrow-p
                    (should-not (string-search success-output result))
                  (should (string-search success-output result))
                  (with-temp-buffer
                    (insert-file-contents (file-name-concat cache "run-2" "value"))
                    (should (equal "protected input contents" (buffer-string)))))
                (with-temp-buffer
                  (insert-file-contents counter)
                  (should (equal "2" (buffer-string))))
                ;; Ask still requires explicit outside-read authority; a command
                ;; profile does not supply it to native tools.
                (setf (niwa-session-permission-mode session) 'ask)
                (run "Read" (list :file_path cache))
                (should-not result)
                (should (= 1 (length (niwa-session-permission-queue session))))
                (press "d")
                (should result)
                ;; The same workload under a different command/expression cannot
                ;; use the original command's mount, even with operation approval.
                (setf (niwa-session-permission-mode session) 'edits)
                (run tool-name
                     (if (equal tool-name "Bash")
                         (list :command (concat "sh " (shell-quote-argument script))
                               :description "Probe Bash authority profile")
                       (list :expression (concat "(progn " expression ")") :mode "fresh")))
                (await-result)
                (should (string-match-p "sandbox: bubblewrap" result))
                (should-not (string-search success-output result))
                (should-not (file-exists-p (file-name-concat cache "run-3" "value")))
                (with-current-buffer view
                  (should (equal draft (niwa-view-composer-draft-text nil t)))))))
        (niwa-permission-queue-abort-all session)
        (niwa-execution-teardown-session session)
        (when (buffer-live-p view) (kill-buffer view))
        (when (buffer-live-p data) (kill-buffer data))
        (delete-directory parent t)
        (niwa-workspace-clear-registry))))
  (tool-name narrow-p)
  :doc "Bash remembers already granted input alongside selected cache tree"
  "Bash" nil
  :doc "Bash capability narrowing affects later calls, not the approved invocation"
  "Bash" t
  :doc "fresh Eval remembers already granted input alongside selected cache tree"
  "Eval" nil
  :doc "fresh Eval capability narrowing affects later calls, not the approved invocation"
  "Eval" t)

(niwa-deftest niwa-permission-queue--queued-execution-authority
  (:quiet t :doc "tree approval clears only queued executions with complete authority")
  (let ((niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (plist-get availability :reason))))
    (let* ((parent (make-temp-file "niwa-pq-mixed-exec-" t))
           (root (file-name-concat parent "workspace"))
           (tree (file-name-concat parent "design"))
           (first (file-name-concat tree "first"))
           (covered (file-name-concat tree "covered"))
           (unrelated (file-name-concat parent "unrelated"))
           (script (file-name-concat root "validate"))
           (data (generate-new-buffer " *test-pq-mixed-data*"))
           (view (generate-new-buffer " *test-pq-mixed-view*"))
           (workspace (niwa-workspace-get-or-create 'project root root "mixed"))
           (session (niwa-session-create "main" workspace))
           (niwa-permission-rules nil)
           (niwa-permission-reviewer 'user)
           results)
      (unwind-protect
          (progn
            (make-directory root)
            (make-directory tree)
            (dolist (path (list first covered unrelated))
              (with-temp-file path (insert "resource contents")))
            (with-temp-file script
              (insert "#!/bin/sh\nset -eu\n"
                      "for n in 1 2 3 4; do\n"
                      (format " if mkdir %s/run-\"$n\" 2>/dev/null; then\n"
                              (shell-quote-argument root))
                      "  printf 'executed once'; exit 0\n fi\ndone\nexit 42\n"))
            (set-file-modes script #o700)
            (niwa-tool-exec--register)
            (setf (niwa-session-permission-mode session) 'edits
                  (niwa-session-sandbox-mode session) 'required)
            (with-current-buffer data
              (org-mode)
              (setq-local niwa--session session)
              (setq-local temporary-file-directory root))
            (niwa-view--setup view data)
            (cl-letf (((symbol-function 'niwa--prompt-block-face) (lambda () 'ask)))
              (with-current-buffer data
                (dolist (request (list (list 'first first nil)
                                      (list 'network first t)
                                      (list 'unrelated unrelated nil)
                                      (list 'covered covered nil)))
                  (let ((id (car request)))
                    (niwa-pipeline-run-tool
                     (niwa-tool-get "Bash")
                     (lambda (result) (push (cons id result) results))
                     (list :command script
                           :description "Probe Bash authority profile"
                           :sandbox_permissions "with_additional_permissions"
                           :additional_permissions
                           (append (when (nth 2 request) '(:network t))
                                   (list :file_system
                                         (list :read (vector (nth 1 request)))))
                           :justification "Run validation with its requested resources")))))
              (should (= 4 (length (niwa-session-permission-queue session))))
              (should-not results)
              (with-current-buffer view
                (let* ((entry (car (niwa-session-permission-queue session)))
                       (id (niwa-queue--entry-metadata-get entry :interaction-id))
                       (overlay (gethash id niwa-view--interaction-overlays))
                       (choice (format "Read %s (recursive)" tree)))
                  (goto-char (overlay-start overlay))
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (_prompt collection &rest _)
                               (should (assoc choice collection)) choice)))
                    (call-interactively (lookup-key (overlay-get overlay 'keymap) "g")))
                  (setq overlay (gethash id niwa-view--interaction-overlays))
                  (goto-char (overlay-start overlay))
                  (call-interactively (lookup-key (overlay-get overlay 'keymap) "s"))))
              (let ((queue (niwa-session-permission-queue session)))
                (should (= 2 (length queue)))
                (should (plist-get (plist-get (car queue) :requested-additional-permissions)
                                   :network))
                (should (equal unrelated
                               (plist-get
                                (car (plist-get (plist-get (cadr queue)
                                                          :requested-additional-permissions)
                                                :file-system)) :path))))
              (let ((deadline (+ (float-time) 20)))
                (while (and (< (length results) 2) (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should (= 2 (length results)))
              (dolist (id '(first covered))
                (should (string-match-p "sandbox: bubblewrap" (cdr (assq id results))))
                (should (string-match-p "executed once" (cdr (assq id results)))))
              (should (file-directory-p (file-name-concat root "run-1")))
              (should (file-directory-p (file-name-concat root "run-2")))
              (should-not (file-exists-p (file-name-concat root "run-3")))))
        (niwa-permission-queue-abort-all session)
        (niwa-execution-teardown-session session)
        (when (buffer-live-p view) (kill-buffer view))
        (when (buffer-live-p data) (kill-buffer data))
        (delete-directory parent t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-permission-queue--independent-profile
  (:quiet t :doc "independent tree selection survives extent changes and mixed-scope approval")
  (let* ((root (make-temp-file "niwa-pq-independent-" t))
         (cache (file-name-concat root "cache"))
         (input (file-name-concat root "input"))
         (data (generate-new-buffer " *test-pq-independent-data*"))
         (view (generate-new-buffer " *test-pq-independent-view*"))
         (workspace (niwa-workspace-get-or-create 'project root root "independent"))
         (session (niwa-session-create "main" workspace))
         (niwa-permission-rules nil)
         (niwa-permission-reviewer 'user)
         result)
    (unwind-protect
        (progn
          (make-directory cache)
          (with-temp-file input (insert "input"))
          ;; Unconfined Bash keeps exact defaults regardless of earlier probes.
          (setf (niwa-session-sandbox-mode session) 'off)
          (with-current-buffer data
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup view data)
          (cl-letf (((symbol-function 'niwa--prompt-block-face) (lambda () 'ask)))
            (with-current-buffer data
              (niwa-tool-exec-permission-check-bash-async
               nil
               (list :command "make report"
                     :sandbox_permissions "with_additional_permissions"
                     :additional_permissions
                     (list :file_system (list :read (vector input) :write (vector cache)))
                     :justification "Read input and write cache")
               (lambda (value) (setq result value))))
            (should-not result)
            (dolist (step (list (list "p" (format "Write %s (exact)" cache)
                                     "Independent path access")
                               (list "g" (format "Write %s (exact)" cache)
                                     (format "Write %s (recursive)" cache))
                               (list "s")))
              (with-current-buffer view
                (let* ((entry (car (niwa-session-permission-queue session)))
                       (id (niwa-queue--entry-metadata-get entry :interaction-id))
                       (ov (gethash id niwa-view--interaction-overlays))
                       (choices (cdr step))
                       (last-command-event (aref (car step) 0)))
                  (goto-char (overlay-start ov))
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (_prompt collection &rest _)
                               (let ((choice (pop choices)))
                                 (should (assoc choice collection))
                                 choice))))
                    (call-interactively (lookup-key (overlay-get ov 'keymap) (car step)))))))
            (should (eq 'allow result))
            (should-not (niwa-session-permission-queue session))
            (should (equal (list (list :path cache :access 'write :recursive t))
                           (niwa-session-resource-grants session)))
            (should
             (equal (list (list :path input :access 'read))
                    (plist-get
                     (niwa-tool-exec-permission--remembered-additional-profile
                      "Bash" "make report" `(:session ,session :workspace ,workspace))
                     :file-system)))))
      (niwa-permission-queue-abort-all session)
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-permission-queue-profile)
;;; test-niwa-permission-queue-profile.el ends here
