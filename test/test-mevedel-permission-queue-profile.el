;;; test-mevedel-permission-queue-profile.el -- Complete execution profiles -*- lexical-binding: t -*-

;;; Commentary:

;; Real prompt selection, profile reuse, and confined filesystem effects.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution)
(require 'mevedel-permission-prompt)
(require 'mevedel-permission-queue)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-fs)
(require 'mevedel-tools)
(require 'mevedel-view)

(mevedel-deftest mevedel-permission-queue--complete-profile
  (:quiet t)
  (let ((mevedel-sandbox--probe-cache nil))
    (let ((availability (mevedel-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (plist-get availability :reason))))
    (let* ((tool-name ,tool-name)
           (narrow-p ,narrow-p)
           (parent (make-temp-file "mevedel-pq-profile-" t))
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
           (args (if (equal tool-name "Bash")
                     (list :command script)
                   (list :expression expression :mode "batch")))
           (data (generate-new-buffer " *test-pq-profile-data*"))
           (view (generate-new-buffer " *test-pq-profile-view*"))
           (workspace (mevedel-workspace-get-or-create 'project root root "profile"))
           (session (mevedel-session-create "main" workspace))
           (mevedel-permission-rules nil)
           (mevedel-permission-reviewer 'user)
           (mevedel-protected-paths (list (cons source 'inaccessible)))
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
            (mevedel-tool-fs--register)
            (mevedel-tool-exec--register)
            (setf (mevedel-session-permission-mode session) 'edits
                  (mevedel-session-sandbox-mode session) 'required)
            (with-current-buffer data
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local temporary-file-directory root))
            (mevedel-view--setup view data)
            (with-current-buffer view
              (mevedel-view-test--insert-composer-draft draft))
            (cl-letf (((symbol-function 'mevedel--prompt-block-face) (lambda () 'ask)))
              (cl-labels
                  ((press (key)
                     (with-current-buffer view
                       (let* ((entry (car (mevedel-session-permission-queue session)))
                              (id (mevedel-queue--entry-metadata-get entry :interaction-id))
                              (overlay (gethash id mevedel-view--interaction-overlays))
                              (last-command-event (aref key 0)))
                         (goto-char (overlay-start overlay))
                         (let ((action (lookup-key (overlay-get overlay 'keymap) key)))
                           (should (commandp action))
                           (call-interactively action)))))
                   (run (tool input)
                     (setq result nil)
                     (with-current-buffer data
                       (mevedel-pipeline-run-tool
                        (mevedel-tool-get tool)
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
                (should (= 1 (length (mevedel-session-permission-queue session))))
                (when (equal tool-name "Eval")
                  (let ((event (mevedel-permission-queue--log-props
                                (car (mevedel-session-permission-queue session)))))
                    (should (equal "batch" (plist-get event :eval-mode)))
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
                (should (string-match-p "profile execution complete" result))
                (with-temp-buffer
                  (insert-file-contents (file-name-concat cache "run-1" "value"))
                  (should (equal "protected input contents" (buffer-string))))
                ;; Execution approval must not create independent cache access.
                (should-not
                 (mevedel-permission-rules-resource-granted-p
                  cache 'write (mevedel-session-resource-grants session) t))
                ;; Revoking the native input grant cannot revoke command access.
                (setf (mevedel-session-resource-grants session) nil)
                (setf (mevedel-session-permission-mode session) 'edits)
                (with-current-buffer data
                  (let* ((request
                          (mevedel-tool-exec-permission-effective-sandbox-request
                           args tool-name operation (and (equal tool-name "Eval") 'batch)))
                         (profile (plist-get request :additional-permissions)))
                    (should (eq (not narrow-p) (plist-get profile :network)))))
                (run tool-name args)
                (await-result)
                (should-not (mevedel-session-permission-queue session))
                (should (string-match-p "sandbox: bubblewrap" result))
                (if narrow-p
                    (should-not (string-match-p "profile execution complete" result))
                  (should (string-match-p "profile execution complete" result))
                  (with-temp-buffer
                    (insert-file-contents (file-name-concat cache "run-2" "value"))
                    (should (equal "protected input contents" (buffer-string)))))
                (with-temp-buffer
                  (insert-file-contents counter)
                  (should (equal "2" (buffer-string))))
                ;; Ask still requires explicit outside-read authority; a command
                ;; profile does not supply it to native tools.
                (setf (mevedel-session-permission-mode session) 'ask)
                (run "Read" (list :file_path cache))
                (should-not result)
                (should (= 1 (length (mevedel-session-permission-queue session))))
                (press "d")
                (should result)
                ;; The same workload under a different command/expression cannot
                ;; use the original command's mount, even with operation approval.
                (setf (mevedel-session-permission-mode session) 'edits)
                (run tool-name
                     (if (equal tool-name "Bash")
                         (list :command (concat "sh " (shell-quote-argument script)))
                       (list :expression (concat "(progn " expression ")") :mode "batch")))
                (await-result)
                (should (string-match-p "sandbox: bubblewrap" result))
                (should-not (string-match-p "profile execution complete" result))
                (should-not (file-exists-p (file-name-concat cache "run-3" "value")))
                (with-current-buffer view
                  (should (equal draft (mevedel-view--input-text)))))))
        (mevedel-permission-queue-abort-all session)
        (mevedel-execution-teardown-session session)
        (when (buffer-live-p view) (kill-buffer view))
        (when (buffer-live-p data) (kill-buffer data))
        (delete-directory parent t)
        (mevedel-workspace-clear-registry))))
  (tool-name narrow-p)
  :doc "Bash remembers already granted input alongside selected cache tree"
  "Bash" nil
  :doc "Bash capability narrowing affects later calls, not the approved invocation"
  "Bash" t
  :doc "batch Eval remembers already granted input alongside selected cache tree"
  "Eval" nil
  :doc "batch Eval capability narrowing affects later calls, not the approved invocation"
  "Eval" t)

(mevedel-deftest mevedel-permission-queue--queued-execution-authority
  (:quiet t :doc "tree approval clears only queued executions with complete authority")
  (let ((mevedel-sandbox--probe-cache nil))
    (let ((availability (mevedel-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (plist-get availability :reason))))
    (let* ((parent (make-temp-file "mevedel-pq-mixed-exec-" t))
           (root (file-name-concat parent "workspace"))
           (tree (file-name-concat parent "design"))
           (first (file-name-concat tree "first"))
           (covered (file-name-concat tree "covered"))
           (unrelated (file-name-concat parent "unrelated"))
           (script (file-name-concat root "validate"))
           (data (generate-new-buffer " *test-pq-mixed-data*"))
           (view (generate-new-buffer " *test-pq-mixed-view*"))
           (workspace (mevedel-workspace-get-or-create 'project root root "mixed"))
           (session (mevedel-session-create "main" workspace))
           (mevedel-permission-rules nil)
           (mevedel-permission-reviewer 'user)
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
            (mevedel-tool-exec--register)
            (setf (mevedel-session-permission-mode session) 'edits
                  (mevedel-session-sandbox-mode session) 'required)
            (with-current-buffer data
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local temporary-file-directory root))
            (mevedel-view--setup view data)
            (cl-letf (((symbol-function 'mevedel--prompt-block-face) (lambda () 'ask)))
              (with-current-buffer data
                (dolist (request (list (list 'first first nil)
                                      (list 'network first t)
                                      (list 'unrelated unrelated nil)
                                      (list 'covered covered nil)))
                  (let ((id (car request)))
                    (mevedel-pipeline-run-tool
                     (mevedel-tool-get "Bash")
                     (lambda (result) (push (cons id result) results))
                     (list :command script
                           :sandbox_permissions "with_additional_permissions"
                           :additional_permissions
                           (append (when (nth 2 request) '(:network t))
                                   (list :file_system
                                         (list :read (vector (nth 1 request)))))
                           :justification "Run validation with its requested resources")))))
              (should (= 4 (length (mevedel-session-permission-queue session))))
              (should-not results)
              (with-current-buffer view
                (let* ((entry (car (mevedel-session-permission-queue session)))
                       (id (mevedel-queue--entry-metadata-get entry :interaction-id))
                       (overlay (gethash id mevedel-view--interaction-overlays))
                       (choice (format "Read %s (recursive)" tree)))
                  (goto-char (overlay-start overlay))
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (_prompt collection &rest _)
                               (should (assoc choice collection)) choice)))
                    (call-interactively (lookup-key (overlay-get overlay 'keymap) "g")))
                  (setq overlay (gethash id mevedel-view--interaction-overlays))
                  (goto-char (overlay-start overlay))
                  (call-interactively (lookup-key (overlay-get overlay 'keymap) "s"))))
              (let ((queue (mevedel-session-permission-queue session)))
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
        (mevedel-permission-queue-abort-all session)
        (mevedel-execution-teardown-session session)
        (when (buffer-live-p view) (kill-buffer view))
        (when (buffer-live-p data) (kill-buffer data))
        (delete-directory parent t)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-permission-queue--independent-profile
  (:quiet t :doc "independent tree selection survives extent changes and mixed-scope approval")
  (let* ((root (make-temp-file "mevedel-pq-independent-" t))
         (cache (file-name-concat root "cache"))
         (input (file-name-concat root "input"))
         (data (generate-new-buffer " *test-pq-independent-data*"))
         (view (generate-new-buffer " *test-pq-independent-view*"))
         (workspace (mevedel-workspace-get-or-create 'project root root "independent"))
         (session (mevedel-session-create "main" workspace))
         (mevedel-permission-rules nil)
         (mevedel-permission-reviewer 'user)
         result)
    (unwind-protect
        (progn
          (make-directory cache)
          (with-temp-file input (insert "input"))
          (with-current-buffer data
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view data)
          (cl-letf (((symbol-function 'mevedel--prompt-block-face) (lambda () 'ask)))
            (with-current-buffer data
              (mevedel-tool-exec-permission-check-bash-async
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
                (let* ((entry (car (mevedel-session-permission-queue session)))
                       (id (mevedel-queue--entry-metadata-get entry :interaction-id))
                       (ov (gethash id mevedel-view--interaction-overlays))
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
            (should-not (mevedel-session-permission-queue session))
            (should (equal (list (list :path cache :access 'write :recursive t))
                           (mevedel-session-resource-grants session)))
            (should
             (equal (list (list :path input :access 'read))
                    (plist-get
                     (mevedel-tool-exec-permission--remembered-additional-profile
                      "Bash" "make report" `(:session ,session :workspace ,workspace))
                     :file-system)))))
      (mevedel-permission-queue-abort-all session)
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-permission-queue-profile)
;;; test-mevedel-permission-queue-profile.el ends here
