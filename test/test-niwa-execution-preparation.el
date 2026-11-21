;;; test-niwa-execution-preparation.el --- Owned async discovery -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real discovery children through the execution facade, including
;; cancellation and authority changes while the directory scan is pending.

;;; Code:

(require 'helpers (file-name-concat (file-name-directory
                                    (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat (file-name-directory
                           (or load-file-name buffer-file-name))
                          "niwa-execution-test-helpers"))
(require 'niwa-sandbox)
(require 'niwa-permission-rules)

(niwa-deftest niwa-execution--prepare
  (:doc "discovery is owned, cancellable, fresh and refuses changed authority")
  (progn
   (skip-unless (and (eq system-type 'gnu/linux) (executable-find "find")
                     (plist-get (niwa-sandbox-probe) :available)))
  (dolist (managed '(nil t))
    (dolist (action '(complete cancel request-cancel request-cancelled owner-stop mode rule protected buffer-death root-replaced grants additional-roots))
      (let* ((root (make-temp-file "niwa-discovery-" t))
             (session (test-niwa-execution--session root))
             (buffer (generate-new-buffer " *discovery-owner*"))
             (bin (file-name-concat root "bin"))
             (gate (file-name-concat root "continue"))
             (ran (file-name-concat root "ran"))
             (find (executable-find "find"))
             (niwa-protected-paths '(("**/.git/**" . read-only)))
             (niwa-sandbox--probe-cache (copy-tree niwa-sandbox--probe-cache))
             (exec-path (cons bin exec-path))
             (process-environment (copy-sequence process-environment))
             (calls 0) result cancel)
        (unwind-protect
            (progn
              (make-directory bin)
              (make-directory (file-name-concat root "nested \u00fc\nname" ".git") t)
              (with-temp-file (file-name-concat bin "find")
                (insert "#!/bin/sh\ncount=0\nwhile [ ! -e " (shell-quote-argument gate)
                        " ]; do\n count=$((count+1)); [ $count -lt 500 ] || exit 1\n sleep .01\ndone\nexec "
                        (shell-quote-argument find) " \"$@\"\n"))
              (set-file-modes (file-name-concat bin "find") #o700)
              (setenv "PATH" (concat bin path-separator (getenv "PATH")))
              (setf (niwa-session-sandbox-mode session) 'required)
              (with-current-buffer buffer
                (setq-local niwa--session session)
                (setq-local niwa--current-request
                            (niwa-request--create
                             :session session :cancelled-p (eq action 'request-cancelled)))
                (if managed
                    (niwa-execution-start-bash
                     (lambda (value) (cl-incf calls) (setq result value))
                     :session session :data-buffer buffer :owner "main"
                     :request niwa--current-request
                     :command (list "sh" "-c" "touch ran; printf done")
                     :workdir root :writable-roots (list root)
                     :artifact-directory root :yield-time-ms nil)
                  (setq cancel
                        (niwa-execution-start-one-shot
                         (lambda (value) (cl-incf calls) (setq result value))
                         :session session :owner "main" :name "discovery-test"
                         :command (list "sh" "-c" "touch ran; printf done")
                         :workdir root :writable-roots (list root)))))
              ;; Managed admission rejects an already-cancelled request before
              ;; discovery.  Otherwise the gated scan has returned to Lisp.
              (if (and managed (eq action 'request-cancelled))
                  (progn
                    (should result)
                    (should (eq 'aborted (plist-get (plist-get result :facts) :termination))))
                (should-not result))
              (should-not (file-exists-p ran))
              (pcase action
                ('request-cancel
                 (setf (niwa-request-cancelled-p
                        (buffer-local-value 'niwa--current-request buffer)) t))
                ('cancel
                 (if managed
                     (niwa-execution-stop-user
                      session (plist-get (car (niwa-execution-list-user session)) :execution-id))
                   (funcall cancel)))
                ('owner-stop (niwa-execution-stop-owner session "main"))
                ('additional-roots
                 (with-current-buffer buffer
                   (setq-local niwa-workspace-additional-roots
                               (list (cons root (list temporary-file-directory))))))
                ('buffer-death (kill-buffer buffer))
                ('root-replaced
                 (rename-file root (concat root ".moved"))
                 (make-directory root))
                ('grants (setf (niwa-session-resource-grants session)
                               (list (list :path root :access 'write :recursive t))))
                ('mode (setf (niwa-session-sandbox-mode session) 'off))
                ('rule (setf (niwa-session-permission-rules session)
                             '(("Bash" :action deny))))
                ('protected (setq niwa-protected-paths '(("**/.git/**" . inaccessible)))))
              (with-temp-file gate (insert "continue"))
              (unless (and (eq action 'owner-stop) (not managed))
                (test-niwa-execution--wait (lambda () result))
                (should (= 1 calls)))
              (ert-info ((format "managed: %S; action: %S" managed action))
                (if (eq action 'complete)
                    (progn
                      (should-not (plist-get result :error))
                      (should (file-exists-p ran))
                      (should (= 0 (plist-get (or (plist-get result :facts) result) :exit-code))))
                  (should-not (file-exists-p ran))))
              (niwa-execution-teardown-session session)
              (should (= 0 (hash-table-count
                            (niwa-execution--state-records
                             (niwa-session-execution-state session))))))
          (niwa-execution-teardown-session session)
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (delete-directory root t)
          (when (file-directory-p (concat root ".moved"))
            (delete-directory (concat root ".moved") t))))))))

(provide 'test-niwa-execution-preparation)
;;; test-niwa-execution-preparation.el ends here
