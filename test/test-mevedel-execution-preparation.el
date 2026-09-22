;;; test-mevedel-execution-preparation.el --- Owned async discovery -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real discovery children through the execution facade, including
;; cancellation and authority changes while the directory scan is pending.

;;; Code:

(require 'helpers (file-name-concat (file-name-directory
                                    (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat (file-name-directory
                           (or load-file-name buffer-file-name))
                          "mevedel-execution-test-helpers"))
(require 'mevedel-sandbox)
(require 'mevedel-permission-rules)

(mevedel-deftest mevedel-execution--prepare
  (:doc "discovery is owned, cancellable, fresh and refuses changed authority")
  (progn
   (skip-unless (and (eq system-type 'gnu/linux) (executable-find "find")
                     (plist-get (mevedel-sandbox-probe) :available)))
  (dolist (managed '(nil t))
    (dolist (action '(complete cancel owner-stop mode rule protected buffer-death root-replaced grants additional-roots))
      (let* ((root (make-temp-file "mevedel-discovery-" t))
             (session (test-mevedel-execution--session root))
             (buffer (generate-new-buffer " *discovery-owner*"))
             (bin (file-name-concat root "bin"))
             (gate (file-name-concat root "continue"))
             (ran (file-name-concat root "ran"))
             (find (executable-find "find"))
             (mevedel-protected-paths '(("**/.git/**" . read-only)))
             (mevedel-sandbox--probe-cache (copy-tree mevedel-sandbox--probe-cache))
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
              (setf (mevedel-session-sandbox-mode session) 'required)
              (with-current-buffer buffer
                (if managed
                    (mevedel-execution-start-bash
                     (lambda (value) (cl-incf calls) (setq result value))
                     :session session :data-buffer buffer :owner "main"
                     :command (list "sh" "-c" "touch ran; printf done")
                     :workdir root :writable-roots (list root)
                     :artifact-directory root :yield-time-ms nil)
                  (setq cancel
                        (mevedel-execution-start-one-shot
                         (lambda (value) (cl-incf calls) (setq result value))
                         :session session :owner "main" :name "discovery-test"
                         :command (list "sh" "-c" "touch ran; printf done")
                         :workdir root :writable-roots (list root)))))
              ;; The gated find is still running: launch has returned to Lisp.
              (should-not result)
              (should-not (file-exists-p ran))
              (pcase action
                ('cancel
                 (if managed
                     (mevedel-execution-stop-user
                      session (plist-get (car (mevedel-execution-list-user session)) :execution-id))
                   (funcall cancel)))
                ('owner-stop (mevedel-execution-stop-owner session "main"))
                ('additional-roots
                 (with-current-buffer buffer
                   (setq-local mevedel-workspace-additional-roots
                               (list (cons root (list temporary-file-directory))))))
                ('buffer-death (kill-buffer buffer))
                ('root-replaced
                 (rename-file root (concat root ".moved"))
                 (make-directory root))
                ('grants (setf (mevedel-session-resource-grants session)
                               (list (list :path root :access 'write :recursive t))))
                ('mode (setf (mevedel-session-sandbox-mode session) 'off))
                ('rule (setf (mevedel-session-permission-rules session)
                             '(("Bash" :action deny))))
                ('protected (setq mevedel-protected-paths '(("**/.git/**" . inaccessible)))))
              (with-temp-file gate (insert "continue"))
              (unless (and (eq action 'owner-stop) (not managed))
                (test-mevedel-execution--wait (lambda () result))
                (should (= 1 calls)))
              (if (eq action 'complete)
                  (progn
                    (should-not (plist-get result :error))
                    (should (file-exists-p ran))
                    (should (= 0 (plist-get (or (plist-get result :facts) result) :exit-code))))
                (should-not (file-exists-p ran)))
              (mevedel-execution-teardown-session session)
              (should (= 0 (hash-table-count
                            (mevedel-execution--state-records
                             (mevedel-session-execution-state session))))))
          (mevedel-execution-teardown-session session)
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (delete-directory root t)
          (when (file-directory-p (concat root ".moved"))
            (delete-directory (concat root ".moved") t))))))))

(provide 'test-mevedel-execution-preparation)
;;; test-mevedel-execution-preparation.el ends here
