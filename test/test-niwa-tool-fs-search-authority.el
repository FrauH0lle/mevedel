;;; test-niwa-tool-fs-search-authority.el --- Virtual search authority tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise virtual-document helpers through the registered tool pipeline.

;;; Code:

(require 'niwa-pipeline)
(require 'niwa-execution)
(require 'niwa-sandbox)
(require 'niwa-tool-fs-search)
(require 'niwa-permission-queue)
(require 'niwa-journal-store)
(require 'niwa-shared-editing)
(require 'niwa-tool-editing)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-tool-fs-search-virtual-authority (:quiet t)
  ,test
  (test)
  :doc "registered virtual Grep preserves session policy independently of its local target"
  (let* ((root (make-temp-file "niwa-search-authority-" t))
         (workspace (niwa-workspace--create :root root :id root :type 'project))
         (preference (if (plist-get (niwa-sandbox-probe) :available)
                         'required 'best-effort))
         (session (niwa-session--create
                   :workspace workspace :working-directory root
                   :permission-mode 'full-auto :sandbox-mode preference))
         (niwa-permission-mode 'ask)
         (niwa-sandbox-mode 'off)
         (niwa-journal-max-age-days nil)
         (niwa-shared-editing--runtimes (make-hash-table :test #'equal))
         (niwa-artifact-lease--held (make-hash-table :test #'equal))
         (start-helper (symbol-function 'niwa-execution-start-helper))
         (prepare (symbol-function 'niwa-sandbox-prepare-start))
         shared)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local niwa--session session)
          (setq-local temporary-file-directory root)
          (niwa-session-set-root-buffer session (current-buffer))
          (insert "DECISIVE unsaved root evidence\n")
          (niwa-tool-fs--register)
          (niwa-journal-store-publish-digest
           root (list :capture-id (make-string 64 ?a) :session "source-session"
                      :session-name "Source" :workspace (make-string 64 ?b)
                      :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
                      :turns '(1) :turn-ids (list (make-string 64 ?d))
                      :created "2026-10-24T12:00:00Z" :model "provider:model")
           "## Done\n- DECISIVE published evidence (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")
          (niwa-shared-editing-create
           workspace "document" "DECISIVE shared evidence" "Host"
           (lambda (value) (setq shared value)))
          (niwa-test--await 10 "shared item creation" shared)
          (should-not (plist-get shared :error))
          (dolist (modes '((ask full-auto best-effort) (full-auto ask off) (ask ask off)))
            (setq niwa-permission-mode (car modes)
                  niwa-sandbox-mode (caddr modes))
            (setf (niwa-session-permission-mode session) (cadr modes))
            (dolist (address (list "history://root" "memory://journal/"
                                   (concat "shared://" (plist-get shared :id))))
              (ert-info ((format "global %s, session %s, %s" (car modes) (cadr modes) address))
                (let (result launches confinement)
                  (cl-letf (((symbol-function 'niwa-execution-start-helper)
                             (lambda (callback name command reads writes &rest keys)
                               ;; Observe, but do not replace, real helper execution.
                               (push (list :session (plist-get keys :session)
                                           :policy-session niwa--session
                                           :reads reads) launches)
                               (apply start-helper callback name command reads writes keys)))
                            ((symbol-function 'niwa-sandbox-prepare-start)
                             (lambda (&rest arguments)
                               (push (nth 7 arguments) confinement)
                               (apply prepare arguments))))
                    (niwa-pipeline-run-tool
                     (niwa-tool-get "Grep") (lambda (value) (setq result value))
                     (list :path address :pattern "DECISIVE" :output_mode "content"))
                    (niwa-test--await 10 "registered Grep settlement" result))
                  (should (stringp result))
                  (should-not (string-prefix-p "Error:" result))
                  (should (string-search "DECISIVE" result))
                  (should (string-search (string-remove-suffix "/" address) result))
                  (should (= 1 (length launches)))
                  (should (equal (list (if (eq (cadr modes) 'full-auto) 'off preference))
                                 confinement))
                  (should-not (plist-get (car launches) :session))
                  (should (eq session (plist-get (car launches) :policy-session)))
                  (should (cl-every (lambda (path) (not (file-remote-p path)))
                                    (plist-get (car launches) :reads)))))))
          (should-not (niwa-session-permission-queue session)))
      (niwa-permission-queue-abort-all session)
      (niwa-execution-teardown-all)
      (niwa-shared-editing-stop)
      (maphash (lambda (_directory held)
                 (when (timerp (plist-get held :timer))
                   (cancel-timer (plist-get held :timer))))
               niwa-artifact-lease--held)
      (delete-directory root t)))

  :doc "registered history Grep still refuses permission or sandbox changes during discovery"
  (progn
    (skip-unless (and (eq system-type 'gnu/linux) (executable-find "find")
                      (plist-get (niwa-sandbox-probe) :available)))
    (dolist (change '(permission sandbox))
      (let* ((root (make-temp-file "niwa-search-mode-change-" t))
             (workspace (niwa-workspace--create :root root :id root :type 'project))
             (session (niwa-session--create
                       :workspace workspace :working-directory root
                       :permission-mode 'ask :sandbox-mode 'required))
             (niwa-permission-mode 'full-auto)
             (niwa-sandbox-mode 'off)
             (niwa-protected-paths '(("**/.git/**" . read-only)))
             (bin (file-name-concat root "bin"))
             (gate (file-name-concat root "continue"))
             (entered (file-name-concat root "entered"))
             (ran (file-name-concat root "rg-ran"))
             (find (executable-find "find"))
             (rg (executable-find "rg"))
             (exec-path (cons bin exec-path))
             (process-environment (copy-sequence process-environment))
             (start-helper (symbol-function 'niwa-execution-start-helper))
             result failure)
        (unwind-protect
            (with-temp-buffer
              (org-mode)
              (setq-local niwa--session session)
              (setq-local temporary-file-directory root)
              (niwa-session-set-root-buffer session (current-buffer))
              (insert "DECISIVE unsaved root evidence\n")
              (make-directory bin)
              (with-temp-file (file-name-concat bin "find")
                (insert "#!/bin/sh\ntouch " (shell-quote-argument entered)
                        "\ncount=0\nwhile [ ! -e " (shell-quote-argument gate)
                        " ]; do\n count=$((count+1)); [ $count -lt 500 ] || exit 1\n sleep .01\ndone\nexec "
                        (shell-quote-argument find) " \"$@\"\n"))
              (with-temp-file (file-name-concat bin "rg")
                (insert "#!/bin/sh\ntouch " (shell-quote-argument ran)
                        "\nexec " (shell-quote-argument rg) " \"$@\"\n"))
              (set-file-modes (file-name-concat bin "find") #o700)
              (set-file-modes (file-name-concat bin "rg") #o700)
              (setenv "PATH" (concat bin path-separator (getenv "PATH")))
              (niwa-tool-fs--register)
              (cl-letf (((symbol-function 'niwa-execution-start-helper)
                         (lambda (callback &rest arguments)
                           (apply start-helper
                                  (lambda (value)
                                    (setq failure (plist-get value :error))
                                    (funcall callback value))
                                  arguments))))
                (niwa-pipeline-run-tool
                 (niwa-tool-get "Grep") (lambda (value) (setq result value))
                 '(:path "history://root" :pattern "DECISIVE" :output_mode "content"))
                (niwa-test--await 5 "discovery entered" (file-exists-p entered))
                (should-not result)
                (should-not (file-exists-p ran))
                (pcase change
                  ('permission (setf (niwa-session-permission-mode session) 'full-auto))
                  ('sandbox (setf (niwa-session-sandbox-mode session) 'off)))
                (with-temp-file gate (insert "continue"))
                (niwa-test--await 5 "changed authority settlement" result))
              (should (equal '(error "Execution authority changed during preparation; submit the operation again")
                             failure))
              (should (string-prefix-p "Error:" result))
              (should-not (file-exists-p ran)))
          (niwa-permission-queue-abort-all session)
          (niwa-execution-teardown-all)
          (delete-directory root t))))))

(provide 'test-niwa-tool-fs-search-authority)
;;; test-niwa-tool-fs-search-authority.el ends here
