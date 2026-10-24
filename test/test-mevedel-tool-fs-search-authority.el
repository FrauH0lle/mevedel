;;; test-mevedel-tool-fs-search-authority.el --- Virtual search authority tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise virtual-document helpers through the registered tool pipeline.

;;; Code:

(require 'mevedel-pipeline)
(require 'mevedel-execution)
(require 'mevedel-sandbox)
(require 'mevedel-tool-fs-search)
(require 'mevedel-permission-queue)
(require 'mevedel-journal-store)
(require 'mevedel-shared-editing)
(require 'mevedel-tool-editing)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-fs-search-virtual-authority (:quiet t)
  ,test
  (test)
  :doc "registered virtual Grep preserves session policy independently of its local target"
  (let* ((root (make-temp-file "mevedel-search-authority-" t))
         (workspace (mevedel-workspace--create :root root :id root :type 'project))
         (preference (if (plist-get (mevedel-sandbox-probe) :available)
                         'required 'best-effort))
         (session (mevedel-session--create
                   :workspace workspace :working-directory root
                   :permission-mode 'full-auto :sandbox-mode preference))
         (mevedel-permission-mode 'ask)
         (mevedel-sandbox-mode 'off)
         (mevedel-journal-max-age-days nil)
         (mevedel-shared-editing--runtimes (make-hash-table :test #'equal))
         (mevedel-artifact-lease--held (make-hash-table :test #'equal))
         (start-helper (symbol-function 'mevedel-execution-start-helper))
         (prepare (symbol-function 'mevedel-sandbox-prepare-start))
         shared)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session session)
          (setq-local temporary-file-directory root)
          (mevedel-session-set-root-buffer session (current-buffer))
          (insert "DECISIVE unsaved root evidence\n")
          (mevedel-tool-fs--register)
          (mevedel-journal-store-publish-digest
           root (list :capture-id (make-string 64 ?a) :session "source-session"
                      :session-name "Source" :workspace (make-string 64 ?b)
                      :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
                      :turns '(1) :turn-ids (list (make-string 64 ?d))
                      :created "2026-10-24T12:00:00Z" :model "provider:model")
           "## Done\n- DECISIVE published evidence (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")
          (mevedel-shared-editing-create
           workspace "document" "DECISIVE shared evidence" "Host"
           (lambda (value) (setq shared value)))
          (mevedel-test--await 10 "shared item creation" shared)
          (should-not (plist-get shared :error))
          (dolist (modes '((ask full-auto best-effort) (full-auto ask off) (ask ask off)))
            (setq mevedel-permission-mode (car modes)
                  mevedel-sandbox-mode (caddr modes))
            (setf (mevedel-session-permission-mode session) (cadr modes))
            (dolist (address (list "history://root" "memory://journal/"
                                   (concat "shared://" (plist-get shared :id))))
              (ert-info ((format "global %s, session %s, %s" (car modes) (cadr modes) address))
                (let (result launches confinement)
                  (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                             (lambda (callback name command reads writes &rest keys)
                               ;; Observe, but do not replace, real helper execution.
                               (push (list :session (plist-get keys :session)
                                           :policy-session mevedel--session
                                           :reads reads) launches)
                               (apply start-helper callback name command reads writes keys)))
                            ((symbol-function 'mevedel-sandbox-prepare-start)
                             (lambda (&rest arguments)
                               (push (nth 7 arguments) confinement)
                               (apply prepare arguments))))
                    (mevedel-pipeline-run-tool
                     (mevedel-tool-get "Grep") (lambda (value) (setq result value))
                     (list :path address :pattern "DECISIVE" :output_mode "content"))
                    (mevedel-test--await 10 "registered Grep settlement" result))
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
          (should-not (mevedel-session-permission-queue session)))
      (mevedel-permission-queue-abort-all session)
      (mevedel-execution-teardown-all)
      (mevedel-shared-editing-stop)
      (maphash (lambda (_directory held)
                 (when (timerp (plist-get held :timer))
                   (cancel-timer (plist-get held :timer))))
               mevedel-artifact-lease--held)
      (delete-directory root t)))

  :doc "registered history Grep still refuses permission or sandbox changes during discovery"
  (progn
    (skip-unless (and (eq system-type 'gnu/linux) (executable-find "find")
                      (plist-get (mevedel-sandbox-probe) :available)))
    (dolist (change '(permission sandbox))
      (let* ((root (make-temp-file "mevedel-search-mode-change-" t))
             (workspace (mevedel-workspace--create :root root :id root :type 'project))
             (session (mevedel-session--create
                       :workspace workspace :working-directory root
                       :permission-mode 'ask :sandbox-mode 'required))
             (mevedel-permission-mode 'full-auto)
             (mevedel-sandbox-mode 'off)
             (mevedel-protected-paths '(("**/.git/**" . read-only)))
             (bin (file-name-concat root "bin"))
             (gate (file-name-concat root "continue"))
             (entered (file-name-concat root "entered"))
             (ran (file-name-concat root "rg-ran"))
             (find (executable-find "find"))
             (rg (executable-find "rg"))
             (exec-path (cons bin exec-path))
             (process-environment (copy-sequence process-environment))
             (start-helper (symbol-function 'mevedel-execution-start-helper))
             result failure)
        (unwind-protect
            (with-temp-buffer
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local temporary-file-directory root)
              (mevedel-session-set-root-buffer session (current-buffer))
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
              (mevedel-tool-fs--register)
              (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                         (lambda (callback &rest arguments)
                           (apply start-helper
                                  (lambda (value)
                                    (setq failure (plist-get value :error))
                                    (funcall callback value))
                                  arguments))))
                (mevedel-pipeline-run-tool
                 (mevedel-tool-get "Grep") (lambda (value) (setq result value))
                 '(:path "history://root" :pattern "DECISIVE" :output_mode "content"))
                (mevedel-test--await 5 "discovery entered" (file-exists-p entered))
                (should-not result)
                (should-not (file-exists-p ran))
                (pcase change
                  ('permission (setf (mevedel-session-permission-mode session) 'full-auto))
                  ('sandbox (setf (mevedel-session-sandbox-mode session) 'off)))
                (with-temp-file gate (insert "continue"))
                (mevedel-test--await 5 "changed authority settlement" result))
              (should (equal '(error "Execution authority changed during preparation; submit the operation again")
                             failure))
              (should (string-prefix-p "Error:" result))
              (should-not (file-exists-p ran)))
          (mevedel-permission-queue-abort-all session)
          (mevedel-execution-teardown-all)
          (delete-directory root t))))))

(provide 'test-mevedel-tool-fs-search-authority)
;;; test-mevedel-tool-fs-search-authority.el ends here
