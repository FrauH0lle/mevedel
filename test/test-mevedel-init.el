;;; test-mevedel-init.el --- Tests for mevedel-init.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-init)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Init prompt

(mevedel-deftest mevedel-init--prompt
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-init-" t)))
           (work-dir (file-name-concat root-dir "packages" "api")))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "renders workspace paths and the user focus into the init prompt"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "initproj"))
         (session (mevedel-session-create "main" ws work-dir))
         (data-buffer (generate-new-buffer " *mev-init-data*")))
    (unwind-protect
        (progn
          (make-directory work-dir t)
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (setq-local default-directory work-dir))
          (let ((prompt (mevedel-init--prompt "prefer hooks" data-buffer)))
            (should (string-match-p (regexp-quote root-dir) prompt))
            (should (string-match-p (regexp-quote work-dir) prompt))
            (should (string-match-p (regexp-quote
                                     (file-name-concat root-dir "AGENTS.md"))
                                    prompt))
            (should (string-match-p (regexp-quote
                                     (file-name-concat root-dir
                                                       "AGENTS.local.md"))
                                    prompt))
            (should (string-match-p (regexp-quote
                                     (file-name-concat root-dir
                                                       ".agents" "skills"))
                                    prompt))
            (should (string-match-p (regexp-quote
                                     (file-name-concat root-dir
                                                       ".agents" "memory"))
                                    prompt))
            (should (string-match-p (regexp-quote
                                     (expand-file-name "~/.agents/skills"))
                                    prompt))
            (should (string-match-p (regexp-quote
                                     (expand-file-name "~/.agents/memory"))
                                    prompt))
            (should-not (string-match-p "CLAUDE\\.md" prompt))
            (should-not (string-match-p "\\.claude/skills" prompt))
            (should (string-match-p "User-provided focus: prefer hooks"
                                    prompt))))
      (kill-buffer data-buffer))))


;;
;;; Dispatch

(mevedel-deftest mevedel-init--dispatch
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-init-dispatch-" t))))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "view-backed dispatch sends the rendered prompt with compact slash display"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "initproj"))
         (session (mevedel-session-create "main" ws root-dir))
         (data-buffer (generate-new-buffer " *mev-init-dispatch-data*"))
         (view-buffer (generate-new-buffer " *mev-init-dispatch-view*"))
         sent-input
         sent-display
         history
         forked)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer view-buffer)
            (setq-local default-directory root-dir))
          (with-current-buffer view-buffer
            (setq-local mevedel--data-buffer data-buffer))
          (cl-letf (((symbol-function 'mevedel-view--forward-input)
                     (lambda (input display before-send &rest _rest)
                       (setq sent-input input)
                       (setq sent-display display)
                       (funcall before-send)))
                    ((symbol-function 'mevedel-view-history-add)
                     (lambda (text) (setq history text)))
                    ((symbol-function 'mevedel-view--fork-if-pending)
                     (lambda () (setq forked t))))
            (let ((result (with-current-buffer data-buffer
                            (mevedel-cmd--init "docs"))))
              (should (eq 'mevedel-view-sent result))))
          (should (equal "/init docs" sent-display))
          (should (equal "/init docs" history))
          (should forked)
          (should (string-match-p "User-provided focus: docs" sent-input)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer)))))

(mevedel-deftest mevedel-init-install-slash-command ()
  ,test
  (test)
  :doc "registers /init as a local slash command"
  (should (eq #'mevedel-cmd--init
              (alist-get "init" mevedel-slash-commands nil nil #'equal))))


(provide 'test-mevedel-init)
;;; test-mevedel-init.el ends here
