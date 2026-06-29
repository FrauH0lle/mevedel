;;; test-mevedel-workspace.el --- Tests for mevedel-workspace.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'mevedel-structs)
(require 'mevedel-workspace)

;; Defined in mevedel-chat.el; declared here so dynamic binding works
;; without pulling in the full chat module at test time.
(defvar mevedel-plans-directory)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Workspace detection helpers

(mevedel-deftest mevedel-workspace--file-workspace
  (:doc "`mevedel-workspace--file-workspace' returns (file . FILENAME) for file-visiting buffers")
  ,test
  (test)
  :doc "returns nil when buffer not visiting a file"
  (with-temp-buffer
    (should (null (mevedel-workspace--file-workspace))))

  :doc "returns (file . FILENAME) when buffer visits a file"
  (let ((tmp (make-temp-file "mevedel-ws-")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmp)
          (unwind-protect
              (let ((result (mevedel-workspace--file-workspace)))
                (should (eq 'file (car result)))
                (should (equal tmp (cdr result))))
            (kill-buffer (current-buffer))))
      (delete-file tmp))))

(mevedel-deftest mevedel-workspace--project-workspace
  (:doc "`mevedel-workspace--project-workspace' returns (project . ROOT) or nil"
   :vars* ((fake-project-dir (file-name-as-directory
                              (make-temp-file "mevedel-ws-proj-" t))))
   :after-each (delete-directory fake-project-dir t))
  ,test
  (test)
  :doc "returns nil when no project detected"
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _prompt _dir) nil)))
    (with-temp-buffer
      (setq default-directory fake-project-dir)
      (should (null (mevedel-workspace--project-workspace)))))

  :doc "returns (project . ROOT) when project.el finds one"
  (cl-letf* ((fake-project (list 'transient fake-project-dir))
             ((symbol-function 'project-current)
              (lambda (&optional _prompt _dir) fake-project))
             ((symbol-function 'project-root)
              (lambda (project) (nth 1 project))))
    (with-temp-buffer
      (setq default-directory fake-project-dir)
      (let ((result (mevedel-workspace--project-workspace)))
        (should (eq 'project (car result)))
        (should (equal fake-project-dir (cdr result)))))))


;;
;;; Workspace type accessors

(mevedel-deftest mevedel-workspace--project-root
  (:doc "`mevedel-workspace--project-root' validates that id is a real project root"
   :vars* ((dir (file-name-as-directory (make-temp-file "mevedel-ws-pr-" t))))
   :after-each (delete-directory dir t))
  ,test
  (test)
  :doc "returns id when dir exists and project-current accepts it"
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _prompt _dir) '(transient "ok"))))
    (should (equal dir (mevedel-workspace--project-root dir))))

  :doc "errors when id is not absolute"
  (should-error (mevedel-workspace--project-root "relative/path/")
                :type 'error)

  :doc "errors when directory does not exist"
  (should-error (mevedel-workspace--project-root "/nonexistent-mevedel-ws-test/")
                :type 'error)

  :doc "errors when project-current returns nil for the dir"
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _prompt _dir) nil)))
    (should-error (mevedel-workspace--project-root dir)
                  :type 'error)))

(mevedel-deftest mevedel-workspace--project-name
  (:doc "`mevedel-workspace--project-name' falls back to directory name when project.el is silent")
  ,test
  (test)
  :doc "uses project-name when project-current returns a project"
  (cl-letf* ((fake-project '(transient "/tmp/some-proj/"))
             ((symbol-function 'project-current)
              (lambda (&optional _prompt _dir) fake-project))
             ((symbol-function 'project-name)
              (lambda (_project) "some-proj")))
    (should (equal "some-proj" (mevedel-workspace--project-name "/tmp/some-proj/"))))

  :doc "falls back to last path segment when no project detected"
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _prompt _dir) nil)))
    (should (equal "some-proj"
                   (mevedel-workspace--project-name "/tmp/some-proj/")))))


;;
;;; Main workspace accessor

(mevedel-deftest mevedel-workspace
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "returns session's workspace when session is active"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/wsproj/" "/tmp/wsproj/" "wsproj"))
         (session (mevedel-session-create "main" ws)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (should (eq ws (mevedel-workspace)))))

  :doc "returns cached workspace when set and no session"
  (let ((ws (mevedel-workspace-get-or-create
             'project "/tmp/wsproj/" "/tmp/wsproj/" "wsproj")))
    (with-temp-buffer
      (setq-local mevedel--workspace ws)
      (should (eq ws (mevedel-workspace)))))

  :doc "auto-detects via workspace-functions when no session/cache"
  (let ((detected nil))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _prompt _dir) nil)))
      (with-temp-buffer
        (let ((mevedel-workspace-functions
               (list (lambda ()
                       (setq detected t)
                       (cons 'file "/tmp/some-file.el")))))
          (let ((ws (mevedel-workspace)))
            (should detected)
            (should (mevedel-workspace-p ws))
            (should (eq 'file (mevedel-workspace-type ws)))
            (should (equal "/tmp/some-file.el" (mevedel-workspace-id ws))))))))

  :doc "returns nil when no workspace function matches"
  (with-temp-buffer
    (let ((mevedel-workspace-functions (list (lambda () nil))))
      (should (null (mevedel-workspace))))))


;;
;;; Allowed roots

(mevedel-deftest mevedel--all-allowed-roots
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "includes default roots and plans directory"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/rootsproj/" "/tmp/rootsproj/" "rootsproj"))
         (mevedel-workspace-additional-roots nil)
         (mevedel-plans-directory "/tmp/plans/"))
    (with-temp-buffer
      (setq-local mevedel--workspace ws)
      (let ((roots (mevedel--all-allowed-roots)))
        (should (member "/tmp/rootsproj/" roots))
        (should (member "/tmp/rootsproj/.mevedel/memory/" roots))
        (should (member (file-name-as-directory
                         (expand-file-name temporary-file-directory))
                        roots))
        (should (member "/tmp/plans/" roots)))))

  :doc "includes additional roots configured for workspace"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/rootsproj/" "/tmp/rootsproj/" "rootsproj"))
         (mevedel-workspace-additional-roots
          '(("/tmp/rootsproj/" . ("/tmp/extra-a/" "/tmp/extra-b/"))))
         (mevedel-plans-directory "/tmp/plans/"))
    (with-temp-buffer
      (setq-local mevedel--workspace ws)
      (let ((roots (mevedel--all-allowed-roots)))
        (should (member "/tmp/extra-a/" roots))
        (should (member "/tmp/extra-b/" roots))))))

(mevedel-deftest mevedel-workspace--file-in-allowed-roots-p
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-ws-roots-" t)))
           (extra-dir (file-name-as-directory
                       (make-temp-file "mevedel-ws-extra-" t)))
           (outside-dir (file-name-as-directory
                         (make-temp-file "mevedel-ws-outside-" t))))
   :after-each
   (progn
     (mevedel-workspace-clear-registry)
     (delete-directory root-dir t)
     (delete-directory extra-dir t)
     (delete-directory outside-dir t)))
  ,test
  (test)
  :doc "returns matching root for file inside workspace"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "rootsproj"))
         (mevedel-workspace-additional-roots nil)
         (mevedel-plans-directory "/tmp/plans/")
         (file (file-name-concat root-dir "inside.txt")))
    (write-region "" nil file)
    (with-temp-buffer
      (setq-local mevedel--workspace ws)
      (should (equal root-dir
                     (mevedel-workspace--file-in-allowed-roots-p file)))))

  :doc "returns matching additional root when file lives there"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "rootsproj"))
         (mevedel-workspace-additional-roots
          (list (cons root-dir (list extra-dir))))
         (mevedel-plans-directory "/tmp/plans/")
         (file (file-name-concat extra-dir "extra.txt")))
    (write-region "" nil file)
    (with-temp-buffer
      (setq-local mevedel--workspace ws)
      (should (member
               (mevedel-workspace--file-in-allowed-roots-p file)
               (list extra-dir
                     (file-name-as-directory
                      (expand-file-name temporary-file-directory)))))))

  :doc "returns nil for file outside any allowed root"
  (let* ((parent-dir (file-name-as-directory
                      (make-temp-file "mevedel-ws-parent-" t)))
         (root (file-name-as-directory
                (file-name-concat parent-dir "root")))
         (outside (file-name-as-directory
                   (file-name-concat parent-dir "outside")))
         (ws (mevedel-workspace-get-or-create
              'project root root "rootsproj"))
         (mevedel-workspace-additional-roots nil)
         (mevedel-plans-directory "/var/tmp/mevedel-plans/")
         (temporary-file-directory (file-name-concat parent-dir "tmp/"))
         (file (file-name-concat outside "outside.txt")))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory outside)
          (make-directory temporary-file-directory)
          (write-region "" nil file)
          (with-temp-buffer
            (setq-local mevedel--workspace ws)
            (should (null (mevedel-workspace--file-in-allowed-roots-p file)))))
      (delete-directory parent-dir t))))


;;
;;; Generated state ignore

(mevedel-deftest mevedel-workspace-ensure-generated-state-ignored
  (:doc "`mevedel-workspace-ensure-generated-state-ignored' writes exact generated-state excludes")
  ,test
  (test)
  (unless (executable-find "git")
    (ert-skip "git is required"))
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-ws-ignore-" t)))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "ignore"))
         (exclude (file-name-concat root ".git" "info" "exclude")))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil "init" root)))
          (mevedel-workspace-ensure-generated-state-ignored workspace)
          (mevedel-workspace-ensure-generated-state-ignored workspace)
          (with-temp-buffer
            (insert-file-contents exclude)
            (let ((content (buffer-string)))
              (dolist (entry '("/.mevedel/sessions/"
                               "/.mevedel/tool-results/"
                               "/.mevedel/input-history.el"
                               "/.mevedel/media/"
                               "/.mevedel/plugin-data/"))
                (goto-char (point-min))
                (should (re-search-forward
                         (concat "^" (regexp-quote entry) "$") nil t))
                (should (= 1 (how-many
                              (concat "^" (regexp-quote entry) "$")
                              (point-min) (point-max)))))
              (should-not (string-match-p "^/.mevedel/$" content)))))
      (delete-directory root t))))

(provide 'test-mevedel-workspace)
;;; test-mevedel-workspace.el ends here
