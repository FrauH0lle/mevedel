;;; test-mevedel-workspace.el --- Tests for mevedel-workspace.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-execution-target)
(require 'mevedel-structs)
(require 'mevedel-workspace)

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
                (should (file-equal-p tmp (cdr result))))
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
;;; Workspace registry

(mevedel-deftest mevedel-workspace-get-or-create
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "creates workspace on first call"
  (let ((ws (mevedel-workspace-get-or-create
             'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (should (mevedel-workspace-p ws))
    (should (eq 'project (mevedel-workspace-type ws)))
    (should (equal "p1" (mevedel-workspace-name ws)))
    (should (mevedel-file-cache-p (mevedel-workspace-file-cache ws))))

  :doc "returns same struct on second call"
  (let ((ws1 (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
        (ws2 (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1-renamed")))
    (should (eq ws1 ws2))
    (should (equal "p1" (mevedel-workspace-name ws2))))

  :doc "different IDs create different workspaces"
  (let ((ws1 (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
        (ws2 (mevedel-workspace-get-or-create
              'project "/tmp/p2/" "/tmp/p2/" "p2")))
    (should-not (eq ws1 ws2))
    (should (equal "p1" (mevedel-workspace-name ws1)))
    (should (equal "p2" (mevedel-workspace-name ws2))))

  :doc "different types with same ID create different workspaces"
  (let ((ws1 (mevedel-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1-project"))
        (ws2 (mevedel-workspace-get-or-create
              'file "/tmp/p1/" "/tmp/p1/" "p1-file")))
    (should-not (eq ws1 ws2)))

  :doc "normalizes tilde project roots"
  (let* ((root "~/mevedel-workspace-root/")
         (expected (expand-file-name root))
         (ws (mevedel-workspace-get-or-create
              'project root root "home-root")))
    (should (equal expected (mevedel-workspace-id ws)))
    (should (equal expected (mevedel-workspace-root ws))))

  :doc "deduplicates project root aliases after expansion"
  (let* ((root "~/mevedel-workspace-root/")
         (expanded (expand-file-name root))
         (ws1 (mevedel-workspace-get-or-create
               'project root root "home-root"))
         (ws2 (mevedel-workspace-get-or-create
               'project expanded expanded "expanded-root")))
    (should (eq ws1 ws2)))

  :doc "keeps non-project identifiers opaque"
  (let* ((root "~/mevedel-file-root/")
         (ws (mevedel-workspace-get-or-create
              'file "relative-id" root "file-root")))
    (should (equal "relative-id" (mevedel-workspace-id ws)))
    (should (equal (expand-file-name root)
                   (mevedel-workspace-root ws)))))

(mevedel-deftest mevedel-workspace-clear-registry
  (:doc "`mevedel-workspace-clear-registry' removes all entries")
  (let ((ws (mevedel-workspace-get-or-create 'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (mevedel-workspace-clear-registry)
    (should-not
     (eq ws (mevedel-workspace-get-or-create 'project "/tmp/p1/" "/tmp/p1/" "p1")))))


;;
;;; Workspace helpers

(mevedel-deftest mevedel-workspace-state-dir
  (:doc "`mevedel-workspace-state-dir' returns .mevedel/ under root")
  ,test
  (test)
  :doc "returns .mevedel under root"
  (let ((ws (mevedel-workspace--create :root "/tmp/project/")))
    (should (equal (file-name-concat (expand-file-name "/tmp/project/")
                                     ".mevedel/")
                   (mevedel-workspace-state-dir ws))))

  :doc "expands tilde roots"
  (let* ((root "~/mevedel-test-root/")
         (ws (mevedel-workspace--create :root root)))
    (should (equal (file-name-concat (expand-file-name root) ".mevedel/")
                   (mevedel-workspace-state-dir ws)))))

(mevedel-deftest mevedel-workspace-find-state-file
  (:doc "`mevedel-workspace-find-state-file' checks project then global")
  ,test
  (test)
  :doc "returns project path when project file exists"
  (let* ((dir (make-temp-file "mevedel-test-" t))
         (mevedel-dir (file-name-concat dir ".mevedel/"))
         (ws (mevedel-workspace--create :root (file-name-as-directory dir))))
    (unwind-protect
        (progn
          (make-directory mevedel-dir t)
          (write-region "" nil (file-name-concat mevedel-dir "config.el"))
          (should (equal (file-name-concat mevedel-dir "config.el")
                         (mevedel-workspace-find-state-file ws "config.el"))))
      (delete-directory dir t)))

  :doc "falls back to global path when project file missing"
  (let* ((dir (make-temp-file "mevedel-test-" t))
         (global-dir (make-temp-file "mevedel-global-" t))
         (mevedel-user-dir (file-name-as-directory global-dir))
         (ws (mevedel-workspace--create :root (file-name-as-directory dir))))
    (unwind-protect
        (progn
          (write-region "" nil (file-name-concat global-dir "config.el"))
          (should (equal (file-name-concat global-dir "config.el")
                         (mevedel-workspace-find-state-file ws "config.el"))))
      (delete-directory dir t)
      (delete-directory global-dir t)))

  :doc "returns project path when neither exists"
  (let* ((dir (make-temp-file "mevedel-test-" t))
         (root (file-name-as-directory
                (file-name-concat dir "missing-project")))
         (mevedel-user-dir
          (file-name-as-directory
           (file-name-concat dir "missing-global")))
         (ws (mevedel-workspace--create :root root)))
    (unwind-protect
        (should (equal (file-name-concat root ".mevedel/config.el")
                       (mevedel-workspace-find-state-file ws "config.el")))
      (delete-directory dir t))))


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
  :doc "includes default roots and memory directories"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/rootsproj/" "/tmp/rootsproj/" "rootsproj"))
         (mevedel-workspace-additional-roots nil)
         (mevedel-memory-dirs '(".mevedel/memory/" ".agents/memory/")))
    (with-temp-buffer
      (setq-local mevedel--workspace ws)
      (let ((roots (mevedel--all-allowed-roots))
            (root (file-name-as-directory (expand-file-name "/tmp/rootsproj/"))))
        (should (member root roots))
        (should (member (file-name-concat root ".mevedel/memory/") roots))
        (should (member (file-name-concat root ".agents/memory/") roots))
        (should (member (file-name-as-directory
                         (expand-file-name temporary-file-directory))
                        roots)))))

  :doc "includes additional roots configured for workspace"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/rootsproj/" "/tmp/rootsproj/" "rootsproj"))
         (mevedel-workspace-additional-roots
          (list
           (cons (file-name-as-directory (expand-file-name "/tmp/rootsproj/"))
                 (list "/tmp/extra-a/" "/tmp/extra-b/")))))
    (with-temp-buffer
      (setq-local mevedel--workspace ws)
      (let ((roots (mevedel--all-allowed-roots)))
        (should (member (file-name-as-directory (expand-file-name "/tmp/extra-a/"))
                        roots))
        (should (member (file-name-as-directory (expand-file-name "/tmp/extra-b/"))
                        roots)))))

  :doc "uses target TMPDIR or target /tmp without granting client temp paths"
  (let* ((remote-root "/ssh:user@host:/srv/project/")
         (workspace (mevedel-workspace--create
                     :type 'project :id remote-root :root remote-root
                     :name "remote"))
         (session (mevedel-session-create "main" workspace))
         (target (mevedel-session-execution-target session))
         (mevedel-memory-dirs nil)
         (mevedel-workspace-additional-roots nil))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local default-directory remote-root)
      (setq-local temporary-file-directory "/client/tmp/")
      (dolist (case '((:environment (("TMPDIR" . "/var/tmp/target"))
                       :expected "/ssh:user@host:/var/tmp/target/")
                      (:environment nil
                       :expected "/ssh:user@host:/tmp/")))
        (setf (mevedel-execution-target-environment target)
              (plist-get case :environment))
        (let ((roots (mevedel--all-allowed-roots)))
          (should (member (plist-get case :expected) roots))
          (should-not (member "/ssh:user@host:/client/tmp/" roots))
          (should-not (member "/client/tmp/" roots)))))))

(mevedel-deftest mevedel-workspace-file-in-allowed-roots-p
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
         (file (file-name-concat root-dir "inside.txt")))
    (write-region "" nil file)
    (with-temp-buffer
      (setq-local mevedel--workspace ws)
      (should (equal root-dir
                     (mevedel-workspace-file-in-allowed-roots-p file)))))

  :doc "returns matching additional root when file lives there"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "rootsproj"))
         (mevedel-workspace-additional-roots
          (list (cons root-dir (list extra-dir))))
         (file (file-name-concat extra-dir "extra.txt")))
    (write-region "" nil file)
    (with-temp-buffer
      (setq-local mevedel--workspace ws)
      (should (member
               (mevedel-workspace-file-in-allowed-roots-p file)
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
            (should (null (mevedel-workspace-file-in-allowed-roots-p file)))))
      (delete-directory parent-dir t))))


;;
;;; Generated state ignore

(mevedel-deftest mevedel-workspace-ensure-generated-state-ignored
  (:doc "`mevedel-workspace-ensure-generated-state-ignored' writes exact generated-state excludes")
  ,test
  (test)
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-ws-ignore-" t)))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "ignore"))
         (exclude (file-name-concat root ".git" "info" "exclude")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory exclude) t)
          (cl-letf (((symbol-function 'process-file)
                     (lambda (&rest _)
                       (ert-fail "Generated-state ignore invoked Git"))))
            (mevedel-workspace-ensure-generated-state-ignored workspace)
            (mevedel-workspace-ensure-generated-state-ignored workspace))
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
      (delete-directory root t)))

  :doc "settles the exclusion once per root instead of on every save"
  ;; Re-deriving costs a `locate-dominating-file' walk plus two `file-truename'
  ;; resolutions, which on a remote workspace is a round trip per path
  ;; component -- paid on every save to find nothing to do.
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-ws-ignore-once-" t)))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "once"))
         (exclude (file-name-concat root ".git" "info" "exclude"))
         (walks 0))
    (unwind-protect
        (progn
          (make-directory (file-name-directory exclude) t)
          (clrhash mevedel-workspace--generated-state-ignored)
          (cl-letf* ((real (symbol-function 'locate-dominating-file))
                     ((symbol-function 'locate-dominating-file)
                      (lambda (file name)
                        (cl-incf walks)
                        (funcall real file name))))
            (mevedel-workspace-ensure-generated-state-ignored workspace)
            (should (= 1 walks))
            (mevedel-workspace-ensure-generated-state-ignored workspace)
            (mevedel-workspace-ensure-generated-state-ignored workspace)
            (should (= 1 walks))
            ;; A root outside Git settles too; deciding that costs the walk.
            (let* ((bare (file-name-as-directory
                          (make-temp-file "mevedel-ws-ignore-bare-" t)))
                   (other (mevedel-workspace--create
                           :type 'project :id bare :root bare :name "bare")))
              (unwind-protect
                  (progn
                    (setq walks 0)
                    (mevedel-workspace-ensure-generated-state-ignored other)
                    (should (= 1 walks))
                    (mevedel-workspace-ensure-generated-state-ignored other)
                    (should (= 1 walks)))
                (delete-directory bare t)))))
      (clrhash mevedel-workspace--generated-state-ignored)
      (delete-directory root t)))

  :doc "writes a linked worktree's common exclude and retries target failure"
  (let* ((parent (make-temp-file "mevedel-ws-linked-ignore-" t))
         (root (file-name-concat parent "root"))
         (linked (file-name-concat parent "linked"))
         (exclude (file-name-concat root ".git" "info" "exclude")))
    (unwind-protect
        (progn
          (make-directory root)
          (let ((default-directory (file-name-as-directory root)))
            (should (zerop (process-file "git" nil nil nil
                                         "init" "--quiet")))
            (with-temp-file (file-name-concat root "tracked")
              (insert "tracked\n"))
            (should (zerop (process-file "git" nil nil nil
                                         "add" "tracked")))
            (should
             (zerop
              (process-file "git" nil nil nil
                            "-c" "user.name=Mevedel Test"
                            "-c" "user.email=mevedel@example.invalid"
                            "commit" "--quiet" "-m" "initial")))
            (should
             (zerop
              (process-file "git" nil nil nil
                            "worktree" "add" "--detach" "--quiet"
                            linked))))
          (let ((workspace
                 (mevedel-workspace--create
                  :type 'project :id linked :root linked :name "linked")))
            (mevedel-workspace-ensure-generated-state-ignored workspace))
          (with-temp-buffer
            (insert-file-contents exclude)
            (should (string-match-p
                     "^/.mevedel/sessions/$" (buffer-string))))
          (clrhash mevedel-workspace--generated-state-ignored)
          (let* ((qualified (concat "/mevedelmock:linked:" linked "/"))
                 (workspace
                  (mevedel-workspace--create
                   :type 'project :id qualified :root qualified
                   :name "remote-linked"))
                 (real-process-file (symbol-function 'process-file))
                 (failed nil))
            (mevedel-test--with-local-shell-tramp '("linked")
              (cl-letf (((symbol-function 'process-file)
                         (lambda (program infile destination display
                                  &rest args)
                           (if (and (not failed)
                                    (equal program "git")
                                    (equal args '("rev-parse" "--git-path"
                                                  "info/exclude")))
                               (progn
                                 (setq failed t)
                                 1)
                             (apply real-process-file
                                    program infile destination display
                                    args)))))
                (mevedel-workspace-ensure-generated-state-ignored workspace)
                (should-not
                 (gethash qualified
                          mevedel-workspace--generated-state-ignored))
                (mevedel-workspace-ensure-generated-state-ignored workspace)
                (should
                 (gethash qualified
                          mevedel-workspace--generated-state-ignored))))
            (should failed)))
      (clrhash mevedel-workspace--generated-state-ignored)
      (delete-directory parent t)))

  :doc "writes generated-state excludes through a TRAMP workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-ws-remote-ignore-" t)))
         (qualified (concat "/mevedelmock:workspace:" root))
         (workspace (mevedel-workspace--create
                     :type 'project :id qualified :root qualified
                     :name "remote-ignore"))
         (exclude (file-name-concat root ".git" "info" "exclude")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory exclude) t)
          (mevedel-test--with-local-shell-tramp '("workspace")
            (cl-letf (((symbol-function 'process-file)
                       (lambda (&rest _)
                         (ert-fail "Generated-state ignore invoked Git"))))
              (mevedel-workspace-ensure-generated-state-ignored workspace)))
          (with-temp-buffer
            (insert-file-contents exclude)
            (should (string-match-p
                     "^/.mevedel/sessions/$" (buffer-string)))))
      (delete-directory root t)))

  :doc "does not follow project-controlled Git metadata outside the workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-ws-external-git-" t)))
         (metadata (file-name-as-directory
                    (make-temp-file "mevedel-ws-external-meta-" t)))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "external"))
         (exclude (file-name-concat metadata "info" "exclude")))
    (unwind-protect
        (progn
          (make-symbolic-link metadata (file-name-concat root ".git"))
          (cl-letf (((symbol-function 'process-file)
                     (lambda (&rest _)
                       (ert-fail "Generated-state ignore invoked Git"))))
            (mevedel-workspace-ensure-generated-state-ignored workspace))
          (should-not (file-exists-p exclude)))
      (delete-directory root t)
      (delete-directory metadata t))))

(mevedel-deftest mevedel-workspace-file-buffers ()
  ,test
  (test)
  :doc "returns live buffers visiting files under the workspace root"
  (let* ((root (make-temp-file "mevedel-wfb-" t))
         (inside (file-name-concat root "inside.el"))
         (outside (make-temp-file "mevedel-wfb-outside-" nil ".el"))
         buffers)
    (unwind-protect
        (progn
          (write-region "" nil inside nil 'silent)
          (setq buffers (list (find-file-noselect inside)
                              (find-file-noselect outside)))
          (let ((workspace (mevedel-workspace-get-or-create
                            'project root root "wfb")))
            (let ((found (mevedel-workspace-file-buffers workspace)))
              (should (memq (car buffers) found))
              (should-not (memq (cadr buffers) found))))
          ;; No workspace, no scan.
          (should-not (mevedel-workspace-file-buffers nil)))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (set-buffer-modified-p nil)
          (kill-buffer buffer)))
      (mevedel-workspace-clear-registry)
      (delete-file outside)
      (delete-directory root t))))

(provide 'test-mevedel-workspace)
;;; test-mevedel-workspace.el ends here
