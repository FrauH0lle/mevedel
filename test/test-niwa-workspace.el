;;; test-niwa-workspace.el --- Tests for niwa-workspace.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-execution-target)
(require 'niwa-structs)
(require 'niwa-workspace)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


(niwa-deftest niwa-workspace-current ()
  ,test
  (test)
  :doc "returns the buffer's workspace and refuses a buffer without one"
  (cl-letf (((symbol-function 'niwa-workspace) (lambda (&rest _) 'workspace)))
    (should (eq 'workspace (niwa-workspace-current))))
  (cl-letf (((symbol-function 'niwa-workspace) #'ignore))
    (should-error (niwa-workspace-current) :type 'user-error)))


;;
;;; Workspace detection helpers

(niwa-deftest niwa-workspace--file-workspace
  (:doc "`niwa-workspace--file-workspace' returns (file . FILENAME) for file-visiting buffers")
  ,test
  (test)
  :doc "returns nil when buffer not visiting a file"
  (with-temp-buffer
    (should (null (niwa-workspace--file-workspace))))

  :doc "returns (file . FILENAME) when buffer visits a file"
  (let ((tmp (make-temp-file "niwa-ws-")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmp)
          (unwind-protect
              (let ((result (niwa-workspace--file-workspace)))
                (should (eq 'file (car result)))
                (should (file-equal-p tmp (cdr result))))
            (kill-buffer (current-buffer))))
      (delete-file tmp))))

(niwa-deftest niwa-workspace--project-workspace
  (:doc "`niwa-workspace--project-workspace' returns (project . ROOT) or nil"
   :vars* ((fake-project-dir (file-name-as-directory
                              (make-temp-file "niwa-ws-proj-" t))))
   :after-each (delete-directory fake-project-dir t))
  ,test
  (test)
  :doc "returns nil when no project detected"
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _prompt _dir) nil)))
    (with-temp-buffer
      (setq default-directory fake-project-dir)
      (should (null (niwa-workspace--project-workspace)))))

  :doc "returns (project . ROOT) when project.el finds one"
  (cl-letf* ((fake-project (list 'transient fake-project-dir))
             ((symbol-function 'project-current)
              (lambda (&optional _prompt _dir) fake-project))
             ((symbol-function 'project-root)
              (lambda (project) (nth 1 project))))
    (with-temp-buffer
      (setq default-directory fake-project-dir)
      (let ((result (niwa-workspace--project-workspace)))
        (should (eq 'project (car result)))
        (should (equal fake-project-dir (cdr result)))))))


;;
;;; Workspace type accessors

(niwa-deftest niwa-workspace--directory-workspace
  (:vars* ((root (file-name-as-directory
                 (make-temp-file "niwa-directory-workspace-" t))))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "uses the current directory without a project marker"
  (let ((default-directory root))
    (should (equal (cons 'project root)
                   (niwa-workspace--directory-workspace))))

  :doc "nested directories reuse a durable root but ignore plain config directories"
  (let ((default-directory (file-name-concat root "nested/")))
    (make-directory default-directory)
    (make-directory (file-name-concat root ".niwa"))
    (should (equal (cons 'project default-directory)
                   (niwa-workspace--directory-workspace)))
    (with-temp-file (file-name-concat root ".niwa/workspace-id")
      (insert (make-string 64 ?a) "\n"))
    (should (equal (cons 'project root)
                   (niwa-workspace--directory-workspace)))))

(niwa-deftest niwa-workspace--project-root
  (:doc "`niwa-workspace--project-root' validates that id is a directory"
   :vars* ((dir (file-name-as-directory (make-temp-file "niwa-ws-pr-" t))))
   :after-each (delete-directory dir t))
  ,test
  (test)
  :doc "returns id when dir exists and project-current accepts it"
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _prompt _dir) '(transient "ok"))))
    (should (equal dir (niwa-workspace--project-root dir))))

  :doc "errors when id is not absolute"
  (should-error (niwa-workspace--project-root "relative/path/")
                :type 'error)

  :doc "errors when directory does not exist"
  (should-error (niwa-workspace--project-root "/nonexistent-niwa-ws-test/")
                :type 'error)

  :doc "accepts a plain directory without a project marker"
  (progn
    (should-not (project-current nil dir))
    (should (equal dir (niwa-workspace--project-root dir)))))

(niwa-deftest niwa-workspace--project-name
  (:doc "`niwa-workspace--project-name' falls back to directory name when project.el is silent")
  ,test
  (test)
  :doc "uses project-name when project-current returns a project"
  (cl-letf* ((fake-project '(transient "/tmp/some-proj/"))
             ((symbol-function 'project-current)
              (lambda (&optional _prompt _dir) fake-project))
             ((symbol-function 'project-name)
              (lambda (_project) "some-proj")))
    (should (equal "some-proj" (niwa-workspace--project-name "/tmp/some-proj/"))))

  :doc "falls back to last path segment when no project detected"
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _prompt _dir) nil)))
    (should (equal "some-proj"
                   (niwa-workspace--project-name "/tmp/some-proj/")))))


;;
;;; Workspace registry

(niwa-deftest niwa-workspace-get-or-create
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "creates workspace on first call"
  (let ((ws (niwa-workspace-get-or-create
             'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (should (niwa-workspace-p ws))
    (should (eq 'project (niwa-workspace-type ws)))
    (should (equal "p1" (niwa-workspace-name ws)))
    (should (niwa-file-cache-p (niwa-workspace-file-cache ws))))

  :doc "returns same struct on second call"
  (let ((ws1 (niwa-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
        (ws2 (niwa-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1-renamed")))
    (should (eq ws1 ws2))
    (should (equal "p1" (niwa-workspace-name ws2))))

  :doc "different IDs create different workspaces"
  (let ((ws1 (niwa-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1"))
        (ws2 (niwa-workspace-get-or-create
              'project "/tmp/p2/" "/tmp/p2/" "p2")))
    (should-not (eq ws1 ws2))
    (should (equal "p1" (niwa-workspace-name ws1)))
    (should (equal "p2" (niwa-workspace-name ws2))))

  :doc "different types with same ID create different workspaces"
  (let ((ws1 (niwa-workspace-get-or-create
              'project "/tmp/p1/" "/tmp/p1/" "p1-project"))
        (ws2 (niwa-workspace-get-or-create
              'file "/tmp/p1/" "/tmp/p1/" "p1-file")))
    (should-not (eq ws1 ws2)))

  :doc "normalizes tilde project roots"
  (let* ((root "~/niwa-workspace-root/")
         (expected (expand-file-name root))
         (ws (niwa-workspace-get-or-create
              'project root root "home-root")))
    (should (equal expected (niwa-workspace-id ws)))
    (should (equal expected (niwa-workspace-root ws))))

  :doc "deduplicates project root aliases after expansion"
  (let* ((root "~/niwa-workspace-root/")
         (expanded (expand-file-name root))
         (ws1 (niwa-workspace-get-or-create
               'project root root "home-root"))
         (ws2 (niwa-workspace-get-or-create
               'project expanded expanded "expanded-root")))
    (should (eq ws1 ws2)))

  :doc "keeps non-project identifiers opaque"
  (let* ((root "~/niwa-file-root/")
         (ws (niwa-workspace-get-or-create
              'file "relative-id" root "file-root")))
    (should (equal "relative-id" (niwa-workspace-id ws)))
    (should (equal (expand-file-name root)
                   (niwa-workspace-root ws)))))

(niwa-deftest niwa-workspace-clear-registry
  (:doc "`niwa-workspace-clear-registry' removes all entries")
  (let ((ws (niwa-workspace-get-or-create 'project "/tmp/p1/" "/tmp/p1/" "p1")))
    (niwa-workspace-clear-registry)
    (should-not
     (eq ws (niwa-workspace-get-or-create 'project "/tmp/p1/" "/tmp/p1/" "p1")))))


;;
;;; Workspace helpers

(niwa-deftest niwa-workspace-state-dir
  (:doc "`niwa-workspace-state-dir' returns .niwa/ under root")
  ,test
  (test)
  :doc "returns .niwa under root"
  (let ((ws (niwa-workspace--create :root "/tmp/project/")))
    (should (equal (file-name-concat (expand-file-name "/tmp/project/")
                                     ".niwa/")
                   (niwa-workspace-state-dir ws))))

  :doc "expands tilde roots"
  (let* ((root "~/niwa-test-root/")
         (ws (niwa-workspace--create :root root)))
    (should (equal (file-name-concat (expand-file-name root) ".niwa/")
                   (niwa-workspace-state-dir ws)))))


;;
;;; Main workspace accessor

(niwa-deftest niwa-workspace
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "returns session's workspace when session is active"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/wsproj/" "/tmp/wsproj/" "wsproj"))
         (session (niwa-session-create "main" ws)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (should (eq ws (niwa-workspace)))))

  :doc "returns cached workspace when set and no session"
  (let ((ws (niwa-workspace-get-or-create
             'project "/tmp/wsproj/" "/tmp/wsproj/" "wsproj")))
    (with-temp-buffer
      (setq-local niwa--workspace ws)
      (should (eq ws (niwa-workspace)))))

  :doc "auto-detects via workspace-functions when no session/cache"
  (let ((detected nil))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _prompt _dir) nil)))
      (with-temp-buffer
        (let ((niwa-workspace-functions
               (list (lambda ()
                       (setq detected t)
                       (cons 'file "/tmp/some-file.el")))))
          (let ((ws (niwa-workspace)))
            (should detected)
            (should (niwa-workspace-p ws))
            (should (eq 'file (niwa-workspace-type ws)))
            (should (equal "/tmp/some-file.el" (niwa-workspace-id ws))))))))

  :doc "returns nil when no workspace function matches"
  (with-temp-buffer
    (let ((niwa-workspace-functions (list (lambda () nil))))
      (should (null (niwa-workspace)))))

  :doc "file buffers and Dired share the same plain-directory authority"
  (let ((root (file-name-as-directory (make-temp-file "niwa-ws-directory-" t))))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory root)
          (let ((workspace (niwa-workspace)))
            (setq buffer-file-name (file-name-concat root "notes.md"))
            (should (eq workspace (niwa-workspace)))
            (should (eq 'portable
                        (niwa-session-authority-mode-for-workspace
                         (niwa-workspace))))))
      (delete-directory root t))))


;;
;;; Allowed roots

(niwa-deftest niwa--all-allowed-roots
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "includes default roots and memory directories"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/rootsproj/" "/tmp/rootsproj/" "rootsproj"))
         (niwa-workspace-additional-roots nil)
         (niwa-memory-dirs '(".niwa/memory/" ".agents/memory/")))
    (with-temp-buffer
      (setq-local niwa--workspace ws)
      (let ((roots (niwa--all-allowed-roots))
            (root (file-name-as-directory (expand-file-name "/tmp/rootsproj/"))))
        (should (member root roots))
        (should (member (file-name-concat root ".niwa/memory/") roots))
        (should (member (file-name-concat root ".agents/memory/") roots))
        (should (member (file-name-as-directory
                         (expand-file-name temporary-file-directory))
                        roots)))))

  :doc "includes additional roots configured for workspace"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/rootsproj/" "/tmp/rootsproj/" "rootsproj"))
         (niwa-workspace-additional-roots
          (list
           (cons (file-name-as-directory (expand-file-name "/tmp/rootsproj/"))
                 (list "/tmp/extra-a/" "/tmp/extra-b/")))))
    (with-temp-buffer
      (setq-local niwa--workspace ws)
      (let ((roots (niwa--all-allowed-roots)))
        (should (member (file-name-as-directory (expand-file-name "/tmp/extra-a/"))
                        roots))
        (should (member (file-name-as-directory (expand-file-name "/tmp/extra-b/"))
                        roots)))))

  :doc "uses target TMPDIR or target /tmp without granting client temp paths"
  (let* ((remote-root "/ssh:user@host:/srv/project/")
         (workspace (niwa-workspace--create
                     :type 'project :id remote-root :root remote-root
                     :name "remote"))
         (session (niwa-session-create "main" workspace))
         (target (niwa-session-execution-target session))
         (niwa-memory-dirs nil)
         (niwa-workspace-additional-roots nil))
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local default-directory remote-root)
      (setq-local temporary-file-directory "/client/tmp/")
      (dolist (case '((:environment (("TMPDIR" . "/var/tmp/target"))
                       :expected "/ssh:user@host:/var/tmp/target/")
                      (:environment nil
                       :expected "/ssh:user@host:/tmp/")))
        (setf (niwa-execution-target-environment target)
              (plist-get case :environment))
        (let ((roots (niwa--all-allowed-roots)))
          (should (member (plist-get case :expected) roots))
          (should-not (member "/ssh:user@host:/client/tmp/" roots))
          (should-not (member "/client/tmp/" roots)))))))

(niwa-deftest niwa-workspace-file-in-allowed-roots-p
  (:before-each (niwa-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "niwa-ws-roots-" t)))
           (extra-dir (file-name-as-directory
                       (make-temp-file "niwa-ws-extra-" t)))
           (outside-dir (file-name-as-directory
                         (make-temp-file "niwa-ws-outside-" t))))
   :after-each
   (progn
     (niwa-workspace-clear-registry)
     (delete-directory root-dir t)
     (delete-directory extra-dir t)
     (delete-directory outside-dir t)))
  ,test
  (test)
  :doc "returns matching root for file inside workspace"
  (let* ((ws (niwa-workspace-get-or-create
              'project root-dir root-dir "rootsproj"))
         (niwa-workspace-additional-roots nil)
         (file (file-name-concat root-dir "inside.txt")))
    (write-region "" nil file)
    (with-temp-buffer
      (setq-local niwa--workspace ws)
      (should (equal root-dir
                     (niwa-workspace-file-in-allowed-roots-p file)))))

  :doc "returns matching additional root when file lives there"
  (let* ((ws (niwa-workspace-get-or-create
              'project root-dir root-dir "rootsproj"))
         (niwa-workspace-additional-roots
          (list (cons root-dir (list extra-dir))))
         (file (file-name-concat extra-dir "extra.txt")))
    (write-region "" nil file)
    (with-temp-buffer
      (setq-local niwa--workspace ws)
      (should (member
               (niwa-workspace-file-in-allowed-roots-p file)
               (list extra-dir
                     (file-name-as-directory
                      (expand-file-name temporary-file-directory)))))))

  :doc "returns nil for file outside any allowed root"
  (let* ((parent-dir (file-name-as-directory
                      (make-temp-file "niwa-ws-parent-" t)))
         (root (file-name-as-directory
                (file-name-concat parent-dir "root")))
         (outside (file-name-as-directory
                   (file-name-concat parent-dir "outside")))
         (ws (niwa-workspace-get-or-create
              'project root root "rootsproj"))
         (niwa-workspace-additional-roots nil)
         (temporary-file-directory (file-name-concat parent-dir "tmp/"))
         (file (file-name-concat outside "outside.txt")))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory outside)
          (make-directory temporary-file-directory)
          (write-region "" nil file)
          (with-temp-buffer
            (setq-local niwa--workspace ws)
            (should (null (niwa-workspace-file-in-allowed-roots-p file)))))
      (delete-directory parent-dir t))))


;;
;;; Generated state ignore

(niwa-deftest niwa-workspace-ensure-generated-state-ignored
  (:doc "`niwa-workspace-ensure-generated-state-ignored' writes exact generated-state excludes")
  ,test
  (test)
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-ws-ignore-" t)))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "ignore"))
         (exclude (file-name-concat root ".git" "info" "exclude")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory exclude) t)
          (cl-letf (((symbol-function 'process-file)
                     (lambda (&rest _)
                       (ert-fail "Generated-state ignore invoked Git"))))
            (niwa-workspace-ensure-generated-state-ignored workspace)
            (niwa-workspace-ensure-generated-state-ignored workspace))
          (with-temp-buffer
            (insert-file-contents exclude)
            (let ((content (buffer-string)))
              (dolist (entry '("/.niwa/sessions/"
                               "/.niwa/tool-results/"
                               "/.niwa/input-history.el"
                               "/.niwa/leases/"
                               "/.niwa/state/"))
                (goto-char (point-min))
                (should (re-search-forward
                         (concat "^" (regexp-quote entry) "$") nil t))
                (should (= 1 (how-many
                              (concat "^" (regexp-quote entry) "$")
                              (point-min) (point-max)))))
              (should-not (string-match-p "^/.niwa/$" content)))))
      (delete-directory root t)))

  :doc "settles the exclusion once per root instead of on every save"
  ;; Re-deriving costs a `locate-dominating-file' walk plus two `file-truename'
  ;; resolutions, which on a remote workspace is a round trip per path
  ;; component -- paid on every save to find nothing to do.
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-ws-ignore-once-" t)))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "once"))
         (exclude (file-name-concat root ".git" "info" "exclude"))
         (walks 0))
    (unwind-protect
        (progn
          (make-directory (file-name-directory exclude) t)
          (clrhash niwa-workspace--generated-state-ignored)
          (cl-letf* ((real (symbol-function 'locate-dominating-file))
                     ((symbol-function 'locate-dominating-file)
                      (lambda (file name)
                        (cl-incf walks)
                        (funcall real file name))))
            (niwa-workspace-ensure-generated-state-ignored workspace)
            (should (= 1 walks))
            (niwa-workspace-ensure-generated-state-ignored workspace)
            (niwa-workspace-ensure-generated-state-ignored workspace)
            (should (= 1 walks))
            ;; A root outside Git settles too; deciding that costs the walk.
            (let* ((bare (file-name-as-directory
                          (make-temp-file "niwa-ws-ignore-bare-" t)))
                   (other (niwa-workspace--create
                           :type 'project :id bare :root bare :name "bare")))
              (unwind-protect
                  (progn
                    (setq walks 0)
                    (niwa-workspace-ensure-generated-state-ignored other)
                    (should (= 1 walks))
                    (niwa-workspace-ensure-generated-state-ignored other)
                    (should (= 1 walks)))
                (delete-directory bare t)))))
      (clrhash niwa-workspace--generated-state-ignored)
      (delete-directory root t)))

  :doc "writes a linked worktree's common exclude and retries target failure"
  (let* ((parent (make-temp-file "niwa-ws-linked-ignore-" t))
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
                            "-c" "user.name=Niwa Test"
                            "-c" "user.email=niwa@example.invalid"
                            "commit" "--quiet" "-m" "initial")))
            (should
             (zerop
              (process-file "git" nil nil nil
                            "worktree" "add" "--detach" "--quiet"
                            linked))))
          (let ((workspace
                 (niwa-workspace--create
                  :type 'project :id linked :root linked :name "linked")))
            (niwa-workspace-ensure-generated-state-ignored workspace))
          (with-temp-buffer
            (insert-file-contents exclude)
            (should (string-match-p
                     "^/.niwa/sessions/$" (buffer-string))))
          (clrhash niwa-workspace--generated-state-ignored)
          (let* ((qualified (concat "/niwamock:linked:" linked "/"))
                 (workspace
                  (niwa-workspace--create
                   :type 'project :id qualified :root qualified
                   :name "remote-linked"))
                 (real-process-file (symbol-function 'process-file))
                 (failed nil))
            (niwa-test--with-local-shell-tramp '("linked")
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
                (niwa-workspace-ensure-generated-state-ignored workspace)
                (should-not
                 (gethash qualified
                          niwa-workspace--generated-state-ignored))
                (niwa-workspace-ensure-generated-state-ignored workspace)
                (should
                 (gethash qualified
                          niwa-workspace--generated-state-ignored))))
            (should failed)))
      (clrhash niwa-workspace--generated-state-ignored)
      (delete-directory parent t)))

  :doc "writes generated-state excludes through a TRAMP workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-ws-remote-ignore-" t)))
         (qualified (concat "/niwamock:workspace:" root))
         (workspace (niwa-workspace--create
                     :type 'project :id qualified :root qualified
                     :name "remote-ignore"))
         (exclude (file-name-concat root ".git" "info" "exclude")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory exclude) t)
          (niwa-test--with-local-shell-tramp '("workspace")
            (cl-letf (((symbol-function 'process-file)
                       (lambda (&rest _)
                         (ert-fail "Generated-state ignore invoked Git"))))
              (niwa-workspace-ensure-generated-state-ignored workspace)))
          (with-temp-buffer
            (insert-file-contents exclude)
            (should (string-match-p
                     "^/.niwa/sessions/$" (buffer-string)))))
      (delete-directory root t)))

  :doc "does not follow project-controlled Git metadata outside the workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-ws-external-git-" t)))
         (metadata (file-name-as-directory
                    (make-temp-file "niwa-ws-external-meta-" t)))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "external"))
         (exclude (file-name-concat metadata "info" "exclude")))
    (unwind-protect
        (progn
          (make-symbolic-link metadata (file-name-concat root ".git"))
          (cl-letf (((symbol-function 'process-file)
                     (lambda (&rest _)
                       (ert-fail "Generated-state ignore invoked Git"))))
            (niwa-workspace-ensure-generated-state-ignored workspace))
          (should-not (file-exists-p exclude)))
      (delete-directory root t)
      (delete-directory metadata t))))

(provide 'test-niwa-workspace)
;;; test-niwa-workspace.el ends here
