;;; test-mevedel-pin.el --- Tests for symlink-proof tool path opening -*- lexical-binding: t -*-

;;; Commentary:

;; Tests `mevedel-pin' and the Read, ApplyPatch, Grep and resource opens that
;; rely on it.  A component swapped for a symlink after authorization, or a
;; dangling link resolved after it, is refused instead of followed into a
;; protected tree; a workspace link is authorized on the file it reaches.
;; Every protected tree here is a decoy; no real key material is touched.

;;; Code:

(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-hooks)
(require 'mevedel-permission-queue)
(require 'mevedel-pin)
(require 'mevedel-pipeline)
(require 'mevedel-resource)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'mevedel-tool-fs)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-fs-search)
(require 'mevedel-tool-patch)
(require 'mevedel-tool-patch-registration)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)

(defun test-mevedel-pin--call-with-trees (function)
  "Call FUNCTION with a workspace and a decoy protected tree.
FUNCTION receives a plist of :ws, :outside, :ssh and :session.  OUTSIDE/.ssh
holds DECOY-KEY and is inaccessible; WS/a/key holds PLAIN."
  (let* ((top (file-truename (make-temp-file "mevedel-pin-" t)))
         (outside (file-name-concat top "outside"))
         (ssh (file-name-concat outside ".ssh"))
         (ws (file-name-as-directory (file-name-concat top "ws")))
         (mevedel-permission-rules nil)
         (mevedel-permission-mode 'edits)
         (mevedel-protected-paths `((,(concat ssh "/**") . inaccessible))))
    (unwind-protect
        (progn
          (mevedel-tool-fs--register)
          (mevedel-tool-patch-register)
          (make-directory ssh t)
          (make-directory (file-name-concat ws "a") t)
          (write-region "DECOY-KEY\n" nil (file-name-concat ssh "key") nil 'silent)
          (write-region "PLAIN\n" nil (file-name-concat ws "a/key") nil 'silent)
          (let* ((workspace (mevedel-workspace-get-or-create 'project ws ws "ws"))
                 (session (mevedel-session-create "main" workspace)))
            (setf (mevedel-session-permission-mode session) 'edits)
            (funcall function (list :ws ws :outside outside :ssh ssh :session session))))
      (delete-directory top t))))

(defun test-mevedel-pin--run (session tool args &optional before)
  "Run TOOL with ARGS through the pipeline in SESSION.
BEFORE, when non-nil, runs once after authorization, just before the
handler.  Return (:queued QUEUED :result RESULT)."
  (let* ((handler (mevedel-tool-handler (mevedel-tool-get tool)))
         (advice (lambda (&rest _)
                   (when before (funcall (prog1 before (setq before nil))))))
         queued result)
    (advice-add handler :before advice)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                   (lambda (_event _payload callback &rest _) (funcall callback nil)))
                  ((symbol-function 'mevedel-permission--enqueue)
                   (lambda (entry &optional _session) (setq queued entry))))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (setq default-directory
                  (mevedel-workspace-root (mevedel-session-workspace session)))
            (mevedel-pipeline-run-tool (mevedel-tool-get tool)
                                       (lambda (value) (setq result value)) args)
            (let ((deadline (+ (float-time) 10)))
              (while (and (not (or queued result)) (< (float-time) deadline))
                (accept-process-output nil 0.02)))))
      (advice-remove handler advice))
    (list :queued (and queued t) :result (format "%s" result))))

(defun test-mevedel-pin--swap (directory target)
  "Return a function replacing DIRECTORY with a symlink to TARGET."
  (lambda ()
    (rename-file directory (concat directory ".real"))
    (make-symbolic-link target directory)))

(defun test-mevedel-pin--patch (operation path &optional body)
  "Return a patch performing OPERATION on PATH with BODY lines."
  (format "*** Begin Patch\n*** %s File: %s\n%s*** End Patch\n"
          operation path (or body "+OWNED\n")))

(mevedel-deftest mevedel-pin-call-with-file ()
  ,test
  (test)
  :doc "pins a canonical file and refuses a symlinked or missing component"
  (let* ((top (file-truename (make-temp-file "mevedel-pin-unit-" t)))
         (file (file-name-concat top "real/f")))
    (unwind-protect
        (progn
          (make-directory (file-name-concat top "real"))
          (write-region "CONTENT" nil file nil 'silent)
          (make-symbolic-link (file-name-concat top "real") (file-name-concat top "link"))
          (should (equal "CONTENT"
                         (mevedel-pin-with-file file
                           (with-temp-buffer
                             (mevedel-pin-insert-file-contents file)
                             (buffer-string)))))
          (should-error (mevedel-pin-with-file (file-name-concat top "link/f") nil)
                        :type 'mevedel-pin-refused)
          (should-error (mevedel-pin-with-file (file-name-concat top "real/missing") nil)
                        :type 'mevedel-pin-refused))
      (delete-directory top t)))
  :doc "writes create missing directories and replace a planted final link"
  (let* ((top (file-truename (make-temp-file "mevedel-pin-write-" t)))
         (target (file-name-concat top "elsewhere"))
         (path (file-name-concat top "new/dir/file")))
    (unwind-protect
        (progn
          (write-region "KEEP" nil target nil 'silent)
          (mevedel-pin-write-file-atomically path "ONE")
          (should (equal "ONE" (with-temp-buffer (insert-file-contents path) (buffer-string))))
          (delete-file path)
          (make-symbolic-link target path)
          (mevedel-pin-write-file-atomically path "TWO")
          (should-not (file-symlink-p path))
          (should (equal "KEEP" (with-temp-buffer (insert-file-contents target) (buffer-string))))
          (mevedel-pin-delete-file path)
          (should-not (file-exists-p path)))
      (delete-directory top t))))

(mevedel-deftest mevedel-pin-open-path ()
  ,test
  (test)
  :doc "a directory swapped for a protected link after authorization is refused"
  (test-mevedel-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (session (plist-get trees :session))
            (path (file-name-concat ws "a/key")))
       ;; Negative control: the same read without the swap.
       (should (string-match-p "PLAIN" (plist-get (test-mevedel-pin--run
                                                   session "Read" (list :file_path path))
                                                  :result)))
       (let ((run (test-mevedel-pin--run
                   session "Read" (list :file_path path)
                   (test-mevedel-pin--swap (file-name-concat ws "a") (plist-get trees :ssh)))))
         (should-not (string-match-p "DECOY" (plist-get run :result)))
         (should (string-match-p "symlink in authorized path" (plist-get run :result)))))))
  :doc "a dangling link resolved after authorization is refused"
  (test-mevedel-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (session (plist-get trees :session))
            (later (file-name-concat ws "later")))
       (make-symbolic-link later (file-name-concat ws "d"))
       (let ((run (test-mevedel-pin--run
                   session "Read" (list :file_path (file-name-concat ws "d/key"))
                   (lambda () (make-symbolic-link (plist-get trees :ssh) later)))))
         (should-not (string-match-p "DECOY" (plist-get run :result))))
       ;; Negative control: the target appears as a real directory.
       (delete-file later)
       (let ((run (test-mevedel-pin--run
                   session "Read" (list :file_path (file-name-concat ws "d/key"))
                   (lambda ()
                     (make-directory later)
                     (write-region "LATER\n" nil (file-name-concat later "key") nil 'silent)))))
         (should (string-match-p "LATER" (plist-get run :result))))))))

(mevedel-deftest mevedel-pin-write-file-atomically ()
  ,test
  (test)
  :doc "an Add whose directory is swapped for a protected link writes nothing there"
  (test-mevedel-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (ssh (plist-get trees :ssh))
            (session (plist-get trees :session)))
       (test-mevedel-pin--run
        session "ApplyPatch"
        (list :patch (test-mevedel-pin--patch "Add" (file-name-concat ws "a/new")))
        (test-mevedel-pin--swap (file-name-concat ws "a") ssh))
       (should-not (file-exists-p (file-name-concat ssh "new")))
       ;; Negative control: without the swap the file lands in the workspace.
       (delete-file (file-name-concat ws "a"))
       (rename-file (file-name-concat ws "a.real") (file-name-concat ws "a"))
       (test-mevedel-pin--run
        session "ApplyPatch"
        (list :patch (test-mevedel-pin--patch "Add" (file-name-concat ws "a/new"))))
       (should (file-exists-p (file-name-concat ws "a/new"))))))
  :doc "an Add below a dangling link that resolves after authorization writes nothing there"
  (test-mevedel-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (ssh (plist-get trees :ssh))
            (session (plist-get trees :session))
            (later (file-name-concat ws "later")))
       (make-symbolic-link later (file-name-concat ws "d"))
       (test-mevedel-pin--run
        session "ApplyPatch"
        (list :patch (test-mevedel-pin--patch "Add" (file-name-concat ws "d/new")))
        (lambda () (make-symbolic-link ssh later)))
       (should-not (file-exists-p (file-name-concat ssh "new")))
       ;; Negative control: nothing intervenes, so the directory is created.
       (delete-file later)
       (test-mevedel-pin--run
        session "ApplyPatch"
        (list :patch (test-mevedel-pin--patch "Add" (file-name-concat ws "d/new"))))
       (should (file-exists-p (file-name-concat later "new")))
       (should-not (file-symlink-p later)))))
  :doc "Edits asks before writing through a linked directory or file outside the workspace"
  (test-mevedel-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (home (file-name-concat (plist-get trees :outside) "home"))
            (session (plist-get trees :session)))
       (make-directory home)
       (write-region "ORIG\n" nil (file-name-concat home "rc") nil 'silent)
       (make-symbolic-link home (file-name-concat ws "h"))
       (make-symbolic-link (file-name-concat home "rc") (file-name-concat ws "rclink"))
       ;; Only the workspace is writable without asking.
       (cl-letf (((symbol-function 'mevedel--all-allowed-roots)
                  (lambda (&rest _) (list ws))))
         (should (plist-get (test-mevedel-pin--run
                             session "ApplyPatch"
                             (list :patch (test-mevedel-pin--patch
                                           "Add" (file-name-concat ws "h/new"))))
                            :queued))
         (should (plist-get (test-mevedel-pin--run
                             session "ApplyPatch"
                             (list :patch (test-mevedel-pin--patch
                                           "Update" (file-name-concat ws "rclink")
                                           "@@\n-ORIG\n+OWNED\n")))
                            :queued))
         ;; Negative control: an ordinary workspace file is written unasked.
         (should-not (plist-get (test-mevedel-pin--run
                                 session "ApplyPatch"
                                 (list :patch (test-mevedel-pin--patch
                                               "Add" (file-name-concat ws "plain-new"))))
                                :queued)))
       (should-not (file-exists-p (file-name-concat home "new")))
       (should (equal "ORIG\n" (with-temp-buffer
                                 (insert-file-contents (file-name-concat home "rc"))
                                 (buffer-string))))
       (should (file-exists-p (file-name-concat ws "plain-new")))))))

(mevedel-deftest mevedel-pin-call-with-directory ()
  ,test
  (test)
  :doc "confined Grep through a directory swapped for a protected link finds no decoy"
  (let* ((mevedel-sandbox--probe-cache nil)
         (availability (mevedel-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (unless (executable-find "rg") (ert-skip "rg is unavailable"))
    (test-mevedel-pin--call-with-trees
     (lambda (trees)
       (let* ((ws (plist-get trees :ws))
              (session (plist-get trees :session))
              (args (list :pattern "PLAIN|DECOY" :path (file-name-concat ws "a")
                          :output_mode "content")))
         ;; Negative control: the unswapped directory is searched.
         (should (string-match-p "PLAIN" (plist-get (test-mevedel-pin--run session "Grep" args)
                                                    :result)))
         (should-not (string-match-p
                      "DECOY"
                      (plist-get (test-mevedel-pin--run
                                  session "Grep" args
                                  (test-mevedel-pin--swap (file-name-concat ws "a")
                                                          (plist-get trees :ssh)))
                                 :result))))))))

(mevedel-deftest mevedel-pin-insert-file-contents ()
  ,test
  (test)
  :doc "a work://shared read whose directory is swapped for a protected link is refused"
  (test-mevedel-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (ssh (plist-get trees :ssh))
            (session (plist-get trees :session))
            (shared (file-name-concat ws ".mevedel/shared/notes")))
       (make-directory shared t)
       (write-region "SHARED-PLAIN\n" nil (file-name-concat shared "key") nil 'silent)
       ;; Negative control: the unswapped note is read.
       (should (string-match-p
                "SHARED-PLAIN"
                (plist-get (test-mevedel-pin--run
                            session "Read" (list :file_path "work://shared/notes/key"))
                           :result)))
       (should-not (string-match-p
                    "DECOY"
                    (plist-get (test-mevedel-pin--run
                                session "Read" (list :file_path "work://shared/notes/key")
                                (test-mevedel-pin--swap shared ssh))
                               :result)))))))

(provide 'test-mevedel-pin)

;;; test-mevedel-pin.el ends here
