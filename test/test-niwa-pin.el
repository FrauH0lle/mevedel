;;; test-niwa-pin.el --- Tests for symlink-proof tool path opening -*- lexical-binding: t -*-

;;; Commentary:

;; Tests `niwa-pin' and the Read, ApplyPatch, Grep and resource opens that
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
(require 'niwa-hooks)
(require 'niwa-permission-queue)
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))
(require 'niwa-pin)
(require 'niwa-pipeline)
(require 'niwa-resource)
(require 'niwa-sandbox)
(require 'niwa-structs)
(require 'niwa-tool-fs)
(require 'niwa-tool-fs-read)
(require 'niwa-tool-fs-search)
(require 'niwa-tool-patch)
(require 'niwa-tool-patch-registration)
(require 'niwa-tool-registry)
(require 'niwa-workspace)

(defun test-niwa-pin--call-with-trees (function)
  "Call FUNCTION with a workspace and a decoy protected tree.
FUNCTION receives a plist of :ws, :outside, :ssh and :session.  OUTSIDE/.ssh
holds DECOY-KEY and is inaccessible; WS/a/key holds PLAIN."
  (let* ((top (file-truename (make-temp-file "niwa-pin-" t)))
         (outside (file-name-concat top "outside"))
         (ssh (file-name-concat outside ".ssh"))
         (ws (file-name-as-directory (file-name-concat top "ws")))
         (niwa-permission-rules nil)
         (niwa-permission-mode 'edits)
         (niwa-protected-paths `((,(concat ssh "/**") . inaccessible))))
    (unwind-protect
        (progn
          (niwa-tool-fs--register)
          (niwa-tool-patch-register)
          (make-directory ssh t)
          (make-directory (file-name-concat ws "a") t)
          (write-region "DECOY-KEY\n" nil (file-name-concat ssh "key") nil 'silent)
          (write-region "PLAIN\n" nil (file-name-concat ws "a/key") nil 'silent)
          (let* ((workspace (niwa-workspace-get-or-create 'project ws ws "ws"))
                 (session (niwa-session-create "main" workspace)))
            (setf (niwa-session-permission-mode session) 'edits)
            (funcall function (list :ws ws :outside outside :ssh ssh :session session))))
      (delete-directory top t))))

(defun test-niwa-pin--run (session tool args &optional before)
  "Run TOOL with ARGS through the pipeline in SESSION.
BEFORE, when non-nil, runs once after authorization, just before the
handler.  Return (:queued QUEUED :result RESULT)."
  (let* ((handler (niwa-tool-handler (niwa-tool-get tool)))
         (advice (lambda (&rest _)
                   (when before (funcall (prog1 before (setq before nil))))))
         queued result)
    (advice-add handler :before advice)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-hooks-run-event)
                   (lambda (_event _payload callback &rest _) (funcall callback nil))))
          (with-temp-buffer
            (setq-local niwa--session session)
            (setq default-directory
                  (niwa-workspace-root (niwa-session-workspace session)))
            (niwa-pq-test--with-view-here view
              (niwa-pipeline-run-tool (niwa-tool-get tool)
                                         (lambda (value) (setq result value)) args)
              (let ((deadline (+ (float-time) 10)))
                (while (and (not (or (niwa-pq-test--cards view) result))
                            (< (float-time) deadline))
                  (accept-process-output nil 0.02)))
              (setq queued (and (niwa-pq-test--cards view) t))
              ;; Leave the prompt unanswered, and keep nothing on the session.
              (let ((settled result))
                (niwa-permission-queue-abort-all session)
                (setq result settled)))))
      (advice-remove handler advice))
    (list :queued queued :result (format "%s" result))))

(defun test-niwa-pin--swap (directory target)
  "Return a function replacing DIRECTORY with a symlink to TARGET."
  (lambda ()
    (rename-file directory (concat directory ".real"))
    (make-symbolic-link target directory)))

(defun test-niwa-pin--patch (operation path &optional body)
  "Return a patch performing OPERATION on PATH with BODY lines."
  (format "*** Begin Patch\n*** %s File: %s\n%s*** End Patch\n"
          operation path (or body "+OWNED\n")))

(niwa-deftest niwa-pin-call-with-file ()
  ,test
  (test)
  :doc "pins a canonical file and refuses a symlinked or missing component"
  (let* ((top (file-truename (make-temp-file "niwa-pin-unit-" t)))
         (file (file-name-concat top "real/f")))
    (unwind-protect
        (progn
          (make-directory (file-name-concat top "real"))
          (write-region "CONTENT" nil file nil 'silent)
          (make-symbolic-link (file-name-concat top "real") (file-name-concat top "link"))
          (should (equal "CONTENT"
                         (niwa-pin-with-file file
                           (with-temp-buffer
                             (niwa-pin-insert-file-contents file)
                             (buffer-string)))))
          (should-error (niwa-pin-with-file (file-name-concat top "link/f") nil)
                        :type 'niwa-pin-refused)
          (should-error (niwa-pin-with-file (file-name-concat top "real/missing") nil)
                        :type 'niwa-pin-refused))
      (delete-directory top t)))
  :doc "writes create missing directories and replace a planted final link"
  (let* ((top (file-truename (make-temp-file "niwa-pin-write-" t)))
         (target (file-name-concat top "elsewhere"))
         (path (file-name-concat top "new/dir/file")))
    (unwind-protect
        (progn
          (write-region "KEEP" nil target nil 'silent)
          (niwa-pin-write-file-atomically path "ONE")
          (should (equal "ONE" (with-temp-buffer (insert-file-contents path) (buffer-string))))
          (delete-file path)
          (make-symbolic-link target path)
          (niwa-pin-write-file-atomically path "TWO")
          (should-not (file-symlink-p path))
          (should (equal "KEEP" (with-temp-buffer (insert-file-contents target) (buffer-string))))
          (niwa-pin-delete-file path)
          (should-not (file-exists-p path)))
      (delete-directory top t))))

(niwa-deftest niwa-pin-open-path ()
  ,test
  (test)
  :doc "a directory swapped for a protected link after authorization is refused"
  (test-niwa-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (session (plist-get trees :session))
            (path (file-name-concat ws "a/key")))
       ;; Negative control: the same read without the swap.
       (should (string-match-p "PLAIN" (plist-get (test-niwa-pin--run
                                                   session "Read" (list :file_path path))
                                                  :result)))
       (let ((run (test-niwa-pin--run
                   session "Read" (list :file_path path)
                   (test-niwa-pin--swap (file-name-concat ws "a") (plist-get trees :ssh)))))
         (should-not (string-match-p "DECOY" (plist-get run :result)))
         (should (string-match-p "symlink in authorized path" (plist-get run :result)))))))
  :doc "a dangling link resolved after authorization is refused"
  (test-niwa-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (session (plist-get trees :session))
            (later (file-name-concat ws "later")))
       (make-symbolic-link later (file-name-concat ws "d"))
       (let ((run (test-niwa-pin--run
                   session "Read" (list :file_path (file-name-concat ws "d/key"))
                   (lambda () (make-symbolic-link (plist-get trees :ssh) later)))))
         (should-not (string-match-p "DECOY" (plist-get run :result))))
       ;; Negative control: the target appears as a real directory.
       (delete-file later)
       (let ((run (test-niwa-pin--run
                   session "Read" (list :file_path (file-name-concat ws "d/key"))
                   (lambda ()
                     (make-directory later)
                     (write-region "LATER\n" nil (file-name-concat later "key") nil 'silent)))))
         (should (string-match-p "LATER" (plist-get run :result))))))))

(niwa-deftest niwa-pin-write-file-atomically ()
  ,test
  (test)
  :doc "an Add whose directory is swapped for a protected link writes nothing there"
  (test-niwa-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (ssh (plist-get trees :ssh))
            (session (plist-get trees :session)))
       (test-niwa-pin--run
        session "ApplyPatch"
        (list :patch (test-niwa-pin--patch "Add" (file-name-concat ws "a/new")))
        (test-niwa-pin--swap (file-name-concat ws "a") ssh))
       (should-not (file-exists-p (file-name-concat ssh "new")))
       ;; Negative control: without the swap the file lands in the workspace.
       (delete-file (file-name-concat ws "a"))
       (rename-file (file-name-concat ws "a.real") (file-name-concat ws "a"))
       (test-niwa-pin--run
        session "ApplyPatch"
        (list :patch (test-niwa-pin--patch "Add" (file-name-concat ws "a/new"))))
       (should (file-exists-p (file-name-concat ws "a/new"))))))
  :doc "an Add below a dangling link that resolves after authorization writes nothing there"
  (test-niwa-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (ssh (plist-get trees :ssh))
            (session (plist-get trees :session))
            (later (file-name-concat ws "later")))
       (make-symbolic-link later (file-name-concat ws "d"))
       (test-niwa-pin--run
        session "ApplyPatch"
        (list :patch (test-niwa-pin--patch "Add" (file-name-concat ws "d/new")))
        (lambda () (make-symbolic-link ssh later)))
       (should-not (file-exists-p (file-name-concat ssh "new")))
       ;; Negative control: nothing intervenes, so the directory is created.
       (delete-file later)
       (test-niwa-pin--run
        session "ApplyPatch"
        (list :patch (test-niwa-pin--patch "Add" (file-name-concat ws "d/new"))))
       (should (file-exists-p (file-name-concat later "new")))
       (should-not (file-symlink-p later)))))
  :doc "Edits asks before writing through a linked directory or file outside the workspace"
  (test-niwa-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (home (file-name-concat (plist-get trees :outside) "home"))
            (session (plist-get trees :session)))
       (make-directory home)
       (write-region "ORIG\n" nil (file-name-concat home "rc") nil 'silent)
       (make-symbolic-link home (file-name-concat ws "h"))
       (make-symbolic-link (file-name-concat home "rc") (file-name-concat ws "rclink"))
       ;; Only the workspace is writable without asking.
       (cl-letf (((symbol-function 'niwa--all-allowed-roots)
                  (lambda (&rest _) (list ws))))
         (should (plist-get (test-niwa-pin--run
                             session "ApplyPatch"
                             (list :patch (test-niwa-pin--patch
                                           "Add" (file-name-concat ws "h/new"))))
                            :queued))
         (should (plist-get (test-niwa-pin--run
                             session "ApplyPatch"
                             (list :patch (test-niwa-pin--patch
                                           "Update" (file-name-concat ws "rclink")
                                           "@@\n-ORIG\n+OWNED\n")))
                            :queued))
         ;; Negative control: an ordinary workspace file is written unasked.
         (should-not (plist-get (test-niwa-pin--run
                                 session "ApplyPatch"
                                 (list :patch (test-niwa-pin--patch
                                               "Add" (file-name-concat ws "plain-new"))))
                                :queued)))
       (should-not (file-exists-p (file-name-concat home "new")))
       (should (equal "ORIG\n" (with-temp-buffer
                                 (insert-file-contents (file-name-concat home "rc"))
                                 (buffer-string))))
       (should (file-exists-p (file-name-concat ws "plain-new")))))))

(defconst test-niwa-pin--swapper
  "import ctypes, os, struct, sys
watch, target = sys.argv[1], sys.argv[2]
libc = ctypes.CDLL(None, use_errno=True)
fd = libc.inotify_init()
libc.inotify_add_watch(fd, watch.encode(), 0x100)
print('WATCHING', flush=True)
while True:
    buf = os.read(fd, 65536); i = 0
    while i < len(buf):
        _, _, _, ln = struct.unpack_from('iIII', buf, i)
        name = buf[i+16:i+16+ln].rstrip(b'\\0').decode(); i += 16 + ln
        if name.startswith('.niwa-write-'):
            path = os.path.join(watch, name)
            try:
                os.unlink(path); os.symlink(target, path)
            except OSError:
                pass
"
  "Swap each new niwa temporary write file for a symlink to a decoy.")

(defun test-niwa-pin--decoy-state (decoy)
  "Return DECOY's content and mode."
  (list (with-temp-buffer (insert-file-contents decoy) (buffer-string))
        (file-modes decoy)))

(defun test-niwa-pin--patch-burst (session ws prefix count)
  "Run COUNT ApplyPatch Adds and Updates in SESSION of PREFIX files below WS/a."
  (dotimes (i count)
    (let ((added (file-name-concat ws (format "a/%s-add-%d" prefix i)))
          (updated (file-name-concat ws (format "a/%s-up-%d" prefix i))))
      (test-niwa-pin--run session "ApplyPatch"
                             (list :patch (test-niwa-pin--patch "Add" added "+INJECTED\n")))
      (write-region "LINE\n" nil updated nil 'silent)
      (test-niwa-pin--run session "ApplyPatch"
                             (list :patch (test-niwa-pin--patch
                                           "Update" updated "@@\n-LINE\n+INJECTED\n"))))))

(defun test-niwa-pin--call-with-stub-helper (source function)
  "Call FUNCTION with a fresh local helper running Python SOURCE.
FUNCTION receives the directory holding the stub."
  (let* ((directory (make-temp-file "niwa-pin-stub-" t))
         (niwa-pin--script (file-name-concat directory "stub.py"))
         (niwa-pin--helpers nil))
    (unwind-protect
        (progn
          (write-region source nil niwa-pin--script nil 'silent)
          (funcall function directory))
      (dolist (entry niwa-pin--helpers)
        (when (process-live-p (cdr entry)) (delete-process (cdr entry))))
      (delete-directory directory t))))

(niwa-deftest niwa-pin--helper ()
  ,test
  (test)
  :doc "remote bootstrap waits until the child owns stdin before sending source"
  (let ((source (with-temp-buffer
                  (insert-file-contents niwa-pin--script)
                  (buffer-string))))
    (test-niwa-pin--call-with-stub-helper
     source
     (lambda (directory)
       (let ((make (symbol-function 'make-process))
             (gate (file-name-concat directory "exec-ready"))
             (path (file-name-concat directory "input")))
         (write-region "DATA" nil path nil 'silent)
         (cl-letf (((symbol-function 'make-process)
                    (lambda (&rest arguments)
                      (let* ((default-directory directory)
                             (command (plist-get arguments :command))
                             (wrapper
                              (concat "import os,select,sys,time\n"
                                      "while not os.path.exists(sys.argv[1]): time.sleep(.001)\n"
                                      "if select.select([0],[],[],0)[0]: os.read(0,65536)\n"
                                      "os.execvp(sys.argv[2],sys.argv[2:])\n"))
                             (process
                              (apply make
                                     (plist-put
                                      (plist-put arguments :file-handler nil) :command
                                      (append (list (executable-find "python3") "-I" "-c" wrapper gate)
                                              command)))))
                        ;; Model a login shell that can read ahead until exec.
                        (run-at-time 0 nil (lambda () (write-region "go" nil gate nil 'silent)))
                        process))))
           (let ((reply (niwa-pin--request
                         "/ssh:bootstrap:" (list :op "file" :path path))))
             (should (plist-get reply :ok))
             (niwa-pin--request "/ssh:bootstrap:"
                                  (list :op "release" :pin (plist-get reply :pin)))))))))
  :doc "remote bootstrap timeout kills the unready child"
  (test-niwa-pin--call-with-stub-helper
   ""
   (lambda (directory)
     (let ((make (symbol-function 'make-process))
           (niwa-pin-timeout .1) process)
       (cl-letf (((symbol-function 'make-process)
                  (lambda (&rest arguments)
                    (let ((default-directory directory))
                      (setq process
                            (apply make
                                   (plist-put
                                    (plist-put arguments :file-handler nil) :command
                                    (list (executable-find "python3") "-I" "-c"
                                          "import time;time.sleep(30)"))))))))
         (unwind-protect
             (progn
               (should-error (niwa-pin--helper "/ssh:bootstrap:") :type 'niwa-pin-refused)
               (should-not (process-live-p process))
               (should-not niwa-pin--helpers))
           (when (process-live-p process) (delete-process process))))))))

(niwa-deftest niwa-pin--request ()
  ,test
  (test)
  :doc "a slow write succeeds past the timeout; a late reply is dropped"
  (test-niwa-pin--call-with-stub-helper
   ;; Each request is answered only after the test creates `gate-OP' beside
   ;; the stub, so no reply races the deadline and none is awaited by sleeping.
   "import json, os, sys, time
for line in sys.stdin:
    request = json.loads(line)
    gate = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'gate-' + request['op'])
    while not os.path.exists(gate):
        time.sleep(.001)
    print(json.dumps({'id': request['id'], 'ok': True}), flush=True)
"
   (lambda (directory)
     (skip-unless (executable-find "python3"))
     (let* ((niwa-pin-timeout 0.1)
            (path (file-name-concat directory "out"))
            (gate (lambda (op) (write-region "go" nil (file-name-concat directory (concat "gate-" op))
                                             nil 'silent)))
            (text "")
            ;; True once the helper's output holds the complete, newline
            ;; terminated reply for ID; the filter only parses such lines.
            (consumed (lambda (id)
                        (string-match-p (format "\"id\": %d[,}][^\n]*\n" id) text)))
            (watch (lambda (_process output) (setq text (concat text output))))
            (timer nil))
       (advice-add 'niwa-pin--filter :after watch)
       (unwind-protect
           (progn
             ;; A write waits while the helper lives: the helper stays silent
             ;; for twice the timeout, a real-clock offset the write outlasts.
             (let ((released nil) (started (float-time)))
               (setq timer (run-at-time (* 2 niwa-pin-timeout) nil
                                        (lambda () (funcall gate "write") (setq released t))))
               (should (progn (niwa-pin-write-file-atomically path "DATA") t))
               (should released)
               (should (>= (- (float-time) started) (* 2 niwa-pin-timeout))))
             ;; Other requests still give up at the deadline ...
             (should-error (niwa-pin--request nil (list :op "file" :path path))
                           :type 'niwa-pin-refused)
             ;; ... and their late reply is not kept for anyone.  Release the
             ;; helper only now, wait until the real filter has been handed that
             ;; reply's terminating newline, then check that nothing was kept.
             (let ((process (alist-get nil niwa-pin--helpers))
                   (late niwa-pin--next-id)
                   (deadline (+ (float-time) 10)))
               (should (process-live-p process))
               ;; Split-delivery control: an id seen in a partial chunk is not
               ;; consumed until its newline arrives.
               (niwa-pin--filter process "{\"id\": 9999")
               (should-not (funcall consumed 9999))
               (should (equal "{\"id\": 9999" (process-get process 'niwa-pin-pending)))
               (niwa-pin--filter process ", \"ok\": true}\n")
               (should (funcall consumed 9999))
               (should (equal "" (process-get process 'niwa-pin-pending)))
               (should-not (funcall consumed late))
               (funcall gate "file")
               (while (and (not (funcall consumed late))
                           (< (float-time) deadline))
                 (accept-process-output process 0.05))
               (should (funcall consumed late))
               (should (zerop (hash-table-count
                               (process-get process 'niwa-pin-replies))))))
         (when timer (cancel-timer timer))
         (advice-remove 'niwa-pin--filter watch)))))
  :doc "a write to a helper that exits fails promptly"
  (test-niwa-pin--call-with-stub-helper
   "import sys
sys.stdin.readline()
"
   (lambda (directory)
     (skip-unless (executable-find "python3"))
     (let ((start (float-time)))
       (should-error (niwa-pin-write-file-atomically
                      (file-name-concat directory "out") "DATA")
                     :type 'niwa-pin-refused)
       (should (< (- (float-time) start) 3))))))

(niwa-deftest niwa-pin-write-race ()
  ,test
  (test)
  :doc "a temporary write file swapped for a symlink cannot redirect Add or Update"
  (test-niwa-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (session (plist-get trees :session))
            (home (file-name-concat (plist-get trees :outside) "home"))
            (decoy (file-name-concat home "authorized_keys")))
       (make-directory home)
       (write-region "DECOY-ORIG\n" nil decoy nil 'silent)
       (set-file-modes decoy #o600)
       ;; Deterministic: plant the link the moment a temporary file appears.
       (let ((plant (lambda (name)
                      (when (string-match-p "\\.niwa-write-" name)
                        (delete-file name)
                        (make-symbolic-link decoy name))
                      name)))
         (advice-add 'make-temp-file :filter-return plant)
         (unwind-protect (test-niwa-pin--patch-burst session ws "planted" 3)
           (advice-remove 'make-temp-file plant)))
       (should (equal (list "DECOY-ORIG\n" #o600) (test-niwa-pin--decoy-state decoy)))
       ;; Concurrent: an inotify swapper outside Emacs, as a confined process could run.
       (when-let* ((python (executable-find "python3")))
         (let* ((script (file-name-concat (plist-get trees :outside) "swapper.py"))
                (output (generate-new-buffer " *pin-swapper*"))
                swapper)
           (write-region test-niwa-pin--swapper nil script nil 'silent)
           (unwind-protect
               (progn
                 (setq swapper (make-process
                                :name "pin-swapper" :buffer output :noquery t
                                :command (list python "-I" script
                                               (file-name-concat ws "a") decoy)))
                 (let ((deadline (+ (float-time) 5)))
                   (while (and (not (with-current-buffer output
                                      (string-search "WATCHING" (buffer-string))))
                               (< (float-time) deadline))
                     (accept-process-output swapper 0.05)))
                 (test-niwa-pin--patch-burst session ws "raced" 60))
             (when (process-live-p swapper) (delete-process swapper))
             (kill-buffer output)))
         (should (equal (list "DECOY-ORIG\n" #o600) (test-niwa-pin--decoy-state decoy))))
       ;; Negative control: undisturbed writes land in the workspace.
       (test-niwa-pin--patch-burst session ws "plain" 1)
       (should (equal "INJECTED\n" (with-temp-buffer
                                     (insert-file-contents (file-name-concat ws "a/plain-add-0"))
                                     (buffer-string))))
       (should (equal "INJECTED\n" (with-temp-buffer
                                     (insert-file-contents (file-name-concat ws "a/plain-up-0"))
                                     (buffer-string))))))))

(niwa-deftest niwa-pin-confined-search ()
  ,test
  (test)
  :doc "confined Grep through a directory swapped for a protected link finds no decoy"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (unless (executable-find "rg") (ert-skip "rg is unavailable"))
    (test-niwa-pin--call-with-trees
     (lambda (trees)
       (let* ((ws (plist-get trees :ws))
              (session (plist-get trees :session))
              (args (list :pattern "PLAIN|DECOY" :path (file-name-concat ws "a")
                          :output_mode "content")))
         ;; Negative control: the unswapped directory is searched.
         (should (string-match-p "PLAIN" (plist-get (test-niwa-pin--run session "Grep" args)
                                                    :result)))
         (should-not (string-match-p
                      "DECOY"
                      (plist-get (test-niwa-pin--run
                                  session "Grep" args
                                  (test-niwa-pin--swap (file-name-concat ws "a")
                                                          (plist-get trees :ssh)))
                                 :result))))))))

(niwa-deftest niwa-pin-insert-file-contents ()
  ,test
  (test)
  :doc "a work://shared read whose directory is swapped for a protected link is refused"
  (test-niwa-pin--call-with-trees
   (lambda (trees)
     (let* ((ws (plist-get trees :ws))
            (ssh (plist-get trees :ssh))
            (session (plist-get trees :session))
            (shared (file-name-concat ws ".niwa/shared/notes")))
       (make-directory shared t)
       (write-region "SHARED-PLAIN\n" nil (file-name-concat shared "key") nil 'silent)
       ;; Negative control: the unswapped note is read.
       (should (string-match-p
                "SHARED-PLAIN"
                (plist-get (test-niwa-pin--run
                            session "Read" (list :file_path "work://shared/notes/key"))
                           :result)))
       (should-not (string-match-p
                    "DECOY"
                    (plist-get (test-niwa-pin--run
                                session "Read" (list :file_path "work://shared/notes/key")
                                (test-niwa-pin--swap shared ssh))
                               :result)))))))

(provide 'test-niwa-pin)

;;; test-niwa-pin.el ends here
