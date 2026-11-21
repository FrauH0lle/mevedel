;;; test-niwa-migrate-from-mevedel.el --- One-off mevedel -> niwa migrator -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise scripts/migrate-from-mevedel.el on temporary fixture trees only.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst niwa-mig-test--script
  (expand-file-name "../scripts/migrate-from-mevedel.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of the standalone script under test.")

(load niwa-mig-test--script nil t)

(defun niwa-mig-test--w (root rel &optional text)
  "Write TEXT (default REL) to REL below ROOT."
  (let ((f (expand-file-name rel root)))
    (make-directory (file-name-directory f) t)
    (with-temp-file f (insert (or text rel)))
    f))

(defun niwa-mig-test--r (root rel)
  "Return contents of REL below ROOT, or nil when missing."
  (let ((f (expand-file-name rel root)))
    (when (file-exists-p f)
      (with-temp-buffer (insert-file-contents f) (buffer-string)))))

(defconst niwa-mig-test--kept
  '(("artifacts/a1/meta.el" . "(:id \"a1\" :dedicated-session \"stale\")\n")
    ("artifacts/.state/a1/versions.el" . "  (1 2 3)\t\n\n")
    ("artifacts/.state/room/link" . "mevedel-link-bytes")
    ("state/media/m.png" . "PNG\n\r\n bytes")
    ("memory/MEMORY.md" . "- index mevedel\n")
    ("shared/note.md" . "A shared note\n")
    ("journal/digest.md" . "A published digest\n")
    ("recovery-archives/archive/evidence.el" . "old evidence")
    ("input-history.el" . "input history")
    ("plugins.el" . "plugin settings")
    ("state/plugin-data/plugin/token" . "opaque plugin state")
    ("skills/s/SKILL.md" . "---\nname: s\n---\n")
    ("hooks/h.sh" . "#!/bin/sh\n")
    ("workspace-id" . "abc123\n")
    ("lobby" . "secret") ("lobby-push-1.json" . "{}"))
  "Kept workspace files that must arrive byte-identical.")

(defconst niwa-mig-test--dropped
  '("sessions/s1/session.org" "leases/l.el" ".mevedel-pending-executions"
    ".mevedel-spool-1" ".mevedel-auth" "schedules/S/runs/old.el"
    "state/shared-notes/cycle.el" "state/journal/passes/P/prepared.el"
    "state/memory-write/receipt" "state/diagnostics/log"
    "sessions.pre-store/a/x" "sessions.pre-v0.5.12/b/x" "sessions.pre-v0.5.13/c/x")
  "Workspace state that remains only in the backup.")

(defmacro niwa-mig-test--with-ws (&rest body)
  "Run BODY with `root' bound to a fixture workspace; `src' is its .mevedel."
  `(let* ((root (make-temp-file "niwa-mig-" t))
          (src (expand-file-name ".mevedel" root)))
     (unwind-protect
         (progn
           (dolist (kv niwa-mig-test--kept)
             (niwa-mig-test--w src (car kv) (cdr kv)))
           (dolist (d niwa-mig-test--dropped) (niwa-mig-test--w src d "nil\n"))
           (niwa-mig-test--w src "schedules/S/runs/old.el" "(:outcome done)\n")
           (let ((instructions
                  "(:format 1 :files ((\"file\" :instructions ((:properties (mevedel-id \"i1\" mevedel-note-text \"mevedel stays\" mevedel-note-settings (:preset mevedel-note)))))))\n"))
             (niwa-mig-test--w src "instructions.el" instructions)
             (niwa-mig-test--w src "exports/notes.el" instructions))
           (niwa-mig-test--w src "hooks.el"
                             "((PreToolUse (:hooks ((:type command :command \"bash .mevedel/hooks/h.sh\" :description \".mevedel/stays\")))))\n")
           (niwa-mig-test--w src "hooks.json"
                             "{\"hooks\":{\"PreToolUse\":[{\"hooks\":[{\"command\":\"bash .mevedel/hooks/h.sh\",\"description\":\".mevedel/stays\"}]}]}}\n")
           (niwa-mig-test--w src "schedules/S/schedule.el"
                             "(:id \"S\" :preset mevedel-implement :prompt \"mevedel\")\n")
           (niwa-mig-test--w src "skills-state.el"
                             (format "(:disabled-keys (\"workspace:.mevedel/skills/s\" \"file:%s/.mevedel/skills/s\"))\n" root))
           (niwa-mig-test--w src "permissions.el"
                             "(:rules nil :resource-grants ((:path \"/p/.mevedel/x\" :access read)))\n")
           ,@body)
       (delete-directory root t))))

(defun niwa-mig-test--backups (root)
  "List migration backup names in ROOT."
  (directory-files root nil "\\`\\.mevedel\\.pre-niwa-[0-9]\\{8\\}-[0-9]\\{6\\}\\'"))

(ert-deftest niwa-migrate-from-mevedel/workspace ()
  (niwa-mig-test--with-ws
   (should (niwa-migrate-from-mevedel--migrate root))
   (let ((dst (expand-file-name ".niwa" root)))
     (should-not (file-exists-p src))
     (should-not (file-exists-p (expand-file-name ".niwa.staging" root)))
     (should (= 1 (length (niwa-mig-test--backups root))))
     (dolist (kv niwa-mig-test--kept)
       (should (equal (cdr kv) (niwa-mig-test--r dst (car kv)))))
     (dolist (d niwa-mig-test--dropped)
       (should-not (file-exists-p (expand-file-name d dst))))
     ;; Dropped families remain in the backup.
     (should (file-exists-p (expand-file-name
                             "sessions/s1/session.org"
                             (expand-file-name (car (niwa-mig-test--backups root)) root))))
     (let ((i (niwa-mig-test--r dst "instructions.el")))
       (should (string-match-p "niwa-id" i))
       (should-not (string-match-p "mevedel-id\\|mevedel-note-text" i))
       (should (string-match-p "niwa-note)" i))
       (should (string-match-p "mevedel stays" i))
       (should (equal i (niwa-mig-test--r dst "exports/notes.el"))))
     (dolist (hook '("hooks.el" "hooks.json"))
       (let ((text (niwa-mig-test--r dst hook)))
         (should (string-match-p (regexp-quote "bash .niwa/hooks/h.sh") text))
         (should (string-match-p (regexp-quote ".mevedel/stays") text))))
     (let ((s (niwa-mig-test--r dst "schedules/S/schedule.el")))
       (should (string-match-p ":preset niwa-implement" s))
       (should (string-match-p ":prompt \"mevedel\"" s)))
     (let ((k (niwa-mig-test--r dst "skills-state.el")))
       (should (string-match-p (regexp-quote "workspace:.niwa/skills/s") k))
       (should (string-match-p (regexp-quote (concat root "/.niwa/skills/s")) k))
       (should-not (string-match-p "\\.mevedel" k)))
     (should-not (string-match-p "\\.mevedel" (niwa-mig-test--r dst "permissions.el")))
     (should (string-match-p "\\.niwa" (niwa-mig-test--r dst "permissions.el"))))
   ;; Idempotent: nothing left to migrate.
   (should-not (niwa-migrate-from-mevedel--migrate root))
   (should (= 1 (length (niwa-mig-test--backups root))))))

(ert-deftest niwa-migrate-from-mevedel/user-directory ()
  (let* ((home (make-temp-file "niwa-mig-home-" t))
         (src (expand-file-name ".mevedel" home)))
    (unwind-protect
        (progn
          (dolist (f '("memory/m.md" "skills/s/SKILL.md" "lobbies.el" "remote-artifacts.eld"
                       "artifact-rooms.eld" "plugins/p/x" "whiteboard-libraries/w.excalidrawlib"))
            (niwa-mig-test--w src f))
          (niwa-mig-test--w src "skills-state.el" "(:disabled-keys (\"workspace:.mevedel/a\"))")
          (niwa-mig-test--w src "permissions.el"
                            "(:rules nil :resource-grants ((:path \"/h/.mevedel/x\" :access read)))")
          (niwa-mig-test--w src "workspace-schedules/w.el" "\"/work/.mevedel/project\"\n")
          (niwa-mig-test--w src "workspace-schedules/unchanged.el" "\"/work/project\"\n\n")
          (dolist (f '("trusted-hooks.el" "native/lib.so" "wake-ups/w.el"
                       "claude-agent-logs/agent.log" "agent-stderr.log"))
            (niwa-mig-test--w src f))
          (should (niwa-migrate-from-mevedel--migrate src))
          (let ((dst (expand-file-name ".niwa" home)))
            (dolist (f '("memory/m.md" "skills/s/SKILL.md" "lobbies.el" "remote-artifacts.eld"
                         "artifact-rooms.eld" "plugins/p/x" "whiteboard-libraries/w.excalidrawlib"))
              (should (equal f (niwa-mig-test--r dst f))))
            (dolist (f '("trusted-hooks.el" "native" "wake-ups"
                         "claude-agent-logs" "agent-stderr.log"))
              (should-not (file-exists-p (expand-file-name f dst))))
            (should (equal "\"/work/.niwa/project\"\n" (niwa-mig-test--r dst "workspace-schedules/w.el")))
            (should (equal "\"/work/project\"\n\n" (niwa-mig-test--r dst "workspace-schedules/unchanged.el")))
            (should (string-match-p "\\.niwa/a" (niwa-mig-test--r dst "skills-state.el")))
            (should (string-match-p "\\.niwa/x" (niwa-mig-test--r dst "permissions.el")))
            (should (= 1 (length (niwa-mig-test--backups home))))))
      (delete-directory home t))))

(defmacro niwa-mig-test--refuses (name setup &optional reason)
  "Define refusal test NAME with fixture changes SETUP and optional REASON."
  `(ert-deftest ,(intern (format "niwa-migrate-from-mevedel/refuses-%s" name)) ()
     (niwa-mig-test--with-ws
      ,setup
      (let ((before (niwa-mig-test--r src "workspace-id")))
        (let ((err (should-error (niwa-migrate-from-mevedel--migrate root))))
          ,(when reason `(should (string-search ,reason (error-message-string err)))))
        (should (equal before (niwa-mig-test--r src "workspace-id")))
        (should-not (niwa-mig-test--backups root))
        (should-not (and (not (equal ,name "collision"))
                         (file-exists-p (expand-file-name ".niwa" root))))))))

(niwa-mig-test--refuses "symlinked-root"
  (progn (rename-file src (expand-file-name "real" root))
         (make-symbolic-link "real" src)))
(niwa-mig-test--refuses "invalid-link"
  (make-symbolic-link "/etc/passwd" (expand-file-name "memory/link" src)))
(niwa-mig-test--refuses "unknown-entry"
  (niwa-mig-test--w src "mystery/x.el"))
(niwa-mig-test--refuses "collision"
  (niwa-mig-test--w root ".niwa/x"))
(niwa-mig-test--refuses "unfinished-shared-note-write"
  (niwa-mig-test--w src "state/shared-notes/pending/p.el" "(:id \"p\")\n"))
(niwa-mig-test--refuses "unfinished-memory-write"
  (niwa-mig-test--w src "state/memory-write/pending/p.el" "(:id \"p\")\n"))
(niwa-mig-test--refuses "unfinished-file-batch"
  (make-directory (file-name-concat src "state/memory-write/.file-batch-123456") t)
  "Unfinished file batch")
(niwa-mig-test--refuses "inflight-shared-note-cycle"
  (niwa-mig-test--w src "state/shared-notes/cycle.el" "(:id \"c\" :inflight t)")
  "Inflight shared-note cycle")
(niwa-mig-test--refuses "active-lease"
  (niwa-mig-test--w src "leases/artifacts/a1/00000000000000000001.el"
                    (format "(:status active :expires-at %S)\n" (+ (float-time) 3600))))
(niwa-mig-test--refuses "claimed-schedule-run"
  (niwa-mig-test--w src "schedules/S/runs/R1.el" "(:id \"R1\" :claimed t)\n"))
(niwa-mig-test--refuses "save-as-child-lease"
  (niwa-mig-test--w src "sessions/.mevedel-save-as-stage/.lease/00000000000000000001.el"
                    (format "(:status active :expires-at %S)\n" (+ (float-time) 3600))))
(niwa-mig-test--refuses "accepted-expiry"
  (progn
    (niwa-mig-test--w src "state/journal/expiry/owner.json"
                      "{\"generation\":1,\"owner\":\"owner\",\"expires-at\":1,\"entries\":[]}")
    (niwa-mig-test--w src "state/journal/mutation/00000000000000000001.outcome"
                      "{\"generation\":1,\"owner\":\"owner\",\"expires-at\":1,\"status\":\"completed\",\"payload\":\"hash\"}")))

(ert-deftest niwa-migrate-from-mevedel/settled-journal-state-allowed ()
  (niwa-mig-test--with-ws
   (niwa-mig-test--w src "state/journal/captures/C1/ready" "")
   (niwa-mig-test--w src "state/journal/captures/C1/retired" "")
   (niwa-mig-test--w src "schedules/S/runs/R2.el"
                     "(:id \"R2\" :claimed t :outcome ok :settled-at 1)\n")
   (niwa-mig-test--w src "leases/artifacts/a1/00000000000000000001.el"
                     "(:status released :expires-at 1)\n")
   (niwa-mig-test--w src "state/journal/expiry/owner.json"
                     "{\"generation\":1,\"owner\":\"owner\",\"expires-at\":1,\"entries\":[]}")
   (should (niwa-migrate-from-mevedel--migrate root))))

(ert-deftest niwa-migrate-from-mevedel/keep-published-drop-pending-journal ()
  (niwa-mig-test--with-ws
   (let ((digest "journal/2026-11-21-published.md")
         (bytes "---\nkind: digest\n---\n\nPublished mevedel history.\n")
         (ready "state/journal/captures/pending-capture/ready")
         (pin "sessions/s1/.journal-pins/pending-capture.json")
         (dst (file-name-concat root ".niwa"))
         messages backup)
     (niwa-mig-test--w src digest bytes)
     (niwa-mig-test--w src ready "")
     (niwa-mig-test--w src pin "{}")
     (niwa-mig-test--w src "state/shared-notes/cycle.el" "(:id \"c\" :inflight nil)")
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
       (setq backup (niwa-migrate-from-mevedel--migrate root)))
     (should (equal bytes (niwa-mig-test--r dst digest)))
     (should (equal bytes (niwa-mig-test--r backup digest)))
     (should (file-exists-p (file-name-concat backup ready)))
     (should (file-exists-p (file-name-concat backup pin)))
     (should-not (file-exists-p (file-name-concat dst ready)))
     (should-not (file-exists-p (file-name-concat dst pin)))
     (should (cl-some (lambda (text) (string-match-p "backup.*pending-capture" text)) messages)))))

(ert-deftest niwa-migrate-from-mevedel/stale-staging-recovers ()
  (niwa-mig-test--with-ws
   (niwa-mig-test--w root ".niwa.staging/junk" "half")
   (should (niwa-migrate-from-mevedel--migrate root))
   (should-not (file-exists-p (expand-file-name ".niwa.staging" root)))
   (should-not (file-exists-p (expand-file-name ".niwa/junk" root)))
   (should (equal "abc123\n" (niwa-mig-test--r (expand-file-name ".niwa" root) "workspace-id")))))

(ert-deftest niwa-migrate-from-mevedel/main-processes-dirs-then-exits ()
  (niwa-mig-test--with-ws
   (let (code)
     (cl-letf (((symbol-function 'kill-emacs) (lambda (&optional c) (setq code (or c 0)) (throw 'done nil))))
       (let ((command-line-args-left (list "--" root)))
         (catch 'done (niwa-migrate-from-mevedel-main))))
     (should (eq 0 code))
     (should (file-directory-p (expand-file-name ".niwa" root))))))

(ert-deftest niwa-migrate-from-mevedel/failed-backup-rolls-back ()
  (niwa-mig-test--with-ws
   (let ((rename (symbol-function 'rename-file)))
     (cl-letf (((symbol-function 'rename-file)
                (lambda (source target &optional overwrite)
                  (if (equal source src)
                      (error "Injected backup failure")
                    (funcall rename source target overwrite)))))
       (should-error (niwa-migrate-from-mevedel--migrate root))))
   (should (equal "abc123\n" (niwa-mig-test--r src "workspace-id")))
   (should-not (niwa-mig-test--backups root))
   (should-not (file-exists-p (file-name-concat root ".niwa")))
   (should-not (file-exists-p (file-name-concat root ".niwa.staging")))))

(ert-deftest niwa-migrate-from-mevedel/held-writer-locks ()
  (niwa-mig-test--with-ws
   (dolist (subdir '("state/memory-write" "state/shared-notes"))
     (let ((dir (file-name-concat src subdir)))
       (make-directory dir t)
       (with-temp-buffer
         (let ((process (make-process
                         :name "niwa-cutover-lock-test" :buffer (current-buffer) :noquery t
                         :connection-type 'pipe
                         :command (list "bash" "-c"
                                        "exec 9<\"$1\"; flock -n 9 || exit; echo locked; read -r line"
                                        "bash" dir)))
               (deadline (+ (float-time) 5)))
           (unwind-protect
               (progn
                 (while (and (zerop (buffer-size)) (process-live-p process) (< (float-time) deadline))
                   (accept-process-output process 0.1))
                 (should (equal (buffer-string) "locked\n"))
                 (should (process-live-p process))
                 (should (string-search "Active writer lock"
                                        (error-message-string
                                         (should-error (niwa-migrate-from-mevedel--migrate root)))))
                 (should (file-directory-p src))
                 (should-not (file-exists-p (file-name-concat root ".niwa"))))
             (delete-process process))))))))

(ert-deftest niwa-migrate-from-mevedel/refuses-linked-argument-or-ancestor ()
  (niwa-mig-test--with-ws
   (let ((alias (file-name-concat root "alias"))
         (real (file-name-concat root "real")))
     (make-directory real)
     (make-symbolic-link real alias)
     (dolist (path (list alias (file-name-concat alias ".mevedel") (concat alias "/..")))
       (should (string-search "Refusing symbolic link"
                              (error-message-string
                               (should-error (niwa-migrate-from-mevedel--migrate path))))))
     (delete-file alias))
   (should (file-directory-p src))
   (should-not (file-exists-p (file-name-concat root ".niwa")))
   (should-not (niwa-mig-test--backups root))))

(ert-deftest niwa-migrate-from-mevedel/resumes-after-promotion ()
  (niwa-mig-test--with-ws
   (let ((emacs (file-name-concat invocation-directory invocation-name))
         (dst (file-name-concat root ".niwa"))
         (form `(let ((rename (symbol-function 'rename-file)))
                  (cl-letf (((symbol-function 'rename-file)
                             (lambda (from to &optional overwrite)
                               (when (equal from ,src) (kill-emacs 70))
                               (funcall rename from to overwrite))))
                    (niwa-migrate-from-mevedel--migrate ,root)))))
     (with-temp-buffer
       (should (= 70 (call-process emacs nil t nil "-Q" "--batch" "-l" niwa-mig-test--script
                                   "--eval" (prin1-to-string form)))))
     (should (file-directory-p src))
     (should (file-regular-p (file-name-concat dst ".cutover-ready")))
     (should-not (niwa-mig-test--backups root))
     (let ((inode (file-attribute-inode-number (file-attributes dst)))
           (backup (niwa-migrate-from-mevedel--migrate root)))
       (should (equal inode (file-attribute-inode-number (file-attributes dst))))
       (should (equal "abc123\n" (niwa-mig-test--r backup "workspace-id"))))
     (should-not (file-exists-p (file-name-concat dst ".cutover-ready")))
     (should-not (file-exists-p src))
     (should-not (niwa-migrate-from-mevedel--migrate root)))))

(ert-deftest niwa-migrate-from-mevedel/rewrites-only-kept-paths ()
  (niwa-mig-test--with-ws
   (niwa-mig-test--w src "hooks/spaced hook.sh" "kept")
   (with-temp-file (file-name-concat src "instructions.el")
     (prin1 '(:files ((".mevedel/hooks/spaced hook.sh" :instructions nil)
                      (".mevedel/sessions/s1/session.org" :instructions nil)
                      (".mevedel/sample" :instructions nil))) (current-buffer)))
   (let* ((command "bash '.mevedel/hooks/spaced hook.sh'; printf '.mevedel/sample'; cat .mevedel/sessions/s1/session.org")
          (expected (string-replace ".mevedel/hooks/" ".niwa/hooks/" command))
          (dst (file-name-concat root ".niwa")))
     (with-temp-file (file-name-concat src "hooks.el")
       (prin1 `((PreToolUse (:hooks ((:type command :command ,command))))) (current-buffer)))
     (niwa-mig-test--w src "hooks.json"
                       (json-serialize `(:hooks (:PreToolUse [(:hooks [(:command ,command)])]))))
     (should (niwa-migrate-from-mevedel--migrate root))
     (should (equal '(".niwa/hooks/spaced hook.sh" ".mevedel/sessions/s1/session.org" ".mevedel/sample")
                    (mapcar #'car (plist-get (read (niwa-mig-test--r dst "instructions.el")) :files))))
     (dolist (file '("hooks.el" "hooks.json"))
       (should (string-search expected (niwa-mig-test--r dst file)))))))

(ert-deftest niwa-migrate-from-mevedel/preserves-config-bytes ()
  (niwa-mig-test--with-ws
   (let* ((json (concat "{\r\n  \"metadata\": {\"command\": \".mevedel/hooks/h.sh\"},\r\n"
                        "  \"hooks\": {\"PreToolUse\": [{ \"hooks\": [{\r\n"
                        "    \"description\": \"café: .mevedel/hooks/h.sh\",\r\n"
                        "    \"command\" : \".\\u006devedel\\/hooks/h.sh; .mevedel/hooks/h.sh;"
                        " printf '.mevedel/sample'\",\r\n    \"timeout\": 1e0\r\n  }]}]}\r\n}\r\n\t"))
          (lisp (concat ";; .mevedel/hooks/h.sh\n((PreToolUse\n"
                        "  (:hooks ((:type command :description \".mevedel/hooks/h.sh\"\n"
                        "            :command \"bash \\\".mevedel/hooks/h.sh\\\"\")))))\n\n"))
          (instructions
           (concat ";; mevedel-id and .mevedel/hooks/h.sh stay here\n(:format  1\n"
                   " :files ((\".mevedel/hooks/h.sh\" . (:instructions\n"
                   "          ((:properties (mevedel-id \".mevedel/hooks/h.sh\"\n"
                   "                         mevedel-note-settings (:preset mevedel-note))))))))  "))
          (entries `(("hooks.json" ,json
                      ,(string-replace ".\\u006devedel\\/hooks/h.sh; .mevedel/hooks/h.sh"
                                       ".niwa\\/hooks/h.sh; .niwa/hooks/h.sh" json))
                     ("hooks.el" ,lisp
                      ,(string-replace "bash \\\".mevedel/" "bash \\\".niwa/" lisp))
                     ("instructions.el" ,instructions
                      ,(string-replace "mevedel-note-settings (:preset mevedel-note)"
                                       "niwa-note-settings (:preset niwa-note)"
                                       (string-replace "(mevedel-id " "(niwa-id "
                                                       (string-replace "((\".mevedel/" "((\".niwa/" instructions))))))
          (dst (file-name-concat root ".niwa"))
          (coding-system-for-write 'utf-8-unix))
     (dolist (entry entries)
       (niwa-mig-test--w src (car entry) (cadr entry)))
     (should (niwa-migrate-from-mevedel--migrate root))
     (dolist (entry entries)
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally (file-name-concat dst (car entry)))
         (should (equal (buffer-string) (encode-coding-string (nth 2 entry) 'utf-8-unix))))))))

(provide 'test-niwa-migrate-from-mevedel)
;;; test-niwa-migrate-from-mevedel.el ends here
