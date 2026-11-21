;;; test-niwa-migrate-from-mevedel.el --- One-off mevedel -> niwa migrator -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise scripts/migrate-from-mevedel.el on temporary fixture trees only.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../scripts/migrate-from-mevedel.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

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
          (dolist (f '("trusted-hooks.el" "native/lib.so" "wake-ups/w.el"
                       "claude-agent-logs/agent.log" "agent-stderr.log" "workspace-schedules/w.el"))
            (niwa-mig-test--w src f))
          (should (niwa-migrate-from-mevedel--migrate src))
          (let ((dst (expand-file-name ".niwa" home)))
            (dolist (f '("memory/m.md" "skills/s/SKILL.md" "lobbies.el" "remote-artifacts.eld"
                         "artifact-rooms.eld" "plugins/p/x" "whiteboard-libraries/w.excalidrawlib"))
              (should (equal f (niwa-mig-test--r dst f))))
            (dolist (f '("trusted-hooks.el" "native" "wake-ups"
                         "claude-agent-logs" "agent-stderr.log" "workspace-schedules"))
              (should-not (file-exists-p (expand-file-name f dst))))
            (should (string-match-p "\\.niwa/a" (niwa-mig-test--r dst "skills-state.el")))
            (should (string-match-p "\\.niwa/x" (niwa-mig-test--r dst "permissions.el")))
            (should (= 1 (length (niwa-mig-test--backups home))))))
      (delete-directory home t))))

(defmacro niwa-mig-test--refuses (name setup)
  "Define refusal test NAME with the fixture changes in SETUP."
  `(ert-deftest ,(intern (format "niwa-migrate-from-mevedel/refuses-%s" name)) ()
     (niwa-mig-test--with-ws
      ,setup
      (let ((before (niwa-mig-test--r src "workspace-id")))
        (should-error (niwa-migrate-from-mevedel--migrate root))
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
(niwa-mig-test--refuses "inflight-cycle"
  (niwa-mig-test--w src "state/shared-notes/cycle.el" "(:id \"c\" :inflight t)\n"))
(niwa-mig-test--refuses "unretired-capture"
  (niwa-mig-test--w src "state/journal/captures/C1/ready" ""))
(niwa-mig-test--refuses "journal-pins"
  (niwa-mig-test--w src "sessions/s1/.journal-pins/p.json" "{}\n"))
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

(provide 'test-niwa-migrate-from-mevedel)
;;; test-niwa-migrate-from-mevedel.el ends here
