;;; local-host.el --- Isolated host for local latency runs -*- lexical-binding: t; -*-

;;; Commentary:

;; Started by local.sh in batch Emacs with a throwaway HOME.  Shares one
;; session of a temporary workspace through the local relay in
;; NIWA_BENCH_RELAY, writes its links to links.json, and serves the
;; host-trace report: touching `report' writes report.eld, touching
;; `stop' exits.

;;; Code:

(require 'niwa)
(require 'niwa-chat)
(require 'niwa-tools)
(require 'niwa-collaboration)
(require 'niwa-tool-editing)
(require 'niwa-shared-editing)
(require 'niwa-view)
(load (expand-file-name "host-trace.el" (file-name-directory load-file-name)) nil t)

(niwa-tools-register)
(niwa--define-presets)
;; Sessions record their backend; this one is never called.
(require 'gptel-openai)
(setq gptel-backend (gptel-make-openai "bench" :key "unused" :models '(bench))
      gptel-model 'bench)

(defvar bench-root (getenv "NIWA_BENCH_ROOT"))
(defvar bench-workspace (file-name-concat bench-root "workspace/"))
(make-directory bench-workspace t)
(defvar bench-session
  (niwa-session-create
   "bench" (niwa-workspace--create :type 'project :root bench-workspace)
   bench-workspace))
(puthash (niwa-execution-target-identity (niwa-session-execution-target bench-session))
         t niwa-session-durability--disclosed-targets)
(setq niwa-collaboration-relay-url (getenv "NIWA_BENCH_RELAY"))
(defvar bench-buffer (get-buffer-create " *latency bench*"))
(with-current-buffer bench-buffer
  (niwa-chat-prepare-transcript-buffer)
  (setq-local niwa--session bench-session)
  (setq default-directory bench-workspace)
  (niwa-session-set-root-buffer bench-session bench-buffer)
  (niwa-session-set-pending-input-paused bench-session t)
  (niwa-view--ensure bench-buffer)
  (niwa-tool-editing--register)
  (let ((room (niwa-collaboration--start bench-session bench-buffer)))
    (write-region (json-encode (list :full (plist-get room :link-full)))
                  nil (file-name-concat bench-root "links.json") nil 'silent)))
(host-trace-start)

(let ((report (file-name-concat bench-root "report"))
      (stop (file-name-concat bench-root "stop")))
  (while (not (file-exists-p stop))
    (accept-process-output nil 0.01)
    (when (file-exists-p report)
      (delete-file report)
      (with-temp-file (file-name-concat bench-root "report.eld")
        (pp (host-trace-report) (current-buffer)))
      (host-trace-reset))))

;;; local-host.el ends here
