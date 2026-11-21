;;; test-niwa-memory-list-loading.el --- Table loading presentation -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise native table transitions, not just header or internal loading flags.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'memory-history-fixtures
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "memory-history-fixtures"))
(require 'niwa-memory-list)
(require 'niwa-memory-library)
(require 'niwa-memory-list-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "niwa-memory-list-test-support"))

(niwa-deftest niwa-memory-list--update-notice
    (:vars* ((root (make-temp-file "niwa-memory-loading-" t))
	     (memory (file-name-concat root "memory"))
	     (workspace (niwa-workspace--create :root root :type 'project))
	     (context (niwa-cockpit-workspace-context workspace))
	     (niwa-memory-dirs (list memory))
	     (niwa-memory-decision--inhibit-recovery nil))
	    :after-each ((when (get-buffer "*niwa memory*") (kill-buffer "*niwa memory*"))
			 (niwa-memory-decision-stop-recovery)
			 (delete-directory root t)))
  (save-window-excursion
    (make-directory memory)
    (niwa-workspace-identity-ensure root)
    ,test)
  (test)
  :doc "recovery and scan notices are display-only; ready Memories and completed rows stay usable"
  (progn
    (write-region "Current topic.\n" nil (file-name-concat memory "topic.md") nil 'silent)
    (niwa-test-memory-history-create workspace 1 2 512)
    (niwa-test-memory-history-create workspace 1 2 512 t)
    ;; An earlier topic observation lets Memories show rows at once.
    (niwa-memory-library-list workspace)
    (niwa-memory-list-open context)
    (with-current-buffer "*niwa memory*"
      ;; Without counts, Memories shows at once while history chooses the view.
      (should (eq 'memories niwa-memory-list--view))
      (should niwa-memory-list--job)
      (should (= 1 (length tabulated-list-entries)))
      (niwa-memory-list-candidates)
      (should (string-search "Loading history" (overlay-get niwa-memory-list--notice 'before-string)))
      (should (= (point-min) (overlay-start niwa-memory-list--notice)))
      (should (string-empty-p (buffer-string)))
      (should-not tabulated-list-entries)
      (should-not (tabulated-list-get-id))
      (should-not (niwa-cockpit-surface-selected t))
      (should-error (niwa-cockpit-surface-details) :type 'user-error)
      (with-timeout (45 (ert-fail "Recovery did not admit history scan"))
	(while niwa-memory-list--recovery (accept-process-output nil .01)))
      ;; Topics may already be current while history still runs; restart both
      ;; observations so the remaining checks see their loading state.
      (niwa-memory-list-refresh)
      (should niwa-memory-list--job)
      (should (string-search "Loading history" (overlay-get niwa-memory-list--notice 'before-string)))
      (niwa-memory-list-history)
      (should (string-search "Loading history" (overlay-get niwa-memory-list--notice 'before-string)))
      (niwa-memory-list-memories)
      (should (= 1 (length tabulated-list-entries)))
      (should-not niwa-memory-list--notice)
      (should (niwa-cockpit-surface-selected))
      (should (string-search "refreshing memories" (niwa-memory-list--header nil context)))
      (niwa-memory-list-candidates)
      (should (string-search "Loading history" (overlay-get niwa-memory-list--notice 'before-string)))
      (niwa-test-memory-list-await)
      (should (= 2 (length tabulated-list-entries)))
      (should-not niwa-memory-list--notice)
      (should (tabulated-list-get-id))
      (niwa-memory-list-history)
      (should (= 2 (length tabulated-list-entries)))
      (should-not niwa-memory-list--notice)))
  :doc "completed empty views say empty rather than loading and never create an actionable row"
  (progn
    (niwa-memory-list-open context)
    (with-current-buffer "*niwa memory*"
      (niwa-test-memory-list-await)
      (dolist (view '(candidates history memories))
	(niwa-memory-list-switch view)
	(when (eq view 'memories) (niwa-test-memory-list-await-memories))
	(let ((notice (overlay-get niwa-memory-list--notice 'before-string)))
	  (should (string-search (pcase view ('candidates "No pending memory candidates")
					('history "No retained memory history")
					('memories "No stored memories")) notice))
	  (should-not (string-search "Loading" notice)))
	(should (string-empty-p (buffer-string)))
	(should-not tabulated-list-entries)
	(should-not (tabulated-list-get-id))
	(should (= 0 (hash-table-count niwa-cockpit--row-items)))
	(should-error (niwa-cockpit-surface-selected) :type 'user-error))))
  :doc "scan failure is retryable in the table, independent empty Memories stays complete, and mode close removes notice"
  (progn
    (niwa-test-memory-history-create workspace 1 1 512 t)
    (niwa-memory-list-open context)
    (with-current-buffer "*niwa memory*"
      (with-timeout (45 (ert-fail "Recovery did not admit history scan"))
	(while niwa-memory-list--recovery (accept-process-output nil .01)))
      ;; Candidates collects history even once counts are known.
      (niwa-memory-list-candidates)
      (unless niwa-memory-list--job (niwa-memory-list-refresh))
      (niwa-memory-list--received (current-buffer) niwa-memory-list--job '(:error "Injected scan failure"))
      (should (string-search "Could not load memory data" (overlay-get niwa-memory-list--notice 'before-string)))
      (should (string-search "g to retry" (overlay-get niwa-memory-list--notice 'before-string)))
      (should-not (niwa-cockpit-surface-selected t))
      (niwa-memory-list-memories)
      ;; Memories may already have completed while history recovery was pending.
      (niwa-test-memory-list-await-memories)
      (should (string-search "No stored memories" (overlay-get niwa-memory-list--notice 'before-string)))
      (niwa-memory-list-candidates)
      (call-interactively (key-binding (kbd "g")))
      (should (string-search "Loading history" (overlay-get niwa-memory-list--notice 'before-string)))
      (niwa-test-memory-list-await)
      (should (= 1 (length tabulated-list-entries)))
      (should-not niwa-memory-list--notice)
      (niwa-memory-list-refresh)
      (let ((notice niwa-memory-list--notice))
	(fundamental-mode)
	(should-not (overlay-buffer notice)))))
  :doc "recovery timeout retains the independent scan and rows; unavailable owner and native quit remain visible"
  (progn
    (niwa-test-memory-history-create workspace 1 1 512 t)
    (niwa-memory-list-open context)
    (with-current-buffer "*niwa memory*"
      (niwa-memory-list-candidates)
      (let ((job niwa-memory-list--job)
            (watcher niwa-memory-list--recovery))
        (should job)
        (should (processp (plist-get job :worker)))
        (plist-put watcher :expires 0)
        (niwa-memory-list--observe-recovery (current-buffer) watcher)
        (should (eq job niwa-memory-list--job))
        (should-not niwa-memory-list--recovery)
        (should-not (memq (plist-get watcher :timer) timer-list)))
      (should-not niwa-memory-list--error)
      (should (string-search "Recovery observation timed out" (niwa-memory-list--header nil context)))
      (niwa-test-memory-list-await)
      (should (= 1 (length tabulated-list-entries)))
      (should (niwa-cockpit-surface-selected))
      (should (string-search "g refreshes data" (niwa-memory-list--header nil context)))
      (niwa-memory-list--unavailable "Memory display owner is unavailable; reopen the cockpit")
      (should (string-search "owner is unavailable" (overlay-get niwa-memory-list--notice 'before-string)))
      (let ((notice niwa-memory-list--notice))
        (call-interactively (key-binding (kbd "q")))
        (should-not (overlay-buffer notice))))))

(niwa-deftest niwa-memory-list--visible
    (:vars* ((root (niwa-test-memory-history-root 'memory-list--visible))
	     (memory (file-name-concat root "memory"))
	     (workspace (niwa-workspace--create :root root :type 'project))
	     (context (niwa-cockpit-workspace-context workspace))
	     (niwa-memory-dirs (list memory))
	     (niwa-memory-decision--inhibit-recovery nil))
	    :after-each ((when (get-buffer "*niwa memory*") (kill-buffer "*niwa memory*"))
			 (niwa-memory-decision-stop-recovery)
			 (delete-directory root t)))
  (save-window-excursion
    (niwa-workspace-identity-ensure root)
    (niwa-test-memory-history-build
     'memory-list--visible root
     (lambda ()
       (make-directory memory)
       (write-region "Current topic.\n" nil (file-name-concat memory "topic.md") nil 'silent)
       (niwa-test-memory-history-create workspace 1 1 512 t)))
    ,test)
  (test)
  :doc "a prior observation without alerts opens Memories at once without collecting history"
  (progn
    (setf (niwa-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (niwa-memory-list-open context)
    (with-current-buffer "*niwa memory*"
      (should (eq 'memories niwa-memory-list--view))
      (should-not niwa-memory-list--job)
      ;; Topics load after display; history is never collected for them.
      (should (string-search "Loading memories" (overlay-get niwa-memory-list--notice 'before-string)))
      (niwa-test-memory-list-await-memories)
      (should (= 1 (length tabulated-list-entries)))
      (should-not niwa-memory-list--notice)
      (should-not niwa-memory-list--job)
      (let ((header (niwa-memory-list--header nil context)))
        (should (string-search "History not loaded" header))
        (should (string-search "0 pending (cached)" header)))
      ;; Refresh and recovery completion keep Memories history-free.
      (niwa-test-memory-list-await)
      (niwa-memory-list-refresh)
      (should-not niwa-memory-list--job)
      (should (= 1 (length tabulated-list-entries)))
      ;; Proposal views collect history on demand.
      (niwa-memory-list-candidates)
      (should niwa-memory-list--job)
      (should (string-search "Loading history" (overlay-get niwa-memory-list--notice 'before-string)))
      (niwa-test-memory-list-await)
      (should (eq 'candidates niwa-memory-list--view))
      (should (= 1 (plist-get (niwa-workspace-memory-observation workspace) :pending)))
      (should (string-search "1 pending |" (niwa-memory-list--header nil context)))
      (should (= 1 (length tabulated-list-entries)))))
  :doc "earlier topic rows show at once marked refreshing, and refresh in place, including on g"
  (progn
    (niwa-memory-library-list workspace)
    (write-region "Second topic.\n" nil (file-name-concat memory "second.md") nil 'silent)
    (setf (niwa-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (niwa-memory-list-open context)
    (with-current-buffer "*niwa memory*"
      (should (equal '("topic.md") (mapcar (lambda (row) (plist-get row :file)) (niwa-cockpit-surface-items))))
      (should (string-search "refreshing memories" (niwa-memory-list--header nil context)))
      (niwa-test-memory-list-await-memories)
      (should (equal '("second.md" "topic.md") (mapcar (lambda (row) (plist-get row :file)) (niwa-cockpit-surface-items))))
      (should-not (string-search "refreshing memories" (niwa-memory-list--header nil context)))
      (goto-char (point-max)) (forward-line -1)
      (let ((selected (tabulated-list-get-id)))
        (delete-file (file-name-concat memory "second.md"))
        (niwa-memory-list-refresh)
        (should (= 2 (length tabulated-list-entries)))
        (should (equal selected (tabulated-list-get-id)))
        (should (string-search "refreshing memories" (niwa-memory-list--header nil context)))
        (niwa-test-memory-list-await-memories)
        (should (equal '("topic.md") (mapcar (lambda (row) (plist-get row :file)) (niwa-cockpit-surface-items)))))))
  :doc "a late topic observation for a closed or refreshed table is ignored"
  (progn
    (setf (niwa-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (niwa-memory-list-open context)
    (with-current-buffer "*niwa memory*"
      (let ((old niwa-memory-list--memories-job))
        (niwa-memory-list-refresh)
        (should-not (eq old niwa-memory-list--memories-job))
        (should-not (memq (plist-get old :timer) timer-list))
        (let ((current niwa-memory-list--memories-job))
          (kill-buffer)
          (should-not (memq (plist-get current :timer) timer-list))))))
  :doc "a memory configuration change during observation restarts it for the new roots"
  (let ((other (file-name-concat root "other"))
        (refresh (symbol-function 'niwa-memory-library-refresh)) (observations 0))
    (make-directory other)
    (write-region "Other topic.\n" nil (file-name-concat other "other.md") nil 'silent)
    (setf (niwa-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (cl-letf (((symbol-function 'niwa-memory-library-refresh)
               (lambda (&rest args) (cl-incf observations) (apply refresh args))))
      (niwa-memory-list-open context)
      (with-current-buffer "*niwa memory*"
        (setq niwa-memory-dirs (list other))
        (with-timeout (45 (ert-fail "Reconfigured topics were not observed"))
          (while (not (eq 'current niwa-memory-list--memories-state)) (accept-process-output nil .01)))
        (should (= 2 observations))
        (should (equal '("other.md") (mapcar (lambda (row) (plist-get row :file)) (niwa-cockpit-surface-items))))
        (should-not (string-search "refreshing memories" (niwa-memory-list--header nil context))))))
  :doc "an unexpected inventory failure becomes a visible unavailable row, not a stuck refresh"
  (progn
    (setf (niwa-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (cl-letf (((symbol-function 'niwa-memory-library-refresh)
               (lambda (&rest _) (error "Injected inventory failure"))))
      (niwa-memory-list-open context)
      (with-current-buffer "*niwa memory*"
        (niwa-test-memory-list-await-memories)
        (let ((row (car (niwa-cockpit-surface-items))))
          (should (eq 'unavailable (plist-get row :status)))
          (should (equal "Injected inventory failure" (plist-get row :error))))
        (should-not (string-search "refreshing memories" (niwa-memory-list--header nil context))))))
  :doc "observed alerts open Candidates, which wait for the history scan"
  (progn
    (setf (niwa-workspace-memory-observation workspace) (list :at (float-time) :pending 1 :unavailable 0))
    (niwa-memory-list-open context)
    (with-current-buffer "*niwa memory*"
      (should niwa-memory-list--job)
      (should (eq 'candidates niwa-memory-list--view))
      (should (string-search "Loading history" (overlay-get niwa-memory-list--notice 'before-string)))
      (niwa-test-memory-list-await)
      (should (= 1 (length tabulated-list-entries)))))
  :doc "without an observation Memories shows at once; found candidates are signalled on their tab, never switched to"
  (progn
    (should-not (niwa-workspace-memory-observation workspace))
    (niwa-memory-list-open context)
    (with-current-buffer "*niwa memory*"
      (should (eq 'memories niwa-memory-list--view))
      (should niwa-memory-list--job)
      (let ((header (niwa-memory-list--header nil context)))
        (should (string-search "proposals not checked" header))
        (should (string-prefix-p "Candidates | Memories" header)))
      (niwa-test-memory-list-await)
      (should (eq 'memories niwa-memory-list--view))
      (should (= 1 (length tabulated-list-entries)))
      (let* ((header (niwa-memory-list--header nil context))
             (count (string-search "(1)" header)))
        (should (string-prefix-p "Candidates (1) | Memories" header))
        (should (eq 'warning (get-text-property count 'face header)))
        (should (commandp (lookup-key (get-text-property count 'local-map header) [tab-line mouse-1]))))
      ;; Known counts keep Memories history-free until a proposal view is shown.
      (niwa-memory-list-refresh)
      (should-not niwa-memory-list--job)
      (niwa-memory-list-candidates)
      (should niwa-memory-list--job)
      (niwa-test-memory-list-await)
      (should (= 1 (length tabulated-list-entries))))))

(provide 'test-niwa-memory-list-loading)
;;; test-niwa-memory-list-loading.el ends here
