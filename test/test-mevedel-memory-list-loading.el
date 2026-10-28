;;; test-mevedel-memory-list-loading.el --- Table loading presentation -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise native table transitions, not just header or internal loading flags.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'memory-history-fixtures
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "memory-history-fixtures"))
(require 'mevedel-memory-list)
(require 'mevedel-memory-library)
(require 'mevedel-memory-list-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name)) "mevedel-memory-list-test-support"))

(mevedel-deftest mevedel-memory-list--update-notice
    (:vars* ((root (make-temp-file "mevedel-memory-loading-" t))
	     (memory (file-name-concat root "memory"))
	     (workspace (mevedel-workspace--create :root root :type 'project))
	     (context (mevedel-cockpit-workspace-context workspace))
	     (mevedel-memory-dirs (list memory))
	     (mevedel-memory-decision--inhibit-recovery nil))
	    :after-each ((when (get-buffer "*mevedel memory*") (kill-buffer "*mevedel memory*"))
			 (mevedel-memory-decision-stop-recovery)
			 (delete-directory root t)))
  (save-window-excursion
    (make-directory memory)
    (mevedel-workspace-identity-ensure root)
    ,test)
  (test)
  :doc "recovery and scan notices are display-only; ready Memories and completed rows stay usable"
  (progn
    (write-region "Current topic.\n" nil (file-name-concat memory "topic.md") nil 'silent)
    (mevedel-test-memory-history-create workspace 1 2 512)
    (mevedel-test-memory-history-create workspace 1 2 512 t)
    ;; An earlier topic observation lets Memories show rows at once.
    (mevedel-memory-library-list workspace)
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      ;; Without counts, Memories shows at once while history chooses the view.
      (should (eq 'memories mevedel-memory-list--view))
      (should mevedel-memory-list--job)
      (should (= 1 (length tabulated-list-entries)))
      (mevedel-memory-list-candidates)
      (should (string-search "Loading history" (overlay-get mevedel-memory-list--notice 'before-string)))
      (should (= (point-min) (overlay-start mevedel-memory-list--notice)))
      (should (string-empty-p (buffer-string)))
      (should-not tabulated-list-entries)
      (should-not (tabulated-list-get-id))
      (should-not (mevedel-cockpit-surface-selected t))
      (should-error (mevedel-cockpit-surface-details) :type 'user-error)
      (with-timeout (45 (ert-fail "Recovery did not admit history scan"))
	(while mevedel-memory-list--recovery (accept-process-output nil .01)))
      ;; A no-op recovery keeps its scan, which may already be done; refresh
      ;; so the remaining checks observe a loading scan deterministically.
      (unless mevedel-memory-list--job (mevedel-memory-list-refresh))
      (should mevedel-memory-list--job)
      (should (string-search "Loading history" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-memory-list-history)
      (should (string-search "Loading history" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-memory-list-memories)
      (should (= 1 (length tabulated-list-entries)))
      (should-not mevedel-memory-list--notice)
      (should (mevedel-cockpit-surface-selected))
      (should (string-search "refreshing memories" (mevedel-memory-list--header nil context)))
      (mevedel-memory-list-candidates)
      (should (string-search "Loading history" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-test-memory-list-await)
      (should (= 2 (length tabulated-list-entries)))
      (should-not mevedel-memory-list--notice)
      (should (tabulated-list-get-id))
      (mevedel-memory-list-history)
      (should (= 2 (length tabulated-list-entries)))
      (should-not mevedel-memory-list--notice)))
  :doc "completed empty views say empty rather than loading and never create an actionable row"
  (progn
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (mevedel-test-memory-list-await)
      (dolist (view '(candidates history memories))
	(mevedel-memory-list-switch view)
	(when (eq view 'memories) (mevedel-test-memory-list-await-memories))
	(let ((notice (overlay-get mevedel-memory-list--notice 'before-string)))
	  (should (string-search (pcase view ('candidates "No pending memory candidates")
					('history "No retained memory history")
					('memories "No stored memories")) notice))
	  (should-not (string-search "Loading" notice)))
	(should (string-empty-p (buffer-string)))
	(should-not tabulated-list-entries)
	(should-not (tabulated-list-get-id))
	(should (= 0 (hash-table-count mevedel-cockpit--row-items)))
	(should-error (mevedel-cockpit-surface-selected) :type 'user-error))))
  :doc "scan failure is retryable in the table, independent empty Memories stays complete, and mode close removes notice"
  (progn
    (mevedel-test-memory-history-create workspace 1 1 512 t)
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (with-timeout (45 (ert-fail "Recovery did not admit history scan"))
	(while mevedel-memory-list--recovery (accept-process-output nil .01)))
      ;; Candidates collects history even once counts are known.
      (mevedel-memory-list-candidates)
      (unless mevedel-memory-list--job (mevedel-memory-list-refresh))
      (mevedel-memory-list--received (current-buffer) mevedel-memory-list--job '(:error "Injected scan failure"))
      (should (string-search "Could not load memory data" (overlay-get mevedel-memory-list--notice 'before-string)))
      (should (string-search "g to retry" (overlay-get mevedel-memory-list--notice 'before-string)))
      (should-not (mevedel-cockpit-surface-selected t))
      (mevedel-memory-list-memories)
      (should (string-search "Loading memories" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-test-memory-list-await-memories)
      (should (string-search "No stored memories" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-memory-list-candidates)
      (call-interactively (key-binding (kbd "g")))
      (should (string-search "Loading history" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-test-memory-list-await)
      (should (= 1 (length tabulated-list-entries)))
      (should-not mevedel-memory-list--notice)
      (mevedel-memory-list-refresh)
      (let ((notice mevedel-memory-list--notice))
	(fundamental-mode)
	(should-not (overlay-buffer notice)))))
  :doc "recovery timeout retains the independent scan and rows; unavailable owner and native quit remain visible"
  (progn
    (mevedel-test-memory-history-create workspace 1 1 512 t)
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (mevedel-memory-list-candidates)
      (let ((job mevedel-memory-list--job)
            (watcher mevedel-memory-list--recovery))
        (should job)
        (should (processp (plist-get job :worker)))
        (plist-put watcher :expires 0)
        (mevedel-memory-list--observe-recovery (current-buffer) watcher)
        (should (eq job mevedel-memory-list--job))
        (should-not mevedel-memory-list--recovery)
        (should-not (memq (plist-get watcher :timer) timer-list)))
      (should-not mevedel-memory-list--error)
      (should (string-search "Recovery observation timed out" (mevedel-memory-list--header nil context)))
      (mevedel-test-memory-list-await)
      (should (= 1 (length tabulated-list-entries)))
      (should (mevedel-cockpit-surface-selected))
      (should (string-search "g refreshes data" (mevedel-memory-list--header nil context)))
      (mevedel-memory-list--unavailable "Memory display owner is unavailable; reopen the cockpit")
      (should (string-search "owner is unavailable" (overlay-get mevedel-memory-list--notice 'before-string)))
      (let ((notice mevedel-memory-list--notice))
        (call-interactively (key-binding (kbd "q")))
        (should-not (overlay-buffer notice))))))

(mevedel-deftest mevedel-memory-list--visible
    (:vars* ((root (make-temp-file "mevedel-memory-visible-" t))
	     (memory (file-name-concat root "memory"))
	     (workspace (mevedel-workspace--create :root root :type 'project))
	     (context (mevedel-cockpit-workspace-context workspace))
	     (mevedel-memory-dirs (list memory))
	     (mevedel-memory-decision--inhibit-recovery nil))
	    :after-each ((when (get-buffer "*mevedel memory*") (kill-buffer "*mevedel memory*"))
			 (mevedel-memory-decision-stop-recovery)
			 (delete-directory root t)))
  (save-window-excursion
    (make-directory memory)
    (mevedel-workspace-identity-ensure root)
    (write-region "Current topic.\n" nil (file-name-concat memory "topic.md") nil 'silent)
    (mevedel-test-memory-history-create workspace 1 1 512 t)
    ,test)
  (test)
  :doc "a prior observation without alerts opens Memories at once without collecting history"
  (progn
    (setf (mevedel-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (should (eq 'memories mevedel-memory-list--view))
      (should-not mevedel-memory-list--job)
      ;; Topics load after display; history is never collected for them.
      (should (string-search "Loading memories" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-test-memory-list-await-memories)
      (should (= 1 (length tabulated-list-entries)))
      (should-not mevedel-memory-list--notice)
      (should-not mevedel-memory-list--job)
      (let ((header (mevedel-memory-list--header nil context)))
        (should (string-search "History not loaded" header))
        (should (string-search "0 pending | 0 recovery (cached)" header)))
      ;; Refresh and recovery completion keep Memories history-free.
      (mevedel-test-memory-list-await)
      (mevedel-memory-list-refresh)
      (should-not mevedel-memory-list--job)
      (should (= 1 (length tabulated-list-entries)))
      ;; Proposal views collect history on demand.
      (mevedel-memory-list-candidates)
      (should mevedel-memory-list--job)
      (should (string-search "Loading history" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-test-memory-list-await)
      (should (eq 'candidates mevedel-memory-list--view))
      (should (= 1 (plist-get (mevedel-workspace-memory-observation workspace) :pending)))
      (should (string-search "1 pending | 0 recovery |" (mevedel-memory-list--header nil context)))
      (should (= 1 (length tabulated-list-entries)))))
  :doc "earlier topic rows show at once marked refreshing, and refresh in place, including on g"
  (progn
    (mevedel-memory-library-list workspace)
    (write-region "Second topic.\n" nil (file-name-concat memory "second.md") nil 'silent)
    (setf (mevedel-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (should (equal '("topic.md") (mapcar (lambda (row) (plist-get row :file)) (mevedel-cockpit-surface-items))))
      (should (string-search "refreshing memories" (mevedel-memory-list--header nil context)))
      (mevedel-test-memory-list-await-memories)
      (should (equal '("second.md" "topic.md") (mapcar (lambda (row) (plist-get row :file)) (mevedel-cockpit-surface-items))))
      (should-not (string-search "refreshing memories" (mevedel-memory-list--header nil context)))
      (goto-char (point-max)) (forward-line -1)
      (let ((selected (tabulated-list-get-id)))
        (delete-file (file-name-concat memory "second.md"))
        (mevedel-memory-list-refresh)
        (should (= 2 (length tabulated-list-entries)))
        (should (equal selected (tabulated-list-get-id)))
        (should (string-search "refreshing memories" (mevedel-memory-list--header nil context)))
        (mevedel-test-memory-list-await-memories)
        (should (equal '("topic.md") (mapcar (lambda (row) (plist-get row :file)) (mevedel-cockpit-surface-items)))))))
  :doc "a late topic observation for a closed or refreshed table is ignored"
  (progn
    (setf (mevedel-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (let ((old mevedel-memory-list--memories-job))
        (mevedel-memory-list-refresh)
        (should-not (eq old mevedel-memory-list--memories-job))
        (should-not (memq (plist-get old :timer) timer-list))
        (let ((current mevedel-memory-list--memories-job))
          (kill-buffer)
          (should-not (memq (plist-get current :timer) timer-list))))))
  :doc "a memory configuration change during observation restarts it for the new roots"
  (let ((other (file-name-concat root "other"))
        (refresh (symbol-function 'mevedel-memory-library-refresh)) (observations 0))
    (make-directory other)
    (write-region "Other topic.\n" nil (file-name-concat other "other.md") nil 'silent)
    (setf (mevedel-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (cl-letf (((symbol-function 'mevedel-memory-library-refresh)
               (lambda (&rest args) (cl-incf observations) (apply refresh args))))
      (mevedel-memory-list-open context)
      (with-current-buffer "*mevedel memory*"
        (setq mevedel-memory-dirs (list other))
        (with-timeout (45 (ert-fail "Reconfigured topics were not observed"))
          (while (not (eq 'current mevedel-memory-list--memories-state)) (accept-process-output nil .01)))
        (should (= 2 observations))
        (should (equal '("other.md") (mapcar (lambda (row) (plist-get row :file)) (mevedel-cockpit-surface-items))))
        (should-not (string-search "refreshing memories" (mevedel-memory-list--header nil context))))))
  :doc "an unexpected inventory failure becomes a visible unavailable row, not a stuck refresh"
  (progn
    (setf (mevedel-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 0 :unavailable 0))
    (cl-letf (((symbol-function 'mevedel-memory-library-refresh)
               (lambda (&rest _) (error "Injected inventory failure"))))
      (mevedel-memory-list-open context)
      (with-current-buffer "*mevedel memory*"
        (mevedel-test-memory-list-await-memories)
        (let ((row (car (mevedel-cockpit-surface-items))))
          (should (eq 'unavailable (plist-get row :status)))
          (should (equal "Injected inventory failure" (plist-get row :error))))
        (should-not (string-search "refreshing memories" (mevedel-memory-list--header nil context))))))
  :doc "observed alerts open Candidates, which wait for the history scan"
  (progn
    (setf (mevedel-workspace-memory-observation workspace) (list :at (float-time) :pending 0 :recovery 1 :unavailable 0))
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (should mevedel-memory-list--job)
      (should (eq 'candidates mevedel-memory-list--view))
      (should (string-search "Loading history" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-test-memory-list-await)
      (should (= 1 (length tabulated-list-entries)))))
  :doc "without an observation Memories shows at once; found candidates are signalled on their tab, never switched to"
  (progn
    (should-not (mevedel-workspace-memory-observation workspace))
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (should (eq 'memories mevedel-memory-list--view))
      (should mevedel-memory-list--job)
      (let ((header (mevedel-memory-list--header nil context)))
        (should (string-search "proposals not checked" header))
        (should (string-prefix-p "Candidates | Memories" header)))
      (mevedel-test-memory-list-await)
      (should (eq 'memories mevedel-memory-list--view))
      (should (= 1 (length tabulated-list-entries)))
      (let* ((header (mevedel-memory-list--header nil context))
             (count (string-search "(1)" header)))
        (should (string-prefix-p "Candidates (1) | Memories" header))
        (should (eq 'warning (get-text-property count 'face header)))
        (should (commandp (lookup-key (get-text-property count 'local-map header) [tab-line mouse-1]))))
      ;; Known counts keep Memories history-free until a proposal view is shown.
      (mevedel-memory-list-refresh)
      (should-not mevedel-memory-list--job)
      (mevedel-memory-list-candidates)
      (should mevedel-memory-list--job)
      (mevedel-test-memory-list-await)
      (should (= 1 (length tabulated-list-entries))))))

(provide 'test-mevedel-memory-list-loading)
;;; test-mevedel-memory-list-loading.el ends here
