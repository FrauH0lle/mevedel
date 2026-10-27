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
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (should (string-search "Loading memory data" (overlay-get mevedel-memory-list--notice 'before-string)))
      (should (= (point-min) (overlay-start mevedel-memory-list--notice)))
      (should (string-empty-p (buffer-string)))
      (should-not tabulated-list-entries)
      (should-not (tabulated-list-get-id))
      (should-not (mevedel-cockpit-surface-selected t))
      (should-error (mevedel-cockpit-surface-details) :type 'user-error)
      (with-timeout (45 (ert-fail "Recovery did not admit history scan"))
	(while mevedel-memory-list--recovery (accept-process-output nil .01)))
      (should mevedel-memory-list--job)
      (should (string-search "Loading history" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-memory-list-history)
      (should (string-search "Loading history" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-memory-list-memories)
      (should (= 1 (length tabulated-list-entries)))
      (should-not mevedel-memory-list--notice)
      (should (mevedel-cockpit-surface-selected))
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
      (mevedel-memory-list--received (current-buffer) mevedel-memory-list--job '(:error "Injected scan failure"))
      (should (string-search "Could not load memory data" (overlay-get mevedel-memory-list--notice 'before-string)))
      (should (string-search "g to retry" (overlay-get mevedel-memory-list--notice 'before-string)))
      (should-not (mevedel-cockpit-surface-selected t))
      (mevedel-memory-list-memories)
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
  :doc "recovery timeout and unavailable owner are visible; native quit releases the notice"
  (progn
    (mevedel-memory-list-open context)
    (with-current-buffer "*mevedel memory*"
      (plist-put mevedel-memory-list--recovery :expires 0)
      (mevedel-memory-list--observe-recovery (current-buffer) mevedel-memory-list--recovery)
      (should (string-search "Could not load memory data" (overlay-get mevedel-memory-list--notice 'before-string)))
      (mevedel-memory-list--unavailable "Memory display owner is unavailable; reopen the cockpit")
      (should (string-search "owner is unavailable" (overlay-get mevedel-memory-list--notice 'before-string)))
      (let ((notice mevedel-memory-list--notice))
        (call-interactively (key-binding (kbd "q")))
        (should-not (overlay-buffer notice))))))

(provide 'test-mevedel-memory-list-loading)
;;; test-mevedel-memory-list-loading.el ends here
