;;; test-niwa-report.el --- Information panel behavior -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise native report behavior through the cockpit's information entry.

;;; Code:

(require 'niwa-cockpit)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-cockpit-show-help ()
		 ,test
		 (test)

		 :doc "opens native sections and folds without changing their source text"
		 (save-window-excursion
		   (unwind-protect
		       (progn
			 (niwa-cockpit-show-help
			  "*niwa report test*"
			  '(:title "Session info" :subtitle "main"
				   :sections ((:id request :title "Request" :body "State  running\n")
					      (:id record :title "Record" :body "Exact **record**\n* raw heading\n"
						   :folded t))))
			 (with-current-buffer "*niwa report test*"
			   (should buffer-read-only)
			   (should-not truncate-lines)
			   (goto-char (point-min))
			   (should (looking-at-p "\\* Session info"))
			   (search-forward "Exact **record**")
			   (should (invisible-p (1- (point))))
			   (goto-char (point-min))
			   (search-forward "Record")
			   (button-activate (button-at (1- (point))))
			   (search-forward "Exact **record**")
			   (should-not (invisible-p (1- (point))))
			   (should (string-search "Exact **record**\n* raw heading\n"
						  (buffer-string)))))
		     (when (get-buffer "*niwa report test*")
		       (kill-buffer "*niwa report test*")))))

(niwa-deftest niwa-report-select-section ()
		 ,test
		 (test)

		 :doc "navigates memory sections and retains the reader across refresh"
		 (save-window-excursion
		   (let ((origin (generate-new-buffer " *report owner*"))
			 (report '(:title "Memory proposal" :subtitle "Topic · pending"
					  :identity topic :navigator t :initial body
					  :sections ((:id decision :title "Decision" :body "Pending review\n")
						     (:id body :title "Proposed body" :body "Exact proposed body\n")
						     (:id changes :title "Changes" :body "-old\n+new\n")))))
		     (unwind-protect
			 (progn
			   (switch-to-buffer origin)
			   (insert "> untouched draft\nsecond line")
			   (goto-char 5)
			   (niwa-cockpit-show-help "*niwa report test*" report)
			   (with-current-buffer "*niwa report test*"
			     (should (string-search "Exact proposed body" (buffer-string)))
			     (should-not (string-search "Pending review" (buffer-string)))
			     (call-interactively (key-binding (kbd "n")))
			     (should (string-search "-old\n+new\n" (buffer-string)))
			     (search-forward "+new")
			     (let ((position (point)))
			       (with-current-buffer origin
				 (niwa-cockpit-show-help "*niwa report test*" report))
			       (should (= position (point))))
			     (call-interactively (key-binding (kbd "q"))))
			   (should (eq (current-buffer) origin))
			   (should (= (point) 5))
			   (should (equal (buffer-string) "> untouched draft\nsecond line")))
		       (when (get-buffer "*niwa report test*")
			 (kill-buffer "*niwa report test*"))
		       (kill-buffer origin)))))

(niwa-deftest niwa-report-refresh ()
		 ,test (test)
		 :doc "retains source position as the title grows and refresh never steals focus"
		 (save-window-excursion
		   (let ((origin (generate-new-buffer " *report refresh owner*"))
			 (subtitle "Pending") inspector)
		     (unwind-protect
			 (cl-labels ((report ()
				       (list :title "Memory proposal" :subtitle subtitle :identity 'item
					     :navigator t :initial 'body :refresh #'report
					     :sections '((:id body :title "Body" :body "first line\nreading here\nlast line")))))
			   (switch-to-buffer origin)
			   (insert "> exact draft\ncontinued") (goto-char 4)
			   (setq inspector (niwa-cockpit-show-help "*niwa report refresh test*" (report)))
			   (with-current-buffer inspector (search-forward "reading here"))
			   (select-window (display-buffer origin))
			   (let ((selected (selected-window)))
			     (setq subtitle "Applied\nA longer status explanation")
			     (with-current-buffer inspector
			       (niwa-report-refresh)
			       (should (looking-back "reading here" (line-beginning-position))))
			     (should (eq selected (selected-window))))
			   (with-current-buffer origin
			     (should (= (point) 4))
			     (should (equal (buffer-string) "> exact draft\ncontinued"))))
		       (when (buffer-live-p inspector) (kill-buffer inspector))
		       (kill-buffer origin)))))

(niwa-deftest niwa-report-diagnostic-owner ()
  ,test (test)
  :doc "retains attribution after the originating buffer dies"
  (require 'niwa-report)
  (require 'niwa-structs)
  (let ((origin (generate-new-buffer " *report diagnostic owner*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer origin
            (setq-local niwa-cockpit--context
                        (list :workspace (niwa-workspace--create
                                          :root "/tmp/report-owner"))))
          (niwa-report-render '(:title "Report" :sections nil) origin)
          (kill-buffer origin)
          (should (equal (error-message-string
                          (should-error (niwa-report-refresh) :type 'user-error))
                         "niwa [/tmp/report-owner]: No live source to refresh this report"))
          (niwa-report--unavailable '(error "Failed at 50%"))
          (should (string-search "niwa [/tmp/report-owner]: Failed at 50%"
                                 (buffer-string))))
      (when (buffer-live-p origin) (kill-buffer origin))))

  :doc "explicit captured root or special scope overrides a foreign origin"
  (require 'niwa-report)
  (require 'niwa-structs)
  (with-temp-buffer
    (setq-local niwa--workspace (niwa-workspace--create :root "/tmp/foreign"))
    (let ((origin (current-buffer)))
      (dolist (case '(("/tmp/captured" . "niwa [/tmp/captured]: Failed")
                      (global . "niwa [global]: Failed")
                      (unavailable . "niwa [workspace unavailable]: Failed")))
        (with-temp-buffer
          (niwa-report-render
           (list :title "Report" :sections nil :diagnostic-scope (car case)) origin)
          (niwa-report--unavailable '(error "Failed"))
          (should (string-search (cdr case) (buffer-string))))))))

(niwa-deftest niwa-report-select-section-revoked ()
		 ,test (test)
		 :doc "revoked access clears already displayed private content on section navigation"
		 (save-window-excursion
		   (let ((allowed t) inspector)
		     (unwind-protect
			 (progn
			   (setq inspector
				 (niwa-cockpit-show-help
				  "*niwa report private test*"
				  (list :title "Private memory" :navigator t
					:validate (lambda () (unless allowed (user-error "Access revoked")))
					:sections '((:id body :title "Body" :body "Private topic text")
						    (:id evidence :title "Evidence" :body "Private evidence")))))
			   (setq allowed nil)
			   (with-current-buffer inspector
			     (should-error (niwa-report-next) :type 'user-error)
			     (should-not (string-search "Private topic text" (buffer-string)))
			     (should (string-search "unavailable" (buffer-string)))))
		       (when (buffer-live-p inspector) (kill-buffer inspector))))))

(niwa-deftest niwa-report-fontification ()
		 ,test (test)
		 :doc "shows distinct diff faces while preserving every source character"
		 (save-window-excursion
		   (let ((diff "--- a/file\n+++ b/file\n@@ -1 +1 @@\n-old\n+new\n") inspector)
		     (unwind-protect
			 (progn
			   (setq inspector
				 (niwa-cockpit-show-help
				  "*niwa report font test*"
				  (list :title "Captured change"
					:sections (list (list :id 'diff :title "Diff" :body diff :mode 'diff-mode)))))
			   (with-current-buffer inspector
			     (should (string-search diff (buffer-string)))
			     (search-forward "-old")
			     (let ((removed (get-text-property (1- (point)) 'face)))
			       (search-forward "+new")
			       (should removed)
			       (should (get-text-property (1- (point)) 'face))
			       (should-not (equal removed (get-text-property (1- (point)) 'face))))))
		       (when (buffer-live-p inspector) (kill-buffer inspector))))))

(niwa-deftest niwa-report-layout ()
		 ,test (test)
		 :doc "a narrow inspector gives the index seven lines and retains a readable pane"
		 (save-window-excursion
		   (delete-other-windows)
		   (let ((origin (generate-new-buffer " *report layout owner*"))
			 (display-buffer-alist '(("report layout test" (display-buffer-same-window))))
			 inspector)
		     (unwind-protect
			 (progn
			   (switch-to-buffer origin)
			   (setq inspector
				 (niwa-cockpit-show-help
				  "*niwa report layout test*"
				  '(:title "Memory" :navigator t
					   :sections ((:id body :title "Body" :body "Exact body")))) )
			   (let ((reader (get-buffer-window inspector))
				 (index (get-buffer-window "*niwa report layout test* sections")))
			     (should (window-live-p index))
			     (should (= 7 (window-total-height index)))
			     (should (> (window-total-height reader) (window-total-height index)))
			     (should-not (buffer-local-value 'truncate-partial-width-windows inspector)))
			   (with-current-buffer inspector (niwa-report-quit))
			   (should (one-window-p))
			   (should (eq (window-buffer) origin)))
		       (when (buffer-live-p inspector) (kill-buffer inspector))
		       (kill-buffer origin)))))

(provide 'test-niwa-report)
;;; test-niwa-report.el ends here
