;;; test-niwa-view-agent-reminders.el --- Agent reminder rows -*- lexical-binding: t -*-

;;; Commentary:
;; Agent status refreshes must preserve adjacent reminder disclosures.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-composer)
(require 'niwa-view-stream)
(require 'niwa-transcript-audit)
(require 'niwa-tools)

(niwa-deftest niwa-view--refresh-agent-rendering-now/reminders ()
		 ,test
		 (test)
		 :doc "status refresh preserves one independently expanded reminder and composer"
		 (niwa-view-test--with-buffers
		  (niwa-tools-register)
		  (let ((path "/root/worker")
			(draft "> quoted draft\nsecond line"))
		    (niwa-view-test--insert-data data-buf "*** Task\n#+begin_tool ToolCall\n" nil)
		    (niwa-view-test--insert-data
		     data-buf
		     (concat
		      "(:name \"ToolCall\" :args (:expression \"(Agent)\"))\n\nStarted.\n"
		      (niwa-tool-render-data-format
		       `(:kind ptc :direct-tool "Agent" :outcome completed
			       :calls ((:id "call_agent/1" :order 1 :tool "Agent"
					    :args (:task_name "worker") :status success
					    :result "Started."
					    :render-data (:kind collaboration-event
								:event started :path ,path
								:status running))))
		       "call_agent"))
		     '(tool . "call_agent"))
		    (niwa-view-test--insert-data data-buf "#+end_tool\n" nil)
		    (niwa-view-test--insert-data
		     data-buf
		     (niwa--format-hook-audit-record
		      '(:type injected-reminders :phase mid-turn
			      :items ((:type agent-roster :body "Direct child agents: /root/worker")
				      (:type context-resources :body "Resources available."))))
		     nil)
		    (with-current-buffer view-buf
		      (niwa-view--full-rerender)
		      (goto-char (point-min))
		      (search-forward "2 system reminders")
		      (niwa-view-toggle-section)
		      (niwa-view-test--insert-composer-draft draft 4)
		      (dotimes (_ 5)
			(should (car (niwa-view--agent-handle-refresh-points path)))
			(should (niwa-view--refresh-agent-rendering-now path))
			(should (= 1 (niwa-view-test--count-substring
				      "2 system reminders (agent-roster, context-resources)"
				      (buffer-string))))
			(should (equal draft (niwa-view-composer-draft-text)))
			(should (= 4 (let ((view (current-buffer)))
			      (with-current-buffer (niwa-view-composer-buffer view)
				(- (point) (niwa-view-composer-draft-start view))))))
			(save-excursion
			  (goto-char (point-min))
			  (search-forward "2 system reminders")
			  (should-not (get-text-property (point) 'niwa-view-collapsed))
			  (search-forward "Resources available.")))))))

(provide 'test-niwa-view-agent-reminders)
;;; test-niwa-view-agent-reminders.el ends here
