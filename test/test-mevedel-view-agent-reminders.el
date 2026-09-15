;;; test-mevedel-view-agent-reminders.el --- Agent reminder rows -*- lexical-binding: t -*-

;;; Commentary:
;; Agent status refreshes must preserve adjacent reminder disclosures.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-stream)
(require 'mevedel-transcript-audit)
(require 'mevedel-tools)

(mevedel-deftest mevedel-view--refresh-agent-rendering-now/reminders ()
		 ,test
		 (test)
		 :doc "status refresh preserves one independently expanded reminder and composer"
		 (mevedel-view-test--with-buffers
		  (mevedel-tools-register)
		  (let ((path "/root/worker")
			(draft "> quoted draft\nsecond line"))
		    (mevedel-view-test--insert-data data-buf "*** Task\n#+begin_tool ToolCall\n" nil)
		    (mevedel-view-test--insert-data
		     data-buf
		     (concat
		      "(:name \"ToolCall\" :args (:expression \"(Agent)\"))\n\nStarted.\n"
		      (mevedel-tool-render-data-format
		       `(:kind ptc :direct-tool "Agent" :outcome completed
			       :calls ((:id "call_agent/1" :order 1 :tool "Agent"
					    :args (:task_name "worker") :status success
					    :result "Started."
					    :render-data (:kind collaboration-event
								:event started :path ,path
								:status running))))
		       "call_agent"))
		     '(tool . "call_agent"))
		    (mevedel-view-test--insert-data data-buf "#+end_tool\n" nil)
		    (mevedel-view-test--insert-data
		     data-buf
		     (mevedel--format-hook-audit-record
		      '(:type injected-reminders :phase mid-turn
			      :items ((:type agent-roster :body "Direct child agents: /root/worker")
				      (:type context-resources :body "Resources available."))))
		     nil)
		    (with-current-buffer view-buf
		      (mevedel-view--full-rerender)
		      (goto-char (point-min))
		      (search-forward "2 system reminders")
		      (mevedel-view-toggle-section)
		      (mevedel-view-test--insert-composer-draft draft 4)
		      (dotimes (_ 5)
			(should (car (mevedel-view--agent-handle-refresh-points path)))
			(should (mevedel-view--refresh-agent-rendering-now path))
			(should (= 1 (mevedel-view-test--count-substring
				      "2 system reminders (agent-roster, context-resources)"
				      (buffer-string))))
			(should (equal draft (mevedel-view--input-text)))
			(should (= (point) (+ 4 (mevedel-view--input-start))))
			(save-excursion
			  (goto-char (point-min))
			  (search-forward "2 system reminders")
			  (should-not (get-text-property (point) 'mevedel-view-collapsed))
			  (search-forward "Resources available.")))))))

(provide 'test-mevedel-view-agent-reminders)
;;; test-mevedel-view-agent-reminders.el ends here
