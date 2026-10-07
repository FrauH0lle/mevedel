;;; test-mevedel-claude-code-input.el --- Subscription composer attachments -*- lexical-binding: t -*-

;;; Commentary:
;; User attachments pass through the normal composer and real ACP transport.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)
(require 'mevedel-claude-code-agent)
(require 'mevedel-init)

(mevedel-deftest mevedel-view-send/claude-busy (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil) view)
      (unwind-protect
          (progn
            (mevedel-claude-code-register)
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
            (setf (mevedel-engine-info request) '(:external-history t))
            (setq view (mevedel-view--ensure buffer))
            (with-current-buffer view
              (dolist (input '("> Keep this next\nwith its second line" "Then continue"))
                (goto-char (point-max))
                (insert input)
                (mevedel-view-send)
                (should (equal "" (mevedel-view--input-text)))))
            (let ((entries (mevedel-session-pending-follow-ups session)))
              (should (equal '("> Keep this next\nwith its second line" "Then continue")
                             (mapcar (lambda (entry) (plist-get entry :input)) entries)))
              (should-not (equal (plist-get (car entries) :id)
                                 (plist-get (cadr entries) :id))))
            (should (eq request mevedel--current-request))
            (should (= 0 (mevedel-session-turn-count session))))
        (when (buffer-live-p view) (kill-buffer view))))))

(mevedel-deftest gptel-send/claude-session (:quiet t)
  (dolist (route '(raw paired init))
    (mevedel-engine-test--with-session
      (mevedel-request-end)
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            (installed (advice-member-p #'mevedel-skills--gptel-send-advice
                                        'gptel-send))
            view)
        (unwind-protect
            (progn
              (mevedel-claude-code-register)
              (mevedel-model-set-session-provider
               session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
              (setq-local gptel-system-prompt "Raw send fixture" gptel-tools nil)
              (when (eq route 'paired)
                (setq view (mevedel-view--ensure buffer)))
              (unless installed
                (advice-add 'gptel-send :around #'mevedel-skills--gptel-send-advice))
              (cl-letf (((symbol-function 'gptel-request)
                         (lambda (&rest _) (ert-fail "Subscription send reached the API")))
                        ((symbol-function 'mevedel-claude-code-launch)
                         (mevedel-engine-test--claude-launch
                          (lambda (_system mcp _model _effort &optional id _hook)
                            nil))))
                (if (eq route 'init)
                    (mevedel-init "raw-init-evidence")
                  (mevedel--insert-user-turn "raw-send-evidence")
                  (gptel-send))
                (with-timeout (5 (ert-fail "Raw send did not settle"))
                  (while (mevedel-turn-busy-p buffer)
                    (accept-process-output nil 0.01))))
              (should (string-search "answer:" (buffer-string)))
              (should (string-search
                       (if (eq route 'init) "raw-init-evidence" "raw-send-evidence")
                       (buffer-string)))
              (should (= 1 (mevedel-session-turn-count session)))
              (should (eq 'ready (plist-get
                                 (alist-get "root" (mevedel-session-external-conversations session)
                                            nil nil #'equal) :state))))
          (unless installed
            (advice-remove 'gptel-send #'mevedel-skills--gptel-send-advice))
          (when (buffer-live-p view) (kill-buffer view)))))))

(mevedel-deftest gptel-send/claude-admission (:quiet t)
  (dolist (state '(busy read-only steering))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (installed (advice-member-p #'mevedel-skills--gptel-send-advice 'gptel-send)))
        (unwind-protect
            (progn
              (mevedel-claude-code-register)
              (mevedel-model-set-session-provider
               session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
              (when (eq state 'read-only)
                (mevedel-request-end)
                (setq-local mevedel-session--read-only-mode t))
              (unless installed
                (advice-add 'gptel-send :around #'mevedel-skills--gptel-send-advice))
              (cl-letf (((symbol-function 'gptel-request)
                         (lambda (&rest _) (ert-fail "Rejected input reached API")))
                        ((symbol-function 'mevedel-claude-code-launch)
                         (lambda (&rest _) (ert-fail "Rejected input launched an agent"))))
                (mevedel--insert-user-turn "must remain unsent")
                (let ((error (should-error (gptel-send (and (eq state 'steering) 0))
                                          :type 'user-error)))
                  (should (string-match-p
                           (pcase state
                             ('busy "already active")
                             ('read-only "read-only")
                             ('steering "unavailable.*external"))
                           (error-message-string error)))))
              (unless (eq state 'read-only)
                (should (eq request mevedel--current-request)))
              (should (= 0 (mevedel-session-turn-count session))))
          (unless installed
            (advice-remove 'gptel-send #'mevedel-skills--gptel-send-advice)))))))

(mevedel-deftest mevedel-view--forward-input-now/claude-image (:quiet t)
		 (pcase-dolist (`(,acknowledgement ,attachment) '((t mention) (:false mention) ("image-mismatch" mention)
								  (t context) (t both)))
		   (mevedel-engine-test--with-session
		    (mevedel-request-end)
		    (let ((view (generate-new-buffer " *claude-image-composer*"))
			  (gptel--known-backends nil)
			  (mevedel-claude-code-directory (file-name-concat root "claude"))
			  (file (file-name-concat root "sample.png"))
			  (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII="))
		      (unwind-protect
			  (progn
			    (with-temp-buffer
			      (set-buffer-multibyte nil)
			      (insert (base64-decode-string data))
			      (let ((coding-system-for-write 'no-conversion))
				(write-region (point-min) (point-max) file nil 'silent)))
			    (mevedel-claude-code-register)
			    (mevedel-model-set-session-provider
			     session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
			    (setq-local gptel-system-prompt "Image fixture" gptel-tools nil)
			    (when (memq attachment '(context both))
			      (setq-local gptel-use-context t gptel-context (list (list file :mime "image/png"))))
			    (mevedel-chat-install-request-hooks)
			    (mevedel-view--setup view buffer)
			    (cl-letf (((symbol-function 'mevedel-claude-code-launch)
				       (mevedel-engine-test--claude-launch
                                        (lambda (_system mcp _model _effort &optional id _hook)
					  (list :meta `((expectedImage . ,data)
						        (promptAcknowledgement . ,acknowledgement)))))))
			      (with-current-buffer view
				(mevedel-view--forward-input-now
				 (if (eq attachment 'context) "Describe the attached image"
				   (format "Describe @file:{%s}" file)))
				(goto-char (mevedel-view--input-start))
				(insert "> Keep the next draft\nwith two lines"))
			      (with-timeout (5 (ert-fail "Image request did not settle"))
				(while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
			    (should (string-search "Image accepted" (buffer-string)))
			    (should (eq (if (eq acknowledgement t) 'ready 'uncertain)
					(plist-get (alist-get "root" (mevedel-session-external-conversations session)
							      nil nil #'equal) :state)))
			    (should (= (if (eq acknowledgement t) (if (eq attachment 'both) 2 1) 0)
				       (hash-table-count (mevedel-session-mentions-shown session))))
			    (should (= 1 (mevedel-session-turn-count session)))
			    (should-not (string-search data (buffer-string)))
			    (with-current-buffer view
			      (should (equal "> Keep the next draft\nwith two lines" (mevedel-view--input-text)))))
			(when (buffer-live-p view) (kill-buffer view)))))))

(mevedel-deftest mevedel-agent-control-spawn/claude-image (:quiet t)
		 (mevedel-engine-test--with-session
		  (let ((gptel--known-backends nil)
			(mevedel-claude-code-directory (file-name-concat root "claude"))
			(file (file-name-concat root "sample.png"))
			(data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
			record invocation)
		    (with-temp-buffer
		      (set-buffer-multibyte nil)
		      (insert (base64-decode-string data))
		      (let ((coding-system-for-write 'no-conversion))
			(write-region (point-min) (point-max) file nil 'silent)))
		    (mevedel-claude-code-register)
		    (mevedel-model-set-session-provider
		     session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
		    (unwind-protect
			(cl-letf (((symbol-function 'mevedel-claude-code-launch)
				   (mevedel-engine-test--claude-launch
                                    (lambda (_system mcp _model _effort &optional id _hook)
				      (list :meta `((expectedImage . ,data)))))))
			  (dotimes (phase 2)
			    (let ((prompt (format "Describe @file:{%s}" file)))
			      (if (= phase 0)
				  (mevedel-agent-control-spawn
				   session "image" prompt
				   (lambda (value) (setq record (plist-get value :record)))
				   :agent (mevedel-agent--create :name "image" :description "Describe an image"
								 :tools nil :max-turns 2 :system-prompt "Describe the image")
				   :on-invocation (lambda (value) (setq invocation value)))
				(mevedel-agent-control-followup session "/root/image" prompt)
				(setq invocation (mevedel-agent-record-invocation record))))
			    (with-timeout (5 (ert-fail "Image child did not settle"))
			      (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
				(accept-process-output nil 0.01)))
			    (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
			    (should (string-suffix-p "Image accepted" (mevedel-agent-record-settled-result record)))
			    (should (= 0 (hash-table-count (mevedel-session-mentions-shown session))))))
		      (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
			(kill-buffer (mevedel-agent-record-conversation-buffer record)))))))

(mevedel-deftest mevedel--start-directive-discussion/claude-image (:quiet t)
		 (mevedel-engine-test--with-session
		  (mevedel-request-end)
		  (let* ((gptel--known-backends nil)
			 (mevedel-show-chat-buffer nil)
			 (inhibit-interaction t)
			 (gptel--known-presets (copy-tree gptel--known-presets))
			 (mevedel-preset--registry (copy-tree mevedel-preset--registry))
			 (mevedel-claude-code-directory (file-name-concat root "claude"))
			 (source (find-file-noselect (file-name-concat root "subject.txt")))
			 (file (file-name-concat root "sample.png"))
			 (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
			 directive record done)
		    (unwind-protect
			(progn
			  (mevedel-tools-register)
			  (mevedel--define-presets)
			  (with-temp-buffer
			    (set-buffer-multibyte nil)
			    (insert (base64-decode-string data))
			    (let ((coding-system-for-write 'no-conversion))
			      (write-region (point-min) (point-max) file nil 'silent)))
			  (mevedel-claude-code-register)
			  (mevedel-model-set-session-provider
			   session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
			  (setq-local gptel-system-prompt "Describe the image" gptel-tools nil)
			  (with-current-buffer source
			    (setq-local mevedel--workspace workspace)
			    (insert "subject\n") (save-buffer)
			    (setq directive (mevedel--create-directive-in
					     source (point-min) (1- (point-max)) nil
					     (format "Describe @file:{%s}" file))
				  record (mevedel--directive-record directive))
			    (setf (mevedel-directive-session-id record) (mevedel-session-session-id session)))
			  (cl-letf (((symbol-function 'mevedel-claude-code-launch)
				     (mevedel-engine-test--claude-launch
                                      (lambda (_system mcp _model _effort &optional id _hook)
				        (should-not id)
				        (list :meta `((expectedImage . ,data)))))))
			    (with-current-buffer source
			      (mevedel--start-directive-discussion
			       directive (lambda (err _owner) (should-not err) (setq done t))))
			    (with-timeout (5 (ert-fail "Image directive did not settle"))
			      (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
			  (should done)
			  (should (string-search "Image accepted"
						 (mevedel-directive-discussion-turn-result
						  (car (mevedel-directive-discussion record)))))
			  (should (= 0 (hash-table-count (mevedel-session-mentions-shown session)))))
		      (when (buffer-live-p source)
			(with-current-buffer source (set-buffer-modified-p nil))
			(kill-buffer source))))))

(provide 'test-mevedel-claude-code-input)
;;; test-mevedel-claude-code-input.el ends here
