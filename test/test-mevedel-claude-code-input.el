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
  (mevedel-engine-test--with-claude-session
    (let (view)
      (unwind-protect
          (progn
            (setf (mevedel-engine-info request) '(:external-history t))
            (setq view (mevedel-view--ensure buffer))
            (with-current-buffer view
              (dolist (input '("> Keep this next\nwith its second line" "Then continue"))
                (goto-char (point-max))
                (insert input)
                (mevedel-view-send)
                (should (equal "" (mevedel-view--input-text)))))
            ;; A busy Claude turn takes the composer as steering, like HTTP.
            (let ((entries (mevedel-session-pending-steering session)))
              (should (equal '("> Keep this next\nwith its second line" "Then continue")
                             (mapcar (lambda (entry) (plist-get entry :input)) entries)))
              (should (equal (list (mevedel-request-id request) (mevedel-request-id request))
                             (mapcar (lambda (entry) (plist-get entry :request-id)) entries)))
              (should-not (equal (plist-get (car entries) :id)
                                 (plist-get (cadr entries) :id))))
            (should-not (mevedel-session-pending-follow-ups session))
            (should (eq request mevedel--current-request))
            (should (= 0 (mevedel-session-turn-count session))))
        (when (buffer-live-p view) (kill-buffer view))))))

;; Steering is queued at a hook or prompt boundary so its moment is exact.
(mevedel-deftest mevedel--send-request/claude-steering (:quiet t)
  (dolist (mode '(hook mismatch final held))
    (mevedel-engine-test--with-claude-session
      (let ((file (file-name-concat root "evidence.txt"))
            steered)
        (write-region "Evidence" nil file nil 'silent)
        (setq-local gptel-system-prompt "Steering fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (when (eq mode 'held)
          (mevedel-session-set-pending-input-paused session t))
        (cl-labels ((steer ()
                      (unless steered
                        (setq steered t)
                        (mevedel-session-enqueue-pending-input
                         session 'steering
                         (list :input "STEER-4417" :model-input "STEER-4417"
                               :transcript-payload "STEER-4417"
                               :request-id (mevedel-request-id request))))))
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (_system _mcp _model _effort &optional _id hook)
                        (list :control (lambda (owner event)
                                         (unless (eq mode 'final) (steer))
                                         (mevedel-claude-code--control owner event))
                              :complete-prompt (lambda (owner outcome)
                                                 (when (eq mode 'final) (steer))
                                                 (mevedel-claude-code--complete-prompt owner outcome))
                              :meta `((hookCommand . ,hook)
                                      (hookAcknowledgement . ,(if (eq mode 'mismatch) "mismatch" t))
                                      (responseText . "Done")
                                      (continuationPrompts . [((responseText . "Steered reply"))])
                                      (toolBatches
                                       . ,(apply #'vector
                                                 (mapcar (lambda (id)
                                                           (vector `((name . "Read") (id . ,id)
                                                                     (args . ((file_path . ,file))))))
                                                         (if (eq mode 'final) '("only")
                                                           '("first" "second")))))))))))
            (insert "Read the evidence")
            (mevedel--send-request "Read the evidence")
            (mevedel-test--await 5 "Steering turn did not settle"
              (not (mevedel-turn-busy-p buffer)))))
        (ert-info ((format "mode=%S" mode))
          (should (eq (if (eq mode 'mismatch) 'error 'success)
                      (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
          (should (= (if (memq mode '(hook final)) 1 0)
                     (how-many "STEER-4417" (point-min) (point-max))))
          (should (eq (eq mode 'final) (and (string-search "Steered reply" (buffer-string)) t)))
          (pcase mode
            ((or 'hook 'final)
             (should-not (mevedel-session-pending-steering session))
             (should-not (mevedel-session-pending-follow-ups session)))
            ('mismatch
             (should (eq 'failed-turn
                         (plist-get (car (mevedel-session-pending-steering session)) :state))))
            ('held
             ;; A native turn cannot park: held steering leads the follow-ups.
             (should-not (mevedel-session-pending-steering session))
             (should (equal '("STEER-4417")
                            (mapcar (lambda (entry) (plist-get entry :input))
                                    (mevedel-session-pending-follow-ups session)))))))))))

(mevedel-deftest gptel-send/claude-session (:quiet t)
  (dolist (route '(raw paired init))
    (mevedel-engine-test--with-claude-session
      (mevedel-request-end)
      (let ((installed (advice-member-p #'mevedel-skills--gptel-send-advice
                                        'gptel-send))
            view)
        (unwind-protect
            (progn
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
                (mevedel-test--await 5 "Raw send did not settle" (not (mevedel-turn-busy-p buffer))))
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
    (mevedel-engine-test--with-claude-session
      (let ((installed (advice-member-p #'mevedel-skills--gptel-send-advice 'gptel-send)))
        (unwind-protect
            (progn
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
		   (mevedel-engine-test--with-claude-session
		    (mevedel-request-end)
		    (let ((view (generate-new-buffer " *claude-image-composer*"))
			  (file (file-name-concat root "sample.png"))
			  (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII="))
		      (unwind-protect
			  (progn
			    (with-temp-buffer
			      (set-buffer-multibyte nil)
			      (insert (base64-decode-string data))
			      (let ((coding-system-for-write 'no-conversion))
				(write-region (point-min) (point-max) file nil 'silent)))
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
			      (mevedel-test--await 5 "Image request did not settle"
                                (not (mevedel-turn-busy-p buffer))))
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
		 (mevedel-engine-test--with-claude-session
		  (let ((file (file-name-concat root "sample.png"))
			(data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
			record invocation)
		    (with-temp-buffer
		      (set-buffer-multibyte nil)
		      (insert (base64-decode-string data))
		      (let ((coding-system-for-write 'no-conversion))
			(write-region (point-min) (point-max) file nil 'silent)))
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
			    (mevedel-test--await 5 "Image child did not settle"
                              (mevedel-agent-invocation-runtime-settled-p invocation))
			    (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
			    (should (string-suffix-p "Image accepted" (mevedel-agent-record-settled-result record)))
			    (should (= 0 (hash-table-count (mevedel-session-mentions-shown session))))))
		      (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
			(kill-buffer (mevedel-agent-record-conversation-buffer record)))))))

(mevedel-deftest mevedel--start-directive-discussion/claude-image (:quiet t)
		 (mevedel-engine-test--with-claude-session
		  (mevedel-request-end)
		  (let* ((mevedel-show-chat-buffer nil)
			 (inhibit-interaction t)
			 (gptel--known-presets (copy-tree gptel--known-presets))
			 (mevedel-preset--registry (copy-tree mevedel-preset--registry))
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
			    (mevedel-test--await 5 "Image directive did not settle"
                              (not (mevedel-turn-busy-p buffer))))
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
