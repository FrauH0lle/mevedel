;;; test-mevedel-claude-code-input.el --- Subscription composer attachments -*- lexical-binding: t -*-

;;; Commentary:
;; User attachments pass through the normal composer and real ACP transport.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-agent)
(require 'mevedel-claude-code-context)
(require 'mevedel-acp)
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
;; Authored text and images both continue the native turn as user prompts.
(mevedel-deftest mevedel--send-request/claude-steering (:quiet t)
  (dolist (mode '(boundary oversized mismatch missing foreign nested split final held image image-final image-resized image-missing))
    (mevedel-engine-test--with-claude-session
      (let* ((file (file-name-concat root "evidence.txt"))
             (image (file-name-concat root "steer.png"))
             (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
             (input (cond ((memq mode '(image image-final image-resized image-missing))
                           (format "STEER-4417 @file:{%s}" image))
                          ((eq mode 'oversized) (concat "STEER-4417 " (make-string 11000 ?x)))
                          (t "STEER-4417")))
             (final (memq mode '(final image-final)))
             (submit (symbol-function 'mevedel-acp-prompt))
             steered prompts decisions submissions)
        (write-region "Evidence" nil file nil 'silent)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert (base64-decode-string data))
          (let ((coding-system-for-write 'no-conversion))
            (write-region (point-min) (point-max) image nil 'silent)))
        (setq-local gptel-system-prompt "Steering fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (when (eq mode 'held)
          (mevedel-session-set-pending-input-paused session t))
        (cl-labels ((steer ()
                      (unless steered
                        (setq steered t)
                        (mevedel-session-enqueue-pending-input
                         session 'steering
                         (list :input input :model-input input
                               :transcript-payload input
                               :request-id (mevedel-request-id request))))))
          (cl-letf (((symbol-function 'mevedel-acp-prompt)
                     (lambda (&rest args)
                       (push (copy-tree (cadr args)) submissions)
                       (apply submit args)))
                    ((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (_system _mcp _model _effort &optional _id hook)
                        (list :observe (lambda (owner event)
                                         (when (equal "user" (map-nested-elt event '(params message type)))
                                           (push (copy-tree (map-nested-elt event '(params message message content))) prompts))
                                         (mevedel-claude-code--observe owner event))
                              :control (lambda (owner event)
                                         (unless final (steer))
                                         (let ((decision (mevedel-claude-code--control owner event)))
                                           (push decision decisions)
                                           decision))
                              :complete-prompt (lambda (owner outcome)
                                                 (when final (steer))
                                                 (mevedel-claude-code--complete-prompt owner outcome))
                              :meta `((hookCommand . ,hook)
                                      (responseText . "Done")
                                      (continuationPrompts
                                       . [((responseText . "Steered reply")
                                           (promptAcknowledgement
                                            . ,(pcase mode
                                                 ('mismatch "authored-mismatch")
                                                 ('missing :false)
                                                 ((or 'foreign 'nested 'split 'image-resized 'image-missing)
                                                  (symbol-name mode))
                                                 (_ t))))])
                                      (toolBatches
                                       . ,(apply #'vector
                                                 (mapcar (lambda (id)
                                                           (vector `((name . "Read") (id . ,id)
                                                                     (args . ((file_path . ,file))))))
                                                         (if final '("only")
                                                           '("first" "second")))))))))))
            (insert "Read the evidence")
            (mevedel--send-request "Read the evidence")
            (mevedel-test--await 5 "Steering turn did not settle"
              (not (mevedel-turn-busy-p buffer)))))
        (ert-info ((format "mode=%S" mode))
          (should (eq (if (memq mode '(mismatch missing foreign nested split image-missing)) 'error 'success)
                      (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
          (should (= (if (memq mode '(boundary oversized final image image-final image-resized)) 1 0)
                     (how-many "STEER-4417" (point-min) (point-max))))
          ;; Every deliverable steering batch needs a native user prompt.
          (should (eq (not (eq mode 'held))
                      (and (string-search "Steered reply" (buffer-string)) t)))
          (dolist (decision decisions)
            (should-not (string-search "STEER-4417"
                                       (or (plist-get (plist-get decision :hookSpecificOutput)
                                                      :additionalContext) ""))))
          (unless (memq mode '(held missing foreign nested split mismatch image-missing))
            (should (seq-some (lambda (parts)
                                (seq-some (lambda (part)
                                            (let ((text (alist-get 'text part)))
                                              (if (memq mode '(image image-final image-resized))
                                                  (and text (string-prefix-p "STEER-4417" text))
                                                (equal text input))))
                                          parts)) prompts))
            (should-not (string-search "Continue the current task" (prin1-to-string prompts))))
          (when (eq mode 'oversized)
            ;; Count dispatches, not the fixture's deliberately duplicated echoes.
            (should (= 1 (seq-count (lambda (parts)
                                     (seq-some (lambda (part) (equal input (alist-get 'text part))) parts))
                                   submissions))))
          (should-not (string-search data (buffer-string)))
          (pcase mode
            ((or 'boundary 'oversized 'final 'image 'image-final 'image-resized)
             (should-not (mevedel-session-pending-steering session))
             (should-not (mevedel-session-pending-follow-ups session)))
            ((or 'mismatch 'missing 'foreign 'nested 'split 'image-missing)
             (should (eq 'failed-turn
                         (plist-get (car (mevedel-session-pending-steering session)) :state))))
            ('held
             ;; A native turn cannot park: held steering leads the follow-ups.
             (should-not (mevedel-session-pending-steering session))
             (should (equal '("STEER-4417")
                            (mapcar (lambda (entry) (plist-get entry :input))
                                    (mevedel-session-pending-follow-ups session)))))))))))

(mevedel-deftest mevedel--send-request/claude-startup-steering (:quiet t)
  (dolist (ack '(t "image-one-missing" "authored-mismatch" "split"))
    (mevedel-engine-test--with-claude-session
      (let* ((original (file-name-concat root "original.png"))
             (steering (file-name-concat root "steering.png"))
             (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
             (input (format "Original image @file:{%s}" original))
             (steer (format "STARTUP-AUTHORED-1913 @file:{%s}" steering))
             (prepare (symbol-function 'mevedel-claude-code-context-input))
             queued echoes)
        (dolist (file (list original steering))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (base64-decode-string data))
            (let ((coding-system-for-write 'no-conversion))
              (write-region (point-min) (point-max) file nil 'silent))))
        (setq-local gptel-system-prompt "Startup steering fixture" gptel-tools nil)
        (cl-letf (((symbol-function 'mevedel-claude-code-context-input)
                   (lambda (owner text)
                     (prog1 (funcall prepare owner text)
                       (unless queued
                         (setq queued t)
                         (mevedel-session-enqueue-pending-input
                          session 'steering (list :input steer :model-input steer :transcript-payload steer
                                                  :request-id (mevedel-request-id request)))))))
                  ((symbol-function 'mevedel-claude-code-launch)
                   (mevedel-engine-test--claude-launch
                    (lambda (_system _mcp _model _effort &optional _id _hook)
                      (list :observe (lambda (owner event)
                                       (when (equal "user" (map-nested-elt event '(params message type)))
                                         (push (copy-tree (map-nested-elt event '(params message message content))) echoes))
                                       (mevedel-claude-code--observe owner event))
                            :meta `((promptAcknowledgement . ,ack)))))))
          (mevedel--insert-user-turn input)
          (mevedel--send-request input)
          (mevedel-test--await 5 "Startup steering did not settle"
            (not (mevedel-turn-busy-p buffer))))
        (ert-info ((format "startup ack=%S" ack))
          (should queued)
          (should (eq (if (eq ack t) 'success 'error)
                      (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
          (should (= (if (eq ack t) 1 0) (how-many "STARTUP-AUTHORED-1913" (point-min) (point-max))))
          (if (eq ack t)
              (progn
                (should-not (mevedel-session-pending-steering session))
                (should (= 2 (seq-count (lambda (part) (equal "image" (alist-get 'type part))) (car echoes)))))
            (should (eq 'failed-turn (plist-get (car (mevedel-session-pending-steering session)) :state)))))))))

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
		 (pcase-dolist (`(,acknowledgement ,attachment) '((t mention) (:false mention) ("image-missing" mention)
								  ("image-resized" mention) (t context) (t both)))
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
			    (should (eq (if (member acknowledgement '(t "image-resized")) 'ready 'uncertain)
					(plist-get (alist-get "root" (mevedel-session-external-conversations session)
							      nil nil #'equal) :state)))
			    (should (= (if (member acknowledgement '(t "image-resized"))
					   (if (eq attachment 'both) 2 1)
					 0)
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
				(mevedel-agent-control-send-message session "/root/image" prompt)
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
