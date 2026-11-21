;;; test-niwa-claude-code-input.el --- Subscription composer attachments -*- lexical-binding: t -*-

;;; Commentary:
;; User attachments pass through the normal composer and real ACP transport.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)
(require 'niwa-claude-code)
(require 'niwa-claude-code-agent)
(require 'niwa-claude-code-context)
(require 'niwa-acp)
(require 'niwa-init)
(require 'niwa-view-composer)

(niwa-deftest niwa-view-send/claude-busy (:quiet t)
  (niwa-engine-test--with-claude-session
    (let (view)
      (unwind-protect
          (progn
            (setf (niwa-engine-info request) '(:external-history t))
            (setq view (niwa-view--ensure buffer))
            (save-window-excursion
              (set-window-buffer (selected-window) view)
              (select-window (should (niwa-view-composer-display (selected-window))))
              (dolist (input '("> Keep this next\nwith its second line" "Then continue"))
                (niwa-view-composer-insert input)
                (execute-kbd-macro (kbd "C-c RET"))
                (should (equal "" (niwa-view-composer-draft-text)))))
            ;; A busy Claude turn takes the composer as steering, like HTTP.
            (let ((entries (niwa-session-pending-steering session)))
              (should (equal '("> Keep this next\nwith its second line" "Then continue")
                             (mapcar (lambda (entry) (plist-get entry :input)) entries)))
              (should (equal (list (niwa-request-id request) (niwa-request-id request))
                             (mapcar (lambda (entry) (plist-get entry :request-id)) entries)))
              (should-not (equal (plist-get (car entries) :id)
                                 (plist-get (cadr entries) :id))))
            (should-not (niwa-session-pending-follow-ups session))
            (should (eq request niwa--current-request))
            (should (= 0 (niwa-session-turn-count session))))
        (when (buffer-live-p view) (kill-buffer view))))))

;; Steering is queued at a hook or prompt boundary so its moment is exact.
;; Authored text and images both continue the native turn as user prompts.
(niwa-deftest niwa--send-request/claude-steering (:quiet t)
  (dolist (mode '(boundary oversized mismatch missing foreign nested split final held image image-final image-resized image-missing))
    (niwa-engine-test--with-claude-session
      (let* ((file (file-name-concat root "evidence.txt"))
             (image (file-name-concat root "steer.png"))
             (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
             (input (cond ((memq mode '(image image-final image-resized image-missing))
                           (format "STEER-4417 @file:{%s}" image))
                          ((eq mode 'oversized) (concat "STEER-4417 " (make-string 11000 ?x)))
                          (t "STEER-4417")))
             (final (memq mode '(final image-final)))
             (submit (symbol-function 'niwa-acp-prompt))
             steered prompts decisions submissions)
        (write-region "Evidence" nil file nil 'silent)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert (base64-decode-string data))
          (let ((coding-system-for-write 'no-conversion))
            (write-region (point-min) (point-max) image nil 'silent)))
        (setq-local gptel-system-prompt "Steering fixture"
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (when (eq mode 'held)
          (niwa-session-set-pending-input-paused session t))
        (cl-labels ((steer ()
                      (unless steered
                        (setq steered t)
                        (niwa-session-enqueue-pending-input
                         session 'steering
                         (list :input input :model-input input
                               :transcript-payload input
                               :request-id (niwa-request-id request))))))
          (cl-letf (((symbol-function 'niwa-acp-prompt)
                     (lambda (&rest args)
                       (push (copy-tree (cadr args)) submissions)
                       (apply submit args)))
                    ((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (_system _mcp _model _effort &optional _id hook)
                        (list :observe (lambda (owner event)
                                         (when (equal "user" (map-nested-elt event '(params message type)))
                                           (push (copy-tree (map-nested-elt event '(params message message content))) prompts))
                                         (niwa-claude-code--observe owner event))
                              :control (lambda (owner event)
                                         (unless final (steer))
                                         (let ((decision (niwa-claude-code--control owner event)))
                                           (push decision decisions)
                                           decision))
                              :complete-prompt (lambda (owner outcome)
                                                 (when final (steer))
                                                 (niwa-claude-code--complete-prompt owner outcome))
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
            (niwa--send-request "Read the evidence")
            (niwa-test--await 5 "Steering turn did not settle"
              (not (niwa-turn-busy-p buffer)))))
        (ert-info ((format "mode=%S" mode))
          (should (eq (if (memq mode '(mismatch missing foreign nested split image-missing)) 'error 'success)
                      (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
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
             (should-not (niwa-session-pending-steering session))
             (should-not (niwa-session-pending-follow-ups session)))
            ((or 'mismatch 'missing 'foreign 'nested 'split 'image-missing)
             (should (eq 'failed-turn
                         (plist-get (car (niwa-session-pending-steering session)) :state))))
            ('held
             ;; A native turn cannot park: held steering leads the follow-ups.
             (should-not (niwa-session-pending-steering session))
             (should (equal '("STEER-4417")
                            (mapcar (lambda (entry) (plist-get entry :input))
                                    (niwa-session-pending-follow-ups session)))))))))))

(niwa-deftest niwa--send-request/claude-startup-steering (:quiet t)
  (dolist (ack '(t "image-one-missing" "authored-mismatch" "split"))
    (niwa-engine-test--with-claude-session
      (let* ((original (file-name-concat root "original.png"))
             (steering (file-name-concat root "steering.png"))
             (data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
             (input (format "Original image @file:{%s}" original))
             (steer (format "STARTUP-AUTHORED-1913 @file:{%s}" steering))
             (prepare (symbol-function 'niwa-claude-code-context-input))
             queued echoes)
        (dolist (file (list original steering))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (base64-decode-string data))
            (let ((coding-system-for-write 'no-conversion))
              (write-region (point-min) (point-max) file nil 'silent))))
        (setq-local gptel-system-prompt "Startup steering fixture" gptel-tools nil)
        (cl-letf (((symbol-function 'niwa-claude-code-context-input)
                   (lambda (owner text)
                     (prog1 (funcall prepare owner text)
                       (unless queued
                         (setq queued t)
                         (niwa-session-enqueue-pending-input
                          session 'steering (list :input steer :model-input steer :transcript-payload steer
                                                  :request-id (niwa-request-id request)))))))
                  ((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system _mcp _model _effort &optional _id _hook)
                      (list :observe (lambda (owner event)
                                       (when (equal "user" (map-nested-elt event '(params message type)))
                                         (push (copy-tree (map-nested-elt event '(params message message content))) echoes))
                                       (niwa-claude-code--observe owner event))
                            :meta `((promptAcknowledgement . ,ack)))))))
          (niwa--insert-user-turn input)
          (niwa--send-request input)
          (niwa-test--await 5 "Startup steering did not settle"
            (not (niwa-turn-busy-p buffer))))
        (ert-info ((format "startup ack=%S" ack))
          (should queued)
          (should (eq (if (eq ack t) 'success 'error)
                      (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
          (should (= (if (eq ack t) 1 0) (how-many "STARTUP-AUTHORED-1913" (point-min) (point-max))))
          (if (eq ack t)
              (progn
                (should-not (niwa-session-pending-steering session))
                (should (= 2 (seq-count (lambda (part) (equal "image" (alist-get 'type part))) (car echoes)))))
            (should (eq 'failed-turn (plist-get (car (niwa-session-pending-steering session)) :state)))))))))

(niwa-deftest gptel-send/claude-session (:quiet t)
  (dolist (route '(raw paired init))
    (niwa-engine-test--with-claude-session
      (niwa-request-end)
      (let ((installed (advice-member-p #'niwa-skills--gptel-send-advice
                                        'gptel-send))
            view)
        (unwind-protect
            (progn
              (setq-local gptel-system-prompt "Raw send fixture" gptel-tools nil)
              (when (eq route 'paired)
                (setq view (niwa-view--ensure buffer)))
              (unless installed
                (advice-add 'gptel-send :around #'niwa-skills--gptel-send-advice))
              (cl-letf (((symbol-function 'gptel-request)
                         (lambda (&rest _) (ert-fail "Subscription send reached the API")))
                        ((symbol-function 'niwa-claude-code-launch)
                         (niwa-engine-test--claude-launch
                          (lambda (_system mcp _model _effort &optional id _hook)
                            nil))))
                (if (eq route 'init)
                    (niwa-init "raw-init-evidence")
                  (niwa--insert-user-turn "raw-send-evidence")
                  (gptel-send))
                (niwa-test--await 5 "Raw send did not settle" (not (niwa-turn-busy-p buffer))))
              (should (string-search "answer:" (buffer-string)))
              (should (string-search
                       (if (eq route 'init) "raw-init-evidence" "raw-send-evidence")
                       (buffer-string)))
              (should (= 1 (niwa-session-turn-count session)))
              (should (eq 'ready (plist-get
                                 (alist-get "root" (niwa-session-external-conversations session)
                                            nil nil #'equal) :state))))
          (unless installed
            (advice-remove 'gptel-send #'niwa-skills--gptel-send-advice))
          (when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest gptel-send/claude-admission (:quiet t)
  (dolist (state '(busy read-only steering))
    (niwa-engine-test--with-claude-session
      (let ((installed (advice-member-p #'niwa-skills--gptel-send-advice 'gptel-send)))
        (unwind-protect
            (progn
              (when (eq state 'read-only)
                (niwa-request-end)
                (setq-local niwa-session--read-only-mode t))
              (unless installed
                (advice-add 'gptel-send :around #'niwa-skills--gptel-send-advice))
              (cl-letf (((symbol-function 'gptel-request)
                         (lambda (&rest _) (ert-fail "Rejected input reached API")))
                        ((symbol-function 'niwa-claude-code-launch)
                         (lambda (&rest _) (ert-fail "Rejected input launched an agent"))))
                (niwa--insert-user-turn "must remain unsent")
                (let ((error (should-error (gptel-send (and (eq state 'steering) 0))
                                          :type 'user-error)))
                  (should (string-match-p
                           (pcase state
                             ('busy "already active")
                             ('read-only "read-only")
                             ('steering "unavailable.*external"))
                           (error-message-string error)))))
              (unless (eq state 'read-only)
                (should (eq request niwa--current-request)))
              (should (= 0 (niwa-session-turn-count session))))
          (unless installed
            (advice-remove 'gptel-send #'niwa-skills--gptel-send-advice)))))))

(niwa-deftest niwa-view--forward-input-now/claude-image (:quiet t)
		 (pcase-dolist (`(,acknowledgement ,attachment) '((t mention) (:false mention) ("image-missing" mention)
								  ("image-resized" mention) (t context) (t both)))
		   (niwa-engine-test--with-claude-session
		    (niwa-request-end)
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
			    (niwa-chat-install-request-hooks)
			    (niwa-view--setup view buffer)
			    (cl-letf (((symbol-function 'niwa-claude-code-launch)
				       (niwa-engine-test--claude-launch
                                        (lambda (_system mcp _model _effort &optional id _hook)
					  (list :meta `((expectedImage . ,data)
						        (promptAcknowledgement . ,acknowledgement)))))))
			      (with-current-buffer view
				(niwa-view--forward-input-now
				 (if (eq attachment 'context) "Describe the attached image"
				   (format "Describe @file:{%s}" file)))
				(niwa-view-composer-insert "> Keep the next draft\nwith two lines"))
			      (niwa-test--await 5 "Image request did not settle"
                                (not (niwa-turn-busy-p buffer))))
			    (should (string-search "Image accepted" (buffer-string)))
			    (should (eq (if (member acknowledgement '(t "image-resized")) 'ready 'uncertain)
					(plist-get (alist-get "root" (niwa-session-external-conversations session)
							      nil nil #'equal) :state)))
			    (should (= (if (member acknowledgement '(t "image-resized"))
					   (if (eq attachment 'both) 2 1)
					 0)
				       (hash-table-count (niwa-session-mentions-shown session))))
			    (should (= 1 (niwa-session-turn-count session)))
			    (should-not (string-search data (buffer-string)))
			    (with-current-buffer view
			      (should (equal "> Keep the next draft\nwith two lines" (niwa-view-composer-draft-text)))))
			(when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest niwa-agent-control-spawn/claude-image (:quiet t)
		 (niwa-engine-test--with-claude-session
		  (let ((file (file-name-concat root "sample.png"))
			(data "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aS1sAAAAASUVORK5CYII=")
			record invocation)
		    (with-temp-buffer
		      (set-buffer-multibyte nil)
		      (insert (base64-decode-string data))
		      (let ((coding-system-for-write 'no-conversion))
			(write-region (point-min) (point-max) file nil 'silent)))
		    (unwind-protect
			(cl-letf (((symbol-function 'niwa-claude-code-launch)
				   (niwa-engine-test--claude-launch
                                    (lambda (_system mcp _model _effort &optional id _hook)
				      (list :meta `((expectedImage . ,data)))))))
			  (dotimes (phase 2)
			    (let ((prompt (format "Describe @file:{%s}" file)))
			      (if (= phase 0)
				  (niwa-agent-control-spawn
				   session "image" prompt
				   (lambda (value) (setq record (plist-get value :record)))
				   :agent (niwa-agent--create :name "image" :description "Describe an image"
								 :tools nil :max-turns 2 :system-prompt "Describe the image")
				   :on-invocation (lambda (value) (setq invocation value)))
				(niwa-agent-control-send-message session "/root/image" prompt)
				(setq invocation (niwa-agent-record-invocation record))))
			    (niwa-test--await 5 "Image child did not settle"
                              (niwa-agent-invocation-runtime-settled-p invocation))
			    (should (eq 'completed (niwa-agent-record-settled-outcome record)))
			    (should (string-suffix-p "Image accepted" (niwa-agent-record-settled-result record)))
			    (should (= 0 (hash-table-count (niwa-session-mentions-shown session))))))
		      (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
			(kill-buffer (niwa-agent-record-conversation-buffer record)))))))

(provide 'test-niwa-claude-code-input)
;;; test-niwa-claude-code-input.el ends here
