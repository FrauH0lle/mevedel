;;; test-mevedel-directive-request.el --- Tests for mevedel-directive-request.el -*- lexical-binding: t -*-

;;; Commentary:

;; The directive request lifecycle: prompt construction, admission,
;; dispatch, terminal settlement, and the user-facing directive commands
;; that drive it.

;;; Code:

(require 'mevedel-directive-request)
(require 'mevedel-chat)
(require 'mevedel)
(require 'mevedel-prompt-submission)
(require 'mevedel-tool-render-data)
(require 'mevedel-view-zone)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar gptel--known-presets)

(mevedel-deftest mevedel--request-changes-prompt ()
  ,test
  (test)
  :doc "uses fresh context and only the immediately preceding attempt"
  (let* ((older
          (mevedel-directive-attempt--create
           :directive-request "Original" :request "Older exact request"
           :result "Older answer" :outcome 'success :patch "older patch"
           :capture 'complete :captured-at "2026-07-01T10:00:00+0200"))
         (latest
         (mevedel-directive-attempt--create
           :directive-request "Current request" :request "Latest exact request"
           :result "Latest answer" :outcome 'success :patch "latest patch"
           :plan "Latest accepted plan"
           :capture 'incomplete :captured-at "2026-07-02T11:30:00+0200"))
         (directive
          (mevedel-directive--create
           :id "directive" :request "Current request"
           :anchor '(:state attached) :state 'implemented
           :attempts (list older latest)))
         (prompt
          (mevedel--request-changes-prompt
           "Current request\n\nFresh reference" directive
           "> change one\nchange two")))
    (dolist (text '("Current repository state is authoritative"
                    "Fresh reference" "> change one\nchange two"
                    "Latest answer" "latest patch"
                    "2026-07-02T11:30:00+0200" "INCOMPLETE"
                    "historical" "Latest accepted plan"))
      (should (string-search text prompt)))
    (dolist (text '("Older exact request" "Older answer" "older patch"
                    "Latest exact request"))
      (should-not (string-search text prompt))))

  :doc "requires feedback unless unconsumed subdirectives provide changes"
  (let ((directive
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :state 'implemented
          :attempts
          (list
           (mevedel-directive-attempt--create
            :directive-request "Request" :request "Exact"
            :result "Done" :outcome 'success :patch "" :capture 'complete
            :captured-at "2026-07-02T12:00:00+0200")))))
    (should-error
     (mevedel--request-changes-prompt "Request" directive "")
     :type 'user-error)
    (should
     (string-search
      "newly supplied directive context"
      (mevedel--request-changes-prompt "Request\nChild change" directive nil
                                       t)))
    (mevedel-directive-set-request directive "Edited request")
    (should-error
     (mevedel--request-changes-prompt "Request" directive "Feedback")
     :type 'user-error)))

(mevedel-deftest mevedel--retry-directive-prompt ()
  ,test
  (test)
  :doc "uses the latest error and observed partial changes with optional guidance"
  (let* ((directive
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)
           :state 'failed
           :attempts
           (list
            (mevedel-directive-attempt--create
             :directive-request "Request" :request "Older"
             :result "Old failure" :outcome 'error :patch "old patch"
             :capture 'complete :captured-at "2026-07-01T10:00:00+0200")
            (mevedel-directive-attempt--create
             :directive-request "Request" :request "SECRET LATEST REQUEST"
             :result "Latest failure" :outcome 'error :patch "partial patch"
             :plan "Failed accepted plan"
             :capture 'incomplete
             :captured-at "2026-07-02T11:30:00+0200"))))
         (guided
          (mevedel--retry-directive-prompt
           "Request\n\nFresh reference" directive "Try the parser first"))
         (unguided
          (mevedel--retry-directive-prompt
           "Request\n\nFresh reference" directive "")))
    (dolist (text '("Current repository state is authoritative"
                    "Fresh reference" "Latest failure" "partial patch"
                    "Try the parser first" "Failed accepted plan"))
      (should (string-search text guided)))
    (dolist (text '("Old failure" "old patch" "SECRET LATEST REQUEST"))
      (should-not (string-search text guided)))
    (should-not (string-search "OPTIONAL GUIDANCE" unguided))
    (mevedel-directive-set-request directive "Edited request")
    (should-error
     (mevedel--retry-directive-prompt "Request" directive "")
     :type 'user-error)))

(mevedel-deftest mevedel--attach-directive-skills ()
  ,test
  (test)
  :doc "appends selected skills and validates them against the session"
  (progn
    (let* ((source "/tmp/alpha/SKILL.md")
           (skill (mevedel-skill--create
                   :name "alpha" :source-file source
                   :user-invocable-p t :active-p t))
           (session (mevedel-session--create :name "main"
                                             :skills (list skill)))
           (record (mevedel-directive--create
                    :id "directive" :request "Request"
                    :skills (list (list :name "alpha"
                                        :source-file source))))
           (chat-buffer (generate-new-buffer " *directive-skills-chat*")))
      (unwind-protect
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session)
            (cl-letf (((symbol-function 'mevedel-skills-input-prepare-user-input)
                       (lambda (input _) input)))
              (let ((result (mevedel--attach-directive-skills
                             "PROMPT" record chat-buffer)))
                (should (string-search "Implementation skills:" result))
                (should (string-search "Use $alpha" result)))
              (setf (mevedel-session-skills session) nil)
              (should-error
               (mevedel--attach-directive-skills
                "PROMPT" record chat-buffer))))
        (kill-buffer chat-buffer))))

  :doc "returns the prompt unchanged without a selection"
  (let ((record (mevedel-directive--create :id "d" :request "r"))
        (chat-buffer (generate-new-buffer " *directive-skills-none*")))
    (unwind-protect
        (should (equal "PROMPT"
                       (mevedel--attach-directive-skills
                        "PROMPT" record chat-buffer)))
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-discuss-directive
  (:doc "opens shared directive scope and submits the initial discussion")
  (let ((directive (make-overlay (point-min) (point-min)))
        scoped started)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--instructions-at)
                   (lambda (&rest _) (list directive)))
                  ((symbol-function 'mevedel--highest-priority-instruction)
                   (lambda (instructions &optional _) (car instructions)))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (instruction _) instruction))
                  ((symbol-function 'mevedel--directive-record)
                   (lambda (_) (mevedel-directive--create)))
                  ((symbol-function 'mevedel-view-enter-directive-scope)
                   (lambda (selected action &rest _)
                     (setq scoped (list selected action))))
                  ((symbol-function 'mevedel--directive-status)
                   (lambda (_) nil))
                  ((symbol-function 'mevedel--start-directive-discussion)
                   (lambda (selected &rest _)
                     (setq started selected)
                     'accepted)))
          (should (eq 'accepted (mevedel-discuss-directive)))
          (should (equal (list directive 'discuss) scoped))
          (should (eq directive started)))
      (delete-overlay directive))))

(mevedel-deftest mevedel-implement-directive ()
  ,test
  (test)

  :doc "starts planning instead of implementation when the directive opts in"
  (let ((directive (make-overlay (point-min) (point-min)))
        (record
         (mevedel-directive--create
          :request "Request" :planning-enabled t))
        planned processed)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--instructions-at)
                   (lambda (&rest _) (list directive)))
                  ((symbol-function 'mevedel--highest-priority-instruction)
                   (lambda (instructions &optional _) (car instructions)))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (instruction _) instruction))
                  ((symbol-function 'mevedel--directive-record)
                   (lambda (_) record))
                  ((symbol-function 'mevedel-directive-plan-start)
                   (lambda (selected action prompt-fn callback &optional _)
                     (setq planned
                           (list selected action prompt-fn callback))))
                  ((symbol-function 'mevedel--process-directive)
                   (lambda (&rest _) (setq processed t))))
          (mevedel-implement-directive #'ignore)
          (should (equal (list directive 'implement
                               #'mevedel--implement-directive-prompt #'ignore)
                         planned))
          (should-not processed))
      (delete-overlay directive)))

  :doc "rejects a fresh implementation after an attempt exists"
  (let ((directive (make-overlay (point-min) (point-min)))
        (record
         (mevedel-directive--create
          :request "Request"
          :attempts
          (list (mevedel-directive-attempt--create
                 :directive-request "Request" :outcome 'success))))
        processed)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--instructions-at)
                   (lambda (&rest _) (list directive)))
                  ((symbol-function 'mevedel--highest-priority-instruction)
                   (lambda (instructions &optional _) (car instructions)))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (instruction _) instruction))
                  ((symbol-function 'mevedel--directive-record)
                   (lambda (_) record))
                  ((symbol-function 'mevedel--process-directive)
                   (lambda (&rest _) (setq processed t))))
          (should-error (mevedel-implement-directive) :type 'user-error)
          (should-not processed))
      (delete-overlay directive))))

(mevedel-deftest mevedel--directive-save-buffer-p ()
  ,test
  (test)

  :doc "accepts normal modified file buffers"
  (with-temp-buffer
    (setq buffer-file-name "/tmp/mevedel-source.el")
    (set-buffer-modified-p t)
    (should (mevedel--directive-save-buffer-p)))

  :doc "skips mevedel data and agent transcript buffers"
  (with-temp-buffer
    (setq buffer-file-name "/tmp/segment-0001.chat.org")
    (setq-local mevedel--session 'session)
    (set-buffer-modified-p t)
    (should-not (mevedel--directive-save-buffer-p)))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/agents/explorer.chat.org")
    (setq-local mevedel--agent-invocation 'invocation)
    (set-buffer-modified-p t)
    (should-not (mevedel--directive-save-buffer-p))))

(mevedel-deftest mevedel--directive-model-policy ()
  ,test
  (test)

  :doc "returns nil when the directive inherits its session model"
  (with-temp-buffer
    (insert "directive")
    (should-not
     (mevedel--directive-model-policy
      (make-overlay (point-min) (point-max)))))

  :doc "resolves and validates a pinned provider and effort"
  (with-temp-buffer
    (insert "directive")
    (let ((directive (make-overlay (point-min) (point-max)))
          (backend (list :backend 'directive))
          validated)
      (overlay-put directive
                   'mevedel-directive-model-provider "Directive:model")
      (overlay-put directive
                   'mevedel-directive-reasoning-effort 'high)
      (cl-letf (((symbol-function 'mevedel-model-resolve-provider)
                 (lambda (provider &optional _)
                   (should (equal provider "Directive:model"))
                   (list :backend backend :model 'model)))
                ((symbol-function 'mevedel-model-validate-effort)
                 (lambda (model effort)
                   (setq validated (list model effort))
                   effort)))
        (should
         (equal (list :backend backend :model 'model :effort 'high)
                (mevedel--directive-model-policy directive)))
        (should (equal '(model high) validated))))))

(mevedel-deftest mevedel--process-directive
		 (:before-each (mevedel-workspace-clear-registry)
			       :after-each (mevedel-workspace-clear-registry))
		 ,test
		 (test)

		 :doc "isolates the prompt, binds the session, and settles a durable attempt"
		 (let* ((tmpdir (file-name-as-directory
				 (make-temp-file "mevedel-directive-" t)))
			(file (file-name-concat tmpdir "sample.txt"))
			(buf (find-file-noselect file))
                        (override-backend (list :backend 'directive))
                        (override-model 'directive-model)
			captured-prompt captured-args captured-fsm captured-chat
			callback-result override-validated later-child)
		   (unwind-protect
		       (with-current-buffer buf
			 (erase-buffer)
			 (insert "alpha\nbeta\n")
			 (write-region (point-min) (point-max) file nil 'silent)
			 (set-buffer-modified-p nil)
			 (goto-char (point-min))
			 (let* ((directive (mevedel--create-directive-in
					    buf (point-min) (line-end-position)
					    nil "Change alpha."))
				(submitted-child
				 (mevedel--create-directive-in
				  buf (1+ (point-min)) (+ 3 (point-min))
				  nil "Use the first detail."))
				(submitted-id
				 (overlay-get submitted-child 'mevedel-uuid))
				(older-attempt
				 (mevedel-directive-attempt--create
				  :directive-request "Older request"
				  :request "Older exact request" :result "Older result"
				  :outcome 'error :patch "" :capture 'complete
				  :captured-at "2026-08-01T01:00:00+0200"
				  :checkpoint '(:session-id "older" :turn 1))))
			   (setf (mevedel-directive-attempts
				  (mevedel--directive-record directive))
				 (list older-attempt))
			   (overlay-put directive 'mevedel-directive-action 'implement)
                           (overlay-put directive
                                        'mevedel-directive-model-provider
                                        "Directive:directive-model")
                           (overlay-put directive
                                        'mevedel-directive-reasoning-effort
                                        'high)
			   (cl-letf (((symbol-function 'save-some-buffers)
				      (lambda (&rest _) nil))
				     ((symbol-function 'display-buffer)
				      (lambda (&rest _) nil))
				     ((symbol-function 'gptel--save-state)
				      #'ignore)
				     ((symbol-function 'gptel--apply-preset)
				      (lambda (&rest _) nil))
                                     ((symbol-function
                                       'mevedel-model-resolve-provider)
                                      (lambda (provider &optional _)
                                        (should
                                         (equal provider
                                                "Directive:directive-model"))
                                        (list :backend override-backend
                                              :model override-model)))
                                     ((symbol-function
                                       'mevedel-model-validate-effort)
                                      (lambda (model effort)
                                        (when (eq model override-model)
                                          (should (eq effort 'high))
                                          (setq override-validated t))
                                        effort))
				     ((symbol-function 'gptel-request)
				      (lambda (prompt &rest args)
					(setq captured-prompt prompt
					      captured-args args
					      captured-chat (plist-get args :buffer))
					(let ((fsm (plist-get args :fsm)))
					  (setf (gptel-fsm-info fsm)
						(list :buffer captured-chat
						      :position (plist-get args :position)
						      :callback (lambda (&rest _) nil)))
					  (setq captured-fsm fsm)
					  fsm))))
			     (mevedel--process-directive
			      submitted-child '(:system "test")
			      #'mevedel--implement-directive-prompt
			      (lambda (err fsm)
				(setq callback-result
				      (list err (eq fsm captured-fsm)))))
			     (should (string-match-p "IMPLEMENTATION REQUEST"
						     captured-prompt))
			     (should (string-match-p "Change alpha" captured-prompt))
			     (should (string-match-p "Use the first detail" captured-prompt))
			     (should (eq captured-chat (plist-get captured-args :buffer)))
			     (should (markerp (plist-get captured-args :position)))
                             (should override-validated)
                             (let* ((transforms
                                     (plist-get captured-args :transforms))
                                    (directive-transform
                                     (car (last transforms)))
                                    session-backend session-model session-effort)
                               (with-current-buffer captured-chat
                                 (setq session-backend gptel-backend
                                       session-model gptel-model
                                       session-effort gptel-reasoning-effort))
                               (with-temp-buffer
                                 (setq-local gptel-backend 'prompt-backend
                                             gptel-model 'prompt-model
                                             gptel-reasoning-effort 'low)
                                 (funcall directive-transform nil)
                                 (should (eq gptel-backend override-backend))
                                 (should (eq gptel-model override-model))
                                 (should (eq gptel-reasoning-effort 'high)))
                               (with-current-buffer captured-chat
                                 (should (eq gptel-backend session-backend))
                                 (should (eq gptel-model session-model))
                                 (should
                                  (eq gptel-reasoning-effort
                                      session-effort))))
			     (with-current-buffer captured-chat
			       (should (eq 'implementing
					   (mevedel--directive-status directive)))
			       (should (equal (overlay-get directive 'mevedel-uuid)
					      mevedel--current-directive-uuid))
			       (save-excursion
				 (goto-char (point-min))
				 (should (search-forward ":PROMPT:" nil t))
				 (should-not (get-text-property (point) 'gptel))))
			     (let ((view-buf (buffer-local-value 'mevedel--view-buffer
								 captured-chat)))
			       (should (buffer-live-p view-buf))
			       (with-current-buffer view-buf
				 (let ((view-text (buffer-substring-no-properties
						   (point-min) mevedel-view--input-marker)))
				   (should (string-match-p "Implement: Change alpha"
							   view-text))
				   (should (string-match-p "Prompt" view-text)))
				 (goto-char (point-min))
				 (search-forward "Implement:")
				 (should (eq 'mevedel-view-directive-action
					     (get-text-property (match-beginning 0)
								'font-lock-face)))
				 (should (markerp mevedel-view--in-flight-turn-start))
				 (should (markerp mevedel-view--data-turn-start))
				 (should (overlayp
					  (mevedel-view-zone-region 'progress)))))
			     (let ((response-start
				    (plist-get captured-args :position)))
			       (should (markerp response-start))
			       (with-current-buffer captured-chat
				 (goto-char response-start)
				 (insert (propertize "Answer text.\n" 'gptel 'response)))
			       (let ((view-buf (buffer-local-value 'mevedel--view-buffer
								   captured-chat)))
				 (with-current-buffer view-buf
				   (mevedel-view-render-live-update captured-chat)))
			       (let ((view-buf (buffer-local-value 'mevedel--view-buffer
								   captured-chat)))
				 (with-current-buffer view-buf
				   (should (string-search "Answer text"
							  (buffer-string))))))
			     (setf (gptel-fsm-info captured-fsm)
				   (append
				    (gptel-fsm-info captured-fsm)
				    '(:mevedel-directive-patch "diff --git a/sample.txt b/sample.txt\n"
				      :mevedel-directive-capture complete
				      :mevedel-directive-covered-files ("/tmp/sample.txt")
				      :mevedel-directive-gaps nil)))
			     (setq later-child
				   (mevedel--create-directive-in
				    buf (+ 3 (point-min)) (+ 5 (point-min))
				    nil "Keep later detail."))
			     (mevedel--remove-directive-presentation submitted-child)
				     (funcall (plist-get (gptel-fsm-info captured-fsm)
							 :mevedel-request-callback)
					      nil captured-fsm)
				     (with-current-buffer captured-chat
				       (mevedel--turn-commit captured-fsm)
				       (mevedel-request-end))
				     (should (equal '(nil t) callback-result))
			     (should (eq 'implemented
					 (mevedel--directive-status directive)))
			     (let* ((record (mevedel--directive-record directive))
				    (attempt
				     (car (last (mevedel-directive-attempts record))))
				    (checkpoint
				     (mevedel-directive-attempt-checkpoint attempt)))
			       (should (= 2 (length
					     (mevedel-directive-attempts record))))
			       (should (eq older-attempt
					   (car (mevedel-directive-attempts record))))
			       (should (equal (mevedel-directive-session-id record)
					      (plist-get checkpoint :session-id)))
			       (should (= 1 (plist-get checkpoint :turn)))
			       (should (equal captured-prompt
					      (mevedel-directive-attempt-request attempt)))
			       (should (eq 'implement
					   (mevedel-directive-attempt-action attempt)))
			       (should (equal "Change alpha."
					      (mevedel-directive-attempt-directive-request
					       attempt)))
			       (should
				(stringp
				 (mevedel-directive-attempt-captured-at attempt)))
			       (should (equal "Answer text.\n"
					      (mevedel-directive-attempt-result attempt)))
			       (should (eq 'success
					   (mevedel-directive-attempt-outcome attempt)))
			       (should (eq 'complete
					   (mevedel-directive-attempt-capture attempt)))
			       (should
				(equal
				 (list submitted-id)
				 (mapcar
				  #'mevedel-subdirective-id
				  (mevedel-directive-attempt-consumed-subdirectives
				   attempt))))
			       (should-not (overlay-buffer submitted-child))
			       (should (overlay-buffer later-child))
			       (should
				(equal
				 (list (overlay-get later-child 'mevedel-uuid))
				 (mapcar #'mevedel-subdirective-id
					 (mevedel-directive-subdirectives record)))))
			     (with-current-buffer captured-chat
			       (should (string-search ":PROMPT:" (buffer-string)))
			       (should (string-search "Answer text" (buffer-string)))
			       (should (= 2 (length
					     (mevedel-transcript-audit-records
					      (buffer-string)
					      'directive-turn-boundary))))
                               (mevedel-session-artifacts-save
				mevedel--session captured-chat)
			       (let ((segment buffer-file-name))
				 (with-temp-buffer
				   (insert-file-contents segment)
				   (org-mode)
				   (mevedel-transcript-restore-properties)
				   (should (= 2 (length
						 (mevedel-transcript-audit-records
						  (buffer-string)
						  'directive-turn-boundary))))
				   (should (string-search "Answer text"
						  (buffer-string)))))
			       (should-not mevedel--current-directive-uuid)))))
		     (when (buffer-live-p buf)
		       (kill-buffer buf))
		     (when (and captured-chat (buffer-live-p captured-chat))
		       (let ((view-buf (buffer-local-value 'mevedel--view-buffer
							   captured-chat)))
			 (when (buffer-live-p view-buf)
			   (kill-buffer view-buf)))
		       (kill-buffer captured-chat))
			     (delete-directory tmpdir t)))

		 :doc "records failure and abort outcomes while retaining full turns"
		 (dolist (case '((error failed error "transport failed")
				 (abort aborted aborted "Request aborted")))
		   (pcase-let* ((`(,kind ,state ,outcome ,result) case)
				(tmpdir (file-name-as-directory
					 (make-temp-file "mevedel-directive-terminal-" t)))
				(file (file-name-concat tmpdir "sample.txt"))
				(gap-file (file-name-concat tmpdir "gap.txt"))
				(buf (find-file-noselect file))
				(captured-fsm nil)
				(captured-chat nil)
				(failure-called nil)
				(provider-aborted nil))
		     (unwind-protect
			 (with-current-buffer buf
			   (insert "source\n")
			   (write-region (point-min) (point-max) file nil 'silent)
			   (set-buffer-modified-p nil)
			   (let* ((directive
				   (mevedel--create-directive-in
				    buf (point-min) (1- (point-max)) nil "Change it"))
				  (child
				   (mevedel--create-directive-in
				    buf (1+ (point-min)) (- (point-max) 2)
				    nil "Keep this detail")))
			     (overlay-put directive 'mevedel-directive-action 'implement)
			     (cl-letf (((symbol-function 'save-some-buffers)
					(lambda (&rest _) nil))
				       ((symbol-function 'display-buffer)
					(lambda (&rest _) nil))
				       ((symbol-function 'gptel--apply-preset)
					(lambda (&rest _) nil))
				       ((symbol-function 'gptel-request)
					(lambda (_prompt &rest args)
					  (setq captured-chat (plist-get args :buffer))
					  (let ((fsm (plist-get args :fsm)))
					    (setf (gptel-fsm-info fsm)
						  (list :buffer captured-chat
							:position (plist-get args :position)
							:callback (lambda (&rest _) nil)))
					    (setq captured-fsm fsm)
					    fsm))))
			       (mevedel--process-directive
				directive '(:system "test")
				#'mevedel--implement-directive-prompt nil)
			       (if (eq kind 'abort)
				   (progn
				     (with-current-buffer captured-chat
				       (puthash
					file "source\n"
					(mevedel-request-file-snapshots
					 mevedel--current-request))
				       (puthash
					gap-file '(:gap "capture denied")
					(mevedel-request-file-snapshots
					 mevedel--current-request)))
				     (cl-letf
					 (((symbol-function
					    'ask-user-about-supersession-threat)
					   (lambda (&rest _) nil)))
				       (with-temp-file file
					 (insert "changed\n")))
				     (let ((gptel--request-alist
					    (list
					     (cons
					      'directive
					      (cons
					       captured-fsm
					       (lambda ()
						 (setq provider-aborted t)))))))
				       (cl-letf
					   (((symbol-function
					      'mevedel--fail-turn)
					     (lambda (_fsm status)
					       (setq failure-called status)))
					    ((symbol-function
					      'mevedel--replace-patch-buffer)
					     #'ignore))
					 (mevedel-test--with-captured-messages
					  nil
					  (gptel-abort captured-chat)))))
				 (setf (gptel-fsm-state captured-fsm) 'ERRS
				       (gptel-fsm-info captured-fsm)
				       (plist-put
					(gptel-fsm-info captured-fsm)
					:error '(:message "transport failed")))
				 (funcall
				  (plist-get (gptel-fsm-info captured-fsm)
					     :mevedel-request-callback)
				  nil captured-fsm))
			       (when (eq kind 'abort)
				 (should provider-aborted)
				 (should (eq 'aborted failure-called)))
			       (with-current-buffer captured-chat
				 (mevedel--turn-commit captured-fsm)
				 (mevedel-request-end))
			       (let* ((record (mevedel--directive-record directive))
				      (attempt
				       (car (mevedel-directive-attempts record))))
				 (should (eq state (mevedel-directive-state record)))
				 (should
				  (eq outcome
				      (mevedel-directive-attempt-outcome attempt)))
				 (should
				  (equal result
					 (mevedel-directive-attempt-result attempt)))
				 (when (eq kind 'abort)
				   (should
				    (string-match-p
				     "changed"
				     (mevedel-directive-attempt-patch attempt)))
				   (should
				    (eq 'incomplete
					(mevedel-directive-attempt-capture
					 attempt)))
				   (should
				    (equal
				     (list file)
				     (mevedel-directive-attempt-covered-files
				      attempt)))
				   (should
				    (equal
				     (list (cons gap-file "capture denied"))
				     (mevedel-directive-attempt-gaps attempt))))
				 (should-not
				  (mevedel-directive-attempt-consumed-subdirectives
				   attempt))
				 (should (overlay-buffer child))
				 (should (= 1 (length
					       (mevedel-directive-subdirectives record)))))
			       (with-current-buffer captured-chat
				 (should (= 2 (length
					       (mevedel-transcript-audit-records
						(buffer-string)
						'directive-turn-boundary))))
				 (should (string-search ":PROMPT:"
							(buffer-string)))))))
		       (when (buffer-live-p buf)
			 (kill-buffer buf))
		       (when (buffer-live-p captured-chat)
			 (let ((view-buf
				(buffer-local-value 'mevedel--view-buffer
						    captured-chat)))
			   (when (buffer-live-p view-buf)
			     (kill-buffer view-buf)))
			 (kill-buffer captured-chat))
		       (delete-directory tmpdir t))))

                 :doc "starts discussion from the directive as a first-class turn"
                 (let* ((tmpdir (file-name-as-directory
                                 (make-temp-file "mevedel-discussion-turn-" t)))
                        (file (file-name-concat tmpdir "sample.txt"))
                        (buf (find-file-noselect file))
                        (mevedel-action-preset-alist
                         '((discuss . (:system "test"))))
                        captured-fsm captured-chat captured-prompt)
                   (unwind-protect
                       (with-current-buffer buf
                         (insert "source\n")
                         (write-region (point-min) (point-max) file nil 'silent)
                         (set-buffer-modified-p nil)
                         (let ((directive
                                (mevedel--create-directive-in
                                 buf (point-min) (1- (point-max)) nil
                                 "Explain it")))
                           (overlay-put directive 'mevedel-directive-action 'discuss)
                           (cl-letf (((symbol-function 'save-some-buffers)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'display-buffer)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel--apply-preset)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel-request)
                                      (lambda (prompt &rest args)
                                        (setq captured-prompt prompt
                                              captured-chat
                                              (plist-get args :buffer))
                                        (let ((fsm (plist-get args :fsm)))
                                          (setf (gptel-fsm-info fsm)
                                                (list
                                                 :buffer captured-chat
                                                 :position
                                                 (plist-get args :position)
                                                 :callback
                                                 (lambda (&rest _) nil)))
                                          (setq captured-fsm fsm)
                                          fsm))))
                             (mevedel--start-directive-discussion
                              directive nil)
                             (should (eq 'discussing
                                         (mevedel--directive-status directive)))
                             (let ((response-start
                                    (plist-get (gptel-fsm-info captured-fsm)
                                               :position)))
                               (with-current-buffer captured-chat
                                 (goto-char response-start)
                                 (insert (propertize "Because.\n"
                                                     'gptel 'response))))
                             (funcall
                              (plist-get (gptel-fsm-info captured-fsm)
                                         :mevedel-request-callback)
                              nil captured-fsm)
                             (with-current-buffer captured-chat
                               (mevedel--turn-commit captured-fsm)
                               (mevedel-request-end))
                             (let* ((record
                                     (mevedel--directive-record directive))
                                    (turn
                                     (car
                                      (mevedel-directive-discussion record))))
                               (should (eq 'discussed
                                           (mevedel-directive-state record)))
                               (should-not
                                (mevedel-directive-attempts record))
                               (should (equal "Explain it"
                                              (mevedel-directive-discussion-turn-message
                                               turn)))
                               (should (equal "Explain it"
                                              (mevedel-directive-discussion-turn-directive-request
                                               turn)))
                               (should (equal captured-prompt
                                              (mevedel-directive-discussion-turn-request
                                               turn)))
                               (should-not
                                (string-match-p "### QUESTION:" captured-prompt))
                               (should (equal "Because.\n"
                                              (mevedel-directive-discussion-turn-result
                                               turn)))
                               (should (= 1
                                          (mevedel-directive-discussion-turn-sequence
                                           turn)))
                               (setf
                                (mevedel-directive-attempts record)
                                (list
                                 (mevedel-directive-attempt--create
                                  :sequence 2
                                  :action 'implement
                                  :directive-request "Explain it"
                                  :request "Implement it"
                                  :result "Done."
                                  :outcome 'success
                                  :patch ""
                                  :capture 'complete
                                  :captured-at "2026-08-01T01:00:00+0200"))
                                (mevedel-directive-state record) 'implemented)
                               (mevedel--discuss-directive-turn
                                directive "Anything else?" 1 nil)
                               (let ((response-start
                                      (plist-get
                                       (gptel-fsm-info captured-fsm)
                                       :position)))
                                 (with-current-buffer captured-chat
                                   (goto-char response-start)
                                   (insert (propertize "No.\n"
                                                       'gptel 'response))))
                               (funcall
                                (plist-get (gptel-fsm-info captured-fsm)
                                           :mevedel-request-callback)
                                nil captured-fsm)
                               (should (eq 'implemented
                                           (mevedel-directive-state record)))
                               (should (= 3
                                          (mevedel-directive-discussion-turn-sequence
                                           (car
                                            (last
                                             (mevedel-directive-discussion
                                              record)))))))
                             (with-current-buffer captured-chat
                               (should (= 4 (length
                                             (mevedel-transcript-audit-records
                                              (buffer-string)
                                              'directive-turn-boundary))))
                               (should (string-search "Because."
                                                      (buffer-string)))
                               (should (string-search "No."
                                                      (buffer-string))))
                             (with-current-buffer captured-chat
                               (mevedel-request-end)
                               (mevedel-session-artifacts-save
                                mevedel--session captured-chat))))
                     (when (buffer-live-p buf)
                       (kill-buffer buf))
                     (when (buffer-live-p captured-chat)
                       (let ((view-buf
                              (buffer-local-value 'mevedel--view-buffer
                                                  captured-chat)))
                         (when (buffer-live-p view-buf)
                           (kill-buffer view-buf)))
                       (kill-buffer captured-chat))
                     (delete-directory tmpdir t))))

                 :doc "discards synthetic source context when startup is quit"
                 (let* ((workspace
                         (mevedel-workspace--create
                          :type 'file :id "source-missing" :root "/tmp"
                          :name "source-missing"))
                        (record
                         (mevedel-directive--create
                          :id "directive" :request "Request"
                          :anchor
                          '(:state source-missing :file "/tmp/missing.el"
                            :start 1 :end 1
                            :evidence (:schema 1 :bodyless t)
                            :properties
                            (mevedel-instruction t
                             mevedel-uuid "directive"
                             mevedel-instruction-type directive))))
                        (context
                         (progn
                           (mevedel-workspace-set-directives
                            workspace (list record))
                           (mevedel--directive-action-context
                            record workspace)))
                        (directive (plist-get context :directive))
                        (transient-buffer (overlay-buffer directive)))
                   (unwind-protect
                       (progn
                         (overlay-put directive
                                      'mevedel-directive-action 'implement)
                        (cl-letf
                            (((symbol-function
                               'mevedel--directive-session-buffer)
                               (lambda (&rest _)
                                 (signal 'quit nil))))
                           (let (quit-p)
                             (condition-case nil
                                 (mevedel--process-directive
                                  directive '(:system "test")
                                  #'mevedel--implement-directive-prompt nil)
                               (quit (setq quit-p t)))
                             (should quit-p)))
                         (should-not (buffer-live-p transient-buffer))
                         (should-not (overlay-buffer directive)))
                     (when (buffer-live-p transient-buffer)
                       (kill-buffer transient-buffer))))

                 :doc "rolls back reservation, status, and boundary on startup error"
                 (let* ((tmpdir (file-name-as-directory
                                 (make-temp-file "mevedel-directive-startup-" t)))
                        (file (file-name-concat tmpdir "sample.txt"))
                        (buf (find-file-noselect file))
                        captured-chat)
                   (unwind-protect
                       (with-current-buffer buf
                         (insert "source\n")
                         (write-region (point-min) (point-max) file nil 'silent)
                         (set-buffer-modified-p nil)
                         (let ((directive
                                (mevedel--create-directive-in
                                 buf (point-min) (1- (point-max)) nil
                                 "Change it")))
                           (overlay-put directive 'mevedel-directive-action
                                        'implement)
                           (cl-letf (((symbol-function 'save-some-buffers)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'display-buffer)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel--apply-preset)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel-request)
                                      (lambda (_prompt &rest args)
                                        (setq captured-chat
                                              (plist-get args :buffer))
                                        (error "Synchronous startup failure"))))
                             (should-error
                              (mevedel--process-directive
                               directive '(:system "test")
                               #'mevedel--implement-directive-prompt nil))
                             (should-not (mevedel--directive-status directive))
                             (with-current-buffer captured-chat
                               (should-not mevedel--current-request)
                               (should-not mevedel--current-directive-uuid)
                               (should-not mevedel--directive-read-only-request-p)
                               (should-not
                                (mevedel-transcript-audit-records
                                 (buffer-string)
                                 'directive-turn-boundary))))))
                     (when (buffer-live-p buf)
                       (kill-buffer buf))
                     (when (buffer-live-p captured-chat)
                       (let ((view-buf
                              (buffer-local-value 'mevedel--view-buffer
                                                  captured-chat)))
                         (when (buffer-live-p view-buf)
                           (kill-buffer view-buf)))
                       (kill-buffer captured-chat))
                     (delete-directory tmpdir t)))

                 :doc "preserves prior state and an unrelated active request on pre-reservation errors"
                 (let* ((workspace
                         (mevedel-workspace--create
                          :type 'file :id "pre-reservation" :root "/tmp"
                          :name "pre-reservation"))
                        (source (generate-new-buffer " *directive-source*"))
                        (chat (generate-new-buffer " *directive-chat*"))
                        (session (mevedel-session-create "main" workspace)))
                   (unwind-protect
                       (with-current-buffer source
                         (insert "source")
                         (setq-local mevedel--workspace workspace)
                         (let* ((directive
                                 (mevedel--create-directive-in
                                  source (point-min) (point-max) nil "Change it"))
                                (record (mevedel--directive-record directive)))
                           (overlay-put directive 'mevedel-directive-action
                                        'implement)
                           (setf (mevedel-directive-state record) 'implemented)
                           (with-current-buffer chat
                             (setq-local mevedel--session session))
                           (cl-letf (((symbol-function
                                      'mevedel--directive-session-buffer)
                                     (lambda (&rest _) (cons chat nil))))
                             (should-error
                              (mevedel--process-directive
                               directive '(:system "test")
                               (lambda (_) (error "Prompt construction failed"))
                               nil))
                             (should (eq 'implemented
                                         (mevedel-directive-state record)))
                             (let ((active
                                    (mevedel-request--create
                                     :session session :turn 9)))
                               (with-current-buffer chat
                                 (setq-local mevedel--current-request active
                                             mevedel--current-directive-uuid
                                             "other-directive"
                                             mevedel--directive-read-only-request-p
                                             t))
                               (should-error
                                (mevedel--process-directive
                                 directive '(:system "test") #'identity nil)
                                :type 'user-error)
                               (with-current-buffer chat
                                 (should (eq active mevedel--current-request))
                                 (should (equal "other-directive"
                                                mevedel--current-directive-uuid))
                                 (should mevedel--directive-read-only-request-p))))))
                     (kill-buffer source)
                     (kill-buffer chat)))

                 :doc "reuses the directive session even when another is current"
                 (let* ((workspace (mevedel-workspace--create
                                    :type 'file :id "bound" :root "/tmp"
                                    :name "bound"))
                        (bound-session (mevedel-session-create "bound" workspace))
                        (other-session (mevedel-session-create "other" workspace))
                        (bound-buffer (generate-new-buffer " *bound-session*"))
                        (other-buffer (generate-new-buffer " *other-session*"))
                        (record (mevedel-directive--create
                                 :id "directive" :request "Request"
                                 :anchor '(:state attached)
                                 :session-id "bound-id")))
                   (unwind-protect
                       (progn
                         (setf (mevedel-session-session-id bound-session)
                               "bound-id"
                               (mevedel-session-session-id other-session)
                               "other-id")
                         (with-current-buffer bound-buffer
                           (setq-local mevedel--session bound-session))
                         (with-current-buffer other-buffer
                           (setq-local mevedel--session other-session))
                         (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                                    (lambda (_workspace)
                                      (list (cons "other" other-buffer)
                                            (cons "bound" bound-buffer)))))
                           (should
                            (equal (cons bound-buffer nil)
                                   (mevedel--directive-session-buffer
                                    record workspace)))))
                     (kill-buffer bound-buffer)
                     (kill-buffer other-buffer))))

(mevedel-deftest mevedel--process-directive-detached-callback
  (:before-each (mevedel-workspace-clear-registry)
                :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "detached source still settles a successful inspectable attempt"
  (let* ((tmpdir (file-name-as-directory
                  (make-temp-file "mevedel-directive-detached-" t)))
         (file (file-name-concat tmpdir "sample.txt"))
         (buf (find-file-noselect file))
         captured-fsm captured-chat callback-result)
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "alpha\nbeta\n")
          (write-region (point-min) (point-max) file nil 'silent)
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (let ((directive (mevedel--create-directive-in
                            buf (point-min) (line-end-position)
                            nil "Change alpha.")))
            (overlay-put directive 'mevedel-directive-action 'implement)
            (cl-letf (((symbol-function 'save-some-buffers)
                       (lambda (&rest _) nil))
                      ((symbol-function 'display-buffer)
                       (lambda (&rest _) nil))
                      ((symbol-function 'gptel--apply-preset)
                       (lambda (&rest _) nil))
                      ((symbol-function 'gptel-request)
                       (lambda (_prompt &rest args)
                         (setq captured-chat (plist-get args :buffer))
                         (let ((fsm (plist-get args :fsm)))
                           (setf (gptel-fsm-info fsm)
                                 (list :buffer captured-chat
                                       :position (plist-get args :position)
                                       :callback (lambda (&rest _) nil)))
                           (setq captured-fsm fsm)
                           fsm))))
              (mevedel--process-directive
               directive '(:system "test")
               #'mevedel--implement-directive-prompt
               (lambda (err fsm)
                 (setq callback-result (list err (eq fsm captured-fsm)))))
              (delete-region (overlay-start directive)
                             (overlay-end directive))
              (let ((detached
                     (mevedel--find-directive-by-uuid
                      (mevedel-directive-id
                       (car (mevedel-workspace-directives
                             (with-current-buffer buf
                               (mevedel-workspace))))))))
                (should (overlay-buffer detached))
                (should (= (overlay-start detached) (overlay-end detached))))
              (funcall (plist-get (gptel-fsm-info captured-fsm)
                                  :mevedel-request-callback)
                       nil captured-fsm)
              (should (equal '(nil t) callback-result))
              (let* ((record (car (mevedel-workspace-directives
                                   (with-current-buffer buf
                                     (mevedel-workspace)))))
                     (attempt (car (mevedel-directive-attempts record))))
                (should (eq 'implemented (mevedel-directive-state record)))
                (should (eq 'success
                            (mevedel-directive-attempt-outcome attempt))))
              (with-current-buffer captured-chat
                (should-not mevedel--current-directive-uuid)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (and captured-chat (buffer-live-p captured-chat))
        (let ((view-buf (buffer-local-value 'mevedel--view-buffer
                                            captured-chat)))
          (when (buffer-live-p view-buf)
            (kill-buffer view-buf)))
        (kill-buffer captured-chat))
      (delete-directory tmpdir t))))


;;
;;; Abort

(mevedel-deftest mevedel--directive-session-buffer ()
  ,test
  (test)
  :doc "resumes the bound persisted session on demand"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "resume" :root "/tmp" :name "resume"))
         (record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :anchor '(:state attached) :session-id "saved-id"))
         (restored (generate-new-buffer " *restored-directive-session*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                   (lambda (_workspace) nil))
                  ((symbol-function 'mevedel-session-persistence-resume-id)
                   (lambda (seen-workspace seen-id)
                     (should (eq workspace seen-workspace))
                     (should (equal "saved-id" seen-id))
                     restored)))
          (should (equal (cons restored nil)
                         (mevedel--directive-session-buffer
                          record workspace))))
      (kill-buffer restored)))

  :doc "requires explicit rebind and leaves historical links unchanged"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "rebind" :root "/tmp" :name "rebind"))
         (attempt
          (mevedel-directive-attempt--create
           :checkpoint '(:session-id "lost-id" :turn 3)))
         (record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :anchor '(:state attached) :session-id "lost-id"
                  :attempts (list attempt)))
         (replacement (generate-new-buffer " *replacement-session*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                   (lambda (_workspace) nil))
                  ((symbol-function 'mevedel-session-persistence-resume-id)
                   (lambda (&rest _) nil))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'mevedel--chat-buffer)
                   (lambda (&rest _) replacement)))
          (should (equal (cons replacement t)
                         (mevedel--directive-session-buffer record workspace)))
          (should (equal "lost-id" (mevedel-directive-session-id record)))
          (should
           (equal '(:session-id "lost-id" :turn 3)
                  (mevedel-directive-attempt-checkpoint attempt))))
      (kill-buffer replacement)))

  :doc "keeps an unavailable binding when rebind is declined"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "decline" :root "/tmp" :name "decline"))
         (record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :anchor '(:state attached) :session-id "lost-id")))
    (cl-letf (((symbol-function 'mevedel--workspace-sessions)
               (lambda (_workspace) nil))
              ((symbol-function 'mevedel-session-persistence-resume-id)
               (lambda (&rest _) nil))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-error
       (mevedel--directive-session-buffer record workspace)
       :type 'user-error))))

(mevedel-deftest mevedel--directive-discussion-transcript
  (:doc "renders only current-request local messages and replies in order")
  (let ((record
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :discussion
          (list
           (mevedel-directive-discussion-turn--create
            :directive-request "Older request"
            :message "Stale" :request "Hidden stale request"
            :result "Stale answer" :outcome 'success)
           (mevedel-directive-discussion-turn--create
            :directive-request "Request"
            :message "One" :request "Hidden request one"
            :result (concat
                     "Answer one\n\n"
                     (mevedel-tool-render-data-format
                      '(:kind request-summary :elapsed-seconds 1.0)))
            :outcome 'success)
           (mevedel-directive-discussion-turn--create
            :directive-request "Request"
            :message "Two" :request "Hidden request two"
            :result "Transport failed" :outcome 'error)))))
    (should
     (equal "User: One\nAssistant: Answer one\n\nUser: Two\nAssistant (error): Transport failed"
            (mevedel--directive-discussion-transcript record)))))

(mevedel-deftest mevedel--discuss-directive-prompt
  (:doc "includes fresh context, complete local discussion, and selected result")
  (let* ((attempt
          (mevedel-directive-attempt--create
           :request "Implement exact" :result "Implementation answer"
           :outcome 'success :patch "diff --git a/a b/a\n"
           :capture 'complete :covered-files '("/tmp/a") :gaps nil
           :checkpoint '(:session-id "session-1" :turn 1)))
         (record
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)
           :attempts (list attempt)
           :discussion
           (list
            (mevedel-directive-discussion-turn--create
             :directive-request "Request"
             :message "First question" :request "Old exact request"
             :result "First answer" :outcome 'success
             :checkpoint '(:session-id "session-1" :turn 2)))))
         (prompt
          (mevedel--discuss-directive-prompt
           "Fresh directive and references" record "Follow up" 1)))
    (should (string-match-p "Fresh directive and references" prompt))
    (should (string-match-p "First question" prompt))
    (should (string-match-p "First answer" prompt))
    (should (string-match-p "Follow up" prompt))
    (should (string-match-p "Implementation answer" prompt))
    (should (string-match-p "diff --git" prompt))
    (should-not (string-match-p "Old exact request" prompt))))

(mevedel-deftest mevedel--implement-discussion-prompt
  (:doc "turns the complete local discussion into implementation feedback")
  (let* ((record
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)
           :state 'discussed
           :discussion
           (list
            (mevedel-directive-discussion-turn--create
             :directive-request "Request"
             :message "Prefer a small API" :request "Exact"
             :result "Use one entry point" :outcome 'success
             :checkpoint '(:session-id "session-1" :turn 1)))))
         (prompt
          (mevedel--implement-discussion-prompt "Fresh references" record)))
    (should (string-match-p "IMPLEMENTATION REQUEST" prompt))
    (should (string-match-p "Fresh references" prompt))
    (should (string-match-p "Prefer a small API" prompt))
    (should (string-match-p "Use one entry point" prompt))
    (setf (mevedel-directive-attempts record)
          (list (mevedel-directive-attempt--create
                 :directive-request "Request" :outcome 'success)))
    (should-error
     (mevedel--implement-discussion-prompt "Fresh references" record)
     :type 'user-error)))

(mevedel-deftest mevedel--directive-implementation-prompt
  (:doc "previews the complete prompt for the directive's next implementation")
  (let* ((attempt
          (mevedel-directive-attempt--create
           :directive-request "Request" :request "Exact"
           :result "Previous answer" :outcome 'success
           :patch "historical patch" :capture 'complete
           :captured-at "2026-08-02T01:00:00+0200"))
         (directive
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)
           :state 'implemented :attempts (list attempt)))
         (prompt
          (mevedel--directive-implementation-prompt
           "Fresh references" directive "Change the parser")))
    (dolist (text '("Fresh references" "Change the parser"
                    "Previous answer" "historical patch"))
      (should (string-search text prompt)))
    (setf (mevedel-directive-state directive) 'failed
          (mevedel-directive-attempt-outcome attempt) 'error
          (mevedel-directive-attempt-result attempt) "Transport failed")
    (setq prompt
          (mevedel--directive-implementation-prompt
           "Fresh references" directive "Try locally"))
    (should (string-search "Transport failed" prompt))
    (should (string-search "Try locally" prompt))
    (setf (mevedel-directive-request directive) "Edited request"
          (mevedel-directive-discussion directive)
          (list
           (mevedel-directive-discussion-turn--create
            :directive-request "Edited request"
            :message "Prefer the smaller API" :request "Exact discussion"
            :result "Agreed" :outcome 'success))
          (mevedel-directive-state directive) 'discussed)
    (setq prompt
          (mevedel--directive-implementation-prompt
           "Fresh references" directive))
    (should (string-search "DISCUSSION FEEDBACK" prompt))
    (should (string-search "Prefer the smaller API" prompt))))

(mevedel-deftest mevedel--implement-discussion
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global))
   :doc "dispatches ordinary implementation with complete discussion feedback")
  (let ((workspace (mevedel-workspace--create
                    :type 'file :id "implement-discussion" :root "/tmp"
                    :name "implement-discussion"))
        (mevedel-action-preset-alist
         '((implement . (:system "test"))))
        captured)
    (with-temp-buffer
      (insert "source")
      (setq-local mevedel--workspace workspace)
      (let* ((directive
              (mevedel--create-directive-in
               (current-buffer) (point-min) (point-max) nil "Request"))
             (record (mevedel--directive-record directive)))
        (setf (mevedel-directive-discussion record)
              (list
               (mevedel-directive-discussion-turn--create
                :directive-request "Request"
                :message "Keep it small" :request "Exact"
                :result "Agreed" :outcome 'success))
              (mevedel-directive-state record) 'discussed)
        (cl-letf (((symbol-function 'mevedel--process-directive)
                   (lambda (seen preset prompt-fn callback &optional options)
                     (setq captured
                           (list seen preset (funcall prompt-fn "Fresh")
                                 callback options))
                     'accepted)))
          (should (eq 'accepted
                      (mevedel--implement-discussion directive #'ignore)))
          (should (eq directive (nth 0 captured)))
          (should (equal '(:system "test") (nth 1 captured)))
          (should (string-match-p "Fresh" (nth 2 captured)))
          (should (string-match-p "Keep it small" (nth 2 captured)))
          (should (eq #'ignore (nth 3 captured)))
          (should-not (nth 4 captured)))))))

(mevedel-deftest mevedel--request-directive-changes
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "uses ordinary implementation authority and accepts child-only feedback"
  (let ((workspace (mevedel-workspace--create
                    :type 'file :id "changes" :root "/tmp" :name "changes"))
        (mevedel-action-preset-alist '((implement . implement-preset)))
        captured)
    (with-temp-buffer
      (insert "source")
      (setq-local mevedel--workspace workspace)
      (let* ((directive
              (mevedel--create-directive-in
               (current-buffer) (point-min) (point-max) nil "Request"))
             (_child
              (mevedel--create-directive-in
               (current-buffer) (1+ (point-min)) (1- (point-max))
               nil "Child change"))
             (record (mevedel--directive-record directive)))
        (setf (mevedel-directive-state record) 'implemented
              (mevedel-directive-attempts record)
              (list
               (mevedel-directive-attempt--create
                :directive-request "Request" :request "Exact"
                :result "Done" :outcome 'success :patch "old patch"
                :capture 'complete
                :captured-at "2026-08-02T01:00:00+0200")))
        (cl-letf (((symbol-function 'mevedel--process-directive)
                   (lambda (seen preset prompt-fn callback &optional options)
                     (setq captured
                           (list seen preset
                                 (funcall prompt-fn "Fresh\nChild change")
                                 callback options
                                 (overlay-get seen 'mevedel-directive-action)))
                     'accepted)))
          (should
           (eq 'accepted
               (mevedel--request-directive-changes directive "" #'ignore)))
          (should (eq directive (nth 0 captured)))
          (should (eq 'implement-preset (nth 1 captured)))
          (should (string-search "Child change" (nth 2 captured)))
          (should (eq #'ignore (nth 3 captured)))
          (should-not (nth 4 captured))
          (should (eq 'request-changes (nth 5 captured)))))))

  :doc "rejects Request changes without feedback or new subdirectives"
  (let ((record
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :state 'implemented
          :attempts
          (list
           (mevedel-directive-attempt--create
            :directive-request "Request" :outcome 'success)))))
    (with-temp-buffer
      (insert "source")
      (let ((directive (make-overlay (point-min) (point-max))))
        (cl-letf (((symbol-function 'mevedel--directive-record)
                   (lambda (_) record))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (seen _) seen)))
          (should-error
           (mevedel--request-directive-changes directive "" nil)
           :type 'user-error))))))

(mevedel-deftest mevedel--retry-directive
  (:doc "accepts empty guidance and uses ordinary implementation authority")
  (let ((record
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :state 'failed
          :attempts
          (list
           (mevedel-directive-attempt--create
            :directive-request "Request" :request "Exact"
            :result "Failure" :outcome 'error :patch "partial"
            :capture 'incomplete :captured-at "2026-08-02T01:00:00+0200"))))
        (mevedel-action-preset-alist '((implement . implement-preset)))
        captured)
    (with-temp-buffer
      (insert "source")
      (let ((directive (make-overlay (point-min) (point-max))))
        (cl-letf (((symbol-function 'mevedel--directive-record)
                   (lambda (_) record))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (seen _) seen))
                  ((symbol-function 'mevedel--process-directive)
                   (lambda (seen preset prompt-fn callback &optional options)
                     (setq captured
                           (list seen preset (funcall prompt-fn "Fresh")
                                 callback options
                                 (overlay-get seen 'mevedel-directive-action)))
                     'accepted)))
          (should (eq 'accepted (mevedel--retry-directive directive "" nil)))
          (should (eq 'implement-preset (nth 1 captured)))
          (should (string-search "Failure" (nth 2 captured)))
          (should (eq 'retry (nth 5 captured))))))))

(provide 'test-mevedel-directive-request)
;;; test-mevedel-directive-request.el ends here
