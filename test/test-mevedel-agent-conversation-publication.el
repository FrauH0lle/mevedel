;;; test-mevedel-agent-conversation-publication.el --- Agent save transactions -*- lexical-binding: t -*-

;;; Commentary:
;; Retained transcript and metadata commits use real portable storage.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-journal-test-support"))
(require 'mevedel-agent-runtime)
(require 'mevedel-execution-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-execution-test-helpers"))

(mevedel-deftest mevedel-agent-conversation--write (:quiet t)
  ,test
  (test)
  :doc "publishes a new agent transcript and dirty metadata in one commit"
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session parent)
     (with-current-buffer parent
       (insert "Root conversation\n")
       (mevedel-session-artifacts-save session parent t))
     (let* ((child (generate-new-buffer " *agent-publication*"))
	    (inv (mevedel-agent-invocation--create
		  :agent (mevedel-agent--create :name "default")
		  :agent-id "default--publication" :path "/root/check"
		  :parent-session session :parent-data-buffer parent
		  :buffer child :parent-turn 1))
	    (publisher (symbol-function 'mevedel-session-publication-publish))
	    (request (symbol-function 'gptel-request))
	    (publications 0))
       (unwind-protect
	   (progn
	     (setf (mevedel-agent-invocation-frozen-configuration inv)
		   (with-current-buffer parent
		     (mevedel-agent-exec-freeze-configuration
		      "default" inv (list :backend gptel-backend :model gptel-model))))
	     (with-current-buffer child
	       (org-mode)
	       (setq-local mevedel--session session mevedel--agent-invocation inv)
	       (mevedel-agent-conversation-configure inv child)
	       (mevedel-agent-runtime--setup-transcript inv child)
	       (insert "* Agent Task: check\n\nDurable task\n"))
	     (cl-letf (((symbol-function 'mevedel-session-publication-publish)
			(lambda (&rest args)
			  (cl-incf publications)
			  (apply publisher args))))
		      (should (mevedel-agent-conversation-save inv))
		      ;; Exercise the native request setup without provider I/O.
		      ;; Its frozen locals must not force a second transcript save.
		      (cl-letf (((symbol-function 'gptel-request)
				 (lambda (prompt &rest args)
				   (apply request prompt :dry-run t args))))
			       (with-current-buffer parent
				 (mevedel-agent-exec-run #'ignore "default" "check" inv child))))
	     (should (= 1 publications))
	     (should-not (mevedel-agent-invocation-sidecar-dirty inv))
	     (should-not (with-current-buffer child (buffer-modified-p)))
	     (should (string-match-p
		      "Durable task"
		      (mevedel-session-artifacts-read-artifact
		       session (mevedel-agent-invocation-transcript-relative-path inv) t)))
	     (should (string-match-p
		      "default--publication"
		      (mevedel-session-artifacts-read-artifact session "session.meta.el" t)))
	     ;; A failed combined commit retains the dirty transcript and
	     ;; metadata for retry; the prior published head stays usable.
	     (let ((head (plist-get (mevedel-session-publication session) :head)))
	       (with-current-buffer child (goto-char (point-max)) (insert "retry text\n"))
	       (setf (mevedel-agent-invocation-sidecar-dirty inv) t)
	       (cl-letf (((symbol-function 'mevedel-session-publication--commit-marker-publication)
			  (lambda (&rest _) (error "Injected head failure"))))
			(should-not (mevedel-agent-conversation-save inv)))
	       (should (equal head (plist-get (mevedel-session-publication session) :head)))
	       (should (mevedel-agent-invocation-sidecar-dirty inv))
	       (should (with-current-buffer child (buffer-modified-p)))
	       (should (mevedel-session-pending-publication session))
	       (mevedel-session-publication-retry session)
	       (should (mevedel-agent-conversation-save inv))
	       (should-not (mevedel-agent-invocation-sidecar-dirty inv))
	       (should (string-match-p
			"retry text"
			(mevedel-session-artifacts-read-artifact
			 session (mevedel-agent-invocation-transcript-relative-path inv) t))))
	     ;; The terminal result is already durable before DONE's hook
	     ;; changes. Those use the existing coalescing save and still
	     ;; flush synchronously on an explicit checkpoint/teardown.
	     (let ((fsm (gptel-make-fsm
			 :info (list :buffer child
				     :position (with-current-buffer child (point-min-marker))
				     :tracking-marker (with-current-buffer child (point-max-marker))
				     :mevedel-agent-invocation inv))))
	       (setf (mevedel-agent-invocation-runtime-settled-p inv) t)
	       (with-current-buffer child
		 (setq-local gptel-post-response-functions
			     (list (lambda (&rest _) (goto-char (point-max))
				     (insert "post-response hook\n")))))
	       (setq publications 0)
	       (cl-letf (((symbol-function 'mevedel-session-publication-publish)
			  (lambda (&rest args) (cl-incf publications) (apply publisher args))))
			(mevedel-agent-exec--handle-done-save fsm)
			(should (= 0 publications))
			(should (timerp (mevedel-agent-invocation-transcript-save-timer inv)))
			(should (mevedel-agent-conversation-save inv))
			(should (> publications 0))
			(should-not (mevedel-agent-invocation-transcript-save-timer inv))
			;; A not-yet-settled answer must still checkpoint now.
			(setq publications 0)
			(setf (mevedel-agent-invocation-runtime-settled-p inv) nil)
			(mevedel-agent-exec--handle-done-save fsm)
			(should (> publications 0)))
	       (mevedel-session-artifacts-save session parent)
	       (should (string-match-p
			"post-response hook"
			(mevedel-session-artifacts-read-artifact
			 session (mevedel-agent-invocation-transcript-relative-path inv) t)))))
	 (mevedel-agent-conversation--cancel-save inv)
	 (with-current-buffer child
	   (setq-local kill-buffer-hook nil)
	   (set-buffer-modified-p nil))
	 (kill-buffer child))))))

(defun test-mevedel-agent-conversation-publication--settle-behind-gate (event)
  "Settle a real portable agent turn with EVENT behind a busy publication gate.
Return a plist of the observed order and final state."
  (let (result)
    (mevedel-test-journal-capture--with-portable-session
     (lambda (session parent)
       (with-current-buffer parent
         (insert "Root conversation\n")
         (mevedel-session-artifacts-save session parent t))
       (let* ((child (generate-new-buffer " *agent-gate-child*"))
              (inv (mevedel-agent-invocation--create
                    :agent (mevedel-agent--create :name "default")
                    :agent-id "default--gate" :path "/root/check"
                    :parent-session session :parent-data-buffer parent
                    :buffer child :parent-turn 1))
              (log nil)
              (barriers nil)
              (barrier (symbol-function 'mevedel-session-publication-barrier)))
         (unwind-protect
             (progn
               (setf (mevedel-agent-invocation-frozen-configuration inv)
                     (with-current-buffer parent
                       (mevedel-agent-exec-freeze-configuration
                        "default" inv (list :backend gptel-backend :model gptel-model))))
               (with-current-buffer child
                 (org-mode)
                 (setq-local mevedel--session session mevedel--agent-invocation inv)
                 (mevedel-agent-conversation-configure inv child)
                 (mevedel-agent-runtime--setup-transcript inv child)
                 (insert "* Agent Task: check\n\nDurable gate task\n"))
               (setf (mevedel-agent-invocation-runtime-settle-callback inv)
                     (lambda (_inv _visible _event token)
                       (push (list 'callback (and token t)) log)
                       nil))
               ;; A real accepted preparation holds the gate; the settlement
               ;; must queue behind it and later run under its own token.
               (setf (mevedel-session-publication-preparations session)
                     (list (cons (lambda (_token) (push 'earlier log)) nil)))
               (should (mevedel-session-publication-busy-p session))
               (cl-letf (((symbol-function 'mevedel-session-publication-barrier)
                          (lambda (&rest args)
                            (push args barriers)
                            (apply barrier args))))
                 (should-not
                  (mevedel-agent-runtime--settle inv "finished" event))
                 (should-not (mevedel-agent-invocation-runtime-settled-p inv))
                 (test-mevedel-execution--wait
                  (lambda () (mevedel-agent-invocation-runtime-settled-p inv))))
               (setq result
                     (list :log (reverse log)
                           :barriers (cl-remove-if #'cadr barriers)
                           :status (mevedel-agent-invocation-transcript-status inv)
                           :busy (mevedel-session-publication-busy-p session)
                           :token (mevedel-session-publication-preparation-token
                                   session)
                           :transcript
                           (ignore-errors
                             (mevedel-session-artifacts-read-artifact
                              session
                              (mevedel-agent-invocation-transcript-relative-path inv)
                              t)))))
           (with-current-buffer child
             (setq-local kill-buffer-hook nil)
             (set-buffer-modified-p nil))
           (kill-buffer child)))))
    result))

(mevedel-deftest mevedel-agent-runtime--settle/busy-gate (:quiet t)
  ,test
  (test)
  :doc "a completed turn queues behind a real held gate and settles under its token"
  (let ((result (test-mevedel-agent-conversation-publication--settle-behind-gate
                 nil)))
    (should (equal '(earlier (callback t)) (plist-get result :log)))
    ;; Every barrier inside the queued settlement carried its token.
    (should-not (plist-get result :barriers))
    (should (eq 'completed (plist-get result :status)))
    (should-not (plist-get result :busy))
    (should-not (plist-get result :token))
    (should (string-match-p "Durable gate task" (plist-get result :transcript))))
  :doc "an errored turn's cleanup also completes without draining its own gate"
  (let ((result (test-mevedel-agent-conversation-publication--settle-behind-gate
                 (list :mevedel-agent-terminal-status 'error
                       :error-details "Provider failed"))))
    (should (equal '(earlier (callback t)) (plist-get result :log)))
    ;; Every barrier inside the queued cleanup carried its token.
    (should-not (plist-get result :barriers))
    (should (eq 'error (plist-get result :status)))
    (should-not (plist-get result :busy))
    (should-not (plist-get result :token))
    (should (string-match-p "Durable gate task" (plist-get result :transcript)))))

(provide 'test-mevedel-agent-conversation-publication)
;;; test-mevedel-agent-conversation-publication.el ends here
