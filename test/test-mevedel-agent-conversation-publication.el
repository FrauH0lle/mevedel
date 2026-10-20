;;; test-mevedel-agent-conversation-publication.el --- Agent save transactions -*- lexical-binding: t -*-

;;; Commentary:
;; Retained transcript and metadata commits use real portable storage.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-journal-test-support"))
(require 'mevedel-agent-runtime)
(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))
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

(defun test-mevedel-agent-conversation-publication--with-agent (function)
  "Call FUNCTION with a committed portable session and a dirty retained agent.
FUNCTION receives the session, root buffer, child buffer and invocation."
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session parent)
     (with-current-buffer parent
       (insert "Root conversation\n")
       (mevedel-session-artifacts-save session parent t))
     (let* ((child (generate-new-buffer " *agent-prepare*"))
            (inv (mevedel-agent-invocation--create
                  :agent (mevedel-agent--create :name "default")
                  :agent-id "default--prepare" :path "/root/check"
                  :parent-session session :parent-data-buffer parent
                  :buffer child :parent-turn 1)))
       (unwind-protect
           (progn
             (setf (mevedel-agent-invocation-frozen-configuration inv)
                   (with-current-buffer parent
                     (mevedel-agent-exec-freeze-configuration
                      "default" inv (list :backend gptel-backend
                                          :model gptel-model))))
             (with-current-buffer child
               (org-mode)
               (setq-local mevedel--session session
                           mevedel--agent-invocation inv)
               (mevedel-agent-conversation-configure inv child)
               (mevedel-agent-runtime--setup-transcript inv child)
               (insert "* Agent Task: check\n\nPrepared task\n"))
             (funcall function session parent child inv))
         (mevedel-agent-conversation--cancel-save inv)
         (with-current-buffer child
           (setq-local kill-buffer-hook nil)
           (set-buffer-modified-p nil))
         (kill-buffer child))))))

(defun test-mevedel-agent-conversation-publication--commit (session artifacts)
  "Commit prepared ARTIFACTS for SESSION the way an adopted job would."
  (mevedel-session-durability-with-transaction
    (mevedel-session-publication-publish session artifacts t t)))

(mevedel-deftest mevedel-agent-conversation--owns-transcript-p (:quiet t)
  ,test
  (test)
  :doc "accepts only the live buffer visiting the canonical transcript"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (should (mevedel-agent-conversation--owns-transcript-p inv))
     (with-current-buffer child
       (set-visited-file-name (make-temp-name "/tmp/mevedel-elsewhere-") t t))
     (should-not (mevedel-agent-conversation--owns-transcript-p inv))
     (with-current-buffer child (set-visited-file-name nil t t))
     (should-not (mevedel-agent-conversation--owns-transcript-p inv)))))

(mevedel-deftest mevedel-agent-conversation--freeze-preamble (:quiet t)
  ,test
  (test)
  :doc "settles tentative state, captures a checkpoint and stamps the entry"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let (cleared updates)
       (cl-letf (((symbol-function 'mevedel-ptc-checkpoint-clear-settled)
                  (lambda (_s path &rest _) (push path cleared)))
                 ((symbol-function
                   'mevedel-session-persistence-update-transcript-entry)
                  (lambda (_s id plist) (push (cons id plist) updates))))
         (with-current-buffer child
           (should (mevedel-agent-conversation--freeze-preamble inv session))))
       (should (equal '("/root/check") cleared))
       (should (equal "default--prepare" (caar updates)))
       (should (plist-get (cdar updates) :updated-at))))))

(mevedel-deftest mevedel-agent-conversation--freeze-content (:quiet t)
  ,test
  (test)
  :doc "runs before-save hooks once and freezes bytes, tick and checkpoint"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (with-current-buffer child
       (let ((runs 0))
         (add-hook 'before-save-hook (lambda () (cl-incf runs)) nil t)
         (let ((frozen (mevedel-agent-conversation--freeze-content
                        inv session)))
           (should (= 1 runs))
           (should (equal (buffer-substring-no-properties
                           (point-min) (point-max))
                          (plist-get frozen :content)))
           (should (= (buffer-chars-modified-tick)
                      (plist-get frozen :frozen-tick)))
           (should (plist-get frozen :checkpoint))))))))

(mevedel-deftest mevedel-agent-conversation--finish-save (:quiet t)
  ,test
  (test)
  :doc "cleans the buffer only when unchanged since the freeze"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (_session _parent child _inv)
     (with-current-buffer child
       (let ((hooks 0))
         (add-hook 'after-save-hook (lambda () (cl-incf hooks)) nil t)
         (let ((tick (buffer-chars-modified-tick)))
           (insert "later edit\n")
           (mevedel-agent-conversation--finish-save tick)
           (should (buffer-modified-p))
           (should (= 1 hooks)))
         (mevedel-agent-conversation--finish-save
          (buffer-chars-modified-tick))
         (should-not (buffer-modified-p))
         (should (= 2 hooks)))))))

(defun test-mevedel-agent-conversation-publication--unresolved (body)
  "Call BODY with an agent whose async checkpoint ended unresolved and edited.
BODY receives the session, the child buffer and the invocation.  The failed
head and the unreadable observation are injected only while the job runs."
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((child (generate-new-buffer " *agent-kill*"))
            (inv (mevedel-agent-invocation--create
                  :agent (mevedel-agent--create :name "default")
                  :agent-id "default--kill" :path "/root/check"
                  :parent-session session :parent-data-buffer buffer
                  :buffer child :parent-turn 1)))
       (unwind-protect
           (progn
             (setf (mevedel-agent-invocation-frozen-configuration inv)
                   (with-current-buffer buffer
                     (mevedel-agent-exec-freeze-configuration
                      "default" inv (list :backend gptel-backend
                                          :model gptel-model))))
             (with-current-buffer child
               (org-mode)
               (setq-local mevedel--session session
                           mevedel--agent-invocation inv)
               (mevedel-agent-conversation-configure inv child)
               (mevedel-agent-runtime--setup-transcript inv child)
               (mevedel-agent-conversation--install-kill-guards)
               (insert "* Agent Task: kill\n\nBEFORE-FREEZE\n"))
             (mevedel-test--with-captured-diagnostics nil
               (cl-letf (((symbol-function
                           'mevedel-session-durability-commit-publication-head-async)
                          (lambda (_o _h callback)
                            (funcall callback nil
                                     '(file-error "Head write did not run"))))
                         ((symbol-function
                           'mevedel-session-durability-observe-publication-head-async)
                          (lambda (_owner callback)
                            (funcall callback nil '(file-error "unreadable")))))
                 (mevedel-session-persistence-checkpoint-buffer child)
                 (test-mevedel-publication-async--wait
                  (lambda ()
                    (when-let* ((job (mevedel-session-publication-job session)))
                      (eq 'unresolved
                          (mevedel-session-publication--job-phase job)))))))
             (with-current-buffer child
               (goto-char (point-max))
               (insert "AFTER-FREEZE\n"))
             (funcall body session child inv))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (ignore-errors (mevedel-session-publication-abandon session)))
         (setf (mevedel-session-publication-job session) nil)
         (when (buffer-live-p child)
           (with-current-buffer child
             (setq-local kill-buffer-hook nil kill-buffer-query-functions nil)
             (set-buffer-modified-p nil))
           (kill-buffer child)))))))

(mevedel-deftest mevedel-agent-conversation--owes-p (:quiet t)
  ,test
  (test)
  :doc "an owned transcript owes bytes while modified or its sidecar is dirty"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (should (mevedel-agent-conversation--owes-p inv))
     (with-current-buffer child (set-buffer-modified-p nil))
     (setf (mevedel-agent-invocation-sidecar-dirty inv) nil)
     (should-not (mevedel-agent-conversation--owes-p inv))
     (setf (mevedel-agent-invocation-sidecar-dirty inv) t)
     (should (mevedel-agent-conversation--owes-p inv))
     (setf (mevedel-agent-invocation-sidecar-dirty inv) nil)
     (with-current-buffer child
       (insert "x")
       (set-visited-file-name (make-temp-name "/tmp/mevedel-elsewhere-") t t))
     (should-not (mevedel-agent-conversation--owes-p inv)))))

(mevedel-deftest mevedel-agent-conversation--unsaved-reason (:quiet t)
  ,test
  (test)
  :doc "saves owed bytes and explains a save that leaves them unpublished"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (should-not (mevedel-agent-conversation--unsaved-reason inv))
     (should-not (with-current-buffer child (buffer-modified-p)))
     (should-not (mevedel-agent-conversation--unsaved-reason inv))
     (with-current-buffer child (insert "more\n"))
     (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
                (lambda (&rest _) nil)))
       (let ((reason (mevedel-agent-conversation--unsaved-reason inv)))
         (should (string-match-p "default--prepare" reason))
         (should (string-match-p "publication-retry" reason)))
       ;; A canceled save refuses with a message instead of a quit bubbling.
       (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
                  (lambda (&rest _) (signal 'quit nil))))
         (should (string-match-p
                  "canceled"
                  (mevedel-agent-conversation--unsaved-reason inv))))))))

(mevedel-deftest mevedel-agent-conversation-allow-buffer-kill-p (:quiet t)
  ,test
  (test)
  :doc "a clean or non-agent buffer may be killed, a failing save refuses"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (with-current-buffer child
       (should (mevedel-agent-conversation-allow-buffer-kill-p))
       (should-not (buffer-modified-p))
       (insert "pending\n")
       (let (message)
         (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
                    (lambda (&rest _) nil)))
           (mevedel-test--with-captured-diagnostics message
             (should-not (mevedel-agent-conversation-allow-buffer-kill-p))))
         (should (string-match-p "publication-abandon" message)))
       (should (mevedel-agent-invocation-p inv))))
   )
  (with-temp-buffer
    (should (mevedel-agent-conversation-allow-buffer-kill-p)))
  :doc "an unresolved checkpoint with a late edit refuses the kill and keeps bytes"
  (test-mevedel-agent-conversation-publication--unresolved
   (lambda (session child inv)
     (let (message killed)
       (mevedel-test--with-captured-diagnostics message
         (setq killed (kill-buffer child)))
       (should-not killed)
       (should (buffer-live-p child))
       (should (string-match-p "publication-retry" message))
       (with-current-buffer child
         (should (string-match-p "AFTER-FREEZE" (buffer-string)))
         (should (buffer-modified-p)))
       ;; Explicit recovery frees the gate; the next kill publishes the late
       ;; edit and proceeds.
       (mevedel-test--with-captured-diagnostics nil
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (ignore-errors (mevedel-session-publication-retry session))
           (ignore-errors (mevedel-session-publication-abandon session))))
       (setf (mevedel-session-publication-job session) nil)
       (mevedel-test--with-captured-diagnostics nil
         (setq killed (kill-buffer child)))
       (should killed)
       (should-not (buffer-live-p child))
       (should (string-match-p
                "AFTER-FREEZE"
                (mevedel-session-artifacts-read-artifact
                 session
                 (mevedel-agent-invocation-transcript-relative-path inv)
                 t))))))
  :doc "production exit is refused by the persistence gate until recovery"
  (test-mevedel-agent-conversation-publication--unresolved
   (lambda (session _child _inv)
     (let (exited message)
       (cl-letf (((symbol-function 'kill-emacs)
                  (lambda (&rest _) (setq exited t)))
                 ((symbol-function 'save-some-buffers) #'ignore)
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (let ((confirm-kill-processes nil)
               (confirm-kill-emacs nil))
           (mevedel-test--with-captured-diagnostics message
             (save-buffers-kill-emacs))
           (should-not exited)
           (should (string-match-p "publication" message))
           (mevedel-test--with-captured-diagnostics nil
             (ignore-errors (mevedel-session-publication-retry session))
             (ignore-errors (mevedel-session-publication-abandon session)))
           (setf (mevedel-session-publication-job session) nil)
           (mevedel-test--with-captured-diagnostics nil
             (save-buffers-kill-emacs))
           (should exited)))))))

(mevedel-deftest mevedel-agent-conversation--preserve-unsaved (:quiet t)
  ,test
  (test)
  :doc "writes the buffer bytes to a recovery file"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (let ((file (mevedel-agent-conversation--preserve-unsaved inv)))
       (unwind-protect
           (should (equal (with-current-buffer child (buffer-string))
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))
         (delete-file file))))))

(mevedel-deftest mevedel-agent-conversation--install-kill-guards (:quiet t)
  ,test
  (test)
  :doc "installs the query and hook buffer-locally"
  (with-temp-buffer
    (mevedel-agent-conversation--install-kill-guards)
    (should (memq #'mevedel-agent-conversation-allow-buffer-kill-p
                  kill-buffer-query-functions))
    (should (memq #'mevedel-agent-conversation--on-buffer-kill
                  kill-buffer-hook))
    (should (local-variable-p 'kill-buffer-hook))))

(mevedel-deftest mevedel-agent-conversation--on-buffer-kill (:quiet t)
  ,test
  (test)
  :doc "a failed save is reported with a recovery file, never silently dropped"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (let (message)
       (with-current-buffer child
         (insert "unsaved tail\n")
         (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
                    (lambda (&rest _) nil)))
           (mevedel-test--with-captured-diagnostics message
             (mevedel-agent-conversation--on-buffer-kill))))
       (should (string-match-p "unpublished changes; recovered to " message))
       (let ((file (and (string-match "recovered to \\(.+\\)$" message)
                        (string-trim (match-string 1 message)))))
         (unwind-protect
             (should (string-match-p
                      "unsaved tail"
                      (with-temp-buffer (insert-file-contents file)
                                        (buffer-string))))
           (when file (delete-file file))))
       (should (mevedel-agent-invocation-p inv)))
     ;; A canceled save still recovers the bytes.
     (let (message)
       (with-current-buffer child
         (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
                    (lambda (&rest _) (signal 'quit nil))))
           (mevedel-test--with-captured-diagnostics message
             (mevedel-agent-conversation--on-buffer-kill))))
       (should (string-match-p "recovered to " message))
       (when (string-match "recovered to \\(.+\\)$" message)
         (delete-file (string-trim (match-string 1 message)))))
     ;; A save that publishes everything reports nothing.
     (let (message)
       (with-current-buffer child
         (mevedel-test--with-captured-diagnostics message
           (mevedel-agent-conversation--on-buffer-kill)))
       (should-not (string-match-p "recovered" message))))))

(mevedel-deftest mevedel-agent-conversation--drain-gate (:quiet t)
  ,test
  (test)
  :doc "passes the caller's token to the barrier"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (session _parent _child inv)
     (let (calls)
       (cl-letf (((symbol-function 'mevedel-session-publication-barrier)
                  (lambda (&rest args) (push args calls) t)))
         (should (mevedel-agent-conversation--drain-gate inv 'own-token)))
       (should (equal (list (list session 'own-token)) calls)))))
  :doc "skips the barrier for a session that cannot own a gate"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (session _parent _child inv)
     (let (calls)
       (setf (mevedel-session-save-path session) nil)
       (cl-letf (((symbol-function 'mevedel-session-publication-barrier)
                  (lambda (&rest args) (push args calls) t)))
         (should (mevedel-agent-conversation--drain-gate inv nil)))
       (should-not calls))))
  :doc "an unresolved barrier reports and returns nil"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (_session _parent _child inv)
     (let (message)
       (cl-letf (((symbol-function 'mevedel-session-publication-barrier)
                  (lambda (&rest _) (user-error "Outcome is unresolved"))))
         (mevedel-test--with-captured-diagnostics message
           (should-not (mevedel-agent-conversation--drain-gate inv nil))))
       (should (string-match-p "default--prepare" message))
       (should (string-match-p "unresolved" message))))))

(mevedel-deftest mevedel-agent-conversation--write/overlapping-job (:quiet t)
  ,test
  (test)
  :doc "a synchronous save drains an accepted job before it freezes the agent"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((child (generate-new-buffer " *agent-overlap*"))
            (inv (mevedel-agent-invocation--create
                  :agent (mevedel-agent--create :name "default")
                  :agent-id "default--overlap" :path "/root/check"
                  :parent-session session :parent-data-buffer buffer
                  :buffer child :parent-turn 1))
            (freeze (symbol-function 'mevedel-agent-conversation--freeze-content))
            acks frozen-while-job message)
       (unwind-protect
           (progn
             (setf (mevedel-agent-invocation-frozen-configuration inv)
                   (with-current-buffer buffer
                     (mevedel-agent-exec-freeze-configuration
                      "default" inv (list :backend gptel-backend
                                          :model gptel-model))))
             (with-current-buffer child
               (org-mode)
               (setq-local mevedel--session session
                           mevedel--agent-invocation inv)
               (mevedel-agent-conversation-configure inv child)
               (mevedel-agent-runtime--setup-transcript inv child)
               (insert "* Agent Task: check\n\nOverlapping task\n"))
             ;; An accepted asynchronous job owns the gate, then a retained
             ;; agent saves synchronously while it is still in flight.
             (mevedel-session-publication-submit
              session
              (lambda ()
                (list (mevedel-session-artifacts--sidecar-artifact
                       session buffer)))
              (lambda (ok err) (push (list ok err) acks)))
             (should (mevedel-session-publication-busy-p session))
             (cl-letf (((symbol-function
                         'mevedel-agent-conversation--freeze-content)
                        (lambda (&rest args)
                          (setq frozen-while-job
                                (list (and (mevedel-session-publication-job session) t)
                                      (length acks)))
                          (apply freeze args))))
               (mevedel-test--with-captured-diagnostics message
                 (setf (mevedel-agent-invocation-sidecar-dirty inv) t)
                 (should (mevedel-agent-conversation-save inv))))
             ;; The agent froze only after the job committed and acknowledged.
             (should (equal '(nil 1) frozen-while-job))
             (should (equal '((t nil)) acks))
             (should-not (string-match-p "prepared\\|deferred\\|closed" message))
             (should-not (mevedel-session-publication-busy-p session))
             (should-not (mevedel-agent-invocation-sidecar-dirty inv))
             (should (string-match-p
                      "Overlapping task"
                      (mevedel-session-artifacts-read-artifact
                       session
                       (mevedel-agent-invocation-transcript-relative-path inv)
                       t)))
             (should (string-match-p
                      "default--overlap"
                      (mevedel-session-artifacts-read-artifact
                       session "session.meta.el" t))))
         (mevedel-agent-conversation--cancel-save inv)
         (with-current-buffer child
           (setq-local kill-buffer-hook nil)
           (set-buffer-modified-p nil))
         (kill-buffer child))))))

(mevedel-deftest mevedel-agent-conversation-prepare-save (:quiet t)
  ,test
  (test)
  :doc "freezes transcript and sidecar without mutating until the finish runs"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let* ((head (plist-get (mevedel-session-publication session) :head))
            (relative (mevedel-agent-invocation-transcript-relative-path inv))
            (plan (mevedel-agent-conversation-prepare-save inv))
            (artifacts (plist-get plan :artifacts)))
       (should plan)
       (should (= 2 (length artifacts)))
       (should (string-match-p "Prepared task" (plist-get (car artifacts) :content)))
       (should (equal (expand-file-name relative (mevedel-session-save-path session))
                      (plist-get (car artifacts) :path)))
       (should (equal "session.meta.el"
                      (file-name-nondirectory
                       (plist-get (cadr artifacts) :path))))
       (should (plist-get (cadr artifacts) :checkpoint))
       ;; Preparation commits nothing and marks nothing saved.
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (should (with-current-buffer child (buffer-modified-p)))
       (setf (mevedel-agent-invocation-sidecar-dirty inv) t)
       (test-mevedel-agent-conversation-publication--commit session artifacts)
       (should (with-current-buffer child (buffer-modified-p)))
       (should (mevedel-agent-invocation-sidecar-dirty inv))
       (funcall (plist-get plan :finish))
       (should-not (with-current-buffer child (buffer-modified-p)))
       (should-not (mevedel-agent-invocation-sidecar-dirty inv))
       (should (string-match-p
                "Prepared task"
                (mevedel-session-artifacts-read-artifact session relative t))))))
  :doc "a failed commit never calls finish, so the buffer stays modified"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let ((plan (mevedel-agent-conversation-prepare-save inv))
           (head (plist-get (mevedel-session-publication session) :head)))
       (setf (mevedel-agent-invocation-sidecar-dirty inv) t)
       (cl-letf (((symbol-function
                   'mevedel-session-publication--commit-marker-publication)
                  (lambda (&rest _) (error "Injected head failure"))))
         (should-error
          (test-mevedel-agent-conversation-publication--commit
           session (plist-get plan :artifacts))))
       (should (equal head (plist-get (mevedel-session-publication session) :head)))
       (should (with-current-buffer child (buffer-modified-p)))
       (should (mevedel-agent-invocation-sidecar-dirty inv))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (mevedel-session-publication-abandon session)))))
  :doc "an edit between freeze and finish keeps the buffer modified and uncommitted"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let ((plan (mevedel-agent-conversation-prepare-save inv))
           (relative (mevedel-agent-invocation-transcript-relative-path inv)))
       (with-current-buffer child
         (goto-char (point-max))
         (insert "typed during the job\n"))
       (test-mevedel-agent-conversation-publication--commit
        session (plist-get plan :artifacts))
       (funcall (plist-get plan :finish))
       (should (with-current-buffer child (buffer-modified-p)))
       (should-not (string-match-p
                    "typed during the job"
                    (mevedel-session-artifacts-read-artifact session relative t))))))
  :doc "supersedes a pending debounced save and declines when not applicable"
  :doc "a property-only change after the freeze stays dirty and the next save serializes it"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let* ((plan (mevedel-agent-conversation-prepare-save inv))
            (chars-before (with-current-buffer child (buffer-chars-modified-tick)))
            start end)
       (with-current-buffer child
         (save-excursion
           (goto-char (point-min))
           (re-search-forward "Prepared task")
           (setq start (match-beginning 0) end (match-end 0)))
         ;; Characters are untouched; only the gptel classification changes.
         (let ((inhibit-read-only t))
           (put-text-property start end 'gptel 'ignore))
         (should (= chars-before (buffer-chars-modified-tick))))
       (test-mevedel-agent-conversation-publication--commit
        session (plist-get plan :artifacts))
       (funcall (plist-get plan :finish))
       ;; The frozen artifacts predate the change, so the buffer must stay dirty.
       (should (with-current-buffer child (buffer-modified-p)))
       (let* ((next (mevedel-agent-conversation-prepare-save inv))
              (content (plist-get (car (plist-get next :artifacts)) :content)))
         (should (string-match-p "GPTEL_BOUNDS" content))
         ;; Bounds are positions in the saved text, drawer included.
         (let ((saved-start (1+ (string-match "Prepared task" content))))
           (should (string-match-p
                    (format "(ignore (%d %d))" saved-start
                            (+ saved-start (- end start)))
                    content)))))))
  :doc "supersedes a pending debounced save and declines when not applicable"
  (test-mevedel-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (let ((mevedel-agent-conversation-save-debounce 1000))
       (mevedel-agent-conversation-save inv t)
       (should (timerp (mevedel-agent-invocation-transcript-save-timer inv)))
       (should (mevedel-agent-conversation-prepare-save inv))
       (should-not (mevedel-agent-invocation-transcript-save-timer inv)))
     ;; A buffer that does not visit the canonical transcript is declined.
     (with-current-buffer child
       (set-visited-file-name (make-temp-name "/tmp/mevedel-elsewhere-") t t))
     (should-not (mevedel-agent-conversation-prepare-save inv)))))

(provide 'test-mevedel-agent-conversation-publication)
;;; test-mevedel-agent-conversation-publication.el ends here
