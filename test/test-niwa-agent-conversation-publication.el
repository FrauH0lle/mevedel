;;; test-niwa-agent-conversation-publication.el --- Agent save transactions -*- lexical-binding: t -*-

;;; Commentary:
;; Retained transcript and metadata commits use real portable storage.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-journal-test-support"))
(require 'niwa-agent-runtime)
(require 'niwa-session-publication)
(require 'niwa-session-publication-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-session-publication-test-helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-execution-test-helpers"))

(niwa-deftest niwa-agent-conversation--write (:quiet t)
  ,test
  (test)
  :doc "publishes a new agent transcript and dirty metadata in one commit"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session parent)
     (with-current-buffer parent
       (insert "Root conversation\n")
       (niwa-session-artifacts-save session parent t))
     (let* ((child (generate-new-buffer " *agent-publication*"))
	    (inv (niwa-agent-invocation--create
		  :agent (niwa-agent--create :name "default")
		  :agent-id "default--publication" :path "/root/check"
		  :parent-session session :parent-data-buffer parent
		  :buffer child :parent-turn 1))
	    (publisher (symbol-function 'niwa-session-publication-publish))
	    (request (symbol-function 'gptel-request))
	    (publications 0))
       (unwind-protect
	   (progn
	     (setf (niwa-agent-invocation-frozen-configuration inv)
		   (with-current-buffer parent
		     (niwa-agent-exec-freeze-configuration
		      "default" inv (list :backend gptel-backend :model gptel-model))))
	     (with-current-buffer child
	       (org-mode)
	       (setq-local niwa--session session niwa--agent-invocation inv)
	       (niwa-agent-conversation-configure inv child)
	       (niwa-agent-runtime--setup-transcript inv child)
	       (insert "* Agent Task: check\n\nDurable task\n"))
	     (cl-letf (((symbol-function 'niwa-session-publication-publish)
			(lambda (&rest args)
			  (cl-incf publications)
			  (apply publisher args))))
		      ;; An earlier failure reason does not survive a later save.
		      (setf (niwa-agent-invocation-persistence-failure inv) "stale")
		      (should (niwa-agent-conversation-save inv))
		      (should-not (niwa-agent-invocation-persistence-failure inv))
		      ;; Exercise the native request setup without provider I/O.
		      ;; Its frozen locals must not force a second transcript save.
		      (cl-letf (((symbol-function 'gptel-request)
				 (lambda (prompt &rest args)
				   (apply request prompt :dry-run t args))))
			       (with-current-buffer parent
				 (niwa-agent-exec-run #'ignore "default" "check" inv child))))
	     (should (= 1 publications))
	     (should-not (niwa-agent-invocation-sidecar-dirty inv))
	     (should-not (with-current-buffer child (buffer-modified-p)))
	     (should (string-match-p
		      "Durable task"
		      (niwa-session-artifacts-read-artifact
		       session (niwa-agent-invocation-transcript-relative-path inv) t)))
	     (should (string-match-p
		      "default--publication"
		      (niwa-session-artifacts-read-artifact session "session.meta.el" t)))
	     ;; A failed combined commit retains the dirty transcript and
	     ;; metadata for retry; the prior published head stays usable.
	     (let ((head (plist-get (niwa-session-publication session) :head)))
	       (with-current-buffer child (goto-char (point-max)) (insert "retry text\n"))
	       (setf (niwa-agent-invocation-sidecar-dirty inv) t)
	       (cl-letf (((symbol-function 'niwa-session-publication--commit-marker-publication)
			  (lambda (&rest _) (error "Injected head failure"))))
			(should-not (niwa-agent-conversation-save inv)))
	       (should (equal head (plist-get (niwa-session-publication session) :head)))
	       (should (niwa-agent-invocation-sidecar-dirty inv))
	       (should (with-current-buffer child (buffer-modified-p)))
	       (should (niwa-session-pending-publication session))
	       (niwa-session-publication-retry session)
	       (should (niwa-agent-conversation-save inv))
	       (should-not (niwa-agent-invocation-sidecar-dirty inv))
	       (should (string-match-p
			"retry text"
			(niwa-session-artifacts-read-artifact
			 session (niwa-agent-invocation-transcript-relative-path inv) t))))
	     ;; The terminal result is already durable before DONE's hook
	     ;; changes. Those use the existing coalescing save and still
	     ;; flush synchronously on an explicit checkpoint/teardown.
	     (let ((fsm (gptel-make-fsm
			 :info (list :buffer child
				     :position (with-current-buffer child (point-min-marker))
				     :tracking-marker (with-current-buffer child (point-max-marker))
				     :niwa-agent-invocation inv))))
	       (setf (niwa-agent-invocation-runtime-settled-p inv) t)
	       (with-current-buffer child
		 (setq-local gptel-post-response-functions
			     (list (lambda (&rest _) (goto-char (point-max))
				     (insert "post-response hook\n")))))
	       (setq publications 0)
	       (cl-letf (((symbol-function 'niwa-session-publication-publish)
			  (lambda (&rest args) (cl-incf publications) (apply publisher args))))
			(niwa-agent-exec--handle-done-save fsm)
			(should (= 0 publications))
			(should (timerp (niwa-agent-invocation-transcript-save-timer inv)))
			(should (niwa-agent-conversation-save inv))
			(should (> publications 0))
			(should-not (niwa-agent-invocation-transcript-save-timer inv))
			;; A not-yet-settled answer must still checkpoint now.
			(setq publications 0)
			(setf (niwa-agent-invocation-runtime-settled-p inv) nil)
			(niwa-agent-exec--handle-done-save fsm)
			(should (> publications 0)))
	       (niwa-session-artifacts-save session parent)
	       (should (string-match-p
			"post-response hook"
			(niwa-session-artifacts-read-artifact
			 session (niwa-agent-invocation-transcript-relative-path inv) t)))))
	 (niwa-agent-conversation--cancel-save inv)
	 (with-current-buffer child
	   (setq-local kill-buffer-hook nil)
	   (set-buffer-modified-p nil))
	 (kill-buffer child))))))

(defun test-niwa-agent-conversation-publication--settle-behind-gate (event)
  "Settle a real portable agent turn with EVENT behind a busy publication gate.
Return a plist of the observed order and final state."
  (let (result)
    (niwa-test-journal-capture--with-portable-session
     (lambda (session parent)
       (with-current-buffer parent
         (insert "Root conversation\n")
         (niwa-session-artifacts-save session parent t))
       (let* ((child (generate-new-buffer " *agent-gate-child*"))
              (inv (niwa-agent-invocation--create
                    :agent (niwa-agent--create :name "default")
                    :agent-id "default--gate" :path "/root/check"
                    :parent-session session :parent-data-buffer parent
                    :buffer child :parent-turn 1))
              (log nil)
              (barriers nil)
              (barrier (symbol-function 'niwa-session-publication-barrier)))
         (unwind-protect
             (progn
               (setf (niwa-agent-invocation-frozen-configuration inv)
                     (with-current-buffer parent
                       (niwa-agent-exec-freeze-configuration
                        "default" inv (list :backend gptel-backend :model gptel-model))))
               (with-current-buffer child
                 (org-mode)
                 (setq-local niwa--session session niwa--agent-invocation inv)
                 (niwa-agent-conversation-configure inv child)
                 (niwa-agent-runtime--setup-transcript inv child)
                 (insert "* Agent Task: check\n\nDurable gate task\n"))
               (setf (niwa-agent-invocation-runtime-settle-callback inv)
                     (lambda (_inv _visible _event token)
                       (push (list 'callback (and token t)) log)
                       nil))
               ;; A real accepted preparation holds the gate; the settlement
               ;; must queue behind it and later run under its own token.
               (setf (niwa-session-publication-preparations session)
                     (list (cons (lambda (_token) (push 'earlier log)) nil)))
               (should (niwa-session-publication-busy-p session))
               (cl-letf (((symbol-function 'niwa-session-publication-barrier)
                          (lambda (&rest args)
                            (push args barriers)
                            (apply barrier args))))
                 (should-not
                  (niwa-agent-runtime--settle inv "finished" event))
                 (should-not (niwa-agent-invocation-runtime-settled-p inv))
                 (test-niwa-execution--wait
                  (lambda () (niwa-agent-invocation-runtime-settled-p inv))))
               (setq result
                     (list :log (reverse log)
                           :barriers (cl-remove-if #'cadr barriers)
                           :status (niwa-agent-invocation-transcript-status inv)
                           :busy (niwa-session-publication-busy-p session)
                           :token (niwa-session-publication-preparation-token
                                   session)
                           :transcript
                           (ignore-errors
                             (niwa-session-artifacts-read-artifact
                              session
                              (niwa-agent-invocation-transcript-relative-path inv)
                              t)))))
           (with-current-buffer child
             (setq-local kill-buffer-hook nil)
             (set-buffer-modified-p nil))
           (kill-buffer child)))))
    result))

(niwa-deftest niwa-agent-runtime--settle/busy-gate (:quiet t)
  ,test
  (test)
  :doc "a completed turn queues behind a real held gate and settles under its token"
  (let ((result (test-niwa-agent-conversation-publication--settle-behind-gate
                 nil)))
    (should (equal '(earlier (callback t)) (plist-get result :log)))
    ;; Every barrier inside the queued settlement carried its token.
    (should-not (plist-get result :barriers))
    (should (eq 'completed (plist-get result :status)))
    (should-not (plist-get result :busy))
    (should-not (plist-get result :token))
    (should (string-match-p "Durable gate task" (plist-get result :transcript))))
  :doc "an errored turn's cleanup also completes without draining its own gate"
  (let ((result (test-niwa-agent-conversation-publication--settle-behind-gate
                 (list :niwa-agent-terminal-status 'error
                       :error-details "Provider failed"))))
    (should (equal '(earlier (callback t)) (plist-get result :log)))
    ;; Every barrier inside the queued cleanup carried its token.
    (should-not (plist-get result :barriers))
    (should (eq 'error (plist-get result :status)))
    (should-not (plist-get result :busy))
    (should-not (plist-get result :token))
    (should (string-match-p "Durable gate task" (plist-get result :transcript)))))

(defun test-niwa-agent-conversation-publication--with-agent (function)
  "Call FUNCTION with a committed portable session and a dirty retained agent.
FUNCTION receives the session, root buffer, child buffer and invocation."
  (niwa-test-journal-capture--with-portable-session
   (lambda (session parent)
     (with-current-buffer parent
       (insert "Root conversation\n")
       (niwa-session-artifacts-save session parent t))
     (let* ((child (generate-new-buffer " *agent-prepare*"))
            (inv (niwa-agent-invocation--create
                  :agent (niwa-agent--create :name "default")
                  :agent-id "default--prepare" :path "/root/check"
                  :parent-session session :parent-data-buffer parent
                  :buffer child :parent-turn 1)))
       (unwind-protect
           (progn
             (setf (niwa-agent-invocation-frozen-configuration inv)
                   (with-current-buffer parent
                     (niwa-agent-exec-freeze-configuration
                      "default" inv (list :backend gptel-backend
                                          :model gptel-model))))
             (with-current-buffer child
               (org-mode)
               (setq-local niwa--session session
                           niwa--agent-invocation inv)
               (niwa-agent-conversation-configure inv child)
               (niwa-agent-runtime--setup-transcript inv child)
               (insert "* Agent Task: check\n\nPrepared task\n"))
             (funcall function session parent child inv))
         (niwa-agent-conversation--cancel-save inv)
         (with-current-buffer child
           (setq-local kill-buffer-hook nil)
           (set-buffer-modified-p nil))
         (kill-buffer child))))))

(defun test-niwa-agent-conversation-publication--commit (session artifacts)
  "Commit prepared ARTIFACTS for SESSION the way an adopted job would."
  (niwa-session-durability-with-transaction
    (niwa-session-publication-publish session artifacts t t)))

(niwa-deftest niwa-agent-conversation--owns-transcript-p (:quiet t)
  ,test
  (test)
  :doc "accepts only the live buffer visiting the canonical transcript"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (should (niwa-agent-conversation--owns-transcript-p inv))
     (with-current-buffer child
       (set-visited-file-name (make-temp-name "/tmp/niwa-elsewhere-") t t))
     (should-not (niwa-agent-conversation--owns-transcript-p inv))
     (with-current-buffer child (set-visited-file-name nil t t))
     (should-not (niwa-agent-conversation--owns-transcript-p inv)))))

(niwa-deftest niwa-agent-conversation--freeze-preamble (:quiet t)
  ,test
  (test)
  :doc "settles tentative state, captures a checkpoint and stamps the entry"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let (cleared updates)
       (cl-letf (((symbol-function 'niwa-ptc-checkpoint-clear-settled)
                  (lambda (_s path &rest _) (push path cleared)))
                 ((symbol-function
                   'niwa-session-persistence-update-transcript-entry)
                  (lambda (_s id plist) (push (cons id plist) updates))))
         (with-current-buffer child
           (should (niwa-agent-conversation--freeze-preamble inv session))))
       (should (equal '("/root/check") cleared))
       (should (equal "default--prepare" (caar updates)))
       (should (plist-get (cdar updates) :updated-at))))))

(niwa-deftest niwa-agent-conversation--freeze-content (:quiet t)
  ,test
  (test)
  :doc "runs before-save hooks once and freezes bytes, tick and checkpoint"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (with-current-buffer child
       (let ((runs 0))
         (add-hook 'before-save-hook (lambda () (cl-incf runs)) nil t)
         (let ((frozen (niwa-agent-conversation--freeze-content
                        inv session)))
           (should (= 1 runs))
           (should (equal (buffer-substring-no-properties
                           (point-min) (point-max))
                          (plist-get frozen :content)))
           (should (= (buffer-chars-modified-tick)
                      (plist-get frozen :frozen-tick)))
           (should (plist-get frozen :checkpoint))))))))

(niwa-deftest niwa-agent-conversation--finish-save (:quiet t)
  ,test
  (test)
  :doc "cleans the buffer only when unchanged since the freeze"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (_session _parent child _inv)
     (with-current-buffer child
       (let ((hooks 0))
         (add-hook 'after-save-hook (lambda () (cl-incf hooks)) nil t)
         (let ((tick (buffer-chars-modified-tick)))
           (insert "later edit\n")
           (niwa-agent-conversation--finish-save tick)
           (should (buffer-modified-p))
           (should (= 1 hooks)))
         (niwa-agent-conversation--finish-save
          (buffer-chars-modified-tick))
         (should-not (buffer-modified-p))
         (should (= 2 hooks)))))))

(defun test-niwa-agent-conversation-publication--unresolved (body)
  "Call BODY with an agent whose async checkpoint ended unresolved and edited.
BODY receives the session, the child buffer and the invocation.  The failed
head and the unreadable observation are injected only while the job runs."
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((child (generate-new-buffer " *agent-kill*"))
            (inv (niwa-agent-invocation--create
                  :agent (niwa-agent--create :name "default")
                  :agent-id "default--kill" :path "/root/check"
                  :parent-session session :parent-data-buffer buffer
                  :buffer child :parent-turn 1)))
       (unwind-protect
           (progn
             (setf (niwa-agent-invocation-frozen-configuration inv)
                   (with-current-buffer buffer
                     (niwa-agent-exec-freeze-configuration
                      "default" inv (list :backend gptel-backend
                                          :model gptel-model))))
             (with-current-buffer child
               (org-mode)
               (setq-local niwa--session session
                           niwa--agent-invocation inv)
               (niwa-agent-conversation-configure inv child)
               (niwa-agent-runtime--setup-transcript inv child)
               (niwa-agent-conversation--install-kill-guards)
               (insert "* Agent Task: kill\n\nBEFORE-FREEZE\n"))
             (niwa-test--with-captured-diagnostics nil
               (cl-letf (((symbol-function
                           'niwa-session-durability-commit-publication-head-async)
                          (lambda (_o _h callback)
                            (funcall callback nil
                                     '(file-error "Head write did not run"))))
                         ((symbol-function
                           'niwa-session-durability-observe-publication-head-async)
                          (lambda (_owner callback)
                            (funcall callback nil '(file-error "unreadable")))))
                 (niwa-session-persistence-checkpoint-buffer child)
                 (test-niwa-publication-async--wait
                  (lambda ()
                    (when-let* ((job (niwa-session-publication-job session)))
                      (eq 'unresolved
                          (niwa-session-publication--job-phase job)))))))
             (with-current-buffer child
               (goto-char (point-max))
               (insert "AFTER-FREEZE\n"))
             (funcall body session child inv))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (ignore-errors (niwa-session-publication-abandon session)))
         (setf (niwa-session-publication-job session) nil)
         (when (buffer-live-p child)
           (with-current-buffer child
             (setq-local kill-buffer-hook nil kill-buffer-query-functions nil)
             (set-buffer-modified-p nil))
           (kill-buffer child)))))))

(niwa-deftest niwa-agent-conversation--owes-p (:quiet t)
  ,test
  (test)
  :doc "an owned transcript owes bytes while modified or its sidecar is dirty"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (should (niwa-agent-conversation--owes-p inv))
     (with-current-buffer child (set-buffer-modified-p nil))
     (setf (niwa-agent-invocation-sidecar-dirty inv) nil)
     (should-not (niwa-agent-conversation--owes-p inv))
     (setf (niwa-agent-invocation-sidecar-dirty inv) t)
     (should (niwa-agent-conversation--owes-p inv))
     (setf (niwa-agent-invocation-sidecar-dirty inv) nil)
     (with-current-buffer child
       (insert "x")
       (set-visited-file-name (make-temp-name "/tmp/niwa-elsewhere-") t t))
     (should-not (niwa-agent-conversation--owes-p inv)))))

(niwa-deftest niwa-agent-conversation--unsaved-reason (:quiet t)
  ,test
  (test)
  :doc "saves owed bytes and explains a save that leaves them unpublished"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (should-not (niwa-agent-conversation--unsaved-reason inv))
     (should-not (with-current-buffer child (buffer-modified-p)))
     (should-not (niwa-agent-conversation--unsaved-reason inv))
     (with-current-buffer child (insert "more\n"))
     (cl-letf (((symbol-function 'niwa-agent-conversation-save)
                (lambda (&rest _) nil)))
       (let ((reason (niwa-agent-conversation--unsaved-reason inv)))
         (should (string-match-p "default--prepare" reason))
         (should (string-match-p "publication-retry" reason)))
       ;; A canceled save refuses with a message instead of a quit bubbling.
       (cl-letf (((symbol-function 'niwa-agent-conversation-save)
                  (lambda (&rest _) (signal 'quit nil))))
         (should (string-match-p
                  "canceled"
                  (niwa-agent-conversation--unsaved-reason inv))))))))

(niwa-deftest niwa-agent-conversation-allow-buffer-kill-p (:quiet t)
  ,test
  (test)
  :doc "a clean or non-agent buffer may be killed, a failing save refuses"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (with-current-buffer child
       (should (niwa-agent-conversation-allow-buffer-kill-p))
       (should-not (buffer-modified-p))
       (insert "pending\n")
       (let (message)
         (cl-letf (((symbol-function 'niwa-agent-conversation-save)
                    (lambda (&rest _) nil)))
           (niwa-test--with-captured-diagnostics message
             (should-not (niwa-agent-conversation-allow-buffer-kill-p))))
         (should (string-match-p "publication-abandon" message)))
       (should (niwa-agent-invocation-p inv))))
   )
  (with-temp-buffer
    (should (niwa-agent-conversation-allow-buffer-kill-p)))
  :doc "an unresolved checkpoint with a late edit refuses the kill and keeps bytes"
  (test-niwa-agent-conversation-publication--unresolved
   (lambda (session child inv)
     (let (message killed)
       (niwa-test--with-captured-diagnostics message
         (setq killed (kill-buffer child)))
       (should-not killed)
       (should (buffer-live-p child))
       (should (string-match-p "publication-retry" message))
       (with-current-buffer child
         (should (string-match-p "AFTER-FREEZE" (buffer-string)))
         (should (buffer-modified-p)))
       ;; Explicit recovery frees the gate; the next kill publishes the late
       ;; edit and proceeds.
       (niwa-test--with-captured-diagnostics nil
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (ignore-errors (niwa-session-publication-retry session))
           (ignore-errors (niwa-session-publication-abandon session))))
       (setf (niwa-session-publication-job session) nil)
       (niwa-test--with-captured-diagnostics nil
         (setq killed (kill-buffer child)))
       (should killed)
       (should-not (buffer-live-p child))
       (should (string-match-p
                "AFTER-FREEZE"
                (niwa-session-artifacts-read-artifact
                 session
                 (niwa-agent-invocation-transcript-relative-path inv)
                 t))))))
  :doc "production exit is refused by the persistence gate until recovery"
  (test-niwa-agent-conversation-publication--unresolved
   (lambda (session _child _inv)
     (let (exited message)
       (cl-letf (((symbol-function 'kill-emacs)
                  (lambda (&rest _) (setq exited t)))
                 ((symbol-function 'save-some-buffers) #'ignore)
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (let ((confirm-kill-processes nil)
               (confirm-kill-emacs nil))
           (niwa-test--with-captured-diagnostics message
             (save-buffers-kill-emacs))
           (should-not exited)
           (should (string-match-p "publication" message))
           (niwa-test--with-captured-diagnostics nil
             (ignore-errors (niwa-session-publication-retry session))
             (ignore-errors (niwa-session-publication-abandon session)))
           (setf (niwa-session-publication-job session) nil)
           (niwa-test--with-captured-diagnostics nil
             (save-buffers-kill-emacs))
           (should exited)))))))

(niwa-deftest niwa-agent-conversation--preserve-unsaved (:quiet t)
  ,test
  (test)
  :doc "writes the buffer bytes to a recovery file"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (let ((file (niwa-agent-conversation--preserve-unsaved inv)))
       (unwind-protect
           (should (equal (with-current-buffer child (buffer-string))
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))
         (delete-file file))))))

(niwa-deftest niwa-agent-conversation--install-kill-guards (:quiet t)
  ,test
  (test)
  :doc "installs the query and hook buffer-locally"
  (with-temp-buffer
    (niwa-agent-conversation--install-kill-guards)
    (should (memq #'niwa-agent-conversation-allow-buffer-kill-p
                  kill-buffer-query-functions))
    (should (memq #'niwa-agent-conversation--on-buffer-kill
                  kill-buffer-hook))
    (should (local-variable-p 'kill-buffer-hook))))

(niwa-deftest niwa-agent-conversation--on-buffer-kill (:quiet t)
  ,test
  (test)
  :doc "a failed save is reported with a recovery file, never silently dropped"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (let (message)
       (with-current-buffer child
         (insert "unsaved tail\n")
         (cl-letf (((symbol-function 'niwa-agent-conversation-save)
                    (lambda (&rest _) nil)))
           (niwa-test--with-captured-diagnostics message
             (niwa-agent-conversation--on-buffer-kill))))
       (should (string-match-p "unpublished changes; recovered to " message))
       (let ((file (and (string-match "recovered to \\(.+\\)$" message)
                        (string-trim (match-string 1 message)))))
         (unwind-protect
             (should (string-match-p
                      "unsaved tail"
                      (with-temp-buffer (insert-file-contents file)
                                        (buffer-string))))
           (when file (delete-file file))))
       (should (niwa-agent-invocation-p inv)))
     ;; A canceled save still recovers the bytes.
     (let (message)
       (with-current-buffer child
         (cl-letf (((symbol-function 'niwa-agent-conversation-save)
                    (lambda (&rest _) (signal 'quit nil))))
           (niwa-test--with-captured-diagnostics message
             (niwa-agent-conversation--on-buffer-kill))))
       (should (string-match-p "recovered to " message))
       (when (string-match "recovered to \\(.+\\)$" message)
         (delete-file (string-trim (match-string 1 message)))))
     ;; A save that publishes everything reports nothing.
     (let (message)
       (with-current-buffer child
         (niwa-test--with-captured-diagnostics message
           (niwa-agent-conversation--on-buffer-kill)))
       (should-not (string-match-p "recovered" message))))))

(niwa-deftest niwa-agent-conversation--drain-gate (:quiet t)
  ,test
  (test)
  :doc "passes the caller's token to the barrier"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (session _parent _child inv)
     (let (calls)
       (cl-letf (((symbol-function 'niwa-session-publication-barrier)
                  (lambda (&rest args) (push args calls) t)))
         (should (niwa-agent-conversation--drain-gate inv 'own-token)))
       (should (equal (list (list session 'own-token)) calls)))))
  :doc "skips the barrier for a session that cannot own a gate"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (session _parent _child inv)
     (let (calls)
       (setf (niwa-session-save-path session) nil)
       (cl-letf (((symbol-function 'niwa-session-publication-barrier)
                  (lambda (&rest args) (push args calls) t)))
         (should (niwa-agent-conversation--drain-gate inv nil)))
       (should-not calls))))
  :doc "an unresolved barrier reports and returns nil"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (_session _parent _child inv)
     (let (message)
       (cl-letf (((symbol-function 'niwa-session-publication-barrier)
                  (lambda (&rest _) (user-error "Outcome is unresolved"))))
         (niwa-test--with-captured-diagnostics message
           (should-not (niwa-agent-conversation--drain-gate inv nil))))
       (should (string-match-p "default--prepare" message))
       (should (string-match-p "unresolved" message))
       ;; Kept for the dispatch error that follows a failed first save.
       (should (equal "publication gate: Outcome is unresolved"
                      (niwa-agent-invocation-persistence-failure inv)))))))

(niwa-deftest niwa-agent-conversation--write/overlapping-job (:quiet t)
  ,test
  (test)
  :doc "a synchronous save drains an accepted job before it freezes the agent"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((child (generate-new-buffer " *agent-overlap*"))
            (inv (niwa-agent-invocation--create
                  :agent (niwa-agent--create :name "default")
                  :agent-id "default--overlap" :path "/root/check"
                  :parent-session session :parent-data-buffer buffer
                  :buffer child :parent-turn 1))
            (freeze (symbol-function 'niwa-agent-conversation--freeze-content))
            acks frozen-while-job message)
       (unwind-protect
           (progn
             (setf (niwa-agent-invocation-frozen-configuration inv)
                   (with-current-buffer buffer
                     (niwa-agent-exec-freeze-configuration
                      "default" inv (list :backend gptel-backend
                                          :model gptel-model))))
             (with-current-buffer child
               (org-mode)
               (setq-local niwa--session session
                           niwa--agent-invocation inv)
               (niwa-agent-conversation-configure inv child)
               (niwa-agent-runtime--setup-transcript inv child)
               (insert "* Agent Task: check\n\nOverlapping task\n"))
             ;; An accepted asynchronous job owns the gate, then a retained
             ;; agent saves synchronously while it is still in flight.
             (niwa-session-publication-submit
              session
              (lambda ()
                (list (niwa-session-artifacts--sidecar-artifact
                       session buffer)))
              (lambda (ok err) (push (list ok err) acks)))
             (should (niwa-session-publication-busy-p session))
             (cl-letf (((symbol-function
                         'niwa-agent-conversation--freeze-content)
                        (lambda (&rest args)
                          (setq frozen-while-job
                                (list (and (niwa-session-publication-job session) t)
                                      (length acks)))
                          (apply freeze args))))
               (niwa-test--with-captured-diagnostics message
                 (setf (niwa-agent-invocation-sidecar-dirty inv) t)
                 (should (niwa-agent-conversation-save inv))))
             ;; The agent froze only after the job committed and acknowledged.
             (should (equal '(nil 1) frozen-while-job))
             (should (equal '((t nil)) acks))
             (should-not (string-match-p "prepared\\|deferred\\|closed" message))
             (should-not (niwa-session-publication-busy-p session))
             (should-not (niwa-agent-invocation-sidecar-dirty inv))
             (should (string-match-p
                      "Overlapping task"
                      (niwa-session-artifacts-read-artifact
                       session
                       (niwa-agent-invocation-transcript-relative-path inv)
                       t)))
             (should (string-match-p
                      "default--overlap"
                      (niwa-session-artifacts-read-artifact
                       session "session.meta.el" t))))
         (niwa-agent-conversation--cancel-save inv)
         (with-current-buffer child
           (setq-local kill-buffer-hook nil)
           (set-buffer-modified-p nil))
         (kill-buffer child))))))

(niwa-deftest niwa-agent-conversation-prepare-save (:quiet t)
  ,test
  (test)
  :doc "freezes transcript and sidecar without mutating until the finish runs"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let* ((head (plist-get (niwa-session-publication session) :head))
            (relative (niwa-agent-invocation-transcript-relative-path inv))
            (plan (niwa-agent-conversation-prepare-save inv))
            (artifacts (plist-get plan :artifacts)))
       (should plan)
       (should (= 2 (length artifacts)))
       (should (string-match-p "Prepared task" (plist-get (car artifacts) :content)))
       (should (equal (expand-file-name relative (niwa-session-save-path session))
                      (plist-get (car artifacts) :path)))
       (should (equal "session.meta.el"
                      (file-name-nondirectory
                       (plist-get (cadr artifacts) :path))))
       (should (plist-get (cadr artifacts) :checkpoint))
       ;; Preparation commits nothing and marks nothing saved.
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (should (with-current-buffer child (buffer-modified-p)))
       (setf (niwa-agent-invocation-sidecar-dirty inv) t)
       (test-niwa-agent-conversation-publication--commit session artifacts)
       (should (with-current-buffer child (buffer-modified-p)))
       (should (niwa-agent-invocation-sidecar-dirty inv))
       (funcall (plist-get plan :finish))
       (should-not (with-current-buffer child (buffer-modified-p)))
       (should-not (niwa-agent-invocation-sidecar-dirty inv))
       (should (string-match-p
                "Prepared task"
                (niwa-session-artifacts-read-artifact session relative t))))))
  :doc "a failed commit never calls finish, so the buffer stays modified"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let ((plan (niwa-agent-conversation-prepare-save inv))
           (head (plist-get (niwa-session-publication session) :head)))
       (setf (niwa-agent-invocation-sidecar-dirty inv) t)
       (cl-letf (((symbol-function
                   'niwa-session-publication--commit-marker-publication)
                  (lambda (&rest _) (error "Injected head failure"))))
         (should-error
          (test-niwa-agent-conversation-publication--commit
           session (plist-get plan :artifacts))))
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (should (with-current-buffer child (buffer-modified-p)))
       (should (niwa-agent-invocation-sidecar-dirty inv))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (niwa-session-publication-abandon session)))))
  :doc "an edit between freeze and finish keeps the buffer modified and uncommitted"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let ((plan (niwa-agent-conversation-prepare-save inv))
           (relative (niwa-agent-invocation-transcript-relative-path inv)))
       (with-current-buffer child
         (goto-char (point-max))
         (insert "typed during the job\n"))
       (test-niwa-agent-conversation-publication--commit
        session (plist-get plan :artifacts))
       (funcall (plist-get plan :finish))
       (should (with-current-buffer child (buffer-modified-p)))
       (should-not (string-match-p
                    "typed during the job"
                    (niwa-session-artifacts-read-artifact session relative t))))))
  :doc "a property-only change after the freeze stays dirty and the next save serializes it"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (session _parent child inv)
     (let* ((plan (niwa-agent-conversation-prepare-save inv))
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
       (test-niwa-agent-conversation-publication--commit
        session (plist-get plan :artifacts))
       (funcall (plist-get plan :finish))
       ;; The frozen artifacts predate the change, so the buffer must stay dirty.
       (should (with-current-buffer child (buffer-modified-p)))
       (let* ((next (niwa-agent-conversation-prepare-save inv))
              (content (plist-get (car (plist-get next :artifacts)) :content)))
         (should (string-match-p "GPTEL_BOUNDS" content))
         ;; Bounds are positions in the saved text, drawer included.
         (let ((saved-start (1+ (string-match "Prepared task" content))))
           (should (string-match-p
                    (format "(ignore (%d %d))" saved-start
                            (+ saved-start (- end start)))
                    content)))))))
  :doc "supersedes a pending debounced save and declines when not applicable"
  (test-niwa-agent-conversation-publication--with-agent
   (lambda (_session _parent child inv)
     (let ((niwa-agent-conversation-save-debounce 1000))
       (niwa-agent-conversation-save inv t)
       (should (timerp (niwa-agent-invocation-transcript-save-timer inv)))
       (should (niwa-agent-conversation-prepare-save inv))
       (should-not (niwa-agent-invocation-transcript-save-timer inv)))
     ;; A buffer that does not visit the canonical transcript is declined.
     (with-current-buffer child
       (set-visited-file-name (make-temp-name "/tmp/niwa-elsewhere-") t t))
     (should-not (niwa-agent-conversation-prepare-save inv)))))

(niwa-deftest niwa-agent-conversation--write/without-durable-authority (:quiet t)
  ,test
  (test)
  :doc "writes nothing and keeps the transcript modified"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session parent)
     (let* ((child (generate-new-buffer " *agent-no-authority*"))
            (inv (niwa-agent-invocation--create
                  :agent (niwa-agent--create :name "default")
                  :agent-id "default--no-authority" :path "/root/check"
                  :parent-session session :parent-data-buffer parent
                  :buffer child :parent-turn 1))
            (publications 0))
       (unwind-protect
           (progn
             (setf (niwa-agent-invocation-frozen-configuration inv)
                   (with-current-buffer parent
                     (niwa-agent-exec-freeze-configuration
                      "default" inv (list :backend gptel-backend :model gptel-model))))
             (with-current-buffer child
               (org-mode)
               (setq-local niwa--session session niwa--agent-invocation inv)
               (niwa-agent-conversation-configure inv child)
               (niwa-agent-runtime--setup-transcript inv child)
               (insert "* Agent Task: check\n\nUnpublished task\n")
               (should buffer-file-name))
             ;; The removed fallback saved this canonical file directly.
             (let ((workspace (niwa-session-workspace session))
                   (mode (niwa-session-authority-mode session)))
               (unwind-protect
                   (progn
                     (setf (niwa-session-workspace session) nil
                           (niwa-session-authority-mode session) nil)
                     (cl-letf (((symbol-function 'niwa-session-publication-publish)
                                (lambda (&rest _) (cl-incf publications) nil)))
                       (should-not (niwa-agent-conversation--write inv))))
                 (setf (niwa-session-workspace session) workspace
                       (niwa-session-authority-mode session) mode)))
             (should (= 0 publications))
             (with-current-buffer child
               (should (buffer-modified-p))
               (should-not (and (file-exists-p buffer-file-name)
                                (with-temp-buffer
                                  (insert-file-contents buffer-file-name)
                                  (search-forward "Unpublished task" nil t))))))
         (niwa-agent-conversation--cancel-save inv)
         (with-current-buffer child
           (setq-local kill-buffer-hook nil)
           (set-buffer-modified-p nil))
         (kill-buffer child))))))

(provide 'test-niwa-agent-conversation-publication)
;;; test-niwa-agent-conversation-publication.el ends here
