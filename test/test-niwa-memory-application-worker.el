;;; test-niwa-memory-application-worker.el --- Owned automatic application -*- lexical-binding: t -*-

;;; Commentary:
;; Real children, retained proposals and editor interleavings protect the async boundary.

;;; Code:

(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "memory-history-fixtures"))
(require 'niwa-journal-worker)
(require 'niwa-journal-claim)
(require 'niwa-journal-store)
(require 'niwa-memory-decision)
(require 'niwa-memory-pass)
(require 'niwa-memory-notes)
(require 'niwa-memory-store)
(require 'niwa-memory-write)
(require 'niwa-permission-queue)
(require 'niwa-resource)
(require 'niwa-session-control-fs)
(require 'niwa-tool-patch)

(defvar niwa-test-application-items 1
  "Number of fresh pending notes in an application fixture.")

(defun niwa-test-application-remote-history (workspace)
  "Accept metadata-only historical remote scope in WORKSPACE, without target IO."
  (let* ((scope (niwa-memory-scope-capture workspace))
         (directory "/ssh:memory-fixture.invalid:/original/")
         (remote (list :kind 'memory :label "Historical remote" :mode 'auto
                       :dir directory :configured-dir directory :client nil
                       :target (niwa-execution-target-identity (niwa-execution-target-create directory))
                       :before nil :existing nil :directories nil :complete t))
         (scope (plist-put scope :roots (append (plist-get scope :roots) (list (cons "remote" remote)))))
         (claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180)))
    (unwind-protect
        (let* ((prepared (niwa-memory-store-prepare workspace claim scope nil ""))
               (bundle (niwa-memory-store-accept-proposals
                        workspace prepared '(:proposals nil :no-action "- Historical scope metadata.") nil "user" nil)))
          (niwa-memory-store-publish workspace (plist-get prepared :id))
          bundle)
      (niwa-journal-claim-settle claim 'cancelled ""))))

(defun niwa-test-application-reject (workspace accepted proposal &optional reason)
  "Record a rejection of ACCEPTED PROPOSAL in WORKSPACE with optional REASON."
  (plist-get (niwa-memory-decision-reject workspace (plist-get (plist-get accepted :prepared) :id)
                                            (plist-get proposal :id) :reason (or reason "Fixture decision")) :decision))

(defun niwa-test-application-wait (predicate)
  "Drive the real event loop until PREDICATE, with a progress watchdog.
This bounds a hung test, not application latency; multiple real children and
checked writes must be able to finish under single-CPU contention."
  (let ((deadline (+ (float-time) 300)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (niwa-test--application-step)))
  (should (funcall predicate)))

(defun niwa-test-application-start (workspace accepted proposal context callback)
  "Apply ACCEPTED PROPOSAL in WORKSPACE with CONTEXT and CALLBACK.
Observe the real transport owner for fault injection and cleanup assertions;
the public verb, not this helper, constructs and drives the operation."
  (let ((start (symbol-function 'niwa-journal-worker-start)) owner)
    (cl-letf (((symbol-function 'niwa-journal-worker-start)
               (cl-function (lambda (ws operation payload reply &key ((:owner job)))
                 (setq owner job)
                 (funcall start ws operation payload reply :owner job)))))
      (niwa-memory-decision-apply
       workspace (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
       :automatic context :callback callback))
    owner))

(defmacro niwa-test-application-fixture (&rest body)
  "Run BODY with an accepted pending note, STATE and its real WORKSPACE."
  (declare (indent 0) (debug t))
  `(let* ((fixture-key (list 'application-worker niwa-test-application-items))
          (root (niwa-test-memory-history-root fixture-key))
          (workspace (niwa-workspace--create :root root))
          (niwa-memory-dirs nil)
          (niwa-memory-consolidation-mode 'auto)
          (niwa-journal-cleanup--inhibit-scheduling t)
          (kill-emacs-hook kill-emacs-hook)
          (_identity (niwa-workspace-identity-ensure root))
          (accepted (niwa-test-memory-history-build
                     fixture-key root
                     (lambda ()
                       (car (niwa-test-memory-history-create workspace 1 niwa-test-application-items 100 t)))))
          (proposal (car (plist-get accepted :proposals)))
          (path (plist-get (cdr (car (plist-get proposal :before))) :path))
          (key (niwa-memory-store--claim-directory workspace))
          (context (niwa-memory-decision-automatic-context workspace (current-buffer)))
          (state (list :workspace workspace :origin-buffer (current-buffer)
                       :settled t
                       :claim (plist-get (plist-get accepted :prepared) :claim)))
          result)
     (unwind-protect
         (niwa-test--with-idle-application
           (puthash key state niwa-memory-pass--running) ,@body)
       (when-let* ((job (plist-get state :application)))
         (plist-put job :done t)
         (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
       (when-let* ((operation (gethash key niwa-memory-decision--operations)))
         (niwa-memory-decision--operation-stop operation))
       (when-let* ((retry (gethash workspace niwa-memory-pass--deferred)))
         (cancel-timer (plist-get retry :timer)) (remhash workspace niwa-memory-pass--deferred))
       (remhash key niwa-memory-pass--running)
       (when-let* ((buffer (find-buffer-visiting path)))
         (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer))
       (delete-directory root t))))

(niwa-deftest niwa-test-application-fixture
  (:doc "restored setup has independent bytes and proposal data, with no decisions or live ownership")
  (let ((niwa-permission-queue--settled-cells (make-hash-table :test #'eq))
        original-file original-path original-id entry cell)
    (niwa-test-application-fixture
      (setq original-file (copy-sequence (plist-get proposal :file))
            original-path (copy-sequence path)
            original-id (copy-sequence (plist-get proposal :id))
            entry (list :workspace workspace)
            cell (niwa-permission-queue--ensure-settled-cell entry))
      ;; Change both the live data and disk; neither may alter the snapshot.
      (aset (plist-get proposal :file) 0 ?X)
      (plist-put proposal :fixture-tampered t)
      (write-region "Changed fixture bytes" nil path nil 'silent))
    (niwa-test-application-fixture
      ;; Restoring a path must not reset an unrelated permission's identity.
      (should (eq cell (niwa-permission-queue--ensure-settled-cell entry)))
      (should (equal original-path path))
      (should (equal original-file (plist-get proposal :file)))
      (should (equal original-id (plist-get proposal :id)))
      (should-not (plist-get proposal :fixture-tampered))
      (should (equal (make-string 100 ?x) (niwa-session-control-fs-read-file path)))
      (should-not (niwa-journal-claim-owned-p (plist-get state :claim)))
      (should-not (niwa-memory-decision-status workspace original-id))
      (should-not (gethash key niwa-memory-decision--operations))
      ;; The restored bytes still authenticate through the native public verb.
      (should (eq 'applied
                  (plist-get (niwa-memory-decision-apply
                              workspace (plist-get (plist-get accepted :prepared) :id) original-id)
                             :status)))
      (should-not (file-exists-p path)))))

(niwa-deftest niwa-memory-decision--dispatch-application
  (:doc "a real minibuffer defers the transaction; idle reposts it, and duplicate events apply only once")
  (niwa-test-application-fixture
    (let* ((entry (symbol-function 'niwa-memory-decision--apply-owned))
           (event 'niwa-memory-application)
           (handler (lookup-key special-event-map (vector event)))
           calls prompt-seen)
      (cl-letf (((symbol-function 'niwa-memory-decision--apply-owned)
                 (lambda (&rest args)
                   (push (list inhibit-quit (active-minibuffer-window)) calls)
                   (apply entry args))))
        (niwa-test-application-start workspace accepted proposal context (lambda (reply) (setq result reply)))
        (niwa-test-application-wait (lambda () (eq event (car unread-command-events))))
        (pop unread-command-events)
        (minibuffer-with-setup-hook
            (lambda ()
              (setq prompt-seen (active-minibuffer-window))
              (funcall handler)
              (should-not calls)
              (should (file-exists-p path)))
          (execute-kbd-macro (kbd "M-: nil RET")))
        (should prompt-seen)
        (niwa-test-application-wait (lambda () (eq event (car unread-command-events))))
        ;; The retry timer only posted the event, not the file transaction.
        (should-not calls)
        (should (file-exists-p path))
        (niwa-test-application-wait (lambda () result))
        (funcall handler)
        (should (equal calls '((nil nil))))
        (should (eq 'applied (plist-get result :status)))
        (should-not (file-exists-p path))))))

(niwa-deftest niwa-memory-pass--apply-async ()
  ,test
  (test)
  :doc "automatic proposals wait for idle and never start the next transaction with pending input"
  (let ((niwa-test-application-items 3))
    (niwa-test-application-fixture
      (let* ((start (symbol-function 'niwa-journal-worker-start))
             (other (plist-get (cdr (car (plist-get (cadr (plist-get accepted :proposals)) :before))) :path))
             idle-callbacks quiet (pending t))
        (cl-letf (((symbol-function 'current-idle-time) (lambda () (and quiet (seconds-to-time 2))))
                  ((symbol-function 'input-pending-p) (lambda (&rest _) pending))
                  ((symbol-function 'run-with-idle-timer)
                   (lambda (_delay _repeat function &rest args)
                     (push (lambda () (apply function args)) idle-callbacks)
                     (timer-create)))
                  ((symbol-function 'niwa-journal-worker-start)
                   (lambda (ws kind payload callback &rest args)
                     (apply start ws kind
                            payload (lambda (reply)
                              (when (eq kind 'memory-decision-publish) (setq pending t))
                              (funcall callback reply)) args))))
          ;; Explicit user application remains immediate even during activity.
          (let ((explicit (nth 2 (plist-get accepted :proposals))))
            (should (eq 'applied (plist-get (niwa-memory-decision-apply
                                            workspace (plist-get (plist-get accepted :prepared) :id)
                                            (plist-get explicit :id)) :status)))
            (should-not idle-callbacks))
          (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
          (should (= 1 (length idle-callbacks)))
          (funcall (pop idle-callbacks))
          (should (file-exists-p path))
          (should (file-exists-p other))
          (should-not (gethash key niwa-memory-decision--operations))
          (setq quiet t pending nil)
          (funcall (pop idle-callbacks))
          (niwa-test-application-wait (lambda () (and pending idle-callbacks)))
          (should-not (file-exists-p path))
          (should (file-exists-p other))
          (should-not result)
          (let ((claims (directory-files key nil niwa-journal-claim--name-regexp)))
            (should (cdr (niwa-journal-claim--newest key claims t)))
            (funcall (pop idle-callbacks))
            (should (equal claims (directory-files key nil niwa-journal-claim--name-regexp)))
            (should-not (gethash key niwa-memory-decision--operations)))
          (should (file-exists-p other))
          (setq pending nil quiet nil)
          (funcall (pop idle-callbacks))
          (should (file-exists-p other))
          (setq quiet t)
          (niwa-test-application-wait
           (lambda ()
             (setq pending nil)
             (when idle-callbacks (funcall (pop idle-callbacks)))
             result))
          (should (= 2 (plist-get result :applied-count)))
          (should (= 0 (plist-get result :held-count) (plist-get result :unapplied-count)))
          (should-not (file-exists-p other))
          (should-not (plist-get result :error))))))

  :doc "activity during recovery releases its fixed claim; cancellation leaves the proposal for a fresh pending cycle"
  (niwa-test-application-fixture
    (let ((start (symbol-function 'niwa-journal-worker-start))
          (quiet t) idle-callbacks returned claim)
      (cl-letf (((symbol-function 'current-idle-time) (lambda () (and quiet (seconds-to-time 2))))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_delay _repeat function &rest args)
                   (push (lambda () (apply function args)) idle-callbacks)
                   (timer-create)))
                ((symbol-function 'niwa-journal-worker-start)
                 (lambda (ws kind payload callback &rest args)
                   (apply start ws kind
                          payload (lambda (reply)
                            (when (and (eq kind 'memory-recover) (not claim))
                              (setq quiet nil claim (copy-tree (plist-get (gethash key niwa-memory-decision--operations) :claim))))
                            (funcall callback reply)
                            (setq returned t)) args))))
        (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
        (should (= 1 (length idle-callbacks)))
        (funcall (pop idle-callbacks))
        (niwa-test-application-wait (lambda () returned))
        (should (file-exists-p path))
        (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
        (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome claim) :status)))
        (should (equal (niwa-journal-claim--record claim)
                       (niwa-journal-claim--read (niwa-journal-claim--path claim nil) nil)))
        (should-not (gethash key niwa-memory-decision--operations))
        (should (niwa-memory-pass-cancel workspace))
        (niwa-test-application-wait (lambda () result))
        (should (plist-get result :cancelled))
        (should (= 0 (plist-get result :applied-count)))
        (should (= 1 (plist-get result :held-count)))
        ;; Start without ACCEPTED or the old job's identities, like a later
        ;; normal cycle recovering durable undecided proposals after exit.
        (setq quiet t result nil idle-callbacks nil)
        (plist-put state :stop-apply nil)
        (plist-put state :application-context nil)
        (niwa-memory-pass--apply-async state nil (lambda (value) (setq result value)))
        (niwa-test-application-wait
         (lambda () (when idle-callbacks (funcall (pop idle-callbacks))) result))
        (should (= 1 (plist-get result :applied-count)))
        (should (= 0 (plist-get result :held-count) (plist-get result :unapplied-count)))
        (should-not (plist-get result :error))
        (should-not (file-exists-p path))
        (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome claim) :status)))
        ;; Unsupported automatic retries keep the existing native executor,
        ;; rather than acquiring a new idle-scheduling policy accidentally.
        (let ((native (car (niwa-test-memory-history-create workspace 1 1 100 t))))
          (setq quiet nil result nil idle-callbacks nil)
          (cl-letf (((symbol-function 'niwa-journal-worker-local-p) (lambda (_ &optional _roots) nil)))
            (niwa-memory-pass--apply-async state native (lambda (value) (setq result value)))
            (niwa-test-application-wait (lambda () result)))
          (should-not idle-callbacks)
          (should (= 1 (plist-get result :applied-count)))
          (should-not (plist-get result :error))))))

  :doc "publication runs in a targeted child after slot election and before the public projection appears"
  (niwa-test-application-fixture
    (let ((start (symbol-function 'niwa-journal-worker-start))
          (children 0) operation)
      (cl-letf (((symbol-function 'niwa-journal-worker-start)
                 (lambda (ws kind payload callback &rest args)
                   (when (eq kind 'memory-decision-publish)
                     (cl-incf children)
                     (setq operation (gethash key niwa-memory-decision--operations))
                     (should-not result)
                     (should-not (file-exists-p path))
                     (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
                     (should (eq 'completed
                                 (plist-get (niwa-journal-claim-outcome (plist-get operation :claim)) :status)))
                     (should-not (seq-find (lambda (entry) (eq 'decision (plist-get entry :kind)))
                                           (niwa-journal-store-entries root))))
                   (apply start ws kind payload callback args))))
        (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
        (niwa-test-application-wait (lambda () result)))
      (should (= 1 children))
      (should (= 1 (plist-get result :applied-count)))
      (should (= 0 (plist-get result :held-count)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should-not (plist-get result :error))
      (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
      (should-not (file-exists-p path))))

  :doc "automatic application reads public history only in children, never at the editor write boundary"
  (niwa-test-application-fixture
    (let ((program (symbol-function 'niwa-session-control-fs-run-program))
          (journal (file-name-as-directory (niwa-journal-store-directory root)))
          (reads 0))
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                 (lambda (operations &rest args)
                   (dolist (operation operations)
                     (when (and (eq (plist-get operation :op) 'read)
                                (string-prefix-p journal (plist-get operation :path)))
                       (cl-incf reads)))
                   (apply program operations args))))
        (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
        (niwa-test-application-wait (lambda () result)))
      (should (= 0 reads))
      (should (= 1 (plist-get result :applied-count)))
      (should (= 0 (plist-get result :held-count)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should-not (plist-get result :error))
      (should-not (file-exists-p path))
      (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))))

  :doc "a child-selected terminal decision is freshly authenticated before skipping target mutation"
  (niwa-test-application-fixture
    (let* ((decision (niwa-test-application-reject workspace accepted proposal (make-string 4096 ?r)))
           (file (file-name-concat (niwa-memory-decision--directory workspace)
                                   (concat (plist-get decision :decision-id) ".el")))
           (text (niwa-session-control-fs-read-file file))
           (start (symbol-function 'niwa-journal-worker-start)) injected)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'niwa-journal-worker-start)
                       (lambda (ws kind payload callback &rest args)
                         (apply start ws kind
                                payload (lambda (reply)
                                  (when (and (not injected) (plist-get reply :previous))
                                    (setq injected t)
                                    (write-region "\n" nil file t 'silent))
                                  (funcall callback reply)) args))))
              (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
              (niwa-test-application-wait (lambda () result)))
            (should injected)
            (should (string-match-p "Accepted memory decision changed" (plist-get result :error)))
            (should (= 0 (plist-get result :applied-count)))
            (should (file-exists-p path)))
        (write-region text nil file nil 'silent))))

  :doc "publication launch failure or cancellation retains the true result, recoverable decision and no clean predecessor"
  (dolist (failure '(launch cancel))
    (niwa-test-application-fixture
      (let ((start (symbol-function 'niwa-journal-worker-start))
            job entered (written 0) (callbacks 0))
        (cl-letf (((symbol-function 'niwa-journal-worker-start)
                   (lambda (ws kind payload callback &rest args)
                     (if (not (eq kind 'memory-decision-publish))
                         (apply start ws kind payload callback args)
                       (setq entered t)
                       (when (eq failure 'launch) (error "Injected publication launch failure"))
                       (let ((child (apply start ws kind payload callback args)))
                         (signal-process child 'SIGSTOP)
                         (run-at-time .01 nil (lambda () (niwa-memory-decision--operation-stop job)))
                         child)))))
          (setq job (niwa-memory-decision--start-operation
                     workspace
                     (lambda (claim live _reply)
                       (niwa-memory-decision--apply-owned
                        workspace claim (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
                        :on-applied (lambda (paths) (cl-incf written (length paths))) :live live))
                     (lambda (value) (cl-incf callbacks) (setq result value))
                     nil nil (list :application t :pass-id (plist-get (plist-get accepted :prepared) :id)
                                   :proposal-id (plist-get proposal :id))))
          (niwa-test-application-wait (lambda () result)))
        (should entered)
        (should (= 1 written callbacks))
        (should (eq 'applied (plist-get (plist-get result :value) :status)))
        (should-not (plist-get result :recovery))
        (should-not (file-exists-p path))
        (if (eq failure 'cancel) (should (plist-get result :cancelled))
          (should (string-match-p "Injected publication launch failure" (plist-get result :error))))
        (should (eq 'completed (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status)))
        (niwa-memory-decision-recover workspace)
        (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
        (should-not (niwa-memory-decision-recover workspace))
        (should (= 1 written callbacks)))))

  :doc "real child recovery and pending discovery yield before completion; pass reports once"
  (niwa-test-application-fixture
    (let ((start (symbol-function 'niwa-journal-worker-start))
	  (origin (current-buffer)) child tick (callbacks 0))
      (plist-put state :prepared (plist-get accepted :prepared))
      (plist-put state :settled nil)
      (plist-put state :accepted accepted)
      (plist-put state :entry (plist-get accepted :review))
      (plist-put state :publication-finished t)
      (plist-put state :telemetry '(:mode auto))
      (plist-put state :callback (lambda (value) (cl-incf callbacks) (setq result value)))
      (cl-letf (((symbol-function 'niwa-memory-decision-recover)
		 (lambda (&rest _) (ert-fail "Decision recovery ran in editor")))
	        ((symbol-function 'niwa-memory-store-recover)
		 (lambda (&rest _) (ert-fail "Store recovery ran in editor")))
	        ((symbol-function 'niwa-memory-decision--pending-items)
		 (lambda (&rest _) (ert-fail "Pending discovery ran in editor")))
	        ((symbol-function 'niwa-journal-worker-start)
		 (lambda (&rest args)
		   (let ((process (apply start args)))
		     (unless child
		       (setq child process)
		       (signal-process process 'SIGSTOP)
		       (run-at-time .01 nil
				    (lambda ()
				      (should (eq state (gethash key niwa-memory-pass--running)))
				      (should-not result)
				      (setq tick t)))
		       (run-at-time .05 nil (lambda () (when (process-live-p process) (signal-process process 'SIGCONT)))))
		     process))))
        (niwa-memory-pass--finish state '(:outcome success))
        (should-not result)
        (niwa-test-application-wait (lambda () result)))
      (should tick)
      (should (= 1 callbacks))
      (should (= 1 (plist-get result :applied-count)))
      (should (= 1 (plist-get result :updated-files)))
      (should-not (file-exists-p path))
      (should-not (gethash key niwa-memory-pass--running))
      (should (eq origin (plist-get state :origin-buffer)))))

  :doc "a session starting to use the note during child work holds it, unlike a turn elsewhere"
  (dolist (late '(use turn))
    (niwa-test-application-fixture
      (let ((start (symbol-function 'niwa-journal-worker-start))
	    (conversation (generate-new-buffer " *application-live-session*")))
        (unwind-protect
	    (progn
	      (with-current-buffer conversation
	        (setq-local niwa--session (niwa-session--create :workspace workspace)))
	      (cl-letf (((symbol-function 'niwa-journal-worker-start)
			 (lambda (ws operation payload callback &rest args)
			   (apply start ws operation
				  payload (lambda (reply)
				    (with-current-buffer conversation
				      (if (eq late 'use)
					  (niwa-resource-prepare
					   'read (niwa-memory-notes-address (plist-get proposal :file))
					   (list :session niwa--session))
					(setq-local niwa--current-request '(:id "unrelated-turn"))))
				    (funcall callback reply)) args))))
	        (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
	        (niwa-test-application-wait (lambda () result)))
	      (if (eq late 'turn)
		  (progn
		    (should (= 1 (plist-get result :applied-count)))
		    (should-not (file-exists-p path)))
		(should (= 1 (plist-get result :held-count)))
		(should (= 0 (plist-get result :applied-count)))
		(should (file-exists-p path))
		(should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))))
	  (kill-buffer conversation)))))

  :doc "a visiting buffer modified during child work is preserved and never applied"
  (niwa-test-application-fixture
    (let ((start (symbol-function 'niwa-journal-worker-start))
          (create-lockfiles nil) buffer)
      (cl-letf (((symbol-function 'niwa-journal-worker-start)
		 (lambda (ws operation payload callback &rest args)
		   (apply start ws operation
			  payload (lambda (reply)
			    (unless buffer
			      (setq buffer (find-file-noselect path))
			      (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved")))
			    (funcall callback reply)) args))))
        (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
        (niwa-test-application-wait (lambda () result)))
      (should (= 0 (plist-get result :applied-count)))
      (should (= 1 (plist-get result :held-count)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should-not (plist-get result :error))
      (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
      (let ((input (plist-get (niwa-memory-decision-inspect
                               workspace :pass (plist-get (plist-get accepted :prepared) :id)
                               :proposal (plist-get proposal :id)) :check)))
        (should (eq 'waiting (plist-get input :status)))
        (should (string-match-p "unsaved" (plist-get input :reason))))
      (should (equal (make-string 100 ?x) (niwa-session-control-fs-read-file path)))
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should (string-suffix-p "unsaved" (buffer-string))))))

  :doc "late admission changes after note preparation refuse mutation at the actual write boundary"
  ;; `sync' is the production fallback for worker-ineligible targets.
  (dolist (case '((use . async) (edit . async) (use . sync) (edit . sync)))
    (niwa-test-application-fixture
      (let ((late (car case))
            (commit (symbol-function 'niwa-memory-notes-commit)) injected
	    (conversation (generate-new-buffer " *application-late-turn*")))
        (unwind-protect
	    (progn
	      (with-current-buffer conversation
	        (setq-local niwa--session (niwa-session--create :workspace workspace)))
                      (cl-letf (((symbol-function 'niwa-memory-notes-commit)
                                 (lambda (&rest args)
                                   (unless injected
                                     (setq injected t)
                                     (if (eq late 'use)
                                         (with-current-buffer conversation
                                           (niwa-resource-prepare
                                            'read (niwa-memory-notes-address (plist-get proposal :file))
                                            (list :session niwa--session)))
                                       (with-current-buffer (find-file-noselect path) (goto-char (point-max)) (insert "unsaved"))))
                                   (apply commit args))))
	        (if (eq (cdr case) 'sync)
		    (setq result (niwa-memory-pass--apply state accepted))
		  (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
		  (niwa-test-application-wait (lambda () result))))
	      (should injected)
	      (should (= 0 (plist-get result :applied-count)))
	      (when (eq late 'use)
		(should (= 1 (plist-get result :held-count)))
		(should (gethash workspace niwa-memory-pass--deferred)))
	      (should (equal (make-string 100 ?x) (niwa-session-control-fs-read-file path)))
	      (should-not (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status))))
	  (when-let* ((retry (gethash workspace niwa-memory-pass--deferred)))
	    (cancel-timer (plist-get retry :timer))
	    (remhash workspace niwa-memory-pass--deferred))
	  (kill-buffer conversation)))))

  :doc "batch cancellation during a real child cannot rearm or notify twice on late success"
  (niwa-test-application-fixture
    (let ((start (symbol-function 'niwa-journal-worker-start)) operation (callbacks 0))
      (cl-letf (((symbol-function 'niwa-journal-worker-start)
		 (lambda (&rest args)
		   (let ((child (apply start args)))
		     (signal-process child 'SIGSTOP)
		     (run-at-time .05 nil (lambda ()
					    (setq operation (gethash key niwa-memory-decision--operations))
					    (niwa-memory-pass-cancel workspace)))
		     child))))
        (niwa-memory-pass--apply-async
	 state accepted (lambda (value) (cl-incf callbacks) (setq result value)))
        (niwa-test-application-wait (lambda () result)))
      (should operation)
      (niwa-memory-decision--operation-ready operation '(:ok t))
      (should (= 1 callbacks))
      (should (plist-get (plist-get state :application) :done))
      (should (= 0 (plist-get result :applied-count)))
      (should (file-exists-p path))))

  :doc "a second candidate published during one attempt is freshly observed and never renotified"
  (let ((niwa-test-application-items 2))
    (niwa-test-application-fixture
      (let* ((items (plist-get accepted :proposals))
	     (pass (plist-get (plist-get accepted :prepared) :id))
             (apply-owned (symbol-function 'niwa-memory-decision--apply-owned)) first injected trace)
        (should (= 2 (length items)))
        (cl-letf (((symbol-function 'niwa-memory-decision--apply-owned)
		   (lambda (&rest args)
                     (push (list :proposal (nth 3 args) :notify (not (null (nth 4 args)))) trace)
		     (prog1 (apply apply-owned args) (setq first t))))
                  ((symbol-function 'niwa-memory-decision--idle-p)
                   (lambda () (or (not first) injected))))
	  (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
                  (niwa-test-application-wait (lambda () first))
                  ;; The second human decision runs in the foreground, not in the worker's timer.
                  (should (eq 'applied (plist-get (niwa-memory-decision-apply
                                                   workspace pass (plist-get (cadr items) :id)) :status)))
                  (setq injected t)
	  (niwa-test-application-wait (lambda () result)))
        (should injected)
        (ert-info ((prin1-to-string trace))
                  (should (= 1 (plist-get result :applied-count))))
        (should (= 1 (plist-get result :updated-files)))
        (dolist (item items)
          (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get item :id)) :status)))))))

  :doc "accepted evidence and original workspace identity are reauthenticated after child completion"
  (dolist (tamper '(accepted identity))
    (niwa-test-application-fixture
      (let* ((start (symbol-function 'niwa-journal-worker-start))
             (file (if (eq tamper 'accepted)
                       (file-name-concat (niwa-memory-store--directory workspace (plist-get (plist-get accepted :prepared) :id)) "accepted.el")
                     (file-name-concat root ".niwa" "workspace-id")))
             (text (niwa-session-control-fs-read-file file)) injected)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'niwa-journal-worker-start)
                         (lambda (ws operation payload callback &rest args)
                           (apply start ws operation
                                  payload (lambda (reply)
                                    (unless injected
                                      (setq injected t)
                                      (write-region (if (eq tamper 'accepted) (concat text "\n") (concat (make-string 64 ?f) "\n"))
                                                    nil file nil 'silent))
                                    (funcall callback reply)) args))))
                (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
                (niwa-test-application-wait (lambda () result)))
              (should injected)
              (should (= 0 (plist-get result :applied-count)))
              (if (eq tamper 'accepted)
                  (should (> (plist-get result :unapplied-count) 0))
                (should (> (plist-get result :held-count) 0))
                (should-not (plist-get result :error)))
              (should (equal (make-string 100 ?x) (niwa-session-control-fs-read-file path))))
          (write-region text nil file nil 'silent))))))

(niwa-deftest niwa-memory-pass--pending-items ()
  ,test
  (test)
  :doc "terminal proposals across a full production batch and another pass require only one discovery child"
  (let ((niwa-test-application-items 16))
    (niwa-test-application-fixture
     (let ((start (symbol-function 'niwa-journal-worker-start)) (children 0)
           (bundles (list accepted)))
       ;; Keep the native 16-note admission bound and cross the eight-record
       ;; decision-read bound twice.  One changed version adds a second pass;
       ;; the other fifteen unchanged, already captured versions stay excluded.
       (should (= 16 niwa-memory-notes--max-notes (length (plist-get accepted :proposals))))
       (push (car (niwa-test-memory-history-create workspace 1 1 101 t)) bundles)
       (should (= 1 (length (plist-get (car bundles) :proposals))))
       (should (= 17 (apply #'+ (mapcar (lambda (bundle) (length (plist-get bundle :proposals))) bundles))))
       (let* ((last-bundle (car bundles))
              (last-item (car (plist-get last-bundle :proposals))))
         (dolist (bundle bundles)
           (dolist (item (plist-get bundle :proposals))
             (unless (eq item last-item)
               (niwa-test-application-reject workspace bundle item))))
         ;; Negative control: selection must not simply return an empty roster
         ;; beside terminal history, including the extra pass beyond a full batch.
         (niwa-test-memory-history-owned workspace
           (let ((items (plist-get (niwa-memory-decision-pending workspace) :items)))
             (should (= 1 (length items)))
             (should (equal (plist-get last-item :id) (plist-get (car items) :proposal)))
             (should (equal (plist-get (plist-get last-bundle :prepared) :id)
                            (plist-get (car items) :pass)))))
         (niwa-test-application-reject workspace last-bundle last-item))
        (niwa-test-memory-history-owned workspace
          (should-not (plist-get (niwa-memory-decision-pending workspace) :items)))
        (cl-letf (((symbol-function 'niwa-journal-worker-start)
                   (lambda (&rest args) (cl-incf children) (apply start args))))
          (niwa-memory-pass--apply-async state nil (lambda (value) (setq result value)))
          (niwa-test-application-wait (lambda () result)))
        (should (= 1 children))
        (should (= 0 (plist-get result :applied-count)))
        (should (= 0 (plist-get result :unapplied-count)))
        (should-not (plist-get result :error)))))
  :doc "undecided extraction and its waiting note disposal remain selectable beside decided history"
  (niwa-test-application-fixture
    (niwa-test-application-reject workspace accepted proposal)
    (let* ((memory (file-name-concat root "memory"))
           (niwa-memory-dirs (list memory))
           (note (file-name-concat root ".niwa" "shared" "dependency.md"))
           (address (niwa-memory-notes-address "dependency.md")))
      (make-directory memory t)
      (write-region "A durable extraction must precede note disposal." nil note nil 'silent)
      (set-file-times note (seconds-to-time (- (float-time) (* 15 86400))))
      (let* ((capture (niwa-memory-notes-capture workspace nil :mode 'auto))
             (scope (niwa-memory-scope-capture workspace))
             (scope (plist-put scope :roots (append (plist-get scope :roots) (list (plist-get capture :root)))))
             (memory-id (car (seq-find (lambda (row) (eq 'memory (plist-get (cdr row) :kind))) (plist-get scope :roots))))
             (claim (niwa-journal-claim-acquire key 180)))
        (unwind-protect
            (let* ((prepared (niwa-memory-store-prepare workspace claim scope nil ""))
                   (text (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"lesson.md\"\ntype: \"project\"\n"
                                         "title: \"Lesson\"\nhook: \"Lesson\"\nreason: \"Supported\"\nevidence: [%S]\n---\n"
                                         "Preserve extraction dependencies.\n```\n## Update\n- none\n## Merge\n- none\n"
                                         "## Remove\n- none\n## Instructions\n- none\n## No action\n- none")
                                 memory-id address))
                   (pending (niwa-memory-store-accept workspace prepared text nil "test:model" nil (list address)))
                   (items (plist-get pending :proposals))
                   (extraction (seq-find (lambda (item) (eq 'promote (plist-get item :action))) items))
                   (disposal (seq-find (lambda (item) (eq 'note-delete (plist-get item :action))) items)))
              (niwa-memory-store-publish workspace (plist-get prepared :id))
              (should (equal (list (plist-get extraction :id)) (plist-get disposal :requires)))
              (should (eq 'waiting (plist-get (niwa-memory-decision--requirements workspace disposal) :status)))
              (let ((selected (plist-get (niwa-memory-decision-pending workspace) :items)))
                (should (= 2 (length selected)))
                (should (equal (sort (mapcar (lambda (item) (plist-get item :id)) items) #'string<)
                               (sort (mapcar (lambda (item) (plist-get item :proposal)) selected) #'string<))))
              ;; An absent decision holds the note.  Omitting its roster or
              ;; substituting another proposal's decision is a protocol error.
              (dolist (selection '(undecided missing foreign))
                (let (reply)
                  (niwa-memory-decision--start-operation
                   workspace
                   (lambda (owned live observation)
                     (pcase selection
                       ('missing (setq observation (list :previous nil)))
                       ('foreign (setcdr (car (plist-get observation :requirements))
                                         (list :proposal-id (plist-get proposal :id)))))
                     (niwa-memory-decision--apply-owned
                      workspace owned (plist-get prepared :id) (plist-get disposal :id)
                      :live live :observation observation))
                   (lambda (value) (setq reply value))
                   nil nil (list :application t :pass-id (plist-get prepared :id)
                                 :proposal-id (plist-get disposal :id)))
                  (niwa-test-application-wait (lambda () reply))
                  (if (eq selection 'undecided)
                      (should (eq 'waiting (plist-get (plist-get reply :value) :status)))
                    (should (string-match-p
                             (if (eq selection 'missing) "Missing or mismatched" "belongs to another proposal")
                             (plist-get reply :error))))
                  (should (file-exists-p note))))
              ;; A dependent disposal must not reintroduce history scans into
              ;; the editor transaction after its extraction is published.
              (let ((program (symbol-function 'niwa-session-control-fs-run-program))
                    (start (symbol-function 'niwa-journal-worker-start))
                    (journal (file-name-as-directory (niwa-journal-store-directory root)))
                    (reads 0) changed-path original-text)
                (unwind-protect
                    (progn
                      (cl-letf (((symbol-function 'niwa-journal-worker-start)
                                 (lambda (ws kind payload callback &rest args)
                                   (apply start ws kind
                                          payload (lambda (observation)
                                            (when-let* ((entry (cdar (plist-get observation :requirements))))
                                              (setq changed-path
                                                    (file-name-concat (niwa-memory-decision--directory workspace)
                                                                      (concat (plist-get entry :decision-id) ".el"))
                                                    original-text (niwa-session-control-fs-read-file changed-path))
                                              (write-region "\n" nil changed-path t 'silent))
                                            (funcall callback observation)) args))))
                        (niwa-memory-pass--apply-async state pending (lambda (value) (setq result value)))
                        (niwa-test-application-wait (lambda () result)))
                      (should changed-path)
                      (should (string-match-p "Accepted memory decision changed" (plist-get result :error)))
                      (should (= 1 (plist-get result :applied-count)))
                      (should (file-exists-p note)))
                  (when original-text (write-region original-text nil changed-path nil 'silent)))
                (setq result nil)
                (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                           (lambda (operations &rest args)
                             (dolist (operation operations)
                               (when (and (eq (plist-get operation :op) 'read)
                                          (string-prefix-p journal (plist-get operation :path)))
                                 (cl-incf reads)))
                             (apply program operations args))))
                  (niwa-memory-pass--apply-async state pending (lambda (value) (setq result value)))
                  (niwa-test-application-wait (lambda () result)))
                (should (= 0 reads))
                (should (= 1 (plist-get result :applied-count)))
                (should-not (plist-get result :error))
                (should-not (file-exists-p note))
                (should (file-exists-p (file-name-concat memory "lesson.md")))))
          (niwa-journal-claim-settle claim 'cancelled "")))))
  :doc "selection authenticates decided evidence rather than trusting public status to omit it"
  (niwa-test-application-fixture
    (let* ((decision (niwa-test-application-reject workspace accepted proposal))
           (file (file-name-concat (niwa-memory-decision--directory workspace) (concat (plist-get decision :decision-id) ".el"))))
      (write-region "\n" nil file t 'silent)
      (should (equal "Accepted memory decision changed"
                     (error-message-string (should-error (niwa-memory-decision-pending workspace))))))))

(niwa-deftest niwa-memory-decision-apply/worker ()
  ,test
  (test)
  :doc "cancellation fences a real suspended child and ignores duplicate late success"
  (niwa-test-application-fixture
    (let ((automatic (niwa-memory-decision-automatic-context workspace (current-buffer)))
          (start (symbol-function 'niwa-journal-worker-start))
          child claim ready ran (callbacks 0))
      (cl-letf (((symbol-function 'niwa-journal-worker-start)
                 (cl-function (lambda (ws kind payload callback &key owner)
                   (setq ready callback claim (plist-get payload :claim)
                         child (funcall start ws kind payload callback :owner owner))))))
        (niwa-memory-decision-apply
         workspace (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
         :automatic automatic :on-applied (lambda (_) (setq ran t))
         :callback (lambda (value) (cl-incf callbacks) (setq result value))))
      (signal-process child 'SIGSTOP)
      (niwa-memory-decision-cancel automatic)
      (funcall ready '(:ok t))
      (funcall ready '(:ok t))
      (should (= 1 callbacks))
      (should (plist-get result :cancelled))
      (should-not (plist-get result :error))
      (should-not ran)
      (should (file-exists-p path))
      (should-not (process-live-p child))
      (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome claim) :status)))))

  :doc "fixed real deadline expires while child is suspended; takeover and late callback cannot commit"
  (niwa-test-application-fixture
    (let ((acquire (symbol-function 'niwa-journal-claim-acquire))
          (start (symbol-function 'niwa-journal-worker-start))
          (automatic (niwa-memory-decision-automatic-context workspace (current-buffer)))
          child claim ready successor ran (callbacks 0))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'niwa-journal-claim-acquire)
                       (lambda (directory _seconds) (funcall acquire directory 1)))
                      ((symbol-function 'niwa-journal-worker-start)
                       (cl-function (lambda (ws kind payload callback &key owner)
                         (setq ready callback claim (plist-get payload :claim)
                               child (funcall start ws kind payload callback :owner owner))))))
              (niwa-memory-decision-apply
               workspace (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
               :automatic automatic :on-applied (lambda (_) (setq ran t))
               :callback (lambda (value) (cl-incf callbacks) (setq result value))))
            (signal-process child 'SIGSTOP)
            (niwa-test-application-wait (lambda () result))
            (setq successor (funcall acquire key 180))
            (should successor)
            (should (eq 'expired (plist-get (niwa-journal-claim-outcome claim) :status)))
            (funcall ready '(:ok t))
            (should (= 1 callbacks))
            (should-not ran)
            (should (file-exists-p path))
            (should (plist-get result :error)))
        (when successor (niwa-journal-claim-settle successor 'cancelled "")))))

  :doc "origin closure, settled ownership and explicit child failure cannot enter the editor operation"
  (dolist (failure '(origin claim child))
    (niwa-test-application-fixture
      (let ((start (symbol-function 'niwa-journal-worker-start))
            (origin (generate-new-buffer " *operation-origin*")) claim ready ran injected (callbacks 0))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'niwa-journal-worker-start)
                         (lambda (ws operation payload callback &rest args)
                           (setq ready callback claim (plist-get payload :claim))
                           (apply start ws operation
                                  payload (lambda (reply)
                                    (setq injected t)
                                    (pcase failure
                                      ('origin (kill-buffer origin))
                                      ('claim (should (niwa-journal-claim-settle claim 'cancelled ""))))
                                    (funcall callback (if (eq failure 'child) '(:error "Injected child failure") reply))) args))))
                (with-current-buffer origin
                  (setq-local niwa-memory-consolidation-mode 'auto)
                  (niwa-memory-decision-apply
                   workspace (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
                   :automatic (niwa-memory-decision-automatic-context workspace origin)
                   :on-applied (lambda (_) (setq ran t))
                   :callback (lambda (value) (cl-incf callbacks) (setq result value))))
                (niwa-test-application-wait (lambda () result)))
              (should injected)
              (should (= 1 callbacks))
              (should-not ran)
              (if (eq failure 'origin)
                  (progn (should (eq 'held (plist-get result :status))) (should-not (plist-get result :error)))
                (should (plist-get result :error)))
              (should (file-exists-p path))
              (funcall ready '(:ok t))
              (should (= 1 callbacks)))
          (when (buffer-live-p origin) (kill-buffer origin))))))

  :doc "a reentrant duplicate cannot enter or finish before the outer editor function unwinds"
  (niwa-test-application-fixture
    (let ((start (symbol-function 'niwa-journal-worker-start))
          (automatic (niwa-memory-decision-automatic-context workspace (current-buffer)))
          ready observed outer-done (entries 0) (callbacks 0))
      (cl-letf (((symbol-function 'niwa-journal-worker-start)
                 (lambda (ws kind payload callback &rest args)
                   (apply start ws kind
                          payload (lambda (reply)
                            (when (eq kind 'memory-recover) (setq ready callback observed reply))
                            (funcall callback reply)) args))))
        (niwa-memory-decision-apply
         workspace (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
         :automatic automatic
         :on-applied (lambda (_paths)
                       (cl-incf entries)
                       (funcall ready observed)
                       (should (= 0 callbacks))
                       (setq outer-done t))
         :callback (lambda (reply)
                     (should outer-done)
                     (cl-incf callbacks)
                     (setq result reply)))
        (niwa-test-application-wait (lambda () result)))
      (should (= 1 entries callbacks))
      (should (eq 'applied (plist-get result :status)))
      (should-not (plist-get result :cancelled))
      (should-not (file-exists-p path))
      (funcall ready observed)
      (should (= 1 callbacks entries))))

  :doc "live origin rebind, workspace retarget and wrong-job reply refuse continuation"
  (dolist (failure '(session workspace root reply))
    (niwa-test-application-fixture
      (let* ((start (symbol-function 'niwa-journal-worker-start))
             (origin (generate-new-buffer " *operation-retarget*"))
             (session (niwa-session--create :workspace workspace))
             (other (niwa-workspace--create :root (file-name-concat root "unrelated")))
             ran injected callback-origin)
        (unwind-protect
            (progn
              (with-current-buffer origin
                (setq-local niwa--session session niwa-memory-consolidation-mode 'auto))
              (cl-letf (((symbol-function 'niwa-journal-worker-start)
                         (lambda (ws operation payload callback &rest args)
                           (apply start ws operation
                                  payload (lambda (reply)
                                    (setq injected t)
                                    (pcase failure
                                      ('session (with-current-buffer origin
                                                  (setq-local niwa--session (niwa-session--create :workspace workspace))))
                                      ('workspace (setf (niwa-session-workspace session) other))
                                      ('root (setf (niwa-workspace-root workspace) (niwa-workspace-root other)))
                                      ('reply (setq reply (plist-put reply :operation-id "another-operation"))))
                                    (funcall callback reply)) args))))
                (with-current-buffer origin
                  (niwa-memory-decision-apply
                   workspace (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
                   :automatic (niwa-memory-decision-automatic-context workspace origin)
                   :on-applied (lambda (_) (setq ran t))
                   :callback (lambda (reply) (setq callback-origin (current-buffer) result reply))))
                (niwa-test-application-wait (lambda () result)))
              (should injected)
              (should-not ran)
              (if (eq failure 'reply)
                  (should (plist-get result :error))
                (should (eq 'held (plist-get result :status)))
                (should-not (plist-get result :error)))
              (unless (eq failure 'reply) (should-not (eq callback-origin origin))))
          (setf (niwa-workspace-root workspace) root)
          (kill-buffer origin))))))

(niwa-deftest niwa-memory-decision-apply/origin-policy ()
  ,test
  (test)
  :doc "origin-local automatic mode applies while a timer runs in an unrelated propose buffer"
  (niwa-test-application-fixture
    (let ((origin (generate-new-buffer " *auto-origin*"))
          (other (generate-new-buffer " *propose-incidental*")))
      (unwind-protect
          (progn
            (with-current-buffer origin (setq-local niwa-memory-consolidation-mode 'auto))
            (with-current-buffer other (setq-local niwa-memory-consolidation-mode 'propose))
            (let ((automatic (niwa-memory-decision-automatic-context workspace origin)))
              (with-current-buffer other
                (should (niwa-memory-decision-automatic-current-p automatic))
                (niwa-memory-decision-apply
                 workspace (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
                 :automatic automatic :callback (lambda (value) (setq result value)))
                (niwa-test-application-wait (lambda () result))))
            (should (eq 'applied (plist-get result :status)))
            (should-not (file-exists-p path)))
        (kill-buffer origin) (kill-buffer other))))
  :doc "origin-local propose mode holds before worker dispatch despite an unrelated automatic buffer"
  (niwa-test-application-fixture
    (let ((origin (generate-new-buffer " *propose-origin*"))
          (other (generate-new-buffer " *auto-incidental*"))
          (start (symbol-function 'niwa-journal-worker-start)) (children 0))
      (unwind-protect
          (progn
            (with-current-buffer origin (setq-local niwa-memory-consolidation-mode 'propose))
            (with-current-buffer other (setq-local niwa-memory-consolidation-mode 'auto))
            (cl-letf (((symbol-function 'niwa-journal-worker-start)
                       (lambda (&rest args) (cl-incf children) (apply start args))))
              (with-current-buffer other
                (niwa-memory-decision-apply
                 workspace (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
                 :automatic (niwa-memory-decision-automatic-context workspace origin)
                 :callback (lambda (value) (setq result value))))
              (with-current-buffer other (niwa-test-application-wait (lambda () result))))
            (should (= 0 children))
            (should (eq 'held (plist-get result :status)))
            (should (file-exists-p path)))
        (kill-buffer origin) (kill-buffer other))))
  :doc "a late origin policy change refuses the actual write even from another automatic buffer"
  (niwa-test-application-fixture
    (let ((origin (generate-new-buffer " *late-policy-origin*"))
          (other (generate-new-buffer " *late-policy-incidental*"))
          (commit (symbol-function 'niwa-memory-notes-commit)) injected)
      (unwind-protect
          (progn
            (with-current-buffer origin (setq-local niwa-memory-consolidation-mode 'auto))
            (with-current-buffer other (setq-local niwa-memory-consolidation-mode 'auto))
            (cl-letf (((symbol-function 'niwa-memory-notes-commit)
                       (lambda (&rest args)
                         (unless injected
                           (setq injected t)
                           (with-current-buffer origin (setq-local niwa-memory-consolidation-mode 'propose)))
                         (with-current-buffer other (apply commit args)))))
              (with-current-buffer origin
                (niwa-memory-decision-apply
                 workspace (plist-get (plist-get accepted :prepared) :id) (plist-get proposal :id)
                 :automatic (niwa-memory-decision-automatic-context workspace origin)
                 :callback (lambda (value) (setq result value))))
              (with-current-buffer other (niwa-test-application-wait (lambda () result))))
            (should injected)
            (should (eq 'held (plist-get result :status)))
            (should (file-exists-p path))
            (should-not (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status))))
        (kill-buffer origin) (kill-buffer other)))))

(niwa-deftest niwa-memory-decision-cancel ()
  ,test
  (test)
  :doc "one settlement IO failure is reported while a bounded retry fences the child before deletion"
  (niwa-test-application-fixture
    (let ((settle (symbol-function 'niwa-journal-claim-settle))
          (delete (symbol-function 'delete-process)) job events (attempts 0) (callbacks 0))
      (setq job (niwa-test-application-start
                 workspace accepted proposal context (lambda (value) (push 'callback events) (cl-incf callbacks) (setq result value))))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (cl-letf (((symbol-function 'niwa-journal-claim-settle)
                 (lambda (claim &rest args)
                   (if (eq claim (plist-get job :claim))
                       (if (= 1 (cl-incf attempts))
                           (error "Injected settlement IO failure")
                         (prog1 (apply settle claim args) (push 'fenced events)))
                     (apply settle claim args))))
                ((symbol-function 'delete-process)
                 (lambda (process)
                   (when (eq process (plist-get job :worker)) (push 'deleted events))
                   (funcall delete process))))
        (niwa-memory-decision-cancel context))
      (should (= 2 attempts))
      (should (= 1 callbacks))
      (should (equal '(fenced deleted callback) (nreverse events)))
      (should (string-match-p "Injected settlement IO failure" (plist-get result :error)))
      (should-not (plist-get result :unsettled))
      (should-not (gethash key niwa-memory-decision--operations))
      (should-not (process-live-p (plist-get job :worker)))
      (niwa-memory-decision-cancel context)
      (should (= 1 callbacks))))
  :doc "persistent settlement failure reports unresolved authority and retains the tracked child without deleting it"
  (niwa-test-application-fixture
    (let ((acquire (symbol-function 'niwa-journal-claim-acquire)) job deadline (attempts 0) (callbacks 0))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'niwa-journal-claim-acquire)
                       (lambda (directory _seconds) (funcall acquire directory 2))))
              (setq job (niwa-test-application-start
                 workspace accepted proposal context (lambda (value) (cl-incf callbacks) (setq result value)))))
            (setq deadline (plist-get (plist-get job :claim) :expires-at))
            (signal-process (plist-get job :worker) 'SIGSTOP)
            (cl-letf (((symbol-function 'niwa-journal-claim-settle)
                       (lambda (&rest _) (cl-incf attempts) (error "Persistent settlement IO failure"))))
              (niwa-memory-decision-cancel context))
            (should (= 2 attempts))
            (should (= 1 callbacks))
            (should (plist-get result :unsettled))
            (should (string-match-p "Persistent settlement IO failure" (plist-get result :error)))
            (should-not (gethash key niwa-memory-decision--operations))
            (should (niwa-journal-claim-owned-p (plist-get job :claim)))
            (should (process-live-p (plist-get job :worker)))
            (should (memq (plist-get job :worker) niwa-journal-worker--processes))
            (niwa-test-application-wait (lambda () (not (process-live-p (plist-get job :worker)))))
            (should-not (plist-get job :unsettled))
            (should (= 1 callbacks))
            (should (= deadline (plist-get (plist-get job :claim) :expires-at)))
            (should (eq 'expired (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status)))
            (should-not (niwa-journal-claim-owned-p (plist-get job :claim))))
        ;; The test's failed IO is over; explicitly establish the fence before
        ;; stopping its deliberately suspended child.
        (when job
          (niwa-journal-claim-settle (plist-get job :claim) 'cancelled "")
          (when (process-live-p (plist-get job :worker)) (delete-process (plist-get job :worker)))))))
  :doc "a callback exception occurs only after settlement, process stop and registry cleanup"
  (niwa-test-application-fixture
    (let ((callbacks 0) job)
      (setq job (niwa-test-application-start
                 workspace accepted proposal context (lambda (_) (cl-incf callbacks) (error "Injected callback failure"))))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (should (equal "Injected callback failure"
                     (error-message-string (should-error (niwa-memory-decision-cancel context)))))
      (should (= 1 callbacks))
      (should-not (gethash key niwa-memory-decision--operations))
      (should-not (process-live-p (plist-get job :worker)))
      (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status)))
      (niwa-memory-decision-cancel context)
      (should (= 1 callbacks)))))

(niwa-deftest niwa-memory-decision-cancel/fence-proof ()
  ,test
  (test)
  :doc "a nil settlement without an outcome retains the child and original deadline, never treating return as proof"
  (niwa-test-application-fixture
    (let (job (attempts 0))
      (unwind-protect
          (progn
            (setq job (niwa-test-application-start
                 workspace accepted proposal context (lambda (value) (setq result value))))
            (signal-process (plist-get job :worker) 'SIGSTOP)
            (cl-letf (((symbol-function 'niwa-journal-claim-settle)
                       (lambda (&rest _) (cl-incf attempts) nil)))
              (niwa-memory-decision-cancel context))
            (should (= 2 attempts))
            (should (plist-get result :unsettled))
            (should (string-match-p "No durable memory operation fence" (plist-get result :error)))
            (should-not (niwa-journal-claim-outcome (plist-get job :claim)))
            (should (process-live-p (plist-get job :worker)))
            (should (memq (plist-get job :timer) timer-list)))
        (when job
          (niwa-journal-claim-settle (plist-get job :claim) 'cancelled "")
          (niwa-memory-decision--operation-retire job)
          (cancel-timer (plist-get job :timer))))))
  :doc "an authenticated competing outcome permits retirement after native settlement returns nil"
  (niwa-test-application-fixture
    (let ((job (niwa-test-application-start
                 workspace accepted proposal context (lambda (value) (setq result value)))))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (should (niwa-journal-claim-settle (plist-get job :claim) 'failed "competing outcome"))
      (niwa-memory-decision-cancel context)
      (should-not (plist-get result :unsettled))
      (should (plist-get result :cancelled))
      (should-not (plist-get result :error))
      (should-not (process-live-p (plist-get job :worker)))
      (should (eq 'failed (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status)))))
  :doc "the original deadline elects and authenticates expiry before deleting a suspended child"
  (niwa-test-application-fixture
    (let ((acquire (symbol-function 'niwa-journal-claim-acquire)) job (callbacks 0))
      (cl-letf (((symbol-function 'niwa-journal-claim-acquire)
                 (lambda (directory _seconds) (funcall acquire directory 2))))
        (setq job (niwa-test-application-start
                 workspace accepted proposal context (lambda (value) (cl-incf callbacks) (setq result value)))))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (niwa-test-application-wait (lambda () result))
      (should (= 1 callbacks))
      (should-not (plist-get result :unsettled))
      (should-not (process-live-p (plist-get job :worker)))
      (should (eq 'expired (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status))))))

(niwa-deftest niwa-memory-decision--recovered/fence-proof ()
  ,test
  (test)
  :doc "scheduled reply and stop retain a child without a durable release outcome"
  (dolist (phase '(reply stop))
    (niwa-test-application-fixture
      (let (job diagnostics)
        (unwind-protect
            (progn
              (setq job (niwa-memory-decision--start-recovery workspace #'ignore (current-buffer) #'always key))
              (signal-process (plist-get job :worker) 'SIGSTOP)
              (cl-letf (((symbol-function 'niwa-journal-claim-settle) (lambda (&rest _) nil)))
                (niwa-test--with-captured-diagnostics diagnostics
                                                         (if (eq phase 'reply)
                                                             (niwa-memory-decision--recovered workspace job '(:ok t))
                                                           (niwa-memory-decision-stop-recovery))))
              (should (string-match-p "No durable memory operation fence" diagnostics))
              (should-not (plist-get job :released))
              (should (plist-get job :unsettled))
              (should (process-live-p (plist-get job :worker)))
              (should (memq (plist-get job :timer) timer-list))
              (should-not (gethash key niwa-memory-decision--recovery-workers)))
          (when job
            (niwa-journal-claim-settle (plist-get job :claim) 'cancelled "")
            (niwa-memory-decision--operation-retire job)
            (cancel-timer (plist-get job :timer)))))))
  :doc "scheduled original-deadline expiry leaves a real durable outcome before child deletion"
  (niwa-test-application-fixture
    (let ((acquire (symbol-function 'niwa-journal-claim-acquire)) job diagnostics)
      (cl-letf (((symbol-function 'niwa-journal-claim-acquire)
                 (lambda (directory _seconds) (funcall acquire directory 2))))
        (setq job (niwa-memory-decision--start-recovery workspace #'ignore (current-buffer) #'always key)))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (niwa-test--with-captured-diagnostics diagnostics
                                               (niwa-test-application-wait (lambda () (not (process-live-p (plist-get job :worker))))))
      (should (string-match-p "Memory recovery timed out" diagnostics))
      (should (eq 'expired (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status)))
      (should-not (plist-get job :unsettled)))))

(niwa-deftest niwa-memory-decision--operation-finish/quit ()
  ,test
  (test)
  :doc "quit during native settlement remains visible but cannot strand registry, child or final callback"
  (niwa-test-application-fixture
    (let ((settle (symbol-function 'niwa-journal-claim-settle)) job escaped (attempts 0) (callbacks 0))
      (unwind-protect
          (progn
            (setq job (niwa-memory-decision--start-operation
                       workspace #'ignore (lambda (value) (cl-incf callbacks) (setq result value))))
            (signal-process (plist-get job :worker) 'SIGSTOP)
            (cl-letf (((symbol-function 'niwa-journal-claim-settle)
                       (lambda (&rest args)
                         (if (= 1 (cl-incf attempts)) (signal 'quit nil) (apply settle args)))))
              (condition-case nil (niwa-memory-decision--operation-stop job) (quit (setq escaped t))))
            (should-not escaped)
            (should (= 1 callbacks))
            (should (= 2 attempts))
            (should (string-match-p "Quit" (plist-get result :error)))
            (should-not (plist-get result :unsettled))
            (should-not (gethash key niwa-memory-decision--operations))
            (should-not (process-live-p (plist-get job :worker)))
            (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status))))
        (when job
          (niwa-memory-decision--operation-fence (plist-get job :claim))
          (when (process-live-p (plist-get job :worker)) (delete-process (plist-get job :worker)))
          (cancel-timer (plist-get job :timer))
          (remhash key niwa-memory-decision--operations))))))

(niwa-deftest niwa-memory-decision--recovered/quit ()
  ,test
  (test)
  :doc "quit during scheduled release preserves unresolved tracking and original retirement timer"
  (dolist (phase '(reply stop))
    (niwa-test-application-fixture
      (let (job diagnostics escaped)
        (unwind-protect
            (progn
              (setq job (niwa-memory-decision--start-recovery workspace #'ignore (current-buffer) #'always key))
              (signal-process (plist-get job :worker) 'SIGSTOP)
              (cl-letf (((symbol-function 'niwa-journal-claim-settle) (lambda (&rest _) (signal 'quit nil))))
                (niwa-test--with-captured-diagnostics diagnostics
                  (condition-case nil
                      (if (eq phase 'reply)
                          (niwa-memory-decision--recovered workspace job '(:ok t))
                        (niwa-memory-decision-stop-recovery))
                    (quit (setq escaped t)))))
              (should-not escaped)
              (should (string-match-p "Quit" diagnostics))
              (should (plist-get job :unsettled))
              (should (process-live-p (plist-get job :worker)))
              (should (memq (plist-get job :timer) timer-list))
              (should-not (gethash key niwa-memory-decision--recovery-workers)))
          (when job
            (niwa-memory-decision--operation-fence (plist-get job :claim))
            (when (process-live-p (plist-get job :worker)) (delete-process (plist-get job :worker)))
            (cancel-timer (plist-get job :timer))
            (remhash key niwa-memory-decision--recovery-workers)))))))

(niwa-deftest niwa-memory-decision--start-operation/quit ()
  ,test
  (test)
  :doc "quit at each registered setup seam fences the exact claim and delivers one visible cancellation"
  (dolist (kind '(application recovery))
    (dolist (seam '(clock request launch))
      (niwa-test-application-fixture
        (let ((clock (symbol-function 'niwa-session-control-fs-target-time))
              (write (symbol-function 'niwa-journal-worker--write))
              (launch (symbol-function 'make-process))
              (delete (symbol-function 'delete-process))
              (table (if (eq kind 'application) niwa-memory-decision--operations
                       niwa-memory-decision--recovery-workers))
              job child request diagnostics escaped injected (callbacks 0))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'niwa-session-control-fs-target-time)
                           (lambda (directory)
                             (when (and (eq seam 'clock) (not injected) (gethash key table))
                               (setq injected t job (gethash key table))
                               (signal 'quit nil))
                             (funcall clock directory)))
                          ((symbol-function 'niwa-journal-worker--write)
                           (lambda (file value)
                             (when (gethash key table)
                               (setq job (gethash key table) request file)
                               (when (eq seam 'request) (setq injected t) (signal 'quit nil)))
                             (funcall write file value)))
                          ((symbol-function 'make-process)
                           (lambda (&rest args)
                             (let ((process (apply launch args)))
                               (when (and (eq seam 'launch) (equal "niwa-journal-worker" (plist-get args :name)))
                                 (setq child process injected t)
                                 (signal-process child 'SIGSTOP)
                                 ;; A real C-g sets this flag.  The launch/handoff
                                 ;; boundary must not leave a child unrecorded.
                                 (setq quit-flag t)
                                 ;; Force a native pending-quit check when the
                                 ;; protected handoff is intentionally removed.
                                 (unless inhibit-quit (while quit-flag (ignore))))
                               process)))
                          ((symbol-function 'delete-process)
                           (lambda (process)
                             (when (eq process child)
                               (should (niwa-journal-claim-outcome (plist-get job :claim))))
                             (funcall delete process))))
                  (niwa-test--with-captured-diagnostics diagnostics
                    (condition-case nil
                        (if (eq kind 'application)
                            (niwa-memory-decision--start-operation
                             workspace #'ignore (lambda (value) (cl-incf callbacks) (setq result value)))
                          (niwa-memory-decision--start-recovery
                           workspace (lambda () (cl-incf callbacks)) (current-buffer) #'always key))
                      (quit (setq escaped t quit-flag nil)))))
                (should injected)
                (should-not escaped)
                (should-not (gethash key table))
                (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status)))
                (if (eq kind 'application)
                    (progn
                      (should (= 1 callbacks))
                      (should (plist-get result :cancelled))
                      (should (string-match-p "Quit" (plist-get result :reason)))
                      (should-not (plist-get result :error)))
                  (should (= 0 callbacks)) (should (string-match-p "Quit" diagnostics)))
                (when child
                  (should (eq child (plist-get job :worker)))
                  (should-not (process-live-p child)))
                (when request (should-not (file-directory-p (file-name-directory request)))))
            (setq quit-flag nil)
            (when job
              (niwa-memory-decision--operation-fence (plist-get job :claim))
              (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
            (when (and child (process-live-p child)) (delete-process child))
            (when (and request (file-directory-p (file-name-directory request)))
              (delete-directory (file-name-directory request) t))
            (remhash key table)))))))

(niwa-deftest niwa-journal-worker-start/immediate-exit ()
  ,test
  (test)
  :doc "an immediately exiting native child has an owner before its sentinel fences and cleans up"
  (niwa-test-application-fixture
    (let ((launch (symbol-function 'make-process))
          (write (symbol-function 'niwa-journal-worker--write))
          request job child (callbacks 0))
      (cl-letf (((symbol-function 'niwa-journal-worker--write)
                 (lambda (file value) (setq request file) (funcall write file value)))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (when (equal "niwa-journal-worker" (plist-get args :name))
                     (setq args (plist-put args :command
                                           (list (expand-file-name invocation-name invocation-directory)
                                                 "--batch" "-Q" "--eval" "(kill-emacs 0)"))))
                   (setq child (apply launch args)))))
        (setq job (niwa-memory-decision--start-operation
                   workspace #'ignore (lambda (value) (cl-incf callbacks) (setq result value))))
        (should (eq child (plist-get job :worker)))
        (niwa-test-application-wait (lambda () result)))
      (should (= 1 callbacks))
      (should (string-match-p "Journal worker exited with status 0" (plist-get result :error)))
      (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status)))
      (should-not (gethash key niwa-memory-decision--operations))
      (should-not (memq child niwa-journal-worker--processes))
      (should-not (process-live-p child))
      (should-not (file-directory-p (file-name-directory request))))))

(niwa-deftest niwa-journal-worker-start/delivered-quit ()
  ,test
  (test)
  :doc "pending quit is delivered inside owned setup rather than at a later caller loop"
  (niwa-test-application-fixture
    (let ((launch (symbol-function 'make-process)) job child escaped)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'make-process)
                       (lambda (&rest args)
                         (setq child (apply launch args))
                         (signal-process child 'SIGSTOP)
                         (setq quit-flag t)
                         child)))
              (condition-case nil
                  (progn
                    (setq job (niwa-memory-decision--start-operation
                               workspace #'ignore (lambda (reply) (setq result reply))))
                    ;; A pending flag must not be left for a later native check
                    ;; in an unprotected caller or event-loop continuation.
                    (while quit-flag (ignore)))
                (quit (setq escaped t quit-flag nil))))
            (should-not escaped)
            (should (plist-get result :cancelled))
            (should-not (gethash key niwa-memory-decision--operations))
            (should-not (process-live-p child))
            (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status))))
        (setq quit-flag nil)
        (when-let* ((active (gethash key niwa-memory-decision--operations)))
          (niwa-memory-decision--operation-stop active))
        (when (and child (process-live-p child)) (delete-process child))))))

(niwa-deftest niwa-journal-worker-start/inhibited-quit ()
  ,test
  (test)
  :doc "an inherited quit inhibition retains the outer pending flag without stealing cancellation"
  (niwa-test-application-fixture
    (let ((launch (symbol-function 'make-process)) child job pending)
      (unwind-protect
          (progn
            (let ((inhibit-quit t))
              (cl-letf (((symbol-function 'make-process)
                         (lambda (&rest args)
                           (setq child (apply launch args))
                           (signal-process child 'SIGSTOP)
                           (setq quit-flag t)
                           child)))
                (setq job (niwa-memory-decision--start-operation
                           workspace #'ignore (lambda (reply) (setq result reply)))))
              (setq pending quit-flag)
              ;; The outer owner consumes its flag, not worker startup.
              (setq quit-flag nil))
            (should pending)
            (should-not result)
            (should (eq child (plist-get job :worker)))
            (should (memq child niwa-journal-worker--processes))
            (should (niwa-journal-claim-owned-p (plist-get job :claim)))
            (niwa-memory-decision--operation-stop job)
            (should-not (process-live-p child))
            (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status))))
        (setq quit-flag nil)
        (when job (niwa-memory-decision--operation-stop job))))))

(niwa-deftest niwa-memory-pass--application-step/quit ()
  ,test
  (test)
  :doc "quit after popping an item completes visibly without dispatching subsequent work"
  (niwa-test-application-fixture
    (let ((start (symbol-function 'niwa-memory-decision--start-operation))
          (callbacks 0) (dispatches 0) injected)
      (cl-letf (((symbol-function 'niwa-memory-decision--start-operation)
                 (lambda (&rest args)
                   (cl-incf dispatches)
                   (if injected (apply start args) (setq injected t) (signal 'quit nil)))))
        (niwa-memory-pass--apply-async state accepted (lambda (value) (cl-incf callbacks) (setq result value)))
        (niwa-test-application-wait (lambda () result)))
      (should injected)
      (should (= 1 dispatches))
      (should (= 1 callbacks))
      (should (plist-get result :cancelled))
      (should-not (plist-get result :error))
      (should (= 1 (plist-get result :held-count)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should (= 0 (plist-get result :applied-count)))
      (should (plist-get state :stop-apply))
      (should (file-exists-p path))
      (should-not (gethash key niwa-memory-decision--operations)))))

(niwa-deftest niwa-memory-pass--apply-async/quit-batch ()
  ,test
  (test)
  :doc "native setup or cleanup quit stops a two-proposal batch instead of starting the next child"
  (dolist (seam '(request settle))
    (let ((niwa-test-application-items 2))
      (niwa-test-application-fixture
        (let* ((write (symbol-function 'niwa-journal-worker--write))
               (settle (symbol-function 'niwa-journal-claim-settle))
               (start (symbol-function 'niwa-journal-worker-start))
               (other (cadr (plist-get accepted :proposals)))
               (other-path (plist-get (cdr (car (plist-get other :before))) :path))
               operation injected (children 0) (callbacks 0))
          (plist-put state :prepared (plist-get accepted :prepared))
          (plist-put state :settled nil)
          (plist-put state :accepted accepted)
          (plist-put state :entry (plist-get accepted :review))
          (plist-put state :publication-finished t)
          (plist-put state :telemetry '(:mode auto))
          (plist-put state :callback (lambda (value) (cl-incf callbacks) (setq result value)))
          (cl-letf (((symbol-function 'niwa-journal-worker-start)
                     (lambda (&rest args)
                       (when (eq (nth 1 args) 'memory-recover) (cl-incf children))
                       (apply start args)))
                    ((symbol-function 'niwa-journal-worker--write)
                     (lambda (&rest args)
                       (when (and (eq seam 'request) (not injected))
                         (setq injected t operation (gethash key niwa-memory-decision--operations))
                         (signal 'quit nil))
                       (apply write args)))
                    ((symbol-function 'niwa-journal-claim-settle)
                     (lambda (claim &rest args)
                       (let ((job (gethash key niwa-memory-decision--operations)))
                         (when (and (eq seam 'settle) (not injected) job
                                    (plist-get job :done) (eq claim (plist-get job :claim)))
                           (setq injected t operation job) (signal 'quit nil)))
                       (apply settle claim args))))
            (niwa-memory-pass--finish state '(:outcome success))
            (niwa-test-application-wait (lambda () result)))
          (should injected)
          (should (= 1 children))
          (should (= 1 callbacks))
          (should (plist-get state :stop-apply))
          (should (plist-get result :cancelled))
          (if (eq seam 'request)
              (progn
                (should-not (plist-get result :error))
                (should (= 2 (plist-get result :held-count)))
                (should (equal (make-string 100 ?x) (niwa-session-control-fs-read-file path)))
                (should-not (niwa-memory-decision-status workspace (plist-get proposal :id))))
            (should (string-match-p "Quit" (plist-get result :error)))
            (should (= 1 (plist-get result :held-count)))
            (should-not (file-exists-p path))
            (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status))))
          (should (= 0 (plist-get result :unapplied-count)))
          (should (= (if (eq seam 'request) 0 1) (plist-get result :applied-count)))
          (should (= (if (eq seam 'request) 0 1) (plist-get result :updated-files)))
          (should (file-exists-p other-path))
          (should-not (niwa-memory-decision-status workspace (plist-get other :id)))
          (should operation)
          (should (niwa-journal-claim-outcome (plist-get operation :claim)))
          (should-not (process-live-p (plist-get operation :worker)))
          (should-not (gethash key niwa-memory-decision--operations))
          (should-not (gethash key niwa-memory-pass--running))
          (should-not (gethash workspace niwa-memory-pass--deferred))
          (niwa-memory-decision--operation-ready operation '(:ok t))
          (should (= 1 callbacks)))))))

(niwa-deftest niwa-memory-pass--apply-async/cleanup-failure ()
  ,test
  (test)
  :doc "a post-application settlement failure is visible in the final caller outcome"
  (niwa-test-application-fixture
    (let ((settle (symbol-function 'niwa-journal-claim-settle)) operation injected (callbacks 0))
      (plist-put state :prepared (plist-get accepted :prepared))
      (plist-put state :settled nil)
      (plist-put state :accepted accepted)
      (plist-put state :entry (plist-get accepted :review))
      (plist-put state :publication-finished t)
      (plist-put state :telemetry '(:mode auto))
      (plist-put state :callback (lambda (value) (cl-incf callbacks) (setq result value)))
      (cl-letf (((symbol-function 'niwa-journal-claim-settle)
                 (lambda (claim &rest args)
                   (let ((job (gethash key niwa-memory-decision--operations)))
                     (when (and (not injected) job (plist-get job :done) (eq claim (plist-get job :claim)))
                       (setq injected t operation job)
                       (error "Visible settlement IO failure")))
                   (apply settle claim args))))
        (niwa-memory-pass--finish state '(:outcome success))
        (niwa-test-application-wait (lambda () result)))
      (should injected)
      (should (= 1 (plist-get result :applied-count)))
      (should (= 1 (plist-get result :updated-files)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should-not (plist-get result :cancelled))
      (should (string-match-p "Visible settlement IO failure" (plist-get result :error)))
      (should-not (file-exists-p path))
      (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
      (should (eq 'completed (plist-get (niwa-journal-claim-outcome (plist-get operation :claim)) :status)))
      (should-not (gethash key niwa-memory-decision--operations))
      (should-not (gethash key niwa-memory-pass--running))
      (should-not (gethash workspace niwa-memory-pass--deferred))
      (niwa-memory-decision--operation-ready operation '(:ok t))
      (should (= 1 callbacks)))))

(niwa-deftest niwa-memory-decision--worker-local-p ()
  ,test
  (test)
  :doc "authenticated historical remote authority overrides a local current-config dispatch gate"
  (niwa-test-application-fixture
    (let* ((bundle (niwa-test-application-remote-history workspace))
           (scope (plist-get (plist-get bundle :prepared) :scope))
           (truename (symbol-function 'file-truename)) touched)
      (should (niwa-memory-pass--async-application-p workspace accepted))
      (cl-letf (((symbol-function 'file-truename)
                 (lambda (file)
                   (if (file-remote-p file)
                       (progn (setq touched t) file)
                     (funcall truename file)))))
        ;; Changing current configuration is deliberately not revocation of
        ;; this frozen root; the real authority check reaches its original path.
        (should (niwa-memory-scope--root scope "remote")))
      (should touched)
      (let ((file-name-handler-alist
             (cons (cons "memory-fixture[.]invalid"
                         (lambda (operation &rest args)
                           (if (eq operation 'file-remote-p)
                               (let ((file-name-handler-alist (cdr file-name-handler-alist)))
                                 (apply #'file-remote-p args))
                             (ert-fail (format "Remote IO: %S" operation)))))
                   file-name-handler-alist)))
        (should-not (niwa-memory-decision--worker-local-p workspace))))))

(niwa-deftest niwa-memory-pass--apply-async/remote-history ()
  ,test
  (test)
  :doc "real child classifies remote history before recovery; native fallback freshly selects and applies pending work"
  (niwa-test-application-fixture
    (niwa-test-application-remote-history workspace)
    (let ((start (symbol-function 'niwa-journal-worker-start))
          (recover (symbol-function 'niwa-memory-decision-recover))
          (store (symbol-function 'niwa-memory-store-recover))
          (children 0) (recoveries 0) (stores 0) replies recovered)
      (cl-letf (((symbol-function 'niwa-journal-worker-start)
                 (lambda (ws operation payload callback &rest args)
                   (cl-incf children)
                   (apply start ws operation
                          payload (lambda (reply)
                            (push reply replies)
                            (funcall callback reply)) args)))
                ((symbol-function 'niwa-memory-decision-recover)
                 (lambda (&rest args) (cl-incf recoveries) (apply recover args)))
                ((symbol-function 'niwa-memory-store-recover)
                 (lambda (&rest args) (cl-incf stores) (apply store args))))
        (niwa-memory-pass--apply-async state nil (lambda (value) (setq result value)))
        (niwa-test-application-wait (lambda () result))
        ;; Pending recovery is native.  The adjacent proposal claim needs only
        ;; child status discovery and targeted publication, not remote recovery.
        (should (= 3 children))
        (should (= 1 recoveries stores))
        ;; The existing scheduled-recovery callback consumes the same worker
        ;; reply, so it too must perform native recovery rather than skip it.
        (niwa-memory-decision--start-recovery workspace (lambda () (setq recovered t)) (current-buffer) #'always key)
        (niwa-test-application-wait (lambda () recovered)))
      ;; The completed proposal needs no second application child.
      (should (= 4 children))
      (should (= 2 recoveries))
      (should (= 2 stores))
      (should (= 2 (cl-count-if (lambda (reply) (plist-get reply :synchronous)) replies)))
      (should (cl-every (lambda (reply) (not (plist-get reply :accepted))) replies))
      (should (= 1 (plist-get result :applied-count)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should-not (file-exists-p path)))))

(niwa-deftest niwa-memory-decision--recovered/remote-history ()
  ,test
  (test)
  :doc "native fallback checks the original publication claim and settles it before notification"
  (niwa-test-application-fixture
    (niwa-test-application-remote-history workspace)
    (let ((recover (symbol-function 'niwa-memory-decision-recover))
          (store (symbol-function 'niwa-memory-store-recover)) job claim events)
      (cl-letf (((symbol-function 'niwa-memory-decision-recover)
                 (lambda (ws)
                   (should (niwa-journal-claim-owned-p claim))
                   (push 'decisions events)
                   (funcall recover ws)))
                ((symbol-function 'niwa-memory-store-recover)
                 (lambda (ws)
                   (should (niwa-journal-claim-owned-p claim))
                   (push 'store events)
                   (funcall store ws))))
        (setq job (niwa-memory-decision--start-recovery
                   workspace (lambda ()
                               (should (niwa-journal-claim-outcome claim))
                               (push 'notified events)) (current-buffer) #'always key)
              claim (plist-get job :claim))
        (niwa-test-application-wait (lambda () (memq 'notified events))))
      (should (equal '(decisions store notified) (nreverse events)))
      (should (file-exists-p path))
      (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
      (should-not (gethash key niwa-memory-decision--recovery-workers)))))

(niwa-deftest niwa-memory-decision--worker-local-p/uncertain-expiry ()
  ,test
  (test)
  :doc "uncertain acceptance still reaches checked native recovery of interrupted accepted expiry"
  (niwa-test-application-fixture
    (let* ((journal (niwa-journal-store-directory root))
           (digest (niwa-journal-store-publish-digest
                    root (list :capture-id (make-string 64 ?e) :session "closed" :session-name "Closed"
                               :workspace _identity :trigger 'session-end :segment 1
                               :source-revision (make-string 64 ?b) :turns '(1) :turn-ids (list (make-string 64 ?c))
                               :created "2020-01-01T00:00:00Z" :model "test:model")
                    "## Done\n- none\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))
           (mutation (niwa-journal-claim-acquire (niwa-journal-store-claim-directory journal 'mutation) 120))
           (manifest (file-name-concat (niwa-journal-store-state-directory journal) "expiry"
                                       (concat (plist-get mutation :owner) ".json")))
           (text (json-serialize
                  (append (niwa-journal-claim--record mutation)
                          (list :entries (vector (list :file (plist-get digest :file) :kind "digest"
                                                       :id (plist-get digest :capture-id) :private []
                                                       :sha256 (secure-hash 'sha256 (plist-get digest :text))))))))
           (file (file-name-concat (niwa-memory-store--directory workspace (plist-get (plist-get accepted :prepared) :id)) "accepted.el"))
           (original (niwa-session-control-fs-read-file file))
           (start (symbol-function 'niwa-journal-worker-start)) synchronous ran)
      (unwind-protect
          (progn
            (make-directory (file-name-directory manifest) t)
            (write-region text nil manifest nil 'silent)
            (should (niwa-journal-claim-settle mutation 'completed (secure-hash 'sha256 text)))
            (delete-file file)
            (cl-letf (((symbol-function 'niwa-journal-worker-start)
                       (lambda (ws operation payload callback &rest args)
                         (apply start ws operation
                                payload (lambda (reply)
                                  (setq synchronous (plist-get reply :synchronous))
                                  (funcall callback reply)) args))))
              (niwa-memory-decision--start-operation
               workspace (lambda (&rest _) (setq ran t)) (lambda (reply) (setq result reply)))
              (niwa-test-application-wait (lambda () result)))
            (should synchronous)
            (should-not ran)
            (should (plist-get result :error))
            (should-not (file-exists-p (file-name-concat journal (plist-get digest :file))))
            (should-not (file-exists-p file)))
        (write-region original nil file nil 'silent)))))

(niwa-deftest niwa-memory-decision--start-operation/context-error ()
  ,test
  (test)
  :doc "a real malformed context identity cannot strand an acquired workspace claim"
  (niwa-test-application-fixture
    (let* ((file (file-name-concat root ".niwa" "workspace-id"))
           (text (niwa-session-control-fs-read-file file))
           (niwa-workspace-identity--cache (make-hash-table :test #'equal))
           successor)
      (unwind-protect
          (progn
            (write-region "malformed\n" nil file nil 'silent)
            (should (string-match-p "Invalid workspace identity"
                                    (error-message-string
                                     (should-error (niwa-memory-decision--start-operation workspace #'ignore #'ignore)))))
            (should-not (gethash key niwa-memory-decision--operations))
            (setq successor (niwa-journal-claim-acquire key 180))
            (should successor))
        (when successor (niwa-journal-claim-settle successor 'cancelled ""))
        (write-region text nil file nil 'silent)))))

(niwa-deftest niwa-memory-decision--operation-stop/rollback ()
  ,test
  (test)
  :doc "cancelling after the target batch rolls back under the original owner before callback and elects nothing"
  (niwa-test-application-fixture
    (let* ((memory (file-name-concat root ".niwa" "memory"))
           (niwa-memory-dirs (list memory))
           (_directory (make-directory memory t))
           (scope (niwa-memory-scope-capture workspace))
           (root-id (car (seq-find (lambda (row) (eq 'memory (plist-get (cdr row) :kind))) (plist-get scope :roots))))
           (claim (niwa-journal-claim-acquire key 180))
           (prepared (niwa-memory-store-prepare workspace claim scope nil ""))
           (bundle (niwa-memory-store-accept
                    workspace prepared
                    (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"new.md\"\ntype: \"project\"\n"
                                    "title: \"New\"\nhook: \"Guidance\"\nreason: \"Retain\"\nevidence: []\n---\nNew guidance.\n```\n"
                                    "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none") root-id)
                    nil "test:model" nil))
           (mutate (symbol-function 'niwa-memory-write--batch)) job stopped (callbacks 0))
      (niwa-memory-store-publish workspace (plist-get prepared :id))
      (niwa-journal-claim-settle claim 'cancelled "")
      (cl-letf (((symbol-function 'niwa-memory-write--batch)
                 (lambda (&rest args)
                   (prog1 (apply mutate args)
                     (unless stopped
                       (setq stopped t)
                       (should (= 2 (length (nth 2 args))))
                       (should (niwa-journal-claim-owned-p (plist-get job :claim)))
                       (should (= 0 callbacks))
                       (niwa-memory-decision--operation-stop job))))))
        (setq job
              (niwa-memory-decision--start-operation
               workspace
               (lambda (owner live _reply)
                 (niwa-memory-decision--apply-owned
                  workspace owner (plist-get prepared :id) (plist-get (car (plist-get bundle :proposals)) :id) :live live))
               (lambda (reply) (cl-incf callbacks) (setq result reply)) nil nil
               (list :application t :pass-id (plist-get prepared :id) :proposal-id (plist-get (car (plist-get bundle :proposals)) :id))))
        (niwa-test-application-wait (lambda () result)))
      (should stopped)
      (ert-info ((prin1-to-string result))
                (should (eq 'held (plist-get (plist-get result :value) :status)))
                (should (plist-get result :cancelled))
                (should-not (plist-get (plist-get result :value) :decision-id))
                (should-not (plist-get result :error)))
      (should-not (niwa-memory-decision-status workspace (plist-get (car (plist-get bundle :proposals)) :id)))
      (should-not (file-exists-p (file-name-concat memory "new.md")))
      (should-not (file-exists-p (file-name-concat memory "MEMORY.md")))
      (should (= 1 callbacks)))))

(niwa-deftest niwa-memory-pass--apply-async/quit-rollback ()
  ,test
  (test)
  :doc "C-g rolls back under the original target lock or accounts a failed rollback without hiding cancellation"
  (dolist (intervene '(nil t))
    (niwa-test-application-fixture
    (let* ((memory (file-name-concat root ".niwa" "memory"))
           (niwa-memory-dirs (list memory))
           (_directory (make-directory memory t))
           (scope (niwa-memory-scope-capture workspace))
           (root-id (car (seq-find (lambda (row) (eq 'memory (plist-get (cdr row) :kind))) (plist-get scope :roots))))
           (claim (niwa-journal-claim-acquire key 180))
           (prepared (niwa-memory-store-prepare workspace claim scope nil ""))
           (bundle (niwa-memory-store-accept
                    workspace prepared
                    (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"new.md\"\ntype: \"project\"\n"
                                    "title: \"New\"\nhook: \"Guidance\"\nreason: \"Retain\"\nevidence: []\n---\nNew guidance.\n```\n"
                                    "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none") root-id)
                    nil "test:model" nil))
           (mutate (symbol-function 'niwa-memory-write--batch)) interrupted intervened-path (callbacks 0))
      (niwa-memory-store-publish workspace (plist-get prepared :id))
      (niwa-journal-claim-settle claim 'cancelled "")
      (cl-letf (((symbol-function 'niwa-memory-write--batch)
                 (lambda (&rest args)
                   (prog1 (apply mutate args)
                     (unless interrupted
                       (setq interrupted t)
                       (should (= 2 (length (nth 2 args))))
                       (when intervene
                         (setq intervened-path (plist-get (car (nth 2 args)) :path))
                         (write-region "Intervening external edit" nil intervened-path nil 'silent))
                       (signal 'quit nil))))))
        (niwa-memory-pass--apply-async state bundle (lambda (reply) (cl-incf callbacks) (setq result reply)))
        (niwa-test-application-wait (lambda () result)))
      (should interrupted)
      (should (= 1 callbacks))
      (should (plist-get state :stop-apply))
      (should (plist-get result :cancelled))
      (should (= 0 (plist-get result :applied-count)))
      (should (file-exists-p path))
      (should-not (gethash key niwa-memory-decision--operations))
      (should-not (niwa-memory-decision-status workspace (plist-get (car (plist-get bundle :proposals)) :id)))
      (if intervene
          (progn
            (should (string-match-p (regexp-quote intervened-path) (plist-get result :error)))
            (should (= 1 (plist-get result :unapplied-count)))
            (should (equal "Intervening external edit" (niwa-session-control-fs-read-file intervened-path))))
        (should-not (plist-get result :error))
        (should (= 1 (plist-get result :held-count)))
        (should (= 0 (plist-get result :unapplied-count)))
        (should-not (file-exists-p (file-name-concat memory "new.md")))
        (should-not (file-exists-p (file-name-concat memory "MEMORY.md"))))))))

(niwa-deftest niwa-memory-pass--apply-async/settled-cancellation ()
  ,test
  (test)
  :doc "actual completed write is counted before cancellation holds the second proposal"
  (let ((niwa-test-application-items 2))
    (niwa-test-application-fixture
      (let* ((apply-owned (symbol-function 'niwa-memory-decision--apply-owned))
             (other (cadr (plist-get accepted :proposals)))
             (other-path (plist-get (cdr (car (plist-get other :before))) :path))
             stopped (callbacks 0))
        (cl-letf (((symbol-function 'niwa-memory-decision--apply-owned)
                   (lambda (&rest args)
                     (prog1 (apply apply-owned args)
                       (unless stopped
                         (setq stopped t)
                         (niwa-memory-decision--operation-stop
                          (gethash key niwa-memory-decision--operations)))))))
          (niwa-memory-pass--apply-async state accepted (lambda (reply) (cl-incf callbacks) (setq result reply)))
          (niwa-test-application-wait (lambda () result)))
        (should stopped)
        (should (= 1 callbacks))
        (should (plist-get result :cancelled))
        (should-not (plist-get result :error))
        (should (= 1 (plist-get result :applied-count)))
        (should (= 1 (plist-get result :updated-files)))
        (should (= 1 (plist-get result :held-count)))
        (should (= 0 (plist-get result :unapplied-count)))
        (should-not (file-exists-p path))
        (should (file-exists-p other-path))
        (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
        (should-not (niwa-memory-decision-status workspace (plist-get other :id)))
        (should-not (gethash key niwa-memory-decision--operations)))))

  :doc "C-g after actual note mutation retains applied result and stops the remaining proposal"
  (let ((niwa-test-application-items 2))
    (niwa-test-application-fixture
      (let* ((run (symbol-function 'niwa-memory-notes-commit))
             (other (cadr (plist-get accepted :proposals)))
             (other-path (plist-get (cdr (car (plist-get other :before))) :path))
             interrupted (callbacks 0))
        (cl-letf (((symbol-function 'niwa-memory-notes-commit)
                   (lambda (&rest args)
                     (prog1 (apply run args)
                       (unless interrupted (setq interrupted t) (signal 'quit nil))))))
          (niwa-memory-pass--apply-async state accepted (lambda (reply) (cl-incf callbacks) (setq result reply)))
          (niwa-test-application-wait (lambda () result)))
        (should interrupted)
        (should (= 1 callbacks))
        (should (plist-get result :cancelled))
        (should-not (plist-get result :error))
        (should (= 1 (plist-get result :applied-count)))
        (should (= 1 (plist-get result :updated-files)))
        (should (= 1 (plist-get result :held-count)))
        (should (= 0 (plist-get result :unapplied-count)))
        (should-not (file-exists-p path))
        (should (file-exists-p other-path))
        (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
        (should-not (niwa-memory-decision-status workspace (plist-get other :id)))
        (should-not (gethash key niwa-memory-decision--operations)))))

  :doc "an interrupted deletion reply never elects an application from matching bytes"
  (niwa-test-application-fixture
    (let ((run (symbol-function 'niwa-session-control-fs-lock-session-run)) interrupted)
      (cl-letf (((symbol-function 'niwa-session-control-fs-lock-session-run)
                 (lambda (session operations)
                   (prog1 (funcall run session operations)
                     (when (member (list :op 'delete-file :path path) operations)
                       (setq interrupted t)
                       (signal 'quit nil))))))
        (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
        (niwa-test-application-wait (lambda () result)))
      (should interrupted)
      (should (plist-get result :cancelled))
      (should (= 0 (plist-get result :applied-count) (plist-get result :updated-files)))
      (should-not (file-exists-p path))
      (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))))

  :doc "actual authenticated result survives cancellation and genuine final cleanup failure"
  (dolist (cleanup '(nil failure))
    (niwa-test-application-fixture
      (let ((settle (symbol-function 'niwa-journal-claim-settle)) job actual injected (callbacks 0))
        (cl-letf (((symbol-function 'niwa-journal-claim-settle)
                   (lambda (token &rest args)
                     (when (and cleanup job (not injected) (plist-get job :done)
                                (eq token (plist-get job :claim)))
                       (setq injected t)
                       (error "Injected final result cleanup failure"))
                     (apply settle token args))))
          (setq job
                (niwa-memory-decision--start-operation
                 workspace
                 (lambda (claim live _reply)
                   (setq actual (niwa-memory-decision--apply-owned
                                 workspace claim (plist-get (plist-get accepted :prepared) :id)
                                 (plist-get proposal :id) :live live))
                   (niwa-memory-decision--operation-stop job)
                   actual)
                 (lambda (reply) (cl-incf callbacks) (setq result reply))
                 nil nil (list :application t :pass-id (plist-get (plist-get accepted :prepared) :id)
                               :proposal-id (plist-get proposal :id))))
          (niwa-test-application-wait (lambda () result)))
        (should (= 1 callbacks))
        (should (plist-get result :ok))
        (should (plist-get result :cancelled))
        (should (eq 'applied (plist-get actual :status)))
        (should (equal actual (plist-get result :value)))
        (if cleanup
            (progn (should injected) (should (string-match-p "Injected final result cleanup failure" (plist-get result :error))))
          (should-not (plist-get result :error)))
        (niwa-memory-decision--operation-ready job '(:ok t))
        (should (= 1 callbacks))
        (should-not (file-exists-p path))
        (should-not (gethash key niwa-memory-decision--operations))
        ;; The genuine accepted application won completion before the user
        ;; cancellation; the fence's winning status does not classify cause.
        (should (eq 'completed (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status)))))))

(niwa-deftest niwa-memory-decision--start-operation/fallback-consumer ()
  ,test
  (test)
  :doc "unsupported native fallback propagates consumer error or quit once after release"
  (dolist (condition '(error quit))
    (niwa-test-application-fixture
      (let ((niwa-journal-worker--child-p t) job caught (callbacks 0))
        (condition-case err
            (niwa-memory-decision--start-operation
             workspace
             (lambda (_claim _live reply)
               (should (plist-get reply :synchronous))
               (setq job (gethash key niwa-memory-decision--operations))
               '(:status skipped))
             (lambda (_reply)
               (cl-incf callbacks)
               (should (plist-get job :done))
               (should-not (gethash key niwa-memory-decision--operations))
               (signal condition (when (eq condition 'error) '("Injected fallback consumer failure")))))
          ((error quit) (setq caught err)))
        (should job)
        (should (= 1 callbacks))
        (should (eq condition (car caught)))
        (when (eq condition 'error)
          (should (equal "Injected fallback consumer failure" (error-message-string caught))))
        (should-not (gethash key niwa-memory-decision--operations))
        (should-not (memq (plist-get job :timer) timer-list))
        (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :status)))
        (should (file-exists-p path))
        (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))))))

(niwa-deftest niwa-memory-pass--apply-async/applied-cleanup-error ()
  ,test
  (test)
  :doc "public pass retains real applied accounting and actionable cancelled cleanup error"
  (niwa-test-application-fixture
    (let ((apply-owned (symbol-function 'niwa-memory-decision--apply-owned))
          (settle (symbol-function 'niwa-journal-claim-settle))
          job injected (callbacks 0))
      (plist-put state :prepared (plist-get accepted :prepared))
      (plist-put state :settled nil)
      (plist-put state :accepted accepted)
      (plist-put state :entry (plist-get accepted :review))
      (plist-put state :publication-finished t)
      (plist-put state :telemetry '(:mode auto))
      (plist-put state :callback (lambda (value) (cl-incf callbacks) (setq result value)))
      (cl-letf (((symbol-function 'niwa-memory-decision--apply-owned)
                 (lambda (&rest args)
                   (setq job (gethash key niwa-memory-decision--operations))
                   (prog1 (apply apply-owned args)
                     (niwa-memory-decision--operation-stop job))))
                ((symbol-function 'niwa-journal-claim-settle)
                 (lambda (token &rest args)
                   (when (and job (not injected) (plist-get job :done) (eq token (plist-get job :claim)))
                     (setq injected t)
                     (error "Injected applied cleanup failure"))
                   (apply settle token args))))
        (niwa-memory-pass--finish state '(:outcome success))
        (niwa-test-application-wait (lambda () result)))
      (should injected)
      (should (= 1 callbacks))
      (should (plist-get result :cancelled))
      (should (string-match-p "Injected applied cleanup failure" (plist-get result :error)))
      (should (= 1 (plist-get result :applied-count)))
      (should (= 1 (plist-get result :updated-files)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should-not (file-exists-p path))
      (should (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status)))
      (should-not (gethash key niwa-memory-pass--running))
      (should-not (gethash key niwa-memory-decision--operations))
      (should-not (gethash workspace niwa-memory-pass--deferred))
      (niwa-memory-decision--operation-ready job '(:ok t))
      (should (= 1 callbacks)))))

(niwa-deftest niwa-memory-pass--apply-async/recovery-chain ()
  ,test
  (test)
  :doc "ten checked decisions recover history once; pending selection independently verifies continuity"
  (let ((niwa-test-application-items 10))
    (niwa-test-application-fixture
      (let ((start (symbol-function 'niwa-journal-worker-start))
            (full 0) (selection 0) (discovery 0) (publication 0))
        (cl-letf (((symbol-function 'niwa-journal-worker-start)
                   (cl-function (lambda (ws operation payload callback &key owner)
                     (when (eq operation 'memory-decision-publish) (cl-incf publication))
                     (when (eq operation 'memory-recover)
                       (cond ((not (plist-get payload :recovery-predecessor)) (cl-incf full))
                             ((plist-get payload :pending-selection) (cl-incf selection))
                             (t (cl-incf discovery))))
                     (funcall start ws operation payload callback :owner owner)))))
          (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
          (niwa-test-application-wait (lambda () result)))
        (should (= 1 full selection))
        (should (= 9 discovery))
        (should (= 10 publication))
        (should (= 10 (plist-get result :applied-count)))
        (should (= 0 (plist-get result :unapplied-count)))
        (should-not (plist-get result :error))
        (dolist (item (plist-get accepted :proposals))
          (let* ((decision (niwa-memory-decision-status workspace (plist-get item :id)))
                 (slot (niwa-memory-decision--published workspace decision)))
            (should (eq 'applied (plist-get decision :status)))
            (should (equal (plist-get item :before) (plist-get slot :before)))
            (should-not (file-exists-p (plist-get (cdr (car (plist-get item :before))) :path))))))))

  :doc "a foreign consolidation owner between decisions forces new recovery and preserves its decision"
  (let ((niwa-test-application-items 2))
    (niwa-test-application-fixture
      (let ((next (symbol-function 'niwa-memory-pass--application-next))
            (start (symbol-function 'niwa-journal-worker-start))
            (second (cadr (plist-get accepted :proposals))) (full 0) resume)
        (cl-letf (((symbol-function 'niwa-journal-worker-start)
                   (cl-function (lambda (ws operation payload callback &key owner)
                     (when (and (eq operation 'memory-recover) (not (plist-get payload :recovery-predecessor))) (cl-incf full))
                     (funcall start ws operation payload callback :owner owner))))
                  ((symbol-function 'niwa-memory-pass--application-next)
                   (lambda (state job)
                     (if (and (not resume) (= 1 (plist-get job :applied)))
                         (setq resume (lambda () (funcall next state job)))
                       (funcall next state job)))))
          (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
          (niwa-test-application-wait (lambda () resume))
          (let ((claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180)))
            (unwind-protect
                (should (eq 'rejected
                            (plist-get (niwa-memory-decision-reject workspace (plist-get (plist-get accepted :prepared) :id)
                                                                      (plist-get second :id) :reason "Foreign decision") :status)))
              (niwa-journal-claim-settle claim 'cancelled "")))
          (funcall resume)
          (niwa-test-application-wait (lambda () result)))
        (should (= 2 full))
        (should (= 1 (plist-get result :applied-count)))
        (should (eq 'rejected (plist-get (niwa-memory-decision-status workspace (plist-get second :id)) :status)))
        (should (file-exists-p (plist-get (cdr (car (plist-get second :before))) :path))))))

  :doc "a failed operation cannot carry recovery continuity into the next decision"
  (let ((niwa-test-application-items 2))
    (niwa-test-application-fixture
      (let ((apply-owned (symbol-function 'niwa-memory-decision--apply-owned))
            (start (symbol-function 'niwa-journal-worker-start)) (full 0) injected)
        (cl-letf (((symbol-function 'niwa-journal-worker-start)
                   (cl-function (lambda (ws operation payload callback &key owner)
                     (when (and (eq operation 'memory-recover) (not (plist-get payload :recovery-predecessor))) (cl-incf full))
                     (funcall start ws operation payload callback :owner owner))))
                  ((symbol-function 'niwa-memory-decision--apply-owned)
                   (lambda (&rest args)
                     (unless injected (setq injected t) (error "Injected first application failure"))
                     (apply apply-owned args))))
          (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
          (niwa-test-application-wait (lambda () result)))
        (should injected)
        (should (= 2 full))
        (should (= 1 (plist-get result :applied-count)))
        (should (= 1 (plist-get result :unapplied-count)))
        (should (string-match-p "Injected first application failure" (plist-get result :error)))
        (should (file-exists-p path)))))

  :doc "late access, disk changes and unsaved edits still protect a later note after successful recovery"
  (dolist (late '(access edit disk))
    (let ((niwa-test-application-items 2))
      (niwa-test-application-fixture
        (let* ((next (symbol-function 'niwa-memory-pass--application-next))
               (create-lockfiles nil)
               (second (cadr (plist-get accepted :proposals)))
               (second-path (plist-get (cdr (car (plist-get second :before))) :path))
               (conversation (generate-new-buffer " *chain-note-user*")) buffer injected)
          (unwind-protect
              (progn
                (with-current-buffer conversation
                  (setq-local niwa--session (niwa-session--create :workspace workspace)))
                (cl-letf (((symbol-function 'niwa-memory-pass--application-next)
                           (lambda (state job)
                             (when (and (not injected) (= 1 (plist-get job :applied)))
                               (setq injected t)
                               (if (eq late 'access)
                                   (with-current-buffer conversation
                                     (niwa-resource-prepare 'read (niwa-memory-notes-address (plist-get second :file))
                                                               (list :session niwa--session)))
                                 (if (eq late 'disk)
                                     (write-region "Intervening disk change" nil second-path nil 'silent)
                                   (setq buffer (find-file-noselect second-path))
                                   (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved")))))
                             (funcall next state job))))
                  (niwa-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
                  (niwa-test-application-wait (lambda () result)))
                (should injected)
                (should (= 1 (plist-get result :applied-count)))
                (should (file-exists-p second-path))
                (should-not (eq 'applied (plist-get (niwa-memory-decision-status workspace (plist-get second :id)) :status)))
                (if (eq late 'disk)
                    (progn
                      (should (= 1 (plist-get result :unapplied-count)))
                      (should (eq 'stale (plist-get (niwa-memory-decision-status workspace (plist-get second :id)) :status)))
                      (should (equal "Intervening disk change" (niwa-session-control-fs-read-file second-path))))
                  (should (= 1 (plist-get result :held-count)))
                  (should (= 0 (plist-get result :unapplied-count)))
                  (should-not (niwa-memory-decision-status workspace (plist-get second :id)))
                  (when (eq late 'edit)
                    (with-current-buffer buffer (should (buffer-modified-p)))
                    (let ((input (plist-get (niwa-memory-decision-inspect
                                             workspace :pass (plist-get (plist-get accepted :prepared) :id)
                                             :proposal (plist-get second :id)) :check)))
                      (should (eq 'waiting (plist-get input :status)))
                      (should (string-match-p "unsaved" (plist-get input :reason)))))))
            (when (buffer-live-p buffer) (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer))
            (kill-buffer conversation)))))))

(niwa-deftest niwa-memory-decision--recovery-successor-p ()
  ,test
  (test)
  :doc "fresh exact tokens, outcome bytes, actor and complete pinned observations are mandatory"
  (niwa-test-application-fixture
    (let* ((prior (niwa-journal-claim-acquire key 180))
           (outcome (niwa-journal-claim-settle prior 'cancelled ""))
           (claim (niwa-journal-claim-acquire key 180))
           (context (niwa-memory-decision--context workspace (current-buffer)))
           (job (append (list :claim claim) context))
           (predecessor (append (list :claim prior :outcome outcome) context)))
      (unwind-protect
          (progn
            (should (niwa-memory-decision--recovery-successor-p job predecessor))
            (let ((changed (copy-tree predecessor)))
              (plist-put (plist-get changed :outcome) :payload "Not the completed outcome")
              (should-not (niwa-memory-decision--recovery-successor-p job changed)))
            (should-not (niwa-memory-decision--recovery-successor-p (plist-put (copy-sequence job) :origin 'other) predecessor))
            (should-not (niwa-memory-decision--recovery-successor-p (append job '(:decisions-only t)) predecessor))
            (let ((changed (copy-tree predecessor)))
              (plist-put (plist-get changed :claim) :generation (- (plist-get prior :generation) 1))
              (should-not (niwa-memory-decision--recovery-successor-p job changed)))
            (let ((program (symbol-function 'niwa-session-control-fs-run-program)))
              (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                         (lambda (&rest args) (cdr (apply program args)))))
                (should-not (niwa-memory-decision--recovery-successor-p job predecessor)))))
        (niwa-journal-claim-settle claim 'cancelled "")))))

(niwa-deftest niwa-memory-decision-recover-child ()
  ,test
  (test)
  :doc "successor discovery reads the private slot, not the public journal"
  (niwa-test-application-fixture
    (let* ((decision (niwa-test-application-reject workspace accepted proposal))
           (prior (niwa-journal-claim-acquire key 180))
           (outcome (niwa-journal-claim-settle prior 'cancelled ""))
           (claim (niwa-journal-claim-acquire key 180))
           (payload (list :claim claim :operation-id (plist-get claim :owner)
                          :recovery-predecessor (list :claim prior :outcome outcome)
                          :application t :pass-id (plist-get decision :pass-id)
                          :proposal-id (plist-get proposal :id))))
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-journal-store-entries)
                     (lambda (&rest _) (error "Unexpected public journal scan"))))
            (should (eq 'rejected (plist-get (plist-get (niwa-memory-decision-recover-child workspace payload) :previous) :status)))
            (write-region "\n" nil (file-name-concat (niwa-memory-decision--directory workspace)
                                                     (concat (plist-get decision :decision-id) ".el")) t 'silent)
            (should (equal "Accepted memory decision changed"
                           (error-message-string (should-error (niwa-memory-decision-recover-child workspace payload))))))
        (niwa-journal-claim-settle claim 'cancelled "")))))

(provide 'test-niwa-memory-application-worker)
;;; test-niwa-memory-application-worker.el ends here
