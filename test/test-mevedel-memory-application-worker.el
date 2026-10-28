;;; test-mevedel-memory-application-worker.el --- Owned automatic application -*- lexical-binding: t -*-

;;; Commentary:
;; Real children, retained proposals and editor interleavings protect the async boundary.

;;; Code:

(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "memory-history-fixtures"))
(require 'mevedel-journal-worker)
(require 'mevedel-journal-claim)
(require 'mevedel-journal-store)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-session-control-fs)
(require 'mevedel-tool-patch)

(defvar mevedel-test-application-items 1
  "Number of fresh pending notes in an application fixture.")

(defun mevedel-test-application-remote-history (workspace)
  "Accept metadata-only historical remote scope in WORKSPACE, without target IO."
  (let* ((scope (mevedel-memory-scope-capture workspace))
         (directory "/ssh:memory-fixture.invalid:/original/")
         (remote (list :kind 'memory :label "Historical remote" :mode 'auto
                       :dir directory :configured-dir directory :client nil
                       :target (mevedel-execution-target-identity (mevedel-execution-target-create directory))
                       :before nil :existing nil :directories nil :complete t))
         (scope (plist-put scope :roots (append (plist-get scope :roots) (list (cons "remote" remote)))))
         (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
    (unwind-protect
        (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
               (bundle (mevedel-memory-store-accept-proposals
                        workspace prepared '(:proposals nil :no-action "- Historical scope metadata.") nil "user" nil)))
          (mevedel-memory-store-publish workspace (plist-get prepared :id))
          bundle)
      (mevedel-journal-claim-settle claim 'cancelled ""))))

(defun mevedel-test-application-decide (workspace accepted proposal status)
  "Freeze and publish a real accepted STATUS for ACCEPTED PROPOSAL in WORKSPACE.
Use the native acceptance and publication boundaries without repeated recovery
while constructing retained history.  Recovery still owns final pin release."
  (let ((claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
    (should claim)
    (unwind-protect
        (let ((decision (mevedel-memory-decision--accept workspace claim accepted proposal status "Fixture decision")))
          (mevedel-memory-store--mutation
           workspace
           (lambda (mutation)
             (mevedel-memory-store--assert-owned mutation)
             (mevedel-journal-store-publish-decision (mevedel-workspace-root workspace) (plist-get decision :metadata)))))
      (mevedel-journal-claim-settle claim 'cancelled ""))))

(defun mevedel-test-application-wait (predicate)
  "Drive the real event loop until PREDICATE, with a bounded failure deadline."
  (let ((deadline (+ (float-time) 30)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil .01)))
  (should (funcall predicate)))

(defmacro mevedel-test-application-fixture (&rest body)
  "Run BODY with an accepted pending note, STATE and its real WORKSPACE."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-application-" t))
          (workspace (mevedel-workspace--create :root root))
          (mevedel-memory-dirs nil)
          (mevedel-memory-consolidation-mode 'auto)
          (mevedel-journal-cleanup--inhibit-scheduling t)
          (kill-emacs-hook kill-emacs-hook)
          (_identity (mevedel-workspace-identity-ensure root))
          (accepted (car (mevedel-test-memory-history-create workspace 1 mevedel-test-application-items 100 t)))
          (proposal (car (plist-get accepted :proposals)))
          (path (plist-get (cdr (car (plist-get proposal :before))) :path))
          (key (mevedel-memory-store--claim-directory workspace))
          (state (list :workspace workspace :origin-buffer (current-buffer)
                       :settled t
                       :claim (plist-get (plist-get accepted :prepared) :claim)))
          result)
     (unwind-protect
         (progn (puthash key state mevedel-memory-pass--running) ,@body)
       (when-let* ((job (plist-get state :application)))
         (plist-put job :done t)
         (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
       (when-let* ((operation (gethash key mevedel-memory-decision--operations)))
         (mevedel-memory-decision--operation-stop operation))
       (when-let* ((retry (gethash workspace mevedel-memory-pass--deferred)))
         (cancel-timer (plist-get retry :timer)) (remhash workspace mevedel-memory-pass--deferred))
       (remhash key mevedel-memory-pass--running)
       (when-let* ((buffer (find-buffer-visiting path)))
         (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer))
       (delete-directory root t))))

(mevedel-deftest mevedel-memory-pass--apply-async ()
  ,test
  (test)
  :doc "real child recovery and pending discovery yield before completion; pass reports once"
  (mevedel-test-application-fixture
    (let ((start (symbol-function 'mevedel-journal-worker-start))
	  (origin (current-buffer)) child tick (callbacks 0))
      (plist-put state :prepared (plist-get accepted :prepared))
      (plist-put state :settled nil)
      (plist-put state :accepted accepted)
      (plist-put state :entry (plist-get accepted :review))
      (plist-put state :publication-finished t)
      (plist-put state :telemetry '(:mode auto))
      (plist-put state :callback (lambda (value) (cl-incf callbacks) (setq result value)))
      (cl-letf (((symbol-function 'mevedel-memory-decision-recover)
		 (lambda (&rest _) (ert-fail "Decision recovery ran in editor")))
	        ((symbol-function 'mevedel-memory-store-recover)
		 (lambda (&rest _) (ert-fail "Store recovery ran in editor")))
	        ((symbol-function 'mevedel-memory-pass--pending-items)
		 (lambda (&rest _) (ert-fail "Pending discovery ran in editor")))
	        ((symbol-function 'mevedel-journal-worker-start)
		 (lambda (&rest args)
		   (let ((process (apply start args)))
		     (unless child
		       (setq child process)
		       (signal-process process 'SIGSTOP)
		       (run-at-time .01 nil
				    (lambda ()
				      (should (eq state (gethash key mevedel-memory-pass--running)))
				      (should-not result)
				      (setq tick t)))
		       (run-at-time .05 nil (lambda () (when (process-live-p process) (signal-process process 'SIGCONT)))))
		     process))))
        (mevedel-memory-pass--finish state '(:outcome success))
        (should-not result)
        (mevedel-test-application-wait (lambda () result)))
      (should tick)
      (should (= 1 callbacks))
      (should (= 1 (plist-get result :applied-count)))
      (should (= 1 (plist-get result :updated-files)))
      (should-not (file-exists-p path))
      (should-not (gethash key mevedel-memory-pass--running))
      (should (eq origin (plist-get state :origin-buffer)))))

  :doc "a request or pending agent settlement starting during child work holds the note"
  (dolist (pending '(request settlement))
    (mevedel-test-application-fixture
      (let ((start (symbol-function 'mevedel-journal-worker-start))
	    (conversation (generate-new-buffer " *application-live-turn*")))
        (unwind-protect
	    (progn
	      (with-current-buffer conversation
	        (setq-local mevedel--session (mevedel-session--create :workspace workspace)))
	      (cl-letf (((symbol-function 'mevedel-journal-worker-start)
			 (lambda (ws operation callback &rest args)
			   (apply start ws operation
				  (lambda (reply)
				    (with-current-buffer conversation
				      (if (eq pending 'request)
					  (setq-local mevedel--current-request '(:id "late-user-turn"))
				        (setq-local mevedel--turn-settlements-pending t)))
				    (funcall callback reply)) args))))
	        (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
	        (mevedel-test-application-wait (lambda () result)))
	      (should (= 1 (plist-get result :held-count)))
	      (should (= 0 (plist-get result :applied-count)))
	      (should (file-exists-p path))
	      (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id))))
	  (kill-buffer conversation)))))

  :doc "a visiting buffer modified during child work is preserved and never applied"
  (mevedel-test-application-fixture
    (let ((start (symbol-function 'mevedel-journal-worker-start)) buffer)
      (cl-letf (((symbol-function 'mevedel-journal-worker-start)
		 (lambda (ws operation callback &rest args)
		   (apply start ws operation
			  (lambda (reply)
			    (unless buffer
			      (setq buffer (find-file-noselect path))
			      (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved")))
			    (funcall callback reply)) args))))
        (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
        (mevedel-test-application-wait (lambda () result)))
      (should (= 0 (plist-get result :applied-count)))
      (should (= 1 (plist-get result :unapplied-count)))
      (should (equal (make-string 100 ?x) (mevedel-session-control-fs-read-file path)))
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should (string-suffix-p "unsaved" (buffer-string))))))

  :doc "late admission changes after note preparation refuse mutation at the actual write boundary"
  (dolist (late '(turn edit))
    (mevedel-test-application-fixture
      (let ((mkdir (symbol-function 'mevedel-session-control-fs-make-directory)) injected
	    (conversation (generate-new-buffer " *application-late-turn*")))
        (unwind-protect
	    (progn
	      (with-current-buffer conversation
	        (setq-local mevedel--session (mevedel-session--create :workspace workspace)))
	      (cl-letf (((symbol-function 'mevedel-session-control-fs-make-directory)
			 (lambda (directory &rest args)
			   (prog1 (apply mkdir directory args)
			     (when (and (not injected) (string-suffix-p "/blobs" (directory-file-name directory)))
			       (setq injected t)
			       (if (eq late 'turn)
				   (with-current-buffer conversation (setq-local mevedel--current-request '(:id "late")))
				 (with-current-buffer (find-file-noselect path) (goto-char (point-max)) (insert "unsaved"))))))))
	        (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
	        (mevedel-test-application-wait (lambda () result)))
	      (should injected)
	      (should (= 0 (plist-get result :applied-count)))
	      (should (equal (make-string 100 ?x) (mevedel-session-control-fs-read-file path)))
	      (should-not (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status))))
	  (kill-buffer conversation)))))

  :doc "batch cancellation during a real child cannot rearm or notify twice on late success"
  (mevedel-test-application-fixture
    (let ((start (symbol-function 'mevedel-journal-worker-start)) operation (callbacks 0))
      (cl-letf (((symbol-function 'mevedel-journal-worker-start)
		 (lambda (&rest args)
		   (let ((child (apply start args)))
		     (signal-process child 'SIGSTOP)
		     (run-at-time .05 nil (lambda ()
					    (setq operation (gethash key mevedel-memory-decision--operations))
					    (mevedel-memory-pass-cancel workspace)))
		     child))))
        (mevedel-memory-pass--apply-async
	 state accepted (lambda (value) (cl-incf callbacks) (setq result value)))
        (mevedel-test-application-wait (lambda () result)))
      (should operation)
      (mevedel-memory-decision--operation-ready operation '(:ok t))
      (should (= 1 callbacks))
      (should (plist-get (plist-get state :application) :done))
      (should (= 0 (plist-get result :applied-count)))
      (should (file-exists-p path))))

  :doc "a second candidate published during one attempt is freshly observed and never renotified"
  (let ((mevedel-test-application-items 2))
    (mevedel-test-application-fixture
      (let* ((items (plist-get accepted :proposals))
	     (pass (plist-get (plist-get accepted :prepared) :id))
             (apply-owned (symbol-function 'mevedel-memory-decision--apply-owned)) injected trace)
        (should (= 2 (length items)))
        (cl-letf (((symbol-function 'mevedel-memory-decision--apply-owned)
		   (lambda (&rest args)
                     (push (list :proposal (nth 3 args) :notify (not (null (nth 4 args)))) trace)
		     (prog1 (apply apply-owned args)
		       (unless injected
			 (setq injected t)
			 (should (eq 'applied
				     (plist-get (mevedel-memory-decision-apply
						 workspace pass (plist-get (cadr items) :id)) :status))))))))
	  (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
	  (mevedel-test-application-wait (lambda () result)))
        (should injected)
        (ert-info ((prin1-to-string trace))
                  (should (= 1 (plist-get result :applied-count))))
        (should (= 1 (plist-get result :updated-files)))
        (dolist (item items)
          (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get item :id)) :status)))))))

  :doc "accepted evidence and original workspace identity are reauthenticated after child completion"
  (dolist (tamper '(accepted identity))
    (mevedel-test-application-fixture
      (let* ((start (symbol-function 'mevedel-journal-worker-start))
             (file (if (eq tamper 'accepted)
                       (file-name-concat (mevedel-memory-store--directory workspace (plist-get (plist-get accepted :prepared) :id)) "accepted.el")
                     (file-name-concat root ".mevedel" "workspace-id")))
             (text (mevedel-session-control-fs-read-file file)) injected)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                         (lambda (ws operation callback &rest args)
                           (apply start ws operation
                                  (lambda (reply)
                                    (unless injected
                                      (setq injected t)
                                      (write-region (if (eq tamper 'accepted) (concat text "\n") (concat (make-string 64 ?f) "\n"))
                                                    nil file nil 'silent))
                                    (funcall callback reply)) args))))
                (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value)))
                (mevedel-test-application-wait (lambda () result)))
              (should injected)
              (should (= 0 (plist-get result :applied-count)))
              (if (eq tamper 'accepted)
                  (should (> (plist-get result :unapplied-count) 0))
                (should (> (plist-get result :held-count) 0))
                (should-not (plist-get result :error)))
              (should (equal (make-string 100 ?x) (mevedel-session-control-fs-read-file path))))
          (write-region text nil file nil 'silent))))))

(mevedel-deftest mevedel-memory-pass--pending-items ()
  ,test
  (test)
  :doc "64 authenticated terminal proposals select no pending identities and require only one discovery child"
  (let ((mevedel-test-application-items 16))
    (mevedel-test-application-fixture
     (let ((start (symbol-function 'mevedel-journal-worker-start)) (children 0)
           (bundles (list accepted)))
        ;; Keep the native 16-note admission bound: four real passes, not a
       ;; widened request limit or fabricated public status.  Each version is
       ;; distinct, so a preceding pending review does not suppress its capture.
       (dotimes (n 3)
         (push (car (mevedel-test-memory-history-create workspace 1 16 (+ 101 n) t)) bundles))
        (should (= 64 (apply #'+ (mapcar (lambda (bundle) (length (plist-get bundle :proposals))) bundles))))
        (dolist (bundle bundles)
          (dolist (item (plist-get bundle :proposals))
            (mevedel-test-application-decide workspace bundle item 'rejected)))
        (mevedel-test-memory-history-owned workspace
          (should-not (mevedel-memory-pass--pending-items workspace nil)))
        (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                   (lambda (&rest args) (cl-incf children) (apply start args))))
          (mevedel-memory-pass--apply-async state nil (lambda (value) (setq result value)))
          (mevedel-test-application-wait (lambda () result)))
        (should (= 1 children))
        (should (= 0 (plist-get result :applied-count)))
        (should (= 0 (plist-get result :unapplied-count)))
        (should-not (plist-get result :error)))))
  :doc "undecided extraction and its waiting note disposal remain selectable beside decided history"
  (mevedel-test-application-fixture
    (mevedel-test-application-decide workspace accepted proposal 'rejected)
    (let* ((memory (file-name-concat root "memory"))
           (mevedel-memory-dirs (list memory))
           (note (file-name-concat root ".mevedel" "shared" "dependency.md"))
           (address (mevedel-memory-notes-address "dependency.md")))
      (make-directory memory t)
      (write-region "A durable extraction must precede note disposal." nil note nil 'silent)
      (set-file-times note (seconds-to-time (- (float-time) 3600)))
      (let* ((capture (mevedel-memory-notes-capture workspace nil :mode 'auto :quiet-hours 0))
             (scope (mevedel-memory-scope-capture workspace))
             (scope (plist-put scope :roots (append (plist-get scope :roots) (list (plist-get capture :root)))))
             (memory-id (car (seq-find (lambda (row) (eq 'memory (plist-get (cdr row) :kind))) (plist-get scope :roots))))
             (claim (mevedel-journal-claim-acquire key 180)))
        (unwind-protect
            (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                   (text (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"lesson.md\"\ntype: \"project\"\n"
                                         "title: \"Lesson\"\nhook: \"Lesson\"\nreason: \"Supported\"\nevidence: [%S]\n---\n"
                                         "Preserve extraction dependencies.\n```\n## Update\n- none\n## Merge\n- none\n"
                                         "## Remove\n- none\n## Instructions\n- none\n## Notes\n```proposal\nnote: %S\n"
                                         "disposition: \"delete\"\nreason: \"Reviewed\"\n---\n```\n## No action\n- none")
                                 memory-id address address))
                   (pending (mevedel-memory-store-accept workspace prepared text nil "test:model" nil (list address)))
                   (items (plist-get pending :proposals))
                   (extraction (seq-find (lambda (item) (eq 'promote (plist-get item :action))) items))
                   (disposal (seq-find (lambda (item) (eq 'note-delete (plist-get item :action))) items)))
              (mevedel-memory-store-publish workspace (plist-get prepared :id))
              (should (equal (list (plist-get extraction :id)) (plist-get disposal :requires)))
              (should (eq 'waiting (plist-get (mevedel-memory-decision--requirements workspace disposal) :status)))
              (let ((selected (mevedel-memory-pass--pending-items workspace nil)))
                (should (= 2 (length selected)))
                (should (equal (sort (mapcar (lambda (item) (plist-get item :id)) items) #'string<)
                               (sort (mapcar (lambda (item) (plist-get item :proposal)) selected) #'string<)))))
          (mevedel-journal-claim-settle claim 'cancelled "")))))
  :doc "selection authenticates decided evidence rather than trusting public status to omit it"
  (mevedel-test-application-fixture
    (let* ((decision (mevedel-test-application-decide workspace accepted proposal 'rejected))
           (file (file-name-concat (mevedel-memory-decision--directory workspace) (concat (plist-get decision :decision-id) ".el"))))
      (write-region "\n" nil file t 'silent)
      (should (equal "Accepted memory decision changed"
                     (error-message-string (should-error (mevedel-memory-pass--pending-items workspace nil))))))))

(mevedel-deftest mevedel-memory-decision--start-operation ()
  ,test
  (test)
  :doc "cancellation fences a real suspended child and ignores duplicate late success"
  (mevedel-test-application-fixture
    (let (job ran (callbacks 0))
      (setq job (mevedel-memory-decision--start-operation
		 workspace (lambda (&rest _) (setq ran t))
		 (lambda (value) (cl-incf callbacks) (setq result value))))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (mevedel-memory-decision--operation-stop job)
      (mevedel-memory-decision--operation-ready job '(:ok t))
      (mevedel-memory-decision--operation-ready job '(:ok t))
      (should (= 1 callbacks))
      (should (plist-get result :cancelled))
      (should-not (plist-get result :error))
      (should-not ran)
      (should-not (process-live-p (plist-get job :worker)))
      (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))))

  :doc "fixed real deadline expires while child is suspended; takeover and late callback cannot commit"
  (mevedel-test-application-fixture
    (let ((acquire (symbol-function 'mevedel-journal-claim-acquire)) job successor ran (callbacks 0))
      (unwind-protect
	  (progn
	    (cl-letf (((symbol-function 'mevedel-journal-claim-acquire)
		       (lambda (directory _seconds) (funcall acquire directory 1))))
	      (setq job (mevedel-memory-decision--start-operation
			 workspace (lambda (&rest _) (setq ran t))
			 (lambda (value) (cl-incf callbacks) (setq result value)))))
	    (signal-process (plist-get job :worker) 'SIGSTOP)
	    (mevedel-test-application-wait (lambda () result))
	    (setq successor (funcall acquire key 180))
	    (should successor)
	    (should (eq 'expired (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))
	    (mevedel-memory-decision--operation-ready job '(:ok t))
	    (should (= 1 callbacks))
	    (should-not ran)
	    (should (plist-get result :error)))
        (when successor (mevedel-journal-claim-settle successor 'cancelled "")))))

  :doc "origin closure, settled ownership and explicit child failure cannot enter the editor operation"
  (dolist (failure '(origin claim child))
    (mevedel-test-application-fixture
      (let ((start (symbol-function 'mevedel-journal-worker-start))
            (origin (generate-new-buffer " *operation-origin*")) job ran injected (callbacks 0))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                         (lambda (ws operation callback &rest args)
                           (apply start ws operation
                                  (lambda (reply)
                                    (setq injected t)
                                    (pcase failure
                                      ('origin (kill-buffer origin))
                                      ('claim (should (mevedel-journal-claim-settle (plist-get job :claim) 'cancelled ""))))
                                    (funcall callback (if (eq failure 'child) '(:error "Injected child failure") reply))) args))))
                (with-current-buffer origin
                  (setq job (mevedel-memory-decision--start-operation
                             workspace (lambda (&rest _) (setq ran t))
                             (lambda (value) (cl-incf callbacks) (setq result value)))))
                (mevedel-test-application-wait (lambda () result)))
              (should injected)
              (should (= 1 callbacks))
              (should-not ran)
              (if (eq failure 'origin)
                  (progn (should (plist-get result :held)) (should-not (plist-get result :error)))
                (should (plist-get result :error)))
              (mevedel-memory-decision--operation-ready job '(:ok t))
              (should (= 1 callbacks)))
          (when (buffer-live-p origin) (kill-buffer origin))))))

  :doc "a reentrant duplicate cannot enter or finish before the outer editor function unwinds"
  (mevedel-test-application-fixture
    (let (job outer-done (entries 0) (callbacks 0))
      (setq job
            (mevedel-memory-decision--start-operation
             workspace
             (lambda (_claim _live reply)
               (when (= 1 (cl-incf entries))
                 (mevedel-memory-decision--operation-ready job reply)
                 (should (= 0 callbacks))
                 (should-not (plist-get job :done)))
               (setq outer-done t))
             (lambda (reply)
               (should outer-done)
               (cl-incf callbacks)
               (setq result reply))))
      (mevedel-test-application-wait (lambda () result))
      (should (= 1 entries))
      (should (= 1 callbacks))
      (should (plist-get result :ok))
      (should-not (plist-get result :cancelled))
      (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))))

  :doc "live origin rebind, workspace retarget and wrong-job reply refuse continuation"
  (dolist (failure '(session workspace root reply))
    (mevedel-test-application-fixture
      (let* ((start (symbol-function 'mevedel-journal-worker-start))
             (origin (generate-new-buffer " *operation-retarget*"))
             (session (mevedel-session--create :workspace workspace))
             (other (mevedel-workspace--create :root (file-name-concat root "unrelated")))
             ran injected callback-origin)
        (unwind-protect
            (progn
              (with-current-buffer origin (setq-local mevedel--session session))
              (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                         (lambda (ws operation callback &rest args)
                           (apply start ws operation
                                  (lambda (reply)
                                    (setq injected t)
                                    (pcase failure
                                      ('session (with-current-buffer origin
                                                  (setq-local mevedel--session (mevedel-session--create :workspace workspace))))
                                      ('workspace (setf (mevedel-session-workspace session) other))
                                      ('root (setf (mevedel-workspace-root workspace) (mevedel-workspace-root other)))
                                      ('reply (setq reply (plist-put reply :operation-id "another-operation"))))
                                    (funcall callback reply)) args))))
                (with-current-buffer origin
                  (mevedel-memory-decision--start-operation
                   workspace (lambda (&rest _) (setq ran t))
                   (lambda (reply) (setq callback-origin (current-buffer) result reply))))
                (mevedel-test-application-wait (lambda () result)))
              (should injected)
              (should-not ran)
              (if (eq failure 'reply)
                  (should (plist-get result :error))
                (should (plist-get result :held))
                (should-not (plist-get result :error)))
              (unless (eq failure 'reply) (should-not (eq callback-origin origin))))
          (setf (mevedel-workspace-root workspace) root)
          (kill-buffer origin))))))

(mevedel-deftest mevedel-memory-pass--application-live-p/origin-policy ()
  ,test
  (test)
  :doc "origin-local automatic mode applies while a timer runs in an unrelated propose buffer"
  (mevedel-test-application-fixture
    (let ((origin (generate-new-buffer " *auto-origin*"))
          (other (generate-new-buffer " *propose-incidental*"))
          (step (symbol-function 'mevedel-memory-pass--application-step)) observed)
      (unwind-protect
          (progn
            (plist-put state :origin-buffer origin)
            (with-current-buffer origin (setq-local mevedel-memory-consolidation-mode 'auto))
            (with-current-buffer other (setq-local mevedel-memory-consolidation-mode 'propose))
            (cl-letf (((symbol-function 'mevedel-memory-pass--application-step)
                       (lambda (&rest args)
                         (push (list (current-buffer) mevedel-memory-consolidation-mode) observed)
                         (apply step args))))
              (with-current-buffer origin
                (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value))))
              (with-current-buffer other (mevedel-test-application-wait (lambda () result))))
            (should (equal (car (last observed)) (list other 'propose)))
            (should (= 1 (plist-get result :applied-count)))
            (should-not (file-exists-p path)))
        (kill-buffer origin) (kill-buffer other))))
  :doc "origin-local propose mode holds before worker dispatch despite an unrelated automatic buffer"
  (mevedel-test-application-fixture
    (let ((origin (generate-new-buffer " *propose-origin*"))
          (other (generate-new-buffer " *auto-incidental*"))
          (start (symbol-function 'mevedel-journal-worker-start)) (children 0))
      (unwind-protect
          (progn
            (plist-put state :origin-buffer origin)
            (with-current-buffer origin (setq-local mevedel-memory-consolidation-mode 'propose))
            (with-current-buffer other (setq-local mevedel-memory-consolidation-mode 'auto))
            (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                       (lambda (&rest args) (cl-incf children) (apply start args))))
              (with-current-buffer origin
                (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value))))
              (with-current-buffer other (mevedel-test-application-wait (lambda () result))))
            (should (= 0 children))
            (should (= 1 (plist-get result :held-count)))
            (should (= 0 (plist-get result :applied-count)))
            (should (file-exists-p path)))
        (kill-buffer origin) (kill-buffer other))))
  :doc "a late origin policy change refuses the actual write even from another automatic buffer"
  (mevedel-test-application-fixture
    (let ((origin (generate-new-buffer " *late-policy-origin*"))
          (other (generate-new-buffer " *late-policy-incidental*"))
          (mkdir (symbol-function 'mevedel-session-control-fs-make-directory))
          (commit (symbol-function 'mevedel-memory-notes-commit)) injected)
      (unwind-protect
          (progn
            (plist-put state :origin-buffer origin)
            (with-current-buffer origin (setq-local mevedel-memory-consolidation-mode 'auto))
            (with-current-buffer other (setq-local mevedel-memory-consolidation-mode 'auto))
            (cl-letf (((symbol-function 'mevedel-session-control-fs-make-directory)
                       (lambda (directory &rest args)
                         (prog1 (apply mkdir directory args)
                           (when (and (not injected) (string-suffix-p "/blobs" (directory-file-name directory)))
                             (setq injected t)
                             (with-current-buffer origin (setq-local mevedel-memory-consolidation-mode 'propose))))))
                      ((symbol-function 'mevedel-memory-notes-commit)
                       (lambda (&rest args) (with-current-buffer other (apply commit args)))))
              (with-current-buffer origin
                (mevedel-memory-pass--apply-async state accepted (lambda (value) (setq result value))))
              (with-current-buffer other (mevedel-test-application-wait (lambda () result))))
            (should injected)
            (should (= 0 (plist-get result :applied-count)))
            (should (file-exists-p path))
            (should-not (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status))))
        (kill-buffer origin) (kill-buffer other)))))

(mevedel-deftest mevedel-memory-decision--operation-finish ()
  ,test
  (test)
  :doc "one settlement IO failure is reported while a bounded retry fences the child before deletion"
  (mevedel-test-application-fixture
    (let ((settle (symbol-function 'mevedel-journal-claim-settle))
          (delete (symbol-function 'delete-process)) job events (attempts 0) (callbacks 0))
      (setq job (mevedel-memory-decision--start-operation
                 workspace #'ignore (lambda (value) (push 'callback events) (cl-incf callbacks) (setq result value))))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (cl-letf (((symbol-function 'mevedel-journal-claim-settle)
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
        (mevedel-memory-decision--operation-stop job))
      (should (= 2 attempts))
      (should (= 1 callbacks))
      (should (equal '(fenced deleted callback) (nreverse events)))
      (should (string-match-p "Injected settlement IO failure" (plist-get result :error)))
      (should-not (plist-get result :unsettled))
      (should-not (gethash key mevedel-memory-decision--operations))
      (should-not (process-live-p (plist-get job :worker)))
      (mevedel-memory-decision--operation-finish job '(:ok t))
      (should (= 1 callbacks))))
  :doc "persistent settlement failure reports unresolved authority and retains the tracked child without deleting it"
  (mevedel-test-application-fixture
    (let ((acquire (symbol-function 'mevedel-journal-claim-acquire)) job deadline (attempts 0) (callbacks 0))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'mevedel-journal-claim-acquire)
                       (lambda (directory _seconds) (funcall acquire directory 2))))
              (setq job (mevedel-memory-decision--start-operation
                         workspace #'ignore (lambda (value) (cl-incf callbacks) (setq result value)))))
            (setq deadline (plist-get (plist-get job :claim) :expires-at))
            (signal-process (plist-get job :worker) 'SIGSTOP)
            (cl-letf (((symbol-function 'mevedel-journal-claim-settle)
                       (lambda (&rest _) (cl-incf attempts) (error "Persistent settlement IO failure"))))
              (mevedel-memory-decision--operation-stop job))
            (should (= 2 attempts))
            (should (= 1 callbacks))
            (should (plist-get result :unsettled))
            (should (string-match-p "Persistent settlement IO failure" (plist-get result :error)))
            (should-not (gethash key mevedel-memory-decision--operations))
            (should (mevedel-journal-claim-owned-p (plist-get job :claim)))
            (should (process-live-p (plist-get job :worker)))
            (should (memq (plist-get job :worker) mevedel-journal-worker--processes))
            (mevedel-test-application-wait (lambda () (not (process-live-p (plist-get job :worker)))))
            (should-not (plist-get job :unsettled))
            (should (= 1 callbacks))
            (should (= deadline (plist-get (plist-get job :claim) :expires-at)))
            (should (eq 'expired (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))
            (should-not (mevedel-journal-claim-owned-p (plist-get job :claim))))
        ;; The test's failed IO is over; explicitly establish the fence before
        ;; stopping its deliberately suspended child.
        (when job
          (mevedel-journal-claim-settle (plist-get job :claim) 'cancelled "")
          (when (process-live-p (plist-get job :worker)) (delete-process (plist-get job :worker)))))))
  :doc "a callback exception occurs only after settlement, process stop and registry cleanup"
  (mevedel-test-application-fixture
    (let ((callbacks 0) job)
      (setq job (mevedel-memory-decision--start-operation
                 workspace #'ignore (lambda (_) (cl-incf callbacks) (error "Injected callback failure"))))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (should (equal "Injected callback failure"
                     (error-message-string (should-error (mevedel-memory-decision--operation-stop job)))))
      (should (= 1 callbacks))
      (should-not (gethash key mevedel-memory-decision--operations))
      (should-not (process-live-p (plist-get job :worker)))
      (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))
      (mevedel-memory-decision--operation-finish job '(:ok t))
      (should (= 1 callbacks)))))

(mevedel-deftest mevedel-memory-decision--operation-finish/fence-proof ()
  ,test
  (test)
  :doc "a nil settlement without an outcome retains the child and original deadline, never treating return as proof"
  (mevedel-test-application-fixture
    (let (job (attempts 0))
      (unwind-protect
          (progn
            (setq job (mevedel-memory-decision--start-operation
                       workspace #'ignore (lambda (value) (setq result value))))
            (signal-process (plist-get job :worker) 'SIGSTOP)
            (cl-letf (((symbol-function 'mevedel-journal-claim-settle)
                       (lambda (&rest _) (cl-incf attempts) nil)))
              (mevedel-memory-decision--operation-stop job))
            (should (= 2 attempts))
            (should (plist-get result :unsettled))
            (should (string-match-p "No durable memory operation fence" (plist-get result :error)))
            (should-not (mevedel-journal-claim-outcome (plist-get job :claim)))
            (should (process-live-p (plist-get job :worker)))
            (should (memq (plist-get job :timer) timer-list)))
        (when job
          (mevedel-journal-claim-settle (plist-get job :claim) 'cancelled "")
          (mevedel-memory-decision--operation-retire job)
          (cancel-timer (plist-get job :timer))))))
  :doc "an authenticated competing outcome permits retirement after native settlement returns nil"
  (mevedel-test-application-fixture
    (let ((job (mevedel-memory-decision--start-operation
                workspace #'ignore (lambda (value) (setq result value)))))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (should (mevedel-journal-claim-settle (plist-get job :claim) 'failed "competing outcome"))
      (mevedel-memory-decision--operation-stop job)
      (should-not (plist-get result :unsettled))
      (should (plist-get result :cancelled))
      (should-not (plist-get result :error))
      (should-not (process-live-p (plist-get job :worker)))
      (should (eq 'failed (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))))
  :doc "the original deadline elects and authenticates expiry before deleting a suspended child"
  (mevedel-test-application-fixture
    (let ((acquire (symbol-function 'mevedel-journal-claim-acquire)) job (callbacks 0))
      (cl-letf (((symbol-function 'mevedel-journal-claim-acquire)
                 (lambda (directory _seconds) (funcall acquire directory 2))))
        (setq job (mevedel-memory-decision--start-operation
                   workspace #'ignore (lambda (value) (cl-incf callbacks) (setq result value)))))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (mevedel-test-application-wait (lambda () result))
      (should (= 1 callbacks))
      (should-not (plist-get result :unsettled))
      (should-not (process-live-p (plist-get job :worker)))
      (should (eq 'expired (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status))))))

(mevedel-deftest mevedel-memory-decision--recovered/fence-proof ()
  ,test
  (test)
  :doc "scheduled reply and stop retain a child without a durable release outcome"
  (dolist (phase '(reply stop))
    (mevedel-test-application-fixture
      (let (job diagnostics)
        (unwind-protect
            (progn
              (setq job (mevedel-memory-decision--start-recovery workspace #'ignore (current-buffer) #'always key))
              (signal-process (plist-get job :worker) 'SIGSTOP)
              (cl-letf (((symbol-function 'mevedel-journal-claim-settle) (lambda (&rest _) nil)))
                (mevedel-test--with-captured-diagnostics diagnostics
                                                         (if (eq phase 'reply)
                                                             (mevedel-memory-decision--recovered workspace job '(:ok t))
                                                           (mevedel-memory-decision-stop-recovery))))
              (should (string-match-p "No durable memory operation fence" diagnostics))
              (should-not (plist-get job :released))
              (should (plist-get job :unsettled))
              (should (process-live-p (plist-get job :worker)))
              (should (memq (plist-get job :timer) timer-list))
              (should-not (gethash key mevedel-memory-decision--recovery-workers)))
          (when job
            (mevedel-journal-claim-settle (plist-get job :claim) 'cancelled "")
            (mevedel-memory-decision--operation-retire job)
            (cancel-timer (plist-get job :timer)))))))
  :doc "scheduled original-deadline expiry leaves a real durable outcome before child deletion"
  (mevedel-test-application-fixture
    (let ((acquire (symbol-function 'mevedel-journal-claim-acquire)) job diagnostics)
      (cl-letf (((symbol-function 'mevedel-journal-claim-acquire)
                 (lambda (directory _seconds) (funcall acquire directory 2))))
        (setq job (mevedel-memory-decision--start-recovery workspace #'ignore (current-buffer) #'always key)))
      (signal-process (plist-get job :worker) 'SIGSTOP)
      (mevedel-test--with-captured-diagnostics diagnostics
                                               (mevedel-test-application-wait (lambda () (not (process-live-p (plist-get job :worker))))))
      (should (string-match-p "Memory recovery timed out" diagnostics))
      (should (eq 'expired (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))
      (should-not (plist-get job :unsettled)))))

(mevedel-deftest mevedel-memory-decision--operation-finish/quit ()
  ,test
  (test)
  :doc "quit during native settlement remains visible but cannot strand registry, child or final callback"
  (mevedel-test-application-fixture
    (let ((settle (symbol-function 'mevedel-journal-claim-settle)) job escaped (attempts 0) (callbacks 0))
      (unwind-protect
          (progn
            (setq job (mevedel-memory-decision--start-operation
                       workspace #'ignore (lambda (value) (cl-incf callbacks) (setq result value))))
            (signal-process (plist-get job :worker) 'SIGSTOP)
            (cl-letf (((symbol-function 'mevedel-journal-claim-settle)
                       (lambda (&rest args)
                         (if (= 1 (cl-incf attempts)) (signal 'quit nil) (apply settle args)))))
              (condition-case nil (mevedel-memory-decision--operation-stop job) (quit (setq escaped t))))
            (should-not escaped)
            (should (= 1 callbacks))
            (should (= 2 attempts))
            (should (string-match-p "Quit" (plist-get result :error)))
            (should-not (plist-get result :unsettled))
            (should-not (gethash key mevedel-memory-decision--operations))
            (should-not (process-live-p (plist-get job :worker)))
            (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status))))
        (when job
          (mevedel-memory-decision--operation-fence (plist-get job :claim))
          (when (process-live-p (plist-get job :worker)) (delete-process (plist-get job :worker)))
          (cancel-timer (plist-get job :timer))
          (remhash key mevedel-memory-decision--operations))))))

(mevedel-deftest mevedel-memory-decision--recovered/quit ()
  ,test
  (test)
  :doc "quit during scheduled release preserves unresolved tracking and original retirement timer"
  (dolist (phase '(reply stop))
    (mevedel-test-application-fixture
      (let (job diagnostics escaped)
        (unwind-protect
            (progn
              (setq job (mevedel-memory-decision--start-recovery workspace #'ignore (current-buffer) #'always key))
              (signal-process (plist-get job :worker) 'SIGSTOP)
              (cl-letf (((symbol-function 'mevedel-journal-claim-settle) (lambda (&rest _) (signal 'quit nil))))
                (mevedel-test--with-captured-diagnostics diagnostics
                  (condition-case nil
                      (if (eq phase 'reply)
                          (mevedel-memory-decision--recovered workspace job '(:ok t))
                        (mevedel-memory-decision-stop-recovery))
                    (quit (setq escaped t)))))
              (should-not escaped)
              (should (string-match-p "Quit" diagnostics))
              (should (plist-get job :unsettled))
              (should (process-live-p (plist-get job :worker)))
              (should (memq (plist-get job :timer) timer-list))
              (should-not (gethash key mevedel-memory-decision--recovery-workers)))
          (when job
            (mevedel-memory-decision--operation-fence (plist-get job :claim))
            (when (process-live-p (plist-get job :worker)) (delete-process (plist-get job :worker)))
            (cancel-timer (plist-get job :timer))
            (remhash key mevedel-memory-decision--recovery-workers)))))))

(mevedel-deftest mevedel-memory-decision--start-operation/quit ()
  ,test
  (test)
  :doc "quit at each registered setup seam fences the exact claim and delivers one visible cancellation"
  (dolist (kind '(application recovery))
    (dolist (seam '(clock request launch))
      (mevedel-test-application-fixture
        (let ((clock (symbol-function 'mevedel-session-control-fs-target-time))
              (write (symbol-function 'mevedel-journal-worker--write))
              (launch (symbol-function 'make-process))
              (delete (symbol-function 'delete-process))
              (table (if (eq kind 'application) mevedel-memory-decision--operations
                       mevedel-memory-decision--recovery-workers))
              job child request diagnostics escaped injected (callbacks 0))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time)
                           (lambda (directory)
                             (when (and (eq seam 'clock) (not injected) (gethash key table))
                               (setq injected t job (gethash key table))
                               (signal 'quit nil))
                             (funcall clock directory)))
                          ((symbol-function 'mevedel-journal-worker--write)
                           (lambda (file value)
                             (when (gethash key table)
                               (setq job (gethash key table) request file)
                               (when (eq seam 'request) (setq injected t) (signal 'quit nil)))
                             (funcall write file value)))
                          ((symbol-function 'make-process)
                           (lambda (&rest args)
                             (let ((process (apply launch args)))
                               (when (and (eq seam 'launch) (equal "mevedel-journal-worker" (plist-get args :name)))
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
                               (should (mevedel-journal-claim-outcome (plist-get job :claim))))
                             (funcall delete process))))
                  (mevedel-test--with-captured-diagnostics diagnostics
                    (condition-case nil
                        (if (eq kind 'application)
                            (mevedel-memory-decision--start-operation
                             workspace #'ignore (lambda (value) (cl-incf callbacks) (setq result value)))
                          (mevedel-memory-decision--start-recovery
                           workspace (lambda () (cl-incf callbacks)) (current-buffer) #'always key))
                      (quit (setq escaped t quit-flag nil)))))
                (should injected)
                (should-not escaped)
                (should-not (gethash key table))
                (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))
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
              (mevedel-memory-decision--operation-fence (plist-get job :claim))
              (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
            (when (and child (process-live-p child)) (delete-process child))
            (when (and request (file-directory-p (file-name-directory request)))
              (delete-directory (file-name-directory request) t))
            (remhash key table)))))))

(mevedel-deftest mevedel-journal-worker-start/immediate-exit ()
  ,test
  (test)
  :doc "an immediately exiting native child has an owner before its sentinel fences and cleans up"
  (mevedel-test-application-fixture
    (let ((launch (symbol-function 'make-process))
          (write (symbol-function 'mevedel-journal-worker--write))
          request job child (callbacks 0))
      (cl-letf (((symbol-function 'mevedel-journal-worker--write)
                 (lambda (file value) (setq request file) (funcall write file value)))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (when (equal "mevedel-journal-worker" (plist-get args :name))
                     (setq args (plist-put args :command
                                           (list (expand-file-name invocation-name invocation-directory)
                                                 "--batch" "-Q" "--eval" "(kill-emacs 0)"))))
                   (setq child (apply launch args)))))
        (setq job (mevedel-memory-decision--start-operation
                   workspace #'ignore (lambda (value) (cl-incf callbacks) (setq result value))))
        (should (eq child (plist-get job :worker)))
        (mevedel-test-application-wait (lambda () result)))
      (should (= 1 callbacks))
      (should (string-match-p "Journal worker exited with status 0" (plist-get result :error)))
      (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))
      (should-not (gethash key mevedel-memory-decision--operations))
      (should-not (memq child mevedel-journal-worker--processes))
      (should-not (process-live-p child))
      (should-not (file-directory-p (file-name-directory request))))))

(mevedel-deftest mevedel-journal-worker-start/delivered-quit ()
  ,test
  (test)
  :doc "pending quit is delivered inside owned setup rather than at a later caller loop"
  (mevedel-test-application-fixture
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
                    (setq job (mevedel-memory-decision--start-operation
                               workspace #'ignore (lambda (reply) (setq result reply))))
                    ;; A pending flag must not be left for a later native check
                    ;; in an unprotected caller or event-loop continuation.
                    (while quit-flag (ignore)))
                (quit (setq escaped t quit-flag nil))))
            (should-not escaped)
            (should (plist-get result :cancelled))
            (should-not (gethash key mevedel-memory-decision--operations))
            (should-not (process-live-p child))
            (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status))))
        (setq quit-flag nil)
        (when-let* ((active (gethash key mevedel-memory-decision--operations)))
          (mevedel-memory-decision--operation-stop active))
        (when (and child (process-live-p child)) (delete-process child))))))

(mevedel-deftest mevedel-journal-worker-start/inhibited-quit ()
  ,test
  (test)
  :doc "an inherited quit inhibition retains the outer pending flag without stealing cancellation"
  (mevedel-test-application-fixture
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
                (setq job (mevedel-memory-decision--start-operation
                           workspace #'ignore (lambda (reply) (setq result reply)))))
              (setq pending quit-flag)
              ;; The outer owner consumes its flag, not worker startup.
              (setq quit-flag nil))
            (should pending)
            (should-not result)
            (should (eq child (plist-get job :worker)))
            (should (memq child mevedel-journal-worker--processes))
            (should (mevedel-journal-claim-owned-p (plist-get job :claim)))
            (mevedel-memory-decision--operation-stop job)
            (should-not (process-live-p child))
            (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status))))
        (setq quit-flag nil)
        (when job (mevedel-memory-decision--operation-stop job))))))

(mevedel-deftest mevedel-memory-pass--application-step/quit ()
  ,test
  (test)
  :doc "quit after popping an item completes visibly without dispatching subsequent work"
  (mevedel-test-application-fixture
    (let ((start (symbol-function 'mevedel-memory-decision--start-operation))
          (callbacks 0) (dispatches 0) injected)
      (cl-letf (((symbol-function 'mevedel-memory-decision--start-operation)
                 (lambda (&rest args)
                   (cl-incf dispatches)
                   (if injected (apply start args) (setq injected t) (signal 'quit nil)))))
        (mevedel-memory-pass--apply-async state accepted (lambda (value) (cl-incf callbacks) (setq result value)))
        (mevedel-test-application-wait (lambda () result)))
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
      (should-not (gethash key mevedel-memory-decision--operations)))))

(mevedel-deftest mevedel-memory-pass--apply-async/quit-batch ()
  ,test
  (test)
  :doc "native setup or cleanup quit stops a two-proposal batch instead of starting the next child"
  (dolist (seam '(request settle))
    (let ((mevedel-test-application-items 2))
      (mevedel-test-application-fixture
        (let* ((write (symbol-function 'mevedel-journal-worker--write))
               (settle (symbol-function 'mevedel-journal-claim-settle))
               (start (symbol-function 'mevedel-journal-worker-start))
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
          (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                     (lambda (&rest args) (cl-incf children) (apply start args)))
                    ((symbol-function 'mevedel-journal-worker--write)
                     (lambda (&rest args)
                       (when (and (eq seam 'request) (not injected))
                         (setq injected t operation (gethash key mevedel-memory-decision--operations))
                         (signal 'quit nil))
                       (apply write args)))
                    ((symbol-function 'mevedel-journal-claim-settle)
                     (lambda (claim &rest args)
                       (let ((job (gethash key mevedel-memory-decision--operations)))
                         (when (and (eq seam 'settle) (not injected) job
                                    (plist-get job :done) (eq claim (plist-get job :claim)))
                           (setq injected t operation job) (signal 'quit nil)))
                       (apply settle claim args))))
            (mevedel-memory-pass--finish state '(:outcome success))
            (mevedel-test-application-wait (lambda () result)))
          (should injected)
          (should (= 1 children))
          (should (= 1 callbacks))
          (should (plist-get state :stop-apply))
          (should (plist-get result :cancelled))
          (if (eq seam 'request)
              (progn
                (should-not (plist-get result :error))
                (should (= 2 (plist-get result :held-count)))
                (should (equal (make-string 100 ?x) (mevedel-session-control-fs-read-file path)))
                (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id))))
            (should (string-match-p "Quit" (plist-get result :error)))
            (should (= 1 (plist-get result :held-count)))
            (should-not (file-exists-p path))
            (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
            (should-not (plist-get (car (mevedel-memory-write-list workspace)) :marked)))
          (should (= 0 (plist-get result :unapplied-count)))
          (should (= (if (eq seam 'request) 0 1) (plist-get result :applied-count)))
          (should (= (if (eq seam 'request) 0 1) (plist-get result :updated-files)))
          (should (file-exists-p other-path))
          (should-not (mevedel-memory-decision-status workspace (plist-get other :id)))
          (should operation)
          (should (mevedel-journal-claim-outcome (plist-get operation :claim)))
          (should-not (process-live-p (plist-get operation :worker)))
          (should-not (gethash key mevedel-memory-decision--operations))
          (should-not (gethash key mevedel-memory-pass--running))
          (should-not (gethash workspace mevedel-memory-pass--deferred))
          (mevedel-memory-decision--operation-ready operation '(:ok t))
          (should (= 1 callbacks)))))))

(mevedel-deftest mevedel-memory-pass--apply-async/cleanup-failure ()
  ,test
  (test)
  :doc "a post-application settlement failure is visible in the final caller outcome"
  (mevedel-test-application-fixture
    (let ((settle (symbol-function 'mevedel-journal-claim-settle)) operation injected (callbacks 0))
      (plist-put state :prepared (plist-get accepted :prepared))
      (plist-put state :settled nil)
      (plist-put state :accepted accepted)
      (plist-put state :entry (plist-get accepted :review))
      (plist-put state :publication-finished t)
      (plist-put state :telemetry '(:mode auto))
      (plist-put state :callback (lambda (value) (cl-incf callbacks) (setq result value)))
      (cl-letf (((symbol-function 'mevedel-journal-claim-settle)
                 (lambda (claim &rest args)
                   (let ((job (gethash key mevedel-memory-decision--operations)))
                     (when (and (not injected) job (plist-get job :done) (eq claim (plist-get job :claim)))
                       (setq injected t operation job)
                       (error "Visible settlement IO failure")))
                   (apply settle claim args))))
        (mevedel-memory-pass--finish state '(:outcome success))
        (mevedel-test-application-wait (lambda () result)))
      (should injected)
      (should (= 1 (plist-get result :applied-count)))
      (should (= 1 (plist-get result :updated-files)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should-not (plist-get result :cancelled))
      (should (string-match-p "Visible settlement IO failure" (plist-get result :error)))
      (should-not (file-exists-p path))
      (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
      (should-not (plist-get (car (mevedel-memory-write-list workspace)) :marked))
      (should (eq 'completed (plist-get (mevedel-journal-claim-outcome (plist-get operation :claim)) :status)))
      (should-not (gethash key mevedel-memory-decision--operations))
      (should-not (gethash key mevedel-memory-pass--running))
      (should-not (gethash workspace mevedel-memory-pass--deferred))
      (mevedel-memory-decision--operation-ready operation '(:ok t))
      (should (= 1 callbacks)))))

(mevedel-deftest mevedel-memory-decision--worker-local-p ()
  ,test
  (test)
  :doc "authenticated historical remote authority overrides a local current-config dispatch gate"
  (mevedel-test-application-fixture
    (let* ((bundle (mevedel-test-application-remote-history workspace))
           (scope (plist-get (plist-get bundle :prepared) :scope))
           (truename (symbol-function 'file-truename)) touched)
      (should (mevedel-memory-pass--async-application-p workspace accepted))
      (cl-letf (((symbol-function 'file-truename)
                 (lambda (file)
                   (if (file-remote-p file)
                       (progn (setq touched t) file)
                     (funcall truename file)))))
        ;; Changing current configuration is deliberately not revocation of
        ;; this frozen root; the real authority check reaches its original path.
        (should (mevedel-memory-scope--root scope "remote")))
      (should touched)
      (let ((file-name-handler-alist
             (cons (cons "memory-fixture[.]invalid"
                         (lambda (operation &rest args)
                           (if (eq operation 'file-remote-p)
                               (let ((file-name-handler-alist (cdr file-name-handler-alist)))
                                 (apply #'file-remote-p args))
                             (ert-fail (format "Remote IO: %S" operation)))))
                   file-name-handler-alist)))
        (should-not (mevedel-memory-decision--worker-local-p workspace))))))

(mevedel-deftest mevedel-memory-pass--apply-async/remote-history ()
  ,test
  (test)
  :doc "real child classifies remote history before recovery; native fallback freshly selects and applies pending work"
  (mevedel-test-application-fixture
    (mevedel-test-application-remote-history workspace)
    (let ((start (symbol-function 'mevedel-journal-worker-start))
          (recover (symbol-function 'mevedel-memory-decision-recover))
          (store (symbol-function 'mevedel-memory-store-recover))
          (children 0) (recoveries 0) (stores 0) replies recovered)
      (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                 (lambda (ws operation callback &rest args)
                   (cl-incf children)
                   (apply start ws operation
                          (lambda (reply)
                            (push reply replies)
                            (funcall callback reply)) args)))
                ((symbol-function 'mevedel-memory-decision-recover)
                 (lambda (&rest args) (cl-incf recoveries) (apply recover args)))
                ((symbol-function 'mevedel-memory-store-recover)
                 (lambda (&rest args) (cl-incf stores) (apply store args))))
        (mevedel-memory-pass--apply-async state nil (lambda (value) (setq result value)))
        (mevedel-test-application-wait (lambda () result))
        (should (= 2 children recoveries stores))
        ;; The existing scheduled-recovery callback consumes the same worker
        ;; reply, so it too must perform native recovery rather than skip it.
        (mevedel-memory-decision--start-recovery workspace (lambda () (setq recovered t)) (current-buffer) #'always key)
        (mevedel-test-application-wait (lambda () recovered)))
      ;; The now-unmarked completed write needs no second owned recovery child.
      (should (= 3 children recoveries))
      (should (= 3 stores))
      (should (cl-every (lambda (reply) (and (plist-get reply :synchronous) (not (plist-get reply :accepted)))) replies))
      (should (= 1 (plist-get result :applied-count)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should-not (file-exists-p path)))))

(mevedel-deftest mevedel-memory-decision--recovered/remote-history ()
  ,test
  (test)
  :doc "scheduled historical-remote fallback recovers under its exact claim before fencing and marked-write inspection"
  (mevedel-test-application-fixture
    (mevedel-test-application-remote-history workspace)
    (let* ((scope (plist-get (plist-get accepted :prepared) :scope))
           (claim (mevedel-journal-claim-acquire key 180))
           (intent (unwind-protect
                       (mevedel-memory-write-call
                        scope (plist-get proposal :root)
                        (lambda (target) (mevedel-memory-write-prepare workspace claim target accepted proposal)))
                     (mevedel-journal-claim-settle claim 'cancelled "")))
           (recover (symbol-function 'mevedel-memory-decision-recover))
           (store (symbol-function 'mevedel-memory-store-recover))
           (fence (symbol-function 'mevedel-memory-decision--operation-fence))
           (inspect (symbol-function 'mevedel-memory-decision--recover-writes))
           job owner events fence-observation inspection-observation (inspections 0) (callbacks 0))
      (should (plist-get (car (mevedel-memory-write-list workspace)) :marked))
      (cl-labels ((publication ()
                    (when (and job (not (plist-get job :released)))
                      (should (equal owner (plist-get (plist-get job :claim) :owner)))
                      (should (mevedel-journal-claim-owned-p (plist-get job :claim)))
                      t)))
        (cl-letf (((symbol-function 'mevedel-memory-decision-recover)
                   (lambda (&rest args) (when (publication) (push 'decisions events)) (apply recover args)))
                  ((symbol-function 'mevedel-memory-store-recover)
                   (lambda (&rest args) (when (publication) (push 'store events)) (apply store args)))
                  ((symbol-function 'mevedel-memory-decision--operation-fence)
                   (lambda (token)
                     (when (and job (eq token (plist-get job :claim)))
                       (setq fence-observation (reverse events)))
                     (prog1 (funcall fence token)
                       (when (and job (eq token (plist-get job :claim))) (push 'fenced events)))))
                  ((symbol-function 'mevedel-memory-decision--recover-writes)
                   (lambda (actual ids &optional operation-claim)
                     (cl-incf inspections)
                     (should (eq workspace actual))
                     (should (equal (list (plist-get intent :id)) ids))
                     (setq inspection-observation (reverse events))
                     (should (mevedel-journal-claim-outcome (plist-get job :claim)))
                     (should-not (equal owner (plist-get operation-claim :owner)))
                     (apply inspect (list actual ids operation-claim)))))
          (let ((file-name-handler-alist
                 (cons (cons "memory-fixture[.]invalid"
                             (lambda (operation &rest args)
                               (if (eq operation 'file-remote-p)
                                   (let ((file-name-handler-alist (cdr file-name-handler-alist)))
                                     (apply #'file-remote-p args))
                                 (ert-fail (format "Remote IO: %S" operation)))))
                       file-name-handler-alist)))
            (setq job (mevedel-memory-decision--start-recovery
                       workspace (lambda () (cl-incf callbacks)) (current-buffer) #'always key)
                  owner (plist-get (plist-get job :claim) :owner))
            (mevedel-test-application-wait (lambda () (= 1 callbacks))))))
      (should (= 1 inspections callbacks))
      (should (equal '(decisions store) fence-observation))
      (should (equal '(decisions store fenced) inspection-observation))
      (should (equal '(decisions store fenced) (reverse events)))
      (should (file-exists-p path))
      (should-not (plist-get (car (mevedel-memory-write-list workspace)) :marked))
      (should (eq 'unavailable (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
      (should-not (gethash key mevedel-memory-decision--recovery-workers)))))

(mevedel-deftest mevedel-memory-decision--worker-local-p/uncertain-expiry ()
  ,test
  (test)
  :doc "uncertain acceptance still reaches checked native recovery of interrupted accepted expiry"
  (mevedel-test-application-fixture
    (let* ((journal (mevedel-journal-store-directory root))
           (digest (mevedel-journal-store-publish-digest
                    root (list :capture-id (make-string 64 ?e) :session "closed" :session-name "Closed"
                               :workspace _identity :trigger 'session-end :segment 1
                               :source-revision (make-string 64 ?b) :turns '(1) :turn-ids (list (make-string 64 ?c))
                               :created "2020-01-01T00:00:00Z" :model "test:model")
                    "## Done\n- none\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))
           (mutation (mevedel-journal-claim-acquire (mevedel-journal-store-claim-directory journal 'mutation) 120))
           (manifest (file-name-concat (mevedel-journal-store-state-directory journal) "expiry"
                                       (concat (plist-get mutation :owner) ".json")))
           (text (json-serialize
                  (append (mevedel-journal-claim--record mutation)
                          (list :entries (vector (list :file (plist-get digest :file) :kind "digest"
                                                       :id (plist-get digest :capture-id) :private []
                                                       :sha256 (secure-hash 'sha256 (plist-get digest :text))))))))
           (file (file-name-concat (mevedel-memory-store--directory workspace (plist-get (plist-get accepted :prepared) :id)) "accepted.el"))
           (original (mevedel-session-control-fs-read-file file))
           (start (symbol-function 'mevedel-journal-worker-start)) synchronous ran)
      (unwind-protect
          (progn
            (make-directory (file-name-directory manifest) t)
            (write-region text nil manifest nil 'silent)
            (should (mevedel-journal-claim-settle mutation 'completed (secure-hash 'sha256 text)))
            (delete-file file)
            (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                       (lambda (ws operation callback &rest args)
                         (apply start ws operation
                                (lambda (reply)
                                  (setq synchronous (plist-get reply :synchronous))
                                  (funcall callback reply)) args))))
              (mevedel-memory-decision--start-operation
               workspace (lambda (&rest _) (setq ran t)) (lambda (reply) (setq result reply)))
              (mevedel-test-application-wait (lambda () result)))
            (should synchronous)
            (should-not ran)
            (should (plist-get result :error))
            (should-not (file-exists-p (file-name-concat journal (plist-get digest :file))))
            (should-not (file-exists-p file)))
        (write-region original nil file nil 'silent)))))

(mevedel-deftest mevedel-memory-decision--start-operation/context-error ()
  ,test
  (test)
  :doc "a real malformed context identity cannot strand an acquired workspace claim"
  (mevedel-test-application-fixture
    (let* ((file (file-name-concat root ".mevedel" "workspace-id"))
           (text (mevedel-session-control-fs-read-file file))
           (mevedel-workspace-identity--cache (make-hash-table :test #'equal))
           successor)
      (unwind-protect
          (progn
            (write-region "malformed\n" nil file nil 'silent)
            (should (string-match-p "Invalid workspace identity"
                                    (error-message-string
                                     (should-error (mevedel-memory-decision--start-operation workspace #'ignore #'ignore)))))
            (should-not (gethash key mevedel-memory-decision--operations))
            (setq successor (mevedel-journal-claim-acquire key 180))
            (should successor))
        (when successor (mevedel-journal-claim-settle successor 'cancelled ""))
        (write-region text nil file nil 'silent)))))

(mevedel-deftest mevedel-memory-decision--operation-stop/rollback ()
  ,test
  (test)
  :doc "stopping after a first real mutation retains workspace ownership for checked rollback"
  (mevedel-test-application-fixture
    (let* ((target (mevedel-journal-claim-acquire (file-name-concat root "rollback-claims") 180))
           (mutate (symbol-function 'mevedel-memory-write--mutate))
           before changes job stopped outer-done (callbacks 0))
      (unwind-protect
          (progn
            (dolist (name '("first.md" "second.md"))
              (let ((file (file-name-concat root name)))
                (write-region "old" nil file nil 'silent)
                (push (cons name (mevedel-tool-patch--snapshot file)) before)
                (push (list :path file :action 'write :before (cdar before) :bytes "new" :content "new") changes)))
            (let ((intent (list :item '(:root "memory") :before before
                                :after (mevedel-memory-write--after before changes)
                                :accepted '(:prepared (:scope (:roots (("memory" :kind memory))))))))
              (cl-letf (((symbol-function 'mevedel-memory-write--mutate)
                         (lambda (&rest args)
                           (prog1 (apply mutate args)
                             (unless stopped
                               (setq stopped t)
                               (mevedel-memory-decision--operation-stop job))))))
                (setq job
                      (mevedel-memory-decision--start-operation
                       workspace
                       (lambda (claim live _reply)
                         (should-error
                          (mevedel-memory-write--commit
                           workspace target intent changes
                           (lambda ()
                             (mevedel-memory-store--assert-owned claim)
                             (mevedel-memory-store--assert-owned target) t)
                           (lambda () (unless (funcall live) (error "Forward stopped")))))
                         (should (= 0 callbacks))
                         (should (mevedel-journal-claim-owned-p claim))
                         (dolist (row before)
                           (should (equal "old" (mevedel-session-control-fs-read-file (plist-get (cdr row) :path)))))
                         (setq outer-done t))
                       (lambda (reply)
                         (cl-incf callbacks)
                         (setq result reply))))
                (mevedel-test-application-wait (lambda () result))))
            (should stopped)
            (ert-info ((prin1-to-string result)) (should outer-done))
            (should (= 1 callbacks))
            (should (plist-get result :ok)))
        (mevedel-journal-claim-settle target 'cancelled "")))))

(mevedel-deftest mevedel-memory-decision--operation-stop/marked-write ()
  ,test
  (test)
  :doc "cancelling a marked two-path proposal rolls back and resolves its retained intent"
  (mevedel-test-application-fixture
    (let* ((memory (file-name-concat root ".mevedel" "memory"))
           (mevedel-memory-dirs (list memory))
           (_directory (make-directory memory t))
           (scope (mevedel-memory-scope-capture workspace))
           (root-id (car (seq-find (lambda (row) (eq 'memory (plist-get (cdr row) :kind))) (plist-get scope :roots))))
           (claim (mevedel-journal-claim-acquire key 180))
           (prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
           (bundle (mevedel-memory-store-accept
                    workspace prepared
                    (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"new.md\"\ntype: \"project\"\n"
                                    "title: \"New\"\nhook: \"Guidance\"\nreason: \"Retain\"\nevidence: []\n---\nNew guidance.\n```\n"
                                    "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none") root-id)
                    nil "test:model" nil))
           (mutate (symbol-function 'mevedel-memory-write--mutate)) job stopped)
      (mevedel-memory-store-publish workspace (plist-get prepared :id))
      (mevedel-journal-claim-settle claim 'cancelled "")
      (cl-letf (((symbol-function 'mevedel-memory-write--mutate)
                 (lambda (&rest args)
                   (prog1 (apply mutate args)
                     (unless stopped
                       (setq stopped t)
                       (should (cl-some (lambda (row) (plist-get row :marked)) (mevedel-memory-write-list workspace)))
                       (mevedel-memory-decision--operation-stop job))))))
        (setq job
              (mevedel-memory-decision--start-operation
               workspace
               (lambda (owner live _reply)
                 (mevedel-memory-decision--apply-owned
                  workspace owner (plist-get prepared :id) (plist-get (car (plist-get bundle :proposals)) :id) nil live))
               (lambda (reply) (setq result reply))))
        (mevedel-test-application-wait (lambda () result)))
      (should stopped)
      (ert-info ((prin1-to-string result))
                (should (plist-get result :held))
                (should (plist-get result :cancelled))
                (should-not (plist-get result :value))
                (should-not (plist-get result :error)))
      (should-not (mevedel-memory-decision-status workspace (plist-get (car (plist-get bundle :proposals)) :id)))
      (should-not (file-exists-p (file-name-concat memory "new.md")))
      (should-not (file-exists-p (file-name-concat memory "MEMORY.md")))
      (let ((rows (mevedel-memory-write-list workspace)))
        (should (= 1 (length rows)))
        (should-not (plist-get (car rows) :marked))
        (should (eq 'before (mevedel-memory-write-state (plist-get (car rows) :intent))))))))

(mevedel-deftest mevedel-memory-pass--apply-async/quit-rollback ()
  ,test
  (test)
  :doc "C-g rolls back under original claims or accounts a failed rollback without hiding cancellation"
  (dolist (intervene '(nil t))
    (mevedel-test-application-fixture
    (let* ((memory (file-name-concat root ".mevedel" "memory"))
           (mevedel-memory-dirs (list memory))
           (_directory (make-directory memory t))
           (scope (mevedel-memory-scope-capture workspace))
           (root-id (car (seq-find (lambda (row) (eq 'memory (plist-get (cdr row) :kind))) (plist-get scope :roots))))
           (claim (mevedel-journal-claim-acquire key 180))
           (prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
           (bundle (mevedel-memory-store-accept
                    workspace prepared
                    (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"new.md\"\ntype: \"project\"\n"
                                    "title: \"New\"\nhook: \"Guidance\"\nreason: \"Retain\"\nevidence: []\n---\nNew guidance.\n```\n"
                                    "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none") root-id)
                    nil "test:model" nil))
           (mutate (symbol-function 'mevedel-memory-write--mutate)) interrupted intervened-path (callbacks 0))
      (mevedel-memory-store-publish workspace (plist-get prepared :id))
      (mevedel-journal-claim-settle claim 'cancelled "")
      (cl-letf (((symbol-function 'mevedel-memory-write--mutate)
                 (lambda (&rest args)
                   (prog1 (apply mutate args)
                     (unless interrupted
                       (setq interrupted t)
                       (should (cl-some (lambda (row) (plist-get row :marked)) (mevedel-memory-write-list workspace)))
                       (when intervene
                         (setq intervened-path (plist-get (nth 1 args) :path))
                         (write-region "Intervening external edit" nil intervened-path nil 'silent))
                       (signal 'quit nil))))))
        (mevedel-memory-pass--apply-async state bundle (lambda (reply) (cl-incf callbacks) (setq result reply)))
        (mevedel-test-application-wait (lambda () result)))
      (should interrupted)
      (should (= 1 callbacks))
      (should (plist-get state :stop-apply))
      (should (plist-get result :cancelled))
      (should (= 0 (plist-get result :applied-count)))
      (should (file-exists-p path))
      (should-not (gethash key mevedel-memory-decision--operations))
      (let ((rows (mevedel-memory-write-list workspace)))
        (should (= 1 (length rows)))
        (if intervene
            (progn
              (should (string-match-p "checked recovery" (plist-get result :error)))
              (should (= 1 (plist-get result :unapplied-count)))
              (should (eq 'recovery-required
                          (plist-get (mevedel-memory-decision-status workspace (plist-get (car (plist-get bundle :proposals)) :id)) :status)))
              (should (plist-get (car rows) :marked))
              (should (eq 'changed (mevedel-memory-write-state (plist-get (car rows) :intent))))
              (should (equal "Intervening external edit" (mevedel-session-control-fs-read-file intervened-path))))
          (should-not (plist-get result :error))
          (should (= 1 (plist-get result :held-count)))
          (should (= 0 (plist-get result :unapplied-count)))
          (should-not (mevedel-memory-decision-status workspace (plist-get (car (plist-get bundle :proposals)) :id)))
          (should-not (file-exists-p (file-name-concat memory "new.md")))
          (should-not (file-exists-p (file-name-concat memory "MEMORY.md")))
          (should-not (plist-get (car rows) :marked))
          (should (eq 'before (mevedel-memory-write-state (plist-get (car rows) :intent))))))))))

(mevedel-deftest mevedel-memory-write--commit/forward-gate ()
  ,test
  (test)
  :doc "a stopped second forward write still allows checked rollback of the first"
  (let* ((root (make-temp-file "mevedel-forward-rollback-" t))
	 (workspace (mevedel-workspace--create :root root))
	 (target (mevedel-journal-claim-acquire (file-name-concat root "claims") 180))
	 before changes (guards 0))
    (unwind-protect
	(progn
	  (dolist (name '("first.md" "second.md"))
	    (let ((path (file-name-concat root name)))
	      (write-region "old" nil path nil 'silent)
	      (push (cons name (mevedel-tool-patch--snapshot path)) before)
	      (push (list :path path :action 'write :before (cdar before) :bytes "new" :content "new") changes)))
	  (let ((intent (list :item '(:root "memory") :before before
			      :after (mevedel-memory-write--after before changes)
			      :accepted '(:prepared (:scope (:roots (("memory" :kind memory))))))))
	    (should-error
	     (mevedel-memory-write--commit
	      workspace target intent changes
	      (lambda () (mevedel-memory-store--assert-owned target) t)
	      (lambda () (when (= 2 (cl-incf guards)) (error "Forward application stopped"))))))
	  (should (= 2 guards))
	  (dolist (row before)
	    (should (equal "old" (mevedel-session-control-fs-read-file (plist-get (cdr row) :path))))))
      (mevedel-journal-claim-settle target 'cancelled "")
      (delete-directory root t))))

(mevedel-deftest mevedel-memory-pass--apply-async/settled-cancellation ()
  ,test
  (test)
  :doc "actual completed write is counted before cancellation holds the second proposal"
  (let ((mevedel-test-application-items 2))
    (mevedel-test-application-fixture
      (let* ((apply-owned (symbol-function 'mevedel-memory-decision--apply-owned))
             (other (cadr (plist-get accepted :proposals)))
             (other-path (plist-get (cdr (car (plist-get other :before))) :path))
             stopped (callbacks 0))
        (cl-letf (((symbol-function 'mevedel-memory-decision--apply-owned)
                   (lambda (&rest args)
                     (prog1 (apply apply-owned args)
                       (unless stopped
                         (setq stopped t)
                         (mevedel-memory-decision--operation-stop
                          (gethash key mevedel-memory-decision--operations)))))))
          (mevedel-memory-pass--apply-async state accepted (lambda (reply) (cl-incf callbacks) (setq result reply)))
          (mevedel-test-application-wait (lambda () result)))
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
        (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
        (should-not (mevedel-memory-decision-status workspace (plist-get other :id)))
        (should-not (gethash key mevedel-memory-decision--operations)))))

  :doc "C-g after actual note mutation retains applied result and stops the remaining proposal"
  (let ((mevedel-test-application-items 2))
    (mevedel-test-application-fixture
      (let* ((run (symbol-function 'mevedel-memory-write-run))
             (other (cadr (plist-get accepted :proposals)))
             (other-path (plist-get (cdr (car (plist-get other :before))) :path))
             interrupted (callbacks 0))
        (cl-letf (((symbol-function 'mevedel-memory-write-run)
                   (lambda (&rest args)
                     (prog1 (apply run args)
                       (unless interrupted (setq interrupted t) (signal 'quit nil))))))
          (mevedel-memory-pass--apply-async state accepted (lambda (reply) (cl-incf callbacks) (setq result reply)))
          (mevedel-test-application-wait (lambda () result)))
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
        (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
        (should-not (mevedel-memory-decision-status workspace (plist-get other :id)))
        (should-not (plist-get (car (mevedel-memory-write-list workspace)) :marked))
        (should-not (gethash key mevedel-memory-decision--operations)))))

  :doc "actual authenticated result survives cancellation and genuine final cleanup failure"
  (dolist (cleanup '(nil failure))
    (mevedel-test-application-fixture
      (let ((settle (symbol-function 'mevedel-journal-claim-settle)) job actual injected (callbacks 0))
        (cl-letf (((symbol-function 'mevedel-journal-claim-settle)
                   (lambda (token &rest args)
                     (when (and cleanup job (not injected) (plist-get job :done)
                                (eq token (plist-get job :claim)))
                       (setq injected t)
                       (error "Injected final result cleanup failure"))
                     (apply settle token args))))
          (setq job
                (mevedel-memory-decision--start-operation
                 workspace
                 (lambda (claim live _reply)
                   (setq actual (mevedel-memory-decision--apply-owned
                                 workspace claim (plist-get (plist-get accepted :prepared) :id)
                                 (plist-get proposal :id) nil live))
                   (mevedel-memory-decision--operation-stop job)
                   actual)
                 (lambda (reply) (cl-incf callbacks) (setq result reply))))
          (mevedel-test-application-wait (lambda () result)))
        (should (= 1 callbacks))
        (should (plist-get result :ok))
        (should (plist-get result :cancelled))
        (should (eq 'applied (plist-get actual :status)))
        (should (equal actual (plist-get result :value)))
        (if cleanup
            (progn (should injected) (should (string-match-p "Injected final result cleanup failure" (plist-get result :error))))
          (should-not (plist-get result :error)))
        (mevedel-memory-decision--operation-ready job '(:ok t))
        (should (= 1 callbacks))
        (should-not (file-exists-p path))
        (should-not (gethash key mevedel-memory-decision--operations))
        ;; The genuine accepted application won completion before the user
        ;; cancellation; the fence's winning status does not classify cause.
        (should (eq 'completed (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))))))

(mevedel-deftest mevedel-memory-decision--start-operation/fallback-consumer ()
  ,test
  (test)
  :doc "unsupported native fallback propagates consumer error or quit once after release"
  (dolist (condition '(error quit))
    (mevedel-test-application-fixture
      (let ((mevedel-journal-worker--child-p t) job caught (callbacks 0))
        (condition-case err
            (mevedel-memory-decision--start-operation
             workspace
             (lambda (_claim _live reply)
               (should (plist-get reply :synchronous))
               (setq job (gethash key mevedel-memory-decision--operations))
               '(:status skipped))
             (lambda (_reply)
               (cl-incf callbacks)
               (should (plist-get job :done))
               (should-not (gethash key mevedel-memory-decision--operations))
               (signal condition (when (eq condition 'error) '("Injected fallback consumer failure")))))
          ((error quit) (setq caught err)))
        (should job)
        (should (= 1 callbacks))
        (should (eq condition (car caught)))
        (when (eq condition 'error)
          (should (equal "Injected fallback consumer failure" (error-message-string caught))))
        (should-not (gethash key mevedel-memory-decision--operations))
        (should-not (memq (plist-get job :timer) timer-list))
        (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :status)))
        (should (file-exists-p path))
        (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))))))

(mevedel-deftest mevedel-memory-pass--apply-async/applied-cleanup-error ()
  ,test
  (test)
  :doc "public pass retains real applied accounting and actionable cancelled cleanup error"
  (mevedel-test-application-fixture
    (let ((apply-owned (symbol-function 'mevedel-memory-decision--apply-owned))
          (settle (symbol-function 'mevedel-journal-claim-settle))
          job injected (callbacks 0))
      (plist-put state :prepared (plist-get accepted :prepared))
      (plist-put state :settled nil)
      (plist-put state :accepted accepted)
      (plist-put state :entry (plist-get accepted :review))
      (plist-put state :publication-finished t)
      (plist-put state :telemetry '(:mode auto))
      (plist-put state :callback (lambda (value) (cl-incf callbacks) (setq result value)))
      (cl-letf (((symbol-function 'mevedel-memory-decision--apply-owned)
                 (lambda (&rest args)
                   (setq job (gethash key mevedel-memory-decision--operations))
                   (prog1 (apply apply-owned args)
                     (mevedel-memory-decision--operation-stop job))))
                ((symbol-function 'mevedel-journal-claim-settle)
                 (lambda (token &rest args)
                   (when (and job (not injected) (plist-get job :done) (eq token (plist-get job :claim)))
                     (setq injected t)
                     (error "Injected applied cleanup failure"))
                   (apply settle token args))))
        (mevedel-memory-pass--finish state '(:outcome success))
        (mevedel-test-application-wait (lambda () result)))
      (should injected)
      (should (= 1 callbacks))
      (should (plist-get result :cancelled))
      (should (string-match-p "Injected applied cleanup failure" (plist-get result :error)))
      (should (= 1 (plist-get result :applied-count)))
      (should (= 1 (plist-get result :updated-files)))
      (should (= 0 (plist-get result :unapplied-count)))
      (should-not (file-exists-p path))
      (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
      (should-not (gethash key mevedel-memory-pass--running))
      (should-not (gethash key mevedel-memory-decision--operations))
      (should-not (gethash workspace mevedel-memory-pass--deferred))
      (mevedel-memory-decision--operation-ready job '(:ok t))
      (should (= 1 callbacks)))))

(mevedel-deftest mevedel-memory-decision--recover-next-write/unmarked ()
  ,test
  (test)
  :doc "cold scheduled recovery skips unmarked external after-state and freshly observes a later marker"
  (mevedel-test-application-fixture
    (let* ((mevedel-memory-decision--inhibit-recovery nil)
           (scope (plist-get (plist-get accepted :prepared) :scope))
           (claim (mevedel-journal-claim-acquire key 180))
           (intent (mevedel-memory-write-call
                    scope (plist-get proposal :root)
                    (lambda (target)
                      (let ((intent (mevedel-memory-write-prepare workspace claim target accepted proposal)))
                        (mevedel-memory-write-abandon workspace claim target intent)
                        intent))))
           (pin (mevedel-memory-write--pin intent))
           (bad-id (make-string 64 ?0))
           (bad-file (file-name-concat (mevedel-memory-write--directory workspace) (concat bad-id ".el")))
           (start (symbol-function 'mevedel-journal-worker-start))
           (children 0) (callbacks 0) warnings)
      (unwind-protect
          (progn
            (delete-file path)
            (write-region "(:invalid t)" nil bad-file nil 'silent)
            ;; This fixture's synthetic pass is not active recovery work.
            (when (eq state (gethash key mevedel-memory-pass--running))
              (remhash key mevedel-memory-pass--running))
            (cl-letf (((symbol-function 'display-warning) (lambda (_ text &rest _) (push text warnings)))
                      ((symbol-function 'mevedel-journal-worker-start)
                       (lambda (&rest args) (cl-incf children) (apply start args))))
              (mevedel-memory-decision-schedule-recovery workspace (lambda () (cl-incf callbacks)))
              (mevedel-test-application-wait (lambda () (= 1 callbacks)))
              (should (= 1 children))
              (should (= 1 (length warnings)))
              (should (string-match-p bad-id (car warnings)))
              (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
              (should-not (file-exists-p pin))
              ;; Late durable marker delivery must be freshly observed on the
              ;; next maintenance, never replaced by an unmarked seen cache.
              (delete-file bad-file)
              (write-region (plist-get intent :hash) nil pin nil 'silent)
              (setq children 0)
              (mevedel-memory-decision-schedule-recovery workspace (lambda () (cl-incf callbacks)))
              (mevedel-test-application-wait (lambda () (= 2 callbacks)))
              (should (= 2 children)))
            (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status)))
            (should-not (file-exists-p path))
            (should-not (file-exists-p pin))
            (should-not (mevedel-memory-decision-recovering-p workspace)))
        (mevedel-memory-decision-stop-recovery)))))

(provide 'test-mevedel-memory-application-worker)
;;; test-mevedel-memory-application-worker.el ends here
