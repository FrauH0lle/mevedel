;;; test-mevedel-edit-diagnostics.el --- Tests for mevedel-edit-diagnostics.el -*- lexical-binding: t -*-

;;; Commentary:

;; The edit-diagnostics state machine: baseline capture, post-edit
;; classification against baseline and last report, and delivery through
;; the reminder turn-event queue.

;;; Code:

(require 'gptel)
(require 'gptel-request)
(require 'mevedel-edit-diagnostics)
(require 'mevedel-reminders)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-session-persistence)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-edit-diagnostics--record-current
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "classifies the first refresh against baseline and later refreshes against the last report"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "sample.el"))
         (ws (mevedel-workspace-get-or-create 'project root root "diag"))
         (session (mevedel-session-create "main" ws))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         (source nil)
         (backend (gptel-make-openai
                   "reminder-diagnostics" :key "test" :host "example.test"
                   :models '(test)))
         (data (list :messages [(:role "user" :content "task")]))
         (fsm (gptel-make-fsm
               :info (list :buffer chat :backend backend :data data)))
         (old nil)
         (fixed nil)
         (new nil))
    (unwind-protect
        (progn
          (write-region "(message \"ok\")\n" nil file nil 'silent)
          (setq source (find-file-noselect file)
                old (list file 1 "error" "old")
                fixed (list file 2 "warning" "fixed")
                new (list file 3 "warning" "new"))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-edit-diagnostics--collect-in-buffer)
                     (lambda () (list old fixed))))
            (mevedel-edit-diagnostics-before-edit chat file))
          (mevedel-edit-diagnostics--record-current
           chat file (list old new))
          (mevedel-reminders--handle-inject fsm)
          (let ((body (plist-get
                       (aref (plist-get data :messages) 1) :content)))
            (should (string-match-p "New or changed diagnostics" body))
            (should (string-match-p "new" body))
            (should (string-match-p "Pre-existing diagnostics" body))
            (should (string-match-p "old" body))
            (should-not (string-match-p "fixed" body)))
          (mevedel-edit-diagnostics--record-current
           chat file (list (list file 1 "error" "changed")))
          (mevedel-reminders--handle-inject fsm)
          (let ((body (plist-get
                       (aref (plist-get data :messages) 2) :content)))
            (should (string-match-p "changed" body))
            (should-not (string-match-p "Pre-existing diagnostics" body))
            (should-not (string-match-p "\\[error\\] old" body))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t))))

(mevedel-deftest mevedel-edit-diagnostics-after-edit
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "starts active checkers and continues after their fresh results are recorded"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "source.el"))
         (ws (mevedel-workspace-get-or-create
              'project root root "diagnostics"))
         (session (mevedel-session-create "main" ws root))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         source
         started
         telemetry
         continued)
    (unwind-protect
        (progn
          (write-region "(message \"old\")\n" nil file nil 'silent)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local flymake-mode t))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-edit-diagnostics--collect-in-buffer)
                     (lambda () nil)))
            (mevedel-edit-diagnostics-before-edit chat file))
          (cl-letf (((symbol-function 'flymake-start)
                     (lambda (&rest _) (setq started t)))
                    ((symbol-function 'flymake-disabled-backends)
                     (lambda () nil))
                    ((symbol-function 'mevedel-telemetry-current-session)
                     (lambda (&optional _buffer) session))
                    ((symbol-function 'mevedel-telemetry-record)
                     (lambda (_session _event &rest props)
                       (push (plist-get props :outcome) telemetry)))
                    ((symbol-function
                      'mevedel-edit-diagnostics--collect-in-buffer)
                     (lambda () (list (list file 1 "error" "fresh")))))
            (mevedel-edit-diagnostics-after-edit
             chat file (lambda () (setq continued t))))
          (should started)
          (should continued)
          (should (equal '(started ready) (nreverse telemetry)))
          (with-current-buffer chat
            (should (assq 'diagnostics
                          (plist-get mevedel-reminders--turn-events
                                     :items)))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t)))
  :doc "restarts an in-flight Flycheck run before collecting results"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "source.el"))
         (ws (mevedel-workspace-get-or-create
              'project root root "diagnostics"))
         (session (mevedel-session-create "main" ws root))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         source
         events
         continued)
    (unwind-protect
        (progn
          (write-region "(message \"old\")\n" nil file nil 'silent)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local flycheck-mode t))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-edit-diagnostics--collect-in-buffer)
                     (lambda () nil)))
            (mevedel-edit-diagnostics-before-edit chat file))
          (cl-letf (((symbol-function 'flycheck-stop)
                     (lambda () (push 'stop events)))
                    ((symbol-function 'flycheck-get-checker-for-buffer)
                     (lambda () 'test-checker))
                    ((symbol-function 'flycheck-buffer)
                     (lambda ()
                       (push 'start events)
                       (run-hooks 'flycheck-after-syntax-check-hook)))
                    ((symbol-function
                      'mevedel-edit-diagnostics--collect-in-buffer)
                     (lambda ()
                       (push 'collect events)
                       nil)))
            (mevedel-edit-diagnostics-after-edit
             chat file (lambda () (setq continued t))))
          (should continued)
          (should (equal '(stop start collect) (nreverse events))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t)))
  :doc "continues without collecting stale overlays when checker startup fails"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "source.el"))
         (ws (mevedel-workspace-get-or-create
              'project root root "diagnostics"))
         (session (mevedel-session-create "main" ws root))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         source
         collected
         continued)
    (unwind-protect
        (progn
          (write-region "(message \"old\")\n" nil file nil 'silent)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local flycheck-mode t))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-edit-diagnostics--collect-in-buffer)
                     (lambda () nil)))
            (mevedel-edit-diagnostics-before-edit chat file))
          (cl-letf (((symbol-function 'flycheck-stop) #'ignore)
                    ((symbol-function 'flycheck-get-checker-for-buffer)
                     (lambda () 'test-checker))
                    ((symbol-function 'flycheck-buffer)
                     (lambda () (error "Cannot start checker")))
                    ((symbol-function
                      'mevedel-edit-diagnostics--collect-in-buffer)
                     (lambda ()
                       (setq collected t)
                       nil)))
            (mevedel-edit-diagnostics-after-edit
             chat file (lambda () (setq continued t))))
          (should continued)
          (should-not collected))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t)))
  :doc "does not discard an unsaved buffer when the edited file changed on disk"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-diag-" t)))
         (file (file-name-concat root "source.el"))
         (ws (mevedel-workspace-get-or-create
              'project root root "diagnostics"))
         (session (mevedel-session-create "main" ws root))
         (chat (generate-new-buffer " *mevedel-diag-chat*"))
         source
         started
         continued)
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq source (find-file-noselect file))
          (with-current-buffer source
            (setq-local flymake-mode t)
            (goto-char (point-max))
            (insert "unsaved\n"))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create :id "request-1"
                                                 :session session)))
          (cl-letf (((symbol-function
                      'mevedel-edit-diagnostics--collect-in-buffer)
                     (lambda () nil)))
            (mevedel-edit-diagnostics-before-edit chat file))
          (with-temp-buffer
            (insert "external\n")
            (write-region nil nil file nil 'silent))
          (cl-letf (((symbol-function 'flymake-start)
                     (lambda (&rest _) (setq started t))))
            (mevedel-edit-diagnostics-after-edit
             chat file (lambda () (setq continued t))))
          (should continued)
          (should-not started)
          (with-current-buffer source
            (should (string-match-p "unsaved" (buffer-string)))))
      (when (buffer-live-p source)
        (with-current-buffer source
          (set-buffer-modified-p nil))
        (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t))))

(mevedel-deftest mevedel-edit-diagnostics--run-checkers
  ()
  ,test
  (test)
  :doc "settles checker events, timeout, and dead-buffer cleanup exactly once"
  (let ((chat (generate-new-buffer " *mevedel-diag-chat*"))
        (source (generate-new-buffer " *mevedel-diag-source*"))
        (owner 'request-1)
        flymake-report timer-callback cancelled telemetry continued)
    (unwind-protect
        (cl-letf (((symbol-function 'flymake-make-report-fn)
                   (lambda (&rest _)
                     (lambda (&rest _) nil)))
                  ((symbol-function 'flymake-start)
                   (lambda (&rest _)
                     (let ((synchronous
                            (flymake-make-report-fn 'sync-backend)))
                       (setq flymake-report
                             (flymake-make-report-fn 'async-backend))
                       (funcall synchronous nil))))
                  ((symbol-function 'flymake-disabled-backends)
                   (lambda () nil))
                  ((symbol-function 'flycheck-stop) #'ignore)
                  ((symbol-function 'flycheck-get-checker-for-buffer)
                   (lambda () 'test-checker))
                  ((symbol-function 'flycheck-buffer) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (delay repeat function &rest _)
                     (should (= 30 delay))
                     (should-not repeat)
                     (setq timer-callback function)
                     'diagnostic-timer))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (setq cancelled timer)))
                  ((symbol-function 'mevedel-reminders-turn-owner)
                   (lambda (_) owner))
                  ((symbol-function
                    'mevedel-edit-diagnostics--collect-in-buffer)
                   (lambda () '(("source.el" 1 "error" "fresh"))))
                  ((symbol-function
                    'mevedel-edit-diagnostics--record-current)
                   (lambda (&rest _) '(:new (diagnostic))))
                  ((symbol-function
                    'mevedel-edit-diagnostics--record-telemetry)
                   (lambda (_buffer outcome &optional _report)
                     (push outcome telemetry))))
          (mevedel-edit-diagnostics--run-checkers
           chat "/tmp/source.el" source owner
           (lambda () (setq continued t)) t t)
          (should timer-callback)
          (should-not continued)
          (with-current-buffer source
            (run-hooks 'flycheck-after-syntax-check-hook))
          (should-not continued)
          (funcall flymake-report nil)
          (should continued)
          (should (eq 'diagnostic-timer cancelled))
          (should (equal '(started ready) (nreverse telemetry)))
          (with-current-buffer source
            (should-not flycheck-after-syntax-check-hook))
          (setq timer-callback nil
                cancelled nil
                telemetry nil
                continued nil)
          (mevedel-edit-diagnostics--run-checkers
           chat "/tmp/source.el" source owner
           (lambda () (setq continued t)) nil t)
          (funcall timer-callback)
          (should continued)
          (should (eq 'diagnostic-timer cancelled))
          (should (equal '(started timeout) (nreverse telemetry)))
          (setq timer-callback nil
                cancelled nil
                telemetry nil
                continued nil)
          (cl-letf (((symbol-function 'flymake-start)
                     (lambda (&rest _)
                       (flymake-make-report-fn 'failed-backend)))
                    ((symbol-function 'flymake-disabled-backends)
                     (lambda () '(failed-backend))))
            (mevedel-edit-diagnostics--run-checkers
             chat "/tmp/source.el" source owner
             (lambda () (setq continued t)) t nil))
          (should continued)
          (should-not timer-callback)
          (should (equal '(started ready) (nreverse telemetry)))
          (setq timer-callback nil
                cancelled nil
                telemetry nil
                continued nil)
          (let ((selected 0)
                (stopped 0)
                (started 0))
            (cl-letf (((symbol-function 'flycheck-get-checker-for-buffer)
                       (lambda () (cl-incf selected) nil))
                      ((symbol-function 'flycheck-stop)
                       (lambda () (cl-incf stopped)))
                      ((symbol-function 'flycheck-buffer)
                       (lambda () (cl-incf started))))
              (mevedel-edit-diagnostics--run-checkers
               chat "/tmp/source.el" source owner
               (lambda () (setq continued t)) nil t))
            (should (= 1 selected))
            (should (zerop stopped))
            (should (zerop started)))
          (should continued)
          (should-not timer-callback)
          (should-not telemetry)
          (let ((continuation-calls 0)
                late-report)
            (cl-letf (((symbol-function 'flymake-start)
                       (lambda (&rest _)
                         (setq late-report
                               (flymake-make-report-fn 'failed-backend))
                         (error "Cannot start backend"))))
              (mevedel-edit-diagnostics--run-checkers
               chat "/tmp/source.el" source owner
               (lambda () (cl-incf continuation-calls)) t nil))
            (should (= 1 continuation-calls))
            (funcall late-report nil)
            (should (= 1 continuation-calls)))
          (setq timer-callback nil
                cancelled nil
                telemetry nil
                continued nil)
          (mevedel-edit-diagnostics--run-checkers
           chat "/tmp/source.el" source owner
           (lambda () (setq continued t)) nil t)
          (kill-buffer source)
          (funcall timer-callback)
          (should continued)
          (should (eq 'diagnostic-timer cancelled))
          (should (equal '(started stale) (nreverse telemetry))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat)))))

(mevedel-deftest mevedel-edit-diagnostics--format-diagnostic-reports
  ()
  ,test
  (test)
  :doc "prioritizes new diagnostics and enforces per-file and total limits"
  (let* ((files '("/tmp/a.el" "/tmp/b.el" "/tmp/c.el" "/tmp/d.el"))
         (reports
          (mapcar
           (lambda (file)
             (list :path file
                   :new
                   (cl-loop for line from 1 to 12
                            collect (list file line "error"
                                          (format "problem-%d" line)))
                   :preexisting nil))
           files))
         (body (mevedel-edit-diagnostics--format-diagnostic-reports reports)))
    (should (= 30 (with-temp-buffer
                    (insert body)
                    (goto-char (point-min))
                    (how-many "^  "))))
    (should (= 18 (get-text-property
                   0 'mevedel-diagnostics-omitted body)))
    (should (string-match-p "18 diagnostics omitted" body))
    (should (string-match-p "New or changed diagnostics" body))
    (should-not (string-match-p "Pre-existing diagnostics" body))))

(mevedel-deftest mevedel-edit-diagnostics/cold-load ()
  ,test
  (test)
  :doc "loads the compiled owner directly and classifies a report"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-edit-diag-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (dolist (file '("mevedel-edit-diagnostics.el"))
            (copy-file (file-name-concat root file)
                       (file-name-concat compiled-root file)))
          (with-temp-buffer
            (unless (zerop
                     (call-process
                      emacs nil t nil "--batch" "-Q"
                      "-L" root "-L" compiled-root
                      "--eval"
                      (prin1-to-string
                       `(progn
                          (byte-compile-file
                           ,(file-name-concat
                             compiled-root "mevedel-edit-diagnostics.el"))
                          (load ,(file-name-concat
                                  compiled-root "mevedel-edit-diagnostics")
                                nil t)
                          (with-temp-buffer
                            ;; The state is owner-keyed; a stub owner is
                            ;; enough for the cold classification.
                            (fset 'mevedel-reminders-turn-owner
                                  (lambda (&optional _buffer) 'cold-owner))
                            (fset 'mevedel-reminders-queue-turn-event
                                  (lambda (&rest _) t))
                            (setq mevedel-edit-diagnostics--state
                                  (let ((files (make-hash-table
                                                :test #'equal)))
                                    (puthash "/tmp/cold.el"
                                             (list :baseline
                                                   '(("cold.el" 1 "error"
                                                      "old")))
                                             files)
                                    (list :owner 'cold-owner
                                          :files files)))
                            (let ((report
                                   (mevedel-edit-diagnostics--record-current
                                    (current-buffer) "/tmp/cold.el"
                                    '(("cold.el" 1 "error" "old")
                                      ("cold.el" 2 "warning" "new")))))
                              (unless (equal
                                       '(("cold.el" 2 "warning" "new"))
                                       (plist-get report :new))
                                (error "Owner did not classify the report"))))
                          (unless (string-suffix-p
                                   "mevedel-edit-diagnostics.elc"
                                   (or (symbol-file
                                        'mevedel-edit-diagnostics--record-current
                                        'defun)
                                       ""))
                            (error "Behavior has the wrong owner"))))))
              (error "Cold edit-diagnostics load failed: %s"
                     (buffer-string)))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-edit-diagnostics)
;;; test-mevedel-edit-diagnostics.el ends here
