;;; test-mevedel-memory-decision-transaction.el --- Public owned decisions -*- lexical-binding: t -*-

;;; Commentary:
;; Real accepted proposals exercise the caller-facing transaction boundary.

;;; Code:

(require 'memory-history-fixtures
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "memory-history-fixtures"))
(require 'mevedel-journal-claim)
(require 'mevedel-journal-worker)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-journal-store)
(require 'mevedel-session-control-fs)

(defmacro mevedel-test-decision-transaction (&rest body)
  "Run BODY with WORKSPACE, ACCEPTED, PASS and three pending PROPOSALS."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-decision-transaction-" t))
          (workspace (mevedel-workspace--create :root root))
          (mevedel-memory-dirs nil)
          (mevedel-memory-consolidation-mode 'auto)
          (mevedel-journal-cleanup--inhibit-scheduling t)
          (kill-emacs-hook kill-emacs-hook)
          (_identity (mevedel-workspace-identity-ensure root))
          (accepted (car (mevedel-test-memory-history-create workspace 1 3 64 t)))
          (pass (plist-get (plist-get accepted :prepared) :id))
          (proposals (plist-get accepted :proposals))
          (automatic (mevedel-memory-decision-automatic-context workspace (current-buffer))))
     (unwind-protect
         (mevedel-test--with-idle-application ,@body)
       (mevedel-memory-decision-cancel automatic)
       (delete-directory root t))))

(defun mevedel-test-decision-await (predicate)
  "Drive the event loop until PREDICATE is true, bounded by thirty seconds."
  (let ((deadline (+ (float-time) 30)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil .01)))
  (should (funcall predicate)))

(mevedel-deftest mevedel-memory-decision-apply/transaction ()
  ,test
  (test)
  :doc "one automatic context survives serial jobs without inheriting their mutable completion state"
  (mevedel-test-decision-transaction
    (let (continuation paths results (callbacks 0))
      (dolist (proposal proposals)
        (let (result)
          (should-not
           (mevedel-memory-decision-apply
            workspace pass (plist-get proposal :id) :automatic automatic
            :continuation continuation
            :on-applied (lambda (changed) (setq paths (append paths changed)))
            :callback (lambda (value) (cl-incf callbacks) (setq result value))))
          (mevedel-test-decision-await (lambda () result))
          (should-not (plist-get result :error))
          (should-not (plist-get result :cancelled))
          (should (eq 'applied (plist-get result :status)))
          (should (mevedel-memory-decision-automatic-current-p automatic))
          (push result results)
          (setq continuation (plist-get result :continuation))
          (should continuation)))
      (should (= 3 callbacks (length results) (length (delete-dups paths))))
      (should (cl-every (lambda (path) (not (file-exists-p path))) paths))
      (should-not (plist-get (mevedel-memory-decision-pending workspace) :items))))
  :doc "busy automatic admission reports one callback error without leaking a private operation"
  (mevedel-test-decision-transaction
    (let ((claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
          (callbacks 0) result)
      (unwind-protect
          (progn
            (should claim)
            (should-not
             (mevedel-memory-decision-apply
              workspace pass (plist-get (car proposals) :id) :automatic automatic
              :callback (lambda (value) (cl-incf callbacks) (setq result value))))
            (should (= 1 callbacks))
            (should (equal "Memory consolidation is busy" (plist-get result :error)))
            (should-not (plist-get result :continuation))
            (setq result nil)
            (should-not
             (mevedel-memory-decision-pending
              workspace :automatic automatic
              :callback (lambda (value) (cl-incf callbacks) (setq result value))))
            (should (= 2 callbacks))
            (should (equal "Memory consolidation is busy" (plist-get result :error)))
            (should (equal "Caller callback failed"
                           (error-message-string
                            (should-error
                             (mevedel-memory-decision-apply
                              workspace pass (plist-get (car proposals) :id) :automatic automatic
                              :callback (lambda (_value)
                                          (cl-incf callbacks)
                                          (error "Caller callback failed")))))))
            (should (= 3 callbacks))
            (should (mevedel-journal-claim-owned-p claim)))
        (mevedel-journal-claim-settle claim 'cancelled "")))))

(mevedel-deftest mevedel-memory-decision-inspect/transaction ()
  ,test
  (test)
  :doc "proposal inspection copies mutable file states rather than exposing authenticated bundle strings"
  (mevedel-test-decision-transaction
    (let ((read (symbol-function 'mevedel-memory-store-accepted)) observed expected snapshot)
      (cl-letf (((symbol-function 'mevedel-memory-store-accepted)
                 (lambda (&rest args)
                   (setq observed (apply read args)
                         expected (prin1-to-string observed))
                   observed)))
        (setq snapshot (mevedel-memory-decision-inspect
                        workspace :pass pass :proposal (plist-get (car proposals) :id))))
      (let ((state (cdr (car (plist-get snapshot :before)))))
        (aset (plist-get state :path) 0 ?!)
        (aset (plist-get state :bytes) 0 ?!)
        (setcar (plist-get snapshot :before) nil))
      (should (equal expected (prin1-to-string observed)))
      (should (eq 'applied (plist-get (mevedel-memory-decision-apply
                                      workspace pass (plist-get (car proposals) :id)) :status)))))
  :doc "workspace inspection copies proposal strings and row structure"
  (mevedel-test-decision-transaction
    (let ((read (symbol-function 'mevedel-memory-store-accepted)) observed expected snapshot)
      (cl-letf (((symbol-function 'mevedel-memory-store-accepted)
                 (lambda (&rest args)
                   (setq observed (apply read args)
                         expected (prin1-to-string observed))
                   observed)))
        (setq snapshot (mevedel-memory-decision-inspect workspace)))
      (let ((row (car (plist-get snapshot :rows))))
        (aset (plist-get row :id) 0 ?!)
        (aset (plist-get (plist-get row :proposal) :title) 0 ?!)
        (setcar (plist-get snapshot :rows) nil))
      (should (equal expected (prin1-to-string observed)))
      (should (= 3 (length (plist-get (mevedel-memory-decision-inspect workspace) :rows)))))))

(mevedel-deftest mevedel-memory-decision-apply/serial ()
  ,test
  (test)
  :doc "intact serial continuity skips history; an intervening accepted decision is recovered before return"
  (mevedel-test-decision-transaction
    (let ((recover (symbol-function 'mevedel-memory-decision-recover))
          (program (symbol-function 'mevedel-session-control-fs--run-program))
          (recoveries 0) (programs 0) continuation)
      (cl-letf (((symbol-function 'mevedel-memory-decision-recover)
                 (lambda (&rest args) (cl-incf recoveries) (apply recover args)))
                ((symbol-function 'mevedel-session-control-fs--run-program)
                 (lambda (&rest args) (cl-incf programs) (apply program args))))
        (setq continuation
              (plist-get (mevedel-memory-decision-apply workspace pass (plist-get (car proposals) :id)) :continuation))
        (should (= 1 recoveries))
        (setq programs 0)
        (setq continuation
              (plist-get (mevedel-memory-decision-apply workspace pass (plist-get (cadr proposals) :id)
                                                       :continuation continuation) :continuation))
        (should (= 1 recoveries))
        ;; Compiled matched 1-pass/4-note probe: 60 programs per sequenced
        ;; application. Twenty percent slack allows 72, not another scan.
        (should (<= programs 72))
        (let (injected)
          (cl-letf (((symbol-function 'mevedel-journal-store-publish-decision)
                     (lambda (&rest _args) (setq injected t) (error "Injected publication interruption"))))
            (should (equal "Injected publication interruption"
                           (error-message-string
                            (should-error (mevedel-memory-decision-reject
                                           workspace pass (plist-get (nth 2 proposals) :id) :reason "Foreign decision"))))))
          (should injected))
        (should-not (mevedel-memory-decision-status workspace (plist-get (nth 2 proposals) :id)))
        (let ((before recoveries))
          (should (eq 'applied (plist-get (mevedel-memory-decision-apply
                                          workspace pass (plist-get (car proposals) :id)
                                          :continuation continuation) :status)))
          (should (= (1+ before) recoveries)))
        (should (equal "Foreign decision"
                       (plist-get (mevedel-memory-decision-status workspace (plist-get (nth 2 proposals) :id)) :reason))))))
  :doc "early and prepared late holds both discard continuity and force recovery on the next decision"
  (dolist (late '(nil t))
    (mevedel-test-decision-transaction
      (let* ((first (mevedel-memory-decision-apply workspace pass (plist-get (car proposals) :id)))
             (proposal (cadr proposals))
             (path (plist-get (cdr (car (plist-get proposal :before))) :path))
             (prepare (symbol-function 'mevedel-memory-write--prepare))
             (abandon (symbol-function 'mevedel-memory-write--abandon))
             (recover (symbol-function 'mevedel-memory-decision-recover))
             (buffer (find-file-noselect path))
             (prepared 0) (abandoned 0) (recoveries 0) held holding)
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-memory-write--prepare)
                       (lambda (&rest args)
                         (prog1 (apply prepare args)
                           (cl-incf prepared)
                           (when late (setq holding t)))))
                      ((symbol-function 'mevedel-memory-notes-in-use-p)
                       (lambda (&rest _) holding))
                      ((symbol-function 'mevedel-memory-write--abandon)
                       (lambda (&rest args) (cl-incf abandoned) (apply abandon args)))
                      ((symbol-function 'mevedel-memory-decision-recover)
                       (lambda (&rest args) (cl-incf recoveries) (apply recover args))))
              (unless late (with-current-buffer buffer (insert "unsaved")))
              (setq held (mevedel-memory-decision-apply
                          workspace pass (plist-get proposal :id) :continuation (plist-get first :continuation)))
              (should (eq 'held (plist-get held :status)))
              (should-not (plist-get held :continuation))
              (should (= (if late 1 0) prepared abandoned))
              (should (= 0 recoveries))
              (should (file-exists-p path))
              (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
              (should-not (plist-get (mevedel-memory-decision-inspect
                                     workspace :pass pass :proposal (plist-get proposal :id)) :writes))
              (setq holding nil late nil)
              (should (eq 'applied (plist-get (mevedel-memory-decision-apply
                                              workspace pass (plist-get (nth 2 proposals) :id)
                                              :continuation (plist-get held :continuation)) :status)))
              (should (= 1 recoveries)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(provide 'test-mevedel-memory-decision-transaction)
;;; test-mevedel-memory-decision-transaction.el ends here
