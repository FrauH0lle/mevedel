;;; test-niwa-memory-decision-transaction.el --- Public owned decisions -*- lexical-binding: t -*-

;;; Commentary:
;; Real accepted proposals exercise the caller-facing transaction boundary.

;;; Code:

(require 'memory-history-fixtures
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "memory-history-fixtures"))
(require 'niwa-journal-claim)
(require 'niwa-journal-worker)
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-store)
(require 'niwa-memory-write)
(require 'niwa-journal-store)
(require 'niwa-resource)
(require 'niwa-session-control-fs)

(defmacro niwa-test-decision-transaction (&rest body)
  "Run BODY with WORKSPACE, ACCEPTED, PASS and three pending PROPOSALS."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "niwa-decision-transaction-" t))
          (workspace (niwa-workspace--create :root root))
          (niwa-memory-dirs nil)
          (niwa-memory-consolidation-mode 'auto)
          (niwa-journal-cleanup--inhibit-scheduling t)
          (kill-emacs-hook kill-emacs-hook)
          (_identity (niwa-workspace-identity-ensure root))
          (accepted (car (niwa-test-memory-history-create workspace 1 3 64 t)))
          (pass (plist-get (plist-get accepted :prepared) :id))
          (proposals (plist-get accepted :proposals))
          (automatic (niwa-memory-decision-automatic-context workspace (current-buffer))))
     (unwind-protect
         (niwa-test--with-idle-application ,@body)
       (niwa-memory-decision-cancel automatic)
       (delete-directory root t))))

(defun niwa-test-decision-await (predicate)
  "Drive the event loop until PREDICATE is true, bounded by thirty seconds."
  (let ((deadline (+ (float-time) 30)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (niwa-test--application-step)))
  (should (funcall predicate)))

(niwa-deftest niwa-memory-decision-apply/transaction ()
  ,test
  (test)
  :doc "one automatic context survives serial jobs without inheriting their mutable completion state"
  (niwa-test-decision-transaction
    (let (continuation paths results (callbacks 0))
      (dolist (proposal proposals)
        (let (result)
          (should-not
           (niwa-memory-decision-apply
            workspace pass (plist-get proposal :id) :automatic automatic
            :continuation continuation
            :on-applied (lambda (changed) (setq paths (append paths changed)))
            :callback (lambda (value) (cl-incf callbacks) (setq result value))))
          (niwa-test-decision-await (lambda () result))
          (should-not (plist-get result :error))
          (should-not (plist-get result :cancelled))
          (should (eq 'applied (plist-get result :status)))
          (should (niwa-memory-decision-automatic-current-p automatic))
          (push result results)
          (setq continuation (plist-get result :continuation))
          (should continuation)))
      (should (= 3 callbacks (length results) (length (delete-dups paths))))
      (should (cl-every (lambda (path) (not (file-exists-p path))) paths))
      (should-not (plist-get (niwa-memory-decision-pending workspace) :items))))
  :doc "busy automatic admission reports one callback error without leaking a private operation"
  (niwa-test-decision-transaction
    (let ((claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
          (callbacks 0) result)
      (unwind-protect
          (progn
            (should claim)
            (should-not
             (niwa-memory-decision-apply
              workspace pass (plist-get (car proposals) :id) :automatic automatic
              :callback (lambda (value) (cl-incf callbacks) (setq result value))))
            (should (= 1 callbacks))
            (should (equal "Memory consolidation is busy" (plist-get result :error)))
            (should-not (plist-get result :continuation))
            (setq result nil)
            (should-not
             (niwa-memory-decision-pending
              workspace :automatic automatic
              :callback (lambda (value) (cl-incf callbacks) (setq result value))))
            (should (= 2 callbacks))
            (should (equal "Memory consolidation is busy" (plist-get result :error)))
            (should (equal "Caller callback failed"
                           (error-message-string
                            (should-error
                             (niwa-memory-decision-apply
                              workspace pass (plist-get (car proposals) :id) :automatic automatic
                              :callback (lambda (_value)
                                          (cl-incf callbacks)
                                          (error "Caller callback failed")))))))
            (should (= 3 callbacks))
            (should (niwa-journal-claim-owned-p claim)))
        (niwa-journal-claim-settle claim 'cancelled ""))))
  :doc "resource access during the final ownership check holds deletion without electing a slot"
  (dolist (automatic-p '(nil t))
    (niwa-test-decision-transaction
      (let* ((first (niwa-memory-decision-apply workspace pass (plist-get (car proposals) :id)))
             (proposal (cadr proposals))
             (path (plist-get (cdr (car (plist-get proposal :before))) :path))
             (conversation (generate-new-buffer " *decision-late-note-use*"))
             (commit (symbol-function 'niwa-memory-notes-commit))
             entered result)
        (unwind-protect
            (progn
              (with-current-buffer conversation
                (setq-local niwa--session (niwa-session--create :workspace workspace)))
              (cl-letf (((symbol-function 'niwa-memory-notes-commit)
                         (lambda (ws root target changes currentp &optional before-write acknowledged)
                           (funcall commit ws root target changes
                                    (lambda ()
                                      (prog1 (funcall currentp)
                                        ;; Ownership/root IO may dispatch a read
                                        ;; after the earlier admission check.
                                        (unless entered
                                          (setq entered t)
                                          (with-current-buffer conversation
                                            (niwa-resource-prepare
                                             'read (niwa-memory-notes-address (plist-get proposal :file))
                                             (list :session niwa--session))))))
                                    before-write acknowledged))))
                (setq result (niwa-memory-decision-apply
                              workspace pass (plist-get proposal :id)
                              :automatic (and automatic-p automatic)
                              :continuation (plist-get first :continuation))))
              (should entered)
              (should (niwa-memory-notes-in-use-p workspace (plist-get proposal :file)))
              (should (eq 'held (plist-get result :status)))
              (should-not (plist-get result :continuation))
              (should (file-exists-p path))
              (should-not (niwa-memory-decision-status workspace (plist-get proposal :id))))
          (kill-buffer conversation))))))

(niwa-deftest niwa-memory-decision-apply/persistence ()
  (niwa-test-decision-transaction
    (let* ((proposal (car proposals)) (id (plist-get proposal :id))
           (note (plist-get (cdar (plist-get proposal :before)) :path))
           (slot (file-name-concat (niwa-memory-decision--directory workspace) (concat id ".el")))
           (elect (symbol-function 'niwa-memory-decision--elect)) injected)
      ;; Only the elector may create the slot. Even a foreign file arriving
      ;; after the target acknowledged deletion must never be overwritten.
      (cl-letf (((symbol-function 'niwa-memory-decision--elect)
                 (lambda (&rest args)
                   (should-not (file-exists-p note))
                   (setq injected t)
                   (write-region "Foreign state" nil slot nil 'silent)
                   (apply elect args))))
        (should-error (niwa-memory-decision-apply workspace pass id)))
      (should injected)
      (should (equal "Foreign state" (niwa-session-control-fs-read-file slot)))
      (should-error (niwa-memory-decision-status workspace id)))))

(niwa-deftest niwa-memory-decision-inspect/transaction ()
  ,test
  (test)
  :doc "proposal inspection copies mutable file states rather than exposing authenticated bundle strings"
  (niwa-test-decision-transaction
    (let ((read (symbol-function 'niwa-memory-store-accepted)) observed expected snapshot)
      (cl-letf (((symbol-function 'niwa-memory-store-accepted)
                 (lambda (&rest args)
                   (setq observed (apply read args)
                         expected (prin1-to-string observed))
                   observed)))
        (setq snapshot (niwa-memory-decision-inspect
                        workspace :pass pass :proposal (plist-get (car proposals) :id))))
      (let ((state (cdr (car (plist-get snapshot :before)))))
        (aset (plist-get state :path) 0 ?!)
        (aset (plist-get state :bytes) 0 ?!)
        (setcar (plist-get snapshot :before) nil))
      (should (equal expected (prin1-to-string observed)))
      (should (eq 'applied (plist-get (niwa-memory-decision-apply
                                      workspace pass (plist-get (car proposals) :id)) :status)))))
  :doc "workspace inspection copies proposal strings and row structure"
  (niwa-test-decision-transaction
    (let ((read (symbol-function 'niwa-memory-store-accepted)) observed expected snapshot)
      (cl-letf (((symbol-function 'niwa-memory-store-accepted)
                 (lambda (&rest args)
                   (setq observed (apply read args)
                         expected (prin1-to-string observed))
                   observed)))
        (setq snapshot (niwa-memory-decision-inspect workspace)))
      (let ((row (car (plist-get snapshot :rows))))
        (aset (plist-get row :id) 0 ?!)
        (aset (plist-get (plist-get row :proposal) :title) 0 ?!)
        (setcar (plist-get snapshot :rows) nil))
      (should (equal expected (prin1-to-string observed)))
      (should (= 3 (length (plist-get (niwa-memory-decision-inspect workspace) :rows)))))))

(niwa-deftest niwa-memory-decision-apply/serial ()
  (niwa-test-decision-transaction
    (let ((recover (symbol-function 'niwa-memory-decision-recover))
          (program (symbol-function 'niwa-session-control-fs--run-program))
          (recoveries 0) (programs 0) continuation)
      (cl-letf (((symbol-function 'niwa-memory-decision-recover)
                 (lambda (&rest args) (cl-incf recoveries) (apply recover args)))
                ((symbol-function 'niwa-session-control-fs--run-program)
                 (lambda (&rest args) (cl-incf programs) (apply program args))))
        (dolist (proposal proposals)
          (setq programs 0)
          (let ((result (niwa-memory-decision-apply workspace pass (plist-get proposal :id) :continuation continuation)))
            (should (eq 'applied (plist-get result :status)))
            (setq continuation (plist-get result :continuation)))
          ;; Matched four-note probe: 31 programs per decision.
          (should (<= programs 38)))
        (should (zerop recoveries))))))

(provide 'test-niwa-memory-decision-transaction)
;;; test-niwa-memory-decision-transaction.el ends here
