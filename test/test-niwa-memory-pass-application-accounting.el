;;; test-niwa-memory-pass-application-accounting.el --- Confirmed application counts -*- lexical-binding: t -*-

;;; Commentary:
;; Bookkeeping fixtures inject decision outcomes at the public decision verbs,
;; not accepted storage proofs.  Synchronous pending selection keeps its real
;; owned recovery; checked target writes are covered by the decision suites.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-journal-claim)
(require 'niwa-journal-store)
(require 'niwa-journal-worker)
(require 'niwa-memory-decision)
(require 'niwa-memory-notes)
(require 'niwa-memory-pass)
(require 'niwa-memory-store)
(require 'niwa-workspace-identity)

(defun niwa-test-application--proposal (id &optional action)
  "Return bookkeeping metadata for ID with ACTION, defaulting to update."
  (list :id id :action (or action 'update) :file (concat id ".md")))

(defun niwa-test-application--accepted (proposals &optional pass mode)
  "Wrap PROPOSALS in bookkeeping metadata for PASS with frozen MODE."
  (list :prepared (list :id (or pass "current")
                        :scope (list :decision-format 'slots :roots (list (cons "notes" (list :kind 'notes :mode (or mode 'auto))))))
        :proposals proposals))

(defmacro niwa-test-application--fixture (&rest body)
  "Run BODY with a private WORKSPACE, native identity and application STATE."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "niwa-application-count-" t))
          (workspace (niwa-workspace--create :root root))
          (niwa-memory-dirs nil)
          (niwa-memory-consolidation-mode 'auto)
          (niwa-journal-worker--child-p t)
          (niwa-memory-pass--running (make-hash-table :test #'equal))
          (niwa-memory-pass--deferred (make-hash-table :test #'eq))
          (niwa-memory-decision--operations (make-hash-table :test #'equal))
          (state (list :workspace workspace :origin-buffer (current-buffer) :settled t))
          (_identity (niwa-workspace-identity-ensure root)))
     (unwind-protect
         ;; These symbolic proposal names have no persisted slots.
         (cl-letf (((symbol-function 'niwa-memory-decision-status) (lambda (&rest _) nil))) ,@body)
       (when-let* ((job (plist-get state :application)))
         (plist-put job :done t)
         (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
       (maphash (lambda (_ operation) (niwa-memory-decision--operation-stop operation))
                niwa-memory-decision--operations)
       (maphash (lambda (_ retry) (cancel-timer (plist-get retry :timer))) niwa-memory-pass--deferred)
       (delete-directory root t))))

(defun niwa-test-application--async (state accepted decide &optional pending)
  "Run STATE and ACCEPTED with DECIDE outcomes and PENDING identities.
DECIDE receives a proposal identity and its confirmation callback and returns
the decision result; `niwa-memory-decision-interrupted' becomes a
cancellation result carrying its accounted status.  Drive a bounded sequence
directly through the public automatic decision verbs."
  (let ((callbacks 0) result job)
    (cl-letf (((symbol-function 'niwa-memory-decision-apply)
               (cl-function
                (lambda (_workspace _pass proposal &key automatic on-applied callback &allow-other-keys)
                  (should automatic)
                  (should callback)
                  (funcall callback
                           (condition-case err (funcall decide proposal on-applied)
                             (niwa-memory-decision-interrupted
                              (list :status (plist-get (cadr err) :status) :cancelled t :reason (caddr err))))))))
              ((symbol-function 'niwa-memory-decision-pending)
               (cl-function (lambda (_workspace &key automatic callback &allow-other-keys)
                              (should automatic)
                              (funcall callback (list :items pending)))))
              ((symbol-function 'niwa-memory-pass--application-next) #'ignore))
      (setq job (niwa-memory-pass--apply-async
                 state accepted (lambda (value) (cl-incf callbacks) (setq result value))))
      (dotimes (_ 20)
        (unless (plist-get job :done) (niwa-memory-pass--application-step state job)))
      (should (plist-get job :done))
      (should (= 1 callbacks))
      (niwa-memory-pass--application-step state job)
      (should (= 1 callbacks))
      (should-not (plist-get result :error))
      (should (= 0 (hash-table-count niwa-memory-decision--operations)))
      result)))

(niwa-deftest niwa-memory-pass--apply/confirmed-files ()
  ,test
  (test)
  :doc "only nonempty confirmed paths count files, deduplicating shared targets"
  (niwa-test-application--fixture
    (let* ((written (niwa-test-application--proposal "written"))
           (deleted (niwa-test-application--proposal "deleted" 'note-delete))
           (empty (niwa-test-application--proposal "empty"))
           (terminal (niwa-test-application--proposal "terminal"))
           (accepted (niwa-test-application--accepted (list written deleted empty terminal)))
           (notifications 0))
      (cl-letf (((symbol-function 'niwa-memory-pass--apply-automatic)
                 (lambda (_workspace _context _pass proposal _note _file notify &optional _continuation)
                   (unless (equal proposal (plist-get terminal :id))
                     (cl-incf notifications)
                     (funcall notify (unless (equal proposal (plist-get empty :id)) '("changed.md"))))
                   (cons 'applied nil)))
                ((symbol-function 'niwa-memory-pass--apply-pending) (lambda (&rest _) '(0 . 0))))
        (let ((result (niwa-memory-pass--apply state accepted)))
          (should (= 3 notifications))
          (should (= 1 (plist-get result :updated-files)))))))
  :doc "held unavailable stale failed and terminal outcomes without notifications count no files"
  (niwa-test-application--fixture
    (dolist (outcome '(held unapplied applied))
      (cl-letf (((symbol-function 'niwa-memory-pass--apply-automatic) (lambda (&rest _) (cons outcome nil)))
                ((symbol-function 'niwa-memory-pass--apply-pending) (lambda (&rest _) '(0 . 0))))
        (should (= 0 (plist-get (niwa-memory-pass--apply
                                state (niwa-test-application--accepted
                                       (list (niwa-test-application--proposal "unwritten")))) :updated-files))))))
  :doc "earlier notifications contribute files without recounting their terminal totals"
  (niwa-test-application--fixture
    (let (observed)
      (cl-letf (((symbol-function 'niwa-memory-pass--apply-pending)
                 (lambda (_workspace skip _held &optional notify _context _continuation)
                   (should (equal "current" skip))
                   (setq observed notify)
                   (when notify (funcall notify '("earlier.md")))
                   '(4 . 2))))
        (let ((result (niwa-memory-pass--apply state (niwa-test-application--accepted nil))))
          (should observed)
          (should (= 4 (plist-get result :applied-count)))
          (should (= 2 (plist-get result :unapplied-count)))
          (should (= 1 (plist-get result :updated-files)))))))
  :doc "pending quit returns confirmed files before or after notification without escaping"
  (dolist (confirmed '(nil t))
    (niwa-test-application--fixture
      (let* ((earlier (niwa-test-application--accepted
                       (list (niwa-test-application--proposal "earlier")
                             (niwa-test-application--proposal "later")) "earlier-pass"))
             (calls 0) result escaped)
        (cl-letf (((symbol-function 'niwa-memory-notes-passes) (lambda (&rest _) (list earlier)))
                  ((symbol-function 'niwa-memory-pass--apply-automatic)
                   (lambda (_workspace _context pass _proposal _note _file &optional notify _continuation)
                     (should (equal "earlier-pass" pass))
                     (cl-incf calls)
                     (when confirmed (funcall notify '("earlier.md")))
                     (cons 'quit nil))))
          (condition-case nil
              (setq result (niwa-memory-pass--apply state (niwa-test-application--accepted nil)))
            (quit (setq escaped t)))
          (should (= 1 calls))
          (should-not escaped)
          (should (plist-get state :stop-apply))
          (should (= (if confirmed 1 0) (plist-get result :updated-files)))))))
  :doc "a later pending quit retains earlier confirmation and never executes the remainder"
  (niwa-test-application--fixture
    (let* ((earlier (niwa-test-application--accepted
                     (mapcar #'niwa-test-application--proposal '("confirmed" "interrupted" "unreached")) "earlier-pass"))
           trace result escaped)
      (cl-letf (((symbol-function 'niwa-memory-notes-passes) (lambda (&rest _) (list earlier)))
                ((symbol-function 'niwa-memory-pass--apply-automatic)
                 (lambda (_workspace _context pass proposal _note _file &optional notify _continuation)
                   (should (equal "earlier-pass" pass))
                   (push proposal trace)
                   (pcase proposal
                     ("confirmed" (funcall notify '("earlier.md")) (cons 'applied nil))
                     ("interrupted" (cons 'quit nil))
                     (_ (ert-fail "Continued after quit"))))))
        (condition-case nil
            (setq result (niwa-memory-pass--apply state (niwa-test-application--accepted nil)))
          (quit (setq escaped t)))
        (should-not escaped)
        (should (equal '("confirmed" "interrupted") (nreverse trace)))
        (should (plist-get state :stop-apply))
        (should (= 1 (plist-get result :updated-files))))))
  :doc "current-pass cancellation counts only prior notifications and holds the remainder"
  (dolist (confirmed '(nil t))
    (niwa-test-application--fixture
      (let ((calls 0))
        (cl-letf (((symbol-function 'niwa-memory-pass--apply-automatic)
                   (lambda (_workspace _context _pass _proposal _note _file notify &optional _continuation)
                     (cl-incf calls)
                     (when confirmed (funcall notify '("confirmed.md")))
                     (cons 'quit nil)))
                  ((symbol-function 'niwa-memory-pass--apply-pending)
                   (lambda (&rest _) (ert-fail "Pending work ran after quit"))))
          (let ((result (niwa-memory-pass--apply
                         state (niwa-test-application--accepted
                                (mapcar #'niwa-test-application--proposal '("first" "later"))))))
            (should (= 1 calls))
            (should (plist-get state :stop-apply))
            (should (= 1 (plist-get result :held-count)))
            (should (= (if confirmed 1 0) (plist-get result :updated-files)))))))))

(niwa-deftest niwa-memory-pass--apply-pending/confirmed-files ()
  ,test
  (test)
  :doc "the observer receives confirmed paths, never terminal-only decisions"
  (niwa-test-application--fixture
    (let* ((proposals (mapcar #'niwa-test-application--proposal '("written" "terminal" "waiting" "failed")))
           (accepted (niwa-test-application--accepted proposals "earlier")) notifications held)
      (cl-letf (((symbol-function 'niwa-memory-notes-passes) (lambda (&rest _) (list accepted)))
                ((symbol-function 'niwa-memory-pass--apply-automatic)
                 (lambda (_workspace _context _pass proposal _note _file &optional notify _continuation)
                   (cons (pcase proposal
                           ("written" (when notify (funcall notify '("written.md"))) 'applied)
                           ("terminal" 'applied) ("waiting" 'held) (_ 'unapplied))
                         nil))))
        (should (equal '(2 . 1) (niwa-memory-pass--apply-pending
                                workspace "current" (lambda (file) (push file held))
                                (lambda (paths) (push paths notifications)))))
        (should (equal '(("written.md")) notifications))
        (should (equal '("waiting.md") held)))))
  :doc "propose mode neither attempts earlier work nor emits write notifications"
  (niwa-test-application--fixture
    (let ((niwa-memory-consolidation-mode 'propose))
      (cl-letf (((symbol-function 'niwa-memory-pass--apply-automatic)
                 (lambda (&rest _) (ert-fail "Propose mode wrote"))))
        (should (equal '(0 . 0) (niwa-memory-pass--apply-pending
                                workspace nil nil (lambda (&rest _) (ert-fail "Propose mode notified")))))))))

(niwa-deftest niwa-memory-decision-pending/identities ()
  (niwa-test-application--fixture
    (let ((accepted (niwa-test-application--accepted
                     (list (niwa-test-application--proposal "note" 'note-delete)
                           (niwa-test-application--proposal "memory")) "earlier")))
      (cl-letf (((symbol-function 'niwa-memory-notes-passes) (lambda (&rest _) (list accepted))))
        (let ((items (plist-get (niwa-memory-decision-pending workspace :skip "current") :items)))
          (should (= 2 (length items)))
          (should (plist-get (car items) :note))
          (should-not (plist-get (cadr items) :note))
          (should (cl-every (lambda (item) (plist-get item :pending)) items)))))))

(niwa-deftest niwa-memory-pass--apply-async/confirmed-files ()
  ,test
  (test)
  :doc "current and earlier confirmations aggregate once, ignoring empty and terminal-only writes"
  (niwa-test-application--fixture
    (let* ((accepted (niwa-test-application--accepted
                      (mapcar #'niwa-test-application--proposal '("first" "ordinary" "empty" "terminal"))))
           (pending '((:pass "earlier" :proposal "pending" :pending t :note nil :file "pending.md")))
           (calls 0)
           (result (niwa-test-application--async
                    state accepted
                    (lambda (proposal notify)
                      (cl-incf calls)
                      (unless (equal proposal "terminal")
                        (funcall notify (unless (equal proposal "empty") '("shared-path.md"))))
                      '(:status applied)) pending)))
      (should (= 5 calls))
      (should (= 1 (plist-get result :updated-files)))))
  :doc "terminal stale unavailable failed and held replies without notifications count no files"
  (dolist (status '(applied rejected reversed stale unavailable failed held))
    (niwa-test-application--fixture
      (let* ((calls 0)
             (result (niwa-test-application--async
                      state (niwa-test-application--accepted (list (niwa-test-application--proposal "unwritten")))
                      (lambda (&rest _) (cl-incf calls) (list :status status)))))
        (should (= 1 calls))
        (should (= 0 (plist-get result :updated-files))))))
  :doc "pre-admission cancellation or propose mode holds all items without applying"
  (dolist (stop '(cancel propose))
    (niwa-test-application--fixture
      (if (eq stop 'cancel) (plist-put state :stop-apply t) (setq niwa-memory-consolidation-mode 'propose))
      (let ((result (niwa-test-application--async
                     state (niwa-test-application--accepted (list (niwa-test-application--proposal "unwritten")))
                     (lambda (&rest _) (ert-fail "Unadmitted job applied")))))
        (should (= 1 (plist-get result :held-count)))
        (should (= 0 (plist-get result :updated-files))))))
  :doc "native cancellation before notification counts nothing but confirmed writes survive"
  (dolist (confirmed '(nil t))
    (niwa-test-application--fixture
      (let* ((calls 0)
             (result (niwa-test-application--async
                      state (niwa-test-application--accepted
                             (mapcar #'niwa-test-application--proposal '("first" "later")))
                      (lambda (_proposal notify)
                        (cl-incf calls)
                        (when confirmed (funcall notify '("confirmed.md")))
                        (signal 'niwa-memory-decision-interrupted
                                (list (and confirmed '(:status applied)) "Fixture cancellation"))))))
        (should (= 1 calls))
        (should (plist-get result :cancelled))
        (should (= (if confirmed 1 0) (plist-get result :updated-files)))))))

(provide 'test-niwa-memory-pass-application-accounting)
;;; test-niwa-memory-pass-application-accounting.el ends here
