;;; test-mevedel-memory-pass-application-accounting.el --- Confirmed application counts -*- lexical-binding: t -*-

;;; Commentary:
;; Bookkeeping fixtures inject decision outcomes at the public decision verbs,
;; not accepted storage proofs.  Synchronous pending selection keeps its real
;; owned recovery; checked target writes are covered by the decision suites.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-journal-claim)
(require 'mevedel-journal-store)
(require 'mevedel-journal-worker)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-store)
(require 'mevedel-workspace-identity)

(defun mevedel-test-application--proposal (id &optional action)
  "Return bookkeeping metadata for ID with ACTION, defaulting to update."
  (list :id id :action (or action 'update) :file (concat id ".md")))

(defun mevedel-test-application--accepted (proposals &optional pass mode)
  "Wrap PROPOSALS in bookkeeping metadata for PASS with frozen MODE."
  (list :prepared (list :id (or pass "current")
                        :scope (list :roots (list (cons "notes" (list :kind 'notes :mode (or mode 'auto))))))
        :proposals proposals))

(defmacro mevedel-test-application--fixture (&rest body)
  "Run BODY with a private WORKSPACE, native identity and application STATE."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-application-count-" t))
          (workspace (mevedel-workspace--create :root root))
          (mevedel-memory-dirs nil)
          (mevedel-memory-consolidation-mode 'auto)
          (mevedel-journal-worker--child-p t)
          (mevedel-memory-pass--running (make-hash-table :test #'equal))
          (mevedel-memory-pass--deferred (make-hash-table :test #'eq))
          (mevedel-memory-decision--operations (make-hash-table :test #'equal))
          (state (list :workspace workspace :origin-buffer (current-buffer) :settled t))
          (_identity (mevedel-workspace-identity-ensure root)))
     (unwind-protect
         (progn ,@body)
       (when-let* ((job (plist-get state :application)))
         (plist-put job :done t)
         (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
       (maphash (lambda (_ operation) (mevedel-memory-decision--operation-stop operation))
                mevedel-memory-decision--operations)
       (maphash (lambda (_ retry) (cancel-timer (plist-get retry :timer))) mevedel-memory-pass--deferred)
       (delete-directory root t))))

(defun mevedel-test-application--async (state accepted decide &optional pending)
  "Run STATE and ACCEPTED with DECIDE outcomes and PENDING identities.
DECIDE receives a proposal identity and its confirmation callback and returns
the decision result; `mevedel-memory-decision-interrupted' becomes a
cancellation result carrying its accounted status.  Drive a bounded sequence
directly through the public automatic decision verbs."
  (let ((callbacks 0) result job)
    (cl-letf (((symbol-function 'mevedel-memory-decision-apply)
               (cl-function
                (lambda (_workspace _pass proposal &key automatic on-applied callback &allow-other-keys)
                  (should automatic)
                  (should callback)
                  (funcall callback
                           (condition-case err (funcall decide proposal on-applied)
                             (mevedel-memory-decision-interrupted
                              (list :status (plist-get (cadr err) :status) :cancelled t :reason (caddr err))))))))
              ((symbol-function 'mevedel-memory-decision-pending)
               (cl-function (lambda (_workspace &key automatic callback &allow-other-keys)
                              (should automatic)
                              (funcall callback (list :items pending)))))
              ((symbol-function 'mevedel-memory-pass--application-next) #'ignore))
      (setq job (mevedel-memory-pass--apply-async
                 state accepted (lambda (value) (cl-incf callbacks) (setq result value))))
      (dotimes (_ 20)
        (unless (plist-get job :done) (mevedel-memory-pass--application-step state job)))
      (should (plist-get job :done))
      (should (= 1 callbacks))
      (mevedel-memory-pass--application-step state job)
      (should (= 1 callbacks))
      (should-not (plist-get result :error))
      (should (= 0 (hash-table-count mevedel-memory-decision--operations)))
      result)))

(mevedel-deftest mevedel-memory-pass--apply/confirmed-files ()
  ,test
  (test)
  :doc "only nonempty confirmed paths count files, deduplicating shared targets"
  (mevedel-test-application--fixture
    (let* ((written (mevedel-test-application--proposal "written"))
           (deleted (mevedel-test-application--proposal "deleted" 'note-delete))
           (empty (mevedel-test-application--proposal "empty"))
           (terminal (mevedel-test-application--proposal "terminal"))
           (accepted (mevedel-test-application--accepted (list written deleted empty terminal)))
           (notifications 0))
      (cl-letf (((symbol-function 'mevedel-memory-pass--apply-automatic)
                 (lambda (_workspace _context _pass proposal _note _file notify &optional _continuation)
                   (unless (equal proposal (plist-get terminal :id))
                     (cl-incf notifications)
                     (funcall notify (unless (equal proposal (plist-get empty :id)) '("changed.md"))))
                   (cons 'applied nil)))
                ((symbol-function 'mevedel-memory-pass--apply-pending) (lambda (&rest _) '(0 . 0))))
        (let ((result (mevedel-memory-pass--apply state accepted)))
          (should (= 3 notifications))
          (should (= 1 (plist-get result :updated-files)))))))
  :doc "held unavailable stale failed and terminal outcomes without notifications count no files"
  (mevedel-test-application--fixture
    (dolist (outcome '(held unapplied applied))
      (cl-letf (((symbol-function 'mevedel-memory-pass--apply-automatic) (lambda (&rest _) (cons outcome nil)))
                ((symbol-function 'mevedel-memory-pass--apply-pending) (lambda (&rest _) '(0 . 0))))
        (should (= 0 (plist-get (mevedel-memory-pass--apply
                                state (mevedel-test-application--accepted
                                       (list (mevedel-test-application--proposal "unwritten")))) :updated-files))))))
  :doc "earlier notifications contribute files without recounting their terminal totals"
  (mevedel-test-application--fixture
    (let (observed)
      (cl-letf (((symbol-function 'mevedel-memory-pass--apply-pending)
                 (lambda (_workspace skip _held &optional notify _context _continuation)
                   (should (equal "current" skip))
                   (setq observed notify)
                   (when notify (funcall notify '("earlier.md")))
                   '(4 . 2))))
        (let ((result (mevedel-memory-pass--apply state (mevedel-test-application--accepted nil))))
          (should observed)
          (should (= 4 (plist-get result :applied-count)))
          (should (= 2 (plist-get result :unapplied-count)))
          (should (= 1 (plist-get result :updated-files)))))))
  :doc "pending quit returns confirmed files before or after notification without escaping"
  (dolist (confirmed '(nil t))
    (mevedel-test-application--fixture
      (let* ((earlier (mevedel-test-application--accepted
                       (list (mevedel-test-application--proposal "earlier")
                             (mevedel-test-application--proposal "later")) "earlier-pass"))
             (calls 0) result escaped)
        (cl-letf (((symbol-function 'mevedel-memory-notes-passes) (lambda (&rest _) (list earlier)))
                  ((symbol-function 'mevedel-memory-pass--apply-automatic)
                   (lambda (_workspace _context pass _proposal _note _file &optional notify _continuation)
                     (should (equal "earlier-pass" pass))
                     (cl-incf calls)
                     (when confirmed (funcall notify '("earlier.md")))
                     (cons 'quit nil))))
          (condition-case nil
              (setq result (mevedel-memory-pass--apply state (mevedel-test-application--accepted nil)))
            (quit (setq escaped t)))
          (should (= 1 calls))
          (should-not escaped)
          (should (plist-get state :stop-apply))
          (should (= (if confirmed 1 0) (plist-get result :updated-files)))))))
  :doc "a later pending quit retains earlier confirmation and never executes the remainder"
  (mevedel-test-application--fixture
    (let* ((earlier (mevedel-test-application--accepted
                     (mapcar #'mevedel-test-application--proposal '("confirmed" "interrupted" "unreached")) "earlier-pass"))
           trace result escaped)
      (cl-letf (((symbol-function 'mevedel-memory-notes-passes) (lambda (&rest _) (list earlier)))
                ((symbol-function 'mevedel-memory-pass--apply-automatic)
                 (lambda (_workspace _context pass proposal _note _file &optional notify _continuation)
                   (should (equal "earlier-pass" pass))
                   (push proposal trace)
                   (pcase proposal
                     ("confirmed" (funcall notify '("earlier.md")) (cons 'applied nil))
                     ("interrupted" (cons 'quit nil))
                     (_ (ert-fail "Continued after quit"))))))
        (condition-case nil
            (setq result (mevedel-memory-pass--apply state (mevedel-test-application--accepted nil)))
          (quit (setq escaped t)))
        (should-not escaped)
        (should (equal '("confirmed" "interrupted") (nreverse trace)))
        (should (plist-get state :stop-apply))
        (should (= 1 (plist-get result :updated-files))))))
  :doc "current-pass cancellation counts only prior notifications and holds the remainder"
  (dolist (confirmed '(nil t))
    (mevedel-test-application--fixture
      (let ((calls 0))
        (cl-letf (((symbol-function 'mevedel-memory-pass--apply-automatic)
                   (lambda (_workspace _context _pass _proposal _note _file notify &optional _continuation)
                     (cl-incf calls)
                     (when confirmed (funcall notify '("confirmed.md")))
                     (cons 'quit nil)))
                  ((symbol-function 'mevedel-memory-pass--apply-pending)
                   (lambda (&rest _) (ert-fail "Pending work ran after quit"))))
          (let ((result (mevedel-memory-pass--apply
                         state (mevedel-test-application--accepted
                                (mapcar #'mevedel-test-application--proposal '("first" "later"))))))
            (should (= 1 calls))
            (should (plist-get state :stop-apply))
            (should (= 1 (plist-get result :held-count)))
            (should (= (if confirmed 1 0) (plist-get result :updated-files)))))))))

(mevedel-deftest mevedel-memory-pass--apply-pending/confirmed-files ()
  ,test
  (test)
  :doc "the observer receives confirmed paths, never terminal-only decisions"
  (mevedel-test-application--fixture
    (let* ((proposals (mapcar #'mevedel-test-application--proposal '("written" "terminal" "waiting" "failed")))
           (accepted (mevedel-test-application--accepted proposals "earlier")) notifications held)
      (cl-letf (((symbol-function 'mevedel-memory-notes-passes) (lambda (&rest _) (list accepted)))
                ((symbol-function 'mevedel-memory-pass--apply-automatic)
                 (lambda (_workspace _context _pass proposal _note _file &optional notify _continuation)
                   (cons (pcase proposal
                           ("written" (when notify (funcall notify '("written.md"))) 'applied)
                           ("terminal" 'applied) ("waiting" 'held) (_ 'unapplied))
                         nil))))
        (should (equal '(2 . 1) (mevedel-memory-pass--apply-pending
                                workspace "current" (lambda (file) (push file held))
                                (lambda (paths) (push paths notifications)))))
        (should (equal '(("written.md")) notifications))
        (should (equal '("waiting.md") held)))))
  :doc "propose mode neither attempts earlier work nor emits write notifications"
  (mevedel-test-application--fixture
    (let ((mevedel-memory-consolidation-mode 'propose))
      (cl-letf (((symbol-function 'mevedel-memory-pass--apply-automatic)
                 (lambda (&rest _) (ert-fail "Propose mode wrote"))))
        (should (equal '(0 . 0) (mevedel-memory-pass--apply-pending
                                workspace nil nil (lambda (&rest _) (ert-fail "Propose mode notified")))))))))

(mevedel-deftest mevedel-memory-decision-pending/identities ()
  (mevedel-test-application--fixture
    (let ((accepted (mevedel-test-application--accepted
                     (list (mevedel-test-application--proposal "note" 'note-delete)
                           (mevedel-test-application--proposal "memory")) "earlier")))
      (cl-letf (((symbol-function 'mevedel-memory-notes-passes) (lambda (&rest _) (list accepted))))
        (let ((items (plist-get (mevedel-memory-decision-pending workspace :skip "current") :items)))
          (should (= 2 (length items)))
          (should (plist-get (car items) :note))
          (should-not (plist-get (cadr items) :note))
          (should (cl-every (lambda (item) (plist-get item :pending)) items)))))))

(mevedel-deftest mevedel-memory-pass--apply-async/confirmed-files ()
  ,test
  (test)
  :doc "current and earlier confirmations aggregate once, ignoring empty and terminal-only writes"
  (mevedel-test-application--fixture
    (let* ((accepted (mevedel-test-application--accepted
                      (mapcar #'mevedel-test-application--proposal '("first" "ordinary" "empty" "terminal"))))
           (pending '((:pass "earlier" :proposal "pending" :pending t :note nil :file "pending.md")))
           (calls 0)
           (result (mevedel-test-application--async
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
    (mevedel-test-application--fixture
      (let* ((calls 0)
             (result (mevedel-test-application--async
                      state (mevedel-test-application--accepted (list (mevedel-test-application--proposal "unwritten")))
                      (lambda (&rest _) (cl-incf calls) (list :status status)))))
        (should (= 1 calls))
        (should (= 0 (plist-get result :updated-files))))))
  :doc "pre-admission cancellation or propose mode holds all items without applying"
  (dolist (stop '(cancel propose))
    (mevedel-test-application--fixture
      (if (eq stop 'cancel) (plist-put state :stop-apply t) (setq mevedel-memory-consolidation-mode 'propose))
      (let ((result (mevedel-test-application--async
                     state (mevedel-test-application--accepted (list (mevedel-test-application--proposal "unwritten")))
                     (lambda (&rest _) (ert-fail "Unadmitted job applied")))))
        (should (= 1 (plist-get result :held-count)))
        (should (= 0 (plist-get result :updated-files))))))
  :doc "native cancellation before notification counts nothing but confirmed writes survive"
  (dolist (confirmed '(nil t))
    (mevedel-test-application--fixture
      (let* ((calls 0)
             (result (mevedel-test-application--async
                      state (mevedel-test-application--accepted
                             (mapcar #'mevedel-test-application--proposal '("first" "later")))
                      (lambda (_proposal notify)
                        (cl-incf calls)
                        (when confirmed (funcall notify '("confirmed.md")))
                        (signal 'mevedel-memory-decision-interrupted
                                (list (and confirmed '(:status applied)) "Fixture cancellation"))))))
        (should (= 1 calls))
        (should (plist-get result :cancelled))
        (should (= (if confirmed 1 0) (plist-get result :updated-files)))))))

(provide 'test-mevedel-memory-pass-application-accounting)
;;; test-mevedel-memory-pass-application-accounting.el ends here
