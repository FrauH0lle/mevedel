;;; test-mevedel-memory-pass-preservation-count.el --- Confirmed preservation counts -*- lexical-binding: t -*-

;;; Commentary:
;; Bookkeeping tests inject application outcomes, not accepted storage proofs.
;; Async cases retain native actor admission, recovery and claim ownership, using
;; the synchronous worker fallback.  Real acceptance and note writes are covered
;; by the pass-notes and application-worker suites.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-journal-claim)
(require 'mevedel-journal-store)
(require 'mevedel-journal-worker)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-store)
(require 'mevedel-workspace-identity)

(defun mevedel-test-preservation--proposal (id action preserved)
  "Return bookkeeping metadata for ID with ACTION and PRESERVED marker."
  (list :id id :action action :guard-preserved preserved
        :file (concat id ".md")))

(defun mevedel-test-preservation--accepted (proposals &optional pass mode)
  "Wrap PROPOSALS in bookkeeping metadata for PASS with frozen MODE."
  (list :prepared (list :id (or pass "current")
                        :scope (list :roots (list (cons "notes" (list :kind 'notes :mode (or mode 'auto))))))
        :proposals proposals))

(defmacro mevedel-test-preservation--fixture (&rest body)
  "Run BODY with a private WORKSPACE, native identity and application STATE."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-preservation-count-" t))
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
       (maphash (lambda (_ retry) (cancel-timer (plist-get retry :timer)))
                mevedel-memory-pass--deferred)
       (delete-directory root t))))

(defun mevedel-test-preservation--async (state accepted apply-owned &optional pending)
  "Run STATE and ACCEPTED with APPLY-OWNED outcomes and PENDING identities.
Drive a bounded sequence directly instead of scheduling continuation timers.
The native operation wrapper still acquires, authenticates and settles claims."
  (let ((callbacks 0) result job)
    (cl-letf (((symbol-function 'mevedel-memory-decision--apply-owned) apply-owned)
              ((symbol-function 'mevedel-memory-pass--pending-items)
               (lambda (&rest _) pending))
              ((symbol-function 'mevedel-memory-pass--application-next) #'ignore))
      (setq job (mevedel-memory-pass--apply-async
                 state accepted (lambda (value) (cl-incf callbacks) (setq result value))))
      (dotimes (_ 20)
        (unless (plist-get job :done)
          (mevedel-memory-pass--application-step state job)))
      (should (plist-get job :done))
      (should (= 1 callbacks))
      ;; A stale continuation must not apply or report the completed job again.
      (mevedel-memory-pass--application-step state job)
      (should (= 1 callbacks))
      (should-not (plist-get result :error))
      (should-not (> (hash-table-count mevedel-memory-decision--operations) 0))
      result)))

(mevedel-deftest mevedel-memory-pass--apply/preservation-count ()
  ,preservation-count
  (preservation-count)
  :doc "only a notified preserved note-condense with nonempty paths earns credit"
  (mevedel-test-preservation--fixture
    (let* ((preserved (mevedel-test-preservation--proposal "preserved" 'note-condense t))
           (ordinary (mevedel-test-preservation--proposal "ordinary" 'note-condense nil))
           (keep (mevedel-test-preservation--proposal "keep" 'note-keep t))
           (memory (mevedel-test-preservation--proposal "memory" 'update t))
           (empty (mevedel-test-preservation--proposal "empty" 'note-condense t))
           (terminal (mevedel-test-preservation--proposal "terminal" 'note-condense t))
           (accepted (mevedel-test-preservation--accepted
                      (list preserved ordinary keep memory empty terminal)))
           (notifications 0))
      (cl-letf (((symbol-function 'mevedel-memory-pass--apply-automatic)
                 (lambda (_workspace _pass proposal notify)
                   (unless (eq proposal terminal)
                     (cl-incf notifications)
                     (funcall notify (unless (eq proposal empty) '("changed.md"))))
                   'applied))
                ((symbol-function 'mevedel-memory-pass--apply-pending)
                 (lambda (&rest _) '(0 . 0))))
        (let ((result (mevedel-memory-pass--apply state accepted)))
          (should (= 5 notifications))
          (should (= 1 (plist-get result :updated-files)))
          (should (= 1 (plist-get result :guard-preserved-condense-count)))))))

  :doc "held unavailable stale failed and generic terminal outcomes earn no credit"
  (mevedel-test-preservation--fixture
    (dolist (outcome '(held unapplied applied))
      (cl-letf (((symbol-function 'mevedel-memory-pass--apply-automatic)
                 (lambda (&rest _) outcome))
                ((symbol-function 'mevedel-memory-pass--apply-pending)
                 (lambda (&rest _) '(0 . 0))))
        (let ((result (mevedel-memory-pass--apply
                       state (mevedel-test-preservation--accepted
                              (list (mevedel-test-preservation--proposal "unwritten" 'note-condense t))))))
          (should (= 0 (plist-get result :guard-preserved-condense-count)))
          (should (= 0 (plist-get result :updated-files)))))))

  :doc "earlier pending notifications contribute once without counting their terminal totals"
  (mevedel-test-preservation--fixture
    (let ((proposal (mevedel-test-preservation--proposal "earlier" 'note-condense t)) observed)
      (cl-letf (((symbol-function 'mevedel-memory-pass--apply-pending)
                 (lambda (_workspace skip _held &optional notify)
                   (should (equal "current" skip))
                   (setq observed notify)
                   (when notify (funcall notify proposal '("earlier.md")))
                   '(4 . 2))))
        (let ((result (mevedel-memory-pass--apply state (mevedel-test-preservation--accepted nil))))
          (should observed)
          (should (= 4 (plist-get result :applied-count)))
          (should (= 2 (plist-get result :unapplied-count)))
          (should (= 1 (plist-get result :guard-preserved-condense-count)))))))

  :doc "earlier pending quit returns accounting before or after its confirmed-write notification"
  (dolist (confirmed '(nil t))
    (mevedel-test-preservation--fixture
      (let* ((earlier (mevedel-test-preservation--accepted
                       (list (mevedel-test-preservation--proposal "earlier" 'note-condense t)
                             (mevedel-test-preservation--proposal "later" 'note-condense t))
                       "earlier-pass"))
             (calls 0) result escaped)
        (cl-letf (((symbol-function 'mevedel-memory-notes-passes)
                   (lambda (&rest _) (list earlier)))
                  ((symbol-function 'mevedel-memory-pass--apply-automatic)
                   (lambda (_workspace pass _proposal &optional notify)
                     (should (equal "earlier-pass" pass))
                     (cl-incf calls)
                     (when confirmed (funcall notify '("earlier.md")))
                     'quit)))
          ;; The real pending loop raises quit, unlike current-pass outcomes.
          ;; The outer application boundary must still return confirmed credit.
          (condition-case nil
              (setq result (mevedel-memory-pass--apply
                            state (mevedel-test-preservation--accepted nil)))
            (quit (setq escaped t)))
          (should (= 1 calls))
          (should-not escaped)
          (should (plist-get state :stop-apply))
          (should (= (if confirmed 1 0) (plist-get result :updated-files)))
          (should (= (if confirmed 1 0) (plist-get result :guard-preserved-condense-count)))))))

  :doc "earlier pending confirmed credit survives a later proposal quit without executing the remainder"
  (mevedel-test-preservation--fixture
    (let* ((earlier (mevedel-test-preservation--accepted
                     (list (mevedel-test-preservation--proposal "confirmed" 'note-condense t)
                           (mevedel-test-preservation--proposal "interrupted" 'note-condense t)
                           (mevedel-test-preservation--proposal "unreached" 'note-condense t))
                     "earlier-pass"))
           trace result escaped)
      (cl-letf (((symbol-function 'mevedel-memory-notes-passes)
                 (lambda (&rest _) (list earlier)))
                ((symbol-function 'mevedel-memory-pass--apply-automatic)
                 (lambda (_workspace pass proposal &optional notify)
                   (should (equal "earlier-pass" pass))
                   (push (plist-get proposal :id) trace)
                   (pcase (plist-get proposal :id)
                     ("confirmed" (funcall notify '("earlier.md")) 'applied)
                     ("interrupted" 'quit)
                     (_ (ert-fail "Pending work continued after a later quit"))))))
        (condition-case nil
            (setq result (mevedel-memory-pass--apply
                          state (mevedel-test-preservation--accepted nil)))
          (quit (setq escaped t)))
        (should-not escaped)
        (should (equal '("confirmed" "interrupted") (nreverse trace)))
        (should (plist-get state :stop-apply))
        (should (= 1 (plist-get result :updated-files)))
        (should (= 1 (plist-get result :guard-preserved-condense-count))))))

  :doc "cancellation before confirmation earns nothing but a confirmed write keeps credit"
  (dolist (confirmed '(nil t))
    (mevedel-test-preservation--fixture
      (let ((calls 0))
        (cl-letf (((symbol-function 'mevedel-memory-pass--apply-automatic)
                   (lambda (_workspace _pass _proposal notify)
                     (cl-incf calls)
                     (when confirmed (funcall notify '("confirmed.md")))
                     'quit))
                  ((symbol-function 'mevedel-memory-pass--apply-pending)
                   (lambda (&rest _) (ert-fail "Pending work ran after cancellation"))))
          (let ((result (mevedel-memory-pass--apply
                         state (mevedel-test-preservation--accepted
                                (list (mevedel-test-preservation--proposal "first" 'note-condense t)
                                      (mevedel-test-preservation--proposal "later" 'note-condense t))))))
            (should (= 1 calls))
            (should (plist-get state :stop-apply))
            (should (= 1 (plist-get result :held-count)))
            (should (= (if confirmed 1 0) (plist-get result :guard-preserved-condense-count)))))))))

(mevedel-deftest mevedel-memory-pass--apply-pending/preservation-observer ()
  ,preservation-observer
  (preservation-observer)
  :doc "the optional observer receives proposal and actual paths, not terminal-only decisions"
  (mevedel-test-preservation--fixture
    (let* ((written (mevedel-test-preservation--proposal "written" 'note-condense t))
           (terminal (mevedel-test-preservation--proposal "terminal" 'note-condense t))
           (waiting (mevedel-test-preservation--proposal "waiting" 'note-condense t))
           (failed (mevedel-test-preservation--proposal "failed" 'note-condense t))
           (accepted (mevedel-test-preservation--accepted (list written terminal waiting failed) "earlier"))
           notifications held)
      (cl-letf (((symbol-function 'mevedel-memory-notes-passes) (lambda (&rest _) (list accepted)))
                ((symbol-function 'mevedel-memory-pass--apply-automatic)
                 (lambda (_workspace _pass proposal &optional notify)
                   (cond ((eq proposal written) (when notify (funcall notify '("written.md"))) 'applied)
                         ((eq proposal terminal) 'applied)
                         ((eq proposal waiting) 'held)
                         (t 'unapplied)))))
        (should (equal '(2 . 1)
                       (mevedel-memory-pass--apply-pending
                        workspace "current" (lambda (file) (push file held))
                        (lambda (proposal paths) (push (list proposal paths) notifications)))))
        (should (equal (list (list written '("written.md"))) notifications))
        (should (equal '("waiting.md") held)))))

  :doc "propose mode neither attempts earlier work nor emits application notifications"
  (mevedel-test-preservation--fixture
    (let ((mevedel-memory-consolidation-mode 'propose))
      (cl-letf (((symbol-function 'mevedel-memory-pass--apply-automatic)
                 (lambda (&rest _) (ert-fail "Propose mode attempted a write"))))
        (should (equal '(0 . 0)
                       (mevedel-memory-pass--apply-pending
                        workspace nil nil (lambda (&rest _) (ert-fail "Propose mode notified a write")))))))))

(mevedel-deftest mevedel-memory-pass--pending-items/preservation-metadata ()
  ,preservation-metadata
  (preservation-metadata)
  :doc "pending identities carry preservation only for marked note-condenses"
  (mevedel-test-preservation--fixture
    (let ((accepted (mevedel-test-preservation--accepted
                     (list (mevedel-test-preservation--proposal "preserved" 'note-condense t)
                           (mevedel-test-preservation--proposal "ordinary" 'note-condense nil)
                           (mevedel-test-preservation--proposal "keep" 'note-keep t)) "earlier")))
      (cl-letf (((symbol-function 'mevedel-memory-notes-passes) (lambda (&rest _) (list accepted))))
        (let ((items (mevedel-memory-pass--pending-items workspace "current")))
          (should (= 3 (length items)))
          (should (plist-get (car items) :preserved-condense))
          (should-not (plist-get (cadr items) :preserved-condense))
          (should-not (plist-get (caddr items) :preserved-condense))
          (should (cl-every (lambda (item) (plist-get item :pending)) items)))))))

(mevedel-deftest mevedel-memory-pass--apply-async/preservation-count ()
  ,preservation-count
  (preservation-count)
  :doc "current and earlier pending notifications aggregate once, excluding unmarked and empty writes"
  (mevedel-test-preservation--fixture
    (let* ((accepted (mevedel-test-preservation--accepted
                      (list (mevedel-test-preservation--proposal "preserved" 'note-condense t)
                            (mevedel-test-preservation--proposal "ordinary" 'note-condense nil)
                            (mevedel-test-preservation--proposal "keep" 'note-keep t)
                            (mevedel-test-preservation--proposal "empty" 'note-condense t)
                            (mevedel-test-preservation--proposal "terminal" 'note-condense t))))
           (pending '((:pass "earlier" :proposal "pending" :pending t :note t
                       :file "pending.md" :preserved-condense t)))
           (calls 0)
           (result (mevedel-test-preservation--async
                    state accepted
                    (lambda (_workspace claim _pass proposal notify currentp &optional _pending)
                      (should (funcall currentp))
                      (mevedel-memory-store--assert-owned claim)
                      (cl-incf calls)
                      (unless (equal proposal "terminal")
                        (funcall notify (unless (equal proposal "empty") '("shared-path.md"))))
                      '(:status applied))
                    pending)))
      (should (= 6 calls))
      (should (= 1 (plist-get result :updated-files)))
      (should (= 2 (plist-get result :guard-preserved-condense-count)))))

  :doc "terminal stale unavailable failed and held replies without notifications earn no credit"
  (dolist (status '(applied rejected reversed stale unavailable failed held))
    (mevedel-test-preservation--fixture
      (let ((calls 0))
        (let ((result (mevedel-test-preservation--async
                       state (mevedel-test-preservation--accepted
                              (list (mevedel-test-preservation--proposal "unwritten" 'note-condense t)))
                       (lambda (&rest _) (cl-incf calls) (list :status status)))))
          (should (= 1 calls))
          (should (= 0 (plist-get result :updated-files)))
          (should (= 0 (plist-get result :guard-preserved-condense-count)))))))

  :doc "pre-admission cancellation or propose mode holds all items without entering application"
  (dolist (stop '(cancel propose))
    (mevedel-test-preservation--fixture
      (if (eq stop 'cancel) (plist-put state :stop-apply t)
        (setq mevedel-memory-consolidation-mode 'propose))
      (let ((result (mevedel-test-preservation--async
                     state (mevedel-test-preservation--accepted
                            (list (mevedel-test-preservation--proposal "unwritten" 'note-condense t)))
                     (lambda (&rest _) (ert-fail "Unadmitted job entered application")))))
        (should (= 1 (plist-get result :held-count)))
        (should (= 0 (plist-get result :guard-preserved-condense-count))))))

  :doc "native cancellation before notification earns nothing; after confirmation it retains one credit"
  (dolist (confirmed '(nil t))
    (mevedel-test-preservation--fixture
      (let ((calls 0))
        (let ((result (mevedel-test-preservation--async
                       state (mevedel-test-preservation--accepted
                              (list (mevedel-test-preservation--proposal "first" 'note-condense t)
                                    (mevedel-test-preservation--proposal "later" 'note-condense t)))
                       (lambda (_workspace _claim _pass _proposal notify currentp &optional _pending)
                         (should (funcall currentp))
                         (cl-incf calls)
                         (when confirmed (funcall notify '("confirmed.md")))
                         (signal 'mevedel-memory-decision-interrupted
                                 (list (and confirmed '(:status applied)) "Fixture cancellation"))))))
          (should (= 1 calls))
          (should (plist-get result :cancelled))
          (should (= (if confirmed 1 0) (plist-get result :updated-files)))
          (should (= (if confirmed 1 0) (plist-get result :guard-preserved-condense-count))))))))

(provide 'test-mevedel-memory-pass-preservation-count)
;;; test-mevedel-memory-pass-preservation-count.el ends here
