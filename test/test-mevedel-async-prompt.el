;;; test-mevedel-async-prompt.el --- Tests for async confirmation overlays -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for spec 20: async prompt primitives, settlement, cancellers,
;; and the RequestAccess dedup waiter list.  The pipeline-level latch,
;; permission async chain, and step-permission outcome translator are
;; covered in test-mevedel-pipeline.el; tests here focus on the UI
;; primitives themselves.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)
(require 'mevedel-tool-ui)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Settlement primitive

(mevedel-deftest mevedel--prompt--settle ()
  ,test
  (test)
  :doc "fires callback exactly once and removes overlay from pending list"
  (with-temp-buffer
    (let* ((received nil)
           (cb (lambda (o) (push o received)))
           (ov (make-overlay (point) (point) nil t)))
      (overlay-put ov 'mevedel--callback cb)
      (overlay-put ov 'mevedel-user-request t)
      (push ov mevedel--prompt-overlays)
      (mevedel--prompt--settle ov 'approve)
      (should (equal received '(approve)))
      (should-not mevedel--prompt-overlays)
      (should-not (overlay-buffer ov))
      ;; Second settle is a no-op — settlement mark prevents double-fire.
      (mevedel--prompt--settle ov 'deny)
      (should (equal received '(approve)))))

  :doc "settlement mark survives nil callback path"
  (with-temp-buffer
    (let ((ov (make-overlay (point) (point) nil t)))
      (overlay-put ov 'mevedel-user-request t)
      (push ov mevedel--prompt-overlays)
      ;; No callback set — must not error, must still mark settled.
      (mevedel--prompt--settle ov 'aborted)
      (should (overlay-get ov 'mevedel-settled))
      (should-not mevedel--prompt-overlays)))

  :doc "exits prompt transient map when prompt settles"
  (with-temp-buffer
    (let ((called nil)
          (ov (make-overlay (point) (point) nil t)))
      (overlay-put ov 'mevedel-user-request t)
      (overlay-put ov 'mevedel--transient-map-exit
                   (lambda () (setq called t)))
      (push ov mevedel--prompt-overlays)
      (mevedel--prompt--settle ov 'approve)
      (should called))))

(mevedel-deftest mevedel--prompt--overlay-at-point
  (:doc "finds prompt overlays even when point is not on the before-string")
  (with-temp-buffer
    (insert "input\n")
    (let ((ov (make-overlay (point-min) (point-min) nil t)))
      (overlay-put ov 'mevedel-permission-prompt t)
      (push ov mevedel--prompt-overlays)
      (goto-char (point-max))
      (should (eq ov (mevedel--prompt--overlay-at-point
                      'mevedel-permission-prompt))))))


;;
;;; Dismiss-all (canceller path)

(mevedel-deftest mevedel--prompt-dismiss-all ()
  ,test
  (test)
  :doc "settles every pending overlay with aborted"
  (with-temp-buffer
    (let* ((received nil)
           (cb (lambda (o) (push o received)))
           (a (make-overlay (point) (point) nil t))
           (b (make-overlay (point) (point) nil t)))
      (dolist (ov (list a b))
        (overlay-put ov 'mevedel--callback cb)
        (overlay-put ov 'mevedel-user-request t)
        (push ov mevedel--prompt-overlays))
      (mevedel--prompt-dismiss-all)
      (should (= 2 (length received)))
      (should (cl-every (lambda (o) (eq o 'aborted)) received))
      (should-not mevedel--prompt-overlays)))

  :doc "is idempotent — second call after empty list is a no-op"
  (with-temp-buffer
    (let* ((received nil)
           (cb (lambda (o) (push o received)))
           (ov (make-overlay (point) (point) nil t)))
      (overlay-put ov 'mevedel--callback cb)
      (overlay-put ov 'mevedel-user-request t)
      (push ov mevedel--prompt-overlays)
      (mevedel--prompt-dismiss-all)
      (mevedel--prompt-dismiss-all)
      (should (equal received '(aborted))))))


;;
;;; Canceller registration

(mevedel-deftest mevedel--prompt--register-canceller ()
  ,test
  (test)
  :doc "first registration pushes a canceller onto the active request"
  (with-temp-buffer
    (let ((session (mevedel-session--create :name "t")))
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request
                  (mevedel-request--create :session session))
      (mevedel--prompt--register-canceller)
      (should (= 1 (length (mevedel-request-cancellers
                            mevedel--current-request))))))

  :doc "second registration in same request does not double-push"
  (with-temp-buffer
    (let ((session (mevedel-session--create :name "t")))
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request
                  (mevedel-request--create :session session))
      (mevedel--prompt--register-canceller)
      (mevedel--prompt--register-canceller)
      (should (= 1 (length (mevedel-request-cancellers
                            mevedel--current-request))))))

  :doc "kill-buffer-hook installs dismiss-all"
  (with-temp-buffer
    (let ((session (mevedel-session--create :name "t")))
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request
                  (mevedel-request--create :session session))
      (mevedel--prompt--register-canceller)
      (should (memq #'mevedel--prompt-dismiss-all
                    (buffer-local-value 'kill-buffer-hook
                                        (current-buffer)))))))


;;
;;; Concurrent overlays (regression for the deadlock bug)

(mevedel-deftest mevedel--prompt-concurrent-overlays
  (:doc "two overlays can settle independently in user-chosen order")
  (with-temp-buffer
    (let* ((received nil)
           (cb-a (lambda (o) (push (cons 'a o) received)))
           (cb-b (lambda (o) (push (cons 'b o) received)))
           (a (make-overlay (point) (point) nil t))
           (b (make-overlay (point) (point) nil t)))
      (overlay-put a 'mevedel--callback cb-a)
      (overlay-put a 'mevedel-user-request t)
      (overlay-put b 'mevedel--callback cb-b)
      (overlay-put b 'mevedel-user-request t)
      (push a mevedel--prompt-overlays)
      (push b mevedel--prompt-overlays)
      ;; Settle B first, then A — order is user-chosen, neither callback
      ;; should leak through the other's overlay.
      (mevedel--prompt--settle b 'deny)
      (mevedel--prompt--settle a 'approve)
      (should (member '(b . deny) received))
      (should (member '(a . approve) received))
      (should-not mevedel--prompt-overlays))))


;;
;;; Buffer-kill teardown

(mevedel-deftest mevedel--prompt-buffer-kill
  (:doc "killing the chat buffer settles every pending overlay with aborted")
  (let* ((received nil)
         (cb (lambda (o) (push o received)))
         (buf (generate-new-buffer " *prompt-kill-test*")))
    (with-current-buffer buf
      (let ((ov (make-overlay (point) (point) nil t)))
        (overlay-put ov 'mevedel--callback cb)
        (overlay-put ov 'mevedel-user-request t)
        (push ov mevedel--prompt-overlays)
        (add-hook 'kill-buffer-hook #'mevedel--prompt-dismiss-all nil t)))
    (kill-buffer buf)
    (should (equal received '(aborted)))))


;;
;;; RequestAccess dedup with concurrent waiters

(mevedel-deftest mevedel-tools--request-access ()
  ,test
  (test)
  :doc "first prompt drives the overlay; later requests register as waiters"
  (with-temp-buffer
    (let ((calls 0))
      (cl-letf (((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (_root _reason _cb) (cl-incf calls)))
                ((symbol-function 'mevedel-add-project-root) #'ignore))
        (mevedel-tools--request-access "/tmp/foo" "" #'ignore)
        (mevedel-tools--request-access "/tmp/foo" "" #'ignore)
        (mevedel-tools--request-access "/tmp/foo" "" #'ignore))
      (should (= 1 calls))
      (let ((entry (assoc "/tmp/foo" mevedel--pending-access-requests
                          #'string=)))
        (should entry)
        (should (eq 'pending (car (cdr entry))))
        ;; Two later callers became waiters; the first caller has its
        ;; callback held by the prompt body, not on the waiters list.
        (should (= 2 (length (cdr (cdr entry))))))))

  :doc "single resolution fans out to every waiter callback"
  (with-temp-buffer
    (let* ((received nil)
           (cb1 (lambda (o) (push (cons 1 o) received)))
           (cb2 (lambda (o) (push (cons 2 o) received)))
           (cb3 (lambda (o) (push (cons 3 o) received)))
           saved-prompt-cb)
      (cl-letf (((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (_root _reason cb) (setq saved-prompt-cb cb)))
                ((symbol-function 'mevedel-add-project-root) #'ignore))
        (mevedel-tools--request-access "/tmp/bar" "" cb1)
        (mevedel-tools--request-access "/tmp/bar" "" cb2)
        (mevedel-tools--request-access "/tmp/bar" "" cb3)
        ;; Resolve the prompt while the cl-letf override is still in
        ;; scope — the inner lambda calls `mevedel-add-project-root'.
        (funcall saved-prompt-cb 'approve)
        (should (= 3 (length received)))
        ;; Each waiter receives the same UI outcome.
        (should (cl-every (lambda (entry) (eq (cdr entry) 'approve)) received))
        ;; Subsequent cache hits see the collapsed status.
        (let ((received-after nil))
          (mevedel-tools--request-access
           "/tmp/bar" "" (lambda (o) (push o received-after)))
          (should (equal received-after '(approve)))))))

  :doc "feedback outcome reaches every waiter and caches as 'deny"
  (with-temp-buffer
    (let* ((received nil)
           (cb1 (lambda (o) (push o received)))
           (cb2 (lambda (o) (push o received)))
           saved-prompt-cb)
      (cl-letf (((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (_root _reason cb) (setq saved-prompt-cb cb)))
                ((symbol-function 'mevedel-add-project-root) #'ignore))
        (mevedel-tools--request-access "/tmp/feed" "" cb1)
        (mevedel-tools--request-access "/tmp/feed" "" cb2)
        (funcall saved-prompt-cb '(feedback . "use git instead"))
        (should (= 2 (length received)))
        (should (cl-every (lambda (o)
                            (and (consp o)
                                 (eq 'feedback (car o))
                                 (equal "use git instead" (cdr o))))
                          received))
        ;; Cache stores collapsed `deny' — feedback text is per-call
        ;; and not replayed for later cache hits.
        (let ((received-after nil))
          (mevedel-tools--request-access
           "/tmp/feed" "" (lambda (o) (push o received-after)))
          (should (equal received-after '(deny)))))))

  :doc "aborted outcome fans out and caches"
  (with-temp-buffer
    (let* ((received nil)
           (cb (lambda (o) (push o received)))
           saved-prompt-cb)
      (cl-letf (((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (_root _reason cb) (setq saved-prompt-cb cb)))
                ((symbol-function 'mevedel-add-project-root) #'ignore))
        (mevedel-tools--request-access "/tmp/abrt" "" cb)
        (mevedel-tools--request-access "/tmp/abrt" "" cb)
        (funcall saved-prompt-cb 'aborted)
        (should (= 2 (length received)))
        (should (cl-every (lambda (o) (eq o 'aborted)) received))
        ;; Cache hit also sees 'aborted, not collapsed away.
        (let ((received-after nil))
          (mevedel-tools--request-access
           "/tmp/abrt" "" (lambda (o) (push o received-after)))
          (should (equal received-after '(aborted)))))))

  :doc "denial fans out as 'deny to every waiter"
  (with-temp-buffer
    (let* ((received nil)
           (cb1 (lambda (o) (push o received)))
           (cb2 (lambda (o) (push o received)))
           saved-prompt-cb)
      (cl-letf (((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (_root _reason cb) (setq saved-prompt-cb cb)))
                ((symbol-function 'mevedel-add-project-root) #'ignore))
        (mevedel-tools--request-access "/tmp/baz" "" cb1)
        (mevedel-tools--request-access "/tmp/baz" "" cb2)
        (funcall saved-prompt-cb 'deny)
        (should (= 2 (length received)))
        (should (cl-every (lambda (o) (eq o 'deny)) received))))))


;;
;;; RequestAccess result formatting

(mevedel-deftest mevedel-tools--request-access--format-result ()
  ,test
  (test)
  :doc "approve produces the grant string"
  (should (string-match-p
           "Access granted to /tmp/x\\."
           (mevedel-tools--request-access--format-result "/tmp/x" 'approve)))
  :doc "deny produces a plain denial string (no feedback)"
  (let ((s (mevedel-tools--request-access--format-result "/tmp/x" 'deny)))
    (should (string-match-p "Access denied to /tmp/x\\." s))
    (should-not (string-match-p "Feedback:" s)))
  :doc "feedback denial preserves the user's text"
  (let ((s (mevedel-tools--request-access--format-result
            "/tmp/x" '(feedback . "use git"))))
    (should (string-match-p "Access denied to /tmp/x" s))
    (should (string-match-p "Feedback: use git" s)))
  :doc "aborted produces the canceller-path tool-result string"
  (should (equal "Error: aborted"
                 (mevedel-tools--request-access--format-result
                  "/tmp/x" 'aborted))))


(provide 'test-mevedel-async-prompt)
;;; test-mevedel-async-prompt.el ends here
