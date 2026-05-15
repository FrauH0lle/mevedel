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
(require 'mevedel-view)
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

  :doc "removes interaction descriptor when prompt settles"
  (with-temp-buffer
    (let* ((mevedel-view--interaction-descriptors
            (make-hash-table :test #'equal))
           (mevedel-view--interaction-overlays
            (make-hash-table :test #'equal))
           (id '(:permission test))
           (ov (make-overlay (point) (point) nil t)))
      (overlay-put ov 'mevedel-user-request t)
      (overlay-put ov 'mevedel-view-interaction-id id)
      (puthash id ov mevedel-view--interaction-overlays)
      (puthash id (list :id id) mevedel-view--interaction-descriptors)
      (push ov mevedel--prompt-overlays)
      (mevedel--prompt--settle ov 'approve)
      (should-not (gethash id mevedel-view--interaction-descriptors))
      (should-not (gethash id mevedel-view--interaction-overlays))))

  :doc "deleted interaction overlay activation does not fire callback"
  (with-temp-buffer
    (let* ((mevedel-view--interaction-overlays
            (make-hash-table :test #'equal))
           (id '(:permission stale))
           (received nil)
           (ov (make-overlay (point) (point) nil t)))
      (overlay-put ov 'mevedel-user-request t)
      (overlay-put ov 'mevedel-view-interaction-id id)
      (overlay-put ov 'mevedel--callback
                   (lambda (outcome) (push outcome received)))
      (puthash id ov mevedel-view--interaction-overlays)
      (delete-overlay ov)
      (mevedel--prompt--settle ov 'approve)
      (should-not received))))

(mevedel-deftest mevedel--prompt--overlay-at-point
  (:doc "finds prompt overlays at point")
  (with-temp-buffer
    (insert "input\n")
    (goto-char (point-min))
    (let ((ov (make-overlay (point-min) (point-max) nil t)))
      (overlay-put ov 'mevedel-permission-prompt t)
      (should (eq ov (mevedel--prompt--overlay-at-point
                      'mevedel-permission-prompt))))))

(mevedel-deftest mevedel--prompt--overlay-at-point/text-property
  (:doc "finds materialized interaction overlays via text property")
  (with-temp-buffer
    (insert "permission body\n")
    (let ((ov (make-overlay (point-min) (point-max) nil t)))
      (overlay-put ov 'mevedel-permission-prompt t)
      (add-text-properties (point-min) (point-max)
                           (list 'mevedel-view-interaction-overlay ov))
      (goto-char (point-min))
      (should (eq ov (mevedel--prompt--overlay-at-point
                      'mevedel-permission-prompt))))))

(mevedel-deftest mevedel-permission--prompt-commands ()
  ,test
  (test)
  :doc "settle the interaction-zone permission prompt from its real text"
  (with-temp-buffer
    (let* ((mevedel-view--interaction-descriptors
            (make-hash-table :test #'equal))
           (mevedel-view--interaction-overlays
            (make-hash-table :test #'equal))
           (received nil)
           (id '(:permission real-text)))
      (insert "interaction\n\n> ")
      (let ((ov (make-overlay (point-min) (1+ (point-min)) nil t)))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-view-interaction-id id)
        (overlay-put ov 'priority 100)
        (overlay-put ov 'mevedel--callback
                     (lambda (outcome) (setq received outcome)))
        (push ov mevedel--prompt-overlays)
        (puthash id ov mevedel-view--interaction-overlays)
        (puthash id (list :id id) mevedel-view--interaction-descriptors)
        (goto-char (point-min))
        (let ((last-command-event ?a))
          (call-interactively #'mevedel-permission--prompt-approve-once))
        (should (eq received 'allow-once))
        (should-not (gethash id mevedel-view--interaction-overlays)))))

  :doc "fall back to normal typing when no permission prompt is active"
  (with-temp-buffer
    (let ((last-command-event ?a))
      (call-interactively #'mevedel-permission--prompt-approve-once))
    (should (equal (buffer-string) "a")))

  :doc "view-mode binds permission shortcuts for interaction overlays"
  (should (eq (lookup-key mevedel-view-mode-map "a")
              #'mevedel-permission--prompt-approve-once))
  (should (eq (lookup-key mevedel-view-mode-map "s")
              #'mevedel-permission--prompt-approve-session))
  (should (eq (lookup-key mevedel-view-mode-map "f")
              #'mevedel-permission--prompt-feedback)))


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
                ((symbol-function 'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (&rest _) nil))
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
                ((symbol-function 'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (&rest _) nil))
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
                ((symbol-function 'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (&rest _) nil))
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
                ((symbol-function 'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (&rest _) nil))
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
                ((symbol-function 'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-add-project-root) #'ignore))
        (mevedel-tools--request-access "/tmp/baz" "" cb1)
        (mevedel-tools--request-access "/tmp/baz" "" cb2)
        (funcall saved-prompt-cb 'deny)
        (should (= 2 (length received)))
        (should (cl-every (lambda (o) (eq o 'deny)) received)))))

  :doc "already allowed roots approve without opening a prompt"
  (with-temp-buffer
    (let ((calls 0)
          received)
      (cl-letf (((symbol-function 'mevedel-workspace--file-in-allowed-roots-p)
                 (lambda (_root _buffer) "/tmp/allowed/"))
                ((symbol-function 'mevedel--prompt-user-for-access)
                 (lambda (&rest _) (cl-incf calls))))
        (mevedel-tools--request-access
         "/tmp/allowed" "" (lambda (o) (push o received))))
      (should (= 0 calls))
      (should (equal received '(approve))))))


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


;;
;;; Permission prompt buttons

(mevedel-deftest mevedel-permission--prompt-body ()
  ,test
  (test)
  :doc "default prompt includes session allow"
  (cl-letf (((symbol-function 'gptel-agent--block-bg)
             (lambda () 'default)))
    (should (string-match-p
             "allow-session"
             (mevedel-permission--prompt-body "Body\n" nil))))
  :doc "suppressed session allow prompt omits session allow"
  (cl-letf (((symbol-function 'gptel-agent--block-bg)
             (lambda () 'default)))
    (should-not (string-match-p
                 "allow-session"
                 (mevedel-permission--prompt-body "Body\n" nil t))))
  :doc "suppressed session allow keeps deny-session"
  (cl-letf (((symbol-function 'gptel-agent--block-bg)
             (lambda () 'default)))
    (should (string-match-p
             "deny-session"
             (mevedel-permission--prompt-body "Body\n" nil t)))))

(mevedel-deftest mevedel-permission--prompt-approve-session ()
  ,test
  (test)
  :doc "does not settle prompts that suppress session allow"
  (with-temp-buffer
    (let (received)
      (insert "prompt")
      (let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-permission-suppress-allow-session t)
        (overlay-put ov 'mevedel--callback
                     (lambda (outcome) (push outcome received)))
        (push ov mevedel--prompt-overlays)
        (goto-char (point-min))
        (mevedel-permission--prompt-approve-session)
        (should-not received)
        (should (overlay-buffer ov))))))

(mevedel-deftest mevedel-permission--prompt-async-with-content ()
  ,test
  (test)
  :doc "suppressed session allow reaches rendered prompt body and keymap"
  (with-temp-buffer
    (let ((target (current-buffer))
          captured-body
          captured-keymap)
      (cl-letf (((symbol-function 'gptel-agent--block-bg)
                 (lambda () 'default))
                ((symbol-function 'mevedel--prompt--data-buffer)
                 (lambda () target))
                ((symbol-function 'mevedel-view--interaction-target-buffer)
                 (lambda (_data-buffer) target))
                ((symbol-function 'mevedel-view--interaction-register)
                 (lambda (plist)
                   (setq captured-body (plist-get plist :body))
                   (setq captured-keymap (plist-get plist :keymap))
                   (make-overlay (point-min) (point-min))))
                ((symbol-function 'mevedel--prompt--register-canceller)
                 #'ignore))
        (mevedel-permission--prompt-async-with-content
         "Body\n" t #'ignore nil nil t))
      (should-not (string-match-p "allow-session" captured-body))
      (should-not (lookup-key captured-keymap "s"))
      (should (lookup-key captured-keymap "A"))))

  :doc "permission prompt relies on interaction registration for focus"
  (with-temp-buffer
    (let ((target (current-buffer)))
      (insert "input")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'gptel-agent--block-bg)
                 (lambda () 'default))
                ((symbol-function 'mevedel--prompt--data-buffer)
                 (lambda () target))
                ((symbol-function 'mevedel-view--interaction-target-buffer)
                 (lambda (_data-buffer) target))
                ((symbol-function 'mevedel-view--interaction-register)
                 (lambda (_plist)
                   (make-overlay (point-min) (point-min))))
                ((symbol-function 'mevedel--prompt--register-canceller)
                 #'ignore))
        (mevedel-permission--prompt-async-with-content
         "Body\n" t #'ignore)
        (mevedel-permission--prompt-async-eval
         "Eval\n" #'ignore))
      (should (= (point) (point-max))))))

(mevedel-deftest mevedel-permission--prompt-async-bash ()
  ,test
  (test)
  :doc "dangerous Bash prompts suppress session and persistent allow"
  (let (captured-include captured-suppress captured-content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (content include-always _cont &optional _count _entry
                                suppress-allow-session)
                 (setq captured-content content)
                 (setq captured-include include-always)
                 (setq captured-suppress suppress-allow-session))))
      (mevedel-permission--prompt-async-bash
       "sudo pwd" t t nil #'ignore nil
       (list :allow-patterns '("sudo pwd"))))
    (should-not captured-include)
    (should captured-suppress)
    (should (string-match-p
             "Session/permanent allow is disabled"
             captured-content))
    (should-not (string-match-p
                 "Session/always allow will add"
                 captured-content))))


(provide 'test-mevedel-async-prompt)
;;; test-mevedel-async-prompt.el ends here
