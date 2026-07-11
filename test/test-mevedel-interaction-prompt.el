;;; test-mevedel-interaction-prompt.el --- Interaction prompt tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests shared interaction prompt settlement, lookup, cancellation, and
;; concurrent overlay behavior.

;;; Code:

(require 'cl-lib)
(require 'mevedel-interaction-prompt)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel--prompt--settle ()
  ,test
  (test)
  :doc "fires callback exactly once and removes overlay from pending list"
  (with-temp-buffer
    (let* ((received nil)
           (callback (lambda (outcome) (push outcome received)))
           (overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'mevedel--callback callback)
      (overlay-put overlay 'mevedel-user-request t)
      (push overlay mevedel--prompt-overlays)
      (mevedel--prompt--settle overlay 'approve)
      (should (equal received '(approve)))
      (should-not mevedel--prompt-overlays)
      (should-not (overlay-buffer overlay))
      (mevedel--prompt--settle overlay 'deny)
      (should (equal received '(approve)))))

  :doc "settlement mark survives nil callback path"
  (with-temp-buffer
    (let ((overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'mevedel-user-request t)
      (push overlay mevedel--prompt-overlays)
      (mevedel--prompt--settle overlay 'aborted)
      (should (overlay-get overlay 'mevedel-settled))
      (should-not mevedel--prompt-overlays)))

  :doc "removes interaction descriptor when prompt settles"
  (with-temp-buffer
    (let* ((mevedel-view--interaction-descriptors
            (make-hash-table :test #'equal))
           (mevedel-view--interaction-overlays
            (make-hash-table :test #'equal))
           (id '(:permission test))
           (overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'mevedel-user-request t)
      (overlay-put overlay 'mevedel-view-interaction-id id)
      (puthash id overlay mevedel-view--interaction-overlays)
      (puthash id (list :id id) mevedel-view--interaction-descriptors)
      (push overlay mevedel--prompt-overlays)
      (mevedel--prompt--settle overlay 'approve)
      (should-not (gethash id mevedel-view--interaction-descriptors))
      (should-not (gethash id mevedel-view--interaction-overlays))))

  :doc "deleted interaction overlay activation does not fire callback"
  (with-temp-buffer
    (let* ((mevedel-view--interaction-overlays
            (make-hash-table :test #'equal))
           (id '(:permission stale))
           received
           (overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'mevedel-user-request t)
      (overlay-put overlay 'mevedel-view-interaction-id id)
      (overlay-put overlay 'mevedel--callback
                   (lambda (outcome) (push outcome received)))
      (puthash id overlay mevedel-view--interaction-overlays)
      (delete-overlay overlay)
      (mevedel--prompt--settle overlay 'approve)
      (should-not received))))

(mevedel-deftest mevedel--prompt--overlay-at-point
  ()
  ,test
  (test)
  :doc "finds prompt overlays at point"
  (with-temp-buffer
    (insert "input\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max) nil t)))
      (overlay-put overlay 'mevedel-permission-prompt t)
      (should (eq overlay
                  (mevedel--prompt--overlay-at-point
                   'mevedel-permission-prompt)))))

  :doc "finds materialized interaction overlays via text property"
  (with-temp-buffer
    (insert "permission body\n")
    (let ((overlay (make-overlay (point-min) (point-max) nil t)))
      (overlay-put overlay 'mevedel-permission-prompt t)
      (add-text-properties
       (point-min) (point-max)
       (list 'mevedel-view-interaction-overlay overlay))
      (goto-char (point-min))
      (should (eq overlay
                  (mevedel--prompt--overlay-at-point
                   'mevedel-permission-prompt))))))

(mevedel-deftest mevedel--prompt-dismiss-all ()
  ,test
  (test)
  :doc "settles every pending overlay with aborted"
  (with-temp-buffer
    (let* (received
           (callback (lambda (outcome) (push outcome received)))
           (first (make-overlay (point) (point) nil t))
           (second (make-overlay (point) (point) nil t)))
      (dolist (overlay (list first second))
        (overlay-put overlay 'mevedel--callback callback)
        (overlay-put overlay 'mevedel-user-request t)
        (push overlay mevedel--prompt-overlays))
      (mevedel--prompt-dismiss-all)
      (should (= 2 (length received)))
      (should (cl-every (lambda (outcome) (eq outcome 'aborted)) received))
      (should-not mevedel--prompt-overlays)))

  :doc "is idempotent after the pending list is drained"
  (with-temp-buffer
    (let* (received
           (callback (lambda (outcome) (push outcome received)))
           (overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'mevedel--callback callback)
      (overlay-put overlay 'mevedel-user-request t)
      (push overlay mevedel--prompt-overlays)
      (mevedel--prompt-dismiss-all)
      (mevedel--prompt-dismiss-all)
      (should (equal received '(aborted))))))

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

  :doc "second registration in the same request does not duplicate it"
  (with-temp-buffer
    (let ((session (mevedel-session--create :name "t")))
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request
                  (mevedel-request--create :session session))
      (mevedel--prompt--register-canceller)
      (mevedel--prompt--register-canceller)
      (should (= 1 (length (mevedel-request-cancellers
                            mevedel--current-request))))))

  :doc "installs the buffer-kill canceller"
  (with-temp-buffer
    (let ((session (mevedel-session--create :name "t")))
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request
                  (mevedel-request--create :session session))
      (mevedel--prompt--register-canceller)
      (should (memq #'mevedel--prompt-dismiss-all
                    (buffer-local-value 'kill-buffer-hook
                                        (current-buffer))))))

  :doc "source buffer supplies a request for a prompt rendered elsewhere"
  (let ((source-buffer (generate-new-buffer " *test-prompt-source*"))
        (view-buffer (generate-new-buffer " *test-prompt-view*")))
    (unwind-protect
        (let ((session (mevedel-session--create :name "t")) request)
          (with-current-buffer source-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (setq request
                              (mevedel-request--create :session session))))
          (with-current-buffer view-buffer
            (mevedel--prompt--register-canceller source-buffer)
            (should (= 1 (length (mevedel-request-cancellers request))))
            (should (memq #'mevedel--prompt-dismiss-all
                          (buffer-local-value 'kill-buffer-hook
                                              view-buffer)))))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(mevedel-deftest mevedel--prompt-concurrent-overlays
  (:doc "two overlays settle independently in user-chosen order")
  (with-temp-buffer
    (let* (received
           (first-callback
            (lambda (outcome) (push (cons 'first outcome) received)))
           (second-callback
            (lambda (outcome) (push (cons 'second outcome) received)))
           (first (make-overlay (point) (point) nil t))
           (second (make-overlay (point) (point) nil t)))
      (overlay-put first 'mevedel--callback first-callback)
      (overlay-put first 'mevedel-user-request t)
      (overlay-put second 'mevedel--callback second-callback)
      (overlay-put second 'mevedel-user-request t)
      (push first mevedel--prompt-overlays)
      (push second mevedel--prompt-overlays)
      (mevedel--prompt--settle second 'deny)
      (mevedel--prompt--settle first 'approve)
      (should (member '(second . deny) received))
      (should (member '(first . approve) received))
      (should-not mevedel--prompt-overlays))))

(mevedel-deftest mevedel--prompt-buffer-kill
  (:doc "killing the prompt buffer settles pending overlays with aborted")
  (let* (received
         (callback (lambda (outcome) (push outcome received)))
         (buffer (generate-new-buffer " *prompt-kill-test*")))
    (with-current-buffer buffer
      (let ((overlay (make-overlay (point) (point) nil t)))
        (overlay-put overlay 'mevedel--callback callback)
        (overlay-put overlay 'mevedel-user-request t)
        (push overlay mevedel--prompt-overlays)
        (add-hook 'kill-buffer-hook #'mevedel--prompt-dismiss-all nil t)))
    (kill-buffer buffer)
    (should (equal received '(aborted)))))

(provide 'test-mevedel-interaction-prompt)

;;; test-mevedel-interaction-prompt.el ends here
