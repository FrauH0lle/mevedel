;;; test-niwa-interaction-prompt.el --- Interaction prompt tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests shared interaction prompt settlement, lookup, cancellation, and
;; concurrent overlay behavior.

;;; Code:

(require 'cl-lib)
(require 'niwa-interaction-prompt)
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-interaction)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun niwa-interaction-prompt-test--released-registry (action)
  "Return prompt registration state after settling through ACTION."
  (with-temp-buffer
    (let* ((session (niwa-session--create :name "t"))
           (request (niwa-request--create :session session))
           (overlay (make-overlay (point) (point) nil t)))
      (setq-local niwa--session session
                  niwa--current-request request)
      (overlay-put overlay 'niwa-user-request t)
      (push overlay niwa--prompt-overlays)
      (niwa--prompt--register-canceller nil overlay)
      (pcase action
        ('settle (niwa--prompt--settle overlay 'approve))
        ('cancel (niwa-request-drain-cancellers request)))
      niwa--prompt-canceller-registered-for)))

(niwa-deftest niwa--prompt-attribution-line
  ()
  ,test
  (test)
  :doc "suppresses root attribution and renders canonical child paths"
  (progn
    (should (equal "" (niwa--prompt-attribution-line nil)))
    (should (equal "" (niwa--prompt-attribution-line "/root")))
    (should
     (equal "from /root/worker/verifier\n"
            (substring-no-properties
             (niwa--prompt-attribution-line
              "/root/worker/verifier"))))))

(niwa-deftest niwa--prompt--settle (:quiet t)
  ,test
  (test)
  :doc "fires callback exactly once and removes overlay from pending list"
  (with-temp-buffer
    (let* ((received nil)
           (callback (lambda (outcome) (push outcome received)))
           (overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'niwa--callback callback)
      (overlay-put overlay 'niwa-user-request t)
      (push overlay niwa--prompt-overlays)
      (niwa--prompt--settle overlay 'approve)
      (should (equal received '(approve)))
      (should-not niwa--prompt-overlays)
      (should-not (overlay-buffer overlay))
      (niwa--prompt--settle overlay 'deny)
      (should (equal received '(approve)))))

  :doc "settlement mark survives nil callback path"
  (with-temp-buffer
    (let ((overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'niwa-user-request t)
      (push overlay niwa--prompt-overlays)
      (niwa--prompt--settle overlay 'aborted)
      (should (overlay-get overlay 'niwa-settled))
      (should-not niwa--prompt-overlays)))

  :doc "removes interaction descriptor when prompt settles"
  (with-temp-buffer
    (let* ((niwa-view--interaction-descriptors
            (make-hash-table :test #'equal))
           (niwa-view--interaction-overlays
            (make-hash-table :test #'equal))
           (id '(:permission test))
           (overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'niwa-user-request t)
      (overlay-put overlay 'niwa-view-interaction-id id)
      (puthash id overlay niwa-view--interaction-overlays)
      (puthash id (list :id id) niwa-view--interaction-descriptors)
      (push overlay niwa--prompt-overlays)
      (niwa--prompt--settle overlay 'approve)
      (should-not (gethash id niwa-view--interaction-descriptors))
      (should-not (gethash id niwa-view--interaction-overlays))))

  :doc "deleted interaction overlay activation does not fire callback"
  (with-temp-buffer
    (let* ((niwa-view--interaction-overlays
            (make-hash-table :test #'equal))
           (id '(:permission stale))
           received
           (overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'niwa-user-request t)
      (overlay-put overlay 'niwa-view-interaction-id id)
      (overlay-put overlay 'niwa--callback
                   (lambda (outcome) (push outcome received)))
      (puthash id overlay niwa-view--interaction-overlays)
      (delete-overlay overlay)
      (niwa--prompt--settle overlay 'approve)
      (should-not received)))

  :doc "settlement does not leave the completed request strongly reachable"
  (let ((registry
         (niwa-interaction-prompt-test--released-registry 'settle)))
    (garbage-collect)
    (should (eq 'key (hash-table-weakness registry)))
    (should (= 0 (hash-table-count registry)))))

(niwa-deftest niwa--prompt--overlay-at-point
  ()
  ,test
  (test)
  :doc "finds prompt overlays at point"
  (with-temp-buffer
    (insert "input\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max) nil t)))
      (overlay-put overlay 'niwa-permission-prompt t)
      (should (eq overlay
                  (niwa--prompt--overlay-at-point
                   'niwa-permission-prompt)))))

  :doc "finds materialized interaction overlays via text property"
  (with-temp-buffer
    (insert "permission body\n")
    (let ((overlay (make-overlay (point-min) (point-max) nil t)))
      (overlay-put overlay 'niwa-permission-prompt t)
      (add-text-properties
       (point-min) (point-max)
       (list 'niwa-view-interaction-overlay overlay))
      (goto-char (point-min))
      (should (eq overlay
                  (niwa--prompt--overlay-at-point
                   'niwa-permission-prompt))))))

(niwa-deftest niwa--prompt-dismiss-all ()
  ,test
  (test)
  :doc "settles every pending overlay with aborted"
  (with-temp-buffer
    (let* (received
           (callback (lambda (outcome) (push outcome received)))
           (first (make-overlay (point) (point) nil t))
           (second (make-overlay (point) (point) nil t)))
      (dolist (overlay (list first second))
        (overlay-put overlay 'niwa--callback callback)
        (overlay-put overlay 'niwa-user-request t)
        (push overlay niwa--prompt-overlays))
      (niwa--prompt-dismiss-all)
      (should (= 2 (length received)))
      (should (cl-every (lambda (outcome) (eq outcome 'aborted)) received))
      (should-not niwa--prompt-overlays)))

  :doc "is idempotent after the pending list is drained"
  (with-temp-buffer
    (let* (received
           (callback (lambda (outcome) (push outcome received)))
           (overlay (make-overlay (point) (point) nil t)))
      (overlay-put overlay 'niwa--callback callback)
      (overlay-put overlay 'niwa-user-request t)
      (push overlay niwa--prompt-overlays)
      (niwa--prompt-dismiss-all)
      (niwa--prompt-dismiss-all)
      (should (equal received '(aborted))))))

(niwa-deftest niwa--prompt--register-canceller ()
  ,test
  (test)
  :doc "first registration pushes a canceller onto the active request"
  (with-temp-buffer
    (let ((session (niwa-session--create :name "t")))
      (setq-local niwa--session session)
      (setq-local niwa--current-request
                  (niwa-request--create :session session))
      (niwa--prompt--register-canceller)
      (should (= 1 (length (niwa-request-cancellers
                            niwa--current-request))))))

  :doc "second registration in the same request does not duplicate it"
  (with-temp-buffer
    (let ((session (niwa-session--create :name "t")))
      (setq-local niwa--session session)
      (setq-local niwa--current-request
                  (niwa-request--create :session session))
      (niwa--prompt--register-canceller)
      (niwa--prompt--register-canceller)
      (should (= 1 (length (niwa-request-cancellers
                            niwa--current-request))))))

  :doc "installs the buffer-kill canceller"
  (with-temp-buffer
    (let ((session (niwa-session--create :name "t")))
      (setq-local niwa--session session)
      (setq-local niwa--current-request
                  (niwa-request--create :session session))
      (niwa--prompt--register-canceller)
      (should (memq #'niwa--prompt-dismiss-all
                    (buffer-local-value 'kill-buffer-hook
                                        (current-buffer))))))

  :doc "source buffer supplies a request for a prompt rendered elsewhere"
  (let ((source-buffer (generate-new-buffer " *test-prompt-source*"))
        (view-buffer (generate-new-buffer " *test-prompt-view*")))
    (unwind-protect
        (let ((session (niwa-session--create :name "t")) request)
          (with-current-buffer source-buffer
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (setq request
                              (niwa-request--create :session session))))
          (with-current-buffer view-buffer
            (niwa--prompt--register-canceller source-buffer)
            (should (= 1 (length (niwa-request-cancellers request))))
            (should (memq #'niwa--prompt-dismiss-all
                          (buffer-local-value 'kill-buffer-hook
                                              view-buffer)))))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))))

  :doc "request cancellation does not leave the request strongly reachable"
  (let ((registry
         (niwa-interaction-prompt-test--released-registry 'cancel)))
    (garbage-collect)
    (should (eq 'key (hash-table-weakness registry)))
    (should (= 0 (hash-table-count registry)))))

(niwa-deftest niwa--prompt-concurrent-overlays
  (:doc "two overlays settle independently in user-chosen order")
  (with-temp-buffer
    (let* (received
           (first-callback
            (lambda (outcome) (push (cons 'first outcome) received)))
           (second-callback
            (lambda (outcome) (push (cons 'second outcome) received)))
           (first (make-overlay (point) (point) nil t))
           (second (make-overlay (point) (point) nil t)))
      (overlay-put first 'niwa--callback first-callback)
      (overlay-put first 'niwa-user-request t)
      (overlay-put second 'niwa--callback second-callback)
      (overlay-put second 'niwa-user-request t)
      (push first niwa--prompt-overlays)
      (push second niwa--prompt-overlays)
      (niwa--prompt--settle second 'deny)
      (niwa--prompt--settle first 'approve)
      (should (member '(second . deny) received))
      (should (member '(first . approve) received))
      (should-not niwa--prompt-overlays))))

(niwa-deftest niwa--prompt-user-with-overlay
  (:doc "rejects audience values outside the closed collaboration contract")
  (should-error
   (niwa--prompt-user-with-overlay
    "Title" "Content" "Question?" nil #'ignore nil 'writer)
   :type 'error))

(niwa-deftest niwa--prompt-buffer-kill
  (:doc "killing the prompt buffer settles pending overlays with aborted")
  (let* (received
         (callback (lambda (outcome) (push outcome received)))
         (buffer (generate-new-buffer " *prompt-kill-test*")))
    (with-current-buffer buffer
      (let ((overlay (make-overlay (point) (point) nil t)))
        (overlay-put overlay 'niwa--callback callback)
        (overlay-put overlay 'niwa-user-request t)
        (push overlay niwa--prompt-overlays)
        (add-hook 'kill-buffer-hook #'niwa--prompt-dismiss-all nil t)))
    (kill-buffer buffer)
    (should (equal received '(aborted)))))

(provide 'test-niwa-interaction-prompt)

;;; test-niwa-interaction-prompt.el ends here
