;;; test-mevedel-preview-mode.el --- Tests for mevedel-preview-mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-preview-mode)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defun mevedel-preview-test--make-overlay (buffer &optional props)
  "Create a bare preview overlay in BUFFER with PROPS alist merged in.
The overlay spans one character so it's a legal region to delete on
cleanup."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert "x")
    (let ((ov (make-overlay (1- (point)) (point) nil t)))
      (overlay-put ov 'mevedel-inline-preview t)
      (dolist (kv props)
        (overlay-put ov (car kv) (cdr kv)))
      ov)))


;;
;;; Minor mode lifecycle

(mevedel-deftest mevedel-preview-mode ()
  ,test
  (test)
  :doc "registering the first overlay activates the mode"
  (with-temp-buffer
    (should-not mevedel-preview-mode)
    (let ((ov (mevedel-preview-test--make-overlay (current-buffer))))
      (mevedel-preview-mode--register ov)
      (should mevedel-preview-mode)
      (should (equal (list ov) mevedel-preview-mode--pending))))
  :doc "unregistering the last overlay deactivates the mode"
  (with-temp-buffer
    (let ((ov (mevedel-preview-test--make-overlay (current-buffer))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--unregister ov)
      (should-not mevedel-preview-mode)
      (should-not mevedel-preview-mode--pending)))
  :doc "unregistering a non-last overlay keeps the mode active"
  (with-temp-buffer
    (let ((a (mevedel-preview-test--make-overlay (current-buffer)))
          (b (mevedel-preview-test--make-overlay (current-buffer))))
      (mevedel-preview-mode--register a)
      (mevedel-preview-mode--register b)
      (mevedel-preview-mode--unregister a)
      (should mevedel-preview-mode)
      (should (equal (list b) mevedel-preview-mode--pending))))
  :doc "lighter shows pending count"
  (with-temp-buffer
    (let ((a (mevedel-preview-test--make-overlay (current-buffer)))
          (b (mevedel-preview-test--make-overlay (current-buffer))))
      (mevedel-preview-mode--register a)
      (should (equal " Preview[1]" (mevedel-preview-mode--lighter)))
      (mevedel-preview-mode--register b)
      (should (equal " Preview[2]" (mevedel-preview-mode--lighter))))))


;;
;;; Approve / reject / feedback via helpers

(mevedel-deftest mevedel-preview-mode--approve-overlay ()
  ,test
  (test)
  :doc "calls apply-fn, fires callback with approval message, unregisters"
  (with-temp-buffer
    (let* ((result nil)
           (applied nil)
           (ov (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/fake.txt")
                  (mevedel--final-callback . ,(lambda (r) (setq result r)))
                  (mevedel--apply-fn . ,(lambda () (setq applied t)))))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--approve-overlay ov)
      (should applied)
      (should (string-match-p "approved and applied to /tmp/fake.txt" result))
      (should-not mevedel-preview-mode--pending)
      (should-not (overlay-buffer ov))))
  :doc "apply-fn error is reported via callback without throwing"
  (with-temp-buffer
    (let* ((result nil)
           (ov (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/fake.txt")
                  (mevedel--final-callback . ,(lambda (r) (setq result r)))
                  (mevedel--apply-fn . ,(lambda () (error "boom")))))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--approve-overlay ov)
      (should (string-match-p "Error applying changes: boom" result))
      (should-not mevedel-preview-mode--pending))))

(mevedel-deftest mevedel-preview-mode--reject-overlay ()
  ,test
  (test)
  :doc "without feedback, callback gets plain rejection message"
  (with-temp-buffer
    (let* ((result nil)
           (ov (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/fake.txt")
                  (mevedel--final-callback . ,(lambda (r) (setq result r)))))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--reject-overlay ov)
      (should (string-match-p "rejected by user" result))
      (should-not (string-match-p "feedback" result))
      (should-not mevedel-preview-mode--pending)))
  :doc "with feedback, callback gets feedback embedded"
  (with-temp-buffer
    (let* ((result nil)
           (ov (mevedel-preview-test--make-overlay
                (current-buffer)
                `((mevedel--real-path . "/tmp/fake.txt")
                  (mevedel--final-callback . ,(lambda (r) (setq result r)))))))
      (mevedel-preview-mode--register ov)
      (mevedel-preview-mode--reject-overlay ov "use a different approach")
      (should (string-match-p "use a different approach" result))
      (should-not mevedel-preview-mode--pending))))


;;
;;; Batch operations

(mevedel-deftest mevedel-preview-mode-approve-all ()
  ,test
  (test)
  :doc "approves every pending overlay"
  (with-temp-buffer
    (let* ((results nil)
           (cb (lambda (r) (push r results)))
           (a (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/a.txt")
                 (mevedel--final-callback . ,cb)
                 (mevedel--apply-fn . ,(lambda () nil)))))
           (b (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/b.txt")
                 (mevedel--final-callback . ,cb)
                 (mevedel--apply-fn . ,(lambda () nil))))))
      (mevedel-preview-mode--register a)
      (mevedel-preview-mode--register b)
      (mevedel-preview-mode-approve-all)
      (should (= 2 (length results)))
      (should (seq-every-p (lambda (r) (string-match-p "approved" r)) results))
      (should-not mevedel-preview-mode--pending))))

(mevedel-deftest mevedel-preview-mode-reject-all ()
  ,test
  (test)
  :doc "rejects every pending overlay and calls mevedel-abort once"
  (with-temp-buffer
    (let* ((results nil)
           (abort-count 0)
           (cb (lambda (r) (push r results)))
           (a (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/a.txt")
                 (mevedel--final-callback . ,cb))))
           (b (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/b.txt")
                 (mevedel--final-callback . ,cb)))))
      (mevedel-preview-mode--register a)
      (mevedel-preview-mode--register b)
      (cl-letf (((symbol-function 'mevedel-abort)
                 (lambda (&rest _) (cl-incf abort-count))))
        (mevedel-preview-mode-reject-all))
      (should (= 2 (length results)))
      (should (seq-every-p (lambda (r) (string-match-p "rejected" r)) results))
      (should (= 1 abort-count))
      (should-not mevedel-preview-mode--pending)))
  :doc "with no pending overlays, does not call mevedel-abort"
  (with-temp-buffer
    (let ((abort-count 0))
      (cl-letf (((symbol-function 'mevedel-abort)
                 (lambda (&rest _) (cl-incf abort-count))))
        (mevedel-preview-mode-reject-all))
      (should (zerop abort-count)))))

(mevedel-deftest mevedel-preview-mode-dismiss-all ()
  ,test
  (test)
  :doc "cleans up overlays without firing callbacks"
  (with-temp-buffer
    (let* ((call-count 0)
           (cb (lambda (_r) (cl-incf call-count)))
           (a (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/a.txt")
                 (mevedel--final-callback . ,cb))))
           (b (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/b.txt")
                 (mevedel--final-callback . ,cb)))))
      (mevedel-preview-mode--register a)
      (mevedel-preview-mode--register b)
      (mevedel-preview-mode-dismiss-all)
      (should (zerop call-count))
      (should-not mevedel-preview-mode--pending)
      (should-not (overlay-buffer a))
      (should-not (overlay-buffer b)))))


;;
;;; Ediff stub cleanup

(mevedel-deftest mevedel-preview-mode--ediff-stub-cleanup ()
  ,test
  (test)
  :doc "cleanup-overlay deletes the stub file when flag is set"
  (let ((stub (make-temp-file "mevedel-stub-")))
    (with-temp-buffer
      (let ((ov (mevedel-preview-test--make-overlay
                 (current-buffer)
                 `((mevedel--real-path . ,stub)
                   (mevedel--ediff-created-stub . t)))))
        (mevedel-preview-mode--register ov)
        (mevedel-preview-mode--cleanup-overlay ov)
        (should-not (file-exists-p stub)))))
  :doc "cleanup-overlay preserves existing files without the stub flag"
  (let ((real-file (make-temp-file "mevedel-real-")))
    (with-temp-buffer
      (let ((ov (mevedel-preview-test--make-overlay
                 (current-buffer)
                 `((mevedel--real-path . ,real-file)))))
        (mevedel-preview-mode--register ov)
        (mevedel-preview-mode--cleanup-overlay ov)
        (should (file-exists-p real-file))
        (delete-file real-file))))
  :doc "apply-overlay clears the stub flag so approve preserves content"
  (let ((stub (make-temp-file "mevedel-stub-" nil nil "approved content\n")))
    (unwind-protect
        (with-temp-buffer
          (let* ((result nil)
                 (ov (mevedel-preview-test--make-overlay
                      (current-buffer)
                      `((mevedel--real-path . ,stub)
                        (mevedel--ediff-created-stub . t)
                        (mevedel--final-callback . ,(lambda (r) (setq result r)))
                        (mevedel--apply-fn . ,(lambda () nil))))))
            (mevedel-preview-mode--register ov)
            (mevedel-preview-mode--approve-overlay ov)
            (should (string-match-p "approved" result))
            (should (file-exists-p stub))))
      (when (file-exists-p stub) (delete-file stub)))))


;;
;;; cancel-fn wiring

(mevedel-deftest mevedel-preview-mode--register-cancel-fn ()
  ,test
  (test)
  :doc "registering a preview installs dismiss-all as cancel-fn on current request"
  (with-temp-buffer
    (setq-local mevedel--current-request
                (mevedel-request--create
                 :file-snapshots (make-hash-table :test #'equal)))
    (let ((ov (mevedel-preview-test--make-overlay
               (current-buffer)
               `((mevedel--real-path . "/tmp/a.txt")))))
      (mevedel-preview-mode--register ov)
      (should (functionp (mevedel-request-cancel-fn mevedel--current-request)))))
  :doc "cancel-fn invokes dismiss-all on its captured buffer"
  (let ((chat-buf (generate-new-buffer " *preview-test-chat*")))
    (unwind-protect
        (let (cancel-fn)
          (with-current-buffer chat-buf
            (setq-local mevedel--current-request
                        (mevedel-request--create
                         :file-snapshots (make-hash-table :test #'equal)))
            (let* ((call-count 0)
                   (ov (mevedel-preview-test--make-overlay
                        chat-buf
                        `((mevedel--real-path . "/tmp/a.txt")
                          (mevedel--final-callback . ,(lambda (_) (cl-incf call-count)))))))
              (mevedel-preview-mode--register ov)
              (setq cancel-fn (mevedel-request-cancel-fn mevedel--current-request))
              (funcall cancel-fn)
              (should (zerop call-count))
              (should-not (buffer-local-value 'mevedel-preview-mode--pending chat-buf)))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(provide 'test-mevedel-preview-mode)
;;; test-mevedel-preview-mode.el ends here
