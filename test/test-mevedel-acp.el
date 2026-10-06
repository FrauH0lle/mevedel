;;; test-mevedel-acp.el --- ACP conversation lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; Drive a deterministic subprocess through acp.el and the public lifecycle.

;;; Code:

(require 'mevedel-acp)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(defconst mevedel-acp-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(defmacro mevedel-acp-test--with-connection (session-id &rest body)
  "Run BODY with an ACP peer, optionally resuming SESSION-ID."
  (declare (indent 1) (debug t))
  `(let* ((directory (make-temp-file "mevedel-acp-test-" t))
          ready failure
          (connection
           (mevedel-acp-open
            (list :command (executable-find "python3")
                  :args (list mevedel-acp-test--peer)
                  :cwd directory :session-id ,session-id)
            (lambda (_connection) (setq ready t))
            (lambda (error) (setq failure error)))))
     (unwind-protect
         (cl-labels ((await (predicate)
                       (with-timeout (5 (ert-fail "ACP peer did not respond"))
                         (while (not (funcall predicate))
                           (accept-process-output nil 0.01)))))
           (await (lambda () (or ready failure)))
           ,@body)
       (mevedel-acp-close connection)
       (delete-directory directory t))))

(mevedel-deftest mevedel-acp-prompt ()
  ,test
  (test)
  :doc "streams only the owned session and settles each prompt once"
  (mevedel-acp-test--with-connection nil
    (should ready)
    (should-not failure)
    (should (equal "fixture-session" (mevedel-acp-session-id connection)))
    (let ((text "") completions)
      (mevedel-acp-prompt
       connection [((type . "text") (text . "hello"))]
       (lambda (notification)
         (let ((update (alist-get 'update (alist-get 'params notification))))
           (setq text (concat text (alist-get 'text (alist-get 'content update))))))
       (lambda (outcome) (push outcome completions)))
      (await (lambda () completions))
      (should (equal "answer:hello" text))
      (should (eq 'success (plist-get (car completions) :status)))
      (should (equal "end_turn" (plist-get (car completions) :stop-reason)))
      (should (= 1 (length completions)))
      (setq completions nil)
      (mevedel-acp-prompt connection [((type . "text") (text . "again"))]
                          #'ignore (lambda (outcome) (push outcome completions)))
      (await (lambda () completions))
      (should (= 1 (length completions)))))

  :doc "image input is rejected before dispatch when the peer lacks the capability"
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "MEVEDEL_ACP_TEST_NO_IMAGES" "1")
    (mevedel-acp-test--with-connection nil
      (should ready)
      (should-error
       (mevedel-acp-prompt connection [((type . "image") (data . "AA==") (mimeType . "image/png"))]
                           #'ignore #'ignore)
       :type 'user-error)
      (should (eq 'idle (mevedel-acp-state connection)))
      (let (outcome)
        (mevedel-acp-prompt connection [((type . "text") (text . "still usable"))]
                            #'ignore (lambda (value) (setq outcome value)))
        (await (lambda () outcome))
        (should (eq 'success (plist-get outcome :status))))))

  :doc "process loss fails the active prompt without automatically restarting"
  (mevedel-acp-test--with-connection nil
    (let (outcomes)
      (mevedel-acp-prompt connection [((type . "text") (text . "crash"))]
                          #'ignore (lambda (outcome) (push outcome outcomes)))
      (await (lambda () outcomes))
      (should (= 1 (length outcomes)))
      (should (eq 'error (plist-get (car outcomes) :status)))
      (should-error
       (mevedel-acp-prompt connection [((type . "text") (text . "again"))]
                           #'ignore #'ignore)))))

(mevedel-deftest mevedel-acp-cancel
  (:doc "interrupt waits for the cancelled prompt response and keeps history usable")
  (mevedel-acp-test--with-connection nil
    (let (streaming outcomes)
      (mevedel-acp-prompt connection [((type . "text") (text . "wait"))]
                          (lambda (_) (setq streaming t))
                          (lambda (outcome) (push outcome outcomes)))
      (await (lambda () streaming))
      (should-error
       (mevedel-acp-prompt connection [] #'ignore #'ignore))
      (mevedel-acp-cancel connection)
      (await (lambda () outcomes))
      (should (= 1 (length outcomes)))
      (should (eq 'interrupted (plist-get (car outcomes) :status)))
      (setq outcomes nil)
      (mevedel-acp-prompt connection [((type . "text") (text . "after cancel"))]
                          #'ignore (lambda (outcome) (push outcome outcomes)))
      (await (lambda () outcomes))
      (should (eq 'success (plist-get (car outcomes) :status))))))

(mevedel-deftest mevedel-acp-open ()
  ,test
  (test)
  :doc "resume retains the external session identity"
  (mevedel-acp-test--with-connection "fixture-session"
    (should ready)
    (should-not failure)
    (should (equal "fixture-session" (mevedel-acp-session-id connection))))

  :doc "missing retained history fails startup without creating a fresh session"
  (mevedel-acp-test--with-connection "missing-session"
    (should-not ready)
    (should (equal "Missing history" failure))
    (should-error
     (mevedel-acp-prompt connection [] #'ignore #'ignore))))

(mevedel-deftest mevedel-acp-close
  (:doc "close settles a waiting prompt once and is idempotent")
  (mevedel-acp-test--with-connection nil
    (let (streaming outcomes)
      (mevedel-acp-prompt connection [((type . "text") (text . "wait"))]
                          (lambda (_) (setq streaming t))
                          (lambda (outcome) (push outcome outcomes)))
      (await (lambda () streaming))
      (mevedel-acp-close connection)
      (mevedel-acp-close connection)
      (should (= 1 (length outcomes)))
      (should (eq 'interrupted (plist-get (car outcomes) :status))))))

(provide 'test-mevedel-acp)
;;; test-mevedel-acp.el ends here
