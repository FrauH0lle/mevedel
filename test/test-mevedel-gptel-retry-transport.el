;;; test-mevedel-gptel-retry-transport.el --- Curl failure lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; A disposable curl executable exits with the real transport status.  Native
;; sentinels, callbacks and FSM transitions remain intact; recovery uses only
;; the local HTTP fixture, never a provider endpoint.

;;; Code:

(require 'gptel-retry-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "gptel-retry-helpers"))

(defmacro mevedel-retry-transport-test--with-exit (failures prefix &rest body)
  "Run BODY with FAILURES curl exits of 92 after writing PREFIX.
Later attempts use real curl against the fixture's local server."
  (declare (indent 2) (debug t))
  `(let* ((directory (make-temp-file "mevedel-curl-exit-" t))
          (script (file-name-concat directory "curl"))
          (counter (file-name-concat directory "count"))
          (payload (file-name-concat directory "prefix"))
          (curl (gptel--curl-path)))
     (unwind-protect
         (progn
           (with-temp-file payload (insert ,prefix))
           (with-temp-file script
             (insert "#!/bin/sh\n"
                     "n=0\n"
                     "if test -f " (shell-quote-argument counter) "; then n=$(cat "
                     (shell-quote-argument counter) "); fi\n"
                     "n=$((n+1))\nprintf '%s' \"$n\" > "
                     (shell-quote-argument counter) "\n"
                     "if test \"$n\" -le " (number-to-string ,failures) "; then\n"
                     "cat >/dev/null\ncat " (shell-quote-argument payload) "\nexit 92\nfi\n"
                     "exec " (shell-quote-argument curl) " \"$@\"\n"))
           (set-file-modes script #o700)
           (let ((gptel-use-curl script))
             ,@body))
       (delete-directory directory t))))

(ert-deftest mevedel-gptel-retry-dispatch/curl-92-native-sentinels ()
  "Curl 92 retries both sentinel paths, including failures after HTTP200."
  (mevedel-test--with-captured-diagnostics nil
    (dolist (stream '(nil t))
      (dolist (prefix (if stream
                         '("" "HTTP/2 200 OK\r\n\r\nevent: response.output_text.delta\ndata: {\"delta\":\"Abandoned output\"}\n\n")
                       '("" "HTTP/2 200 OK\r\n\r\n")))
        (ert-info ((format "stream=%s headers=%s" stream (not (equal prefix ""))))
          (mevedel-retry-test--with-http
              (list (if stream mevedel-retry-test--success
                      "{\"status\":\"completed\",\"error\":null,\"output\":[{\"type\":\"message\",\"content\":[{\"type\":\"output_text\",\"text\":\"Recovered answer\"}]}]}"))
            (plist-put (gptel-fsm-info fsm) :stream stream)
            (plist-put (gptel-fsm-info fsm) :callback
                       (if stream #'gptel-curl--stream-insert-response #'gptel--insert-response))
            (mevedel-retry-transport-test--with-exit 2 prefix
              (gptel--fsm-transition fsm)
              (mevedel-retry-test--drain-native t)
              (should (eq 'MRETRY (gptel-fsm-state fsm)))
              (should (= 0 failed))
              (should (= 1 (length scheduled)))
              (should-not gptel--request-alist)
              (mevedel-retry-test--drain-native)
              (should (eq 'DONE (gptel-fsm-state fsm)))
              (should (= 1 finished))
              (should (= 0 failed))
              (should (= 1 waits))
              (should (= 1 (length requests)))
              (should (= 2 (length (plist-get (gptel-fsm-info fsm) :mevedel-retry-errors))))
              (should-not scheduled)
              (should-not gptel--request-alist)
              (should-not mevedel-gptel-retry--states)
              (with-current-buffer data-buf
                (should (equal "Recovered answer" (string-trim (buffer-string))))))))))))

(ert-deftest mevedel-gptel-retry-dispatch/curl-92-exhaustion ()
  "Transport failures exhaust once and retain the native terminal state."
  (mevedel-test--with-captured-diagnostics nil
    (dolist (stream '(nil t))
      (dolist (prefix '("" "HTTP/2 200 OK\r\n\r\n"))
        (ert-info ((format "stream=%s headers=%s" stream (not (equal prefix ""))))
          (let ((mevedel-gptel-retry-max-retries 2)
                terminal-callbacks)
            (mevedel-retry-test--with-http nil
              (plist-put (gptel-fsm-info fsm) :stream stream)
              (plist-put (gptel-fsm-info fsm) :callback
                         (lambda (response _info)
                           (when (null response)
                             (push (list (gptel-fsm-state fsm) failed)
                                   terminal-callbacks))))
              (mevedel-retry-transport-test--with-exit 3 prefix
                (gptel--fsm-transition fsm)
                (mevedel-retry-test--drain-native)
                (should (= 1 failed))
                (should (= 0 finished))
                (should (eq 'ERRS (gptel-fsm-state fsm)))
                (should (equal '((TYPE 0)) terminal-callbacks))
                (should (plist-get (gptel-fsm-info fsm) :mevedel-retry-exhausted))
                (should (= 3 (length (plist-get (gptel-fsm-info fsm) :mevedel-retry-errors))))
                (should-not scheduled)
                (should-not gptel--request-alist)
                (should-not mevedel-gptel-retry--states)))))))))

(ert-deftest mevedel-gptel-retry-dispatch/curl-92-disabled ()
  "Disabling retries still delivers the callback before terminal settlement."
  (mevedel-test--with-captured-diagnostics nil
    (dolist (prefix '("" "HTTP/2 200 OK\r\n\r\n"))
      (let ((mevedel-gptel-retry-max-retries 0)
            callbacks)
        (mevedel-retry-test--with-http nil
          (plist-put (gptel-fsm-info fsm) :callback
                     (lambda (response _info)
                       (when (null response)
                         (push (list (gptel-fsm-state fsm) failed) callbacks))))
          (mevedel-retry-transport-test--with-exit 1 prefix
            (gptel--fsm-transition fsm)
            (mevedel-test--await 5 "Curl cleanup did not finish"
              (null gptel--request-alist))
            (should (equal '((TYPE 0)) callbacks))
            (should (eq 'ERRS (gptel-fsm-state fsm)))
            (should (= 1 failed))
            (should (= 0 finished))
            (should-not requests)
            (should-not scheduled)
            (should-not gptel--request-alist)
            (should-not mevedel-gptel-retry--states)))))))

(ert-deftest mevedel-gptel-retry-dispatch/curl-92-before-headers-keeps-type-transition ()
  "Before headers, cleanup must still perform the real WAIT -> TYPE step."
  (mevedel-test--with-captured-diagnostics nil
    (mevedel-retry-test--with-http nil
      (let ((transition (symbol-function 'gptel--fsm-transition))
            cleanup-transitions)
        (cl-letf (((symbol-function 'gptel--fsm-transition)
                   (lambda (machine &optional next)
                     (when (plist-get
                            (plist-get (gptel-fsm-info machine) :mevedel-retry-state)
                            :completing)
                       (push (list (gptel-fsm-state machine)
                                   (and mevedel-gptel-retry--cleanup-header-fsm t))
                             cleanup-transitions))
                     (funcall transition machine next))))
          (mevedel-retry-transport-test--with-exit 1 ""
            (gptel--fsm-transition fsm)
            (mevedel-retry-test--drain-native t)
            (should (equal '((WAIT nil) (TYPE nil))
                           (reverse cleanup-transitions)))
            (should (eq 'MRETRY (gptel-fsm-state fsm)))
            (should (= 0 failed))
            (should (= 1 (length scheduled)))
            (should-not gptel--request-alist)))))))

(ert-deftest mevedel-gptel-retry-dispatch/curl-92-preserves-permanent-refusal ()
  "A provider refusal remains terminal even if the stream then exits 92."
  (mevedel-test--with-captured-diagnostics nil
    (mevedel-retry-test--with-http nil
      (let (callbacks)
        (plist-put (gptel-fsm-info fsm) :callback
                   (lambda (response info)
                     (when (null response)
                       (push (list (gptel-fsm-state fsm) failed
                                   (plist-get (plist-get info :error) :code))
                             callbacks))))
        (mevedel-retry-transport-test--with-exit 1
            (concat "HTTP/2 200 OK\r\n\r\n"
                    "event: response.failed\n"
                    "data: {\"response\":{\"status\":\"failed\",\"error\":{\"code\":\"insufficient_quota\",\"message\":\"Quota exhausted\"}}}\n\n")
          (gptel--fsm-transition fsm)
          (mevedel-retry-test--drain-native)
          (should (equal '((TYPE 0 "insufficient_quota")) callbacks))
          (should (eq 'ERRS (gptel-fsm-state fsm)))
          (should (= 1 failed))
          (should (= 0 finished))
          (should (plist-get (gptel-fsm-info fsm) :mevedel-retry-permanent))
          (should-not (plist-get (gptel-fsm-info fsm) :mevedel-retry-exhausted))
          (should-not (plist-get (gptel-fsm-info fsm) :mevedel-retry-errors))
          (should-not requests)
          (should-not scheduled)
          (should-not gptel--request-alist)
          (should-not mevedel-gptel-retry--states))))))

(provide 'test-mevedel-gptel-retry-transport)
;;; test-mevedel-gptel-retry-transport.el ends here
