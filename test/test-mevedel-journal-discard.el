;;; test-mevedel-journal-discard.el -- Durable omission records -*- lexical-binding: t -*-

;;; Commentary:

;; Validates the closed omission codec and recoverable pin-release ordering.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-journal-claim)
(require 'mevedel-journal-discard)

(mevedel-deftest mevedel-journal-discard-accepted ()
  ,test
  (test)
  :doc "accepts only a closed omission with the original source identity"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (id (plist-get (car (mevedel-journal-capture-list workspace)) :id))
            (payload (mevedel-journal-discard-prepare workspace id))
            (record (mevedel-journal-discard-accepted (list :status 'cancelled :payload payload) id)))
       (should (equal (directory-file-name (file-local-name (mevedel-session-save-path session)))
                      (plist-get record :source-directory)))
       (dolist (change '((:source-directory . "relative") (:source-directory . "/ssh:foreign:/source")
                         (:source-kind . "global") (:source-client . "unknown")
                         (:created . -1) (:reason . "changed") (:extra . t)))
         (let ((invalid (copy-sequence record)))
           (plist-put invalid (car change) (cdr change))
           (should-error (mevedel-journal-discard-accepted
                          (list :status 'cancelled :payload (json-serialize invalid)) id))))
       (should-error (mevedel-journal-discard-accepted
                      (list :status 'cancelled :payload payload) (make-string 64 ?f)))
       (should-error (mevedel-journal-discard-accepted '(:status cancelled :payload "corrupted") id))
       (should-not (mevedel-journal-discard-accepted '(:status completed :payload "Digest") id))
       (should-not (mevedel-journal-discard-accepted
                    '(:status cancelled :payload "{\"class\":\"cancelled\",\"error\":\"Digest cancelled\"}") id))
       (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))))))

(mevedel-deftest mevedel-journal-discard-complete ()
  ,test
  (test)
  :doc "conflicting omission storage preserves the pin until checked completion"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (directory (mevedel-journal-capture--directory workspace id))
            (claim (mevedel-journal-claim-acquire (file-name-concat directory "attempts") 120))
            (record (mevedel-journal-discard-accepted
                     (mevedel-journal-claim-settle claim 'cancelled (mevedel-journal-discard-prepare workspace id)) id))
            (path (file-name-concat directory "discard.json")))
       (write-region "conflicting record" nil path nil 'silent)
       (should-error (mevedel-journal-discard-complete workspace record))
       (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
       (should (file-exists-p (file-name-concat directory "capture.json")))
       (delete-file path)
       (should (equal record (mevedel-journal-discard-complete workspace record)))
       (should (equal record (mevedel-journal-discard-complete workspace record)))
       (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
       (should-not (file-exists-p (file-name-concat directory "capture.json")))))))

(provide 'test-mevedel-journal-discard)
;;; test-mevedel-journal-discard.el ends here
