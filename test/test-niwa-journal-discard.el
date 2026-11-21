;;; test-niwa-journal-discard.el -- Durable omission records -*- lexical-binding: t -*-

;;; Commentary:

;; Validates the closed omission codec and recoverable pin-release ordering.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-journal-claim)
(require 'niwa-journal-discard)

(niwa-deftest niwa-journal-discard-accepted ()
  ,test
  (test)
  :doc "accepts only a closed omission with the original source identity"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (id (plist-get (car (niwa-journal-capture-list workspace)) :id))
            (payload (niwa-journal-discard-prepare workspace id))
            (record (niwa-journal-discard-accepted (list :status 'cancelled :payload payload) id)))
       (should (equal (directory-file-name (file-local-name (niwa-session-save-path session)))
                      (plist-get record :source-directory)))
       (dolist (change '((:source-directory . "relative") (:source-directory . "/ssh:foreign:/source")
                         (:source-kind . "global") (:source-client . "unknown")
                         (:created . -1) (:reason . "changed") (:extra . t)))
         (let ((invalid (copy-sequence record)))
           (plist-put invalid (car change) (cdr change))
           (should-error (niwa-journal-discard-accepted
                          (list :status 'cancelled :payload (json-serialize invalid)) id))))
       (should-error (niwa-journal-discard-accepted
                      (list :status 'cancelled :payload payload) (make-string 64 ?f)))
       (should-error (niwa-journal-discard-accepted '(:status cancelled :payload "corrupted") id))
       (should-not (niwa-journal-discard-accepted '(:status completed :payload "Digest") id))
       (should-not (niwa-journal-discard-accepted
                    '(:status cancelled :payload "{\"class\":\"cancelled\",\"error\":\"Digest cancelled\"}") id))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session)))))))

(niwa-deftest niwa-journal-discard-complete ()
  ,test
  (test)
  :doc "conflicting omission storage preserves the pin until checked completion"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (directory (niwa-journal-capture--directory workspace id))
            (claim (niwa-journal-claim-acquire (file-name-concat directory "attempts") 120))
            (record (niwa-journal-discard-accepted
                     (niwa-journal-claim-settle claim 'cancelled (niwa-journal-discard-prepare workspace id)) id))
            (path (file-name-concat directory "discard.json")))
       (write-region "conflicting record" nil path nil 'silent)
       (should-error (niwa-journal-discard-complete workspace record))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
       (should (file-exists-p (file-name-concat directory "capture.json")))
       (delete-file path)
       (should (equal record (niwa-journal-discard-complete workspace record)))
       (should (equal record (niwa-journal-discard-complete workspace record)))
       (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
       (should-not (file-exists-p (file-name-concat directory "capture.json")))))))

(provide 'test-niwa-journal-discard)
;;; test-niwa-journal-discard.el ends here
