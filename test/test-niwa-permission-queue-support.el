;;; test-niwa-permission-queue-support.el --- Scripted person -*- lexical-binding: t -*-

;;; Commentary:

;; The scripted person presses keys on real cards.  An outcome the card does
;; not offer fails the test; injecting a captured closure is a separate step.

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))

(defun pqs--request (outcomes)
  "Request a once-only card, answer OUTCOMES, return the entry's outcomes."
  (let (got)
    (niwa-pq-test--with-queue (session _data view)
      (ignore view)
      (niwa-pq-test--answering (outcomes :session session :here t)
        (niwa-pq-test--request
         (list :kind 'generic :tool-name "Read" :origin "/root"
               :once-only t :callback (lambda (o) (push o got))))))
    (nreverse got)))

(niwa-deftest niwa-pq-test--step
  (:quiet t)
  ,test
  (test)
  :doc "a scripted outcome the card does not offer fails the test"
  (let ((failure (should-error (pqs--request '(allow-session))
                               :type 'ert-test-failed)))
    (should (string-match-p "offers no key for allow-session"
                            (format "%S" failure))))
  :doc "an outcome the card offers is pressed"
  (should (equal '(allow-once) (pqs--request '(allow-once))))
  :doc "injection is the explicit way to answer with an unoffered outcome"
  (should (equal '(allow-session) (pqs--request '((inject allow-session)))))
  :doc "an unknown outcome fails"
  (should-error (pqs--request '(bogus)) :type 'ert-test-failed))

(provide 'test-niwa-permission-queue-support)
;;; test-niwa-permission-queue-support.el ends here
