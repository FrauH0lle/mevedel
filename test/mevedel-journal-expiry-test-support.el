;;; mevedel-journal-expiry-test-support.el --- Native expiry fixtures -*- lexical-binding: t -*-

;;; Commentary:
;; Advance only the native control program's clock for local expiry tests.
;; Claim, outcome and dependent proof bytes remain unchanged.

;;; Code:

(require 'ert)
(require 'mevedel-journal-claim)
(require 'mevedel-session-control-fs)

(defmacro mevedel-test-journal-with-expired-claims (claims &rest body)
  "Run BODY with CLAIMS expired on the native clock of local targets.
Build the override from the current control script, changing only its
clock/before-time stat result.  Prove the unmodified native guard accepts
each deadline, then that the shifted target clock and guard reject it.
CLAIMS is evaluated once; no claim or dependent record is rewritten."
  (declare (indent 1) (debug t))
  (let ((tokens (make-symbol "tokens"))
        (line (make-symbol "line"))
        (offset (make-symbol "offset")))
    `(let* ((,tokens ,claims)
            (,line "      now=$(stat -c '%Y' -- \"$temporary\") || exit 67\n")
            (,offset 0))
       (should ,tokens)
       (dolist (token ,tokens)
         (let* ((directory (plist-get token :directory))
                (deadline (plist-get token :expires-at))
                (now (mevedel-session-control-fs-target-time directory)))
           (should-not (file-remote-p directory))
           (should (< now deadline))
           (should (eq 'ok (plist-get
                           (car (mevedel-session-control-fs-run-program
                                 (list (list :op 'before-time :path directory
                                             :content (number-to-string deadline)))))
                           :status)))
           (setq ,offset (max ,offset (1+ (- deadline now))))))
       ;; An exact, unique match fails visibly if production changes this seam.
       (should (string-match (regexp-quote ,line) mevedel-session-control-fs--program-script))
       (should-not (string-match (regexp-quote ,line) mevedel-session-control-fs--program-script
                                 (match-end 0)))
       (let ((mevedel-session-control-fs--program-script
              (string-replace ,line (concat ,line (format "      now=$((now + %d))\n" ,offset))
                              mevedel-session-control-fs--program-script)))
         (dolist (token ,tokens)
           (let ((directory (plist-get token :directory))
                 (deadline (plist-get token :expires-at)))
             (should (>= (mevedel-session-control-fs-target-time directory) deadline))
             (should (eq 'mismatch (plist-get
                                   (car (mevedel-session-control-fs-run-program
                                         (list (list :op 'before-time :path directory
                                                     :content (number-to-string deadline)))))
                                   :status)))))
         ,@body))))

(provide 'mevedel-journal-expiry-test-support)
;;; mevedel-journal-expiry-test-support.el ends here
