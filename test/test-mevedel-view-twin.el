;;; test-mevedel-view-twin.el --- Twin report orchestration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Test report paths without imposing absolute timing or allocation budgets.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-twin
         (file-name-concat mevedel-test--worktree-root "benchmark" "mevedel-view-twin"))

(mevedel-deftest mevedel-view-twin--median ()
  ,test
  (test)
  :doc "reports the middle of five timings without changing their order"
  (let ((values '(5 1 3 2 4)))
    (should (= 3 (mevedel-view-twin--median values)))
    (should (equal '(5 1 3 2 4) values))))

(mevedel-deftest mevedel-view-twin-run ()
  ,test
  (test)
  :doc "normal and observation reports use separate fresh-fixture passes"
  (let ((fixtures 0)
        (updates 0)
        (measurements 0)
        (observations 0))
    (cl-letf (((symbol-function 'mevedel-gate--assert-compiled) #'ignore)
              ((symbol-function 'mevedel-gate--with-scenario)
               (lambda (_scenario consumer)
                 (cl-incf fixtures)
                 (funcall consumer (lambda () (cl-incf updates)))))
              ((symbol-function 'mevedel-twin-measure)
               (lambda (update)
                 (cl-incf measurements)
                 (funcall update)
                 '(:floor-deleted 1 :floor-inserted 1 :floor-property-runs 0
                   :real-seconds 0.002 :floor-seconds 0.001)))
              ((symbol-function 'mevedel-twin-observe)
               (lambda (update)
                 (cl-incf observations)
                 (funcall update)
                 '(:real-deleted 20 :real-inserted 20 :real-property-runs 3))))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (mevedel-view-twin-run)
          (mevedel-view-twin-run t))
        (should (string-match-p "stream-chunk normal-bytecode" (buffer-string)))
        (should (string-match-p "zone-tick normal-bytecode" (buffer-string)))
        (should (string-match-p "real-ms=2.000000 floor-ms=1.000000" (buffer-string)))
        (should (string-match-p "stream-chunk generic-call-counts" (buffer-string)))
        (should (string-match-p "zone-tick generic-call-counts" (buffer-string)))))
    (should (= 32 fixtures updates))
    (should (= 10 measurements))
    (should (= 2 observations))))

(provide 'test-mevedel-view-twin)
;;; test-mevedel-view-twin.el ends here
