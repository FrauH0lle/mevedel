;;; test-niwa-view-twin.el --- Twin report orchestration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Test report paths without imposing absolute timing or allocation budgets.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view-twin
         (file-name-concat niwa-test--worktree-root "benchmark" "niwa-view-twin"))

(niwa-deftest niwa-view-twin--median ()
  ,test
  (test)
  :doc "reports the middle of five timings without changing their order"
  (let ((values '(5 1 3 2 4)))
    (should (= 3 (niwa-view-twin--median values)))
    (should (equal '(5 1 3 2 4) values))))

(niwa-deftest niwa-view-twin-run ()
  ,test
  (test)
  :doc "normal and observation reports use separate fresh-fixture passes"
  (let ((fixtures 0)
        (updates 0)
        (measurements 0)
        (observations 0))
    (cl-letf (((symbol-function 'niwa-gate--assert-compiled) #'ignore)
              ((symbol-function 'niwa-gate--with-scenario)
               (lambda (scenario consumer)
                 (cl-incf fixtures)
                 (funcall consumer (lambda () (cl-incf updates) scenario))))
              ((symbol-function 'niwa-twin-measure)
               (lambda (update)
                 (cl-incf measurements)
                 (funcall update)
                 '(:floor-deleted 1 :floor-inserted 1 :floor-property-runs 0
                   :real-seconds 0.002 :floor-seconds 0.001)))
              ((symbol-function 'niwa-twin-observe)
               (lambda (update)
                 (cl-incf observations)
                 (if (eq (funcall update) 'zone-tick)
                     '(:real-deleted 0 :real-inserted 0 :real-property-runs 3)
                   '(:real-deleted 20 :real-inserted 20 :real-property-runs 3)))))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (niwa-view-twin-run)
          (niwa-view-twin-run t))
        (should (string-match-p "stream-chunk normal-bytecode" (buffer-string)))
        (should (string-match-p "zone-tick normal-bytecode" (buffer-string)))
        (should (string-match-p "real-ms=2.000000 floor-ms=1.000000" (buffer-string)))
        (should (string-match-p "stream-chunk generic-call-counts" (buffer-string)))
        (should (string-match-p "zone-tick generic-call-counts" (buffer-string)))))
    (should (= 32 fixtures updates))
    (should (= 10 measurements))
    (should (= 2 observations))))

(provide 'test-niwa-view-twin)
;;; test-niwa-view-twin.el ends here
