;;; test-mevedel-allocation-gate.el --- Allocation harness tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Test counter arithmetic and scenario guards, not machine-specific budgets.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-allocation-gate
         (file-name-concat
          (file-name-directory (directory-file-name
                                (file-name-directory (or load-file-name buffer-file-name))))
          "benchmark" "mevedel-allocation-gate"))

(mevedel-deftest mevedel-gate--net ()
  ,test
  (test)
  :doc "subtracts each overhead counter and clamps negative results"
  (should (equal (mevedel-gate--net
                  '(:conses 30 :string-chars 4 :vector-cells 10 :gcs 0)
                  '(:conses 7 :string-chars 8 :vector-cells 2 :gcs 1))
                 '(:conses 23 :string-chars 0 :vector-cells 8 :gcs 0))))

(mevedel-deftest mevedel-gate-measure ()
  ,test
  (test)
  :doc "subtracts the componentwise minimum of three no-op calibrations"
  (let ((samples '((:conses 9 :string-chars 8 :vector-cells 7 :gcs 0)
                   (:conses 7 :string-chars 9 :vector-cells 8 :gcs 0)
                   (:conses 8 :string-chars 7 :vector-cells 9 :gcs 0)
                   (:conses 37 :string-chars 27 :vector-cells 17 :gcs 2)))
        (calls nil)
        (operation (lambda () 'operation)))
    (cl-letf (((symbol-function 'mevedel-gate--raw)
               (lambda (function)
                 (push function calls)
                 (pop samples))))
      (should (equal (mevedel-gate-measure operation)
                     '(:conses 30 :string-chars 20 :vector-cells 10 :gcs 2))))
    (should-not samples)
    (should (equal (nreverse calls) (list #'ignore #'ignore #'ignore operation)))))

(mevedel-deftest mevedel-gate--raw ()
  ,test
  (test)
  :doc "counts real allocations and collections around one operation"
  (let* ((ran nil)
         (sample (mevedel-gate--raw
                  (lambda ()
                    (setq ran (cons (make-string 100 ?x) (make-vector 20 nil)))
                    (garbage-collect)))))
    (should ran)
    (should (> (plist-get sample :conses) 0))
    (should (>= (plist-get sample :string-chars) 100))
    (should (>= (plist-get sample :vector-cells) 20))
    (should (= (plist-get sample :gcs) 1))))

(mevedel-deftest mevedel-gate-check ()
  ,test
  (test)
  :doc "accepts the exact limit and reports scenario, actual and budget on breach"
  (should-not (mevedel-gate-check 'example '(:conses 30) 30))
  (let ((failure (should-error
                  (mevedel-gate-check 'example '(:conses 31) 30))))
    (should (string-match-p
             "example: 31 conses > 30 budget"
             (error-message-string failure)))))

(mevedel-deftest mevedel-gate--assert-compiled ()
  ,test
  (test)
  :doc "rejects an interpreted measuring helper before any scenario runs"
  (cl-letf (((symbol-function 'mevedel-gate--raw) (lambda (_) nil)))
    (should-error (mevedel-gate--assert-compiled))))

(mevedel-deftest mevedel-gate--with-scenario (:quiet t)
  ,test
  (test)
  :doc "every scenario reaches its real update and rejects a skipped update"
  (dolist (scenario '(stream-chunk composer-keystroke zone-tick))
    (mevedel-gate--with-scenario scenario #'funcall)
    (should-error (mevedel-gate--with-scenario scenario #'ignore))))

(mevedel-deftest mevedel-benchmark--insert-history (:quiet t)
  ,test
  (test)
  :doc "inserts response history without losing the editable composer"
  (mevedel-benchmark--with-view
   (lambda (_data view)
     (with-current-buffer view
       (let ((start (marker-position (mevedel-view--history-insertion-marker))))
         (mevedel-benchmark--insert-history 32)
         (should (= 32 (- (mevedel-view--history-insertion-marker) start)))
         (should (eq 'response (get-text-property start 'mevedel-view-type)))
         (goto-char (mevedel-view--input-start))
         (insert "> draft\nsecond")
         (should (equal "> draft\nsecond" (mevedel-view--input-text))))))))

(provide 'test-mevedel-allocation-gate)
;;; test-mevedel-allocation-gate.el ends here
