;;; test-mevedel-queue.el -- Tests for shared queue engine -*- lexical-binding: t -*-

;;; Commentary:

;; Focused coverage for the private FIFO helper introduced by spec 25.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-structs)
(require 'mevedel-queue)

(defun test-mevedel-queue--session ()
  "Return a minimal session for queue tests."
  (mevedel-session--create
   :name "test"
   :workspace nil
   :permission-queue nil
   :plan-queue nil))

(defun test-mevedel-queue--spec (rendered-cell outcomes-cell &optional error-on)
  "Return a queue spec that records into RENDERED-CELL and OUTCOMES-CELL.
ERROR-ON, when non-nil, is an outcome whose settlement should
signal after recording."
  (mevedel-queue-spec--create
   :name 'test-queue
   :get (lambda (session) (mevedel-session-permission-queue session))
   :set (lambda (session queue)
          (setf (mevedel-session-permission-queue session) queue))
   :render (lambda (entry)
             (setcar rendered-cell
                     (cons (plist-get entry :id) (car rendered-cell))))
   :settle (lambda (entry outcome)
             (setcar outcomes-cell
                     (cons (cons (plist-get entry :id) outcome)
                           (car outcomes-cell)))
             (when (eq outcome error-on)
               (error "settle failed")))
   :entry-origin (lambda (entry) (plist-get entry :origin))))

(mevedel-deftest mevedel-queue--enqueue
  (:doc "shared queue enqueue contract")
  ,test
  (test)

  :doc "no-session enqueue warns and settles aborted without rendering"
  (let ((mevedel--session nil)
        (rendered (cons nil nil))
        (outcomes (cons nil nil)))
    (mevedel-queue--enqueue
     (test-mevedel-queue--spec rendered outcomes)
     (list :id 'a))
    (should (null (car rendered)))
    (should (equal '((a . aborted)) (car outcomes)))))

(mevedel-deftest mevedel-queue--pop
  (:doc "shared queue pop contract")
  ,test
  (test)

  :doc "pop removes head before settling and renders next exactly once"
  (let* ((session (test-mevedel-queue--session))
         (mevedel--session session)
         (rendered (cons nil nil))
         (outcomes (cons nil nil))
         (spec (test-mevedel-queue--spec rendered outcomes)))
    (mevedel-queue--enqueue spec (list :id 'a))
    (mevedel-queue--enqueue spec (list :id 'b))
    (setcar rendered nil)
    (mevedel-queue--pop spec (car (mevedel-session-permission-queue session))
                        'done)
    (should (equal '((a . done)) (car outcomes)))
    (should (equal '(b) (car rendered)))
    (should (= 1 (length (mevedel-session-permission-queue session))))))

(mevedel-deftest mevedel-queue--abort-all
  (:doc "shared queue abort contract")
  ,test
  (test)

  :doc "callback errors warn but do not stop sibling cleanup"
  (let* ((session (test-mevedel-queue--session))
         (mevedel--session session)
         (rendered (cons nil nil))
         (outcomes (cons nil nil))
         (spec (test-mevedel-queue--spec rendered outcomes 'aborted)))
    (mevedel-queue--enqueue spec (list :id 'a))
    (mevedel-queue--enqueue spec (list :id 'b))
    (mevedel-queue--abort-all spec 'aborted session)
    (should (null (mevedel-session-permission-queue session)))
    (should (= 2 (length (car outcomes))))))

(mevedel-deftest mevedel-queue--sweep-origin
  (:doc "shared queue origin sweep contract")
  ,test
  (test)

  :doc "sweep settles matching origins and preserves non-matching entries"
  (let* ((session (test-mevedel-queue--session))
         (mevedel--session session)
         (rendered (cons nil nil))
         (outcomes (cons nil nil))
         (spec (test-mevedel-queue--spec rendered outcomes)))
    (mevedel-queue--enqueue spec (list :id 'a :origin "agent"))
    (mevedel-queue--enqueue spec (list :id 'b :origin "main"))
    (mevedel-queue--enqueue spec (list :id 'c :origin "agent"))
    (mevedel-queue--sweep-origin spec "agent" 'aborted session)
    (should (equal '((c . aborted) (a . aborted)) (car outcomes)))
    (should (= 1 (length (mevedel-session-permission-queue session))))
    (should (eq 'b (plist-get (car (mevedel-session-permission-queue session))
                              :id)))))

(provide 'test-mevedel-queue)
;;; test-mevedel-queue.el ends here
