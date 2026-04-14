;;; test-mevedel-tool-web.el --- Tests for mevedel-tool-web.el -*- lexical-binding: t -*-

;;; Commentary:

;; Registration-only tests: handlers delegate directly to gptel-agent
;; functions, which are covered by gptel-agent's own tests.

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-tool-web)
(require 'gptel-request)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Registration

(mevedel-deftest mevedel-tool-web--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers WebSearch as read-only async web tool"
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "WebSearch" "mevedel")))
      (should tool)
      (should (eq #'mevedel-tool-web--search (mevedel-tool-handler tool)))
      (should (eq t (mevedel-tool-async-p tool)))
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (memq 'web (mevedel-tool-groups tool)))
      (let ((arg-names (mapcar #'car (mevedel-tool-args tool))))
        (should (memq 'query arg-names))
        (should (memq 'count arg-names)))))

  :doc "registers WebFetch with max-result-size"
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "WebFetch" "mevedel")))
      (should tool)
      (should (eq #'mevedel-tool-web--fetch (mevedel-tool-handler tool)))
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (= 50000 (mevedel-tool-max-result-size tool)))
      (should (equal '(url) (mapcar #'car (mevedel-tool-args tool))))))

  :doc "registers YouTube tool"
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "YouTube" "mevedel")))
      (should tool)
      (should (eq #'mevedel-tool-web--youtube (mevedel-tool-handler tool)))
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (= 50000 (mevedel-tool-max-result-size tool)))))

  :doc "all three tools share the web group"
  (progn
    (mevedel-tool-web--register)
    (let ((web-tools (mevedel-tool-for-groups '(web))))
      (should (= 3 (length web-tools)))
      (should (cl-every (lambda (tool) (mevedel-tool-read-only-p tool))
                        web-tools)))))


;;
;;; Handler arg validation

(mevedel-deftest mevedel-tool-web--search
  (:doc "`mevedel-tool-web--search' requires :query")
  (should-error (mevedel-tool-web--search (lambda (_) nil) '())
                :type 'error))

(mevedel-deftest mevedel-tool-web--fetch
  (:doc "`mevedel-tool-web--fetch' requires :url")
  (should-error (mevedel-tool-web--fetch (lambda (_) nil) '())
                :type 'error))

(mevedel-deftest mevedel-tool-web--youtube
  (:doc "`mevedel-tool-web--youtube' requires :url")
  (should-error (mevedel-tool-web--youtube (lambda (_) nil) '())
                :type 'error))

(provide 'test-mevedel-tool-web)
;;; test-mevedel-tool-web.el ends here
