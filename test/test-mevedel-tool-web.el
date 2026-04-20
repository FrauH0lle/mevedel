;;; test-mevedel-tool-web.el --- Tests for mevedel-tool-web.el -*- lexical-binding: t -*-

;;; Commentary:

;; Registration-only tests: the web tools now wrap gptel-agent's
;; upstream structs via :wrap, so the assertions focus on the wrap
;; metadata rather than on any local handler function.

;;; Code:

(require 'mevedel-tool-registry)
(require 'gptel-request)
(require 'gptel-agent-tools)
(require 'mevedel-tool-web)
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
  :doc "registers WebSearch as wrapped read-only web tool"
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "WebSearch" "mevedel-gptel-agent")))
      (should tool)
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (memq 'web (mevedel-tool-groups tool)))
      (let ((arg-names (mapcar #'car (mevedel-tool-args tool))))
        (should (memq 'query arg-names)))))

  :doc "registers WebFetch with max-result-size"
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "WebFetch" "mevedel-gptel-agent")))
      (should tool)
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (= 50000 (mevedel-tool-max-result-size tool)))))

  :doc "registers YouTube tool"
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "YouTube" "mevedel-gptel-agent")))
      (should tool)
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (= 50000 (mevedel-tool-max-result-size tool)))))

  :doc "all three tools share the web group"
  (progn
    (mevedel-tool-web--register)
    (let ((web-tools (mevedel-tool-for-groups '(web))))
      (should (<= 3 (length web-tools)))
      (should (cl-every (lambda (tool) (mevedel-tool-read-only-p tool))
                        web-tools))))

  :doc "wrap leaves the upstream gptel-agent entries in place"
  (progn
    (mevedel-tool-web--register)
    (should (gptel-get-tool '("gptel-agent" "WebSearch")))
    (should (gptel-get-tool '("gptel-agent" "WebFetch")))
    (should (gptel-get-tool '("gptel-agent" "YouTube")))))

(provide 'test-mevedel-tool-web)
;;; test-mevedel-tool-web.el ends here
