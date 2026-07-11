;;; test-mevedel-tool-ui.el --- Interaction tool assembly tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the small user-interaction tool assembly boundary.  Focused Ask and
;; RequestAccess behavior lives in their mirrored test modules.

;;; Code:

(require 'gptel)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-ui)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-ui--deliver-result ()
  ,test
  (test)
  :doc "wraps raw results in the canonical envelope"
  (let (delivered)
    (mevedel-tool-ui--deliver-result
     (lambda (value) (setq delivered value))
     "done")
    (should (equal '(:result "done") delivered)))

  :doc "preserves result metadata envelopes"
  (let ((envelope '(:result "done" :render-data (:kind card)
                    :media ((:type image))))
        delivered)
    (mevedel-tool-ui--deliver-result
     (lambda (value) (setq delivered value))
     envelope)
    (should (eq envelope delivered))))

(mevedel-deftest mevedel-tool-ui--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "assembles the unchanged user-interaction tool surface"
  (progn
    (mevedel-tool-ui--register)
    (dolist (name '("Ask" "RequestAccess" "Agent" "StopAgent"
                    "ToolSearch" "SendMessage"))
      (should (mevedel-tool-get name))))

  :doc "keeps RequestAccess.directory as a semantic path"
  (progn
    (mevedel-tool-ui--register)
    (should
     (eq 'path
         (cadr (assq 'directory
                     (mevedel-tool-args
                      (mevedel-tool-get "RequestAccess"))))))))

(provide 'test-mevedel-tool-ui)

;;; test-mevedel-tool-ui.el ends here
