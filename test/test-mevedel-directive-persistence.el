;;; test-mevedel-directive-persistence.el -- Directive codec tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for the workspace directive record codec.

;;; Code:

(require 'mevedel-directive-persistence)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-directive-persistence-test--attempt ()
  "Return a complete attempt for directive codec tests."
  (mevedel-directive-attempt--create
   :sequence 1 :directive-request "Preserve this"
   :request "prompt" :result "result" :outcome 'success
   :patch "patch" :capture 'complete :covered-files nil :gaps nil
   :captured-at "2026-08-02T00:00:00+0200"
   :checkpoint '(:session-id "session" :turn 1)))

(mevedel-deftest mevedel--deserialize-directives/source-states
  (:vars ())
  ,test
  (test)
  :doc "round trips source-missing and archived records without source buffers"
  (let* ((directory (make-temp-file "mevedel-source-persist-" t))
         (workspace (mevedel-workspace--create
                     :type 'test :id directory :root directory
                     :name "persist"))
         (missing (mevedel-directive--create
                   :id "missing" :request "request"
                   :anchor (list :state 'source-missing
                                 :file (file-name-concat directory "gone.el")
                                 :start 1 :end 7
                                 :evidence '(:schema 1 :bodyless nil
                                             :text "target")
                                 :properties
                                 '(mevedel-instruction t
                                   mevedel-uuid "missing"
                                   mevedel-instruction-type directive))
                   :state nil :session-id nil :attempts nil :discussion nil))
         (archived (mevedel-directive--create
                    :id "archived" :request "history"
                    :anchor (list :state 'archived
                                  :file (file-name-concat directory "old.el")
                                  :start 2 :end 2
                                  :evidence '(:schema 1 :bodyless t)
                                  :properties
                                  '(mevedel-instruction t
                                    mevedel-uuid "archived"
                                    mevedel-instruction-type directive))
                    :state 'implemented :session-id nil
                    :attempts (list (mevedel-directive-persistence-test--attempt))
                    :discussion nil)))
    (unwind-protect
        (progn
          (mevedel-workspace-set-directives workspace (list missing archived))
          (let* ((serialized (mevedel--serialize-directives workspace directory))
                 (restored (mevedel--deserialize-directives serialized directory)))
            (should (equal '(source-missing archived)
                           (mapcar (lambda (record)
                                     (plist-get (mevedel-directive-anchor record)
                                                :state))
                                   restored)))))
      (delete-directory directory t))))


(provide 'test-mevedel-directive-persistence)
;;; test-mevedel-directive-persistence.el ends here
