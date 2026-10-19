;;; test-mevedel-permission-log.el --- Diagnostic persistence tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the durable diagnostic format through both log owners.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-hooks)
(require 'mevedel-permission-log)
(require 'mevedel-telemetry)

(mevedel-deftest mevedel-permission-log
  (:doc "both diagnostic logs retain nested data and stringify runtime objects")
  (let* ((root (make-temp-file "mevedel-diagnostic-codec-" t))
         (session (mevedel-session--create :save-path root))
         (mevedel-hooks-persist-log t)
         (mevedel-permission-log-enabled t)
         (mevedel-telemetry-enabled nil))
    (unwind-protect
        (with-temp-buffer
          (let* ((object (current-buffer))
                 (data (list :vector (vector object '(symbol . 42))
                             :nested (list :same "old" :same "new" :odd)
                             :object object))
                 (expected (list :vector (vector (format "%S" object) '(symbol . 42))
                                 :nested '(:same "new" :odd nil)
                                 :object (format "%S" object))))
            (mevedel-permission-log session 'codec :data data)
            (mevedel-hooks--log session (list :event 'codec :data data))
            (dolist (file (list (mevedel-permission-log-path session)
                               (mevedel-hooks-log-path session)))
              (with-temp-buffer
                (insert-file-contents file)
                (let ((entry (read (current-buffer))))
                  (should (eq 'codec (plist-get entry :event)))
                  (should (equal expected (plist-get entry :data))))
                (should (looking-at-p "\n\\'"))))
            (should (eq object (plist-get data :object)))
            (should (equal '(:same "old" :same "new" :odd)
                           (plist-get data :nested)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-permission-log--persist-content
  (:doc "creates missing parents once and appends UTF-8 independent of the caller")
  (let* ((root (make-temp-file "mevedel-permission-append-" t))
         (session (mevedel-session--create
                   :save-path (file-name-concat root "nested" "session")))
         (mevedel-permission-log-enabled t)
         (content "(:reason \"\u2603\")\n"))
    (unwind-protect
        (let ((coding-system-for-write 'utf-16le))
          (should (mevedel-permission-log--persist-content session content))
          (cl-letf (((symbol-function 'make-directory)
                     (lambda (&rest _) (ert-fail "Recreated existing directory"))))
            (should (mevedel-permission-log--persist-content session content)))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally (mevedel-permission-log-path session))
            (should (equal (encode-coding-string (concat content content) 'utf-8-unix)
                           (buffer-string)))))
      (delete-directory root t))))

(provide 'test-mevedel-permission-log)
;;; test-mevedel-permission-log.el ends here
