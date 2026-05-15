;;; test-mevedel-tool-ui.el --- Tests for mevedel-tool-ui.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-ui)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Ask User

(mevedel-deftest mevedel-tools--ask-user
  (:doc "advances after answers and submits from the review page")
  (let ((data-buffer (generate-new-buffer " *mev-ask-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-view*"))
        (choices '("Yes" "No"))
        rendered-body
        rendered-keymap
        result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--prompt--data-buffer)
                   (lambda () data-buffer))
                  ((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (&optional _data-buffer) view-buffer))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (descriptor)
                     (setq rendered-body (plist-get descriptor :body))
                     (setq rendered-keymap (plist-get descriptor :keymap))
                     (make-overlay (point-min) (point-min)
                                   (current-buffer) nil t)))
                  ((symbol-function 'mevedel--prompt--register-canceller)
                   #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (&rest _args)
                     (pop choices))))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tools--ask-user
           (lambda (value) (setq result value))
           [(:question "Use cache?" :options ["Yes" "No"])
            (:question "Run tests?" :options ["Yes" "No"])])
          (should (string-match-p "Question 1/2" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Question 2/2" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Review Your Answers" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Q1: Use cache\\?" result))
          (should (string-match-p "A1: Yes" result))
          (should (string-match-p "Q2: Run tests\\?" result))
          (should (string-match-p "A2: No" result)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer)))))


(provide 'test-mevedel-tool-ui)
;;; test-mevedel-tool-ui.el ends here
