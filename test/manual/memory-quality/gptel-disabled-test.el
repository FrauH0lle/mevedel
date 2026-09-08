;;; gptel-disabled-test.el -- Real DeepSeek payload regression -*- lexical-binding: t -*-

;;; Commentary:

;; Checks the independent dependency fix without making provider requests.

;;; Code:
(require 'ert)
(require 'gptel)
(require 'gptel-openai-extras)

(ert-deftest deepseek-disabled-effort-payload ()
  (let* ((gptel-backend (gptel-make-deepseek "Quality" :key "unused"))
         (gptel-model 'deepseek-v4-flash))
    (dolist (effort '(nil disabled high max))
      (let* ((gptel-reasoning-effort effort)
             (payload (gptel--request-data gptel-backend '((:role "user" :content "fixture")))))
        (should (equal (plist-get (plist-get payload :thinking) :type)
                       (and effort (if (eq effort 'disabled) "disabled" "enabled"))))
        (if (memq effort '(nil disabled))
            (should-not (plist-member payload :reasoning_effort))
          (should (equal (plist-get payload :reasoning_effort) (symbol-name effort))))))))

(provide 'mevedel-memory-quality-gptel-disabled-test)
;;; gptel-disabled-test.el ends here
