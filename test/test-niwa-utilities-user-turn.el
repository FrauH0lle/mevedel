;;; test-niwa-utilities-user-turn.el --- User turn insertion tests -*- lexical-binding: t -*-

;;; Commentary:

;; Transcript formatting shared by root prompt producers.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-chat)
(require 'niwa-init)
(require 'niwa-agents)
(require 'niwa-review)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-utilities)

(niwa-deftest niwa--insert-user-turn ()
  ,test
  (test)
  :doc "root producers keep the configured separator and start on their own line"
  (dolist (producer '(chat init review composer fork))
    (dolist (case '(("" "\n\n" "\n\nhello\n")
                    ("Answer" "\n\n" "Answer\n\nhello\n")
                    ("Answer" "\n" "Answer\nhello\n")
                    ("Answer" "" "Answer\nhello\n")
                    ("" "" "hello\n")))
      (pcase-let ((`(,initial ,separator ,expected) case))
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (insert (propertize initial 'gptel 'response))
            (setq-local gptel-response-separator separator))
          (cl-letf (((symbol-function 'gptel-send) #'ignore)
                    ((symbol-function 'gptel-request) #'ignore))
            (pcase producer
              ('chat
               (with-current-buffer data-buf
                 (niwa--insert-local-user-turn "hello" nil nil nil t)))
              ('init (niwa-init--send-direct "hello" data-buf))
              ('review (niwa-review--record-direct-turn "hello" data-buf))
              ('composer
               (with-current-buffer view-buf
                 (niwa-view--forward-input-now "hello")))
              ('fork
               (with-current-buffer view-buf
                 (niwa-view--start-fork-skill-turn "hello" "hello")))))
          (with-current-buffer data-buf
            (should (equal expected (buffer-string)))
            (goto-char (point-max))
            (search-backward "hello")
            (should-not (get-text-property (point) 'gptel)))))))

  :doc "returns the body offset and retains only trusted transcript properties"
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-response-separator "\n\n")
    (insert (propertize "Answer" 'gptel 'response))
    (let* ((binding '(:kind skill :token "$alpha"
                     :source-file "/tmp/alpha/SKILL.md"))
           (render (niwa-tool-render-data-format
                    '(:kind inline-skill :name "alpha")))
           (input (concat (propertize "$alpha" 'niwa-mention-binding binding
                                      'gptel 'response 'invisible t)
                          "\n" render))
           (body-start (niwa--insert-user-turn input)))
      (should (= body-start (1+ (length "Answer\n\n"))))
      (should (= (point) (point-max)))
      (should (equal (concat input "\n")
                     (buffer-substring body-start (point))))
      (should (eq 'response (get-text-property (point-min) 'gptel)))
      (should (equal binding
                     (get-text-property body-start 'niwa-mention-binding)))
      (should-not (get-text-property body-start 'gptel))
      (should-not (get-text-property body-start 'invisible))
      (goto-char (+ body-start (length "$alpha\n")))
      (search-forward "<!-- niwa-render-data -->")
      (should (eq t (get-text-property (match-beginning 0) 'niwa-render-data)))
      (should (eq 'niwa-render-data
                  (get-text-property (match-beginning 0) 'gptel))))))

(provide 'test-niwa-utilities-user-turn)
;;; test-niwa-utilities-user-turn.el ends here
