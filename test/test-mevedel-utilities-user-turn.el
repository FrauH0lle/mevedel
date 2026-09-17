;;; test-mevedel-utilities-user-turn.el --- User turn insertion tests -*- lexical-binding: t -*-

;;; Commentary:

;; Transcript formatting shared by root prompt producers.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-chat)
(require 'mevedel-init)
(require 'mevedel-agents)
(require 'mevedel-review)
(require 'mevedel-directive-request)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-utilities)

(mevedel-deftest mevedel--insert-user-turn ()
  ,test
  (test)
  :doc "root producers preserve configured separators and prompt prefixes"
  (dolist (producer '(chat init review composer fork))
    (dolist (case '(("" "\n\n" "*** " "\n\n*** hello\n")
                    ("Answer" "\n\n" "*** " "Answer\n\n*** hello\n")
                    ("*** " "" "*** " "*** hello\n")
                    ("Answer" "" "*** " "Answer\n*** hello\n")
                    ("Answer" "" nil "Answerhello\n")
                    ("Answer" "" "" "Answerhello\n")
                    ("" "\n" "User:\n" "\nUser:\nhello\n")))
      (pcase-let ((`(,initial ,separator ,prefix ,expected) case))
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (insert (propertize initial 'gptel 'response))
            (setq-local gptel-response-separator separator
                        gptel-prompt-prefix-alist `((org-mode . ,prefix))))
          (cl-letf (((symbol-function 'gptel-send) #'ignore))
            (pcase producer
              ('chat
               (with-current-buffer data-buf
                 (mevedel--insert-local-user-turn "hello" nil nil nil t)))
              ('init (mevedel-init--send-direct "hello" data-buf))
              ('review (mevedel-review--record-direct-turn "hello" data-buf))
              ('composer
               (with-current-buffer view-buf
                 (mevedel-view--forward-input-now "hello")))
              ('fork
               (with-current-buffer view-buf
                 (mevedel-view--start-fork-skill-turn "hello" "hello")))))
          (with-current-buffer data-buf
            (should (equal expected (buffer-string)))
            (goto-char (point-max))
            (search-backward "hello")
            (should-not (get-text-property (point) 'gptel)))))))

  :doc "directive headers retain their boundary record and prompt drawer"
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-response-separator "\n\n"
                gptel-prompt-prefix-alist '((org-mode . "*** ")))
    (let ((marker (mevedel--insert-directive-turn
                   "d1" 2 "Change this" "Exact implementation prompt" 'implement)))
      (should (= (point-max) (marker-position marker)))
      (should (string-suffix-p
               "\n\n*** Change this :implement:\n:PROMPT:\nExact implementation prompt\n:END:\n"
               (buffer-string)))
      (goto-char (point-min))
      (search-forward "*** Change this")
      (should-not (get-text-property (1- (point)) 'gptel))
      (should (string-prefix-p
               (mevedel--format-hook-audit-record
                '(:type directive-turn-boundary :edge start
                  :directive-id "d1" :action implement :turn 2))
               (buffer-string)))))

  :doc "returns the body offset and retains only trusted transcript properties"
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-response-separator "\n\n"
                gptel-prompt-prefix-alist '((org-mode . "*** ")))
    (insert (propertize "Answer" 'gptel 'response))
    (let* ((binding '(:kind skill :token "$alpha"
                     :source-file "/tmp/alpha/SKILL.md"))
           (render (mevedel-tool-render-data-format
                    '(:kind inline-skill :name "alpha")))
           (input (concat (propertize "$alpha" 'mevedel-mention-binding binding
                                      'gptel 'response 'invisible t)
                          "\n" render))
           (body-start (mevedel--insert-user-turn input)))
      (should (= body-start (1+ (length "Answer\n\n*** "))))
      (should (= (point) (point-max)))
      (should (equal (concat input "\n")
                     (buffer-substring body-start (point))))
      (should (eq 'response (get-text-property (point-min) 'gptel)))
      (should (equal binding
                     (get-text-property body-start 'mevedel-mention-binding)))
      (should-not (get-text-property body-start 'gptel))
      (should-not (get-text-property body-start 'invisible))
      (goto-char (+ body-start (length "$alpha\n")))
      (search-forward "<!-- mevedel-render-data -->")
      (should (eq t (get-text-property (match-beginning 0) 'mevedel-render-data)))
      (should (eq 'mevedel-render-data
                  (get-text-property (match-beginning 0) 'gptel))))))

(provide 'test-mevedel-utilities-user-turn)
;;; test-mevedel-utilities-user-turn.el ends here
