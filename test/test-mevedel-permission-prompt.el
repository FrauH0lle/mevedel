;;; test-mevedel-permission-prompt.el --- Permission prompt tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests generic, Bash, and Eval permission prompt rendering and controls.

;;; Code:

(require 'cl-lib)
(require 'mevedel-interaction-prompt)
(require 'mevedel-permission-prompt)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Controls

(mevedel-deftest mevedel-permission--prompt-approve-once
  ()
  ,test
  (test)
  :doc "settles the interaction-zone permission prompt from its real text"
  (with-temp-buffer
    (let* ((mevedel-view--interaction-descriptors
            (make-hash-table :test #'equal))
           (mevedel-view--interaction-overlays
            (make-hash-table :test #'equal))
           (received nil)
           (id '(:permission real-text)))
      (insert "interaction\n\n> ")
      (let ((ov (make-overlay (point-min) (1+ (point-min)) nil t)))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-view-interaction-id id)
        (overlay-put ov 'priority 100)
        (overlay-put ov 'mevedel--callback
                     (lambda (outcome) (setq received outcome)))
        (push ov mevedel--prompt-overlays)
        (puthash id ov mevedel-view--interaction-overlays)
        (puthash id (list :id id) mevedel-view--interaction-descriptors)
        (goto-char (point-min))
        (let ((last-command-event ?a))
          (call-interactively #'mevedel-permission--prompt-approve-once))
        (should (eq received 'allow-once))
        (should-not (gethash id mevedel-view--interaction-overlays)))))

  :doc "falls back to normal typing when no permission prompt is active"
  (with-temp-buffer
    (let ((last-command-event ?a))
      (call-interactively #'mevedel-permission--prompt-approve-once))
    (should (equal (buffer-string) "a"))))

(mevedel-deftest mevedel-permission--prompt-approve-session
  (:doc "does not settle prompts that suppress session allow")
  (with-temp-buffer
    (let (received)
      (insert "prompt")
      (let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-permission-suppress-allow-session t)
        (overlay-put ov 'mevedel--callback
                     (lambda (outcome) (push outcome received)))
        (push ov mevedel--prompt-overlays)
        (goto-char (point-min))
        (mevedel-permission--prompt-approve-session)
        (should-not received)
        (should (overlay-buffer ov))))))


;;
;;; Rendering

(mevedel-deftest mevedel-permission--prompt-body
  ()
  ,test
  (test)
  :doc "includes session allow by default"
  (cl-letf (((symbol-function 'gptel-agent--block-bg)
             (lambda () 'default)))
    (should (string-match-p
             "allow-session"
             (mevedel-permission--prompt-body "Body\n" nil))))

  :doc "suppresses session allow without suppressing session deny"
  (cl-letf (((symbol-function 'gptel-agent--block-bg)
             (lambda () 'default)))
    (let ((body (mevedel-permission--prompt-body "Body\n" nil t)))
      (should-not (string-match-p "allow-session" body))
      (should (string-match-p "deny-session" body)))))

(mevedel-deftest mevedel-permission--prompt-async-with-content
  ()
  ,test
  (test)
  :doc "suppressed session allow reaches the body and local keymap"
  (with-temp-buffer
    (let ((target (current-buffer))
          captured-body
          captured-keymap)
      (cl-letf (((symbol-function 'gptel-agent--block-bg)
                 (lambda () 'default))
                ((symbol-function 'mevedel--prompt--data-buffer)
                 (lambda (&optional _buffer) target))
                ((symbol-function 'mevedel-view--interaction-target-buffer)
                 (lambda (_data-buffer) target))
                ((symbol-function 'mevedel-view--interaction-register)
                 (lambda (plist)
                   (setq captured-body (plist-get plist :body))
                   (setq captured-keymap (plist-get plist :keymap))
                   (make-overlay (point-min) (point-min))))
                ((symbol-function 'mevedel--prompt--register-canceller)
                 #'ignore))
        (mevedel-permission--prompt-async-with-content
         "Body\n" t #'ignore nil nil t))
      (should-not (string-match-p "allow-session" captured-body))
      (should (eq (lookup-key captured-keymap (kbd "RET"))
                  #'mevedel-permission--prompt-approve-once))
      (should-not (lookup-key captured-keymap "s"))
      (should (lookup-key captured-keymap "A"))))

  :doc "does not bind permission actions globally in view mode"
  (dolist (key (list (kbd "RET") (kbd "TAB") "a" "s" "A" "d" "D" "f"))
    (should-not (lookup-key mevedel-view-mode-map key))))

(mevedel-deftest mevedel-permission--prompt-async-eval
  (:doc "accepts RET for allow-once")
  (with-temp-buffer
    (let ((target (current-buffer))
          captured-body
          captured-keymap)
      (cl-letf (((symbol-function 'gptel-agent--block-bg)
                 (lambda () 'default))
                ((symbol-function 'mevedel--prompt--data-buffer)
                 (lambda (&optional _buffer) target))
                ((symbol-function 'mevedel-view--interaction-target-buffer)
                 (lambda (_data-buffer) target))
                ((symbol-function 'mevedel-view--interaction-register)
                 (lambda (plist)
                   (setq captured-body (plist-get plist :body))
                   (setq captured-keymap (plist-get plist :keymap))
                   (make-overlay (point-min) (point-min))))
                ((symbol-function 'mevedel--prompt--register-canceller)
                 #'ignore))
        (mevedel-permission--prompt-async-eval "Eval\n" #'ignore))
      (should (string-match-p "RET" captured-body))
      (should (eq (lookup-key captured-keymap (kbd "RET"))
                  #'mevedel-permission--prompt-approve-once))
      (should-not (string-match-p "allow-session" captured-body))
      (should-not (string-match-p "deny-session" captured-body))
      (should-not (lookup-key captured-keymap "s"))
      (should-not (lookup-key captured-keymap "A"))
      (should-not (lookup-key captured-keymap "D")))))

(mevedel-deftest mevedel-permission--format-bash-guardian
  ()
  ,test
  (test)
  :doc "formats guardian risk guidance"
  (let ((text
         (substring-no-properties
          (mevedel-permission--format-bash-guardian
           '(:risk high
             :recommendation deny
             :reason "Downloads and executes remote code.")))))
    (should (string-match-p "Risk: High" text))
    (should (string-match-p "Recommendation: Deny" text))
    (should (string-match-p "Downloads and executes remote code" text)))

  :doc "formats pending guidance"
  (let ((text (substring-no-properties
               (mevedel-permission--format-bash-guardian nil 'pending))))
    (should (string-match-p "Status: Analyzing command risk" text))
    (should-not (string-match-p "Risk:" text)))

  :doc "formats unavailable guidance"
  (let ((text (substring-no-properties
               (mevedel-permission--format-bash-guardian
                nil 'unavailable))))
    (should (string-match-p "Unavailable" text))
    (should-not (string-match-p "Risk:" text))))

(mevedel-deftest mevedel-permission--prompt-async-bash
  ()
  ,test
  (test)
  :doc "dangerous prompts suppress session and persistent allow"
  (let (captured-include captured-suppress captured-content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (content include-always _cont &optional _count _entry
                                suppress-allow-session)
                 (setq captured-content content)
                 (setq captured-include include-always)
                 (setq captured-suppress suppress-allow-session))))
      (mevedel-permission--prompt-async-bash
       "sudo pwd" t t nil #'ignore nil
       (list :allow-patterns '("sudo pwd"))))
    (should-not captured-include)
    (should captured-suppress)
    (should (string-match-p
             "Session/permanent allow is disabled" captured-content))
    (should-not (string-match-p
                 "Session/always allow will add" captured-content)))

  :doc "shows counted detected command summaries"
  (let (captured-content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-with-content)
               (lambda (content _include-always _cont &optional _count _entry
                                _suppress-allow-session)
                 (setq captured-content (substring-no-properties content)))))
      (mevedel-permission--prompt-async-bash
       "git add -- a && git add -- b" nil t nil #'ignore nil
       (list :commands '("git" "git")
             :commands-summary "git (2)")))
    (should (string-match-p "Detected commands: git (2)" captured-content))
    (should-not (string-match-p "git, git" captured-content))))

(provide 'test-mevedel-permission-prompt)

;;; test-mevedel-permission-prompt.el ends here
