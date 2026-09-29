;;; execution-ui-preview.el --- Standalone execution UX study -*- lexical-binding: t -*-

;;; Commentary:
;; Discussion prototype, not a production renderer.  All data is fictional.
;; Load this file and run M-x mevedel-execution-preview.
;; No processes, timers, advice, session edits, or theme changes are installed.

;;; Code:

(require 'button)
(require 'cl-lib)

(defvar-local mevedel-execution-preview--scenario 'success)
(defvar-local mevedel-execution-preview--variant 'all)
(defvar-local mevedel-execution-preview--open nil)

(defconst mevedel-execution-preview--fixtures
  '((running :marker "◌" :face warning :status "running · 3s"
             :output "Loading test files…\nRunning 24 tests…"
             :tail "Running 24 tests…")
    (success :marker "✓" :face success :status "4.3s"
             :output "Loading test files…\nRunning 24 tests…\n24 passed, 0 failed"
             :tail "24 passed, 0 failed")
    (failure :marker "×" :face error :status "exit 1 · 4.3s"
             :output "Loading test files…\nRunning 24 tests…\nFAIL output-is-not-duplicated\nExpected 1 output block, found 2\n23 passed, 1 failed"
             :tail "FAIL output-is-not-duplicated")
    (input :marker "✓" :face success :status "6.1s"
           :output "Refresh snapshots? [y/N]\ny\nSnapshots refreshed.\n24 passed, 0 failed"
           :tail "24 passed, 0 failed"))
  "Shared fictional observations for all three layouts.")

(defun mevedel-execution-preview--button (label action)
  "Insert a text button with LABEL invoking the zero-argument ACTION."
  (insert-text-button label 'follow-link t 'action (lambda (_) (funcall action)))
  (insert "  "))

(defun mevedel-execution-preview--toggle (key)
  "Toggle disclosure KEY, preserving the reader's approximate position."
  (let ((position (point)))
    (if (member key mevedel-execution-preview--open)
        (setq mevedel-execution-preview--open
              (remove key mevedel-execution-preview--open))
      (push key mevedel-execution-preview--open))
    (mevedel-execution-preview--render)
    (goto-char (min position (point-max)))))

(defun mevedel-execution-preview--section (variant kind label text)
  "Insert a disclosure for VARIANT and KIND, using LABEL and TEXT."
  (let* ((key (cons variant kind))
         (open (member key mevedel-execution-preview--open)))
    (insert "      ")
    (mevedel-execution-preview--button
     (concat (if open "▾ " "▸ ") label)
     (lambda () (mevedel-execution-preview--toggle key)))
    (insert "\n")
    (when open
      (dolist (line (split-string text "\n"))
        (insert "        " line "\n")))))

(defun mevedel-execution-preview--card (variant)
  "Insert one VARIANT using the selected scenario."
  (let* ((scenario mevedel-execution-preview--scenario)
         (facts (alist-get scenario mevedel-execution-preview--fixtures))
         (card-p (eq variant 'b))
         (output-open (member (cons variant 'output)
                              mevedel-execution-preview--open))
         (anchor (point))
         (command (if (eq scenario 'input)
                      "./run-tests --update-snapshots" "./run-tests")))
    (insert (propertize
             (pcase variant
               ('a "A  Minimal execution row")
               ('b "B  Result card with output preview")
               ('c "C  Minimal row + completion breadcrumb"))
             'face 'bold)
            "\n\n")
    (insert "  I’ll run the tests, then inspect the renderer.\n\n  ")
    (insert (propertize (concat (plist-get facts :marker) " ")
                        'face (plist-get facts :face)))
    (insert (propertize "Bash" 'face 'font-lock-function-name-face))
    (if card-p
        (insert "  " (propertize (plist-get facts :status) 'face 'shadow)
                "\n    " command "\n")
      (insert ": " command "  "
              (propertize (plist-get facts :status) 'face 'shadow) "\n"))
    (when (and card-p (not output-open))
      (insert "    │ " (propertize (plist-get facts :tail)
                                     'face (if (eq scenario 'failure)
                                               'error 'shadow)) "\n"))
    (when (eq scenario 'input)
      (insert "      ↳ Sent input: " (propertize "y ↵" 'face 'font-lock-string-face)
              "\n"))
    (mevedel-execution-preview--section
     variant 'output "Output" (plist-get facts :output))
    (mevedel-execution-preview--section
     variant 'details "Execution details"
     (concat "$ " command
             "\nWorking directory: /project/mevedel\nAgent: /root\nExecution: demo-exec-129\nSandbox: confined (fictional fixture)"
             (pcase scenario
               ('running "\nState: running")
               ('failure "\nState: exited; code 1")
               (_ "\nState: exited; code 0"))))
    (mevedel-execution-preview--section
     variant 'history "Execution history"
     (concat "0.0s  Bash started\n1.0s  Yielded; still running\n2.0s  WriteStdin: poll; no new output"
             (pcase scenario
               ('running "\n3.0s  WriteStdin: poll; new output added above")
               ('input "\n3.0s  WriteStdin: sent y + newline\n6.1s  Process exited; completion delivered")
               (_ "\n4.3s  WriteStdin: poll; terminal output added above\n4.3s  Completion delivered (no duplicate result)"))))
    (insert "\n  ✓ Read: mevedel-tool-exec.el\n"
            "  ✓ Grep: execution-output (3 matches)\n")
    (when (eq variant 'c)
      (unless (eq scenario 'running)
        (insert "\n  ↳ ")
        (insert (propertize
                 (if (eq scenario 'failure) "Bash failed" "Bash finished")
                 'face (if (eq scenario 'failure) 'error 'shadow)))
        (insert " · " command "  ")
        (mevedel-execution-preview--button
         "Show result"
         (lambda ()
           (cl-pushnew (cons variant 'output) mevedel-execution-preview--open
                       :test #'equal)
           (mevedel-execution-preview--render)
           (goto-char anchor)))
        (insert "\n")))
    (insert "\n" (propertize (make-string 62 ?─) 'face 'shadow) "\n\n")))

(defun mevedel-execution-preview--render ()
  "Render the selected layouts; all controls are local to the preview."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Background execution · design preview\n" 'face '(:height 1.2 :weight bold)))
    (insert (propertize "SIMULATED — no commands run; no session rendering changed\n\n" 'face 'shadow))
    (insert "Scenario: ")
    (dolist (entry '((running . "Running") (success . "Success")
                     (failure . "Failure") (input . "Sent input")))
      (let ((scenario (car entry)))
        (mevedel-execution-preview--button
         (if (eq scenario mevedel-execution-preview--scenario)
             (concat "[" (cdr entry) "]") (cdr entry))
         (lambda ()
           (setq mevedel-execution-preview--scenario scenario)
           (mevedel-execution-preview--render)))))
    (insert "\nLayout:   ")
    (dolist (entry '((all . "Compare all") (a . "A") (b . "B") (c . "C")))
      (let ((variant (car entry)))
        (mevedel-execution-preview--button
         (if (eq variant mevedel-execution-preview--variant)
             (concat "[" (cdr entry) "]") (cdr entry))
         (lambda ()
           (setq mevedel-execution-preview--variant variant)
           (mevedel-execution-preview--render)))))
    (insert "\n\nClick or RET on a control; TAB visits controls; q closes.\n"
            "All variants hide polling and retain one full output disclosure.\n"
            "C deliberately retains a later notice to test whether it earns its space.\n\n")
    (dolist (variant (if (eq mevedel-execution-preview--variant 'all)
                        '(a b c) (list mevedel-execution-preview--variant)))
      (mevedel-execution-preview--card variant))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(define-derived-mode mevedel-execution-preview-mode special-mode "Execution preview"
  "Read-only, standalone comparison of execution presentation variants."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local cursor-type 'box)
  (setq-local header-line-format " Execution UX study — fictional data, clickable disclosures")
  (local-set-key (kbd "TAB") #'forward-button)
  (local-set-key (kbd "<backtab>") #'backward-button))

(defun mevedel-execution-preview ()
  "Open the standalone execution design study without changing any session."
  (interactive)
  (let ((buffer (get-buffer-create "*Execution UI variants*")))
    (with-current-buffer buffer
      (mevedel-execution-preview-mode)
      (mevedel-execution-preview--render))
    (pop-to-buffer buffer)))

(provide 'execution-ui-preview)
;;; execution-ui-preview.el ends here
