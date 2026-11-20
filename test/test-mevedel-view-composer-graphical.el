;;; test-mevedel-view-composer-graphical.el --- Native docked composer tests -*- lexical-binding: t -*-

;;; Commentary:

;; Show a real chat view and edit through Emacs's command loop, on a
;; graphical frame or on the terminal-check pseudo-terminal.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'mevedel-chat)
(require 'mevedel-hooks)
(require 'mevedel-pending-inputs)
(require 'mevedel-skills-core)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)

(defmacro mevedel-view-composer-graphical--with-chat (&rest body)
  "Show a real chat and run BODY with its `data-buf' and `view-buf'."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (not noninteractive))
     (save-window-excursion
       (delete-other-windows)
       (let* ((root (file-name-as-directory (make-temp-file "composer-chat-" t)))
              (mevedel-user-dir (file-name-concat root "user/"))
              (mevedel-plugin-extra-roots nil)
              (mevedel-view-fontify-responses nil)
              (mevedel-view-spinner-animate nil)
              (gptel-display-buffer-action
               '(display-buffer-same-window (body-function . select-window)))
              (workspace (mevedel-workspace--create
                          :type 'project :id root :name "composer-test" :root root))
              (session (mevedel-session-create "composer-test" workspace root))
              (data-buf (generate-new-buffer "*Composer test data*"))
              (view-buf (generate-new-buffer "*Composer test view*")))
         (unwind-protect
             (progn
               (setf (mevedel-session-preset-name session) 'mevedel-implement)
               (with-current-buffer data-buf
                 (mevedel--transcript-org-mode)
                 (setq-local mevedel--session session
                             mevedel--workspace workspace
                             mevedel-view--abort-function #'mevedel-view-test--abort-interactions
                             default-directory root))
               (mevedel-session-set-root-buffer session data-buf)
               (mevedel-view-test--insert-data data-buf "An earlier response.\n" 'response)
               (mevedel-view--setup view-buf data-buf)
               (with-current-buffer view-buf (mevedel-view--full-rerender))
               (mevedel--display-chat-buffer data-buf)
               (redisplay t)
               ,@body)
           (mevedel-chat-discard-buffers data-buf t)
           (when (buffer-live-p view-buf) (kill-buffer view-buf))
           (delete-directory root t))))))

(defun mevedel-view-composer-graphical--events (events command &optional count)
  "Run EVENTS through the command loop and observe COUNT runs of COMMAND."
  (let* ((remaining (or count 1))
         (hook (lambda ()
                 (when (and (eq this-command command)
                            (zerop (cl-decf remaining)))
                   (run-at-time 0 nil #'exit-recursive-edit))))
         (deadline (run-at-time 5 nil #'exit-recursive-edit))
         (unread-command-events events))
    (add-hook 'post-command-hook hook)
    (unwind-protect (recursive-edit)
      (remove-hook 'post-command-hook hook)
      (cancel-timer deadline))
    (should (= remaining 0))
    (redisplay t)))

(defun mevedel-view-composer-graphical--keys (keys command)
  "Type KEYS and wait for COMMAND to finish."
  (mevedel-view-composer-graphical--events (listify-key-sequence (kbd keys)) command))

(defun mevedel-view-composer-graphical--type (text)
  "Type a single line of TEXT as native self-insertion events."
  (mevedel-view-composer-graphical--events
   (string-to-list text) 'self-insert-command (length text)))

(defun mevedel-view-composer-graphical--draft (view)
  "Return the editable draft for VIEW, without display properties."
  (mevedel-view-composer-draft-text view t))

(ert-deftest mevedel-view-composer-graphical/docked-typing ()
  "Show selects an ordinary composer split and typing stays out of the transcript."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((composer (mevedel-view-composer-buffer view-buf))
           (window (selected-window))
           (transcript (get-buffer-window view-buf (selected-frame)))
           (view-text (with-current-buffer view-buf (buffer-string))))
      (should (buffer-live-p composer))
      (should-not (eq composer view-buf))
      (should (eq (window-buffer window) composer))
      (should (eq (window-in-direction 'below transcript) window))
      (should (= (window-pixel-left window) (window-pixel-left transcript)))
      (should (= (window-pixel-width window) (window-pixel-width transcript)))
      (should-not (window-parameter window 'window-side))
      (should (string-search "An earlier response." view-text))
      (should-not (mevedel-pending-inputs--event-wake-refusal session data-buf view-buf))
      (mevedel-view-composer-graphical--type "> unsent draft")
      (mevedel-view-composer-graphical--keys "RET" 'newline)
      (mevedel-view-composer-graphical--type "second line")
      (should (eq (selected-window) window))
      (should (equal "> unsent draft\nsecond line"
                     (mevedel-view-composer-graphical--draft view-buf)))
      (should (eq 'composer-draft
                  (mevedel-pending-inputs--event-wake-refusal session data-buf view-buf)))
      (should (equal view-text (with-current-buffer view-buf (buffer-string)))))))

(ert-deftest mevedel-view-composer-graphical/send-and-history ()
  "Send folds the echo and clears input; history restores an unfinished draft."
  :tags '(mevedel-docked-composer)
  (mevedel-test--with-captured-diagnostics nil
    (mevedel-view-composer-graphical--with-chat
      (let ((mevedel-view-user-input-collapse-line-threshold 1)
            (submitted "Submitted once\nA folded line")
            requests)
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _) (push (current-buffer) requests))))
          (mevedel-view-composer-graphical--type "Submitted once")
          (mevedel-view-composer-graphical--keys "RET" 'newline)
          (mevedel-view-composer-graphical--type "A folded line")
          (mevedel-view-composer-graphical--keys "C-c RET" 'mevedel-view-send))
        (should (equal requests (list data-buf)))
        (should (equal "" (mevedel-view-composer-graphical--draft view-buf)))
        (with-current-buffer data-buf
          (should (string-search submitted (buffer-string))))
        (with-current-buffer view-buf
          (should (string-search "Submitted once (+1 line)" (buffer-string)))
          (should-not (string-search "A folded line" (buffer-string))))
        (mevedel-view-composer-graphical--type "Keep this unfinished draft")
        (mevedel-view-composer-graphical--keys "M-p" 'mevedel-view-history-previous)
        (should (equal submitted (mevedel-view-composer-graphical--draft view-buf)))
        (mevedel-view-composer-graphical--keys "M-n" 'mevedel-view-history-next)
        (should (equal "Keep this unfinished draft"
                       (mevedel-view-composer-graphical--draft view-buf)))))))

(ert-deftest mevedel-view-composer-graphical/completion ()
  "Native completion extends mention, skill and slash tokens in the composer."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let ((suggest-key-bindings nil)
          (skill (file-name-concat root ".mevedel/skills/graphical-skill/SKILL.md")))
      (make-directory (file-name-directory skill) t)
      (with-temp-file skill
        (insert "---\nname: graphical-skill\ndescription: A completion fixture.\n---\n\nReview the fixture.\n"))
      (mevedel-skills-install session data-buf)
      (dolist (pair '(("@fi" . "@file:") ("$graphical-sk" . "$graphical-skill")
                      ("/hel" . "/help")))
        (mevedel-view-composer-replace "" view-buf)
        (mevedel-view-composer-graphical--type (car pair))
        (mevedel-view-composer-graphical--events
         (append (kbd "M-x") "completion-at-point" (listify-key-sequence (kbd "RET")))
         'completion-at-point)
        (should (equal (cdr pair)
                       (string-trim-right
                        (mevedel-view-composer-graphical--draft view-buf))))))))

(ert-deftest mevedel-view-composer-graphical/async-send-keeps-new-input ()
  "An accepted prompt leaves edits made while UserPromptSubmit is pending."
  (mevedel-test--with-captured-diagnostics nil
    (mevedel-view-composer-graphical--with-chat
      (let* (;; User command hooks run from the rule source directory.
             (user-emacs-directory root)
             (release (file-name-concat root "release-hook"))
             (mevedel-hook-rules
              `((UserPromptSubmit
                 ((:matcher "*"
                   :hooks ((:type command :timeout 10 :fail-closed t
                            :command ,(format "while [ ! -e %s ]; do sleep .02; done; printf '{}'"
                                              (shell-quote-argument release)))))))))
             (window (selected-window))
             requests)
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _) (push (current-buffer) requests))))
          (unwind-protect
              (progn
                (mevedel-view-composer-graphical--type "First draft")
                (mevedel-view-composer-graphical--keys "C-c RET" 'mevedel-view-send)
                (should-not requests)
                (mevedel-view-composer-graphical--keys "C-c C-u" 'mevedel-view-history-clear-input)
                (mevedel-view-composer-graphical--type "> later draft")
                (mevedel-view-composer-graphical--keys "RET" 'newline)
                (mevedel-view-composer-graphical--type "second line")
                (let ((caret (point)))
                  (write-region "" nil release nil 'silent)
                  (let ((deadline (+ (float-time) 5)))
                    (while (and (not requests) (< (float-time) deadline))
                      (accept-process-output nil 0.02)))
                  (ert-info ((format "Hook log: %S; diagnostics: %s"
                                     (mevedel-session-hook-log session)
                                     mevedel-test--captured))
                    (should (equal requests (list data-buf)))
                    (should (eq (plist-get (car (last (mevedel-session-hook-log session)))
                                           :status)
                                'ok)))
                  (should (equal "> later draft\nsecond line"
                                 (mevedel-view-composer-graphical--draft view-buf)))
                  (should (eq window (selected-window)))
                  (should (= caret (point)))
                  (with-current-buffer data-buf
                    (should (string-search "First draft" (buffer-string)))
                    (should-not (string-search "later draft" (buffer-string))))))
            (unless requests
              (with-current-buffer view-buf
                (mevedel-view--cancel-pending-submission))
              (write-region "" nil release nil 'silent))))))))

(provide 'test-mevedel-view-composer-graphical)
;;; test-mevedel-view-composer-graphical.el ends here
