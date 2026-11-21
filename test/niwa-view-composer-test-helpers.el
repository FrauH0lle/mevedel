;;; niwa-view-composer-test-helpers.el --- Native composer fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Share the native composer fixture without registering any ERT cases.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'niwa-chat)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-render)

(defun niwa-view-composer-graphical--settle ()
  "Finish native window notifications and the fit they defer."
  (redisplay t)
  (sit-for 0.02)
  (redisplay t))

(defvar niwa-view-composer-graphical--allow-batch nil
  "Non-nil lets a layout-only test use the chat fixture in batch Emacs.")

(defmacro niwa-view-composer-graphical--with-chat (&rest body)
  "Show a real chat and run BODY with its `data-buf' and `view-buf'."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (or niwa-view-composer-graphical--allow-batch
                      (not noninteractive)))
     (save-window-excursion
       (delete-other-windows)
       (let* ((root (file-name-as-directory (make-temp-file "composer-chat-" t)))
              (niwa-user-dir (file-name-concat root "user/"))
              (niwa-plugin-extra-roots nil)
              (niwa-view-fontify-responses nil)
              (niwa-view-spinner-animate nil)
              (gptel-display-buffer-action
               '(display-buffer-same-window (body-function . select-window)))
              (workspace (niwa-workspace--create
                          :type 'project :id root :name "composer-test" :root root))
              (session (niwa-session-create "composer-test" workspace root))
              (data-buf (generate-new-buffer "*Composer test data*"))
              (view-buf (generate-new-buffer "*Composer test view*")))
         (unwind-protect
             (progn
               (setf (niwa-session-preset-name session) 'niwa-implement)
               (with-current-buffer data-buf
                 (niwa--transcript-org-mode)
                 (setq-local niwa--session session
                             niwa--workspace workspace
                             niwa-view--abort-function #'niwa-view-test--abort-interactions
                             default-directory root))
               (niwa-session-set-root-buffer session data-buf)
               (niwa-view-test--insert-data data-buf "An earlier response.\n" 'response)
               (niwa-view--setup view-buf data-buf)
               (with-current-buffer view-buf (niwa-view--full-rerender))
               (niwa--display-chat-buffer data-buf)
               (niwa-view-composer-graphical--settle)
               ,@body)
           (niwa-chat-discard-buffers data-buf t)
           (when (buffer-live-p view-buf) (kill-buffer view-buf))
           (delete-directory root t))))))

(defun niwa-view-composer-graphical--events (events command &optional count)
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

(defun niwa-view-composer-graphical--keys (keys command)
  "Type KEYS and wait for COMMAND to finish."
  (niwa-view-composer-graphical--events (listify-key-sequence (kbd keys)) command))

(defun niwa-view-composer-graphical--type (text)
  "Type a single line of TEXT as native self-insertion events."
  (niwa-view-composer-graphical--events
   (string-to-list text) 'self-insert-command (length text)))

(defun niwa-view-composer-graphical--draft (view)
  "Return the editable draft for VIEW, without display properties."
  (niwa-view-composer-draft-text view t))

(provide 'niwa-view-composer-test-helpers)
;;; niwa-view-composer-test-helpers.el ends here
