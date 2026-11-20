;;; mevedel-view-composer-test-helpers.el --- Native composer fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Share the native composer fixture without registering any ERT cases.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'mevedel-chat)
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

(provide 'mevedel-view-composer-test-helpers)
;;; mevedel-view-composer-test-helpers.el ends here
