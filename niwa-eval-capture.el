;;; niwa-eval-capture.el --- Shared child Eval capture -*- lexical-binding: t -*-

;;; Commentary:

;; Expanded into child scripts so they need no host tool stack.

;;; Code:

(require 'backtrace)
(require 'cl-lib)
(require 'seq)

(eval-and-compile
  (defconst niwa-tool-exec--eval-backtrace-frames 30
    "Maximum number of frames returned by Eval.")
  (defconst niwa-tool-exec--eval-backtrace-chars 8000
    "Maximum number of characters in an Eval backtrace, including its marker.")
  (defconst niwa-tool-exec--eval-backtrace-marker "\n[Backtrace truncated]"
    "Visible marker for an Eval backtrace limited by frames or characters."))

(defmacro niwa-tool-exec--eval-capture-backtrace ()
  "Capture a bounded signal-time backtrace in live or generated child code."
  `(ignore-errors
     (let ((frames (backtrace-get-frames))
           (print-circle t) (print-level 8) (print-length 20))
       (setq frames
             (or (memq (seq-find
                        (lambda (frame)
                          (memq (backtrace-frame-fun frame) '(error signal)))
                        frames)
                       frames)
                 frames))
       (let* ((text (backtrace-to-string
                     (seq-take frames ,niwa-tool-exec--eval-backtrace-frames)))
              (truncated (or (> (length frames) ,niwa-tool-exec--eval-backtrace-frames)
                             (> (length text) ,niwa-tool-exec--eval-backtrace-chars))))
         (if truncated
             (concat (substring text 0 (min (length text)
                                           ,(- niwa-tool-exec--eval-backtrace-chars
                                               (length niwa-tool-exec--eval-backtrace-marker))))
                     ,niwa-tool-exec--eval-backtrace-marker)
           text)))))

(defmacro niwa-eval-capture (form &optional bounded stream-messages)
  "Return printed result sections for FORM, limiting each when BOUNDED.
When STREAM-MESSAGES is non-nil, leave messages on stderr instead of capturing.
The expansion is self-contained for child scripts.  Use the message log:
batch Emacs does not run functions from the variable `set-message-functions'."
  (let ((stdout (make-symbol "stdout")) (messages (make-symbol "messages"))
        (clean (make-symbol "clean")) (result (make-symbol "result"))
        (failure (make-symbol "failure")) (backtrace (make-symbol "backtrace"))
        (err (make-symbol "err")))
    `(let ((,stdout (generate-new-buffer " *eval-output*"))
           (,messages (generate-new-buffer " *eval-messages*"))
           (,clean (lambda (text)
                     (when text
                       (setq text (substring-no-properties text))
                       (if (and ,bounded (> (length text) 16000))
                           (concat (substring text 0 16000) "\n[Output truncated]")
                         text))))
           (print-circle t) ,result ,failure ,backtrace)
       (unwind-protect
           (progn
             (condition-case ,err
                 (let ((standard-output ,stdout)
                       ,@(unless stream-messages
                           `((messages-buffer-name (buffer-name ,messages))
                             (message-log-max t) (inhibit-message t))))
                   (handler-bind
                       ((error (lambda (_err)
                                 (setq ,backtrace (niwa-tool-exec--eval-capture-backtrace)))))
                     (setq ,result ,form)))
               (error (setq ,failure ,err)))
             (list :value (funcall ,clean (format "%S" ,result))
                   :output (funcall ,clean (with-current-buffer ,stdout (buffer-string)))
                   :messages (funcall ,clean (with-current-buffer ,messages (buffer-string)))
                   :backtrace (funcall ,clean ,backtrace)
                   :error (and ,failure (funcall ,clean (format "%S: %S" (car ,failure) (cdr ,failure))))))
         (kill-buffer ,stdout) (kill-buffer ,messages)))))

(provide 'niwa-eval-capture)
;;; niwa-eval-capture.el ends here
