;;; niwa-instruction-test-support.el -- Shared instruction test fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Real-file note and store fixtures shared by owner tests.

;;; Code:

(require 'niwa-notes)

(defun niwa-instruction-test--create-note-in (buffer start end &optional span)
  "Create a note in BUFFER from START to END, with optional explicit SPAN.
Give unnamed geometry fixtures a real source file until BUFFER is killed."
  (with-current-buffer buffer
    (unless buffer-file-name
      (let ((file (make-temp-file "niwa-note-source-" nil ".txt")))
        (write-region (point-min) (point-max) file nil 'silent)
        (setq-local buffer-file-name file)
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (unlock-file file)
                    (when (file-exists-p file) (delete-file file))) t t)))
    (save-mark-and-excursion
      (let ((transient-mark-mode t)
            (mark-active t)
            (niwa-notes--unsaved-noticed
             (copy-sequence niwa-notes--unsaved-noticed)))
        (goto-char end)
        (set-mark start)
        (niwa-test--with-captured-diagnostics nil
          (let ((note (niwa-note-create "")))
            (when span
              (overlay-put note 'niwa-span span)
              (move-overlay note start end buffer)
              (niwa--instruction-normalize-lines note)
              (niwa--update-instruction-overlay note t))
            note))))))

(defun niwa-instruction-test--make-note (content workspace)
  "Create a file buffer containing CONTENT and one note in WORKSPACE."
  (let* ((file (make-temp-file "niwa-overlay-" nil ".txt" content))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (fundamental-mode)
      (setq-local niwa--workspace workspace)
      (set-buffer-modified-p nil)
      (cons buffer
            (niwa-instruction-test--create-note-in
             buffer (point-min) (1- (point-max)))))))

(defun niwa-instruction-test--discard (cell)
  "Kill and delete the file belonging to instruction CELL."
  (when-let* ((buffer (car cell))
              ((buffer-live-p buffer)))
    (let ((file (buffer-file-name buffer)))
      (with-current-buffer buffer
        (setq-local kill-buffer-hook nil)
        (set-buffer-modified-p nil))
      (kill-buffer buffer)
      ;; A change a hook rejects locks the file without modifying the
      ;; buffer, so killing it leaves that lock behind.
      (unlock-file file)
      (when (file-exists-p file)
        (delete-file file)))))

(defun niwa-instruction-test--store-state (workspace)
  "Return WORKSPACE's store state: `active', `new' or nil.
Nil means WORKSPACE keeps no store."
  (when (niwa-instruction-store--applicable-p workspace)
    (if (eq 'absent (plist-get (niwa-instruction-store--read workspace) :status))
        'new
      'active)))

(provide 'niwa-instruction-test-support)
;;; niwa-instruction-test-support.el ends here
