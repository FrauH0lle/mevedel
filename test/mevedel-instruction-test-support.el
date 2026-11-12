;;; mevedel-instruction-test-support.el -- Shared instruction test fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Real-file note and store fixtures shared by owner tests.

;;; Code:

(require 'mevedel-notes)

(defun mevedel-instruction-test--create-note-in (buffer start end &optional span)
  "Create a note in BUFFER from START to END, with optional explicit SPAN.
Give unnamed geometry fixtures a real source file until BUFFER is killed."
  (with-current-buffer buffer
    (unless buffer-file-name
      (let ((file (make-temp-file "mevedel-note-source-" nil ".txt")))
        (write-region (point-min) (point-max) file nil 'silent)
        (setq-local buffer-file-name file)
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (unlock-file file)
                    (when (file-exists-p file) (delete-file file))) t t)))
    (save-mark-and-excursion
      (let ((transient-mark-mode t)
            (mark-active t)
            (mevedel-notes--unsaved-noticed
             (copy-sequence mevedel-notes--unsaved-noticed)))
        (goto-char end)
        (set-mark start)
        (mevedel-test--with-captured-diagnostics nil
          (let ((note (mevedel-note-create "")))
            (when span
              (overlay-put note 'mevedel-span span)
              (move-overlay note start end buffer)
              (mevedel--instruction-normalize-lines note)
              (mevedel--update-instruction-overlay note t))
            note))))))

(defun mevedel-instruction-test--make-note (content workspace)
  "Create a file buffer containing CONTENT and one note in WORKSPACE."
  (let* ((file (make-temp-file "mevedel-overlay-" nil ".txt" content))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (fundamental-mode)
      (setq-local mevedel--workspace workspace)
      (set-buffer-modified-p nil)
      (cons buffer
            (mevedel-instruction-test--create-note-in
             buffer (point-min) (1- (point-max)))))))

(defun mevedel-instruction-test--discard (cell)
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

(defun mevedel-instruction-test--store-state (workspace)
  "Return WORKSPACE's store state: `active', `new' or nil.
Nil means WORKSPACE keeps no store."
  (when (mevedel-instruction-store--applicable-p workspace)
    (if (eq 'absent (plist-get (mevedel-instruction-store--read workspace) :status))
        'new
      'active)))

(provide 'mevedel-instruction-test-support)
;;; mevedel-instruction-test-support.el ends here
