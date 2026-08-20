;;; mevedel-instruction-test-support.el -- Shared instruction test fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Real-file reference, directive, and source fixtures shared by owner tests.

;;; Code:

(defun mevedel-instruction-test--make-reference (content workspace)
  "Create a file buffer containing CONTENT and one reference in WORKSPACE."
  (let* ((file (make-temp-file "mevedel-overlay-" nil ".txt" content))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (fundamental-mode)
      (setq-local mevedel--workspace workspace)
      (set-buffer-modified-p nil)
      (cons buffer
            (mevedel--create-reference-in
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
      (when (file-exists-p file)
        (delete-file file)))))

(defun mevedel-instruction-test--make-directive (content request workspace)
  "Create a file buffer containing CONTENT and one directive in WORKSPACE."
  (let* ((file (make-temp-file "mevedel-directive-" nil ".txt" content))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (fundamental-mode)
      (setq-local mevedel--workspace workspace)
      (set-buffer-modified-p nil)
      (cons buffer
            (mevedel--create-directive-in
             buffer (point-min) (1- (point-max)) nil request)))))

(defun mevedel-instruction-test--source-fixture ()
  "Return `(DIRECTORY WORKSPACE FILE BUFFER OVERLAY RECORD)' for a source file."
  (let* ((directory (make-temp-file "mevedel-source-" t))
         (file (file-name-concat directory "source.el"))
         (workspace (mevedel-workspace--create
                     :type 'file :id directory :root directory
                     :name "source")))
    (with-temp-file file
      (insert "before target after\n"))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (setq-local mevedel--workspace workspace)
        (let* ((start (progn (goto-char (point-min))
                             (search-forward "target")
                             (match-beginning 0)))
               (end (match-end 0))
               (overlay (mevedel--create-directive-in
                         buffer start end nil "Preserve this"))
               (record (mevedel--directive-record overlay)))
          (list directory workspace file buffer overlay record))))))

(defun mevedel-instruction-test--attempt ()
  "Return a complete directive attempt suitable for persistence tests."
  (mevedel-directive-attempt--create
   :sequence 1
   :action 'implement
   :directive-request "Preserve this"
   :request "prompt" :result "result" :outcome 'success
   :patch "patch" :capture 'complete :covered-files nil :gaps nil
   :captured-at "2026-08-02T00:00:00+0200"
   :checkpoint '(:session-id "session" :turn 1)))

(defun mevedel-instruction-test--discard-source (fixture)
  "Discard files and buffers owned by source FIXTURE."
  (mevedel-instruction-test--discard (cons (nth 3 fixture) nil))
  (when-let* ((directory (car fixture))
              ((file-directory-p directory)))
    (delete-directory directory t)))

(provide 'mevedel-instruction-test-support)
;;; mevedel-instruction-test-support.el ends here
