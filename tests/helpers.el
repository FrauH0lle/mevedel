;;; /home/roland/Projekte/mevedel/tests/helpers.el -- Helper functions for tests -*- lexical-binding: t -*-

(defun mevedel-tests--create-diff-buffer (original modified)
  (let* ((orig-temp-file (make-temp-file "mevedel-test-" nil ".txt" original))
         (modified-temp-file (make-temp-file "mevedel-test-" nil ".txt" modified))
         (rel-path (file-relative-name orig-temp-file (temporary-file-directory)))
         (buffer (get-buffer-create "mevedel-test-diff")))

    ;; Generate diff and append to result.
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-min))

      ;; Add the standard git diff header, which allows diff-mode to create new
      ;; files.
      (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))

      ;; Use diff to generate a unified patch with the correct file path.
      (when (or original modified)
        (call-process "diff"
                      nil t nil "-u" "--label"
                      (if original
                          (concat "a/" rel-path)
                        ;; Use /dev/null to denote file creations.
                        "/dev/null")
                      "--label"
                      (if modified
                          (concat "b/" rel-path)
                        ;; Use /dev/null to denote file deletions.
                        "/dev/null")
                      orig-temp-file modified-temp-file)))

    buffer))

(let ((orig "
line1
line2
line3
")
      (new "
line1
line3
line4
"))
  (mevedel-tests--create-diff-buffer orig new))

(when (re-search-forward "line2" nil t)
  (set-mark (match-beginning 0))
  )

(provide 'helpers)
;;; helpers.el ends here
