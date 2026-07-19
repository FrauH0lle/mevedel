;;; test-mevedel-transcript-restore.el --- Transcript restoration tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for persisted gptel property restoration.

;;; Code:

(require 'mevedel-transcript-restore)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `gptel'
(defvar gptel-mode)

;; `org'
(declare-function org-entry-get
                  "org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "org" (pom property value))


;;
;;; Bounds sanitation

(mevedel-deftest mevedel-transcript-restore-sanitize-bounds ()
  ,test
  (test)
  :doc "clamps stale GPTEL_BOUNDS ranges before gptel restore"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: ((response (2 999) (999 1000)) (ignore (1 2)))\n"
            ":END:\n"
            "Body\n")
    (mevedel-transcript-restore-sanitize-bounds)
    (let* ((max (point-max))
           (bounds (read (org-entry-get (point-min) "GPTEL_BOUNDS")))
           (response (alist-get 'response bounds)))
      (should (= max (cadar response)))
      (should (= 1 (length response)))
      (dolist (entry bounds)
        (dolist (range (cdr entry))
          (should (<= (cadr range) max))))))
  :doc "deletes unreadable GPTEL_BOUNDS"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: #<marker>\n"
            ":END:\n"
            "Body\n")
    (mevedel-transcript-restore-sanitize-bounds)
    (should-not (org-entry-get (point-min) "GPTEL_BOUNDS")))
  :doc "does not mark buffers modified while clamping bounds"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: ((response (2 999)))\n"
            ":END:\n"
            "Body\n")
    (set-buffer-modified-p nil)
    (mevedel-transcript-restore-sanitize-bounds)
    (should-not (buffer-modified-p))
    (pcase-let ((`((response (,beg ,end)))
                 (read (org-entry-get (point-min) "GPTEL_BOUNDS"))))
      (should (= beg 2))
      (should (= end (point-max))))))


;;
;;; Property restoration

(mevedel-deftest mevedel-transcript-restore-properties-present-p ()
  ,test
  (test)
  :doc "detects gptel properties anywhere in the requested range"
  (with-temp-buffer
    (insert "plain response tail")
    (put-text-property 7 (point-max) 'gptel 'response)
    (should (mevedel-transcript-restore-properties-present-p
             (point-min) (point-max)))
    (should-not (mevedel-transcript-restore-properties-present-p 1 7))))

(mevedel-deftest mevedel-transcript-restore-properties ()
  ,test
  (test)
  :doc "hydrates persisted bounds then applies canonical structural properties"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
            "User prompt\n"
            "#+begin_reasoning\nThinking.\n#+end_reasoning\n"
            "Assistant answer.\n")
    (dotimes (_ 3)
      (save-excursion
        (goto-char (point-min))
        (search-forward "Assistant answer.")
        (org-entry-put
         (point-min) "GPTEL_BOUNDS"
         (prin1-to-string
          `((response (,(match-beginning 0) ,(match-end 0))))))))
    (set-buffer-modified-p nil)
    (mevedel-transcript-restore-properties)
    (should-not (buffer-modified-p))
    (goto-char (point-min))
    (search-forward "Thinking.")
    (should (eq (get-text-property (match-beginning 0) 'gptel) 'ignore))
    (search-forward "Assistant answer.")
    (should (eq (get-text-property (match-beginning 0) 'gptel) 'response)))
  :doc "does not overwrite newer live properties when restoring only if missing"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
            "Old persisted response.\n"
            "New live response.\n")
    (dotimes (_ 3)
      (save-excursion
        (goto-char (point-min))
        (search-forward "Old persisted response.")
        (org-entry-put
         (point-min) "GPTEL_BOUNDS"
         (prin1-to-string
          `((response (,(match-beginning 0) ,(match-end 0))))))))
    (goto-char (point-min))
    (search-forward "New live response.")
    (put-text-property (match-beginning 0) (match-end 0)
                       'gptel 'response)
    (mevedel-transcript-restore-properties t)
    (goto-char (point-min))
    (search-forward "Old persisted response.")
    (should-not (get-text-property (match-beginning 0) 'gptel))
    (search-forward "New live response.")
    (should (eq (get-text-property (match-beginning 0) 'gptel) 'response))))

(mevedel-deftest mevedel-transcript-restore-gptel-state ()
  ,test
  (test)
  :doc "does not dirty resumed buffers while repairing bounds and properties"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: ((response (2 999)))\n"
            ":END:\n"
            "#+begin_tool\n"
            "(:name \"Bash\" :args (:command \"true\"))\n"
            "ok\n"
            "#+end_tool\n"
            "Focused tests passed\n")
    (setq-local gptel-mode nil)
    (set-buffer-modified-p nil)
    (cl-letf (((symbol-function 'gptel-mode)
               (lambda (&optional _arg)
                 (setq-local gptel-mode t)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "#+begin_tool")
                   (let ((tool-start (match-beginning 0)))
                     (search-forward "sed tests")
                     (add-text-properties
                      tool-start (match-beginning 0)
                      '(gptel (tool . "stale"))))
                   (goto-char (point-min))
                   (search-forward "sed tests")
                   (add-text-properties
                    (match-beginning 0) (point-max)
                    '(gptel response))))))
      (mevedel-transcript-restore-gptel-state))
    (should-not (buffer-modified-p))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Focused tests")
      (should (eq (get-text-property (match-beginning 0) 'gptel)
                  'response)))))

(provide 'test-mevedel-transcript-restore)
;;; test-mevedel-transcript-restore.el ends here
