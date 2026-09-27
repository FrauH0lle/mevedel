;;; test-mevedel-transcript-scan.el --- Resumable transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-transcript)
(require 'mevedel-tool-render-data)

(mevedel-deftest mevedel-transcript-scan-step ()
  ,test
  (test)
  :doc "yields within a full-source scan and preserves crossing container trust"
  (with-temp-buffer
    (org-mode)
    (insert "Prompt\n")
    (let ((start (point)))
      (insert (mevedel-tool-render-data-format
               '(:kind inline-skill :display-text "skill"
                 :prompt "<system-reminder>\nquoted\n</system-reminder>\n")))
      (put-text-property start (point) 'gptel 'mevedel-render-data))
    (dotimes (i 96)
      (insert (propertize (format "Answer %d\n" i) 'gptel 'response))
      (insert (propertize "#+begin_reasoning\nthought\n#+end_reasoning\n"
                          'gptel 'ignore)))
    (let* ((expected (mevedel-transcript-segments (point-min) (point-max)))
           (job (mevedel-transcript-scan-start (point-min) (point-max)))
           (steps 0))
      (while (not (mevedel-transcript-scan-step job))
        (cl-incf steps)
        (should (< steps 500)))
      (should (> steps 2))
      (should (equal expected (mevedel-transcript-scan-result job)))
      (should (eq (caar (cdr (mevedel-transcript-scan-result job)))
                  'render-data))
      (mevedel-transcript-scan-cancel job)))

  :doc "expanded partial bounds and recovered tool blocks match canonical output"
  (with-temp-buffer
    (org-mode)
    (insert (propertize "Response\n" 'gptel 'response))
    (let ((start (point)))
      (insert "#+begin_tool (Read :file_path \"a\")\n"
              "(:name \"Read\" :args (:file_path \"a\"))\n\n"
              "body\n#+end_tool\n")
      (put-text-property (+ start 10) (- (point) 12)
                         'gptel '(tool . "call_1"))
      (insert (propertize "End\n" 'gptel 'response))
      (let* ((from (+ start 15))
             (to (+ start 30))
             (expected (mevedel-transcript-segments from to))
             (job (mevedel-transcript-scan-start from to)))
        (while (not (mevedel-transcript-scan-step job))
          (should-not (mevedel-transcript-scan-result job)))
        (should (equal expected (mevedel-transcript-scan-result job))))))

  :doc "retains source narrowing across steps even when caller widens"
  (with-temp-buffer
    (org-mode)
    (insert (propertize "Outside before\n" 'gptel 'response))
    (let ((from (point)))
      (insert "Inside prompt\n")
      (insert (propertize "Inside answer\n" 'gptel 'response))
      (let ((to (point)))
        (insert (propertize "Outside after\n" 'gptel 'response))
        (narrow-to-region from to)
        (let* ((expected (mevedel-transcript-segments (point-min) (point-max)))
               (job (mevedel-transcript-scan-start (point-min) (point-max))))
          (widen)
          (with-temp-buffer
            (while (not (mevedel-transcript-scan-step job))))
          (should (equal expected (mevedel-transcript-scan-result job)))
          (should (equal '(user response)
                         (mapcar #'car (mevedel-transcript-scan-result job))))))))

  :doc "indexes long prose without control markers across source slices"
  (with-temp-buffer
    (org-mode)
    (insert (propertize (make-string 160000 ?x) 'gptel 'response))
    (let ((job (mevedel-transcript-scan-start (point-min) (point-max)))
          (steps 0))
      (while (not (mevedel-transcript-scan-step job))
        (cl-incf steps)
        (should (< steps 40)))
      (should (> steps 15))
      (should (equal (list (list 'response (point-min) (point-max)))
                     (mevedel-transcript-scan-result job)))))

  :doc "finds a control opener crossing the candidate-index slice boundary"
  (with-temp-buffer
    (org-mode)
    (insert (make-string 16374 ?x) "\n")
    (let ((begin (point)))
      (insert "<system-reminder>\nremember\n</system-reminder>\n")
      (put-text-property begin (point) 'gptel 'ignore))
    (let ((job (mevedel-transcript-scan-start (point-min) (point-max))))
      (while (not (mevedel-transcript-scan-step job)))
      (should (equal '(user reminder)
                     (mapcar #'car (mevedel-transcript-scan-result job))))))

  :doc "cancellation and property-only edits invalidate unfinished results"
  (with-temp-buffer
    (org-mode)
    (insert "Prompt\nResponse\n")
    (let ((job (mevedel-transcript-scan-start (point-min) (point-max))))
      (should-not (mevedel-transcript-scan-result job))
      (mevedel-transcript-scan-step job)
      (put-text-property 2 4 'gptel 'response)
      (should-error (mevedel-transcript-scan-step job))
      (mevedel-transcript-scan-cancel job)
      (should-not (mevedel-transcript-scan-result job)))))

(provide 'test-mevedel-transcript-scan)
;;; test-mevedel-transcript-scan.el ends here
