;;; test-niwa-transcript-scan.el --- Resumable transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-transcript)
(require 'niwa-tool-render-data)

(niwa-deftest niwa-transcript-scan-step ()
  ,test
  (test)
  :doc "yields within a full-source scan and preserves crossing container trust"
  (with-temp-buffer
    (org-mode)
    (insert "Prompt\n")
    (let ((start (point)))
      (insert (niwa-tool-render-data-format
               '(:kind inline-skill :display-text "skill"
                 :prompt "<system-reminder>\nquoted\n</system-reminder>\n")))
      (put-text-property start (point) 'gptel 'niwa-render-data))
    (dotimes (i 96)
      (insert (propertize (format "Answer %d\n" i) 'gptel 'response))
      (insert (propertize "#+begin_reasoning\nthought\n#+end_reasoning\n"
                          'gptel 'ignore)))
    (let* ((expected (niwa-transcript-segments (point-min) (point-max)))
           (job (niwa-transcript-scan-start (point-min) (point-max)))
           (steps 0))
      (while (not (niwa-transcript-scan-step job))
        (cl-incf steps)
        (should (< steps 500)))
      (should (> steps 2))
      (should (equal expected (niwa-transcript-scan-result job)))
      (should (eq (caar (cdr (niwa-transcript-scan-result job)))
                  'render-data))
      (niwa-transcript-scan-cancel job)))

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
             (expected (niwa-transcript-segments from to))
             (job (niwa-transcript-scan-start from to)))
        (while (not (niwa-transcript-scan-step job))
          (should-not (niwa-transcript-scan-result job)))
        (should (equal expected (niwa-transcript-scan-result job))))))

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
        (let* ((expected (niwa-transcript-segments (point-min) (point-max)))
               (job (niwa-transcript-scan-start (point-min) (point-max))))
          (widen)
          (with-temp-buffer
            (while (not (niwa-transcript-scan-step job))))
          (should (equal expected (niwa-transcript-scan-result job)))
          (should (equal '(user response)
                         (mapcar #'car (niwa-transcript-scan-result job))))))))

  :doc "indexes long prose without control markers across source slices"
  (with-temp-buffer
    (org-mode)
    (insert (propertize (make-string 160000 ?x) 'gptel 'response))
    (let ((job (niwa-transcript-scan-start (point-min) (point-max)))
          (steps 0))
      (while (not (niwa-transcript-scan-step job))
        (cl-incf steps)
        (should (< steps 40)))
      (should (> steps 15))
      (should (equal (list (list 'response (point-min) (point-max)))
                     (niwa-transcript-scan-result job)))))

  :doc "finds a control opener crossing the candidate-index slice boundary"
  (with-temp-buffer
    (org-mode)
    (insert (make-string 16374 ?x) "\n")
    (let ((begin (point)))
      (insert "<system-reminder>\nremember\n</system-reminder>\n")
      (put-text-property begin (point) 'gptel 'ignore))
    (let ((job (niwa-transcript-scan-start (point-min) (point-max))))
      (while (not (niwa-transcript-scan-step job)))
      (should (equal '(user reminder)
                     (mapcar #'car (niwa-transcript-scan-result job))))))

  :doc "cancellation and property-only edits invalidate unfinished results"
  (with-temp-buffer
    (org-mode)
    (insert "Prompt\nResponse\n")
    (let ((job (niwa-transcript-scan-start (point-min) (point-max))))
      (should-not (niwa-transcript-scan-result job))
      (niwa-transcript-scan-step job)
      (put-text-property 2 4 'gptel 'response)
      (should-error (niwa-transcript-scan-step job))
      (niwa-transcript-scan-cancel job)
      (should-not (niwa-transcript-scan-result job)))))

(provide 'test-niwa-transcript-scan)
;;; test-niwa-transcript-scan.el ends here
