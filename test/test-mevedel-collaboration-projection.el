;;; test-mevedel-collaboration-projection.el --- Projection cost contracts -*- lexical-binding: t -*-

;;; Commentary:
;; Growing transcripts must not repeatedly recover every historical tool
;; block, and a retained projection must equal a full one.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-collaboration-projection)
(require 'mevedel-tool-render-data)

(mevedel-deftest mevedel-collaboration--canonical-records/bounded-history
  (:doc "stream updates preserve tool records with linear boundary searches")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let ((count 32)
          (scan (symbol-function 'mevedel-transcript--tool-block-end-from-start)))
      (dotimes (index count)
        (insert "#+begin_tool\n"
                (propertize
                 (format "(:name \"Read\" :args (:file_path \"/tmp/fixture\"))\nresult %d\n" index)
                 'gptel (cons 'tool (format "call-%d" index)))
                "#+end_tool\n\n"))
      (dotimes (_ 2)
        (goto-char (point-max))
        (insert (propertize "Another streamed sentence.\n" 'gptel 'response))
        (let ((scans 0) records)
          (cl-letf (((symbol-function 'mevedel-transcript--tool-block-end-from-start)
                     (lambda (&rest args)
                       (cl-incf scans)
                       (apply scan args))))
            (setq records (mevedel-collaboration--canonical-records (current-buffer))))
          (let ((tools (seq-filter (lambda (r) (equal "tool" (plist-get r :kind))) records)))
            (should (= count (length tools)))
            (dotimes (index count)
              (should (equal (format "result %d" index)
                             (plist-get (nth index tools) :result)))))
          (should (<= scans (* 4 count))))))))

(defun mevedel-test--projection-piece (index)
  "Return synthetic transcript text for piece INDEX.
Small value ranges repeat texts, so occurrence-numbered ids are exercised."
  (pcase (% index 7)
    (0 (format "Prompt %d about the parser.\n\n" (random 3)))
    (1 (propertize (format "Answer %d.\n\n" (random 3)) 'gptel 'response))
    (2 (concat "#+begin_tool\n"
               (propertize
                (format "(:name \"Read\" :args (:file_path \"/tmp/f%d\"))\n,* result %d\n"
                        index (random 3))
                'gptel (cons 'tool (format "call-%d-%d" index (random 1000))))
               "#+end_tool\n\n"))
    ;; No tool-use id: identity falls back to text and occurrence.
    (3 (concat "#+begin_tool\n"
               (propertize "(:name \"Bash\" :args (:command \"true\"))\ndone\n"
                           'gptel '(tool))
               "#+end_tool\n\n"))
    (4 (mevedel--format-hook-audit-record
        (list :type 'guest-prompt :name (format "guest-%d" (random 2)))))
    (5 (mevedel-tool-render-data-format
        '(:kind request-summary :elapsed-seconds 1 :outcome error
          :message "Provider unavailable")))
    (_ (propertize "Same answer.\n\n" 'gptel 'response))))

(defun mevedel-test--projection-mutate (step)
  "Apply one random transcript mutation; return its name for STEP."
  (let* ((size (buffer-size))
         (pos (1+ (random (max 1 size))))
         (kind (random 10)))
    (pcase kind
      (0 (goto-char (point-max))
         (insert (propertize (format "Streamed %d.\n" step) 'gptel 'response))
         'stream)
      (1 (goto-char (point-max))
         (insert (mevedel-test--projection-piece (random 7)))
         'append)
      (2 (goto-char pos)
         (insert (mevedel-test--projection-piece (random 7)))
         'insert)
      (3 (delete-region pos (min (point-max) (+ pos (random 24))))
         'delete)
      (4 (put-text-property pos (min (point-max) (+ pos 1 (random 40)))
                            'gptel (nth (random 3) '(response ignore nil)))
         'property)
      ;; Stream inserts and render-data patches run with hooks inhibited.
      (5 (with-silent-modifications
           (put-text-property pos (min (point-max) (+ pos 1 (random 40)))
                              'invisible t))
         'silent-property)
      (6 (when (< pos (point-max))
           (let ((props (text-properties-at pos)))
             (goto-char pos)
             (delete-char 1)
             (insert (apply #'propertize "Z" props))))
         'same-length)
      (7 (let ((text (buffer-string)))
           (erase-buffer)
           (insert text))
         'revert)
      (8 (erase-buffer)
         (dotimes (index (+ 3 (random 6)))
           (insert (mevedel-test--projection-piece index)))
         'reload)
      ;; The parse unescapes Org only in Org buffers.
      (_ (if (derived-mode-p 'org-mode) (fundamental-mode)
           (mevedel--transcript-org-mode))
         'mode))))

(mevedel-deftest mevedel-collaboration--canonical-records/retained
  (:doc "a retained projection equals a full one across transcript mutations")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let ((cache (mevedel-collaboration--projection-cache-create)))
      (random "mevedel-collaboration-retained-projection")
      (dotimes (index 14)
        (insert (mevedel-test--projection-piece index)))
      (dotimes (step 300)
        (let* ((before (buffer-string))
               (mutation (mevedel-test--projection-mutate step))
               (narrow (and (zerop (random 8)) (> (buffer-size) 2)))
               (from (and narrow (1+ (random (/ (buffer-size) 2)))))
               (to (and narrow (+ from 1 (random (- (point-max) from))))))
          (save-restriction
            (when narrow (narrow-to-region from to))
            ;; A mutation can break a tool block; both projections must
            ;; then fail alike.
            (let ((retained (condition-case err
                                (mevedel-collaboration--canonical-records
                                 (current-buffer) nil nil cache)
                              (error err)))
                  (full (condition-case err
                            (mevedel-collaboration--canonical-records
                             (current-buffer))
                          (error err))))
              (should (equal (list step mutation narrow retained)
                             (list step mutation narrow full)))
              ;; Undo a mutation that broke the transcript, so later steps
              ;; keep comparing records rather than the same error.
              (when (and (consp full) (symbolp (car full)) (get (car full) 'error-conditions))
                (widen)
                (erase-buffer)
                (insert before))
              ;; Callers rewrite ids and revisions in the records they get.
              (when (plist-get (car-safe retained) :id)
                (dolist (record retained)
                  (plist-put record :id "rewritten")
                  (plist-put record :revision 99)
                  (plist-put record :guest "rewritten"))))))))))

(mevedel-deftest mevedel-collaboration--canonical-records/streaming
  (:doc "a streamed sentence reprojects only the growing response")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (dotimes (index 8)
      (insert (format "Prompt %d\n\n" index)
              (propertize (format "Answer %d\n\n" index) 'gptel 'response)
              "#+begin_tool\n"
              (propertize (format "(:name \"Read\" :args (:file_path \"/tmp/%d\"))\nresult\n"
                                  index)
                          'gptel (cons 'tool (format "call-%d" index)))
              "#+end_tool\n\n"))
    (insert (propertize "Streaming" 'gptel 'response))
    (let ((cache (mevedel-collaboration--projection-cache-create))
          (parse (symbol-function 'mevedel-view--tool-call-parse))
          (clean (symbol-function 'mevedel-collaboration--clean-response))
          (parses 0) (cleans 0))
      (mevedel-collaboration--canonical-records (current-buffer) nil nil cache)
      (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
                 (lambda (&rest args) (cl-incf parses) (apply parse args)))
                ((symbol-function 'mevedel-collaboration--clean-response)
                 (lambda (&rest args) (cl-incf cleans) (apply clean args))))
        (let ((inhibit-modification-hooks t))
          (insert (propertize " more text." 'gptel 'response)))
        (let ((records (mevedel-collaboration--canonical-records
                        (current-buffer) nil nil cache)))
          (should (equal "Streaming more text."
                         (plist-get (car (last records)) :text)))
          (should (= 0 parses))
          (should (= 1 cleans)))
        ;; An unmodified buffer reprojects nothing at all.
        (mevedel-collaboration--canonical-records (current-buffer) nil nil cache)
        (should (= 1 cleans))))))

(mevedel-deftest mevedel-collaboration--canonical-records/live-inputs
  (:doc "a retained Bash record follows its execution without a text change")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "#+begin_tool\n"
            (propertize "(:name \"Bash\" :args (:command \"make\"))\nstarted\n"
                        'gptel '(tool . "bash-1"))
            "#+end_tool\n")
    (let ((cache (mevedel-collaboration--projection-cache-create))
          terminal)
      (cl-letf (((symbol-function 'mevedel-execution-transcript-pending-render-data)
                 (lambda (_buffer id) (and (equal id "bash-1") terminal))))
        (should (equal "started"
                       (plist-get (car (mevedel-collaboration--canonical-records
                                        (current-buffer) nil nil cache))
                                  :result)))
        (setq terminal '(:execution-output "finished"))
        (let ((retained (mevedel-collaboration--canonical-records
                         (current-buffer) nil nil cache)))
          (should (equal "finished" (plist-get (car retained) :result)))
          (should (equal retained (mevedel-collaboration--canonical-records
                                   (current-buffer)))))))))

(mevedel-deftest mevedel-collaboration--canonical-records/plan-mode
  (:doc "leaving plan mode reprojects a response it hid part of")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "Plan it\n\n"
            (propertize "Here is the plan:\n<proposed_plan>\n# Step one\n"
                        'gptel 'response))
    (let ((cache (mevedel-collaboration--projection-cache-create)))
      (setq-local mevedel--session (mevedel-session--create :name "plan"))
      (setf (mevedel-session-plan-mode mevedel--session) t)
      (mevedel-collaboration--canonical-records (current-buffer) nil nil cache)
      (setf (mevedel-session-plan-mode mevedel--session) nil)
      (should (equal (mevedel-collaboration--canonical-records (current-buffer))
                     (mevedel-collaboration--canonical-records
                      (current-buffer) nil nil cache))))))

(mevedel-deftest mevedel-collaboration--source-unchanged-p
  (:doc "detects character, property and accessibility changes")
  (with-temp-buffer
    (insert "ab" (propertize "cd" 'gptel 'response) "ef")
    (let ((source (mevedel-collaboration--source 2 6)))
      (should (mevedel-collaboration--source-unchanged-p 2 source))
      (should-not (mevedel-collaboration--source-unchanged-p 1 source))
      (put-text-property 4 5 'gptel 'ignore)
      (should-not (mevedel-collaboration--source-unchanged-p 2 source))
      (put-text-property 4 5 'gptel 'response)
      (should (mevedel-collaboration--source-unchanged-p 2 source))
      (save-restriction
        (narrow-to-region 3 7)
        (should-not (mevedel-collaboration--source-unchanged-p 2 source)))
      (goto-char 3)
      (delete-char 1)
      (insert "X")
      (should-not (mevedel-collaboration--source-unchanged-p 2 source)))))

(mevedel-deftest mevedel-collaboration--observe
  (:doc "records lookups only while a segment is projected")
  (progn
    (should (= 3 (mevedel-collaboration--observe #'+ 1 2)))
    (let ((mevedel-collaboration--observed (list nil)))
      (should (= 3 (mevedel-collaboration--observe #'+ 1 2)))
      (should (equal (list (list #'+ '(1 2) 3))
                     (car mevedel-collaboration--observed))))))

(mevedel-deftest mevedel-collaboration--reuse-record-ids
  (:doc "keeps ids along each kind's ordered stream")
  (let* ((old (list (list :id "a1" :kind "assistant")
                    (list :id "pending" :kind "tool" :pending t)
                    (list :id "t1" :kind "tool")))
         (new (list (list :id "x" :kind (copy-sequence "assistant"))
                    (list :id "fixed" :kind "tool" :identity-fixed t)
                    (list :id "y" :kind (copy-sequence "tool"))
                    (list :id "z" :kind "user"))))
    (should (equal '("a1" "fixed" "t1" "z")
                   (mapcar (lambda (record) (plist-get record :id))
                           (mevedel-collaboration--reuse-record-ids old new))))))

(mevedel-deftest mevedel-collaboration--room-canonical-records
  (:doc "a room retains its projection cache between calls")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "Prompt\n\n" (propertize "Answer\n" 'gptel 'response))
    (let* ((room (list :data-buffer (current-buffer)))
           (records (mevedel-collaboration--room-canonical-records room))
           (cache (plist-get room :projection-cache)))
      (should (mevedel-collaboration--projection-cache-p cache))
      (should (equal records (mevedel-collaboration--canonical-records
                              (current-buffer))))
      (mevedel-collaboration--room-canonical-records room)
      (should (eq cache (plist-get room :projection-cache))))))

(provide 'test-mevedel-collaboration-projection)
;;; test-mevedel-collaboration-projection.el ends here
