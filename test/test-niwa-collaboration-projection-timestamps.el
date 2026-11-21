;;; test-niwa-collaboration-projection-timestamps.el --- Request-summary projection -*- lexical-binding: t -*-

;;; Commentary:
;; A saved hidden request summary reaches the browser as the host-rendered
;; closing line of its assistant turn, scoped and identified like other
;; canonical records.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'gptel)
(require 'niwa-collaboration-projection)
(require 'niwa-gptel-retry)
(require 'niwa-session-artifacts)
(require 'niwa-shared-conversation)
(require 'niwa-tool-render-data)
(require 'niwa-transcript-restore)
(require 'niwa-view-render)

(defconst niwa-projection-timestamps-test--summary-data
  '(:kind request-summary :elapsed-seconds 75
          :started-at 1761730000.0 :finished-at 1761730075.0)
  "A successful request summary as the host saves it.")

(defun niwa-projection-timestamps-test--insert-turn (answer summary)
  "Insert a prompt, ANSWER response and hidden SUMMARY render-data."
  (insert "Prompt.\n\n")
  (insert (propertize (concat answer "\n\n") 'gptel 'response))
  (insert (niwa-tool-render-data-format summary) "\n"))

(defun niwa-projection-timestamps-test--summary-records (records)
  "Return the request-summary records in RECORDS."
  (cl-remove-if-not (lambda (record)
                      (equal (plist-get record :kind) "request-summary"))
                    records))

(defun niwa-projection-timestamps-test--summary-text (summary)
  "Return the host's closing line for SUMMARY without text properties."
  (substring-no-properties (niwa-view--request-summary-line summary)))

(niwa-deftest niwa-collaboration--canonical-records/timestamps ()
  ,test
  (test)
  :doc "a saved successful summary projects the host-rendered closing line"
  (with-temp-buffer
    (niwa-projection-timestamps-test--insert-turn
     "Answer." niwa-projection-timestamps-test--summary-data)
    (let* ((records (niwa-collaboration--canonical-records (current-buffer)))
           (summaries (niwa-projection-timestamps-test--summary-records records))
           (summary (car summaries))
           (json (niwa-collaboration--json-record summary)))
      (should (= 1 (length summaries)))
      (should (eq summary (car (last records))))
      (should (equal-including-properties
               (plist-get summary :text)
               (substring-no-properties
                (niwa-view--request-summary-line
                 niwa-projection-timestamps-test--summary-data))))
      (should (equal (plist-get summary :details)
                     (niwa-view--request-summary-details
                      niwa-projection-timestamps-test--summary-data)))
      (should (stringp (plist-get summary :id)))
      (should (equal (mapcar #'car json)
                     '("id" "kind" "revision" "text" "details")))))
  :doc "an old elapsed-only summary projects its line without timing details"
  (with-temp-buffer
    (niwa-projection-timestamps-test--insert-turn
     "Answer." '(:kind request-summary :elapsed-seconds 3))
    (let* ((summary (car (niwa-projection-timestamps-test--summary-records
                          (niwa-collaboration--canonical-records
                           (current-buffer))))))
      (should (equal (plist-get summary :text)
                     (niwa-projection-timestamps-test--summary-text
                      '(:kind request-summary :elapsed-seconds 3))))
      (should-not (plist-member summary :details))
      (should-not (assoc "details" (niwa-collaboration--json-record summary)))))
  :doc "a summary without a closing line or failure projects nothing"
  (with-temp-buffer
    (niwa-projection-timestamps-test--insert-turn
     "Answer." '(:kind request-summary :outcome success))
    (let ((records (niwa-collaboration--canonical-records (current-buffer))))
      (should (equal '("user" "assistant")
                     (mapcar (lambda (record) (plist-get record :kind)) records)))))
  :doc "a failure keeps its record identity and gains the closing line after it"
  (with-temp-buffer
    (let* ((failure (append '(:outcome error :message "Provider unavailable"
                                       :request-id "failed-request" :external-history t
                                       :error-data (:secret "not projected"))
                            niwa-projection-timestamps-test--summary-data))
           (text "Assistant request failed. Provider unavailable"))
      (niwa-projection-timestamps-test--insert-turn "Answer." failure)
      (let* ((records (niwa-collaboration--canonical-records (current-buffer)))
             (failed (nth 2 records))
             (summary (nth 3 records)))
        (should (= 4 (length records)))
        (should (equal (plist-get failed :id)
                       (niwa-collaboration--stable-record-id "failure" text 0)))
        (should (equal (plist-get failed :kind) "assistant"))
        (should (equal (plist-get failed :status) "failed"))
        (should (equal (plist-get failed :text) text))
        (should (equal "failed-request" (plist-get failed :failure-id)))
        (should (eq t (plist-get failed :retryExternal)))
        (should-not (assoc "failure-id" (niwa-collaboration--json-record failed)))
        (should (equal (plist-get summary :kind) "request-summary"))
        (should (equal (plist-get summary :text)
                       (niwa-projection-timestamps-test--summary-text
                        niwa-projection-timestamps-test--summary-data)))
        (should-not (plist-member summary :status))
        (should-not (string-match-p
                     "not projected\\|started-at\\|elapsed"
                     (prin1-to-string
                      (mapcar #'niwa-collaboration--json-record records)))))))
  :doc "repeated identical summaries and failures project distinct identities"
  (with-temp-buffer
    (let* ((old '(:kind request-summary :elapsed-seconds 3))
           (failure (append '(:outcome error :message "Down")
                            niwa-projection-timestamps-test--summary-data))
           (retimed (plist-put (copy-sequence failure)
                               :finished-at 1761730999.0)))
      (niwa-projection-timestamps-test--insert-turn "Answer." (append '(:request-id "one") old))
      (niwa-projection-timestamps-test--insert-turn "Answer." (append '(:request-id "two") old))
      (niwa-projection-timestamps-test--insert-turn "Answer." failure)
      (niwa-projection-timestamps-test--insert-turn "Answer." retimed)
      (let* ((records (niwa-collaboration--canonical-records (current-buffer)))
             (ids (mapcar (lambda (record) (plist-get record :id)) records))
             (failures (cl-remove-if-not
                        (lambda (record) (equal (plist-get record :status) "failed"))
                        records)))
        (should (= 4 (length
                      (niwa-projection-timestamps-test--summary-records records))))
        (should (equal ids (delete-dups (copy-sequence ids))))
        ;; The failure identity is independent of its timing footer.
        (should (equal (mapcar (lambda (record) (plist-get record :id)) failures)
                       (list (niwa-collaboration--stable-record-id
                              "failure" "Assistant request failed. Down" 0)
                             (niwa-collaboration--stable-record-id
                              "failure" "Assistant request failed. Down" 1)))))))
  :doc "summaries and failures inherit shared-item scopes"
  (with-temp-buffer
    (niwa-projection-timestamps-test--insert-turn
     "Answer." (append '(:outcome error)
                       niwa-projection-timestamps-test--summary-data))
    (let ((end (1+ (point-max))))
      (cl-letf (((symbol-function 'niwa-shared-conversation-ranges)
                 (lambda ()
                   (list (list :start 1 :end end
                               :shared (list :itemId "item-1"))))))
        (let ((records (niwa-collaboration--canonical-records
                        (current-buffer))))
          (should (= 4 (length records)))
          (dolist (record (cddr records))
            (should (equal (plist-get record :item) "item-1")))))))
  :doc "closing lines stay outside activity groups and split activity runs"
  (let* ((niwa-view-tool-group-collapse-threshold 1)
         (tool (lambda (id)
                 (list :id id :kind "tool" :status "completed"
                       :presentation (list :name "Read" :groupable t))))
         (summary (list :id "summary" :kind "request-summary" :text "── Worked "))
         (records (niwa-collaboration--activity-records
                   (list (funcall tool "a") (funcall tool "b") summary
                         (funcall tool "c") (funcall tool "d")))))
    (should-not (plist-member (nth 2 records) :activity))
    (should (equal '("a" "a" "c" "c")
                   (mapcar (lambda (record)
                             (plist-get (plist-get record :activity) :id))
                           (list (nth 0 records) (nth 1 records)
                                 (nth 3 records) (nth 4 records))))))
  :doc "a retained projection equals a full one as summaries are appended"
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (let ((cache (niwa-collaboration--projection-cache-create))
          (old '(:kind request-summary :elapsed-seconds 3)))
      (niwa-projection-timestamps-test--insert-turn "Answer." old)
      (should (equal (niwa-collaboration--canonical-records (current-buffer))
                     (niwa-collaboration--canonical-records
                      (current-buffer) nil nil cache)))
      (niwa-projection-timestamps-test--insert-turn "Answer." old)
      (niwa-projection-timestamps-test--insert-turn
       "Answer." (append '(:outcome error)
                         niwa-projection-timestamps-test--summary-data))
      (let ((full (niwa-collaboration--canonical-records (current-buffer))))
        (should (= 3 (length (niwa-projection-timestamps-test--summary-records full))))
        (should (equal full (niwa-collaboration--canonical-records
                             (current-buffer) nil nil cache)))
        ;; An unchanged buffer reuses every retained entry identically.
        (should (equal full (niwa-collaboration--canonical-records
                             (current-buffer) nil nil cache))))))
  :doc "an archived transcript restored from saved bounds projects the same records"
  (let (saved live)
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (niwa-transcript-enable-gptel-mode)
      (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
      (niwa-projection-timestamps-test--insert-turn
       "Answer." niwa-projection-timestamps-test--summary-data)
      (niwa-projection-timestamps-test--insert-turn
       "Second." (append '(:outcome error)
                         niwa-projection-timestamps-test--summary-data))
      (niwa-session-artifacts-stabilize-gptel-bounds)
      (setq live (niwa-collaboration--canonical-records (current-buffer))
            saved (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert saved)
      (delay-mode-hooks (org-mode))
      (niwa-transcript-restore-properties)
      (let ((restored (niwa-collaboration--canonical-records (current-buffer))))
        (should (= 2 (length
                      (niwa-projection-timestamps-test--summary-records restored))))
        (should (equal live restored))))))

(niwa-deftest niwa-collaboration--project-records/retry
  (:doc "only the exact retained failure gets Retry, without polluting cached history")
  (with-temp-buffer
    (let ((room (list :data-buffer (current-buffer)))
          (retry-id "current"))
      (dolist (id '("older" "current"))
        (niwa-projection-timestamps-test--insert-turn
         "Partial answer."
         (append (list :request-id id :outcome 'error :message "Down")
                 niwa-projection-timestamps-test--summary-data)))
      (cl-letf (((symbol-function 'niwa-gptel-retry-available-p)
                 (lambda (buffer &optional _)
                   (should (eq buffer (current-buffer)))
                   retry-id)))
        (let* ((records (niwa-collaboration--project-records room))
               (retryable (cl-remove-if-not
                           (lambda (record) (plist-get record :retryId)) records)))
          (should (= 1 (length retryable)))
          (should (equal "current" (plist-get (car retryable) :retryId))))
        (setq retry-id nil)
        (should-not (cl-some (lambda (record) (plist-get record :retryId))
                             (niwa-collaboration--project-records room)))))))

(provide 'test-niwa-collaboration-projection-timestamps)

;;; test-niwa-collaboration-projection-timestamps.el ends here
