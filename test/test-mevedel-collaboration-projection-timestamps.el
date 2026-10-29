;;; test-mevedel-collaboration-projection-timestamps.el --- Request-summary projection -*- lexical-binding: t -*-

;;; Commentary:
;; A saved hidden request summary reaches the browser as the host-rendered
;; closing line of its assistant turn, scoped and identified like other
;; canonical records.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'gptel)
(require 'mevedel-collaboration-projection)
(require 'mevedel-session-artifacts)
(require 'mevedel-shared-conversation)
(require 'mevedel-tool-render-data)
(require 'mevedel-transcript-restore)
(require 'mevedel-view-render)

(defconst mevedel-projection-timestamps-test--summary-data
  '(:kind request-summary :elapsed-seconds 75
          :started-at 1761730000.0 :finished-at 1761730075.0)
  "A successful request summary as the host saves it.")

(defun mevedel-projection-timestamps-test--insert-turn (answer summary)
  "Insert a prompt, ANSWER response and hidden SUMMARY render-data."
  (insert "Prompt.\n\n")
  (insert (propertize (concat answer "\n\n") 'gptel 'response))
  (insert (mevedel-tool-render-data-format summary) "\n"))

(defun mevedel-projection-timestamps-test--summary-records (records)
  "Return the request-summary records in RECORDS."
  (cl-remove-if-not (lambda (record)
                      (equal (plist-get record :kind) "request-summary"))
                    records))

(defun mevedel-projection-timestamps-test--summary-text (summary)
  "Return the host's closing line for SUMMARY without text properties."
  (substring-no-properties (mevedel-view--request-summary-line summary)))

(mevedel-deftest mevedel-collaboration--canonical-records/timestamps ()
  ,test
  (test)
  :doc "a saved successful summary projects the host-rendered closing line"
  (with-temp-buffer
    (mevedel-projection-timestamps-test--insert-turn
     "Answer." mevedel-projection-timestamps-test--summary-data)
    (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
           (summaries (mevedel-projection-timestamps-test--summary-records records))
           (summary (car summaries))
           (json (mevedel-collaboration--json-record summary)))
      (should (= 1 (length summaries)))
      (should (eq summary (car (last records))))
      (should (equal-including-properties
               (plist-get summary :text)
               (substring-no-properties
                (mevedel-view--request-summary-line
                 mevedel-projection-timestamps-test--summary-data))))
      (should (equal (plist-get summary :details)
                     (mevedel-view--request-summary-details
                      mevedel-projection-timestamps-test--summary-data)))
      (should (stringp (plist-get summary :id)))
      (should (equal (mapcar #'car json)
                     '("id" "kind" "revision" "text" "details")))))
  :doc "an old elapsed-only summary projects its line without timing details"
  (with-temp-buffer
    (mevedel-projection-timestamps-test--insert-turn
     "Answer." '(:kind request-summary :elapsed-seconds 3))
    (let* ((summary (car (mevedel-projection-timestamps-test--summary-records
                          (mevedel-collaboration--canonical-records
                           (current-buffer))))))
      (should (equal (plist-get summary :text)
                     (mevedel-projection-timestamps-test--summary-text
                      '(:kind request-summary :elapsed-seconds 3))))
      (should-not (plist-member summary :details))
      (should-not (assoc "details" (mevedel-collaboration--json-record summary)))))
  :doc "a summary without a closing line or failure projects nothing"
  (with-temp-buffer
    (mevedel-projection-timestamps-test--insert-turn
     "Answer." '(:kind request-summary :outcome success))
    (let ((records (mevedel-collaboration--canonical-records (current-buffer))))
      (should (equal '("user" "assistant")
                     (mapcar (lambda (record) (plist-get record :kind)) records)))))
  :doc "a failure keeps its record identity and gains the closing line after it"
  (with-temp-buffer
    (let* ((failure (append '(:outcome error :message "Provider unavailable"
                                       :error-data (:secret "not projected"))
                            mevedel-projection-timestamps-test--summary-data))
           (text "Assistant request failed. Provider unavailable"))
      (mevedel-projection-timestamps-test--insert-turn "Answer." failure)
      (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
             (failed (nth 2 records))
             (summary (nth 3 records)))
        (should (= 4 (length records)))
        (should (equal (plist-get failed :id)
                       (mevedel-collaboration--stable-record-id "failure" text 0)))
        (should (equal (plist-get failed :kind) "assistant"))
        (should (equal (plist-get failed :status) "failed"))
        (should (equal (plist-get failed :text) text))
        (should (equal (plist-get summary :kind) "request-summary"))
        (should (equal (plist-get summary :text)
                       (mevedel-projection-timestamps-test--summary-text
                        mevedel-projection-timestamps-test--summary-data)))
        (should-not (plist-member summary :status))
        (should-not (string-match-p
                     "not projected\\|started-at\\|elapsed"
                     (prin1-to-string
                      (mapcar #'mevedel-collaboration--json-record records)))))))
  :doc "repeated identical summaries and failures project distinct identities"
  (with-temp-buffer
    (let* ((old '(:kind request-summary :elapsed-seconds 3))
           (failure (append '(:outcome error :message "Down")
                            mevedel-projection-timestamps-test--summary-data))
           (retimed (plist-put (copy-sequence failure)
                               :finished-at 1761730999.0)))
      (mevedel-projection-timestamps-test--insert-turn "Answer." old)
      (mevedel-projection-timestamps-test--insert-turn "Answer." old)
      (mevedel-projection-timestamps-test--insert-turn "Answer." failure)
      (mevedel-projection-timestamps-test--insert-turn "Answer." retimed)
      (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
             (ids (mapcar (lambda (record) (plist-get record :id)) records))
             (failures (cl-remove-if-not
                        (lambda (record) (equal (plist-get record :status) "failed"))
                        records)))
        (should (= 4 (length
                      (mevedel-projection-timestamps-test--summary-records records))))
        (should (equal ids (delete-dups (copy-sequence ids))))
        ;; The failure identity is independent of its timing footer.
        (should (equal (mapcar (lambda (record) (plist-get record :id)) failures)
                       (list (mevedel-collaboration--stable-record-id
                              "failure" "Assistant request failed. Down" 0)
                             (mevedel-collaboration--stable-record-id
                              "failure" "Assistant request failed. Down" 1)))))))
  :doc "summaries and failures inherit directive and shared-item scopes"
  (dolist (scope '((:directive "dir-1") (:item "item-1")))
    (with-temp-buffer
      (mevedel-projection-timestamps-test--insert-turn
       "Answer." (append '(:outcome error)
                         mevedel-projection-timestamps-test--summary-data))
      (let ((end (1+ (point-max))))
        (cl-letf (((symbol-function 'mevedel-collaboration--directive-ranges)
                   (lambda ()
                     (when (plist-get scope :directive)
                       (list (list :start 1 :end end :directive-id "dir-1")))))
                  ((symbol-function 'mevedel-shared-conversation-ranges)
                   (lambda ()
                     (when (plist-get scope :item)
                       (list (list :start 1 :end end
                                   :shared (list :itemId "item-1")))))))
          (let ((records (mevedel-collaboration--canonical-records
                          (current-buffer))))
            (should (= 4 (length records)))
            (dolist (record (cddr records))
              (should (equal (plist-get record (car scope)) (cadr scope)))))))))
  :doc "closing lines stay outside activity groups and split activity runs"
  (let* ((mevedel-view-tool-group-collapse-threshold 1)
         (tool (lambda (id)
                 (list :id id :kind "tool" :status "completed"
                       :presentation (list :name "Read" :groupable t))))
         (summary (list :id "summary" :kind "request-summary" :text "── Worked "))
         (records (mevedel-collaboration--activity-records
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
    (mevedel--transcript-org-mode)
    (let ((cache (mevedel-collaboration--projection-cache-create))
          (old '(:kind request-summary :elapsed-seconds 3)))
      (mevedel-projection-timestamps-test--insert-turn "Answer." old)
      (should (equal (mevedel-collaboration--canonical-records (current-buffer))
                     (mevedel-collaboration--canonical-records
                      (current-buffer) nil nil cache)))
      (mevedel-projection-timestamps-test--insert-turn "Answer." old)
      (mevedel-projection-timestamps-test--insert-turn
       "Answer." (append '(:outcome error)
                         mevedel-projection-timestamps-test--summary-data))
      (let ((full (mevedel-collaboration--canonical-records (current-buffer))))
        (should (= 3 (length (mevedel-projection-timestamps-test--summary-records full))))
        (should (equal full (mevedel-collaboration--canonical-records
                             (current-buffer) nil nil cache)))
        ;; An unchanged buffer reuses every retained entry identically.
        (should (equal full (mevedel-collaboration--canonical-records
                             (current-buffer) nil nil cache))))))
  :doc "an archived transcript restored from saved bounds projects the same records"
  (let (saved live)
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (mevedel-transcript-enable-gptel-mode)
      (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
      (mevedel-projection-timestamps-test--insert-turn
       "Answer." mevedel-projection-timestamps-test--summary-data)
      (mevedel-projection-timestamps-test--insert-turn
       "Second." (append '(:outcome error)
                         mevedel-projection-timestamps-test--summary-data))
      (mevedel-session-artifacts-stabilize-gptel-bounds)
      (setq live (mevedel-collaboration--canonical-records (current-buffer))
            saved (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert saved)
      (delay-mode-hooks (org-mode))
      (mevedel-transcript-restore-properties)
      (let ((restored (mevedel-collaboration--canonical-records (current-buffer))))
        (should (= 2 (length
                      (mevedel-projection-timestamps-test--summary-records restored))))
        (should (equal live restored))))))

(provide 'test-mevedel-collaboration-projection-timestamps)

;;; test-mevedel-collaboration-projection-timestamps.el ends here
