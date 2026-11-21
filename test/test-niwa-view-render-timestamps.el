;;; test-niwa-view-render-timestamps.el --- Turn timestamp tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise terminal timing through the rendered transcript and its saved data.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-structs)
(require 'niwa-tool-render-data)
(require 'niwa-transcript-restore)

(defmacro niwa-timestamps-test--with-zone (zone &rest body)
  "Run BODY with local time ZONE, restoring the process time zone afterwards."
  (declare (indent 1) (debug t))
  `(let ((old-zone (getenv "TZ")))
     (unwind-protect
         (progn (set-time-zone-rule ,zone) ,@body)
       (set-time-zone-rule old-zone))))

(niwa-deftest niwa-view-stream-render-response/timestamps ()
  ,test
  (test)
  :doc "settlement preserves its timestamp, duration, and composer draft"
  (niwa-timestamps-test--with-zone "UTC0"
    (niwa-view-test--with-buffers
      (let* ((system-time-locale "C")
             (started (encode-time 43 29 14 29 10 2026 t))
             (finished (encode-time 20 32 14 29 10 2026 t))
             (now finished)
             (draft "> Keep this draft\nsecond line")
             summary-start)
        (with-current-buffer data-buf
          (setq-local niwa--current-request
                      (niwa-request--create :started-at started)))
        (niwa-view-test--insert-data data-buf "Prompt\n" nil)
        (with-current-buffer data-buf (setq summary-start (point-max)))
        (niwa-view-test--insert-data data-buf "Done.\n" 'response)
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft draft 5))
        (cl-letf (((symbol-function 'current-time) (lambda () now)))
          (with-current-buffer data-buf
            (niwa-view-stream-render-response summary-start (point-max)))
          (with-current-buffer view-buf
            (should (string-search
                     "── Worked for 2m 37s · Thu 29 Oct 14:32 "
                     (buffer-string)))
            (save-excursion
              (goto-char (point-min))
              (search-forward "Thu 29 Oct 14:32")
              (should (equal
                       "Started: Thu 29 Oct 2026 14:29:43 +0000\nFinished: Thu 29 Oct 2026 14:32:20 +0000\nActive work: 2m 37s"
                       (get-text-property (1- (point)) 'help-echo))))
            (should (equal draft (niwa-view-composer-draft-text nil t)))
            (should (= (+ (niwa-view-composer-draft-start) 5)
                       (with-current-buffer (niwa-view-composer-buffer) (point)))))
          ;; A later repeat must keep the first settlement time and duration.
          (setq now (time-add finished 3600))
          (with-current-buffer data-buf
            (niwa-view-stream-render-response summary-start (point-max)))
          (with-current-buffer view-buf
            (niwa-view--full-rerender)
            (should (string-search
                     "── Worked for 2m 37s · Thu 29 Oct 14:32 "
                     (buffer-string)))
            (should (= 1 (niwa-view-test--count-substring
                          "Worked for" (buffer-string)))))))))

  :doc "an aborted paused request keeps its outcome and actual finish time"
  (niwa-timestamps-test--with-zone "UTC0"
    (niwa-view-test--with-buffers
      (let* ((started (encode-time 43 29 14 29 10 2026 t))
             (finished (encode-time 20 32 14 29 10 2026 t))
             (request (niwa-request--create :started-at started)))
        (niwa-request-set-active-work-paused request t (time-add started 60))
        (with-current-buffer data-buf
          (setq-local niwa--current-request request)
          (insert (propertize "Partial response.\n" 'gptel 'response))
          (cl-letf (((symbol-function 'current-time) (lambda () finished)))
            (niwa-view--append-request-summary
             data-buf 1 '(:outcome aborted :message "Interrupted"))
            (niwa-view-stream-render-response 1 (point-max)))
          (goto-char (point-min))
          (search-forward niwa-tool-render-data-open)
          (let ((summary (cdr (niwa-tool-render-data-extract
                               (buffer-substring (match-beginning 0) (point-max))))))
            (should (eq 'aborted (plist-get summary :outcome)))
            (should (equal "Interrupted" (plist-get summary :message)))
            (should (= 60 (plist-get summary :elapsed-seconds)))
            (should (= (float-time finished) (plist-get summary :finished-at)))))
        (with-current-buffer view-buf
          (should (string-search
                   "── Worked for 1m 00s · Thu 29 Oct 14:32 " (buffer-string)))
          (should (= 1 (niwa-view-test--count-substring
                        "Worked for" (buffer-string))))
          (should-not (string-search "request failed" (buffer-string))))))))

(niwa-deftest niwa-view--append-request-summary/paused-timestamps ()
  (niwa-timestamps-test--with-zone "UTC0"
    (niwa-view-test--with-buffers
      (let* ((started (encode-time 43 29 13 29 10 2026 t))
             (finished (encode-time 20 32 14 29 10 2026 t))
             (request (niwa-request--create :started-at started))
             (file (make-temp-file "dated-turn-" nil ".org")))
        (niwa-request-set-active-work-paused request t (time-add started 60))
        (niwa-request-set-active-work-paused request nil (time-add started 3660))
        (with-current-buffer data-buf
          (setq-local niwa--current-request request)
          (insert (propertize "Done.\n" 'gptel 'response))
          (cl-letf (((symbol-function 'current-time) (lambda () finished)))
            (niwa-view--append-request-summary
             data-buf 1 '(:outcome error :message "Provider failed")))
          ;; Persist through gptel's bounds format, then restore from plain bytes.
          (gptel--save-state)
          (write-region (point-min) (point-max) file nil 'silent)
          (erase-buffer)
          (insert-file-contents file)
          (niwa-transcript-restore-properties)
          (setq-local niwa--current-request nil))
        (with-current-buffer view-buf
          (niwa-view--full-rerender)
          (should (string-search
                   "── Worked for 2m 37s · Thu 29 Oct 14:32 " (buffer-string)))
          (goto-char (point-min))
          (search-forward "Thu 29 Oct 14:32")
          (should (string-search
                   "Started: Thu 29 Oct 2026 13:29:43 +0000"
                   (get-text-property (1- (point)) 'help-echo)))
          (should (string-search "Provider failed" (buffer-string))))))))

(niwa-deftest niwa-view--render-request-summary-segment/timestamps ()
  ,test
  (test)
  :doc "renders year boundaries and DST offsets independently of locale"
  (dolist
      (scenario
       '(("UTC0" "2026-12-31T23:59:00Z" "2027-01-01T00:01:00Z"
          "Fri 01 Jan 00:01" "Thu 31 Dec 2026 23:59:00 +0000"
          "Fri 01 Jan 2027 00:01:00 +0000")
         ("Europe/Berlin" "2026-10-25T00:59:00Z" "2026-10-25T01:01:00Z"
          "Sun 25 Oct 02:01" "Sun 25 Oct 2026 02:59:00 +0200"
          "Sun 25 Oct 2026 02:01:00 +0100")))
    (pcase-let ((`(,zone ,start ,finish ,label ,start-detail ,finish-detail) scenario))
      (niwa-timestamps-test--with-zone zone
        (niwa-view-test--with-buffers
          (let ((system-time-locale "de_DE.UTF-8"))
            (niwa-view-test--insert-data data-buf "Answer.\n" 'response)
            (niwa-view-test--insert-data
             data-buf
             (niwa-tool-render-data-format
              (list :kind 'request-summary :elapsed-seconds 120
                    :started-at (float-time (date-to-time start))
                    :finished-at (float-time (date-to-time finish))))
             'ignore)
            (with-current-buffer view-buf
              (niwa-view--full-rerender)
              (goto-char (point-min))
              (search-forward label)
              (let ((details (get-text-property (1- (point)) 'help-echo)))
                (should (string-search start-detail details))
                (should (string-search finish-detail details)))))))))

  :doc "keeps a summary without recorded timestamps duration-only"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Answer.\n" 'response)
    (niwa-view-test--insert-data
     data-buf (niwa-tool-render-data-format
               '(:kind request-summary :elapsed-seconds 7)) 'ignore)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "── Worked for 7s ")
      (should (eolp))
      (should-not (get-text-property (1- (point)) 'help-echo)))))

(provide 'test-niwa-view-render-timestamps)
;;; test-niwa-view-render-timestamps.el ends here
