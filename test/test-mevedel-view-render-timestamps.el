;;; test-mevedel-view-render-timestamps.el --- Turn timestamp tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise terminal timing through the rendered transcript and its saved data.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-structs)
(require 'mevedel-tool-render-data)
(require 'mevedel-transcript-restore)

(defmacro mevedel-timestamps-test--with-zone (zone &rest body)
  "Run BODY with local time ZONE, restoring the process time zone afterwards."
  (declare (indent 1) (debug t))
  `(let ((old-zone (getenv "TZ")))
     (unwind-protect
         (progn (set-time-zone-rule ,zone) ,@body)
       (set-time-zone-rule old-zone))))

(mevedel-deftest mevedel-view-stream-render-response/timestamps ()
  ,test
  (test)
  :doc "settlement preserves its timestamp, duration, and composer draft"
  (mevedel-timestamps-test--with-zone "UTC0"
    (mevedel-view-test--with-buffers
      (let* ((system-time-locale "C")
             (started (encode-time 43 29 14 29 10 2026 t))
             (finished (encode-time 20 32 14 29 10 2026 t))
             (now finished)
             (draft "> Keep this draft\nsecond line")
             summary-start)
        (with-current-buffer data-buf
          (setq-local mevedel--current-request
                      (mevedel-request--create :started-at started)))
        (mevedel-view-test--insert-data data-buf "Prompt\n" nil)
        (with-current-buffer data-buf (setq summary-start (point-max)))
        (mevedel-view-test--insert-data data-buf "Done.\n" 'response)
        (with-current-buffer view-buf
          (goto-char (point-max))
          (insert draft)
          (goto-char (+ (mevedel-view--input-start) 5)))
        (cl-letf (((symbol-function 'current-time) (lambda () now)))
          (with-current-buffer data-buf
            (mevedel-view-stream-render-response summary-start (point-max)))
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
            (should (equal draft (buffer-substring-no-properties
                                  (mevedel-view--input-start) (point-max))))
            (should (= (point) (+ (mevedel-view--input-start) 5))))
          ;; A later repeat must keep the first settlement time and duration.
          (setq now (time-add finished 3600))
          (with-current-buffer data-buf
            (mevedel-view-stream-render-response summary-start (point-max)))
          (with-current-buffer view-buf
            (mevedel-view--full-rerender)
            (should (string-search
                     "── Worked for 2m 37s · Thu 29 Oct 14:32 "
                     (buffer-string)))
            (should (= 1 (mevedel-view-test--count-substring
                          "Worked for" (buffer-string)))))))))

  :doc "an aborted paused request keeps its outcome and actual finish time"
  (mevedel-timestamps-test--with-zone "UTC0"
    (mevedel-view-test--with-buffers
      (let* ((started (encode-time 43 29 14 29 10 2026 t))
             (finished (encode-time 20 32 14 29 10 2026 t))
             (request (mevedel-request--create :started-at started)))
        (mevedel-request-set-active-work-paused request t (time-add started 60))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request request)
          (insert (propertize "Partial response.\n" 'gptel 'response))
          (cl-letf (((symbol-function 'current-time) (lambda () finished)))
            (mevedel-view--append-request-summary
             data-buf 1 '(:outcome aborted :message "Interrupted"))
            (mevedel-view-stream-render-response 1 (point-max)))
          (goto-char (point-min))
          (search-forward mevedel-tool-render-data-open)
          (let ((summary (cdr (mevedel-tool-render-data-extract
                               (buffer-substring (match-beginning 0) (point-max))))))
            (should (eq 'aborted (plist-get summary :outcome)))
            (should (equal "Interrupted" (plist-get summary :message)))
            (should (= 60 (plist-get summary :elapsed-seconds)))
            (should (= (float-time finished) (plist-get summary :finished-at)))))
        (with-current-buffer view-buf
          (should (string-search
                   "── Worked for 1m 00s · Thu 29 Oct 14:32 " (buffer-string)))
          (should (= 1 (mevedel-view-test--count-substring
                        "Worked for" (buffer-string))))
          (should-not (string-search "request failed" (buffer-string))))))))

(mevedel-deftest mevedel-view--append-request-summary/paused-timestamps ()
  (mevedel-timestamps-test--with-zone "UTC0"
    (mevedel-view-test--with-buffers
      (let* ((started (encode-time 43 29 13 29 10 2026 t))
             (finished (encode-time 20 32 14 29 10 2026 t))
             (request (mevedel-request--create :started-at started))
             (file (make-temp-file "dated-turn-" nil ".org")))
        (mevedel-request-set-active-work-paused request t (time-add started 60))
        (mevedel-request-set-active-work-paused request nil (time-add started 3660))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request request)
          (insert (propertize "Done.\n" 'gptel 'response))
          (cl-letf (((symbol-function 'current-time) (lambda () finished)))
            (mevedel-view--append-request-summary
             data-buf 1 '(:outcome error :message "Provider failed")))
          ;; Persist through gptel's bounds format, then restore from plain bytes.
          (gptel--save-state)
          (write-region (point-min) (point-max) file nil 'silent)
          (erase-buffer)
          (insert-file-contents file)
          (mevedel-transcript-restore-properties)
          (setq-local mevedel--current-request nil))
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (should (string-search
                   "── Worked for 2m 37s · Thu 29 Oct 14:32 " (buffer-string)))
          (goto-char (point-min))
          (search-forward "Thu 29 Oct 14:32")
          (should (string-search
                   "Started: Thu 29 Oct 2026 13:29:43 +0000"
                   (get-text-property (1- (point)) 'help-echo)))
          (should (string-search "Provider failed" (buffer-string))))))))

(mevedel-deftest mevedel-view--render-request-summary-segment/timestamps ()
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
      (mevedel-timestamps-test--with-zone zone
        (mevedel-view-test--with-buffers
          (let ((system-time-locale "de_DE.UTF-8"))
            (mevedel-view-test--insert-data data-buf "Answer.\n" 'response)
            (mevedel-view-test--insert-data
             data-buf
             (mevedel-tool-render-data-format
              (list :kind 'request-summary :elapsed-seconds 120
                    :started-at (float-time (date-to-time start))
                    :finished-at (float-time (date-to-time finish))))
             'ignore)
            (with-current-buffer view-buf
              (mevedel-view--full-rerender)
              (goto-char (point-min))
              (search-forward label)
              (let ((details (get-text-property (1- (point)) 'help-echo)))
                (should (string-search start-detail details))
                (should (string-search finish-detail details)))))))))

  :doc "keeps a summary without recorded timestamps duration-only"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf (mevedel-tool-render-data-format
               '(:kind request-summary :elapsed-seconds 7)) 'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "── Worked for 7s ")
      (should (eolp))
      (should-not (get-text-property (1- (point)) 'help-echo)))))

(provide 'test-mevedel-view-render-timestamps)
;;; test-mevedel-view-render-timestamps.el ends here
