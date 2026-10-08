;;; acp-trace.el --- Count ACP reads and pauses -*- lexical-binding: t -*-
;; Case `nopace' never pauses the adapter, for an unpaced comparison.
(defvar acp-trace-reads 0)
(defvar acp-trace-text-reads 0)
(defvar acp-trace-bytes 0)
(defvar acp-trace-pauses 0)
(defvar acp-trace-first nil)
(defvar acp-trace-last nil)
(defvar acp-trace-kinds nil
  "Frame kinds and counts: method, session update or SDK message types only.")
(defun acp-trace--kind (line)
  "Return LINE's frame kind without any content."
  (cond
   ((string-match "\"sessionUpdate\": *\"\\([a-z_]+\\)\"" line)
    (concat "update:" (match-string 1 line)))
   ((string-match-p "_claude/sdkMessage" line)
    (concat "sdk:"
            (if (string-match "\"message\": *{\"type\": *\"\\([a-z_]+\\)\"" line)
                (match-string 1 line) "?")
            (if (string-match "\"event\": *{\"type\": *\"\\([a-z_]+\\)\"" line)
                (concat "/" (match-string 1 line)) "")))
   ((string-match "\"method\": *\"\\([^\"]+\\)\"" line)
    (concat "method:" (match-string 1 line)))
   ((string-match-p "\"result\"" line) "result")
   (t "other")))
(with-eval-after-load 'mevedel-acp
  (when (equal (getenv "MEVEDEL_LAB_CASE") "nopace")
    (setq mevedel-acp--pace-after-text-reads most-positive-fixnum))
  (advice-add 'mevedel-acp--pace :before
              (lambda (_connection _process output)
                (setq acp-trace-first (or acp-trace-first (float-time))
                      acp-trace-last (float-time))
                (cl-incf acp-trace-reads)
                (cl-incf acp-trace-bytes (string-bytes output))
                (when (mevedel-acp--streaming-only-p output)
                  (cl-incf acp-trace-text-reads))
                (dolist (line (split-string output "\n" t))
                  (cl-incf (alist-get (acp-trace--kind line) acp-trace-kinds 0 nil #'equal)))))
  (advice-add 'mevedel-transport-pause-process :after-while
              (lambda (&rest _) (cl-incf acp-trace-pauses) t)))
(defun request-lab-diagnostic-result ()
  "Return bounded ACP read statistics."
  (list :reads acp-trace-reads :text-reads acp-trace-text-reads
        :bytes acp-trace-bytes :pauses acp-trace-pauses
        :seconds (and acp-trace-first (- acp-trace-last acp-trace-first))
        :kinds (seq-take (sort acp-trace-kinds (lambda (a b) (> (cdr a) (cdr b)))) 12)))
(provide 'acp-trace)
