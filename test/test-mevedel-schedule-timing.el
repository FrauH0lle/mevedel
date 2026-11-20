;;; test-mevedel-schedule-timing.el --- Pure schedule timing -*- lexical-binding: t -*-

;;; Commentary:
;; Timing parsing and next-run calculation under real time zones set through
;; the process environment.  Expected epoch seconds come from worked examples
;; checked with GNU date, not from the code's own method.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-schedule)

(defmacro mevedel-schedule-timing-test--in-zone (zone &rest body)
  "Run BODY with the process environment's TZ set to ZONE."
  (declare (indent 1) (debug t))
  `(let ((previous (getenv "TZ")))
     (unwind-protect
         (progn (setenv "TZ" ,zone) ,@body)
       (setenv "TZ" previous))))

(defun mevedel-schedule-timing-test--runs (text after count)
  "Return COUNT successive fire times of timing TEXT later than AFTER."
  (let ((timing (mevedel-schedule-parse-timing text after))
        runs)
    (dotimes (_ count)
      (push (setq after (mevedel-schedule-next-run timing after)) runs))
    (nreverse runs)))

(mevedel-deftest mevedel-schedule-next-run ()
  ,test
  (test)
  :doc "cron weekdays skip the weekend"
  (mevedel-schedule-timing-test--in-zone "Europe/Zurich"
    ;; Fri 2026-11-20 13:00 -> Mon 09:00, Tue 09:00.
    (should (equal '(1795420800 1795507200)
                   (mevedel-schedule-timing-test--runs "0 9 * * 1-5" 1795176000 2))))

  :doc "cron lists, ranges and steps combine"
  (mevedel-schedule-timing-test--in-zone "Europe/Zurich"
    ;; 2026-11-21 08:05, 08:35, 10:05.
    (should (equal '(1795244700 1795246500 1795251900)
                   (mevedel-schedule-timing-test--runs "5,35 8-10/2 * * *" 1795176000 3))))

  :doc "restricted day-of-month and day-of-week match either"
  (mevedel-schedule-timing-test--in-zone "Europe/Zurich"
    ;; The 13th or a Friday: Fri 2026-11-27 09:00 comes before Dec 13.
    (should (equal '(1795766400)
                   (mevedel-schedule-timing-test--runs "0 9 13 * 5" 1795176000 1)))
    ;; Day of week 7 is Sunday: Sun 2026-11-22 00:00.
    (should (equal '(1795302000)
                   (mevedel-schedule-timing-test--runs "0 0 * * 7" 1795176000 1))))

  :doc "a cron time in the skipped hour fires once at the first minute after the gap"
  (mevedel-schedule-timing-test--in-zone "Europe/Zurich"
    ;; 2027-03-28: 02:00 CET jumps to 03:00 CEST.  02:30 -> 03:00 CEST, then
    ;; 2027-03-29 02:30 CEST.
    (should (equal '(1806195600 1806280200)
                   (mevedel-schedule-timing-test--runs "30 2 * * *" 1806145200 2)))
    ;; Every quarter hour from 01:50 CET: 02:00-02:45 collapse into one 03:00.
    (should (equal '(1806195600 1806196500)
                   (mevedel-schedule-timing-test--runs "*/15 * * * *" 1806195000 2))))
  (mevedel-schedule-timing-test--in-zone "America/New_York"
    ;; 2027-03-14: 02:00 EST jumps to 03:00 EDT.
    (should (equal '(1805007600)
                   (mevedel-schedule-timing-test--runs "30 2 * * *" 1804957200 1))))

  :doc "a cron time in the repeated hour fires once, at its first occurrence"
  (mevedel-schedule-timing-test--in-zone "Europe/Zurich"
    ;; 2027-10-31: 03:00 CEST falls back to 02:00 CET.  02:30 CEST, then not
    ;; 02:30 CET (1824946200) but 2027-11-01 02:30 CET.
    (should (equal '(1824942600 1825032600)
                   (mevedel-schedule-timing-test--runs "30 2 * * *" 1824890400 2))))
  (mevedel-schedule-timing-test--in-zone "America/New_York"
    ;; 2027-11-07: 02:00 EDT falls back to 01:00 EST.  01:30 EDT, then
    ;; 2027-11-08 01:30 EST.
    (should (equal '(1825565400 1825655400)
                   (mevedel-schedule-timing-test--runs "30 1 * * *" 1825516800 2))))

  :doc "an interval is timed in absolute seconds from the previous run, across a gap"
  (mevedel-schedule-timing-test--in-zone "America/New_York"
    ;; 2027-03-14 01:50 EST -> 03:10 EDT, twenty real minutes later.
    (should (equal '(1805008200 1805009400)
                   (mevedel-schedule-timing-test--runs "every 20m" 1805007000 2)))))

(mevedel-deftest mevedel-schedule-parse-timing ()
  ,test
  (test)
  :doc "recognizes one-shot, interval and cron timing"
  (mevedel-schedule-timing-test--in-zone "Europe/Zurich"
    (should (equal '(:kind once :at 1795179600)
                   (mevedel-schedule-parse-timing "1h" 1795176000)))
    (should (equal '(:kind interval :seconds 1200)
                   (mevedel-schedule-parse-timing "every 20m" 1795176000)))
    (should (equal '(:kind interval :seconds 60)
                   (mevedel-schedule-parse-timing "every 1m" 1795176000)))
    (should (equal '(:kind cron :expression "0 9 * * 1-5")
                   (mevedel-schedule-parse-timing " 0 9 * * 1-5 " 1795176000))))

  :doc "a local time in the skipped hour moves to the gap's end; a repeated one takes its first occurrence"
  (mevedel-schedule-timing-test--in-zone "Europe/Zurich"
    (should (equal '(:kind once :at 1806195600)
                   (mevedel-schedule-parse-timing "2027-03-28 02:30" 1795176000)))
    (should (equal '(:kind once :at 1824942600)
                   (mevedel-schedule-parse-timing "2027-10-31 02:30" 1795176000))))
  (mevedel-schedule-timing-test--in-zone "America/New_York"
    (should (equal '(:kind once :at 1805007600)
                   (mevedel-schedule-parse-timing "2027-03-14 02:30" 1795176000)))
    (should (equal '(:kind once :at 1825565400)
                   (mevedel-schedule-parse-timing "2027-11-07 01:30" 1795176000))))

  :doc "unsupported cron syntax, short intervals and impossible dates are clear errors"
  (mevedel-schedule-timing-test--in-zone "Europe/Zurich"
    (dolist (case '(("0 9 * * MON-FRI" . "names")
                    ("0 9 * JAN *" . "names")
                    ("0 9 L * *" . "L, W and \\?")
                    ("0 9 15W * *" . "L, W and \\?")
                    ("0 9 ? * 1" . "L, W and \\?")
                    ("60 * * * *" . "minute field")
                    ("0 9 * * 1-8" . "day-of-week field")
                    ("*/0 * * * *" . "zero step")
                    ("0 9 5-1 * *" . "day-of-month field")
                    ("0 0 31 2 *" . "never fires")
                    ("every 30s" . "at least one minute")
                    ("every soon" . "at least one minute")
                    ("2030-02-31 13:00" . "Unrecognized")
                    ("2030-13-01 14:00" . "Unrecognized")
                    ("2030-11-20 25:90" . "Unrecognized")))
      (ert-info ((car case))
        (let ((err (should-error (mevedel-schedule-parse-timing (car case) 1795176000)
                                 :type 'user-error)))
          (should (string-match-p (cdr case) (cadr err))))))))

(provide 'test-mevedel-schedule-timing)
;;; test-mevedel-schedule-timing.el ends here
