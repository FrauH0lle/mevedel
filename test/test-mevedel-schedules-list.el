;;; test-mevedel-schedules-list.el --- Standing jobs cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the workspace table and its actions from the user's keys.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-menu)
(require 'mevedel-schedules-list)
(require 'mevedel-session-persistence)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-workspace-schedule)
(require 'mevedel-workspace-schedule-runs)

(mevedel-deftest mevedel-schedules-list-open ()
  ,test
  (test)
  :doc "opening the jobs cockpit shows the workspace schedule without disturbing a draft"
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (let* ((ws (mevedel-workspace--create :type 'project :root default-directory
                                            :id "jobs" :name "jobs"))
             (session (mevedel-session-create "planning" ws))
             (job '(:id "s-a" :name "Check build" :status active
                    :timing (:kind interval :seconds 600) :due 1800000000)))
        (with-current-buffer data-buf (setq mevedel--session session))
        (switch-to-buffer view-buf)
        (setq mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "unfinished draft")
        (backward-char 5)
        (let ((origin (selected-window)) (position (point)) (windows (window-list)) (reads 0))
          (cl-letf (((symbol-function 'mevedel-workspace-schedule-list)
                     (lambda (workspace) (should (eq workspace ws)) (cl-incf reads) (list job))))
            (mevedel-menu-open 'schedules)
            (should (string-search "Check build" (buffer-string)))
            (should (string-search "active" (buffer-string)))
            (should (string-search (format-time-string "%Y-%m-%d %H:%M" 1800000000)
                                   (buffer-string)))
            (should (string-search "10m" (buffer-string)))
            (should (equal "s-a" (tabulated-list-get-id)))
            (setq job (plist-put (copy-sequence job) :status 'paused))
            (run-hook-with-args 'mevedel-workspace-schedule-changed-functions
                                (mevedel-workspace--create :root "/other/"))
            (should (= reads 1))
            (with-current-buffer data-buf
              (run-hook-with-args 'mevedel-workspace-schedule-changed-functions (copy-sequence ws)))
            (should (string-search "paused" (buffer-string)))
            (should (equal "s-a" (tabulated-list-get-id)))
            (call-interactively (key-binding (kbd "q")))
            (run-hook-with-args 'mevedel-workspace-schedule-changed-functions ws)
            (should (= reads 2)))
          (should (eq (selected-window) origin))
          (should (equal (window-list) windows))
          (should (eq (current-buffer) view-buf))
          (should (= (point) position))
          (should (equal "unfinished draft" (mevedel-view--input-text))))))))

(mevedel-deftest mevedel-schedules-list-pause ()
  ,test
  (test)
  :doc "pause, resume and delete act on the selected id; a declined deletion keeps the job"
  (save-window-excursion
    (let* ((ws (mevedel-workspace--create :id "keys" :name "Keys" :root default-directory))
           (jobs (mapcar (lambda (id) (list :id id :name "Build" :status 'active
                                           :timing '(:kind cron :expression "0 9 * * *")))
                         '("s-other" "s-chosen")))
           (context (mevedel-cockpit-workspace-context ws (current-buffer)))
           (confirm nil))
      (cl-letf (((symbol-function 'mevedel-workspace-schedule-list)
                 (lambda (workspace) (should (eq workspace ws)) jobs))
                ((symbol-function 'mevedel-workspace-schedule-set-paused)
                 (lambda (workspace id paused)
                   (should (eq workspace ws)) (should (equal id "s-chosen"))
                   (plist-put (cadr jobs) :status (if paused 'paused 'active))
                   (run-hook-with-args 'mevedel-workspace-schedule-changed-functions workspace)))
                ((symbol-function 'mevedel-workspace-schedule-delete)
                 (lambda (workspace id)
                   (should (eq workspace ws)) (should (equal id "s-chosen"))
                   (setq jobs (list (car jobs)))
                   (run-hook-with-args 'mevedel-workspace-schedule-changed-functions workspace)))
                ((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (should (string-search "s-chosen" prompt)) confirm)))
        (mevedel-schedules-list-open context)
        (forward-line 1)
        (dolist (pair '(("p" . "paused") ("r" . "active")))
          (call-interactively (key-binding (kbd (car pair))))
          (should (equal "s-chosen" (tabulated-list-get-id)))
          (should (string-search (cdr pair) (thing-at-point 'line t))))
        (call-interactively (key-binding (kbd "d")))
        (should (= 2 (length jobs)))
        (setq confirm t)
        (call-interactively (key-binding (kbd "d")))
        (should (equal "s-other" (tabulated-list-get-id)))
        (should (= 1 (length jobs)))
        (call-interactively (key-binding (kbd "q")))))))

(mevedel-deftest mevedel-schedules-list--details ()
  ,test
  (test)
  :doc "history distinguishes quiet and notified runs and opens only an existing session"
  (save-window-excursion
    (let* ((ws (mevedel-workspace--create :id "history" :name "History" :root default-directory))
           (job '(:id "s-build" :name "Build" :status paused :pause-reason "Three failed runs"
                  :timing (:kind once :at 42)))
           (runs '((:fired-at 1800000000 :outcome done :notified nil :session-id "quiet-run")
                   (:fired-at 1800009000 :outcome failed :error "Credentials expired"
                    :notified t :session-id "failed-run")))
           (view (generate-new-buffer " *saved run view*"))
           (data (generate-new-buffer " *saved run*")))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-workspace-schedule-list) (lambda (_) (list job)))
                    ((symbol-function 'mevedel-workspace-schedule-runs)
                     (lambda (workspace id) (should (eq ws workspace))
                       (should (equal id "s-build")) runs))
                    ((symbol-function 'mevedel-session-persistence-resume-id)
                     (lambda (workspace id) (should (eq ws workspace))
                       (and (equal id "failed-run") data))))
            (with-current-buffer data
              (setq-local mevedel--view-buffer view
                          mevedel--session (mevedel-session-create "run" ws))
              (setf (mevedel-session-preset-name mevedel--session) 'mevedel-implement))
            (mevedel-schedules-list-open (mevedel-cockpit-workspace-context ws (current-buffer)))
            (call-interactively (key-binding (kbd "RET")))
            (goto-char (point-min))
            (search-forward "Three failed runs")
            (search-forward (format-time-string "%Y-%m-%d %H:%M" 1800009000))
            (should (string-match-p "failed.*notified.*failed-run" (thing-at-point 'line t)))
            (should (string-search "Credentials expired" (thing-at-point 'line t)))
            (search-forward "failed-run")
            (let ((link (button-at (1- (point)))) (report (current-buffer))
                  (details-window (selected-window))
                  (gptel-display-buffer-action '(display-buffer-use-some-window
                                                 (inhibit-same-window . t) (body-function . select-window))))
              (button-activate link)
              (should (eq view (window-buffer (selected-window))))
              (should (eq report (window-buffer details-window)))
              (with-current-buffer report
                (goto-char (point-min))
                (search-forward "quiet-run")
                (should (string-match-p "done.*quiet" (thing-at-point 'line t)))
                (should-error (button-activate (button-at (1- (point)))) :type 'user-error))))
        (kill-buffer data) (kill-buffer view)))))

(provide 'test-mevedel-schedules-list)
;;; test-mevedel-schedules-list.el ends here
