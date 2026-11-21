;;; test-niwa-schedules-list.el --- Standing jobs cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the workspace table and its actions from the user's keys.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-menu)
(require 'niwa-schedules-list)
(require 'niwa-session-persistence)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-workspace-schedule)
(require 'niwa-workspace-schedule-runs)

(niwa-deftest niwa-schedules-list-open ()
  ,test
  (test)
  :doc "opening the jobs cockpit shows the workspace schedule without disturbing a draft"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((ws (niwa-workspace--create :type 'project :root default-directory
                                            :id "jobs" :name "jobs"))
             (session (niwa-session-create "planning" ws))
             (job '(:id "s-a" :name "Check build" :status active
                    :timing (:kind interval :seconds 600) :due 1800000000))
             (draft "unfinished draft"))
        (with-current-buffer data-buf (setq niwa--session session))
        (switch-to-buffer view-buf)
        (setq niwa--session session)
        (niwa-view-test--insert-composer-draft draft (- (length draft) 5))
        (let ((origin (selected-window)) (windows (window-list))
              (offset (niwa-view-test--composer-point-offset)) (reads 0))
          (cl-letf (((symbol-function 'niwa-workspace-schedule-list)
                     (lambda (workspace) (should (eq workspace ws)) (cl-incf reads) (list job))))
            (niwa-menu-open 'schedules)
            (should (string-search "Check build" (buffer-string)))
            (should (string-search "active" (buffer-string)))
            (should (string-search (format-time-string "%Y-%m-%d %H:%M" 1800000000)
                                   (buffer-string)))
            (should (string-search "10m" (buffer-string)))
            (should (equal "s-a" (tabulated-list-get-id)))
            (setq job (plist-put (copy-sequence job) :status 'paused))
            (run-hook-with-args 'niwa-workspace-schedule-changed-functions
                                (niwa-workspace--create :root "/other/"))
            (should (= reads 1))
            (with-current-buffer data-buf
              (run-hook-with-args 'niwa-workspace-schedule-changed-functions (copy-sequence ws)))
            (should (string-search "paused" (buffer-string)))
            (should (equal "s-a" (tabulated-list-get-id)))
            (call-interactively (key-binding (kbd "q")))
            (run-hook-with-args 'niwa-workspace-schedule-changed-functions ws)
            (should (= reads 2)))
          (should (eq (selected-window) origin))
          (should (equal (window-list) windows))
          (should (eq (current-buffer) view-buf))
          (should (= offset (niwa-view-test--composer-point-offset)))
          (should (equal draft (niwa-view-composer-draft-text))))))))

(niwa-deftest niwa-schedules-list-pause ()
  ,test
  (test)
  :doc "pause, resume and delete act on the selected id; a declined deletion keeps the job"
  (save-window-excursion
    (let* ((ws (niwa-workspace--create :id "keys" :name "Keys" :root default-directory))
           (jobs (mapcar (lambda (id) (list :id id :name "Build" :status 'active
                                           :timing '(:kind cron :expression "0 9 * * *")))
                         '("s-other" "s-chosen")))
           (context (niwa-cockpit-workspace-context ws (current-buffer)))
           (confirm nil))
      (cl-letf (((symbol-function 'niwa-workspace-schedule-list)
                 (lambda (workspace) (should (eq workspace ws)) jobs))
                ((symbol-function 'niwa-workspace-schedule-set-paused)
                 (lambda (workspace id paused)
                   (should (eq workspace ws)) (should (equal id "s-chosen"))
                   (plist-put (cadr jobs) :status (if paused 'paused 'active))
                   (run-hook-with-args 'niwa-workspace-schedule-changed-functions workspace)))
                ((symbol-function 'niwa-workspace-schedule-delete)
                 (lambda (workspace id)
                   (should (eq workspace ws)) (should (equal id "s-chosen"))
                   (setq jobs (list (car jobs)))
                   (run-hook-with-args 'niwa-workspace-schedule-changed-functions workspace)))
                ((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (should (string-search "s-chosen" prompt)) confirm)))
        (niwa-schedules-list-open context)
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

(niwa-deftest niwa-schedules-list--details ()
  ,test
  (test)
  :doc "history distinguishes quiet and notified runs and opens only an existing session"
  (save-window-excursion
    (let* ((ws (niwa-workspace--create :id "history" :name "History" :root default-directory))
           (job '(:id "s-build" :name "Build" :status paused :pause-reason "Three failed runs"
                  :timing (:kind once :at 42)))
           (runs '((:fired-at 1800000000 :outcome done :notified nil :session-id "quiet-run")
                   (:fired-at 1800009000 :outcome failed :error "Credentials expired"
                    :notified t :session-id "failed-run")))
           (view (generate-new-buffer " *saved run view*"))
           (data (generate-new-buffer " *saved run*")))
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-workspace-schedule-list) (lambda (_) (list job)))
                    ((symbol-function 'niwa-workspace-schedule-runs)
                     (lambda (workspace id) (should (eq ws workspace))
                       (should (equal id "s-build")) runs))
                    ((symbol-function 'niwa-session-persistence-resume-id)
                     (lambda (workspace id) (should (eq ws workspace))
                       (and (equal id "failed-run") data))))
            (with-current-buffer data
              (setq-local niwa--view-buffer view
                          niwa--session (niwa-session-create "run" ws))
              (setf (niwa-session-preset-name niwa--session) 'niwa-implement))
            (niwa-schedules-list-open (niwa-cockpit-workspace-context ws (current-buffer)))
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

(provide 'test-niwa-schedules-list)
;;; test-niwa-schedules-list.el ends here
