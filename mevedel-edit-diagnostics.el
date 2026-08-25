;;; mevedel-edit-diagnostics.el -- Edit diagnostic reports -*- lexical-binding: t -*-

;;; Commentary:

;; Asynchronous edit-feedback state machine: captures Flymake/Flycheck
;; baselines before a patch touches a file, re-runs the checkers after,
;; classifies what the edit introduced, resolved, or left pre-existing,
;; and queues one coalesced report through the reminder turn-event
;; channel.  The patch tool is its only driver; the reminder module
;; delivers what it queues and knows nothing else about it.

;;; Code:

(require 'cl-lib)

;; `flycheck'
(declare-function flycheck-buffer "ext:flycheck" ())
(declare-function flycheck-error-level "ext:flycheck" (err) t)
(declare-function flycheck-error-line "ext:flycheck" (err) t)
(declare-function flycheck-error-message "ext:flycheck" (err) t)
(declare-function flycheck-get-checker-for-buffer "ext:flycheck" ())
(declare-function flycheck-overlay-errors-in "ext:flycheck" (beg end))
(declare-function flycheck-stop "ext:flycheck" ())
(defvar flycheck-after-syntax-check-hook)
(defvar flycheck-mode)

;; `flymake'
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-disabled-backends "flymake" ())
(declare-function flymake-start "flymake" (&optional defer force))
(defvar flymake-mode)

;; `mevedel-reminders'
(declare-function mevedel-reminders-queue-turn-event "mevedel-reminders"
                  (buffer key body &optional commit))
(declare-function mevedel-reminders-turn-owner "mevedel-reminders"
                  (&optional buffer))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-current-session "mevedel-telemetry"
                  (&optional buffer))
(declare-function mevedel-telemetry-record "mevedel-telemetry"
                  (session event &rest props))
(autoload 'mevedel-telemetry-current-session "mevedel-telemetry")
(autoload 'mevedel-telemetry-record "mevedel-telemetry")

(defvar-local mevedel-edit-diagnostics--state nil
  "Per-owner edit diagnostic baselines and pending reports.")


;;
;;; Diagnostic collection

(defun mevedel-edit-diagnostics--collect-flymake ()
  "Return a list of (FILE LINE LEVEL MSG) for Flymake diagnostics.
Scans the current buffer."
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics))
    (let ((file (buffer-file-name))
          (result nil))
      (dolist (diag (flymake-diagnostics))
        (let* ((beg (flymake-diagnostic-beg diag))
               (line (save-excursion (goto-char beg) (line-number-at-pos)))
               (type (flymake-diagnostic-type diag))
               (level (pcase type
                        (:error "error") (:warning "warning") (:note "note")
                        (_ (format "%s" type))))
               (msg (flymake-diagnostic-text diag)))
          (push (list file line level msg) result)))
      (nreverse result))))

(defun mevedel-edit-diagnostics--collect-flycheck ()
  "Return a list of (FILE LINE LEVEL MSG) for Flycheck errors in current buffer."
  (when (and (bound-and-true-p flycheck-mode)
             (fboundp 'flycheck-overlay-errors-in))
    (let ((file (buffer-file-name))
          (result nil))
      (dolist (err (flycheck-overlay-errors-in (point-min) (point-max)))
        (push (list file
                    (flycheck-error-line err)
                    (format "%s" (flycheck-error-level err))
                    (flycheck-error-message err))
              result))
      (nreverse result))))

(defun mevedel-edit-diagnostics--collect-in-buffer ()
  "Return deduplicated Flymake and Flycheck diagnostics in this buffer."
  (delete-dups
   (append (mevedel-edit-diagnostics--collect-flymake)
           (mevedel-edit-diagnostics--collect-flycheck))))

(defun mevedel-edit-diagnostics--files (buffer)
  "Return BUFFER's live diagnostic file table for its current owner."
  (with-current-buffer buffer
    (when-let* ((owner (mevedel-reminders-turn-owner buffer)))
      (unless (eq owner
                  (plist-get mevedel-edit-diagnostics--state :owner))
        (setq mevedel-edit-diagnostics--state
              (list :owner owner :files (make-hash-table :test #'equal))))
      (plist-get mevedel-edit-diagnostics--state :files))))

(defun mevedel-edit-diagnostics-before-edit (buffer path)
  "Capture BUFFER's diagnostic baseline for PATH before its first edit."
  (when (buffer-live-p buffer)
    (when-let* ((files (mevedel-edit-diagnostics--files buffer))
                (path (expand-file-name path))
                ((not (gethash path files)))
                (source (find-buffer-visiting path)))
      (puthash
       path
       (list :baseline
             (with-current-buffer source
               (mevedel-edit-diagnostics--collect-in-buffer))
             :reported nil :last nil :pending nil)
       files))))

(defun mevedel-edit-diagnostics--diagnostic-rank (diagnostic)
  "Return the severity rank for DIAGNOSTIC."
  (pcase (downcase (format "%s" (nth 2 diagnostic)))
    ("error" 0)
    ("warning" 1)
    ("note" 2)
    (_ 3)))

(defun mevedel-edit-diagnostics--diagnostic-less-p (left right)
  "Return non-nil when diagnostic LEFT should sort before RIGHT."
  (let ((left-rank (mevedel-edit-diagnostics--diagnostic-rank left))
        (right-rank (mevedel-edit-diagnostics--diagnostic-rank right)))
    (cond
     ((/= left-rank right-rank) (< left-rank right-rank))
     ((not (equal (car left) (car right)))
      (string< (or (car left) "") (or (car right) "")))
     ((/= (or (nth 1 left) 0) (or (nth 1 right) 0))
      (< (or (nth 1 left) 0) (or (nth 1 right) 0)))
     (t (string< (or (nth 3 left) "") (or (nth 3 right) ""))))))

(defun mevedel-edit-diagnostics--format-diagnostic-line (diagnostic)
  "Format one DIAGNOSTIC for model-visible output."
  (pcase-let ((`(,file ,line ,level ,message) diagnostic))
    (format "  %s:%d [%s] %s"
            (if file (file-name-nondirectory file) "?")
            (or line 0) level message)))

(defun mevedel-edit-diagnostics--format-diagnostic-reports (reports)
  "Format pending diagnostic REPORTS with per-file and total limits."
  (let* ((new (sort
               (apply #'append
                      (mapcar (lambda (report)
                                (copy-sequence (plist-get report :new)))
                              reports))
               #'mevedel-edit-diagnostics--diagnostic-less-p))
         (preexisting (sort
                       (apply #'append
                              (mapcar
                               (lambda (report)
                                 (copy-sequence
                                  (plist-get report :preexisting)))
                               reports))
                       #'mevedel-edit-diagnostics--diagnostic-less-p))
         (counts (make-hash-table :test #'equal))
         (total 0)
         (omitted 0)
         selected-new
         selected-preexisting)
    (cl-labels
        ((select
          (diagnostics)
          (let (selected)
            (dolist (diagnostic diagnostics)
              (let* ((file (car diagnostic))
                     (count (gethash file counts 0)))
                (if (and (< total 30) (< count 10))
                    (progn
                      (puthash file (1+ count) counts)
                      (cl-incf total)
                      (push diagnostic selected))
                  (cl-incf omitted))))
            (nreverse selected))))
      (setq selected-new (select new)
            selected-preexisting (select preexisting)))
    (when (or selected-new selected-preexisting (> omitted 0))
      (propertize
       (string-join
        (delq
         nil
         (list
          "Diagnostics from files edited in this turn. They constrain completion of the user's current task; they do not create a separate task."
          (when selected-new
            (concat
             "New or changed diagnostics — resolve these as part of the current task:\n"
             (mapconcat #'mevedel-edit-diagnostics--format-diagnostic-line
                        selected-new "\n")))
          (when selected-preexisting
            (concat
             "Pre-existing diagnostics — context only; do not fix them unless they block the requested work, and mention them only when they affect validation or confidence:\n"
             (mapconcat #'mevedel-edit-diagnostics--format-diagnostic-line
                        selected-preexisting "\n")))
          (when (> omitted 0)
            (format "... %d diagnostics omitted by limits." omitted))))
        "\n\n")
       'mevedel-diagnostics-omitted omitted))))

(defun mevedel-edit-diagnostics--pending-reports (buffer)
  "Return pending diagnostic reports owned by BUFFER's current turn."
  (when-let* ((files (mevedel-edit-diagnostics--files buffer)))
    (let (reports)
      (maphash
       (lambda (_path entry)
         (when-let* ((report (plist-get entry :pending)))
           (push report reports)))
       files)
      (nreverse reports))))

(defun mevedel-edit-diagnostics--clear-pending (buffer)
  "Clear diagnostic reports that were delivered for BUFFER."
  (when-let* ((files (mevedel-edit-diagnostics--files buffer)))
    (maphash
     (lambda (path entry)
       (puthash path (plist-put entry :pending nil) files))
     files)))

(defun mevedel-edit-diagnostics--record-current
    (buffer path current)
  "Classify fresh CURRENT diagnostics for BUFFER and PATH, then queue them."
  (when-let* ((files (and (buffer-live-p buffer)
                          (mevedel-edit-diagnostics--files buffer)))
              (path (expand-file-name path))
              (entry (gethash path files)))
    (let* ((current (delete-dups (copy-sequence current)))
           (first (not (plist-get entry :reported)))
           (previous (if first
                         (plist-get entry :baseline)
                       (plist-get entry :last)))
           (new (cl-set-difference current previous :test #'equal))
           (preexisting
            (and first
                 (cl-intersection current (plist-get entry :baseline)
                                  :test #'equal)))
           (resolved (cl-set-difference previous current :test #'equal))
           (report (list :path path :new new :preexisting preexisting
                         :resolved resolved)))
      (setq entry (plist-put entry :reported t)
            entry (plist-put entry :last current)
            entry (plist-put entry :pending report))
      (puthash path entry files)
      (when-let* ((body
                   (mevedel-edit-diagnostics--format-diagnostic-reports
                    (mevedel-edit-diagnostics--pending-reports buffer))))
        (setq report
              (plist-put report :omitted
                         (get-text-property
                          0 'mevedel-diagnostics-omitted body))
              entry (plist-put entry :pending report))
        (puthash path entry files)
        ;; Clearing pending state is this module's own commit action:
        ;; the delivery queue runs it once the event reaches a request
        ;; payload, replacing the type-specific branch delivery had.
        (mevedel-reminders-queue-turn-event
         buffer 'diagnostics body
         (lambda ()
           (when (buffer-live-p buffer)
             (mevedel-edit-diagnostics--clear-pending buffer)))))
      report)))

(defun mevedel-edit-diagnostics--record-telemetry
    (buffer outcome &optional report)
  "Record count-only diagnostic OUTCOME for BUFFER and REPORT."
  (when-let* ((session (mevedel-telemetry-current-session buffer)))
    (mevedel-telemetry-record
     session 'edit-diagnostics
     :outcome outcome
     :new-count (length (plist-get report :new))
     :preexisting-count (length (plist-get report :preexisting))
     :resolved-count (length (plist-get report :resolved))
     :omitted-count (or (plist-get report :omitted) 0))))

(defun mevedel-edit-diagnostics--run-checkers
    (buffer path source owner continuation flymake-p flycheck-p)
  "Run active diagnostic checkers and settle CONTINUATION once.
BUFFER owns the turn for PATH, SOURCE is its visited file buffer, and OWNER
identifies the turn.  FLYMAKE-P and FLYCHECK-P select the active checkers."
  (let ((pending (delq nil (list (and flymake-p 'flymake)
                                 (and flycheck-p 'flycheck))))
        (initializing t)
        (settled nil)
        (started-p nil)
        (flymake-left 0)
        flymake-completions
        (flymake-starting nil)
        flycheck-hook timer)
    (cl-labels
        ((finish
          (outcome collect-p)
          (unless settled
            (setq settled t)
            (when timer
              (cancel-timer timer)
              (setq timer nil))
            (when (and flycheck-hook (buffer-live-p source))
              (with-current-buffer source
                (remove-hook 'flycheck-after-syntax-check-hook
                             flycheck-hook t)))
            (let* ((owner-current-p
                    (and (buffer-live-p buffer)
                         (eq owner (mevedel-reminders-turn-owner buffer))))
                   (source-live-p (buffer-live-p source))
                   (report
                    (when (and owner-current-p source-live-p collect-p)
                      (mevedel-edit-diagnostics--record-current
                       buffer path
                       (with-current-buffer source
                         (mevedel-edit-diagnostics--collect-in-buffer))))))
              (mevedel-edit-diagnostics--record-telemetry
               buffer (if (and owner-current-p source-live-p) outcome 'stale)
               report)
              (when owner-current-p
                (funcall continuation)))))
         (ready
          (checker)
          (setq pending (delq checker pending))
          (when (and (not initializing) (null pending))
            (finish 'ready t)))
         (make-report-advice
          (make-report &rest args)
          (let ((backend (car args))
                (report (apply make-report args))
                (reported nil)
                complete)
            (cl-incf flymake-left)
            (setq complete
                  (lambda ()
                    (unless reported
                      (setq reported t)
                      (cl-decf flymake-left)
                      (when (and (not flymake-starting)
                                 (zerop flymake-left))
                        (ready 'flymake)))))
            (push (cons backend complete) flymake-completions)
            (lambda (&rest report-args)
              (prog1 (apply report report-args)
                (funcall complete))))))
      (with-current-buffer source
        (when flymake-p
          (setq flymake-starting t)
          (advice-add 'flymake-make-report-fn :around #'make-report-advice)
          (unwind-protect
              (condition-case nil
                  (progn
                    (flymake-start nil t)
                    (dolist (backend (flymake-disabled-backends))
                      (when-let* ((entry (assq backend flymake-completions)))
                        (funcall (cdr entry))))
                    (setq started-p t))
                (error (setq pending (delq 'flymake pending))))
            (advice-remove 'flymake-make-report-fn #'make-report-advice)
            (setq flymake-starting nil))
          (when (zerop flymake-left)
            (ready 'flymake)))
        (when flycheck-p
          (if (condition-case nil
                  (not (flycheck-get-checker-for-buffer))
                (error t))
              (ready 'flycheck)
            (setq flycheck-hook (lambda () (ready 'flycheck)))
            (condition-case nil
                (progn
                  (flycheck-stop)
                  (add-hook 'flycheck-after-syntax-check-hook
                            flycheck-hook nil t)
                  (flycheck-buffer)
                  (setq started-p t))
              (error
               (remove-hook 'flycheck-after-syntax-check-hook
                            flycheck-hook t)
               (setq pending (delq 'flycheck pending)))))))
      (setq initializing nil)
      (if (null pending)
          (if started-p
              (progn
                (mevedel-edit-diagnostics--record-telemetry
                 buffer 'started)
                (finish 'ready t))
            (setq settled t)
            (funcall continuation))
        (mevedel-edit-diagnostics--record-telemetry buffer 'started)
        (setq timer
              (run-at-time
               30 nil
               (lambda ()
                 (finish 'timeout nil))))))))

(defun mevedel-edit-diagnostics-after-edit (buffer path continuation)
  "Refresh BUFFER's diagnostics for PATH, then call CONTINUATION.
Only an existing visited buffer is checked.  Active Flymake and Flycheck
checkers get up to 30 seconds to finish."
  (let* ((path (expand-file-name path))
         (files (and (buffer-live-p buffer)
                     (mevedel-edit-diagnostics--files buffer)))
         (entry (and files (gethash path files)))
         (source (and entry (find-buffer-visiting path)))
         (owner (and source (mevedel-reminders-turn-owner buffer))))
    (if (not (and source owner))
        (funcall continuation)
      (with-current-buffer source
        (if (and (not (verify-visited-file-modtime source))
                 (buffer-modified-p source))
            (setq source nil)
          (unless (verify-visited-file-modtime source)
            (condition-case nil
                (revert-buffer t t t)
              (error (setq source nil))))))
      (if (not source)
          (funcall continuation)
        (with-current-buffer source
          (mevedel-edit-diagnostics--run-checkers
           buffer path source owner continuation
           (and (bound-and-true-p flymake-mode)
                (fboundp 'flymake-start))
           (and (bound-and-true-p flycheck-mode)
                (fboundp 'flycheck-buffer)
                (fboundp 'flycheck-get-checker-for-buffer)
                (fboundp 'flycheck-stop))))))))

(provide 'mevedel-edit-diagnostics)
;;; mevedel-edit-diagnostics.el ends here
