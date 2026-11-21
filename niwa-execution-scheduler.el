;;; niwa-execution-scheduler.el -- Fair Bash admission -*- lexical-binding: t -*-

;;; Commentary:

;; Session-scoped readers/writer admission for managed Bash.  Recognized
;; read-only calls may overlap; every other call uses the exclusive lane.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-utilities)

(cl-defstruct (niwa-execution-scheduler
               (:constructor niwa-execution-scheduler--create))
  "Private readers/writer scheduler state."
  (active-readers 0)
  writer-p
  queue)

(cl-defstruct (niwa-execution-scheduler--lease
               (:constructor niwa-execution-scheduler--lease-create))
  "One queued, active, or released scheduler admission."
  scheduler
  mode
  start
  admit-p
  reject
  scope
  state)

(defun niwa-execution-scheduler-create ()
  "Return an empty readers/writer scheduler."
  (niwa-execution-scheduler--create))

(defun niwa-execution-scheduler--startable-p (scheduler mode)
  "Return non-nil when SCHEDULER may admit MODE now."
  (and (not (niwa-execution-scheduler-writer-p scheduler))
       (or (eq mode 'read)
           (zerop
            (niwa-execution-scheduler-active-readers scheduler)))))

(defun niwa-execution-scheduler--deactivate (lease)
  "Release active LEASE without admitting queued work; return its scheduler."
  (let ((scheduler
         (niwa-execution-scheduler--lease-scheduler lease))
        (mode (niwa-execution-scheduler--lease-mode lease)))
    (if (eq mode 'read)
        (cl-decf (niwa-execution-scheduler-active-readers scheduler))
      (setf (niwa-execution-scheduler-writer-p scheduler) nil))
    (setf (niwa-execution-scheduler--lease-state lease) 'released)
    scheduler))

(defun niwa-execution-scheduler--start-active (lease)
  "Invoke active LEASE's start function without stranding admission on error."
  (if (and (niwa-execution-scheduler--lease-admit-p lease)
           (not
            (condition-case err
                (funcall
                 (niwa-execution-scheduler--lease-admit-p lease))
              (error
               (niwa-diagnostic-message
                (niwa-execution-scheduler--lease-scope lease)
                "execution admission check failed: %S" err)
               nil))))
      (let ((scheduler (niwa-execution-scheduler--deactivate lease)))
        (when-let* ((reject
                     (niwa-execution-scheduler--lease-reject lease)))
          (condition-case err
              (funcall reject lease)
            (error
             (niwa-diagnostic-message
              (niwa-execution-scheduler--lease-scope lease)
              "execution scheduler rejection failed: %S" err))))
        (niwa-execution-scheduler--drain scheduler))
    (let ((completed-p nil))
      (unwind-protect
          (progn
            (condition-case err
                (funcall
                 (niwa-execution-scheduler--lease-start lease) lease)
              (t
               (niwa-execution-scheduler-release lease)
               (niwa-diagnostic-message
                (niwa-execution-scheduler--lease-scope lease)
                "execution scheduler start failed: %S" err)))
            (setq completed-p t))
        (unless completed-p
          (niwa-execution-scheduler-release lease))))))

(defun niwa-execution-scheduler--activate (lease)
  "Activate queued LEASE and invoke its start function."
  (let* ((scheduler
          (niwa-execution-scheduler--lease-scheduler lease))
         (mode (niwa-execution-scheduler--lease-mode lease)))
    (setf (niwa-execution-scheduler--lease-state lease) 'active)
    (if (eq mode 'read)
        (cl-incf (niwa-execution-scheduler-active-readers scheduler))
      (setf (niwa-execution-scheduler-writer-p scheduler) t))
    (niwa-execution-scheduler--start-active lease)))

(defun niwa-execution-scheduler--drain (scheduler)
  "Admit the next startable lease from SCHEDULER."
  (when-let* ((lease (car (niwa-execution-scheduler-queue scheduler)))
              (mode (niwa-execution-scheduler--lease-mode lease))
              ((niwa-execution-scheduler--startable-p scheduler mode)))
    (if (eq mode 'exclusive)
        (progn
          (setf (niwa-execution-scheduler-queue scheduler)
                (cdr (niwa-execution-scheduler-queue scheduler)))
          (niwa-execution-scheduler--activate lease))
      (let (readers pending)
        (while (and (niwa-execution-scheduler-queue scheduler)
                    (eq (niwa-execution-scheduler--lease-mode
                         (car (niwa-execution-scheduler-queue scheduler)))
                        'read))
          (push (pop (niwa-execution-scheduler-queue scheduler)) readers))
        (setq readers (nreverse readers))
        (cl-incf (niwa-execution-scheduler-active-readers scheduler)
                 (length readers))
        (dolist (reader readers)
          (setf (niwa-execution-scheduler--lease-state reader) 'active))
        (setq pending readers)
        (unwind-protect
            (while pending
              (niwa-execution-scheduler--start-active (pop pending)))
          (dolist (reader pending)
            (niwa-execution-scheduler-release reader)))))))

(defun niwa-execution-scheduler-submit
    (scheduler mode start &optional admit-p reject scope)
  "Submit START to SCHEDULER in read or exclusive MODE.

START is called with its lease when admitted.  ADMIT-P is rechecked immediately
before START; when it returns nil, REJECT receives the released lease instead.
SCOPE is the originating diagnostic scope, captured before queuing.
Return the opaque lease."
  (unless (memq mode '(read exclusive))
    (error "Unknown execution scheduler mode: %S" mode))
  (let ((lease
         (niwa-execution-scheduler--lease-create
          :scheduler scheduler :mode mode :start start
          :admit-p admit-p :reject reject
          :scope (niwa-diagnostic-scope scope) :state 'queued)))
    (if (and (null (niwa-execution-scheduler-queue scheduler))
             (niwa-execution-scheduler--startable-p scheduler mode))
        (niwa-execution-scheduler--activate lease)
      (setf (niwa-execution-scheduler-queue scheduler)
            (append (niwa-execution-scheduler-queue scheduler)
                    (list lease))))
    lease))

(defun niwa-execution-scheduler-release (lease)
  "Release active LEASE exactly once."
  (when (eq (niwa-execution-scheduler--lease-state lease) 'active)
    (let ((scheduler (niwa-execution-scheduler--deactivate lease)))
      (niwa-execution-scheduler--drain scheduler)
      t)))

(defun niwa-execution-scheduler-cancel (lease)
  "Cancel queued LEASE and return non-nil when it was removed."
  (when (eq (niwa-execution-scheduler--lease-state lease) 'queued)
    (let ((scheduler
           (niwa-execution-scheduler--lease-scheduler lease)))
      (setf (niwa-execution-scheduler-queue scheduler)
            (delq lease (niwa-execution-scheduler-queue scheduler))
            (niwa-execution-scheduler--lease-state lease) 'released)
      (niwa-execution-scheduler--drain scheduler)
      t)))

(provide 'niwa-execution-scheduler)
;;; niwa-execution-scheduler.el ends here
