;;; mevedel-execution-scheduler.el -- Fair Bash admission -*- lexical-binding: t -*-

;;; Commentary:

;; Session-scoped readers/writer admission for managed Bash.  Recognized
;; read-only calls may overlap; every other call uses the exclusive lane.

;;; Code:

(eval-when-compile (require 'cl-lib))

(cl-defstruct (mevedel-execution-scheduler
               (:constructor mevedel-execution-scheduler--create))
  "Private readers/writer scheduler state."
  (active-readers 0)
  writer-p
  queue)

(cl-defstruct (mevedel-execution-scheduler--lease
               (:constructor mevedel-execution-scheduler--lease-create))
  "One queued, active, or released scheduler admission."
  scheduler
  mode
  start
  state)

(defun mevedel-execution-scheduler-create ()
  "Return an empty readers/writer scheduler."
  (mevedel-execution-scheduler--create))

(defun mevedel-execution-scheduler--startable-p (scheduler mode)
  "Return non-nil when SCHEDULER may admit MODE now."
  (and (not (mevedel-execution-scheduler-writer-p scheduler))
       (or (eq mode 'read)
           (zerop
            (mevedel-execution-scheduler-active-readers scheduler)))))

(defun mevedel-execution-scheduler--start-active (lease)
  "Invoke active LEASE's start function without stranding admission on error."
  (let ((completed-p nil))
    (unwind-protect
        (progn
          (condition-case err
              (funcall (mevedel-execution-scheduler--lease-start lease) lease)
            (t
             (mevedel-execution-scheduler-release lease)
             (message "mevedel: execution scheduler start failed: %S" err)))
          (setq completed-p t))
      (unless completed-p
        (mevedel-execution-scheduler-release lease)))))

(defun mevedel-execution-scheduler--activate (lease)
  "Activate queued LEASE and invoke its start function."
  (let* ((scheduler
          (mevedel-execution-scheduler--lease-scheduler lease))
         (mode (mevedel-execution-scheduler--lease-mode lease)))
    (setf (mevedel-execution-scheduler--lease-state lease) 'active)
    (if (eq mode 'read)
        (cl-incf (mevedel-execution-scheduler-active-readers scheduler))
      (setf (mevedel-execution-scheduler-writer-p scheduler) t))
    (mevedel-execution-scheduler--start-active lease)))

(defun mevedel-execution-scheduler--drain (scheduler)
  "Admit the next startable lease from SCHEDULER."
  (when-let* ((lease (car (mevedel-execution-scheduler-queue scheduler)))
              (mode (mevedel-execution-scheduler--lease-mode lease))
              ((mevedel-execution-scheduler--startable-p scheduler mode)))
    (if (eq mode 'exclusive)
        (progn
          (setf (mevedel-execution-scheduler-queue scheduler)
                (cdr (mevedel-execution-scheduler-queue scheduler)))
          (mevedel-execution-scheduler--activate lease))
      (let (readers pending)
        (while (and (mevedel-execution-scheduler-queue scheduler)
                    (eq (mevedel-execution-scheduler--lease-mode
                         (car (mevedel-execution-scheduler-queue scheduler)))
                        'read))
          (push (pop (mevedel-execution-scheduler-queue scheduler)) readers))
        (setq readers (nreverse readers))
        (cl-incf (mevedel-execution-scheduler-active-readers scheduler)
                 (length readers))
        (dolist (reader readers)
          (setf (mevedel-execution-scheduler--lease-state reader) 'active))
        (setq pending readers)
        (unwind-protect
            (while pending
              (mevedel-execution-scheduler--start-active (pop pending)))
          (dolist (reader pending)
            (mevedel-execution-scheduler-release reader)))))))

(defun mevedel-execution-scheduler-submit (scheduler mode start)
  "Submit START to SCHEDULER in read or exclusive MODE.

START is called with its lease when admitted.  Return the opaque lease."
  (unless (memq mode '(read exclusive))
    (error "Unknown execution scheduler mode: %S" mode))
  (let ((lease
         (mevedel-execution-scheduler--lease-create
          :scheduler scheduler :mode mode :start start :state 'queued)))
    (if (and (null (mevedel-execution-scheduler-queue scheduler))
             (mevedel-execution-scheduler--startable-p scheduler mode))
        (mevedel-execution-scheduler--activate lease)
      (setf (mevedel-execution-scheduler-queue scheduler)
            (append (mevedel-execution-scheduler-queue scheduler)
                    (list lease))))
    lease))

(defun mevedel-execution-scheduler-release (lease)
  "Release active LEASE exactly once."
  (when (eq (mevedel-execution-scheduler--lease-state lease) 'active)
    (let ((scheduler
           (mevedel-execution-scheduler--lease-scheduler lease))
          (mode (mevedel-execution-scheduler--lease-mode lease)))
      (if (eq mode 'read)
          (cl-decf (mevedel-execution-scheduler-active-readers scheduler))
        (setf (mevedel-execution-scheduler-writer-p scheduler) nil))
      (setf (mevedel-execution-scheduler--lease-state lease) 'released)
      (mevedel-execution-scheduler--drain scheduler)
      t)))

(defun mevedel-execution-scheduler-cancel (lease)
  "Cancel queued LEASE and return non-nil when it was removed."
  (when (eq (mevedel-execution-scheduler--lease-state lease) 'queued)
    (let ((scheduler
           (mevedel-execution-scheduler--lease-scheduler lease)))
      (setf (mevedel-execution-scheduler-queue scheduler)
            (delq lease (mevedel-execution-scheduler-queue scheduler))
            (mevedel-execution-scheduler--lease-state lease) 'released)
      (mevedel-execution-scheduler--drain scheduler)
      t)))

(provide 'mevedel-execution-scheduler)
;;; mevedel-execution-scheduler.el ends here
