;;; mevedel-view-segments.el -- Historical session segment inspection -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the ephemeral buffers, cursor state, read-only composer presentation,
;; and navigation commands used to inspect archived session segments.  Durable
;; segment bytes and descriptors remain owned by `mevedel-session-artifacts'.

;;; Code:

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-segment
                  "mevedel-session-artifacts" (session number))
(declare-function mevedel-session-artifacts-segments
                  "mevedel-session-artifacts" (session live-buffer))

;; `mevedel-structs'
(declare-function mevedel-session-current-segment
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-view-composer'
(declare-function mevedel-view-composer-session-fork-armed-p
                  "mevedel-view-composer" ())
(declare-function mevedel-view-composer-set-historical-visible
                  "mevedel-view-composer" (visible))

;; `mevedel-view-render'
(declare-function mevedel-view-render-capture-segment-state
                  "mevedel-view-render" ())
(declare-function mevedel-view-render-project-segment
                  "mevedel-view-render" (data-buffer state direction))


;;
;;; State

(defvar-local mevedel-view-segments--number nil
  "Archived segment number currently projected in this view.")

(defvar-local mevedel-view-segments--buffer nil
  "Read-only transcript buffer currently projected in this view.")

(defvar-local mevedel-view-segments--states nil
  "Cursor and disclosure state keyed by archived segment number.")

(defvar-local mevedel-view-segments--live-state nil
  "Cursor and window state captured before historical inspection.")

(defun mevedel-view-historical-segment-p ()
  "Return non-nil when this view projects an archived session segment."
  (and mevedel-view-segments--number
       (buffer-live-p mevedel-view-segments--buffer)))

(defun mevedel-view-segments-current-number ()
  "Return the archived segment projected by this view, or nil."
  (and (mevedel-view-historical-segment-p)
       mevedel-view-segments--number))

(defun mevedel-view-segments-display-buffer ()
  "Return the transcript buffer currently projected by this view."
  (if (mevedel-view-historical-segment-p)
      mevedel-view-segments--buffer
    mevedel--data-buffer))

(defun mevedel-view-segments--session ()
  "Return the live session associated with this view."
  (and (buffer-live-p mevedel--data-buffer)
       (buffer-local-value 'mevedel--session mevedel--data-buffer)))

(defun mevedel-view-segments--cleanup ()
  "Kill the archived segment buffer owned by the current view."
  (when (buffer-live-p mevedel-view-segments--buffer)
    (kill-buffer mevedel-view-segments--buffer))
  (setq mevedel-view-segments--buffer nil
        mevedel-view-segments--number nil))

(defun mevedel-view-segments-initialize ()
  "Initialize historical segment inspection in the current view."
  (setq-local mevedel-view-segments--states (make-hash-table :test #'eql))
  (add-hook 'kill-buffer-hook #'mevedel-view-segments--cleanup nil t))


;;
;;; Presentation

(defvar-keymap mevedel-view-segments--latest-map
  :doc "Keymap for the historical segment Latest action."
  "RET" #'mevedel-view-return-to-latest-segment
  "<mouse-1>" #'mevedel-view-return-to-latest-segment)

(defun mevedel-view-segments-banner ()
  "Return the archived segment banner for the current view."
  (let* ((session (buffer-local-value 'mevedel--session
                                      mevedel--data-buffer))
         (current (or (mevedel-session-current-segment session) 1))
         (latest
          (propertize
           "[Latest]"
           'keymap mevedel-view-segments--latest-map
           'mouse-face 'highlight
           'help-echo "Return to the live session segment")))
    (propertize
     (format "Viewing archived segment %d of %d - read-only %s\n\n"
             mevedel-view-segments--number current latest)
     'read-only t
     'font-lock-face 'shadow
     'mevedel-view-historical-banner t)))


;;
;;; Navigation

(defun mevedel-view-segments--show (number direction)
  "Project session segment NUMBER, landing according to DIRECTION."
  (require 'mevedel-session-artifacts)
  (require 'mevedel-view-render)
  (let* ((session (or (mevedel-view-segments--session)
                      (user-error "Active view has no mevedel session")))
         (current (or (mevedel-session-current-segment session) 1)))
    (if (= number current)
        (mevedel-view-return-to-latest-segment)
      (let* ((new-buffer
              (mevedel-session-artifacts-read-segment session number))
             (old-number mevedel-view-segments--number)
             (old-buffer mevedel-view-segments--buffer)
             (old-state (mevedel-view-render-capture-segment-state))
             (target-state (gethash number mevedel-view-segments--states))
             (view-buffer (current-buffer)))
        (with-current-buffer new-buffer
          (setq-local mevedel--view-buffer view-buffer))
        (if old-number
            (puthash old-number old-state mevedel-view-segments--states)
          (setq mevedel-view-segments--live-state old-state))
        (setq mevedel-view-segments--number number
              mevedel-view-segments--buffer new-buffer)
        (condition-case err
            (progn
              (mevedel-view-composer-set-historical-visible t)
              (mevedel-view-render-project-segment
               new-buffer target-state direction)
              (unless (mevedel-view-composer-session-fork-armed-p)
                (mevedel-view-composer-set-historical-visible nil))
              (when (and (buffer-live-p old-buffer)
                         (not (eq old-buffer new-buffer)))
                (kill-buffer old-buffer)))
          (error
           (setq mevedel-view-segments--number old-number
                 mevedel-view-segments--buffer old-buffer)
           (when (buffer-live-p new-buffer)
             (kill-buffer new-buffer))
           (mevedel-view-composer-set-historical-visible t)
           (mevedel-view-render-project-segment
            (and old-number old-buffer) old-state nil)
           (when (and old-number
                      (not (mevedel-view-composer-session-fork-armed-p)))
             (mevedel-view-composer-set-historical-visible nil))
           (signal (car err) (cdr err))))))))

(defun mevedel-view-previous-segment ()
  "Show the previous session segment without changing session state."
  (interactive)
  (let* ((session (or (mevedel-view-segments--session)
                      (user-error "Active view has no mevedel session")))
         (current (or mevedel-view-segments--number
                      (mevedel-session-current-segment session)))
         (target (1- current)))
    (if (< target 1)
        (message "mevedel: oldest segment")
      (mevedel-view-segments--show target 'backward))))

(defun mevedel-view-next-segment ()
  "Show the next session segment without changing session state."
  (interactive)
  (let* ((session (or (mevedel-view-segments--session)
                      (user-error "Active view has no mevedel session")))
         (latest (or (mevedel-session-current-segment session) 1))
         (current (or mevedel-view-segments--number latest))
         (target (1+ current)))
    (if (> target latest)
        (message "mevedel: latest segment")
      (mevedel-view-segments--show target 'forward))))

(defun mevedel-view-segments--candidate (segment displayed)
  "Return completion text for SEGMENT, marking DISPLAYED."
  (let ((number (plist-get segment :number))
        (status (plist-get segment :status))
        (preview (or (plist-get segment :preview) "(no user message)")))
    (format "%s S%d  %-10s  %s"
            (if (= number displayed) "●" " ")
            number
            (if (plist-get segment :current-p)
                "current"
              (symbol-name status))
            preview)))

(defun mevedel-view-go-to-segment (&optional number)
  "Choose and display session segment NUMBER.

Interactively, offer every canonical segment, including missing and
unreadable entries."
  (interactive)
  (require 'mevedel-session-artifacts)
  (let* ((session (or (mevedel-view-segments--session)
                      (user-error "Active view has no mevedel session")))
         (latest (or (mevedel-session-current-segment session) 1))
         (displayed (or mevedel-view-segments--number latest))
         (segments
          (mevedel-session-artifacts-segments session mevedel--data-buffer))
         (choices
          (mapcar
           (lambda (segment)
             (cons (mevedel-view-segments--candidate segment displayed)
                   (plist-get segment :number)))
           segments))
         (target
          (or number
              (cdr
               (assoc
                (completing-read "Go to segment: " choices nil t)
                choices)))))
    (unless (and (integerp target) (<= 1 target) (<= target latest))
      (user-error "Unknown session segment: %s" target))
    (unless (= target displayed)
      (mevedel-view-segments--show
       target (if (< target displayed) 'backward 'forward)))))

(defun mevedel-view-return-to-latest-segment (&optional _event)
  "Return from archived inspection to the live session segment."
  (interactive)
  (when (mevedel-view-historical-segment-p)
    (require 'mevedel-view-render)
    (let ((old-number mevedel-view-segments--number)
          (old-buffer mevedel-view-segments--buffer)
          (old-state (mevedel-view-render-capture-segment-state))
          (live-state mevedel-view-segments--live-state))
      (puthash old-number old-state mevedel-view-segments--states)
      (setq mevedel-view-segments--number nil
            mevedel-view-segments--buffer nil)
      (condition-case err
          (progn
            (mevedel-view-composer-set-historical-visible t)
            (mevedel-view-render-project-segment
             mevedel--data-buffer live-state 'forward)
            (when (buffer-live-p old-buffer)
              (kill-buffer old-buffer))
            (setq mevedel-view-segments--live-state nil))
        (error
         (setq mevedel-view-segments--number old-number
               mevedel-view-segments--buffer old-buffer)
         (mevedel-view-render-project-segment old-buffer old-state nil)
         (mevedel-view-composer-set-historical-visible nil)
         (signal (car err) (cdr err)))))))

(provide 'mevedel-view-segments)
;;; mevedel-view-segments.el ends here
