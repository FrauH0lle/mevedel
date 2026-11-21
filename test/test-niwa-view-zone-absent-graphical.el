;;; test-niwa-view-zone-absent-graphical.el --- Absent-zone viewport equivalence -*- lexical-binding: t -*-

;;; Commentary:

;; A zone with no fragments, no live region and no stale text has nothing to
;; reconcile, so it needs no viewport transaction.  These native cases drive
;; real stream updates and compare, after every update, each window's start,
;; pixel vscroll, composer row and point, and the buffer's point, with the
;; full transaction that every reconciliation used to run (ADR 0126).
;; Batch Emacs has no native visibility queries, so the cases need a display.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-markdown)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-view-zone)

(defun niwa-view-zone-absent-test--history (kind)
  "Return response text of KIND for the earlier, settled turn."
  (pcase kind
    ('short "Short earlier answer.\n")
    ('wrapped
     (mapconcat (lambda (n) (format "Wrapped %d %s\n" n (make-string 150 ?w)))
                (number-sequence 1 30) ""))
    ('table
     (concat (mapconcat (lambda (n) (format "Line %d before the table.\n\n" n))
                        (number-sequence 1 20) "")
             "| A | B | C |\n|---|---|---|\n"
             (mapconcat (lambda (n) (format "| row %d | value %d with words | more |\n" n n))
                        (number-sequence 1 12) "")
             "\nAfter the table.\n"))
    (_ (mapconcat (lambda (n) (format "Line %d of the earlier response.\n" n))
                  (number-sequence 0 59) ""))))

(defun niwa-view-zone-absent-test--last-page (window)
  "Show the buffer's end on WINDOW's last body row, as growth leaves it."
  (let* ((end (point-max))
         (y (- (cdr (niwa-view--window-body-geometry window))
               (with-selected-window window
                 (save-excursion (goto-char end) (line-pixel-height))))))
    (niwa-view--set-viewport window (niwa-view--viewport-at-y window end y))))

(defun niwa-view-zone-absent-test--arrange (roles)
  "Split the selected window into one window per element of ROLES.
A role is `composer', `top' (composer window showing the buffer start),
`vscroll' (composer window scrolled by a partial row), `composer-2' (a
second composer window two rows higher), `reader', `status' or
`interaction' (a window visiting that row).  Return the windows."
  (delete-other-windows)
  (let ((windows (list (selected-window))))
    (dotimes (_ (1- (length roles)))
      (let ((window (split-window (car (last windows)) nil 'below)))
        (set-window-buffer window (current-buffer))
        (setq windows (append windows (list window)))))
    (redisplay t)
    (cl-mapc
     (lambda (window role)
       (pcase role
         ((or 'composer 'vscroll 'composer-2)
          (set-window-point window (+ 4 (niwa-view-composer-draft-start)))
          (niwa-view-zone-absent-test--last-page window)
          (when (eq role 'composer-2)
            (let ((y (niwa-view--position-screen-y
                      (niwa-view-composer-draft-start) window)))
              (redisplay t)
              (niwa-view--set-viewport
               window (niwa-view--viewport-at-y
                       window (niwa-view-composer-draft-start)
                       (- (or y 0) (* 2 (frame-char-height)))))))
          (when (eq role 'vscroll)
            (set-window-vscroll window (+ (window-vscroll window t) 7) t t)))
         ('top
          (set-window-point window (+ 4 (niwa-view-composer-draft-start)))
          (set-window-start window (point-min) t))
         (_
          (let ((visit (save-excursion
                         (goto-char (point-min))
                         (search-forward (pcase role
                                           ('reader "Line 20 of")
                                           ('status "[Manage]")
                                           ('interaction "Manage queue")))
                         (match-beginning 0))))
            (when (eq role 'status)
              (should (>= visit niwa-view--status-marker)))
            (set-window-point window visit)
            (niwa-view--set-viewport
             window (niwa-view--viewport-at-y window visit (frame-char-height)))))))
     windows roles)
    (select-window (car windows))
    (goto-char (window-point (car windows)))
    (redisplay t)
    windows))

(defun niwa-view-zone-absent-test--observe (windows)
  "Return point and each of WINDOWS' start, vscroll, composer Y and point.
Observe the writers' result and, after redisplay, what the user sees."
  (cl-flet ((state ()
              (list (point)
                    (mapcar (lambda (window)
                              (list (window-start window)
                                    (window-vscroll window t)
                                    (niwa-view--position-screen-y
                                     (niwa-view-composer-draft-start) window)
                                    (window-point window)))
                            windows))))
    (let ((written (state)))
      (redisplay t)
      (list written (state)))))

(defun niwa-view-zone-absent-test--run (spec full)
  "Drive SPEC's stream updates; return observations and the update count.
SPEC is a plist with :history, :roles, :chunks and optional :setup.
:executions lists running background commands for the status panel.
With FULL, absent zones still run the full viewport transaction."
  (let ((absent (symbol-function 'niwa-view-zone--absent-p))
        (session (niwa-session--create :name "absent-zone"))
        (executions (plist-get spec :executions))
        observations)
    (save-window-excursion
      (delete-other-windows)
      (niwa-view-test--with-buffers
        (switch-to-buffer view-buf)
        (setq-local niwa--session session)
        (with-current-buffer data-buf (setq-local niwa--session session))
        (niwa-view-test--insert-data
         data-buf (niwa-view-zone-absent-test--history (plist-get spec :history))
         'response)
        (let ((clock 1000.0)
              (original-float-time (symbol-function 'float-time))
              (niwa-view-spinner-animate nil))
          (cl-letf (((symbol-function 'niwa-view-zone--absent-p)
                     (if full #'ignore absent))
                    ((symbol-function 'current-time) (lambda () (seconds-to-time clock)))
                    ((symbol-function 'float-time)
                     (lambda (&optional time)
                       (if time (funcall original-float-time time) clock)))
                    ((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                    ((symbol-function 'niwa-execution-count-user)
                     (lambda (_) (length executions)))
                    ((symbol-function 'niwa-execution-list-user)
                     (lambda (_) executions)))
            (when-let* ((setup (plist-get spec :setup)))
              (funcall setup session))
            (niwa-view--full-rerender)
            (when (eq (plist-get spec :history) 'table)
              (niwa-view--realign-markdown)
              (should (text-property-not-all (point-min) (point-max)
                                             'niwa-view-table-source nil)))
            (when (plist-get spec :remap)
              (text-scale-set 1))
            (niwa-view-stream-begin-turn
             (niwa-view--history-insertion-marker)
             (with-current-buffer data-buf (point-max)))
            (niwa-view-test--insert-data data-buf "Streaming begins." 'response)
            (niwa-view--render-stream-update data-buf)
            (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
            (when-let* ((stale (plist-get spec :stale)))
              (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                (save-excursion
                  (goto-char (niwa-view--history-insertion-marker))
                  (insert (propertize stale 'niwa-view-zone-namespace 'status
                                      'niwa-view-zone-region 'dead)))))
            (let ((windows (niwa-view-zone-absent-test--arrange
                            (plist-get spec :roles))))
              (push (niwa-view-zone-absent-test--observe windows) observations)
              (dolist (chunk (plist-get spec :chunks))
                (niwa-view-test--insert-data data-buf chunk 'response)
                (niwa-view--render-stream-update data-buf)
                (push (niwa-view-zone-absent-test--observe windows)
                      observations)))))))
    (nreverse observations)))

(defconst niwa-view-zone-absent-test--growth
  '(" word word word" "\n\nA new paragraph." " more words to wrap the line further"
    "\n\n- item one\n- item two" " tail")
  "Stream chunks that grow the live turn by words, rows and paragraphs.")

(niwa-deftest niwa-view-zone-absent-equivalence ()
  (progn
    (skip-unless (display-graphic-p))
    (let* ((spec ,spec)
           (skipped (niwa-view-zone-absent-test--run spec nil))
           (full (niwa-view-zone-absent-test--run spec t)))
      (should (= (length skipped) (1+ (length (plist-get spec :chunks)))))
      (should (equal full skipped))))
  (spec)
  :doc "plain growth keeps the composer row"
  (list :roles '(composer) :chunks niwa-view-zone-absent-test--growth)
  :doc "a short view keeps its start while free space remains"
  (list :history 'short :roles '(top) :chunks '(" word" " more" "\nrow"))
  :doc "a burst from a short view fills the last page"
  (list :history 'short :roles '(top)
        :chunks (list (mapconcat (lambda (n) (format "\nburst %d" n))
                                 (number-sequence 1 80) "")
                      " after"))
  :doc "a history reader keeps its reading position"
  (list :roles '(composer reader) :chunks niwa-view-zone-absent-test--growth)
  :doc "a visited status row keeps its row"
  (list :roles '(composer status) :chunks niwa-view-zone-absent-test--growth
        :executions (list (list :execution-id "absent" :owner "/root"
                                :yielded t :state 'running
                                :description "Background command"
                                :started-at 990.0)))
  :doc "a visited interaction row keeps its row"
  (list :roles '(composer interaction) :chunks niwa-view-zone-absent-test--growth
        :setup (lambda (session)
                 (setf (niwa-session-pending-steering session)
                       (list (list :id 1 :category 'steering :state 'queued
                                   :input "steer here")))))
  :doc "a partial-pixel vscroll keeps the composer row"
  (list :roles '(vscroll) :chunks niwa-view-zone-absent-test--growth)
  :doc "wrapped history under a remapped face keeps the composer row"
  (list :history 'wrapped :remap t :roles '(composer)
        :chunks niwa-view-zone-absent-test--growth)
  :doc "a rendered table in the span keeps the composer row"
  (list :history 'table :roles '(composer) :chunks niwa-view-zone-absent-test--growth)
  :doc "mixed windows anchor independently"
  (list :roles '(composer reader composer-2 interaction)
        :chunks niwa-view-zone-absent-test--growth
        :setup (lambda (session)
                 (setf (niwa-session-pending-steering session)
                       (list (list :id 1 :category 'steering :state 'queued
                                   :input "steer here")))))
  :doc "stale zone text still takes the full transaction"
  (list :roles '(composer reader) :stale "Stale status\n"
        :chunks niwa-view-zone-absent-test--growth))

(provide 'test-niwa-view-zone-absent-graphical)
;;; test-niwa-view-zone-absent-graphical.el ends here
