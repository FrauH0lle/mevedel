;;; test-niwa-view-pair-close.el --- Closing a docked view -*- lexical-binding: t -*-

;;; Commentary:

;; Killing, quitting or deleting a docked view or composer must not signal from
;; window deletion, and must leave a sensible layout.  The deferred repair is a
;; no-op in batch, so these cases run it directly.

;;; Code:

(require 'niwa-view-composer-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-view-composer-test-helpers"))
(require 'niwa-execution)
(require 'niwa-view)
(require 'niwa-view-pair)

(defmacro niwa-view-pair-close--with-chat (&rest body)
  "Run BODY in a batch chat fixture with a docked composer."
  (declare (indent 0))
  `(let ((niwa-view-composer-graphical--allow-batch t))
     (niwa-view-composer-graphical--with-chat
       (should (niwa-view-composer-window (get-buffer-window view-buf)))
       ,@body)))

(defmacro niwa-view-pair-close--signals (&rest body)
  "Return the number of `delete-window' calls inside BODY that signalled."
  `(let ((failures 0))
     (advice-add 'delete-window :around
                 (lambda (fn &rest args)
                   (condition-case err (apply fn args)
                     (error (cl-incf failures) (signal (car err) (cdr err)))))
                 '((name . niwa-view-pair-close)))
     (unwind-protect (progn ,@body)
       (advice-remove 'delete-window 'niwa-view-pair-close))
     failures))

(defun niwa-view-pair-close--shown ()
  "Return the buffers shown on the selected frame."
  (mapcar #'window-buffer (window-list nil 'nomini)))

(defun niwa-view-pair-close--set-restore-record (transcript buffer dock)
  "Give TRANSCRIPT a single quit record restoring BUFFER and selecting DOCK."
  ;; A reused window may also have a newer record, which native quit tries first.
  (set-window-parameter transcript 'quit-restore-prev nil)
  (set-window-parameter transcript 'quit-restore
                        (list 'same (list buffer 1 1 nil) dock (window-buffer transcript))))

(defun niwa-view-pair-close--neighbor ()
  "Split an unrelated *scratch* window above the pair and return it."
  (let ((other (split-window (frame-root-window) nil 'above)))
    (set-window-buffer other (get-buffer-create "*scratch*"))
    other))

(defun niwa-view-pair-close--draft-text (view)
  "Return VIEW's draft text."
  (niwa-view-composer-graphical--draft view))

(ert-deftest niwa-view-pair-close/kill-view ()
  "Killing the view leaves no stale view or draft window, full frame or split."
  (niwa-view-pair-close--with-chat
    (let ((draft (window-buffer (selected-window))))
      (should (= 0 (niwa-view-pair-close--signals (kill-buffer view-buf))))
      (niwa-view-pair--repair (selected-frame))
      (should-not (buffer-live-p draft))
      (should (equal (list (get-buffer "*scratch*")) (niwa-view-pair-close--shown)))))
  (niwa-view-pair-close--with-chat
    (let ((other (niwa-view-pair-close--neighbor)))
      (should (= 0 (niwa-view-pair-close--signals (kill-buffer view-buf))))
      (niwa-view-pair--repair (selected-frame))
      (should (window-live-p other))
      (should (equal (list (get-buffer "*scratch*") (get-buffer "*scratch*"))
                     (niwa-view-pair-close--shown))))))

(ert-deftest niwa-view-pair-close/kill-composer ()
  "Killing the draft requests one close of its view; a veto changes nothing."
  (niwa-view-pair-close--with-chat
    (insert "unsent")
    (let* ((draft (current-buffer))
           (windows (window-list nil 'nomini))
           (asked 0)
           (allow nil))
      (cl-letf (((symbol-function 'niwa-view--allow-session-close-p)
                 (lambda () (when (eq (current-buffer) view-buf) (cl-incf asked)) allow)))
        (should-not (kill-buffer draft))
        (should (= 1 asked))
        (should (buffer-live-p draft))
        (should (buffer-live-p view-buf))
        (niwa-view-pair--repair (selected-frame))
        (should-not (cl-set-exclusive-or windows (window-list nil 'nomini)))
        (should (equal "unsent" (niwa-view-pair-close--draft-text view-buf)))
        (setq allow t asked 0)
        (kill-buffer draft)
        (should (= 1 asked))
        (should-not (buffer-live-p view-buf))
        (should-not (buffer-live-p draft))
        (niwa-view-pair--repair (selected-frame))
        (should (equal (list (get-buffer "*scratch*")) (niwa-view-pair-close--shown)))))))

(ert-deftest niwa-view-pair-close/kill-view-asks-once ()
  "A view's teardown does not give its data buffer a second veto."
  (niwa-view-pair-close--with-chat
    (let ((asks 0))
      ;; A mutation is settled for the view's ask and unsettled for any later one.
      (cl-letf (((symbol-function 'niwa-execution-unsettled-mutation-p)
                 (lambda (_session) (> (cl-incf asks) 1))))
        (kill-buffer view-buf)
        (should (= 1 asks))
        (should-not (buffer-live-p view-buf))
        (should-not (buffer-live-p data-buf))))))

(ert-deftest niwa-view-pair-close/teardown-covers-only-its-data ()
  "A view's data exemption does not cover another session's data buffer."
  (niwa-view-pair-close--with-chat
    (let ((niwa-view--closing-data (get-buffer-create " *other data*")))
      (cl-letf (((symbol-function 'niwa-execution-unsettled-mutation-p)
                 (lambda (_session) t)))
        (should-not (kill-buffer data-buf))
        (should (buffer-live-p data-buf)))
      (kill-buffer niwa-view--closing-data))))

(ert-deftest niwa-view-pair-close/release-covers-only-its-draft ()
  "Releasing one draft does not bypass the close request of another."
  (niwa-view-pair-close--with-chat
    (let ((draft (current-buffer))
          (niwa-view-composer--releasing (get-buffer-create " *other draft*")))
      (cl-letf (((symbol-function 'niwa-view--allow-session-close-p) #'ignore))
        (should-not (kill-buffer draft))
        (should (buffer-live-p view-buf)))
      (kill-buffer niwa-view-composer--releasing))))

(ert-deftest niwa-view-pair-close/quit-key ()
  "The `q' key in a view's display area quits like `quit-window'."
  (niwa-view-pair-close--with-chat
    (let ((draft (window-buffer (selected-window))))
      (select-window (get-buffer-window view-buf))
      (goto-char (point-min))
      (should (eq 'niwa-view-quit (key-binding (kbd "q"))))
      (should (= 0 (niwa-view-pair-close--signals
                    (call-interactively (key-binding (kbd "q"))))))
      (niwa-view-pair--repair (selected-frame))
      (should (equal (list (get-buffer "*scratch*")) (niwa-view-pair-close--shown)))
      (should (buffer-live-p view-buf))
      (should (buffer-live-p draft)))))

(ert-deftest niwa-view-pair-close/quit-composer ()
  "Quitting the dock of a full-frame pair ends like quitting its transcript."
  (dolist (selected '(composer transcript))
    (niwa-view-pair-close--with-chat
      (let* ((dock (selected-window))
             (draft (window-buffer dock))
             (transcript (get-buffer-window view-buf))
             (other (get-buffer-create "*other*")))
        ;; The dock's own history differs from the transcript window's.
        (set-window-prev-buffers dock (list (list other (point-min-marker)
                                                  (point-min-marker))))
        (when (eq selected 'transcript) (select-window transcript))
        (should (= 0 (niwa-view-pair-close--signals (quit-window nil dock))))
        (should (equal (list (get-buffer "*scratch*")) (niwa-view-pair-close--shown)))
        (niwa-view-pair--repair (selected-frame))
        (should (equal (list (get-buffer "*scratch*")) (niwa-view-pair-close--shown)))
        (should (buffer-live-p view-buf))
        (should (buffer-live-p draft))
        (should-not (get-buffer-window draft t))
        (should-not (get-buffer-window view-buf t))
        (kill-buffer other)))))

(ert-deftest niwa-view-pair-close/quit-unknown-target ()
  "With the draft shown in several windows the quit hook leaves the pair alone.
Another frame cannot be made in batch, so the second display is simulated."
  (niwa-view-pair-close--with-chat
    (let* ((dock (selected-window))
           (transcript (get-buffer-window view-buf)))
      (cl-letf (((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _) (list dock dock))))
        (niwa-view-pair--quit-dock))
      (should (window-live-p transcript))
      (should (eq view-buf (window-buffer transcript)))
      (should (eq (niwa-view-composer-window transcript) dock))
      (niwa-view-pair--quit-dock)
      (should-not (window-live-p transcript)))))

(ert-deftest niwa-view-pair-close/quit-kill-veto ()
  "A vetoed kill-quit of the dock keeps view and draft alive and signals nothing."
  (niwa-view-pair-close--with-chat
    (insert "unsent")
    (let ((draft (current-buffer)))
      (cl-letf (((symbol-function 'niwa-view--allow-session-close-p) #'ignore))
        (should (= 0 (niwa-view-pair-close--signals (quit-window t))))
        (should (buffer-live-p view-buf))
        (should (buffer-live-p draft))
        (should (equal "unsent" (niwa-view-pair-close--draft-text view-buf)))))))

(ert-deftest niwa-view-pair-close/quit-restore-record ()
  "The transcript's restore record beats its history when the dock is quit."
  (niwa-view-pair-close--with-chat
    (let* ((dock (selected-window))
           (transcript (get-buffer-window view-buf))
           (a (get-buffer-create "*record*"))
           (b (get-buffer-create "*history*")))
      (set-window-prev-buffers transcript
                               (list (list b (point-min-marker) (point-min-marker))))
      (niwa-view-pair-close--set-restore-record transcript a dock)
      (should (= 0 (niwa-view-pair-close--signals (quit-window nil dock))))
      (should (equal (list a) (niwa-view-pair-close--shown)))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest niwa-view-pair-close/quit-restore-stale-dock-record ()
  "The dock's own stale restore record does not outrank the transcript's."
  (niwa-view-pair-close--with-chat
    (let* ((dock (selected-window))
           (draft (window-buffer dock))
           (transcript (get-buffer-window view-buf))
           (a (get-buffer-create "*record*"))
           (stale (get-buffer-create "*stale*")))
      (niwa-view-pair-close--set-restore-record transcript a dock)
      (set-window-parameter dock 'quit-restore-prev
                            (list 'same (list stale 1 1 nil) dock draft))
      (should (= 0 (niwa-view-pair-close--signals (quit-window nil dock))))
      (should (equal (list a) (niwa-view-pair-close--shown)))
      (kill-buffer a)
      (kill-buffer stale))))

(ert-deftest niwa-view-pair-close/repair-is-observational ()
  "Showing the view in the dock window is not a quit; repair never quits."
  (niwa-view-pair-close--with-chat
    (let ((dock (selected-window)) (quits 0))
      (set-window-dedicated-p dock nil)
      (set-window-buffer dock view-buf)
      (cl-letf (((symbol-function 'quit-window) (lambda (&rest _) (cl-incf quits))))
        (niwa-view-pair--repair (selected-frame)))
      (should (= 0 quits))
      (should (buffer-live-p view-buf)))))

(ert-deftest niwa-view-pair-close/delete-window ()
  "Deleting either member quits a full-frame pair and closes a pair in a split."
  (dolist (member '(composer transcript))
    (niwa-view-pair-close--with-chat
      (insert "keep me")
      (let* ((dock (selected-window))
             (draft (window-buffer dock))
             (transcript (niwa-view-transcript-window dock)))
        (select-window (if (eq member 'composer) dock transcript))
        (delete-window)
        (niwa-view-pair--repair (selected-frame))
        (should (equal (list (get-buffer "*scratch*")) (niwa-view-pair-close--shown)))
        (should (buffer-live-p view-buf))
        (should (buffer-live-p draft))
        (should-not (get-buffer-window view-buf t))
        (should (equal "keep me" (niwa-view-pair-close--draft-text view-buf))))))
  (niwa-view-pair-close--with-chat
    (let ((other (niwa-view-pair-close--neighbor)))
      (select-window (get-buffer-window view-buf))
      (delete-window)
      (should (equal (list other) (window-list nil 'nomini)))
      (should (eq (get-buffer "*scratch*") (window-buffer other))))))

(ert-deftest niwa-view-pair-close/delete-target-and-kill-option ()
  "Explicit deletion acts on its target and never kills, whatever else is set."
  (dolist (variant '(non-selected second-display kill-option draft-local-kill))
    (niwa-view-pair-close--with-chat
      (let* ((dock (selected-window))
             (draft (window-buffer dock))
             (transcript (niwa-view-transcript-window dock))
             (quit-window-kill-buffer (eq variant 'kill-option)))
        (when (eq variant 'draft-local-kill)
          (with-current-buffer draft (setq-local quit-window-kill-buffer t)))
        (when (eq variant 'non-selected) (select-window transcript))
        (cl-letf (((symbol-function 'get-buffer-window-list)
                   (if (eq variant 'second-display)
                       (lambda (&rest _) (list dock dock))
                     (symbol-function 'get-buffer-window-list))))
          ;; Another buffer is current, as when a command closes the window.
          (with-temp-buffer
            (delete-window (if (eq variant 'non-selected) dock transcript))))
        (niwa-view-pair--repair (selected-frame))
        (should (equal (list (get-buffer "*scratch*")) (niwa-view-pair-close--shown)))
        (should (buffer-live-p view-buf))
        (should (buffer-live-p draft))))))

(ert-deftest niwa-view-pair-close/quit-split ()
  "Quitting a pair beside another window leaves no pair window, and the other."
  (niwa-view-pair-close--with-chat
    (let ((other (niwa-view-pair-close--neighbor)))
      (select-window (get-buffer-window view-buf))
      (should (= 0 (niwa-view-pair-close--signals (quit-window))))
      (niwa-view-pair--repair (selected-frame))
      (should (window-live-p other))
      (should (equal (list (get-buffer "*scratch*") (get-buffer "*scratch*"))
                     (niwa-view-pair-close--shown))))))

(ert-deftest niwa-view-pair-close/release-repeats ()
  "Releasing a view again after the first release deletes and signals nothing."
  (niwa-view-pair-close--with-chat
    (let ((transcript (get-buffer-window view-buf)))
      (niwa-view-pair-hide view-buf)
      (should (equal (list transcript) (window-list nil 'nomini)))
      (should (= 0 (niwa-view-pair-close--signals (niwa-view-pair-hide view-buf))))
      (should (equal (list transcript) (window-list nil 'nomini))))))

(provide 'test-niwa-view-pair-close)
;;; test-niwa-view-pair-close.el ends here
