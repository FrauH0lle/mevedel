;;; cpuh-harness.el --- Measure mevedel wakeups in a live Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Load into the Emacs under test (emacsclient --eval '(load ".../cpuh-harness.el")').
;; Every function returns a small value: emacsclient prints return values
;; without any bound, and printing a mevedel struct once pinned the user's
;; Emacs at 100% CPU and 2.6 GB.  Keep it that way.
;;
;; - `cpuh-start' / `cpuh-dump' / `cpuh-stop': count timer callbacks by name
;;   and redisplays between two points.
;; - `cpuh-session-string' / `cpuh-send' / `cpuh-busy-p': drive a throwaway
;;   session in DIR with the `cpu-mock' preset (see mock_server.py).
;; - `cpuh-attend': bypass the frame-focus gate so animation runs while the
;;   tester's terminal has focus.  The frame must still be on screen.
;; - `cpuh-set' / `cpuh-restore': switch animation settings and restore the
;;   user's values afterwards.
;; - `cpuh-unload': remove everything this file installed.

;;; Code:

(require 'gptel-openai)

(defvar cpuh--timers (make-hash-table :test 'equal))
(defvar cpuh--redisplays 0)
(defvar cpuh--data nil "Data buffer of the measured session.")
(defvar cpuh--window-configuration nil)

(defvar cpuh--saved
  (mapcar (lambda (s) (cons s (default-value s)))
          '(mevedel-view-spinner-style mevedel-view-tool-spinner-style
            mevedel-view-spinner-framerate mevedel-view-spinner-battery-framerate
            mevedel-view-spinner-power-policy mevedel-view-spinner-animate
            mevedel-telemetry-enabled))
  "User settings before the first `cpuh-set'.")

(gptel-make-openai "CPU-Mock"
  :host "127.0.0.1:8766" :protocol "http" :endpoint "/v1/chat/completions"
  :stream t :key "x"
  :models '((mock-model :capabilities (tool-use) :context-window 128)))
(mevedel-define-preset cpu-mock
  :parents (mevedel-implement) :backend "CPU-Mock" :model 'mock-model)

(defun cpuh--name (fn)
  "Return a short name for timer function FN."
  (cond ((symbolp fn) (symbol-name fn))
        ((fboundp 'mevedel-telemetry--lag-closure-callee)
         (let ((callee (ignore-errors (mevedel-telemetry--lag-closure-callee fn))))
           (if callee (concat "closure:" (symbol-name callee)) "anonymous")))
        (t "anonymous")))

(defun cpuh--count-timer (timer &rest _)
  (let ((name (cpuh--name (timer--function timer))))
    (puthash name (1+ (gethash name cpuh--timers 0)) cpuh--timers)))

(defun cpuh--count-redisplay (&rest _)
  (setq cpuh--redisplays (1+ cpuh--redisplays)))

(defun cpuh-start ()
  "Reset and start counting timer callbacks and redisplays."
  (clrhash cpuh--timers)
  (setq cpuh--redisplays 0)
  (advice-add 'timer-event-handler :before #'cpuh--count-timer)
  (add-hook 'pre-redisplay-functions #'cpuh--count-redisplay)
  t)

(defun cpuh-stop ()
  "Stop counting."
  (advice-remove 'timer-event-handler #'cpuh--count-timer)
  (remove-hook 'pre-redisplay-functions #'cpuh--count-redisplay)
  t)

(defun cpuh-dump (file label)
  "Append counts since `cpuh-start' to FILE under LABEL."
  (let (rows)
    (maphash (lambda (k v) (push (cons v k) rows)) cpuh--timers)
    (setq rows (sort rows (lambda (a b) (> (car a) (car b)))))
    (with-temp-buffer
      (insert (format "== %s  pre-redisplay-calls=%d\n" label cpuh--redisplays))
      (dolist (row (seq-take rows 25))
        (insert (format "%7d %s\n" (car row) (cdr row))))
      (write-region (point-min) (point-max) file t 'silent)))
  t)

(defun cpuh-session-string (dir preset)
  "Open a new session in git repository DIR with PRESET.
Return \"DATA|VIEW\" buffer names.  The data buffer is renamed after the
first prompt, so later calls find it through `cpuh--data'."
  (setq cpuh--window-configuration (current-window-configuration))
  (with-temp-buffer
    (setq default-directory (file-name-as-directory dir))
    (mevedel))
  (let ((data (seq-find
               (lambda (b)
                 (with-current-buffer b
                   (and (bound-and-true-p mevedel--session)
                        (not (bound-and-true-p mevedel--data-buffer))
                        (string-prefix-p (file-name-as-directory dir)
                                         (expand-file-name default-directory)))))
               (buffer-list))))
    (when preset (mevedel-preset-apply preset data))
    (setq cpuh--data data)
    (concat (buffer-name data) "|" (buffer-name (cpuh-view)))))

(defun cpuh-view ()
  "Return the view of the measured session."
  (seq-find (lambda (b)
              (with-current-buffer b
                (and (bound-and-true-p mevedel--data-buffer)
                     (eq mevedel--data-buffer cpuh--data))))
            (buffer-list)))

(defun cpuh-send (text)
  "Send TEXT from the measured session's composer."
  (with-current-buffer (cpuh-view)
    (goto-char (point-max))
    (insert text)
    (mevedel-view-send))
  t)

(defun cpuh-busy-p ()
  "Return t while the measured session has work in flight."
  (with-current-buffer cpuh--data
    (and (or (mevedel-turn-busy-p (current-buffer))
             (bound-and-true-p mevedel--current-request))
         t)))

(defun cpuh--attended (window)
  (eq (frame-visible-p (window-frame window)) t))

(defun cpuh-attend (on)
  "Bypass the frame-focus animation gate when ON."
  (if on
      (advice-add 'mevedel-view--animation-window-attended-p :override #'cpuh--attended)
    (advice-remove 'mevedel-view--animation-window-attended-p #'cpuh--attended))
  (and on t))

(defun cpuh-set (style tool-style fps telemetry)
  "Use STYLE, TOOL-STYLE, FPS ceiling under the `full' policy, and TELEMETRY."
  (customize-set-variable 'mevedel-view-spinner-power-policy 'full)
  (customize-set-variable 'mevedel-view-spinner-framerate fps)
  (customize-set-variable 'mevedel-view-spinner-style style)
  (customize-set-variable 'mevedel-view-tool-spinner-style tool-style)
  (setq mevedel-telemetry-enabled telemetry)
  t)

(defun cpuh-restore ()
  "Restore the settings saved when this file was loaded."
  (dolist (entry cpuh--saved)
    (if (eq (car entry) 'mevedel-telemetry-enabled)
        (setq mevedel-telemetry-enabled (cdr entry))
      (customize-set-variable (car entry) (cdr entry))))
  t)

(defun cpuh-unload ()
  "Kill the measured session and remove everything this file installed."
  (cpuh-stop)
  (cpuh-attend nil)
  (cpuh-restore)
  (when (and cpuh--data (buffer-live-p cpuh--data))
    (let ((kill-buffer-query-functions nil)
          (view (cpuh-view)))
      (when view (kill-buffer view))
      (when (buffer-live-p cpuh--data) (kill-buffer cpuh--data))))
  (when (window-configuration-p cpuh--window-configuration)
    (set-window-configuration cpuh--window-configuration))
  (setq gptel--known-backends (assoc-delete-all "CPU-Mock" gptel--known-backends)
        mevedel-preset--registry (assq-delete-all 'cpu-mock mevedel-preset--registry)
        gptel--known-presets (assq-delete-all 'cpu-mock gptel--known-presets))
  (let ((n 0))
    (mapatoms (lambda (s)
                (when (string-prefix-p "cpuh-" (symbol-name s))
                  (when (fboundp s) (fmakunbound s))
                  (when (boundp s) (makunbound s))
                  (setq n (1+ n)))))
    n))

(provide 'cpuh-harness)
;;; cpuh-harness.el ends here
