;;; request.el --- Real request renderer experiment -*- lexical-binding: t -*-
(setq native-comp-jit-compilation nil)
(require 'package)
(setq package-user-dir (getenv "MEVEDEL_LAB_ELPA"))
(package-initialize)
(require 'mevedel)
(require 'mevedel-view)
(require 'mevedel-view-stream)
(require 'mevedel-plugin-registry)
(require 'mevedel-permission-mode)
(require 'mevedel-view-native)
(require 'server)
(add-to-list 'display-buffer-alist '("\\*Warnings\\*" (display-buffer-no-window) (allow-no-window . t)))
(setq inhibit-startup-screen t
      server-name (getenv "MEVEDEL_LAB_SERVER")
      server-socket-dir (getenv "MEVEDEL_LAB_HOME")
      frame-title-format "mevedel request animation experiment"
      mevedel-user-dir (file-name-concat (getenv "MEVEDEL_LAB_HOME") "mevedel")
      mevedel-plugin-extra-roots nil
      mevedel-permission-mode 'full-auto
      mevedel-view-spinner-power-policy 'full
      mevedel-view-spinner-style 'bounce
      mevedel-view-tool-spinner-style 'shimmer
      mevedel-view-spinner-framerate 30
      mevedel-view-native-enabled nil)
(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Aporetic Serif Mono-16" nil t)
(set-frame-size nil 1536 888 t)
(mevedel-install)
(load (file-name-concat (file-name-directory load-file-name) "cpuh-harness.elc") nil t)
(server-start)
(declare-function mevedel-view-native--stats "ext:mevedel-view-native" ())
(declare-function cpuh-session-string "cpuh-harness" (dir preset))
(declare-function cpuh-view "cpuh-harness" ())
(declare-function cpuh-send "cpuh-harness" (text))
(declare-function cpuh-busy-p "cpuh-harness" ())
(declare-function cpuh-start "cpuh-harness" ())
(declare-function cpuh-stop "cpuh-harness" ())
(declare-function cpuh-dump "cpuh-harness" (file label))
(defun request-lab-start (style native)
  "Begin a mock request with STYLE and optional NATIVE presentation."
  (unless (cpuh-view)
    (cpuh-session-string (getenv "MEVEDEL_LAB_WORKSPACE") 'cpu-mock)
    (with-current-buffer (cpuh-view) (mevedel-rename-session "Animation experiment")))
  (switch-to-buffer (cpuh-view))
  (delete-other-windows)
  (mevedel-permission-mode-transition 'full-auto)
  (setq mevedel-view-native-enabled native)
  (customize-set-variable 'mevedel-view-spinner-style style)
  (customize-set-variable 'mevedel-view-tool-spinner-style
                          (if (eq style 'static) 'static 'shimmer))
  (cpuh-send "measure animation")
  t)
(defvar mevedel--coalesced-timers)
(declare-function cpuh--name "cpuh-harness" (fn))
(declare-function mevedel-execution-count-user "mevedel-execution" (session))
(defvar request-lab-focus-losses 0)
(defun request-lab-note-focus ()
  "Count focus loss during the CPU sample, including brief interruptions."
  (unless (frame-focus-state) (cl-incf request-lab-focus-losses)))
(add-function :after after-focus-change-function #'request-lab-note-focus)
(defun request-lab-state ()
  "Return bounded evidence about the active view and native presentation."
  (with-current-buffer (cpuh-view)
    (list :visible (and (get-buffer-window (current-buffer)) t)
          :busy (cpuh-busy-p)
          :executions (mevedel-execution-count-user
                       (buffer-local-value 'mevedel--session mevedel--data-buffer))
          :status mevedel-view--spinner-status
          :plan mevedel-view--spinner-timer-plan
          :native (length mevedel-view--native-animation-targets)
          :stats (when (fboundp 'mevedel-view-native--stats) (mevedel-view-native--stats))
          :load mevedel-view-native--load-state
          :focused (frame-focus-state)
          :focus-losses request-lab-focus-losses
          :coalesced (mapcar (lambda (timer) (cpuh--name (timer--function timer)))
                             (bound-and-true-p mevedel--coalesced-timers))
          :tools (length mevedel-view--spinner-tool-targets)
          :entries (length mevedel-view-native--entries))))
(defun request-lab-trace-invalidation (&rest _)
  "Record bounded invalidation evidence in the disposable experiment."
  (when-let* ((path (getenv "MEVEDEL_LAB_TRACE")))
    (let ((row (list
                :content-changed (not (eql mevedel-view-native--content-tick
                                            (buffer-chars-modified-tick)))
                :targets (mapcar (lambda (entry)
                                   (list (marker-position (caaar entry))
                                         (nth 3 entry)))
                                 mevedel-view-native--entries)
                :stack (cl-loop for depth from 0 below 18
                                for frame = (backtrace-frame depth)
                                when (symbolp (nth 1 frame)) collect (nth 1 frame)))))
      ;; Frame identifiers in placements are opaque objects; retain geometry only.
      (setf (plist-get row :targets)
            (mapcar (lambda (entry) (list (car entry) (cdadr entry)))
                    (plist-get row :targets)))
      (with-temp-buffer
        (insert (format "%S\n" row))
        (write-region (point-min) (point-max) path t 'silent)))))
(defun request-lab-trace-sync (fn specs elapsed rearm)
  "Record only geometry and handle reuse for each semantic synchronization."
  (let* ((old mevedel-view-native--entries)
         (result (funcall fn specs elapsed rearm))
         (new mevedel-view-native--entries)
         (changed (cl-count-if
                   (lambda (entry) (not (cl-find (nth 2 entry) old :key (lambda (e) (nth 2 e)))))
                   new)))
    (when (and (> changed 0) old)
      (let ((row (list :new-handles changed :inhibited inhibit-redisplay
                       :old (mapcar (lambda (entry) (list (seq-take (cadr entry) 4) (cdr (nth 3 entry)))) old)
                       :new (mapcar (lambda (entry) (list (seq-take (cadr entry) 4) (cdr (nth 3 entry)))) new))))
        (with-temp-buffer
          (insert (format "%S\n" row))
          (write-region (point-min) (point-max) (getenv "MEVEDEL_LAB_TRACE") t 'silent))))
    result))
(when (getenv "MEVEDEL_LAB_TRACE")
  (advice-add 'mevedel-view-native--invalidate :before #'request-lab-trace-invalidation)
  (advice-add 'mevedel-view-native-sync :around #'request-lab-trace-sync))
(defvar request-lab-pending-tools nil)
(defun request-lab-tools (count)
  "Drive COUNT pending-tool presentation events in the actual request view.
These exercise concurrent presentation without executing concurrent commands."
  (let ((data (buffer-local-value 'mevedel--data-buffer (cpuh-view))))
    (with-current-buffer data
      (while (> (length request-lab-pending-tools) count)
        (mevedel-view-stream-post-tool (pop request-lab-pending-tools)))
      (while (< (length request-lab-pending-tools) count)
        (let ((info (list :name "Bash" :args
                          (list :command (format "pending demonstration %d"
                                                 (length request-lab-pending-tools))))))
          (push info request-lab-pending-tools)
          (mevedel-view-stream-pre-tool info)))))
  t)
(defvar request-lab-frozen nil)
(defun request-lab-check (step)
  "Exercise STEP through the running request view, returning bounded evidence."
  (with-current-buffer (cpuh-view)
    (pcase step
      ('freeze
       (customize-set-variable 'mevedel-view-spinner-animate nil)
       (redisplay t)
       (setq request-lab-frozen
             (mapcar (lambda (target) (get-text-property (car target) 'display))
                     (cons mevedel-view--spinner-label-target
                           mevedel-view--spinner-tool-targets)))
       (= 0 (aref (mevedel-view-native--stats) 0)))
      ('frozen
       (cl-every #'identity
                 (cl-mapcar #'equal-including-properties request-lab-frozen
                            (mapcar (lambda (target) (get-text-property (car target) 'display))
                                    (cons mevedel-view--spinner-label-target
                                          mevedel-view--spinner-tool-targets)))))
      ('thaw (customize-set-variable 'mevedel-view-spinner-animate t) t)
      ('disable-native (customize-set-variable 'mevedel-view-native-enabled nil) t)
      ('enable-native (customize-set-variable 'mevedel-view-native-enabled t) t)
      ('tool-braille (customize-set-variable 'mevedel-view-tool-spinner-style 'braille) t)
      ('tool-ascii (customize-set-variable 'mevedel-view-tool-spinner-style 'ascii) t)
      ('tool-dots (customize-set-variable 'mevedel-view-tool-spinner-style 'dots) t)
      ('tool-shimmer (customize-set-variable 'mevedel-view-tool-spinner-style 'shimmer) t)
      ('dark (load-theme 'wombat t) t)
      ('light (disable-theme 'wombat) t)
      ('larger (text-scale-set 1) t)
      ('split (split-window-right) t)
      ('hscroll (setq truncate-lines t) (set-window-hscroll (selected-window) 4) t)
      ('unscroll (set-window-hscroll (selected-window) 0) t)
      ('unsplit (delete-other-windows) t)
      ('typing
       (goto-char (point-max))
       (run-hooks 'pre-command-hook)
       (> (aref (mevedel-view-native--stats) 0) 0))
      ('draft
       (goto-char (point-max))
       (insert "> retained draft\nsecond line")
       t)
      ('draft-retained
       (string-suffix-p "> retained draft\nsecond line" (buffer-string)))
      (_ (error "Unknown check: %s" step)))))
(defvar request-lab-observations nil)
(defvar request-lab-counts nil)
(defun request-lab--counter (name)
  "Return advice counting calls under NAME."
  (lambda (&rest _) (cl-incf (alist-get name request-lab-counts 0))))
(defun request-lab-observe (seconds path)
  "Sample native/Lisp ownership every 0.1 s for SECONDS; write PATH.
Counts surface opens, closes and invalidations meanwhile.  The sampler is
itself a 10 Hz wakeup, so CPU is measured in a separate pass."
  (setq request-lab-observations nil request-lab-counts nil)
  (dolist (name '(mevedel-view-native--open mevedel-view-native--close
                  mevedel-view-native--invalidate mevedel-view-native-sync))
    (advice-add name :before (request-lab--counter name) '((name . request-lab))))
  (let* ((end (+ (float-time) seconds))
         (timer nil))
    (setq timer
          (run-at-time
           0 0.1
           (lambda ()
             (with-current-buffer (cpuh-view)
               (let ((target mevedel-view--spinner-label-target))
                 (push (list :native (length mevedel-view--native-animation-targets)
                             :entries (length mevedel-view-native--entries)
                             :settling mevedel-view-native--settling
                             :plan (mapcar #'car mevedel-view--spinner-timer-plan)
                             :label-visible
                             (and target (marker-position (car target))
                                  (mevedel-view--animation-target-visible-p
                                   target 'mevedel-view-spinner-frame)
                                  t)
                             :point-in-label
                             (and target (marker-position (car target))
                                  (<= (car target) (window-point) (cdr target)))
                             ;; Where the label sits against the stored range.
                             :geometry
                             (and (getenv "MEVEDEL_LAB_GEOMETRY") target
                                  (marker-position (car target))
                                  (let ((window (get-buffer-window)))
                                    (list (- (cdr target) (car target))
                                          (get-text-property (car target) 'mevedel-view-spinner-frame)
                                          (buffer-substring-no-properties
                                           (car target) (min (point-max) (+ (car target) 12)))
                                          (- (window-end window) (car target))
                                          (- (point-max) (window-point window))
                                          (and (pos-visible-in-window-p
                                                (car target) window)
                                               t)))))
                       request-lab-observations)))
             (when (> (float-time) end)
               (cancel-timer timer)
               (dolist (name '(mevedel-view-native--open mevedel-view-native--close
                               mevedel-view-native--invalidate mevedel-view-native-sync))
                 (advice-remove name 'request-lab))
               (with-temp-file path
                 (insert (format "%S\n" (list :counts request-lab-counts
                                              :samples (length request-lab-observations))))
                 (let (seen)
                   (dolist (row request-lab-observations)
                     (cl-incf (alist-get row seen 0 nil #'equal)))
                   (dolist (row seen) (insert (format "%S\n" row))))))))))
  t)
(when-let* ((diagnostic (getenv "MEVEDEL_LAB_DIAGNOSTIC")))
  (load diagnostic nil t))
(provide 'request-lab)
