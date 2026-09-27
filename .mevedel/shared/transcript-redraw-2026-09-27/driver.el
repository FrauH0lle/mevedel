;;; driver.el --- Isolated settled transcript replay -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'json)
(require 'gptel-openai)
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-transcript-restore)
(require 'mevedel-tools)

(setq inhibit-startup-screen t initial-scratch-message nil
      gc-cons-threshold (* 32 1024 1024) gc-cons-percentage 0.1
      treesit-extra-load-path (list (getenv "PROBE_GRAMMARS")))
(defvar probe-dir (getenv "PROBE_DIRECTORY"))
(defvar probe-mode (getenv "PROBE_MODE"))
(defvar probe-agent (equal (getenv "PROBE_LABEL") "agent"))
(defvar probe-profile (getenv "PROBE_PROFILE"))
(defvar probe-data nil)
(defvar probe-view nil)
(defvar probe-input nil)
(defvar probe-source-hash nil)
(defvar probe-keys nil)
(defvar probe-start nil)
(defvar probe-finish nil)
(defvar probe-initial-return nil)
(defvar probe-gcs nil)
(defvar probe-gc-time nil)
(defvar probe-gc-events nil)
(defvar probe-phases nil)
(defvar probe-restore-ms nil)
(defvar probe-errors nil)

(defun probe-write (name value)
  (with-temp-file (expand-file-name name probe-dir)
    (insert (json-encode value))))
(defun probe-fail (err)
  (probe-write "result.json" (list :error (error-message-string err)))
  (kill-emacs 1))
(defun probe-guard (fn &rest args)
  (condition-case err (apply fn args) (error (probe-fail err))))
(defun probe-key ()
  (interactive)
  (push (float-time) probe-keys)
  (insert "x"))
(defun probe-history ()
  (with-current-buffer probe-view
    (buffer-substring-no-properties (point-min) (mevedel-view--history-insertion-marker))))
(defun probe-source-map ()
  "Return comparable source mappings, validating live marker ownership."
  (with-current-buffer probe-view
    (let ((pos (point-min)) (end (mevedel-view--history-insertion-marker)) out)
      (while (< pos end)
        (let* ((source (get-text-property pos 'mevedel-view-source))
               (next (next-single-property-change pos 'mevedel-view-source nil end)))
          (when (and (consp source) (integer-or-marker-p (car source))
                     (integer-or-marker-p (cdr source)))
            (dolist (coordinate (list (car source) (cdr source)))
              (when (and (markerp coordinate) (not (eq (marker-buffer coordinate) probe-data)))
                (error "Wrong source marker owner")))
            (unless (<= 1 (car source) (cdr source) (with-current-buffer probe-data (point-max)))
              (error "Source mapping out of bounds"))
            (push (list pos next (+ 0 (car source)) (+ 0 (cdr source))) out))
          (setq pos next)))
      (nreverse out))))
(defun probe-file-hash (path)
  (when path
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (secure-hash 'sha256 (current-buffer)))))
(defun probe-done-p ()
  (with-current-buffer probe-view
    (not (or mevedel-view--pending-render-kind mevedel-view-render--batch
             mevedel-view-prepare--jobs mevedel-view-prepare--timer
             mevedel-view-render--owner mevedel-view-render--pending))))
(defun probe-check ()
  (if (probe-done-p)
      (progn
        (setq probe-finish (float-time)
              probe-gcs (- gcs-done probe-gcs)
              probe-gc-time (- gc-elapsed probe-gc-time))
        (probe-write "finished" t))
    (run-at-time .002 nil #'probe-guard #'probe-check)))
(defun probe-phase (name fn &rest args)
  (if (or (not probe-start) probe-finish)
      (apply fn args)
    (let ((start (float-time)) (gc gc-elapsed))
      (unwind-protect (apply fn args)
        (push (list :name (symbol-name name) :start start
                    :ms (* 1000 (- (float-time) start))
                    :gc-ms (* 1000 (- gc-elapsed gc))) probe-phases)))))
(defun probe-gc ()
  (when (and probe-start (not probe-finish))
    (push (list :at (float-time) :cumulative gc-elapsed) probe-gc-events)))
(defun probe-start-work ()
  (setq probe-start (float-time) probe-gcs gcs-done probe-gc-time gc-elapsed)
  (with-current-buffer probe-view
    (if (equal probe-mode "cold")
        (mevedel-view--full-rerender)
      (mevedel-view-rerender probe-view)))
  (setq probe-initial-return (float-time))
  (probe-check))
(defun probe-end-input ()
  (interactive)
  (probe-guard #'probe-finalize))
(defun probe-finalize ()
  (unless (and probe-finish (probe-done-p)) (error "Work did not settle"))
  (when probe-errors (error "Replay diagnostics: %S" probe-errors))
  (with-current-buffer probe-view
    (when (or (text-property-any (point-min) (point-max) 'mevedel-view-type 'history-pending)
              (text-property-any (point-min) (point-max) 'mevedel-view-type 'tool-preparing))
      (error "Unfinished placeholder in settled view")))
  (let* ((draft (with-current-buffer probe-input
                  (if probe-agent (buffer-string) (mevedel-view--input-text))))
         (point-at-end (with-current-buffer probe-input (= (point) (point-max))))
         (history (probe-history))
         (source-map (probe-source-map))
         (source-unchanged (with-current-buffer probe-data
                             (equal probe-source-hash (secure-hash 'sha256 (current-buffer)))))
         (segments (with-current-buffer probe-data
                     (mevedel-transcript-segments (point-min) (point-max))))
         (turns (mevedel-view--group-transcript-turns segments probe-data)))
    (with-current-buffer probe-view (mevedel-view--full-rerender))
    (probe-write
     "result.json"
     (list :label (getenv "PROBE_LABEL") :mode probe-mode :profile (and probe-profile t)
           :started probe-start :finished probe-finish
           :initial-return-ms (* 1000 (- probe-initial-return probe-start))
           :restore-ms probe-restore-ms :gcs probe-gcs :gc-seconds probe-gc-time
           :gc-events (vconcat (reverse probe-gc-events))
           :phases (vconcat (reverse probe-phases))
           :keys (vconcat (reverse probe-keys)) :draft draft :point_at_end point-at-end
           :history_equal (equal history (probe-history)) :source_unchanged source-unchanged
           :source_map_equal (equal source-map (probe-source-map))
           :source_map_sha256 (secure-hash 'sha256 (prin1-to-string source-map))
           :fixture_sha256 (probe-file-hash (getenv "PROBE_CAPTURE"))
           :history_chars (length history) :history_sha256 (secure-hash 'sha256 history)
           :segments (length segments) :turns (length turns)
           :largest_turn_chars (apply #'max 0 (mapcar (lambda (turn) (- (plist-get turn :end) (plist-get turn :start))) turns))
           :diagnostics (vconcat (reverse probe-errors))
           :emacs emacs-version :graphic (display-graphic-p)
           :gc-threshold gc-cons-threshold :gc-percentage gc-cons-percentage
           :markdown (mevedel-view--markdown-grammars-ready-p)
           :libraries (vconcat
                       (mapcar (lambda (symbol)
                                 (let ((path (symbol-file symbol)))
                                   (list :symbol (symbol-name symbol) :path path
                                         :sha256 (probe-file-hash path))))
                               '(gptel-request mevedel-view--full-rerender
                                 mevedel-transcript-segments mevedel-view-prepare-get))))))
  (kill-emacs 0))
(defun probe-init ()
  ;; No real session, publication lifecycle, user init, or network activity.
  (advice-add 'make-network-process :override (lambda (&rest _) (error "Network forbidden in replay")))
  (advice-add 'display-warning :before
              (lambda (&rest args) (push (format "%S" args) probe-errors)))
  (advice-add 'message :before
              (lambda (format-string &rest args)
                (when (and (stringp format-string)
                           (string-match-p "[Ff]ailed\\|[Ee]rror\\|[Ww]arning" format-string))
                  (push (apply #'format format-string args) probe-errors))))
  (mevedel-tools-register)
  (setq probe-data (generate-new-buffer " *redraw-source*"))
  (with-current-buffer probe-data
    (setq default-directory (file-name-as-directory (getenv "HOME")))
    (org-mode)
    (insert-file-contents (getenv "PROBE_CAPTURE"))
    (let ((start (float-time)))
      (mevedel-transcript-restore-properties)
      (setq probe-restore-ms (* 1000 (- (float-time) start))))
    (setq probe-source-hash (secure-hash 'sha256 (current-buffer))))
  (setq probe-view (generate-new-buffer "*redraw-view*"))
  (mevedel-view--setup probe-view probe-data
                       (if probe-agent
                           '(:agent-transcript-p t :side-conversation-p t
                             :agent-path "/root/replay")
                         '(:side-conversation-p t)))
  (switch-to-buffer probe-view)
  (unless (member probe-mode '("cold" "cold-scheduled"))
    (mevedel-view--full-rerender))
  (if probe-agent
      (progn
        (setq buffer-read-only t)
        (setq probe-input (get-buffer-create "*redraw-input*"))
        (select-window (split-window-below -8))
        (switch-to-buffer probe-input)
        (text-mode))
    (setq probe-input probe-view))
  (goto-char (point-max))
  (insert "> draft\nsecond line\n")
  (local-set-key "x" #'probe-key)
  (local-set-key "z" #'probe-end-input)
  (when probe-profile
    (dolist (name '(mevedel-transcript-segments mevedel-view--group-transcript-turns
                    mevedel-view--full-rerender-plan mevedel-view--render-turn
                    mevedel-view-render--start-batch mevedel-view-render--batch-step
                    mevedel-view--prepare-tool-segment mevedel-view--tool-call-parse
                    mevedel-view-disclosure-restore-state
                    mevedel-view--fontify-as))
      (when (fboundp name) (advice-add name :around (apply-partially #'probe-phase name))))
    (add-hook 'post-gc-hook #'probe-gc))
  (garbage-collect)
  (redisplay t)
  (probe-write "ready" t)
  (run-at-time .25 nil #'probe-guard #'probe-start-work))
(run-at-time .1 nil #'probe-guard #'probe-init)
