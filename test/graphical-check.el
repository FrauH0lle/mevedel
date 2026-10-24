;;; graphical-check.el --- Isolated graphical view checks -*- lexical-binding: t -*-

;;; Commentary:

;; Entry point for Eask's graphical-check script.  The batch parent owns the
;; deadline and temporary state; a separate emacs -Q owns rendering and ERT.
;; This is a correctness/capture harness, not a presentation-time benchmark.

;;; Code:

(require 'cl-lib)
(require 'json)

(defconst mevedel-graphical-check--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Checkout whose sources and tests the runner uses.")

(defconst mevedel-graphical-check--files
  '("test-mevedel-view-table-graphical.el" "test-mevedel-view-geometry.el"
    "test-mevedel-view-mouse.el")
  "Default real-display ERT suites.")

(defun mevedel-graphical-check--options (arguments)
  "Parse script ARGUMENTS into output, grammar paths and deadline."
  (let ((output (file-name-concat mevedel-graphical-check--root ".scratch"
                                 "graphical-check"
                                 (format-time-string "%Y%m%d-%H%M%S-%N")))
        (timeout 120)
        grammars)
    (when (equal (car arguments) "--") (pop arguments))
    (while arguments
      (let ((option (pop arguments)))
        (unless (and arguments (member option '("--output" "--grammar-dir" "--timeout")))
          (error "Usage: graphical-check [--output NEW-DIR] [--grammar-dir DIR] [--timeout SECONDS]"))
        (let ((value (pop arguments)))
          (pcase option
            ("--output" (setq output (expand-file-name value)))
            ("--grammar-dir"
             (unless (file-directory-p value)
               (error "Grammar directory does not exist: %s" value))
             (push (expand-file-name value) grammars))
            ("--timeout"
             (unless (string-match-p "\\`[1-9][0-9]*\\'" value)
               (error "Timeout must be a positive integer: %s" value))
             (setq timeout (string-to-number value)))))))
    (list output (nreverse grammars) timeout)))

(defun mevedel-graphical-check--environment (home)
  "Return a private process environment rooted at HOME.
Resolve a relative Wayland socket before replacing XDG_RUNTIME_DIR."
  (let ((process-environment (copy-sequence process-environment))
        (wayland (getenv "WAYLAND_DISPLAY"))
        (runtime (getenv "XDG_RUNTIME_DIR")))
    (unless (or (getenv "DISPLAY") wayland)
      (error "No graphical display: set WAYLAND_DISPLAY or DISPLAY; use an active desktop or isolated compositor"))
    (when (and wayland (not (file-name-absolute-p wayland)))
      (unless runtime (error "Relative WAYLAND_DISPLAY needs XDG_RUNTIME_DIR"))
      (setq wayland (file-name-concat runtime wayland)))
    (when wayland
      (unless (file-exists-p wayland)
        (error "Wayland socket is unavailable: %s; unlock/start the display or use an isolated compositor" wayland))
      (setenv "WAYLAND_DISPLAY" wayland))
    (when (and (getenv "DISPLAY") (not (getenv "XAUTHORITY")))
      (setenv "XAUTHORITY" (expand-file-name "~/.Xauthority")))
    (setenv "HOME" home)
    (dolist (name '("XDG_CACHE_HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME"
                    "XDG_STATE_HOME" "XDG_RUNTIME_DIR"))
      (let ((directory (file-name-concat home name)))
        (make-directory directory t)
        (set-file-modes directory #o700)
        (setenv name directory)))
    ;; Neither application integration nor compilation should reach the host.
    (setenv "DBUS_SESSION_BUS_ADDRESS" nil)
    process-environment))

(defun mevedel-graphical-check--wait (process timeout)
  "Wait for PROCESS for at most TIMEOUT seconds, then terminate it."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.1))
    (when (process-live-p process)
      (delete-process process)
      (error "Graphical Emacs timed out after %ss; display may be locked, disconnected or not presenting" timeout))
    (unless (and (eq (process-status process) 'exit)
                 (= (process-exit-status process) 0))
      (error "Graphical Emacs failed (%s); see ert.log and graphical.log for display, export or grammar diagnostics"
             (process-exit-status process)))))

(defun mevedel-graphical-check-run ()
  "Run graphical fixtures and ERT in a disposable Emacs, never this editor."
  (unless noninteractive (error "Use the Eask graphical-check script, not the live editor"))
  (pcase-let* ((`(,output ,grammars ,timeout)
                (mevedel-graphical-check--options command-line-args-left))
               (temporary (make-temp-file "mevedel-graphical-" t))
               (home (file-name-concat temporary "home"))
               (bootstrap (file-name-concat temporary "bootstrap.el"))
               (log (generate-new-buffer " *graphical-check-log*"))
               (process nil)
               (created nil)
               (status 0))
    (setq command-line-args-left nil)
    (unwind-protect
        (condition-case err
            (progn
              (when (file-exists-p output) (error "Output directory already exists: %s" output))
              (make-directory output t)
              (setq created t)
              (make-directory home t)
              (let ((process-environment (mevedel-graphical-check--environment home))
                    (default-directory mevedel-graphical-check--root))
                (with-temp-file bootstrap
                  (prin1 `(condition-case err
                              (progn
                                (setq load-path ',load-path
                                      treesit-extra-load-path ',grammars
                                      native-comp-jit-compilation nil
                                      treesit-auto-install-grammar 'never)
                                (startup-redirect-eln-cache ,(file-name-concat home "eln"))
                                (load ,(file-name-concat mevedel-graphical-check--root
                                                         "test" "graphical-check.el") nil t)
                                (mevedel-graphical-check--child ,output))
                            (error
                             (princ (format "Graphical bootstrap error: %s\n"
                                            (error-message-string err))
                                    'external-debugging-output)
                             (kill-emacs 1)))
                         (current-buffer)))
                (princ (format "Graphical check: %s (deadline %ss)\n" output timeout))
                (setq process
                      (make-process
                       :name "mevedel-graphical-check" :buffer log :stderr log
                       :noquery t :connection-type 'pipe
                       :command (list (expand-file-name invocation-name invocation-directory)
                                      "-Q" "--no-splash" "--load" bootstrap)))
                (mevedel-graphical-check--wait process timeout))
              (princ (with-current-buffer log (buffer-string)))
              (princ (format "Graphical check passed; exports and ERT report: %s\n" output)))
          (error
           (princ (with-current-buffer log (buffer-string)))
           (when (and created (file-exists-p (file-name-concat output "ert.log")))
             (with-temp-buffer
               (insert-file-contents (file-name-concat output "ert.log"))
               (princ (buffer-string))))
           (princ (format "Graphical check FAILED: %s\nArtifacts: %s\n"
                          (error-message-string err) output))
           (setq status 1)))
      (when (and process (process-live-p process)) (delete-process process))
      (unwind-protect
          (when created
            (with-current-buffer log
              (write-region (point-min) (point-max)
                            (file-name-concat output "graphical.log") nil 'silent)))
        (kill-buffer log)
        (delete-directory temporary t)))
    (unless (zerop status) (kill-emacs status))))

(defun mevedel-graphical-check--preflight ()
  "Reject a non-graphical frame, missing export support or Markdown grammars."
  (unless (and (display-graphic-p) (not noninteractive))
    (error "A real graphical Emacs is required, not batch/terminal Emacs"))
  (unless (and (frame-visible-p (selected-frame)) (fboundp 'x-export-frames))
    (error "A visible frame with Cairo x-export-frames support is required"))
  (require 'treesit)
  (dolist (language '(markdown markdown-inline))
    (unless (treesit-language-available-p language)
      (error "Missing tree-sitter grammar %s: install both markdown and markdown-inline and pass --grammar-dir DIR (search: %S)"
             language treesit-extra-load-path))))

(defun mevedel-graphical-check--capture (output)
  "Render a real transcript and save fontified frame exports under OUTPUT."
  (require 'helpers)
  (require 'mevedel-execution)
  (require 'mevedel-pending-inputs)
  (require 'mevedel-view)
  (require 'mevedel-view-agent)
  (require 'mevedel-view-render)
  (unless (file-equal-p (symbol-file 'mevedel-view--full-rerender 'defun)
                        (file-name-concat mevedel-graphical-check--root
                                          "mevedel-view-render.el"))
    (error "Graphical fixture loaded view code outside this checkout"))
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Review the graphical fixture.\n\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     "# Graphical view fixture\n\nA **bold response**, *emphasis*, and `inline code`.\n\n| Check | Result |\n|---|---|\n| Markdown | Fontified |\n| Frame | Exported |\n\n"
     'response)
    (switch-to-buffer view-buf)
    ;; Font Lock refuses internal buffers whose names begin with a space.
    (rename-buffer "*mevedel graphical fixture*" t)
    (delete-other-windows)
    (let* ((inhibit-read-only t)
           (session (mevedel-session-create
                     "graphical"
                     (mevedel-workspace--create
                      :type 'file :id "graphical" :name "graphical"
                      :root default-directory)))
           bold-position)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setq-local mevedel--session session)
      (mevedel-view-test--insert-composer-draft "> active draft\nsecond line")
      (let* ((clock (float-time))
             (original-float-time (symbol-function 'float-time))
             (executions
              (list (list :execution-id "graphical-bash" :owner "/root" :yielded t
                          :state 'running :description "Commit, run check, fresh, remote"
                          :started-at (- clock 847))
                    (list :execution-id "graphical-monitor" :owner "/root" :yielded t
                          :state 'running :kind 'monitor :description "Watch build events"
                          :event-count 3 :started-at (- clock 65)))))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time)
                     (if time (funcall original-float-time time) clock)))
                  ((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                  ((symbol-function 'mevedel-execution-count-user) (lambda (_) 2))
                  ((symbol-function 'mevedel-execution-list-user) (lambda (_) executions))
                  ((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :path "/root/reviewer" :status 'running
                                 :model-metadata '(:model "opus" :effort "medium")
                                 :started-at (- clock 130))))))
          (mevedel-view--full-rerender)
          ;; Reproduce the failure-review queue with the long file mention that
          ;; used to appear in both the expanded header and the detail body.
          (mevedel-session-enqueue-pending-input
           session 'steering
           '(:input "@file:/home/roland/Bilder/Bildschirmfotos/Bildschirmfoto_20261024_160228.png Can we add an elapsed time beside the background jobs?"))
          (mevedel-session-set-pending-input-failure-paused session t)
          (mevedel-view--interaction-rebuild)
          (goto-char (point-min))
          (search-forward "1. [+]")
          (backward-char)
          (let ((last-nonmenu-event nil))
            (call-interactively (key-binding (kbd "RET"))))
          (mevedel-view--interaction-rebuild)
          (unless (and (string-search "Steering\n  1. [-]\n    @file:" (buffer-string))
                       (= 1 (mevedel-view-test--count-substring "@file:" (buffer-string))))
            (error "Failure-paused steering fixture duplicates or omits expanded input"))
          (redisplay t)
          ;; Exercise the shared semantic tick, not another full/status render.
          (setq clock (1+ clock))
          (mevedel-view--spinner-tick)
          (dolist (label '("remote · 14m 08s" "events · 1m 06s" "reviewer · 2m 11s"))
            (unless (string-search label (buffer-string))
              (error "Background elapsed fixture did not render %s" label)))))
      ;; Views defer table layout to idle.  Drain that production operation
      ;; explicitly: this bootstrap exits before the ordinary idle loop.
      (redisplay t)
      (mevedel-view-table-rerender (selected-window))
      ;; -Q has no global font-lock.  Enable it explicitly so the production
      ;; font-lock-face properties are consumed, rather than copying faces.
      (setq-local font-lock-defaults '(nil t))
      (font-lock-mode 1)
      (font-lock-ensure)
      (unless (and font-lock-mode
                   (text-property-not-all (point-min) (point-max) 'font-lock-face nil))
        (error "Fixture has no active font-lock faces (mode %S, face position %S, chars %d)"
               font-lock-mode
               (text-property-not-all (point-min) (point-max) 'font-lock-face nil)
               (buffer-size)))
      (goto-char (point-min))
      (search-forward "bold response")
      (setq bold-position (match-beginning 0))
      (unless (text-property-not-all (point-min) (point-max) 'mevedel-view-table-cell nil)
        (error "Fixture table was not laid out"))
      (unless (equal "> active draft\nsecond line" (mevedel-view--input-text))
        (error "Fixture redraw changed the composer"))
      (goto-char (point-min))
      (set-window-start nil (point-min))
      (message nil)
      (redisplay t)
      (let ((face (get-text-property bold-position 'font-lock-face)))
        (unless (or (eq face 'markdown-ts-bold)
                    (and (listp face) (memq 'markdown-ts-bold face)))
          (error "Markdown bold face missing after redisplay: %S" face)))
      (unless (posn-at-point bold-position)
        (error "Fixture is not laid out on the graphical frame"))
      (write-region (point-min) (point-max)
                    (file-name-concat output "view.txt") nil 'silent)
      (dolist (type '(png pdf))
        (let ((coding-system-for-write 'no-conversion))
          (with-temp-file (file-name-concat output (format "view.%s" type))
            (set-buffer-multibyte nil)
            (insert (x-export-frames nil type))))))))

(defun mevedel-graphical-check--child (output)
  "Run the graphical checks, save diagnostics in OUTPUT, and always exit."
  (let ((status 1))
    (condition-case err
        (progn
          (mevedel-graphical-check--preflight)
          (set-frame-size nil 110 42)
          (sit-for 0.2)
          (mevedel-graphical-check--capture output)
          (require 'ert)
          (dolist (file mevedel-graphical-check--files)
            (load (file-name-concat mevedel-graphical-check--root "test" file) nil t))
          (let* ((stats (ert-run-tests-batch t))
                 (skipped (ert-stats-skipped stats))
                 (unexpected (ert-stats-completed-unexpected stats)))
            (with-temp-file (file-name-concat output "summary.json")
              (insert (json-encode
                       `((emacs . ,emacs-version)
                         (window-system . ,(symbol-name window-system))
                         (font . ,(frame-parameter nil 'font))
                         (checkout . ,mevedel-graphical-check--root)
                         (view-library . ,(symbol-file 'mevedel-view--full-rerender 'defun))
                         (grammar-paths . ,(vconcat treesit-extra-load-path))
                         (markdown-grammars . ["markdown" "markdown-inline"])
                         (tests . ,(ert-stats-total stats))
                         (unexpected . ,unexpected) (skipped . ,skipped)))))
            ;; Missing fonts or image support must not turn into a green skip.
            (setq status (if (and (> (ert-stats-total stats) 0)
                                  (zerop unexpected) (zerop skipped)) 0 1))))
      (error (message "Graphical check error: %s" (error-message-string err))))
    (when (get-buffer "*Messages*")
      (with-current-buffer "*Messages*"
        (write-region (point-min) (point-max)
                      (file-name-concat output "ert.log") nil 'silent)))
    (kill-emacs status)))

(provide 'graphical-check)
;;; graphical-check.el ends here
