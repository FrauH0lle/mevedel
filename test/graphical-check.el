;;; graphical-check.el --- Isolated graphical view checks -*- lexical-binding: t -*-

;;; Commentary:

;; Entry point for Eask's graphical-check script.  The batch parent owns the
;; deadline and temporary state; a separate emacs -Q owns rendering and ERT.
;; This is a correctness/capture harness, not a presentation-time benchmark.

;;; Code:

(require 'cl-lib)
(require 'json)
;; The view fixture's buffer macro; the runner loads it only in the child.
(cl-eval-when (compile) (require 'helpers))

(declare-function niwa-notes-toggle-labels "niwa-notes" ())
(declare-function niwa-tool-render-data-format "niwa-tool-render-data"
                  (render-data &optional tool-use-id))
(declare-function niwa-view--setup "niwa-view" (view-buf data-buf &optional options))
(declare-function niwa-view--full-rerender "niwa-view-render"
                  (&optional transcript-buffer source-changed-p))
(declare-function niwa-view--interaction-rebuild "niwa-view-interaction"
                  (&optional owned visits))
(declare-function niwa-view--spinner-tick "niwa-view-stream" ())
(declare-function niwa-view-table-rerender "niwa-view-table" (&optional window))
(declare-function niwa-view-composer-draft-text "niwa-view-composer" (&optional view without-properties))
(declare-function niwa-tool-exec--register "niwa-tool-exec-registration" ())
(declare-function niwa-view-toggle-section "niwa-view-disclosure" ())

(defconst niwa-graphical-check--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Checkout whose sources and tests the runner uses.")

(defconst niwa-graphical-check--files
  '("test-niwa-view-table-graphical.el" "test-niwa-view-geometry.el"
    "test-niwa-view-mouse.el" "test-niwa-window-graphical.el"
    "test-niwa-view-user-state.el" "test-niwa-view-zone-absent-graphical.el"
    "test-niwa-note-conversation.el" "test-niwa-view-composer-graphical.el"
    "test-niwa-view-pair-graphical.el"
    "test-niwa-view-pair-surfaces-graphical.el")
  "Default real-display ERT suites.")

(defun niwa-graphical-check--options (arguments)
  "Parse script ARGUMENTS into output, grammar paths and deadline."
  (let ((output (file-name-concat niwa-graphical-check--root ".scratch"
                                 "graphical-check"
                                 (format-time-string "%Y%m%d-%H%M%S-%N")))
        (timeout 300)
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

(defun niwa-graphical-check--grammar-paths ()
  "Find installed grammar directories before isolating the child."
  (let* ((home (or (getenv "NIWA_GRAPHICAL_HOST_HOME") (expand-file-name "~")))
         (config (or (getenv "NIWA_GRAPHICAL_HOST_CONFIG") (getenv "XDG_CONFIG_HOME")))
         (legacy (file-name-concat home ".emacs.d"))
         (xdg (file-name-concat (if (and config (file-name-absolute-p config))
                                    config (file-name-concat home ".config"))
                                "emacs")))
    ;; shortcut: fixed paths only; use --grammar-dir for paths set by personal init.
    (delete-dups
     (append (mapcar #'expand-file-name treesit-extra-load-path)
             (cl-remove-if-not
              #'file-directory-p
              (list (file-name-concat user-emacs-directory "tree-sitter")
                    (file-name-concat legacy "tree-sitter")
                    (file-name-concat xdg "tree-sitter")
                    (file-name-concat legacy ".local/etc/tree-sitter")
                    (file-name-concat xdg ".local/etc/tree-sitter")))))))

(defun niwa-graphical-check--environment (home)
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

(defun niwa-graphical-check--wait (process timeout)
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

(defun niwa-graphical-check-run ()
  "Run graphical fixtures and ERT in a disposable Emacs, never this editor."
  (unless noninteractive (error "Use the Eask graphical-check script, not the live editor"))
  (pcase-let* ((`(,output ,grammars ,timeout)
                (niwa-graphical-check--options command-line-args-left))
               (grammars (or grammars (niwa-graphical-check--grammar-paths)))
               (temporary (make-temp-file "niwa-graphical-" t))
               (home (file-name-concat temporary "home"))
               (bootstrap (file-name-concat temporary "bootstrap.el"))
               (log (generate-new-buffer " *graphical-check-log*"))
               (process nil)
               (created nil)
               (status 0))
    (setq command-line-args-left nil)
    ;; SIGTERM/SIGHUP end batch Emacs through `kill-emacs', which runs this
    ;; hook but skips the `unwind-protect' below.  `delete-process' kills the
    ;; child synchronously.  It cannot also be reaped here: the hook runs
    ;; inside the signal handler, where Emacs does not handle SIGCHLD, so the
    ;; dead child remains a zombie until its adopter reaps it.
    (add-hook 'kill-emacs-hook
              (lambda ()
                (when (and process (process-live-p process)) (delete-process process))
                (when (file-exists-p temporary) (delete-directory temporary t))))
    (unwind-protect
        (condition-case err
            (progn
              (when (file-exists-p output) (error "Output directory already exists: %s" output))
              (make-directory output t)
              (setq created t)
              (make-directory home t)
              (let ((process-environment (niwa-graphical-check--environment home))
                    (default-directory niwa-graphical-check--root))
                (with-temp-file bootstrap
                  (insert ";;; Graphical bootstrap -*- lexical-binding: t -*-\n")
                  (prin1 `(condition-case err
                              (progn
                                (setq load-path ',load-path
                                      treesit-extra-load-path ',grammars
                                      native-comp-jit-compilation nil
                                      treesit-auto-install-grammar 'never)
                                (when (native-comp-available-p)
                                  (startup-redirect-eln-cache ,(file-name-concat home "eln")))
                                (load ,(file-name-concat niwa-graphical-check--root
                                                         "test" "graphical-check.el") nil t)
                                (niwa-graphical-check--child ,output))
                            (error
                             (princ (format "Graphical bootstrap error: %s\n"
                                            (error-message-string err))
                                    'external-debugging-output)
                             (kill-emacs 1)))
                         (current-buffer)))
                (princ (format "Graphical check: %s (deadline %ss)\n" output timeout))
                (setq process
                      (make-process
                       :name "niwa-graphical-check" :buffer log :stderr log
                       :noquery t :connection-type 'pipe
                       :command (list (expand-file-name invocation-name invocation-directory)
                                      "-Q" "--no-splash" "--load" bootstrap)))
                (niwa-graphical-check--wait process timeout))
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

(defun niwa-graphical-check--preflight ()
  "Reject a non-graphical frame, missing export support or Markdown grammars."
  (unless (and (display-graphic-p) (not noninteractive))
    (error "A real graphical Emacs is required, not batch/terminal Emacs"))
  (unless (and (frame-visible-p (selected-frame)) (fboundp 'x-export-frames))
    (error "A visible frame with Cairo x-export-frames support is required"))
  (require 'treesit)
  (dolist (language '(markdown markdown-inline))
    (unless (treesit-language-available-p language)
      (error "Missing tree-sitter grammar %s: install both markdown and markdown-inline and pass --grammar-dir DIR (search: %S)"
             language treesit-extra-load-path))
    (princ (format "Tree-sitter grammar %s: %s\n" language
                   (or (treesit-grammar-location language) "library path unavailable"))
           'external-debugging-output)))

(defun niwa-graphical-check--resize-width (width)
  "Set the selected frame to WIDTH columns and wait for the compositor."
  (let ((deadline (+ (float-time) 5))
        (unread-command-events nil))
    (set-frame-width nil width)
    (while (and (/= (frame-width) width) (< (float-time) deadline))
      (setq unread-command-events nil)
      (sit-for 0.02)
      (redisplay t))
    (unless (= (frame-width) width)
      (error "Compositor did not resize the capture frame to %d columns (got %d)"
             width (frame-width)))))

(defun niwa-graphical-check--capture (output)
  "Render a real transcript and save fontified frame exports under OUTPUT."
  (require 'helpers)
  (require 'niwa-execution)
  (require 'niwa-pending-inputs)
  (require 'niwa-tool-render-data)
  (require 'niwa-view)
  (require 'niwa-view-agent)
  (require 'niwa-view-render)
  (unless (file-equal-p (symbol-file 'niwa-view--full-rerender 'defun)
                        (file-name-concat niwa-graphical-check--root
                                          "niwa-view-render.el"))
    (error "Graphical fixture loaded view code outside this checkout"))
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Review the graphical fixture.\n\n" nil)
    (niwa-view-test--insert-data
     data-buf
     "# Graphical view fixture\n\nA **bold response**, *emphasis*, and `inline code`.\n\n| Check | Result |\n|---|---|\n| Markdown | Fontified |\n| Frame | Exported |\n\n"
     'response)
    (niwa-view-test--insert-data
     data-buf
     (niwa-tool-render-data-format
      (list :kind 'request-summary :elapsed-seconds 157
            :started-at (float-time (encode-time 43 29 14 29 10 2026))
            :finished-at (float-time (encode-time 20 32 14 29 10 2026))))
     'ignore)
    (switch-to-buffer view-buf)
    ;; Font Lock refuses internal buffers whose names begin with a space.
    (rename-buffer "*niwa graphical fixture*" t)
    (delete-other-windows)
    (let* ((inhibit-read-only t)
           (session (niwa-session-create
                     "graphical"
                     (niwa-workspace--create
                      :type 'file :id "graphical" :name "graphical"
                      :root default-directory)))
           bold-position)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (setq-local niwa--session session)
      (niwa-view-test--insert-composer-draft "> active draft\nsecond line")
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
                  ((symbol-function 'niwa-execution-count-user) (lambda (_) 2))
                  ((symbol-function 'niwa-execution-list-user) (lambda (_) executions))
                  ((symbol-function 'niwa-view--agent-status-collect)
                   (lambda ()
                     (list (list :path "/root/reviewer" :status 'running
                                 :model-metadata '(:model "opus" :effort "medium")
                                 :started-at (- clock 130))))))
          (niwa-view--full-rerender)
          ;; Reproduce the failure-review queue with the long file mention that
          ;; used to appear in both the expanded header and the detail body.
          (niwa-session-enqueue-pending-input
           session 'steering
           '(:input "@file:/home/roland/Bilder/Bildschirmfotos/Bildschirmfoto_20261024_160228.png Can we add an elapsed time beside the background jobs?"))
          (niwa-session-set-pending-input-failure-paused session t)
          (niwa-view--interaction-rebuild)
          (goto-char (point-min))
          (search-forward "1. [+]")
          (backward-char)
          (let ((last-nonmenu-event nil))
            (call-interactively (key-binding (kbd "RET"))))
          (niwa-view--interaction-rebuild)
          (unless (and (string-search "Steering\n  1. [-]\n    @file:" (buffer-string))
                       (= 1 (niwa-view-test--count-substring "@file:" (buffer-string))))
            (error "Failure-paused steering fixture duplicates or omits expanded input"))
          (redisplay t)
          ;; Exercise the shared semantic tick, not another full/status render.
          (setq clock (1+ clock))
          (niwa-view--spinner-tick)
          (dolist (label '("remote · 14m 08s" "events · 1m 06s" "reviewer · 2m 11s"))
            (unless (string-search label (buffer-string))
              (error "Background elapsed fixture did not render %s" label)))))
      ;; Views defer table layout to idle.  Drain that production operation
      ;; explicitly: this bootstrap exits before the ordinary idle loop.
      (redisplay t)
      (niwa-view-table-rerender (selected-window))
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
      (unless (text-property-not-all (point-min) (point-max) 'niwa-view-table-cell nil)
        (error "Fixture table was not laid out"))
      (unless (equal "> active draft\nsecond line" (niwa-view-composer-draft-text))
        (error "Fixture redraw changed the composer"))
      (goto-char (point-min))
      (search-forward "── Worked for 2m 37s · Thu 29 Oct 14:32 ")
      (unless (get-text-property (- (point) 2) 'help-echo)
        (error "Fixture timestamp lost its timing tooltip"))
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
            (insert (x-export-frames nil type)))))
      (let ((width (frame-width)))
        (unwind-protect
            (progn
              (niwa-graphical-check--resize-width 60)
              (redisplay t)
              (niwa-view-table-rerender (selected-window))
              (redisplay t)
              (let ((coding-system-for-write 'no-conversion))
                (with-temp-file (file-name-concat output "view-narrow.png")
                  (set-buffer-multibyte nil)
                  (insert (x-export-frames nil 'png)))))
          (niwa-graphical-check--resize-width width))))))

(defun niwa-graphical-check--capture-tool-groups (output)
  "Render activity groups and save collapsed and expanded exports under OUTPUT.
One run mixes Read with Bash calls that ran without confinement; another
holds failures."
  (require 'niwa-tool-exec-registration)
  (require 'niwa-view-disclosure)
  (niwa-view-test--with-buffers
    (niwa-tool-exec--register)
    (let ((sandbox '(:attempt-count 1 :started-count 1 :refused-count 0
                     :sandbox off :filesystem unrestricted :network unrestricted
                     :proc nil :additional-read-count 0 :additional-write-count 0))
          (id 0))
      (cl-flet ((tool (name args &optional failed)
                  (let ((tool-id (format "graphical-%d" (cl-incf id))))
                    (with-current-buffer data-buf
                      (goto-char (point-max))
                      (insert "#+begin_tool\n"
                              (propertize
                               (format "(:name %S :args %S)\n\n%s\n" name args
                                       (if failed "Error: failed" "done"))
                               'gptel (cons 'tool tool-id)))
                      (when (equal name "Bash")
                        (let ((start (point)))
                          (insert (substring-no-properties
                                   (niwa-tool-render-data-format
                                    (list :kind 'bash :command (plist-get args :command)
                                          :state 'completed :termination 'exited
                                          :exit-code (if failed 2 0)
                                          :outcome (if failed 'failure 'success)
                                          :wall-time-seconds 3.0
                                          :status (if failed 'error 'success)
                                          :sandbox-summary sandbox)
                                    tool-id)))
                          (put-text-property start (point) 'niwa-render-data t)))
                      (insert "#+end_tool\n")))))
        (niwa-view-test--insert-data data-buf "Check the build.\n\n" nil)
        (niwa-view-test--insert-data data-buf "Reading the build files.\n\n" 'response)
        (tool "Read" '(:file_path "Eask"))
        (tool "Bash" '(:command "make check"))
        (tool "Bash" '(:command "git status --short"))
        (niwa-view-test--insert-data data-buf "Now the failing cases.\n\n" 'response)
        (tool "Bash" '(:command "make test") t)
        (tool "Bash" '(:command "make lint") t)
        (tool "Bash" '(:command "ls test"))
        (tool "Read" '(:file_path "test/helpers.el"))
        (tool "Grep" '(:pattern "defun niwa-view--group") t)
        (niwa-view-test--insert-data data-buf "Done.\n" 'response))
      (switch-to-buffer view-buf)
      (rename-buffer "*niwa graphical tool groups*" t)
      (delete-other-windows)
      ;; The narrow export resized the frame asynchronously; fit group
      ;; headers to the settled width.
      (redisplay t)
      (sit-for 0.2)
      (niwa-view--full-rerender)
      (setq-local font-lock-defaults '(nil t))
      (font-lock-mode 1)
      (cl-flet ((export (name)
                  (font-lock-ensure)
                  (goto-char (point-min))
                  (set-window-start nil (point-min))
                  (message nil)
                  (redisplay t)
                  (write-region (point-min) (point-max)
                                (file-name-concat output (concat name ".txt"))
                                nil 'silent)
                  (let ((coding-system-for-write 'no-conversion))
                    (with-temp-file (file-name-concat output (concat name ".png"))
                      (set-buffer-multibyte nil)
                      (insert (x-export-frames nil 'png))))))
        (export "tool-groups")
        (let (groups (pos (point-min)))
          (while (setq pos (text-property-any pos (point-max)
                                              'niwa-view-type 'tool-group))
            (push pos groups)
            (setq pos (or (next-single-property-change pos 'niwa-view-type)
                          (point-max))))
          (unless groups
            (error "Tool group fixture folded no activity group"))
          ;; Last first, so expanding one leaves earlier positions valid.
          (dolist (group groups)
            (goto-char group)
            (niwa-view-toggle-section)))
        (export "tool-groups-expanded")))))

(defun niwa-graphical-check--capture-notes (output)
  "Capture source note labels under OUTPUT using real source commands."
  (require 'niwa-notes)
  (require 'niwa-notes-list)
  (let* ((directory (make-temp-file "niwa-graphical-notes-" t))
         (file (file-name-concat directory "example.el"))
         (workspace (niwa-workspace--create
                     :type 'file :id directory :root directory :name "notes"))
         (niwa--instruction-states (make-hash-table :test #'equal))
         source list-buffer)
    (unwind-protect
        (progn
          (write-region
           ";;; Example source notes\n\n(defun find-user (users id)\n  (seq-find (lambda (user) (= (car user) id)) users))\n\n(defun active-users (users)\n  (seq-filter #'cdr users))\n\n;; A code-only note can keep context for later.\n"
           nil file nil 'silent)
          (setq source (find-file-noselect file))
          (switch-to-buffer source)
          (delete-other-windows)
          (setq-local niwa--workspace workspace)
          (goto-char (point-min))
          (forward-line 3)
          (search-forward "seq-find")
          (let ((transient-mark-mode t))
            (set-mark (- (point) 8))
            (activate-mark)
            (niwa-note-create
             "Use an index if this lookup becomes frequent. Keep the complete explanation visible when expanded, including why repeated scans could become expensive as the number of users grows."))
          (goto-char (point-min))
          (forward-line 6)
          (overlay-put (niwa-note-create "Keep the ordering stable")
                       'niwa-note-attention-state 'ready)
          (goto-char (point-max))
          (forward-line -1)
          (niwa-note-create)
          (font-lock-ensure)
          (goto-char (point-min))
          (set-window-start nil (point-min))
          (message nil)
          (redisplay t)
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file (file-name-concat output "notes.png")
              (set-buffer-multibyte nil)
              (insert (x-export-frames nil 'png))))
          (niwa-notes-toggle-labels)
          (redisplay t)
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file (file-name-concat output "notes-expanded.png")
              (set-buffer-multibyte nil)
              (insert (x-export-frames nil 'png))))
          (setq list-buffer (niwa-notes-list workspace))
          (delete-other-windows)
          (goto-char (point-min))
          (redisplay t)
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file (file-name-concat output "notes-list.png")
              (set-buffer-multibyte nil)
              (insert (x-export-frames nil 'png)))))
      (when (buffer-live-p list-buffer) (kill-buffer list-buffer))
      (when (buffer-live-p source) (kill-buffer source))
      (delete-directory directory t))))

(defun niwa-graphical-check--child (output)
  "Run the graphical checks, save diagnostics in OUTPUT, and always exit."
  (let ((status 1))
    (condition-case err
        (progn
          (niwa-graphical-check--preflight)
          (set-frame-size nil 110 42)
          (sit-for 0.2)
          (niwa-graphical-check--capture output)
          (niwa-graphical-check--capture-tool-groups output)
          (niwa-graphical-check--capture-notes output)
          (require 'ert)
          (dolist (file niwa-graphical-check--files)
            (load (file-name-concat niwa-graphical-check--root "test" file) nil t))
          (let* ((stats (ert-run-tests-batch t))
                 (skipped (ert-stats-skipped stats))
                 (unexpected (ert-stats-completed-unexpected stats)))
            ;; ERT's batch summary names skips but omits their conditions.
            ;; Keep environmental refusals visible in the retained log.
            (cl-loop for test across (ert--stats-tests stats)
                     for result = (ert-test-most-recent-result test)
                     when (ert-test-skipped-p result)
                     do (message "Skipped %s:%s" (ert-test-name test)
                                 (ert-reason-for-test-result result)))
            (with-temp-file (file-name-concat output "summary.json")
              (insert (json-encode
                       `((emacs . ,emacs-version)
                         (window-system . ,(symbol-name window-system))
                         (font . ,(frame-parameter nil 'font))
                         (checkout . ,niwa-graphical-check--root)
                         (view-library . ,(symbol-file 'niwa-view--full-rerender 'defun))
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
