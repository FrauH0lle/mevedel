;;; test-mevedel-present-wayland.el --- PGTK presentation on a real compositor -*- lexical-binding: t -*-

;;; Commentary:
;; Enables cheaper presentation in a pgtk daemon against a headless nested
;; KWin and reads the daemon's Wayland requests.  Pixel equality with stock
;; presentation is checked by hand with screenshots; see the 2026 entries of
;; ADR 0119.  Skipped without `kwin_wayland', a pgtk Emacs or the module's
;; build tools (`mevedel-test--wayland-available-p').

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-present-wayland--phases (trace)
  "Return an alist of each marked phase of TRACE to its request lines."
  (let (phases current)
    (dolist (line (split-string trace "\n" t))
      (if (string-match "\\`MEVEDEL-PHASE \\(.+\\)" line)
          (push (setq current (list (match-string 1 line))) phases)
        (when (and current (string-search " -> " line))
          (push line (cdr current)))))
    (mapcar (lambda (phase) (cons (car phase) (nreverse (cdr phase)))) phases)))

(defun mevedel-present-wayland--count (regexp lines)
  "Count LINES matching REGEXP."
  (cl-count-if (lambda (line) (string-match-p regexp line)) lines))

(defun mevedel-present-wayland--trace-window (trace phase)
  "Return raw TRACE lines within 32 lines of PHASE's marker.
Each line is capped at 220 characters to keep failure diagnostics bounded."
  (let* ((lines (split-string trace "\n" t))
         (marker (format "MEVEDEL-PHASE %s" phase))
         (index (cl-position-if (lambda (line) (string-prefix-p marker line)) lines)))
    (when index
      (let ((start (max 0 (- index 32)))
            (end (min (length lines) (+ index 33))))
        (mapconcat (lambda (line)
                     (if (> (length line) 220)
                         (concat (substring line 0 217) "...")
                       line))
                   (cl-subseq lines start end) "\n")))))

(defun mevedel-present-wayland--phase-trace (trace phase)
  "Return raw TRACE lines from PHASE's marker up to the next phase marker."
  (let* ((lines (split-string trace "\n" t))
         (marker (format "MEVEDEL-PHASE %s" phase))
         (start (cl-position-if (lambda (line) (string-prefix-p marker line)) lines)))
    (when start
      (let ((end (cl-position-if
                  (lambda (line) (string-prefix-p "MEVEDEL-PHASE " line))
                  lines :start (1+ start))))
        (mapconcat #'identity
                   (cl-subseq lines (1+ start) (or end (length lines)))
                   "\n")))))

(defun mevedel-present-wayland--sync-roundtrip-completed-p (trace)
  "Return non-nil if TRACE's sync phase has its matching callback done event."
  (let* ((phase (mevedel-present-wayland--phase-trace trace "sync"))
         (request-regexp
          "wl_display#[0-9]+\\.sync(new id wl_callback#\\([0-9]+\\))")
         (request-count (and phase (mevedel-present-wayland--count
                                    request-regexp
                                    (split-string phase "\n" t)))))
    (when (and (= (or request-count 0) 1)
               (string-match request-regexp phase))
      (let ((callback-id (match-string 1 phase))
            (request-end (match-end 0)))
        (string-match-p
         (format "wl_callback#%s\\.done(" (regexp-quote callback-id))
         (substring phase request-end))))))

(defun mevedel-present-wayland--build-sync-module (output)
  "Build the test-only Wayland sync module at OUTPUT."
  (let* ((source (file-name-concat mevedel-test--root "test" "fixtures"
                                   "present-wayland-sync.c"))
         (include-flags
          (cl-loop for directory in (list (expand-file-name "../include" invocation-directory)
                                          invocation-directory)
                   when (file-exists-p (expand-file-name "emacs-module.h" directory))
                   collect (concat "-I" directory))))
    (with-temp-buffer
      (unless (zerop (call-process "pkg-config" nil t nil "--cflags" "--libs"
                                   "gtk+-3.0" "wayland-client"))
        (error "GTK/Wayland development files are unavailable"))
      (let ((flags (split-string-and-unquote (buffer-string))))
        (erase-buffer)
        (unless (zerop (apply #'call-process "cc" nil t nil
                              (append (list "-Wall" "-Wextra" "-Werror"
                                            "-shared" "-fPIC" "-O2")
                                      include-flags (list source "-o" output) flags)))
          (error "Wayland sync module compilation failed: %s"
                 (string-trim (buffer-string))))))
    output))

(mevedel-deftest mevedel-present-wayland--phases
  (:doc "Splits a trace into the requests of each marked phase.")
  (should (equal (mevedel-present-wayland--phases
                  (concat "-> wl_surface#3.commit()\nMEVEDEL-PHASE a\n"
                          "[1] -> wl_surface#3.commit()\nwl_callback#4.done(1)\n"
                          "MEVEDEL-PHASE b\n"))
                 '(("b") ("a" "[1] -> wl_surface#3.commit()")))))

(mevedel-deftest mevedel-present-wayland--trace-window
  (:doc "Bounds a raw trace excerpt around the requested phase marker.")
  (let ((trace (mapconcat (lambda (index)
                            (if (= index 40)
                                "MEVEDEL-PHASE idle"
                              (format "wire line %d" index)))
                          (number-sequence 0 80) "\n")))
    (let ((window (mevedel-present-wayland--trace-window trace "idle")))
      (should (string-match-p "wire line 8" window))
      (should (string-match-p "MEVEDEL-PHASE idle" window))
      (should (string-match-p "wire line 72" window))
      (should-not (string-match-p "wire line 7\\(?:[^0-9]\\|\\'\\)" window))
      (should-not (string-match-p "wire line 73" window)))))

(mevedel-deftest mevedel-present-wayland--sync-roundtrip-completed
  (:doc "Requires the sync request's matching callback completion before idle.")
  (let ((prefix (concat "MEVEDEL-PHASE sync\n"
                        "[1] -> wl_display#1.sync(new id wl_callback#42)\n")))
    (should (mevedel-present-wayland--sync-roundtrip-completed-p
             (concat prefix "[2] wl_callback#42.done(9)\n"
                     "MEVEDEL-PHASE idle\n")))
    (dolist (trace (list (concat prefix "MEVEDEL-PHASE idle\n")
                         (concat prefix "[2] wl_callback#43.done(9)\n"
                                 "MEVEDEL-PHASE idle\n")
                         (concat prefix "MEVEDEL-PHASE idle\n"
                                 "[2] wl_callback#42.done(9)\n")
                         (concat "MEVEDEL-PHASE sync\n"
                                 "[1] wl_callback#42.done(9)\n"
                                 "[2] -> wl_display#1.sync(new id wl_callback#42)\n"
                                 "MEVEDEL-PHASE idle\n")))
      (should-not (mevedel-present-wayland--sync-roundtrip-completed-p trace)))))

(mevedel-deftest mevedel-present-wayland
  (:doc "Default and software GL reuse buffers, skip idle draws, show the bell and survive threads."
   :before-each (skip-unless (mevedel-test--wayland-available-p)))
  (let* ((module-directory (make-temp-file "mevedel-wayland-sync-" t))
         (module-path (file-name-concat
                       module-directory
                       (concat "present-wayland-sync" module-file-suffix)))
         (make-process-function (symbol-function 'make-process))
         sockets)
    (unwind-protect
        (progn
          (mevedel-present-wayland--build-sync-module module-path)
          (dolist (renderer '(default software))
            (let ((process-environment
                   (append (list (concat "MEVEDEL_NATIVE_SYNC_MODULE=" module-path))
                           (when (eq renderer 'software)
                             '("LIBGL_ALWAYS_SOFTWARE=1"
                               "MEVEDEL_NATIVE_PENDING_INPUT=1"))
                           process-environment)))
              (ert-info ((format "Renderer: %s" renderer))
                (pcase-let* ((`(,observed ,status . ,trace)
                             (cl-letf (((symbol-function 'make-process)
                                        (lambda (&rest arguments)
                                          (when (equal (plist-get arguments :name)
                                                       "mevedel-test-kwin")
                                            (push (cadr (member "--socket"
                                                                (plist-get arguments :command)))
                                                  sockets))
                                          (apply make-process-function arguments))))
                               (mevedel-test--run-wayland-fixture "present-wayland.el")))
                            (phases (mevedel-present-wayland--phases trace))
                            (stats (plist-get observed :stats)))
                  ;; Both sequential invocations must launch their own socket,
                  ;; not mistake a terminated predecessor's socket for readiness.
                  (should (= (length sockets) (if (eq renderer 'default) 1 2)))
                  (should (stringp (car sockets)))
                  (should (= (length sockets)
                             (length (delete-dups (copy-sequence sockets)))))
                  (ert-info ((format "Observed: %S" observed))
                    (should-not (plist-get observed :error))
                    (should (= 0 status))
                    (should (eq 'enabled (plist-get observed :state)))
                    (should (plist-get observed :wrong-target-rejected))
                    ;; Painting while a Lisp thread waited used to crash in GDK.
                    (should (plist-get observed :survived))
                    ;; The unchanged-render sample must span its intended 500 ms,
                    ;; even when software GL has pending command input.
                    (ert-info ((format "Raw idle Wayland trace (up to 32 lines either side):\n%s"
                                       (or (mevedel-present-wayland--trace-window trace "idle")
                                           "<idle marker missing>")))
                      (should (>= (plist-get observed :idle-duration) 0.45))
                      ;; The fallback must wait rather than spin while input remains
                      ;; queued; otherwise a full-duration sample can still burn a core.
                      (when (eq renderer 'software)
                        (should (plist-get observed :idle-pending-input))
                        ;; Ten 50 ms slices need about ten cycles; a busy-spin loop
                        ;; has many more even if its wall-clock duration is correct.
                        (should (<= (plist-get observed :idle-cycles) 50))
                        (should (plist-get observed :idle-pending-after))))
                    (should (< (plist-get observed :idle-cpu-duration)
                               (* 0.8 (plist-get observed :idle-duration))))
                    ;; Region redraws happened, and redisplays that drew nothing were
                    ;; dropped.
                    (should (> (aref stats 0) 0))
                    (should (> (aref stats 1) 0))
                    (should (= 1 (aref stats 3))))
                  (ert-info ((format "Phases: %S" (mapcar (lambda (p) (cons (car p) (length (cdr p)))) phases)))
                    ;; The wrong target is rejected before any Wayland roundtrip; the
                    ;; exact private frame target is then synchronized before idle.
                    (should (= 0 (mevedel-present-wayland--count
                                  "wl_display#[0-9]+\\.sync"
                                  (alist-get "wrong-target" phases nil nil #'equal))))
                    (should (= 0 (mevedel-present-wayland--count
                                  "xdg_toplevel#[0-9]+\\.\\(?:set_maximized\\|unset_maximized\\)"
                                  (alist-get "wrong-target" phases nil nil #'equal))))
                    ;; Configure generation is deliberately separate from the tested
                    ;; barrier, so omitting sync leaves these same requests queued.
                    (should (> (mevedel-present-wayland--count
                                "xdg_toplevel#[0-9]+\\.\\(?:set_maximized\\|unset_maximized\\)"
                                (alist-get "queue-configure" phases nil nil #'equal)) 0))
                    (should (= 0 (mevedel-present-wayland--count
                                  "xdg_toplevel#[0-9]+\\.\\(?:set_maximized\\|unset_maximized\\)"
                                  (alist-get "sync" phases nil nil #'equal))))
                    ;; The unchanged idle phase must remain protocol-silent.
                    (ert-info ((format "Raw idle Wayland trace (up to 32 lines either side):\n%s"
                                       (or (mevedel-present-wayland--trace-window trace "idle")
                                           "<idle marker missing>")))
                      (should-not (alist-get "idle" phases nil nil #'equal)))
                    (ert-info ((format "Barrier trace around sync and idle:\nsync:\n%s\nidle:\n%s"
                                       (or (mevedel-present-wayland--trace-window trace "sync")
                                           "<sync marker missing>")
                                       (or (mevedel-present-wayland--trace-window trace "idle")
                                           "<idle marker missing>")))
                      (should (> (mevedel-present-wayland--count
                                  "wl_display#[0-9]+\\.sync"
                                  (alist-get "sync" phases nil nil #'equal)) 0)))
                    (ert-info ((format "Raw sync callback trace:\n%s"
                                       (or (mevedel-present-wayland--trace-window trace "sync")
                                           "<sync marker missing>")))
                      (should (mevedel-present-wayland--sync-roundtrip-completed-p trace)))
                    (should (string-match-p
                             "xdg_toplevel#[0-9]+\\.configure"
                             (mevedel-present-wayland--trace-window trace "sync")))
                    (should (> (mevedel-present-wayland--count
                                "xdg_surface#[0-9]+\\.ack_configure"
                                (alist-get "sync" phases nil nil #'equal)) 0))
                    ;; Each change was presented.
                    (should (> (mevedel-present-wayland--count "wl_surface#[0-9]+\\.commit" (alist-get "insert" phases nil nil #'equal)) 0))
                    ;; The visible bell, drawn on a copy of the frame's surface.
                    (should (> (mevedel-present-wayland--count "wl_surface#[0-9]+\\.commit" (alist-get "bell" phases nil nil #'equal)) 0))
                    (should (>= (mevedel-present-wayland--count "wl_surface#[0-9]+\\.commit" (alist-get "thread" phases nil nil #'equal)) 5))
                    ;; Software EGL also uses shared memory, but neither renderer
                    ;; allocates a fresh shared-memory window copy per frame.
                    (dolist (phase phases)
                      (should (= 0 (mevedel-present-wayland--count "wl_shm_pool#[0-9]+\\.create_buffer" (cdr phase))))))))))
      (delete-directory module-directory t)))))

(mevedel-deftest mevedel-present-wayland/thread
  (:doc "Isolates launched processes and survives a Lisp thread showing a frame.")
  (pcase-let* ((_ (skip-unless (mevedel-test--wayland-available-p)))
               (launch-environments nil)
               (real-make-process (symbol-function 'make-process))
               (process-environment
                (append '("DBUS_SESSION_BUS_ADDRESS=mevedel-invalid-host-bus"
                          "DBUS_STARTER_ADDRESS=mevedel-invalid-starter-bus"
                          "DBUS_STARTER_BUS_TYPE=mevedel-invalid-starter-bus-type")
                        process-environment))
               (`(,observed ,status . ,_)
                (cl-letf (((symbol-function 'make-process)
                           (lambda (&rest arguments)
                             (let ((name (plist-get arguments :name)))
                               (when (member name '("mevedel-test-kwin"
                                                    "mevedel-test-wayland-daemon"))
                                 (dolist (variable '("DBUS_SESSION_BUS_ADDRESS"
                                                     "DBUS_STARTER_ADDRESS"
                                                     "DBUS_STARTER_BUS_TYPE"))
                                   (should-not (getenv variable))))
                               (push (cons name (copy-sequence process-environment))
                                     launch-environments)
                               (apply real-make-process arguments)))))
                  (condition-case err
                      (mevedel-test--run-wayland-fixture "present-wayland-thread.el" 20)
                    (ert-test-failed
                     ;; Stock pgtk itself deadlocks here now and then: the
                     ;; thread waits in `gtk_main_iteration' for the GLib
                     ;; context while the main thread waits in `thread-join'.
                     ;; A crash or an error still fails.
                     (if (equal (cadr err) "Wayland scenario timed out")
                         (ert-skip "Stock pgtk deadlocked showing a frame from a thread")
                       (signal (car err) (cdr err))))))))
    (should (equal (sort (mapcar #'car launch-environments) #'string<)
                   '("mevedel-test-kwin" "mevedel-test-wayland-daemon")))
    (dolist (variable '(("DBUS_SESSION_BUS_ADDRESS" . "mevedel-invalid-host-bus")
                        ("DBUS_STARTER_ADDRESS" . "mevedel-invalid-starter-bus")
                        ("DBUS_STARTER_BUS_TYPE" . "mevedel-invalid-starter-bus-type")))
      (should (equal (getenv (car variable)) (cdr variable)))
      (dolist (launch launch-environments)
        (let ((process-environment (cdr launch)))
          (should-not (getenv (car variable))))))
    (ert-info ((format "Observed: %S" observed))
      (should-not (plist-get observed :error))
      (should (= 0 status))
      (should (eq 'enabled (plist-get observed :state)))
      (should (eq t (plist-get observed :visible))))))

(provide 'test-mevedel-present-wayland)

;;; test-mevedel-present-wayland.el ends here
