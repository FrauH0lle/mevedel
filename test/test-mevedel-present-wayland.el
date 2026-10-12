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

(mevedel-deftest mevedel-present-wayland--phases
  (:doc "Splits a trace into the requests of each marked phase.")
  (should (equal (mevedel-present-wayland--phases
                  (concat "-> wl_surface#3.commit()\nMEVEDEL-PHASE a\n"
                          "[1] -> wl_surface#3.commit()\nwl_callback#4.done(1)\n"
                          "MEVEDEL-PHASE b\n"))
                 '(("b") ("a" "[1] -> wl_surface#3.commit()")))))

(mevedel-deftest mevedel-present-wayland
  (:doc "Presents through GL, only when Emacs drew, and survives waiting threads.")
  (pcase-let* ((_ (skip-unless (mevedel-test--wayland-available-p)))
               (`(,observed ,status . ,trace)
                (mevedel-test--run-wayland-fixture "present-wayland.el"))
               (phases (mevedel-present-wayland--phases trace))
               (stats (plist-get observed :stats)))
    (ert-info ((format "Observed: %S" observed))
      (should-not (plist-get observed :error))
      (should (= 0 status))
      (should (eq 'enabled (plist-get observed :state)))
      ;; Painting while a Lisp thread waited used to crash in GDK.
      (should (plist-get observed :survived))
      ;; Region redraws happened, and redisplays that drew nothing were
      ;; dropped.
      (should (> (aref stats 0) 0))
      (should (> (aref stats 1) 0))
      (should (= 1 (aref stats 3))))
    (ert-info ((format "Phases: %S" (mapcar (lambda (p) (cons (car p) (length (cdr p)))) phases)))
      ;; Each change was presented.
      (should (> (mevedel-present-wayland--count "wl_surface#[0-9]+\\.commit" (alist-get "insert" phases nil nil #'equal)) 0))
      (should (>= (mevedel-present-wayland--count "wl_surface#[0-9]+\\.commit" (alist-get "thread" phases nil nil #'equal)) 5))
      ;; Nothing drawn, nothing sent.
      (should-not (alist-get "idle" phases nil nil #'equal))
      ;; GL buffers, not a fresh shared-memory window copy per frame.
      (dolist (phase phases)
        (should (= 0 (mevedel-present-wayland--count "wl_shm_pool#[0-9]+\\.create_buffer" (cdr phase))))))))

(provide 'test-mevedel-present-wayland)

;;; test-mevedel-present-wayland.el ends here
