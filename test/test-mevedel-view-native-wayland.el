;;; test-mevedel-view-native-wayland.el --- Native presenter on a real compositor -*- lexical-binding: t -*-

;;; Commentary:
;; Runs the native module in a pgtk daemon against a headless nested KWin,
;; so module construction, placement, coordinate translation, protocol order
;; and display teardown are exercised for real rather than through mocks.
;; Skipped without `kwin_wayland', a pgtk Emacs or the module's build tools
;; (`mevedel-test--wayland-available-p').

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-view-native-wayland--trace-check (trace)
  "Return the subsurface requests in TRACE and any ordering violation.
After a subsurface's position changes, it may run desynchronized only once
a frame callback requested on its parent after that change is done.  The
result is (POSITIONS . VIOLATIONS), POSITIONS being (X . Y) in order."
  (let ((subsurfaces (make-hash-table :test #'equal))
        (parents (make-hash-table :test #'equal))
        (pending (make-hash-table :test #'equal))
        (callbacks (make-hash-table :test #'equal))
        positions violations)
    (dolist (line (split-string trace "\n" t))
      (cond
       ((string-match "-> wl_subcompositor#[0-9]+\\.get_subsurface(new id wl_subsurface#\\([0-9]+\\), wl_surface#[0-9]+, wl_surface#\\([0-9]+\\))" line)
        (puthash (match-string 1 line) (match-string 2 line) subsurfaces)
        (puthash (match-string 2 line) t parents))
       ((string-match "-> wl_subsurface#\\([0-9]+\\)\\.set_position(\\(-?[0-9]+\\), \\(-?[0-9]+\\))" line)
        (when (gethash (match-string 1 line) subsurfaces)
          (push (cons (string-to-number (match-string 2 line))
                      (string-to-number (match-string 3 line)))
                positions)
          ;; Positioned anew: wait for a callback requested from now on.
          (puthash (match-string 1 line) 'moved pending)))
       ((string-match "-> wl_surface#\\([0-9]+\\)\\.frame(new id wl_callback#\\([0-9]+\\))" line)
        ;; Object IDs are reused once their callbacks are done.
        (remhash (match-string 2 line) callbacks)
        (when (gethash (match-string 1 line) parents)
          (maphash (lambda (subsurface state)
                     (when (eq state 'moved)
                       (puthash subsurface (match-string 2 line) pending)))
                   pending)))
       ((string-match "wl_callback#\\([0-9]+\\)\\.done" line)
        (puthash (match-string 1 line) t callbacks))
       ((string-match "-> wl_subsurface#\\([0-9]+\\)\\.set_desync()" line)
        (let ((state (gethash (match-string 1 line) pending)))
          (unless (and (stringp state) (gethash state callbacks))
            (push line violations))))))
    (cons (nreverse positions) (nreverse violations))))

(mevedel-deftest mevedel-view-native-wayland--trace-check
  (:doc "Rejects desynchronizing on a callback requested before the last move.")
  (let ((header (concat
                 "-> wl_subcompositor#5.get_subsurface(new id wl_subsurface#9, wl_surface#8, wl_surface#3)\n"
                 "-> wl_subsurface#9.set_position(10, 20)\n"
                 "-> wl_surface#3.frame(new id wl_callback#11)\n")))
    (should (equal '(((10 . 20)) . nil)
                   (mevedel-view-native-wayland--trace-check
                    (concat header "wl_callback#11.done(1)\n-> wl_subsurface#9.set_desync()\n"))))
    ;; Moved again while the first callback was pending, without a new one.
    (should (cdr (mevedel-view-native-wayland--trace-check
                  (concat header "-> wl_subsurface#9.set_position(10, 30)\n"
                          "wl_callback#11.done(1)\n-> wl_subsurface#9.set_desync()\n"))))
    (should-not (cdr (mevedel-view-native-wayland--trace-check
                      (concat header "-> wl_subsurface#9.set_position(10, 30)\n"
                              "-> wl_surface#3.frame(new id wl_callback#12)\n"
                              "wl_callback#11.done(1)\nwl_callback#12.done(2)\n"
                              "-> wl_subsurface#9.set_desync()\n"))))
    ;; A reused ID's earlier completion does not count for the new callback.
    (should (cdr (mevedel-view-native-wayland--trace-check
                  (concat "wl_callback#12.done(0)\n" header
                          "-> wl_subsurface#9.set_position(10, 30)\n"
                          "-> wl_surface#3.frame(new id wl_callback#12)\n"
                          "-> wl_subsurface#9.set_desync()\n"))))))

(mevedel-deftest mevedel-view-native-wayland
  (:doc "Presents, moves and replaces a closed surface on a real compositor.")
  (pcase-let* ((_ (skip-unless (mevedel-test--wayland-available-p)))
               (`(,observed ,status . ,trace)
                (mevedel-test--run-wayland-fixture "native-wayland.el"))
               (first observed)
               (placement (plist-get first :placement))
               (offset (plist-get first :native-offset))
               (checked (mevedel-view-native-wayland--trace-check trace)))
    (ert-info ((format "Observed: %S" observed))
      (should-not (plist-get observed :error))
      (should (= 0 status))
      (should (plist-get first :available))
      (should placement)
      ;; Pango measured the label as wide as redisplay laid it out.
      (should (plist-get first :opened))
      ;; Text and an invisible overlay above both moved it.
      (should (plist-get first :moved-same))
      (should (> (aref (car (plist-get first :moved-placement)) 1)
                 (aref (car placement) 1)))
      (let ((overlay (plist-get first :overlay-placement)))
        (should (equal (nth 0 overlay) (nth 1 overlay))))
      (should (plist-get first :restored))
      (should (plist-get first :reopened)))
    (ert-info ((format "Positions: %S" (car checked)))
      ;; The first surface's position is the placement translated
      ;; from the text area into the toplevel.
      (should (equal (cons (+ (aref (car placement) 0) (nth 0 offset))
                           (+ (aref (car placement) 1) (nth 1 offset)))
                     (car (car checked))))
      (should-not (cdr checked)))))

(provide 'test-mevedel-view-native-wayland)

;;; test-mevedel-view-native-wayland.el ends here
