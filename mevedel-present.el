;;; mevedel-present.el --- Cheaper PGTK presentation -*- lexical-binding: t -*-

;;; Commentary:

;; Stock PGTK Emacs redraws the whole frame after every redisplay, and GTK 3
;; then copies the whole window into a new shared-memory buffer while the
;; compositor still holds the last one.  On a large HiDPI frame that costs
;; about 11 ms of CPU per presented frame, whatever changed: a 10-Hz timer
;; that draws nothing used 15% of a core.  The native module makes PGTK
;; frames on Wayland paint through GL and present only what Emacs drew.
;;
;; This changes how all of this Emacs's frames present, not only mevedel's.
;; It is enabled once, at the first view on a PGTK frame, and lasts until
;; Emacs exits.

;;; Code:

(require 'mevedel-native)

;; `native/mevedel-present.c'
(declare-function mevedel-present--enable "ext:mevedel-present" ())
(declare-function mevedel-present--stats "ext:mevedel-present" ())

(defcustom mevedel-present-enabled t
  "Present PGTK frames on Wayland through GL, redrawing only what changed.
This applies to every frame of this Emacs, from mevedel's first view on a
PGTK frame until Emacs exits; nil keeps Emacs's own presentation.  It
needs the native module, built as `mevedel-view-native-enabled' describes,
or downloaded with `mevedel-native-install'.  With it, breathe and bounce
text animation runs at the power-aware frame rate instead of 8 fps."
  :type 'boolean
  :group 'mevedel)

(defvar mevedel-present--state nil
  "nil until tried, then `enabled' or why frames present as before.")

(defun mevedel-present-active-p ()
  "Return non-nil when PGTK frames present only what changed."
  (eq mevedel-present--state 'enabled))

(defun mevedel-present-ensure (frame)
  "Enable cheaper presentation once, when FRAME is a PGTK frame.
Return non-nil when it is active."
  (when (and mevedel-present-enabled (not mevedel-present--state)
             (not noninteractive) (eq (framep frame) 'pgtk))
    (setq mevedel-present--state
          (if (mevedel-native-load)
              (let ((result (mevedel-present--enable)))
                (if (eq result t)
                    'enabled
                  (message "mevedel: PGTK presentation unchanged: %s" result)
                  result))
            ;; `mevedel-native-load' reported why.
            mevedel-native--load-state)))
  (mevedel-present-active-p))

(provide 'mevedel-present)
;;; mevedel-present.el ends here
