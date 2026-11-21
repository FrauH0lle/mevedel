;;; niwa-present.el --- Cheaper PGTK presentation -*- lexical-binding: t -*-

;;; Commentary:

;; Stock PGTK Emacs redraws the whole frame after every redisplay, and GTK 3
;; then copies the whole window into a new shared-memory buffer while the
;; compositor still holds the last one.  On a large HiDPI frame that costs
;; about 11 ms of CPU per presented frame, whatever changed: a 10-Hz timer
;; that draws nothing used 15% of a core.  The native module makes PGTK
;; frames on Wayland paint through GL and present only what Emacs drew.
;;
;; This changes how all of this Emacs's frames present, not only niwa's.
;; It is enabled once, at the first view on a PGTK frame, and lasts until
;; Emacs exits.

;;; Code:

(require 'niwa-utilities)

(require 'niwa-native)

;; `native/niwa-present.c'
(declare-function niwa-present--enable "ext:niwa-present" ())
(declare-function niwa-present--stats "ext:niwa-present" ())

(defcustom niwa-present-enabled t
  "Present PGTK frames on Wayland through GL, redrawing only what changed.
This applies to every frame of this Emacs, from niwa's first view on a
PGTK frame until Emacs exits; nil keeps Emacs's own presentation.  It
needs the native module, built as `niwa-view-native-enabled' describes,
or downloaded with `niwa-native-install'.  With it, breathe and bounce
text animation runs at the power-aware frame rate instead of 8 fps."
  :type 'boolean
  :group 'niwa)

(defvar niwa-present--state nil
  "nil until the module tries presentation, then `enabled' or its refusal.")

(defun niwa-present-active-p ()
  "Return non-nil when PGTK frames present only what changed."
  (eq niwa-present--state 'enabled))

(defun niwa-present-ensure (frame)
  "Enable cheaper presentation once, when FRAME is a PGTK frame.
Return non-nil when it is active."
  (when (and niwa-present-enabled (not niwa-present--state)
             (not noninteractive) (eq (framep frame) 'pgtk)
             ;; A failure leaves this unset, so an installed module enables
             ;; presentation at the next view.
             (niwa-native-load))
    (setq niwa-present--state
          (let ((result (niwa-present--enable)))
            (if (eq result t)
                'enabled
              (niwa-diagnostic-message 'global "PGTK presentation unchanged: %s" result)
              result))))
  (niwa-present-active-p))

(provide 'niwa-present)
;;; niwa-present.el ends here
