;;; niwa-theme-faces.el --- Theme-derived faces -*- lexical-binding: t -*-

;;; Commentary:

;; Derives registered face attributes from the active theme and reapplies
;; them when themes or measurable frames change.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'color)
(require 'niwa-utilities)

(defcustom niwa-muted-contrast 3.6
  "Minimum contrast ratio for muted text against the background.
Sits between the 3:1 floor for large text and the contrast of ordinary
prose, so text a face mutes reads as quiet without becoming unreadable.
A fixed blend cannot serve every theme: blending the foreground 45% into
the background yields 3.4:1 on a 7.8:1 theme but 6.3:1 on a 21:1 one and
2.0:1 on a 4:1 one, so `niwa--muted-color' searches for this target
instead."
  :type 'float
  :group 'niwa)

(defvar niwa--theme-derived-faces nil
  "Alist of (FACE . FUNCTION) for faces blended from the active theme.
FUNCTION is called with the default face's foreground and background and
returns a plist of face attributes to apply.")

(defun niwa--color-frame ()
  "Return a frame whose colors can be measured, or nil.
On a terminal `color-name-to-rgb' resolves through the terminal color
map, where a colour like #bbc2cf comes back as pure white, so a blend
measured there is meaningless."
  (seq-find (lambda (frame) (> (display-color-cells frame) 256))
            (frame-list)))

(defun niwa--contrast (color-a color-b)
  "Return the contrast ratio between COLOR-A and COLOR-B.
The Y component of CIE XYZ is relative luminance, so `color-srgb-to-xyz'
already does the work."
  (let* ((luminance
          (lambda (color)
            (nth 1 (apply #'color-srgb-to-xyz
                          (niwa--color-name-to-rgb color)))))
         (a (funcall luminance color-a))
         (b (funcall luminance color-b)))
    (/ (+ (max a b) 0.05) (+ (min a b) 0.05))))

(defun niwa--muted-color (foreground background)
  "Return FOREGROUND blended as far toward BACKGROUND as contrast allows.
The most muted blend still clearing `niwa-muted-contrast' wins.  On a
theme whose own foreground is already that low-contrast no blend clears
the target and FOREGROUND is returned unchanged, which is the honest
answer: there is no room below it."
  (or (cl-loop for percent downfrom 65 to 5 by 5
               for color = (niwa--tint foreground background
                                          (/ percent 100.0))
               when (>= (niwa--contrast color background)
                        niwa-muted-contrast)
               return color)
      foreground))

(defun niwa--face-user-styled-p (face)
  "Return non-nil when FACE carries a user or theme spec to defer to."
  (or (get face 'customized-face)
      (get face 'saved-face)
      (get face 'theme-face)))

(defun niwa--derive-theme-faces (&rest _)
  "Apply every derivation in `niwa--theme-derived-faces'.
Faces the user or a theme has styled are left alone, and nothing happens
at all on a display whose colors cannot be measured."
  (when-let* ((frame (niwa--color-frame)))
    ;; `niwa--tint' and `color-name-to-rgb' both resolve against the
    ;; selected frame, which in a daemon may be a terminal.
    (with-selected-frame frame
      (let ((foreground (face-foreground 'default frame t))
            (background (face-background 'default frame t)))
        (when (and foreground background)
          (pcase-dolist (`(,face . ,derive) niwa--theme-derived-faces)
            (when (and (facep face) (not (niwa--face-user-styled-p face)))
              (apply #'set-face-attribute face nil
                     (funcall derive foreground background)))))))))

(defun niwa--derive-theme-face (face derive)
  "Register FACE as derived from the active theme and derive it now.
DERIVE is called with the default face\='s foreground and background and
returns a plist of attributes for FACE."
  (setf (alist-get face niwa--theme-derived-faces) derive)
  (niwa--derive-theme-faces))

;; Themes reset frame attributes, so the blends have to be rewritten after
;; every change; a daemon has no measurable frame until one is created.
(add-hook 'enable-theme-functions #'niwa--derive-theme-faces)
(add-hook 'disable-theme-functions #'niwa--derive-theme-faces)
(add-hook 'after-make-frame-functions #'niwa--derive-theme-faces)


(provide 'niwa-theme-faces)

;;; niwa-theme-faces.el ends here
