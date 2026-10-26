;;; mevedel-evil.el --- Optional Evil integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Call `mevedel-evil-setup' to opt in.  Loading this library alone does not
;; load Evil, change keys, or install hooks.  Ordinary source editing stays
;; with Evil; the source owner handles replacement geometry and undo.

;;; Code:

(require 'cl-lib)

;; `evil'
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-exit-visual-state "evil-states" (&optional repeat))
(declare-function evil-normal-state "evil-states" (&optional arg))
(declare-function evil-set-command-property "evil-common" (command property value))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-visual-range "evil-states" ())
(defvar evil-local-mode)
(defvar evil-motion-state-map)
(defvar evil-normal-state-map)
(defvar evil-state)

;; `mevedel-directive-source'
(declare-function mevedel-instruction-replacement-end "mevedel-directive-source" (&optional buffer))
(declare-function mevedel-instruction-replacement-hold "mevedel-directive-source" (&optional buffer))

;; `mevedel-overlays'
(declare-function mevedel--create-instruction "mevedel-overlays" (type &optional span))

;; `mevedel-view'
(defvar mevedel-view--input-marker)
(defvar mevedel-view-mode-map)

;; `mevedel-view-composer'
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(defvar mevedel-view--composer-keymap)
(defvar mevedel-view--side-conversation-keymap)
(defvar mevedel-view--side-conversation-p)

(defconst mevedel-evil--cockpit-modes
  '(mevedel-artifacts-list-mode mevedel-background-list-mode
    mevedel-memory-list-mode mevedel-pending-inputs-mode
    mevedel-permissions-list-mode mevedel-plugins-list-mode
    mevedel-skills-list-mode mevedel-tools-list-mode
    mevedel-worktree-list-mode)
  "Tabulated cockpits whose generated local maps need Evil bindings.")

(defconst mevedel-evil--map-features
  '((mevedel-report mevedel-report-mode-map)
    (mevedel-journal-jobs mevedel-journal-jobs-mode-map)
    (mevedel-directive-activity mevedel-directive-activity-mode-map)
    (mevedel-directive-frame mevedel-directive-frame-mode-map)
    (mevedel-subscription-usage mevedel-subscription-usage-mode-map)
    (mevedel-collaboration-share mevedel-collaboration--share-map)
    (mevedel-view mevedel-view-mode-map mevedel-view--display-map)
    (mevedel-view-agent mevedel-view--agent-parent-map
                        mevedel-view--agent-handle-map mevedel-view--agent-label-map)
    (mevedel-view-composer mevedel-view--composer-keymap
                          mevedel-view--side-conversation-keymap
                          mevedel-view--armed-session-fork-map)
    (mevedel-view-control-transfer mevedel-view--control-transfer-status-map
                                   mevedel-view--control-transfer-map
                                   mevedel-view--control-transfer-request-map)
    (mevedel-view-execution mevedel-view-execution--action-map)
    (mevedel-view-interaction mevedel-view--pending-plan-map)
    (mevedel-view-segments mevedel-view-segments--latest-map
                           mevedel-view-segments--chats-map)
    (mevedel-patch-review mevedel-patch-review-map mevedel-patch-review-row-map
                          mevedel-patch-review-submit-map
                          mevedel-patch-review-reject-map
                          mevedel-patch-review-feedback-map)
    (mevedel-pending-inputs mevedel-view--pending-inputs-map))
  "Feature boundaries and their user-facing maps.")

(defvar mevedel-evil--installed nil
  "Non-nil after explicit integration setup.")

(defvar mevedel-evil--overlay-maps (make-hash-table :test #'eq :weakness 'key)
  "Native maps already adapted, without retaining discarded fragments.")

(defun mevedel-evil--normal-p ()
  "Return non-nil in an Evil-enabled buffer's normal state."
  (and (bound-and-true-p evil-local-mode) (eq evil-state 'normal)))

(defun mevedel-evil--binding-key (key)
  "Return the Evil action key for vanilla KEY, preserving motion keys."
  (or (cdr (assoc key '(("g" . "gr") ("G" . "gG")
                       ("h" . "H") ("j" . "J")
                       ("k" . "K") ("l" . "L"))))
      key))

(defun mevedel-evil--adapt-map (map &optional editable)
  "Expose MAP's own actions in normal state, and EDITABLE control keys.
Do not copy inherited `text-mode' or `special-mode' bindings.  Insert state
keeps ordinary typing; only prefixed control keys are made explicit."
  (let ((own-map (copy-keymap map)) bindings)
    (set-keymap-parent own-map nil)
    (map-keymap
     (lambda (event command)
       (unless (memq event '(menu-bar normal-state insert-state motion-state
                            visual-state operator-state replace-state emacs-state remap))
         (push (cons (vector event) command) bindings)))
     own-map)
    (dolist (binding bindings)
      (let* ((key (car binding))
             (description (key-description key))
             (command (cdr binding)))
        (evil-define-key* 'normal map
          (kbd (mevedel-evil--binding-key description)) command)
        (when (and editable
                   (or (keymapp command)
                       (string-prefix-p "C-" description)
                       (string-prefix-p "M-" description)
                       (member description '("<backtab>" "S-TAB"))))
          (evil-define-key* 'insert map key command))))
    (unless editable
      (when-let* ((quit (lookup-key map (kbd "q"))) ((commandp quit)))
        (evil-define-key* 'normal map (kbd "q") quit)))))

(defun mevedel-evil--overlay-map (map)
  "Adapt native overlay MAP without relying on Evil auxiliary map lookup.
Overlay and text-property maps outrank Evil's emulation maps.  Filter the
conflicting native actions, preserving vanilla behavior in other states."
  (unless (gethash map mevedel-evil--overlay-maps)
    (let* ((refresh (lookup-key map (kbd "g")))
           (bottom (lookup-key map (kbd "G")))
           (prefix (make-composed-keymap
                    (delq nil
                          (list (and (keymapp refresh) refresh)
                                (lookup-key evil-normal-state-map (kbd "g"))
                                (lookup-key evil-motion-state-map (kbd "g")))))))
      (when (and refresh (not (keymapp refresh)))
        (define-key prefix (kbd "r") refresh))
      (when bottom (define-key prefix (kbd "G") bottom))
      (when (or refresh bottom)
        (define-key map (kbd "g")
                    `(menu-item "" ,refresh
                                :filter ,(lambda (command)
                                           (if (mevedel-evil--normal-p) prefix command)))))
      (dolist (key '("h" "j" "k" "l" "G"))
        (when-let* ((command (lookup-key map (kbd key)))
                    ((not (keymapp command))))
          (unless (equal key "G")
            (define-key map (kbd (mevedel-evil--binding-key key))
                        `(menu-item "" ,command
                                    :filter ,(lambda (action)
                                               (and (mevedel-evil--normal-p) action)))))
          (define-key map (kbd key)
                      `(menu-item "" ,command
                                  :filter ,(lambda (action)
                                             (unless (mevedel-evil--normal-p) action)))))))
    (puthash map t mevedel-evil--overlay-maps))
  map)

(defun mevedel-evil--cockpit (&rest _)
  "Adapt the current cockpit's generated map after surface setup."
  (when (memq major-mode mevedel-evil--cockpit-modes)
    (mevedel-evil--adapt-map (current-local-map))))

(defun mevedel-evil--key-help (text)
  "Translate generated cockpit key help TEXT for Evil normal state."
  (if (not (mevedel-evil--normal-p)) text
    (mapconcat
     (lambda (line)
       (if (not (string-match "\\`\\([^ \n]+\\) +" line)) line
         (let ((key (match-string 1 line)) (end (match-end 0)))
           (format "%-4s %s" (mevedel-evil--binding-key key)
                   (substring line end)))))
     (split-string text "\n") "\n")))

(defun mevedel-evil--feature-maps (entry)
  "Install the loaded feature maps listed in ENTRY."
  (dolist (symbol (cdr entry))
    (when (and (boundp symbol) (keymapp (symbol-value symbol)))
      (if (or (string-suffix-p "-mode-map" (symbol-name symbol))
              (eq symbol 'mevedel-collaboration--share-map))
          (mevedel-evil--adapt-map
           (symbol-value symbol)
           (memq symbol '(mevedel-view-mode-map mevedel-directive-frame-mode-map)))
        (mevedel-evil--overlay-map (symbol-value symbol))))))

(defun mevedel-evil--fragment-maps (&rest maps)
  "Adapt native fragment MAPS before composition, once per map identity."
  (dolist (map maps)
    (when map (mevedel-evil--overlay-map map))))

(defun mevedel-evil--in-composer-p ()
  "Return non-nil in a live view's composer, including its prompt."
  (and (derived-mode-p 'mevedel-view-mode)
       (markerp mevedel-view--input-marker)
       (marker-position mevedel-view--input-marker)
       (>= (point) (marker-position mevedel-view--input-marker))))

(defun mevedel-evil--composer-keys ()
  "Keep composer controls reachable when normal point sits on its prompt.
An empty composer's overlay has no character on which Evil can leave its
normal cursor.  Mirror its controls in the view's state maps, retaining
the ordinary view action outside the composer."
  (dolist (entry (append (accessible-keymaps mevedel-view--composer-keymap)
                         (accessible-keymaps mevedel-view--side-conversation-keymap)))
    (let ((prefix (car entry)))
      (unless (and (> (length prefix) 0) (eq (aref prefix 0) 'remap))
        (map-keymap
         (lambda (event command)
           (when (commandp command)
             (let* ((key (vconcat prefix (vector event)))
                    (fallback (lookup-key mevedel-view-mode-map key)))
               (evil-define-key* '(normal insert) mevedel-view-mode-map key
                 `(menu-item "" ,command
                             :filter ,(lambda (_action)
                                        (if (mevedel-evil--in-composer-p)
                                            (let ((active
                                                   (lookup-key
                                                    (if mevedel-view--side-conversation-p
                                                        mevedel-view--side-conversation-keymap
                                                      mevedel-view--composer-keymap)
                                                    key)))
                                              (and (commandp active) active))
                                          (and (commandp fallback) fallback))))))))
         (cdr entry))))))

(defun mevedel-evil--insert-composer ()
  "Move past a read-only composer prompt when entering an editing state."
  (when (mevedel-evil--in-composer-p)
    (goto-char (max (point) (mevedel-view--input-start)))))

(defun mevedel-evil--source-maps ()
  "Offer RET for source actions only in Evil normal state.
The existing dispatch key and all vanilla bindings remain available."
  (dolist (symbol '(mevedel-reference-actions-map mevedel-directive-actions-map
                    mevedel-directive-processing-actions-map
                    mevedel-directive-succeeded-actions-map
                    mevedel-directive-failed-actions-map))
    (dolist (key '("RET" "<return>"))
      (define-key (symbol-value symbol) (kbd key)
                  '(menu-item "" mevedel--ov-actions-dispatch
                              :filter mevedel-evil--source-action)))))

(defun mevedel-evil--source-action (command)
  "Return source action COMMAND only in normal state."
  (and (mevedel-evil--normal-p) command))

(defun mevedel-evil--selection (function &rest args)
  "Call selection FUNCTION with ARGS using Evil's explicit visual range.
Creation receives a span argument, even when characterwise bounds happen
to cover whole lines.  Block selections are deliberately unsupported."
  (if (not (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual)))
      (apply function args)
    (unwind-protect
        (let* ((range (evil-visual-range))
               (beg (nth 0 range)) (end (nth 1 range))
               (type (nth 2 range))
               (forward (>= (point) (mark)))
               (transient-mark-mode t)
               (mark-active t))
          (when (eq type 'block)
            (user-error "Block selections cannot create or delete instructions"))
          (goto-char (if forward end beg))
          (set-mark (if forward beg end))
          (if args
              (funcall function (car args) (if (eq type 'line) 'line 'char))
            (funcall function)))
      (evil-exit-visual-state)
      (evil-normal-state))))

(defun mevedel-evil--replacement (function &rest args)
  "Hold source replacements while FUNCTION with ARGS enters editing state."
  (if (not (bound-and-true-p evil-local-mode))
      (apply function args)
    (let ((buffer (current-buffer)))
      (mevedel-instruction-replacement-hold buffer)
      (unwind-protect
          (apply function args)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (unless (memq evil-state '(insert replace))
              (mevedel-instruction-replacement-end buffer))))))))

(defun mevedel-evil--replacement-end ()
  "Release the current source buffer's Evil replacement hold."
  (mevedel-instruction-replacement-end))

(defun mevedel-evil--local-mode ()
  "Finalize a replacement when local Evil mode is disabled."
  (unless evil-local-mode (mevedel-instruction-replacement-end)))

;;;###autoload
(defun mevedel-evil-setup ()
  "Opt in to mevedel's Evil keys and selection/editing integration.
Require Evil at call time, without enabling it.  Calling this repeatedly
is harmless.  No evil-collection dependency is needed."
  (interactive)
  (unless (require 'evil nil t)
    (user-error "Evil is not installed; install Evil before calling mevedel-evil-setup"))
  (unless mevedel-evil--installed
    (require 'mevedel-overlays)
    (setq mevedel-evil--installed t)
    (evil-set-initial-state 'mevedel-surface-mode 'insert)
    (dolist (mode (append mevedel-evil--cockpit-modes
                          '(mevedel-view-mode mevedel-report-mode
                            mevedel-directive-activity-mode
                            mevedel-worktree-setup-mode)))
      (evil-set-initial-state mode 'normal))
    (dolist (entry mevedel-evil--map-features)
      (eval-after-load (car entry)
        `(mevedel-evil--feature-maps ',entry)))
    (eval-after-load 'mevedel-cockpit
      '(progn
         (advice-add 'mevedel-cockpit-setup-tabulated-surface
                     :after #'mevedel-evil--cockpit)
         (advice-add 'mevedel-cockpit-surface-key-help-text
                     :filter-return #'mevedel-evil--key-help)))
    (eval-after-load 'mevedel-view
      '(advice-add 'mevedel-view--display-fragment-keymap
                   :before #'mevedel-evil--fragment-maps))
    (eval-after-load 'mevedel-view
      '(eval-after-load 'mevedel-view-composer '(mevedel-evil--composer-keys)))
    (eval-after-load 'mevedel-overlay-ui '(mevedel-evil--source-maps))
    (dolist (command '(mevedel-create-reference mevedel-create-directive
                       mevedel-delete-instructions))
      (evil-set-command-property command :keep-visual t))
    (advice-add 'mevedel--create-instruction :around #'mevedel-evil--selection)
    (advice-add 'mevedel-delete-instructions :around #'mevedel-evil--selection)
    (dolist (command '(evil-change evil-enter-replace-state))
      (advice-add command :around #'mevedel-evil--replacement))
    (add-hook 'evil-insert-state-exit-hook #'mevedel-evil--replacement-end)
    (add-hook 'evil-insert-state-entry-hook #'mevedel-evil--insert-composer)
    (add-hook 'evil-replace-state-entry-hook #'mevedel-evil--insert-composer)
    (add-hook 'evil-replace-state-exit-hook #'mevedel-evil--replacement-end)
    (add-hook 'evil-local-mode-hook #'mevedel-evil--local-mode)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (mevedel-evil--cockpit)))))

(provide 'mevedel-evil)
;;; mevedel-evil.el ends here
