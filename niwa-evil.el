;;; niwa-evil.el --- Optional Evil integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Call `niwa-evil-setup' to opt in.  Loading this library alone does not
;; load Evil, change keys, or install hooks.  Ordinary source editing stays
;; with Evil; the source owner handles replacement geometry and undo.

;;; Code:

(require 'niwa-utilities)

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

;; `niwa-note-source'
(declare-function niwa-instruction-replacement-end "niwa-note-source" (&optional buffer))
(declare-function niwa-instruction-replacement-hold "niwa-note-source" (&optional buffer))

;; `niwa-overlays'


;; `niwa-view-composer'
(declare-function niwa-view-composer-draft-start "niwa-view-composer" (&optional view))

(defconst niwa-evil--cockpit-modes
  '(niwa-artifacts-list-mode niwa-background-list-mode
    niwa-memory-list-mode niwa-pending-inputs-mode
    niwa-permissions-list-mode niwa-plugins-list-mode
    niwa-skills-list-mode niwa-tools-list-mode
    niwa-worktree-list-mode)
  "Tabulated cockpits whose generated local maps need Evil bindings.")

(defconst niwa-evil--map-features
  '((niwa-report niwa-report-mode-map)
    (niwa-journal-jobs niwa-journal-jobs-mode-map)
    (niwa-note-frame niwa-note-frame-mode-map)
    (niwa-subscription-usage niwa-subscription-usage-mode-map)
    (niwa-notes-list niwa-notes-list-mode-map)
    (niwa-collaboration-share niwa-collaboration--share-map)
    (niwa-view niwa-view-mode-map niwa-view--display-map)
    (niwa-view-agent niwa-view--agent-parent-map
                        niwa-view--agent-handle-map niwa-view--agent-label-map)
    (niwa-view-composer niwa-view--composer-keymap
                          niwa-view--side-conversation-keymap
                          niwa-view--armed-session-fork-map)
    (niwa-view-control-transfer niwa-view--control-transfer-status-map
                                   niwa-view--control-transfer-map
                                   niwa-view--control-transfer-request-map)
    (niwa-view-execution niwa-view-execution--action-map)
    (niwa-view-interaction niwa-view--pending-plan-map)
    (niwa-view-segments niwa-view-segments--latest-map
                           niwa-view-segments--chats-map)
    (niwa-patch-review niwa-patch-review-map niwa-patch-review-row-map
                          niwa-patch-review-submit-map
                          niwa-patch-review-reject-map
                          niwa-patch-review-feedback-map)
    (niwa-pending-inputs niwa-view--pending-inputs-map))
  "Feature boundaries and their user-facing maps.")

(defvar niwa-evil--installed nil
  "Non-nil after explicit integration setup.")

(defvar niwa-evil--overlay-maps (make-hash-table :test #'eq :weakness 'key)
  "Native maps already adapted, without retaining discarded fragments.")

(defun niwa-evil--normal-p ()
  "Return non-nil in an Evil-enabled buffer's normal state."
  (and (bound-and-true-p evil-local-mode) (eq evil-state 'normal)))

(defun niwa-evil--binding-key (key)
  "Return the Evil action key for vanilla KEY, preserving motion keys."
  (or (cdr (assoc key '(("g" . "gr") ("G" . "gG")
                       ("h" . "H") ("j" . "J")
                       ("k" . "K") ("l" . "L"))))
      key))

(defun niwa-evil--adapt-map (map &optional editable)
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
          (kbd (niwa-evil--binding-key description)) command)
        (when (and editable
                   (or (keymapp command)
                       (string-prefix-p "C-" description)
                       (string-prefix-p "M-" description)
                       (member description '("<backtab>" "S-TAB"))))
          (evil-define-key* 'insert map key command))))
    (unless editable
      (when-let* ((quit (lookup-key map (kbd "q"))) ((commandp quit)))
        (evil-define-key* 'normal map (kbd "q") quit)))))

(defun niwa-evil--overlay-map (map)
  "Adapt native overlay MAP without relying on Evil auxiliary map lookup.
Overlay and text-property maps outrank Evil's emulation maps.  Filter the
conflicting native actions, preserving vanilla behavior in other states."
  (unless (gethash map niwa-evil--overlay-maps)
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
                                           (if (niwa-evil--normal-p) prefix command)))))
      (dolist (key '("h" "j" "k" "l" "G"))
        (when-let* ((command (lookup-key map (kbd key)))
                    ((not (keymapp command))))
          (unless (equal key "G")
            (define-key map (kbd (niwa-evil--binding-key key))
                        `(menu-item "" ,command
                                    :filter ,(lambda (action)
                                               (and (niwa-evil--normal-p) action)))))
          (define-key map (kbd key)
                      `(menu-item "" ,command
                                  :filter ,(lambda (action)
                                             (unless (niwa-evil--normal-p) action)))))))
    (puthash map t niwa-evil--overlay-maps))
  map)

(defun niwa-evil--cockpit (&rest _)
  "Adapt the current cockpit's generated map after surface setup."
  (when (memq major-mode niwa-evil--cockpit-modes)
    (niwa-evil--adapt-map (current-local-map))))

(defun niwa-evil--key-help (text)
  "Translate generated cockpit key help TEXT for Evil normal state."
  (if (not (niwa-evil--normal-p)) text
    (mapconcat
     (lambda (line)
       (if (not (string-match "\\`\\([^ \n]+\\) +" line)) line
         (let ((key (match-string 1 line)) (end (match-end 0)))
           (format "%-4s %s" (niwa-evil--binding-key key)
                   (substring line end)))))
     (split-string text "\n") "\n")))

(defun niwa-evil--feature-maps (entry)
  "Install the loaded feature maps listed in ENTRY."
  (dolist (symbol (cdr entry))
    (when (and (boundp symbol) (keymapp (symbol-value symbol)))
      (cond
       ((memq symbol '(niwa-view-mode-map niwa-note-frame-mode-map
                       niwa-view--composer-keymap niwa-view--side-conversation-keymap))
        (niwa-evil--adapt-map (symbol-value symbol) t))
       ((or (string-suffix-p "-mode-map" (symbol-name symbol))
            (eq symbol 'niwa-collaboration--share-map))
        (niwa-evil--adapt-map (symbol-value symbol)))
       (t (niwa-evil--overlay-map (symbol-value symbol)))))))

(defun niwa-evil--fragment-maps (&rest maps)
  "Adapt native fragment MAPS before composition, once per map identity."
  (dolist (map maps)
    (when map (niwa-evil--overlay-map map))))

(defun niwa-evil--insert-composer ()
  "Move past a read-only composer prompt when entering an editing state."
  (when-let* (((derived-mode-p 'niwa-composer-mode))
              (start (niwa-view-composer-draft-start))
              ((< (point) start)))
    (goto-char start)))

(defun niwa-evil--selection (function &rest args)
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
            (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Block selections cannot create or delete instructions"))
          (goto-char (if forward end beg))
          (set-mark (if forward beg end))
          (if args
              (funcall function (car args) (if (eq type 'line) 'line 'char))
            (funcall function)))
      (evil-exit-visual-state)
      (evil-normal-state))))

(defun niwa-evil--replacement (function &rest args)
  "Hold source replacements while FUNCTION with ARGS enters editing state."
  (if (not (bound-and-true-p evil-local-mode))
      (apply function args)
    (let ((buffer (current-buffer)))
      (niwa-instruction-replacement-hold buffer)
      (unwind-protect
          (apply function args)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (unless (memq evil-state '(insert replace))
              (niwa-instruction-replacement-end buffer))))))))

(defun niwa-evil--replacement-end ()
  "Release the current source buffer's Evil replacement hold."
  (niwa-instruction-replacement-end))

(defun niwa-evil--local-mode ()
  "Finalize replacements when disabling Evil."
  (unless evil-local-mode
    (niwa-instruction-replacement-end)))

;;;###autoload
(defun niwa-evil-setup ()
  "Opt in to niwa's Evil keys and selection/editing integration.
Require Evil at call time, without enabling it.  Calling this repeatedly
is harmless.  No evil-collection dependency is needed."
  (interactive)
  (unless (require 'evil nil t)
    (niwa-diagnostic-user-error 'global "Evil is not installed; install Evil before calling niwa-evil-setup"))
  (unless niwa-evil--installed
    (require 'niwa-overlays)
    (setq niwa-evil--installed t)
    (evil-set-initial-state 'niwa-surface-mode 'insert)
    (dolist (mode (append niwa-evil--cockpit-modes
                          '(niwa-view-mode niwa-composer-mode
                            niwa-report-mode niwa-notes-list-mode
                            niwa-worktree-setup-mode)))
      (evil-set-initial-state mode 'normal))
    (dolist (entry niwa-evil--map-features)
      (eval-after-load (car entry)
        `(niwa-evil--feature-maps ',entry)))
    (eval-after-load 'niwa-cockpit
      '(progn
         (advice-add 'niwa-cockpit-setup-tabulated-surface
                     :after #'niwa-evil--cockpit)
         (advice-add 'niwa-cockpit-surface-key-help-text
                     :filter-return #'niwa-evil--key-help)))
    (eval-after-load 'niwa-view
      '(advice-add 'niwa-view--display-fragment-keymap
                   :before #'niwa-evil--fragment-maps))
    (dolist (command '(niwa-delete-instructions))
      (evil-set-command-property command :keep-visual t))
    (advice-add 'niwa-delete-instructions :around #'niwa-evil--selection)
    (dolist (command '(evil-change evil-enter-replace-state))
      (advice-add command :around #'niwa-evil--replacement))
    (add-hook 'evil-insert-state-exit-hook #'niwa-evil--replacement-end)
    (add-hook 'evil-insert-state-entry-hook #'niwa-evil--insert-composer)
    (add-hook 'evil-replace-state-entry-hook #'niwa-evil--insert-composer)
    (add-hook 'evil-replace-state-exit-hook #'niwa-evil--replacement-end)
    (add-hook 'evil-local-mode-hook #'niwa-evil--local-mode)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (niwa-evil--cockpit)))))

(provide 'niwa-evil)
;;; niwa-evil.el ends here
