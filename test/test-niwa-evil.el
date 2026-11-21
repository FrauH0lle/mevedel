;;; test-niwa-evil.el --- Optional Evil integration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Drive actual Evil keys, not calls standing in for modal commands.

;;; Code:

(require 'niwa-notes)

(require 'cl-lib)
(defvar evil-redo-function)
(defvar evil-undo-function)
(defvar evil-want-fine-undo)
(require 'niwa)
(require 'niwa-evil)
(require 'niwa-cockpit)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-control-transfer)
(require 'niwa-side-conversation)
(require 'niwa-report)
(require 'niwa-journal-jobs)
(require 'niwa-note-frame)
(require 'niwa-collaboration-share)
(require 'niwa-subscription-usage)
(require 'niwa-notes-list)
(require 'niwa-window)
(require 'niwa-artifacts-list)
(require 'niwa-background-list)
(require 'niwa-memory-list)
(require 'niwa-pending-inputs)
(require 'niwa-permissions-list)
(require 'niwa-plugin-ui)
(require 'niwa-skills-ui)
(require 'niwa-tools-list)
(require 'niwa-worktree)
(require 'niwa-overlay-ui)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))

(defvar niwa-evil-test--snapshot nil)

(defun niwa-evil-test--setup ()
  "Install integration, saving global maps and Evil state for teardown."
  (require 'evil)
  (setq niwa-evil-test--snapshot
        (list
         (mapcar (lambda (entry)
                   (cons entry (copy-keymap (symbol-value entry))))
                 (append
                  (cl-loop for entry in niwa-evil--map-features
                           append (cl-remove-if-not #'boundp (cdr entry)))
                  niwa--actions-maps))
         (copy-tree after-load-alist)
         (mapcar (lambda (entry)
                   (cons (cdr entry) (copy-sequence (symbol-value (cdr entry)))))
                 (evil-state-property t :modes))
         (mapcar (lambda (command)
                   (cons command (copy-tree (symbol-plist command))))
                 '(niwa-delete-instructions))
         niwa--instruction-command-active
         (copy-sequence emulation-mode-map-alists)))
  (niwa-evil-setup))

(defun niwa-evil-test--teardown ()
  "Remove integration state installed by the test fixture."
  (dolist (pair (car niwa-evil-test--snapshot))
    (set (car pair) (cdr pair)))
  (setq after-load-alist (nth 1 niwa-evil-test--snapshot))
  (dolist (pair (nth 2 niwa-evil-test--snapshot))
    (set (car pair) (cdr pair)))
  (dolist (pair (nth 3 niwa-evil-test--snapshot))
    (setplist (car pair) (cdr pair)))
  (setq niwa--instruction-command-active
        (nth 4 niwa-evil-test--snapshot))
  (setq emulation-mode-map-alists (nth 5 niwa-evil-test--snapshot))
  (advice-remove 'niwa-cockpit-setup-tabulated-surface #'niwa-evil--cockpit)
  (advice-remove 'niwa-cockpit-surface-key-help-text #'niwa-evil--key-help)
  (advice-remove 'niwa-view--display-fragment-keymap #'niwa-evil--fragment-maps)
  (advice-remove 'niwa-delete-instructions #'niwa-evil--selection)
  (dolist (command '(evil-change evil-enter-replace-state))
    (advice-remove command #'niwa-evil--replacement))
  (remove-hook 'evil-insert-state-exit-hook #'niwa-evil--replacement-end)
  (remove-hook 'evil-insert-state-entry-hook #'niwa-evil--insert-composer)
  (remove-hook 'evil-replace-state-entry-hook #'niwa-evil--insert-composer)
  (remove-hook 'evil-replace-state-exit-hook #'niwa-evil--replacement-end)
  (remove-hook 'evil-local-mode-hook #'niwa-evil--local-mode)
  (clrhash niwa-evil--overlay-maps)
  (setq niwa-evil--installed nil
        niwa-evil-test--snapshot nil))

(niwa-deftest niwa-evil-test--fixture-restores-command-active
  (:vars ((niwa--instruction-command-active :outer))
   :quiet t)
  ,test
  (test)
  :doc "restores pre-existing command scope after an aborted keyboard command"
  (unwind-protect
      (progn
        (niwa-evil-test--setup)
        ;; A failed keyboard macro can leave the command scope at depth zero.
        (setq niwa--instruction-command-active 0)
        (niwa-evil-test--teardown)
        (should (eq niwa--instruction-command-active :outer)))
    (when niwa-evil-test--snapshot
      (niwa-evil-test--teardown))))

(defmacro niwa-evil-test--buffer (&rest body)
  "Run BODY in a displayed temporary buffer with local Evil enabled."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (text-mode)
       (buffer-enable-undo)
       (evil-local-mode 1)
       (evil-normal-state)
       (let ((evil-undo-function #'undo-only)
             (evil-redo-function #'undo-redo)
             (evil-want-fine-undo nil))
         (unwind-protect (progn ,@body)
           (evil-local-mode -1)
           (dolist (overlay (copy-sequence
                             (alist-get (current-buffer)
                                        (niwa--instruction-alist))))
             (niwa--delete-instruction overlay (current-buffer))))))))

(defun niwa-evil-test--keys (keys)
  "Execute real KEYS through Emacs's keyboard-macro command loop."
  (execute-kbd-macro (kbd keys)))

(defmacro niwa-evil-test--view (side &rest body)
  "Run BODY in an initialized view, using a side composer when SIDE is non-nil.
Keep the real transcript alive and leave recovery and geometry functions intact."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (let ((data (current-buffer)))
       (niwa-evil-test--buffer
         (niwa-view--setup (current-buffer) data
                              (list :side-conversation-p ,side))
         (evil-local-mode 1)
         ,@body))))

(defun niwa-evil-test--composer-fallback (side state)
  "Check real composer routing in STATE for an initialized SIDE view."
  (evil-change-state state)
  (goto-char (point-min))
  (let ((history-yank (key-binding (kbd "C-y")))
        (draft "`word`\nsecond line")
        (view (current-buffer))
        (view-window (selected-window)))
    (should-not (eq history-yank #'niwa-view-yank-dwim))
    (when (eq state 'normal)
      (should (eq history-yank #'evil-scroll-line-up)))
    (niwa-view-composer-insert draft)
    (with-selected-window (niwa-view-composer-display view-window)
      (should (derived-mode-p 'niwa-composer-mode))
      (evil-local-mode 1)
      (evil-change-state state)
      (let* ((start (niwa-view-composer-draft-start view))
             (word (make-overlay (1+ start) (+ start 5)))
             (map (define-keymap "<mouse-3>" #'ignore "C-c C-v" #'forward-char)))
        (overlay-put word 'keymap map)
        (goto-char (+ start 3))
        (dolist (entry `(("C-c RET" . niwa-view-send)
                         ("C-c TAB" . niwa-view-send-follow-up)
                         ("C-y" . niwa-view-yank-dwim)
                         ("C-c C-k" . ,(if side #'niwa-view-abort
                                          #'niwa-view-cancel-composer-state))))
          (should (eq (key-binding (kbd (car entry))) (cdr entry))))
        (should (eq (key-binding (kbd "C-c C-v")) #'forward-char))
        (should (eq (key-binding [mouse-3]) #'ignore))
        (if side
            (progn
              (should (eq (key-binding (kbd "C-c C-z"))
                          #'niwa-side-conversation-close))
              (dolist (entry '(("C-c C-u" . niwa-view-history-clear-input)
                               ("C-c C-w" . niwa-view-retract-pending)
                               ("M-p" . niwa-view-history-previous)))
                (should-not (eq (key-binding (kbd (car entry))) (cdr entry)))))
          (should-not (eq (key-binding (kbd "C-c C-z"))
                          #'niwa-side-conversation-close))
          (should (eq (key-binding (kbd "M-p")) #'niwa-view-history-previous)))
        (let* ((kill-ring '("PASTE"))
               (kill-ring-yank-pointer kill-ring)
               (interprogram-paste-function nil))
          (niwa-evil-test--keys "C-y"))
        (should (equal (niwa-view-composer-draft-text view t) "`woPASTErd`\nsecond line"))
        ;; The transcript keeps its own bindings even while the draft is active.
        (with-selected-window view-window
          (goto-char (point-min))
          (should (eq (key-binding (kbd "C-y")) history-yank))
          (should-not (eq (key-binding (kbd "C-c RET")) #'niwa-view-send)))
        (goto-char (+ start 2))
        (should (eq (key-binding (kbd "C-y")) #'niwa-view-yank-dwim))
        (unless side
          (niwa-evil-test--keys "C-c C-u")
          (should (equal (niwa-view-composer-draft-text view t) "")))))))

(defun niwa-evil-test--cold-composer (evil-first)
  "Check a genuinely cold composer with Evil setup first when EVIL-FIRST."
  (let ((file (symbol-file 'niwa-evil-test--cold-composer 'defun)))
    (should file)
    (with-temp-buffer
      (let ((process
             (make-process
              :name "niwa-evil-cold" :buffer (current-buffer) :noquery t
              :connection-type 'pipe :sentinel #'ignore
              :command
              (list (expand-file-name invocation-name invocation-directory)
                    "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',load-path
                              native-comp-jit-compilation nil)
                        (require 'ert)
                        (should-not (featurep 'evil))
                        (should-not (featurep 'niwa-view-composer))
                        ,@(when evil-first
                            '((require 'niwa-evil)
                              (niwa-evil-setup)
                              (should-not (featurep 'niwa-view-composer))))
                        (require 'niwa-view)
                        (save-window-excursion
                          (with-temp-buffer
                            (org-mode)
                            (let ((data (current-buffer)))
                              (with-temp-buffer
                                (switch-to-buffer (current-buffer))
                                (niwa-view--setup (current-buffer) data)
                                ,@(unless evil-first
                                    '((should-not (featurep 'evil))
                                      (require 'niwa-evil)
                                      (niwa-evil-setup)))
                                ;; Only load the ordinary test fixtures after
                                ;; the cold installation boundary is exercised.
                                (load ,file nil t)
                                (let (messages)
                                  (niwa-test--with-captured-messages messages
                                    (evil-local-mode 1)
                                    (dolist (state '(normal insert))
                                      (niwa-evil-test--composer-fallback nil state)))
                                  ;; Evil clears its state echo with empty
                                  ;; messages; no other diagnostics are expected.
                                  (should (string-match-p "\\`\n*\\'" messages)))))))))))))
        (unwind-protect
            (progn
              (with-timeout (60 (ert-fail "Cold Evil composer timed out"))
                (while (process-live-p process)
                  (accept-process-output process 0.05)))
              (accept-process-output process 0)
              (ert-info ((buffer-string))
                (should (equal 0 (process-exit-status process)))
                (should (equal "" (buffer-string)))))
          (when (process-live-p process) (delete-process process)))))))

(niwa-deftest niwa-evil-composer-routing
  (:before-each (niwa-evil-test--setup)
   :after-each (niwa-evil-test--teardown)
   :quiet t)
  ,test
  (test)
  :doc "Mouse-only word overlays preserve normal and insert composer fallback"
  (dolist (side '(nil t))
    (dolist (state '(normal insert))
      (ert-info ((format "side=%S state=%S" side state))
        (niwa-evil-test--view side
          (niwa-evil-test--composer-fallback side state)))))
  :doc "Explicit contextual commands beat composer fallback in both Evil states"
  (dolist (side '(nil t))
    (dolist (state '(normal insert))
      (dolist (property '(overlay text))
        (ert-info ((format "side=%S state=%S property=%S" side state property))
          (niwa-evil-test--view side
            (niwa-view-composer-insert "word")
            (let ((view (current-buffer)))
              (with-selected-window (niwa-view-composer-display (selected-window))
                (evil-local-mode 1)
                (evil-change-state state)
                (let* ((start (niwa-view-composer-draft-start view))
                       (map (define-keymap "C-y" #'backward-char
                                          "C-c RET" #'forward-char
                                          "<mouse-3>" #'ignore)))
                  (if (eq property 'overlay)
                      (overlay-put (make-overlay start (niwa-view-composer-draft-end view)) 'keymap map)
                    (put-text-property start (niwa-view-composer-draft-end view) 'keymap map))
                  (goto-char (+ start 2))
                  (should (eq (key-binding (kbd "C-y")) #'backward-char))
                  (should (eq (key-binding (kbd "C-c RET")) #'forward-char))
                  (should (eq (key-binding [mouse-3]) #'ignore))
                  (niwa-evil-test--keys "C-y")
                  (should (= (point) (1+ start)))
                  (niwa-evil-test--keys "C-c RET")
                  (should (= (point) (+ start 2)))
                  (should (equal (niwa-view-composer-draft-text view t) "word"))))))))))

  :doc "Empty prompt controls and literal i/R typing keep root and side semantics"
  (dolist (side '(nil t))
    (dolist (command '("i" "R"))
      (niwa-evil-test--view side
        (let ((view (current-buffer)))
          (with-selected-window (niwa-view-composer-display (selected-window))
            (evil-local-mode 1)
            (let* ((start (niwa-view-composer-draft-start view))
                   (prompt (buffer-substring-no-properties (point-min) start)))
              (dolist (state '(normal insert))
                (evil-change-state state)
                (goto-char (1- start))
                (dolist (entry `(("C-c RET" . niwa-view-send)
                                 ("C-c TAB" . niwa-view-send-follow-up)
                                 ("C-c C-k" . ,(if side #'niwa-view-abort
                                                  #'niwa-view-cancel-composer-state))))
                  (should (eq (key-binding (kbd (car entry))) (cdr entry))))
                (when side
                  (should (eq (key-binding (kbd "C-c C-z"))
                              #'niwa-side-conversation-close))))
              (evil-normal-state)
              (goto-char (1- start))
              (niwa-evil-test--keys (concat command " h j k l g G <escape>"))
              (should (equal (buffer-substring-no-properties (point-min) start) prompt))
              (should (equal (niwa-view-composer-draft-text view t) "hjklgG")))))))))

(niwa-deftest niwa-evil-composer-cold-order ()
  ,test
  (test)
  :doc "Evil setup before the composer preserves real normal/insert routing"
  (niwa-evil-test--cold-composer t)
  :doc "An initialized composer before Evil preserves real normal/insert routing"
  (niwa-evil-test--cold-composer nil))

(defun niwa-evil-test--partial-change (span edge command)
  "Check real COMMAND at EDGE of a multiline reference of shape SPAN.
Compute expected text and geometry from the aggregate text replacement,
independently of the product's primitive change hooks."
  (ert-info ((format "span=%S operator=%S line=%S" span command edge))
    (niwa-evil-test--buffer
      (let* ((original "before\ntwo\nthree\nfour\nafter\n")
             (start (if (eq span 'line) 8 9))
             (finish (if (eq span 'line) 23 21)))
        (insert original)
        (let ((overlay (niwa-instruction-test--create-note-in
                        (current-buffer) start finish span)))
          (goto-char 8)
          (forward-line (pcase edge ('first 0) ('middle 1) ('last 2)))
          (when (and (eq span 'char) (eq edge 'first)) (forward-char))
          (let* ((position (point))
                 (whole-line (member command '("c c" "S")))
                 (beg (if whole-line (line-beginning-position) position))
                 (end (cond
                       (whole-line (line-beginning-position 2))
                       ((member command '("c $" "C")) (line-end-position))
                       ((equal command "R") (min (+ position 3) (line-end-position)))
                       (t (1+ position))))
                 (replacement (if whole-line "NEW\n" "NEW"))
                 (expected (concat (substring original 0 (1- beg))
                                   replacement (substring original (1- end))))
                 (expected-start (min start beg))
                 (expected-end (max (+ beg (length replacement))
                                    (+ finish (- (length replacement) (- end beg))))))
            (cl-labels
                ((assert-state
                  (text from to)
                  (should (eq (overlay-buffer overlay) (current-buffer)))
                  (should (equal (buffer-string) text))
                  (should (equal (list (overlay-start overlay) (overlay-end overlay)
                                      (overlay-get overlay 'niwa-span))
                                 (list from to span)))
                  (should (equal (buffer-substring-no-properties
                                  (overlay-start overlay) (overlay-end overlay))
                                 (substring text (1- from) (1- to))))))
              (assert-state original start finish)
              (setq buffer-undo-list nil)
              (niwa-evil-test--keys (concat command " N E W <escape>"))
              (assert-state expected expected-start expected-end)
              (niwa-evil-test--keys "u")
              (assert-state original start finish)
              (niwa-evil-test--keys "C-r")
              (assert-state expected expected-start expected-end))))))))

(niwa-deftest niwa-evil-setup
  (:before-each (niwa-evil-test--setup)
   :after-each (niwa-evil-test--teardown)
   :vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global))
   :quiet t)
  ,test
  (test)
  :doc "setup is idempotent and does not enable global Evil"
  (let ((global evil-mode)
        (hook (copy-sequence evil-insert-state-exit-hook)))
    (niwa-evil-setup)
    (should (eq global evil-mode))
    (should (equal hook evil-insert-state-exit-hook))
    (should (eq (evil-initial-state 'niwa-surface-mode) 'insert))
    (should (eq (evil-initial-state 'niwa-view-mode) 'normal))
    (should (eq (evil-initial-state 'niwa-worktree-setup-mode) 'normal)))
  :doc "all nine cockpits expose actions while hjkl gg and G remain motions"
  (dolist (spec '((niwa-artifacts-list-mode . niwa-artifacts-list--surface)
                  (niwa-background-list-mode . niwa-background-list--surface)
                  (niwa-memory-list-mode . niwa-memory-list--surface)
                  (niwa-pending-inputs-mode . niwa-pending-inputs--surface)
                  (niwa-permissions-list-mode . niwa-permissions-list--surface)
                  (niwa-plugins-list-mode . niwa-plugins-list--surface)
                  (niwa-skills-list-mode . niwa-skills-list--surface)
                  (niwa-tools-list-mode . niwa-tools-list--surface)
                  (niwa-worktree-list-mode . niwa-worktree-list--surface)))
    (niwa-evil-test--buffer
      (funcall (car spec))
      (evil-normal-state)
      (let ((surface (symbol-value (cdr spec))))
        (dolist (entry (plist-get surface :keys))
          (let* ((key (niwa-evil--binding-key (car entry)))
                 (expected (niwa-cockpit--key-command entry)))
            (should (eq (key-binding (kbd key)) expected)))))
      (dolist (key '("h" "j" "k" "l" "gg" "G"))
        (should (string-prefix-p "evil-" (symbol-name (key-binding (kbd key))))))
      (let ((inhibit-read-only t)) (insert "one\ntwo\nthree\n"))
      (goto-char 1)
      (niwa-evil-test--keys "j")
      (should (= 2 (line-number-at-pos)))
      (niwa-evil-test--keys "G")
      (should (= 4 (line-number-at-pos)))
      (niwa-evil-test--keys "g g")
      (should (= 1 (point)))))
  :doc "generated cockpit help names normal-state remapped actions"
  (niwa-evil-test--buffer
    (let ((help (niwa-cockpit-surface-key-help-text
                 niwa-memory-list--surface)))
      (should (string-match-p "^gr +Refresh" help))
      (should (string-match-p "^J +Inspect pending journal" help))
      (should (string-match-p "^K +Kill running" help)))
    (evil-local-mode -1)
    (should (string-match-p "^g +Refresh"
                            (niwa-cockpit-surface-key-help-text
                             niwa-memory-list--surface))))
  :doc "visual delete uses the explicit range and leaves source text unchanged"
  (niwa-evil-test--buffer
    (insert "abc\ndef\n")
    (let ((overlay (niwa-instruction-test--create-note-in (current-buffer) 1 5)))
      (goto-char 2)
      (local-set-key (kbd "C-c d") #'niwa-delete-instructions)
      (niwa-evil-test--keys "V C-c d")
      (should-not (overlay-buffer overlay))
      (should (equal (buffer-string) "abc\ndef\n"))
      (should (eq evil-state 'normal))))
  :doc "view composer preserves C-c controls and literal insert typing"
  (niwa-evil-test--view nil
    (with-selected-window (niwa-view-composer-display (selected-window))
      (evil-local-mode 1)
      (should (eq evil-state 'normal))
      (goto-char (point-max))
      (dolist (state '(normal insert))
        (evil-change-state state)
        (dolist (entry '(("C-c C-w" . niwa-view-retract-pending)
                         ("C-c C-r" . niwa-view-resume-pending)
                         ("C-c RET" . niwa-view-send)
                         ("C-c TAB" . niwa-view-send-follow-up)))
          (should (eq (key-binding (kbd (car entry))) (cdr entry))))
        (let (calls)
          (cl-letf (((symbol-function 'niwa-view-retract-pending)
                     (lambda () (interactive) (push 'retract calls)))
                    ((symbol-function 'niwa-view-resume-pending)
                     (lambda () (interactive) (push 'resume calls))))
            (niwa-evil-test--keys "C-c C-w C-c C-r")
            (should (equal (list state (nreverse calls))
                           (list state '(retract resume)))))))
      (niwa-evil-test--keys "h j k l g G <escape>")
      (should (equal "hjklgG" (niwa-view-composer-draft-text nil t)))))
  :doc "empty side composer keeps abort and close controls in both states"
  (niwa-evil-test--view t
    (with-selected-window (niwa-view-composer-display (selected-window))
      (evil-local-mode 1)
      (goto-char (point-max))
      (dolist (state '(normal insert))
        (evil-change-state state)
        (let (calls)
          (cl-letf (((symbol-function 'niwa-view-abort)
                     (lambda () (interactive) (push 'abort calls)))
                    ((symbol-function 'niwa-side-conversation-close)
                     (lambda () (interactive) (push 'close calls))))
            (niwa-evil-test--keys "C-c C-k C-c C-z")
            (should (equal (nreverse calls) '(abort close))))))))
  :doc "i and R enter an empty root or side composer without editing the prompt"
  (dolist (side '(nil t))
    (dolist (command '("i" "R"))
      (niwa-evil-test--view side
        (let ((view (current-buffer)))
          (with-selected-window (niwa-view-composer-display (selected-window))
            (evil-local-mode 1)
            (let* ((start (niwa-view-composer-draft-start view))
                   (prompt (buffer-substring-no-properties 1 start)))
              (goto-char (1- start))
              (niwa-evil-test--keys (concat command " X <escape>"))
              (should (equal (buffer-substring-no-properties 1 start) prompt))
              (should (equal (niwa-view-composer-draft-text view t) "X"))))))))

  :doc "pending summary text-property keys execute instead of Evil delete"
  (niwa-evil-test--buffer
    (niwa-view-mode)
    (insert (propertize "pending entry\n" 'keymap niwa-view--pending-inputs-map))
    (goto-char 1)
    (let (calls)
      (cl-letf (((symbol-function 'niwa-pending-inputs-drop-at-point)
                 (lambda () (interactive) (push 'drop calls)))
                ((symbol-function 'niwa-pending-inputs-preview-activate)
                 (lambda () (interactive) (push 'preview calls))))
        (niwa-evil-test--keys "d DEL RET")
        (should (equal (nreverse calls) '(drop drop preview)))
        (should (equal (buffer-string) "pending entry\n")))))
  :doc "dynamic interaction fragment actions preserve gg G and hjkl"
  (niwa-evil-test--buffer
    (niwa-view-mode)
    (evil-normal-state)
    (let* ((calls 0)
           (action (lambda () (interactive) (cl-incf calls)))
           (map (niwa-view--display-fragment-keymap
                 (define-keymap "g" action "k" action))))
      (insert (propertize "card\nsecond\n" 'keymap map))
      (goto-char 1)
      (niwa-evil-test--keys "g r K")
      (should (= calls 2))
      (niwa-evil-test--keys "j g g")
      (should (= (point) 1))
      (should (= calls 2))
      (evil-local-mode -1)
      (niwa-evil-test--keys "g k")
      (should (= calls 4))))
  :doc "pending cockpit d u x and gr execute the existing action commands"
  (niwa-evil-test--buffer
    (niwa-pending-inputs-mode)
    (evil-normal-state)
    (let (calls)
      (cl-letf (((symbol-function 'niwa-pending-inputs-mark-delete)
                 (lambda () (interactive) (push 'mark calls)))
                ((symbol-function 'niwa-pending-inputs-unmark)
                 (lambda () (interactive) (push 'unmark calls)))
                ((symbol-function 'niwa-pending-inputs-execute-deletions)
                 (lambda () (interactive) (push 'execute calls)))
                ((symbol-function 'niwa-pending-inputs-refresh)
                 (lambda () (interactive) (push 'refresh calls))))
        (niwa-evil-test--keys "d u x g r")
        (should (equal (nreverse calls) '(mark unmark execute refresh))))))
  :doc "repeated control-transfer composition keeps remapped and vanilla actions"
  (niwa-evil-test--buffer
    (niwa-view-mode)
    (evil-normal-state)
    (insert "card\nsecond\n")
    (let (calls)
      (cl-letf (((symbol-function 'niwa-view-control-transfer-grant)
                 (lambda () (interactive) (push 'grant calls)))
                ((symbol-function 'niwa-view-control-transfer-keep)
                 (lambda () (interactive) (push 'keep calls))))
        (dotimes (_ 2)
          (add-text-properties
           1 (point-max)
           (list 'keymap (niwa-view--display-fragment-keymap
                          niwa-view--control-transfer-map)))
          (goto-char 1)
          (niwa-evil-test--keys "g r K j g g")
          (should (= (point) 1)))
        (should (equal (nreverse calls) '(grant keep grant keep)))
        (setq calls nil)
        (evil-local-mode -1)
        (niwa-evil-test--keys "g k")
        (should (equal (nreverse calls) '(grant keep))))))
  :doc "note frame prefixed close works in normal and insert state"
  (niwa-evil-test--buffer
    (niwa-note-frame-mode 1)
    (dolist (state '(normal insert))
      (evil-change-state state)
      (let (calls)
        (cl-letf (((symbol-function 'niwa-note-frame-close)
                   (lambda () (interactive) (push 'close calls))))
          (niwa-evil-test--keys "C-c C-z")
          (should (equal (nreverse calls) '(close)))))))
  :doc "collaboration share local map actions beat normal operators"
  (niwa-evil-test--buffer
    (use-local-map niwa-collaboration--share-map)
    (let (calls)
      (cl-letf (((symbol-function 'niwa-collaboration-share-toggle)
                 (lambda () (interactive) (push 'toggle calls)))
                ((symbol-function 'niwa-collaboration-share-copy-view)
                 (lambda () (interactive) (push 'view calls)))
                ((symbol-function 'niwa-collaboration-share-copy-full)
                 (lambda () (interactive) (push 'full calls)))
                ((symbol-function 'niwa-collaboration-share-copy-owner)
                 (lambda () (interactive) (push 'owner calls)))
                ((symbol-function 'niwa-collaboration-share-quit)
                 (lambda () (interactive) (push 'quit calls))))
        (niwa-evil-test--keys "TAB c f o q")
        (should (equal (nreverse calls) '(toggle view full owner quit))))))
  :doc "reports and journal help descendants use gr RET and q without motion conflicts"
  (dolist (mode '(niwa-report-mode niwa-journal-jobs-mode
                  niwa-subscription-usage-mode))
    (niwa-evil-test--buffer
      (funcall mode)
      (evil-normal-state)
      (let (calls)
        (cl-letf (((symbol-function 'niwa-report-refresh)
                   (lambda () (interactive) (push 'refresh calls)))
                  ((symbol-function 'niwa-journal-jobs)
                   (lambda () (interactive) (push 'refresh calls)))
                  ((symbol-function 'niwa-journal-inspect)
                   (lambda () (interactive) (push 'activate calls)))
                  ((symbol-function 'niwa-subscription-usage-refresh)
                   (lambda () (interactive) (push 'refresh calls)))
                  ((symbol-function 'push-button)
                   (lambda (&rest _) (interactive) (push 'activate calls)))
                  ((symbol-function 'niwa-report-quit)
                   (lambda () (interactive) (push 'quit calls))))
          (niwa-evil-test--keys "g r RET q")
          (should (equal (nreverse calls) '(refresh activate quit)))))))
  :doc "notes list uses gr, ?, x and q in normal state"
  (niwa-evil-test--buffer
    (niwa-notes-list-mode)
    (evil-normal-state)
    (should (commandp #'niwa-window-quit))
    (let (calls)
      (cl-letf (((symbol-function 'niwa-notes-list-refresh)
                 (lambda () (interactive) (push 'refresh calls)))
                ((symbol-function 'describe-mode)
                 (lambda (&rest _) (interactive) (push 'help calls)))
                ((symbol-function 'niwa-notes-list-done)
                 (lambda () (interactive) (push 'done calls)))
                ((symbol-function 'niwa-window-quit)
                 (lambda (&rest _) (interactive) (push 'quit calls))))
        (niwa-evil-test--keys "g r ? x q")
        (should (equal (nreverse calls) '(refresh help done quit)))))))

(niwa-deftest niwa-evil--replacement
  (:before-each (niwa-evil-test--setup)
   :after-each (niwa-evil-test--teardown)
   :vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global))
   :quiet t)
  ,test
  (test)
  :doc "cc retains the first line of a two-line note"
  (niwa-evil-test--buffer
    (insert "one\ntwo\nthree\nfour\n")
    (goto-char 5)
    (let ((overlay (niwa-instruction-test--create-note-in
                    (current-buffer) 5 15 'line)))
      (should (equal (buffer-substring-no-properties
                      (overlay-start overlay) (overlay-end overlay))
                     "two\nthree\n"))
      (goto-char 5)
      (setq buffer-undo-list nil)
      (niwa-evil-test--keys "c c T W O <escape>")
      (should (equal (buffer-string) "one\nTWO\nthree\nfour\n"))
      (should (equal (list (overlay-start overlay) (overlay-end overlay)
                           (overlay-get overlay 'niwa-span))
                     '(5 15 line)))
      (should (equal (buffer-substring-no-properties
                      (overlay-start overlay) (overlay-end overlay))
                     "TWO\nthree\n"))
      (niwa-evil-test--keys "u")
      (should (equal (buffer-string) "one\ntwo\nthree\nfour\n"))
      (should (equal (list (overlay-start overlay) (overlay-end overlay)
                           (overlay-get overlay 'niwa-span))
                     '(5 15 line)))
      (niwa-evil-test--keys "C-r")
      (should (equal (buffer-string) "one\nTWO\nthree\nfour\n"))
      (should (equal (list (overlay-start overlay) (overlay-end overlay)
                           (overlay-get overlay 'niwa-span))
                     '(5 15 line)))))
  :doc "c cc s S C and R preserve complete replacements in both span shapes"
  (dolist (span '(char line))
    (dolist (command '("c l" "c c" "s" "S" "C" "R" "v c"))
      (niwa-evil-test--buffer
        (insert "before\nx\nafter\n")
        (let ((overlay (niwa-instruction-test--create-note-in
                        (current-buffer) 8 (if (eq span 'line) 10 9) span)))
          (goto-char 8)
          (setq buffer-undo-list nil)
          (niwa-evil-test--keys (concat command " N E W <escape>"))
          (should (overlay-buffer overlay))
          (should (eq (overlay-get overlay 'niwa-span) span))
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (or (eq span 'line) (member command '("c c" "S")))
                             "NEW\n" "NEW")))
          (should (eq evil-state 'normal))
          (niwa-evil-test--keys "u")
          (should (equal (buffer-string) "before\nx\nafter\n"))
          (should (= (overlay-start overlay) 8))
          (should (= (overlay-end overlay) (if (eq span 'line) 10 9)))
          (niwa-evil-test--keys "C-r")
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (or (eq span 'line) (member command '("c c" "S")))
                             "NEW\n" "NEW")))))))
  :doc "dd retains an empty note and u C-r restore source geometry"
  (dolist (span '(char line))
    (niwa-evil-test--buffer
      (insert "before\nx\nafter\n")
      (let ((overlay (niwa-instruction-test--create-note-in
                      (current-buffer) 8 (if (eq span 'line) 10 9) span)))
        (goto-char 8)
        (setq buffer-undo-list nil)
        (niwa-evil-test--keys "d d")
        (should (eq (current-buffer) (overlay-buffer overlay)))
        (should (= 8 (overlay-start overlay) (overlay-end overlay)))
        (niwa-evil-test--keys "u")
        (should (= (overlay-start overlay) 8))
        (should (= (overlay-end overlay) (if (eq span 'line) 10 9)))
        (niwa-evil-test--keys "C-r")
        (should (eq (current-buffer) (overlay-buffer overlay)))
        (should (= 8 (overlay-start overlay) (overlay-end overlay))))))
  :doc "o O add adjacent lines without absorbing them into either span"
  (dolist (span '(char line))
    (dolist (command '("o" "O"))
      (niwa-evil-test--buffer
        (insert "before\nx\nafter\n")
        (let ((overlay (niwa-instruction-test--create-note-in
                        (current-buffer) 8 (if (eq span 'line) 10 9) span)))
          (goto-char 8)
          (niwa-evil-test--keys (concat command " N E W <escape>"))
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (eq span 'line) "x\n" "x")))))))
  :doc "J joins the next line into a line span but not a character span"
  (dolist (span '(char line))
    (niwa-evil-test--buffer
      (insert "before\nx\nafter\n")
      (let ((overlay (niwa-instruction-test--create-note-in
                      (current-buffer) 8 (if (eq span 'line) 10 9) span)))
        (goto-char 8)
        (niwa-evil-test--keys "J")
        (should (equal (buffer-substring-no-properties
                        (overlay-start overlay) (overlay-end overlay))
                       (if (eq span 'line) "x after" "x"))))))
  :doc "linewise p P add adjacent lines without absorbing them"
  (dolist (span '(char line))
    (dolist (command '("p" "P"))
      (niwa-evil-test--buffer
        (insert "NEW\nx\nafter\n")
        (let ((overlay (niwa-instruction-test--create-note-in
                        (current-buffer) 5 (if (eq span 'line) 7 6) span)))
          (goto-char 1)
          (niwa-evil-test--keys (concat "y y j " command))
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (eq span 'line) "x\n" "x")))))))
  :doc "dot repeats a real change session on a second reference"
  (dolist (span '(char line))
    (niwa-evil-test--buffer
      (insert "before\nx\nx\nafter\n")
      (let ((first (niwa-instruction-test--create-note-in
                    (current-buffer) 8 (if (eq span 'line) 10 9) span))
            (second (niwa-instruction-test--create-note-in
                     (current-buffer) 10 (if (eq span 'line) 12 11) span)))
        (goto-char 8)
        (niwa-evil-test--keys "c l N E W <escape>")
        (goto-char (overlay-start second))
        (niwa-evil-test--keys ".")
        (dolist (overlay (list first second))
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (eq span 'line) "NEW\n" "NEW")))))))
  :doc "disabling local Evil finalizes a held empty character change"
  (niwa-evil-test--buffer
    (insert "before\nx\nafter\n")
    (let ((overlay (niwa-instruction-test--create-note-in (current-buffer) 8 9 'char)))
      (goto-char 8)
      (niwa-evil-test--keys "c l")
      (should niwa--instruction-replacement-held)
      (evil-local-mode -1)
      (should-not niwa--instruction-replacement-held)
      (should (eq (current-buffer) (overlay-buffer overlay)))
      (should (= 8 (overlay-start overlay) (overlay-end overlay)))))
  :doc "an aborted real change command releases its replacement scope"
  (niwa-evil-test--buffer
    (insert "before\nx\nafter\n")
    (niwa-instruction-test--create-note-in (current-buffer) 8 9 'char)
    (goto-char 8)
    (let ((buffer-read-only t))
      (should-error (niwa-evil-test--keys "c l")))
    (should-not niwa--instruction-replacement-held))
  :doc "fine undo follows Evil grouping and eventually restores both span shapes"
  (dolist (span '(char line))
    (niwa-evil-test--buffer
      (let ((evil-want-fine-undo t))
        (insert "before\nx\nafter\n")
        (let ((overlay (niwa-instruction-test--create-note-in
                        (current-buffer) 8 (if (eq span 'line) 10 9) span)))
          (goto-char 8)
          (setq buffer-undo-list nil)
          (niwa-evil-test--keys "c l N E W <escape>")
          (let ((steps 0))
            (while (and (not (equal (buffer-string) "before\nx\nafter\n"))
                        (< steps 10))
              (niwa-evil-test--keys "u")
              (cl-incf steps))
            (should (equal (buffer-string) "before\nx\nafter\n"))
            (should (= (overlay-start overlay) 8))
            (should (= (overlay-end overlay) (if (eq span 'line) 10 9)))
            (dotimes (_ steps) (niwa-evil-test--keys "C-r"))
            (should (equal (buffer-substring-no-properties
                            (overlay-start overlay) (overlay-end overlay))
                           (if (eq span 'line) "NEW\n" "NEW")))))))))

(niwa-deftest niwa-evil--partial-replacement
  (:before-each (niwa-evil-test--setup)
   :after-each (niwa-evil-test--teardown)
   :vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global))
   :quiet t
   :doc "multiline partial replacement keeps exact text and span through undo/redo")
  (niwa-evil-test--partial-change ,span ,edge ,command)
  (span edge command)
  'line 'first "c c"
  'line 'middle "c c"
  'line 'last "c c"
  'line 'first "S"
  'line 'middle "S"
  'line 'last "S"
  'line 'first "c $"
  'line 'middle "c $"
  'line 'last "c $"
  'line 'first "C"
  'line 'middle "C"
  'line 'last "C"
  'line 'first "s"
  'line 'middle "s"
  'line 'last "s"
  'line 'first "R"
  'line 'middle "R"
  'line 'last "R"
  'line 'first "c l"
  'line 'middle "c l"
  'line 'last "c l"
  'char 'first "c c"
  'char 'middle "c c"
  'char 'last "c c"
  'char 'first "S"
  'char 'middle "S"
  'char 'last "S"
  'char 'first "c $"
  'char 'middle "c $"
  'char 'last "c $"
  'char 'first "C"
  'char 'middle "C"
  'char 'last "C"
  'char 'first "s"
  'char 'middle "s"
  'char 'last "s"
  'char 'first "R"
  'char 'middle "R"
  'char 'last "R"
  'char 'first "c l"
  'char 'middle "c l"
  'char 'last "c l")

(niwa-deftest niwa-evil--cold-loading ()
  ,test
  (test)
  :doc "entrypoint and optional library cold-load without Evil or side effects"
  (with-temp-buffer
    (let ((form `(progn
                   (setq load-path ',load-path)
                   (require 'niwa)
                   (unless (autoloadp (symbol-function 'niwa-evil-setup))
                     (error "Missing Evil setup autoload"))
                   (require 'niwa-evil)
                   (when (or (featurep 'evil) niwa-evil--installed)
                     (error "Loading activated Evil")))))
      (should (= 0 (call-process invocation-name nil t nil
                                 "-Q" "--batch" "--eval"
                                 (prin1-to-string form))))))
  :doc "missing Evil fails clearly and leaves setup inactive"
  (with-temp-buffer
    (let ((form `(progn
                   (setq load-path ',load-path)
                   (require 'niwa-evil)
                   (let ((original (symbol-function 'require)) raised)
                     (cl-letf (((symbol-function 'require)
                                (lambda (feature &rest args)
                                  (unless (eq feature 'evil)
                                    (apply original feature args)))))
                       (condition-case err (niwa-evil-setup)
                         (user-error
                          (setq raised t)
                          (unless (string-match-p "Evil is not installed"
                                                  (error-message-string err))
                            (signal (car err) (cdr err))))))
                     (unless raised (error "Missing dependency error was not raised")))
                   (when niwa-evil--installed (error "Partial setup")))))
      (should (= 0 (call-process invocation-name nil t nil
                                 "-Q" "--batch" "--eval"
                                 (prin1-to-string form)))))))

(provide 'test-niwa-evil)
;;; test-niwa-evil.el ends here
