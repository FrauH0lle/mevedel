;;; test-mevedel-evil.el --- Optional Evil integration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Drive actual Evil keys, not calls standing in for modal commands.

;;; Code:

(require 'cl-lib)
(defvar evil-redo-function)
(defvar evil-undo-function)
(defvar evil-want-fine-undo)
(require 'mevedel)
(require 'mevedel-evil)
(require 'mevedel-cockpit)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-control-transfer)
(require 'mevedel-side-conversation)
(require 'mevedel-report)
(require 'mevedel-journal-jobs)
(require 'mevedel-directive-activity)
(require 'mevedel-directive-frame)
(require 'mevedel-collaboration-share)
(require 'mevedel-subscription-usage)
(require 'mevedel-artifacts-list)
(require 'mevedel-background-list)
(require 'mevedel-memory-list)
(require 'mevedel-pending-inputs)
(require 'mevedel-permissions-list)
(require 'mevedel-plugin-ui)
(require 'mevedel-skills-ui)
(require 'mevedel-tools-list)
(require 'mevedel-worktree)
(require 'mevedel-overlay-ui)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar mevedel-evil-test--snapshot nil)

(defun mevedel-evil-test--setup ()
  "Install integration, saving global maps and Evil state for teardown."
  (require 'evil)
  (setq mevedel-evil-test--snapshot
        (list
         (mapcar (lambda (entry)
                   (cons entry (copy-keymap (symbol-value entry))))
                 (append
                  (cl-loop for entry in mevedel-evil--map-features
                           append (cl-remove-if-not #'boundp (cdr entry)))
                  mevedel--actions-maps))
         (copy-tree after-load-alist)
         (mapcar (lambda (entry)
                   (cons (cdr entry) (copy-sequence (symbol-value (cdr entry)))))
                 (evil-state-property t :modes))
         (mapcar (lambda (command)
                   (cons command (copy-tree (symbol-plist command))))
                 '(mevedel-create-reference mevedel-create-directive
                   mevedel-delete-instructions))
         mevedel--instruction-command-active))
  (mevedel-evil-setup))

(defun mevedel-evil-test--teardown ()
  "Remove integration state installed by the test fixture."
  (dolist (pair (car mevedel-evil-test--snapshot))
    (set (car pair) (cdr pair)))
  (setq after-load-alist (nth 1 mevedel-evil-test--snapshot))
  (dolist (pair (nth 2 mevedel-evil-test--snapshot))
    (set (car pair) (cdr pair)))
  (dolist (pair (nth 3 mevedel-evil-test--snapshot))
    (setplist (car pair) (cdr pair)))
  (setq mevedel--instruction-command-active
        (nth 4 mevedel-evil-test--snapshot))
  (advice-remove 'mevedel-cockpit-setup-tabulated-surface #'mevedel-evil--cockpit)
  (advice-remove 'mevedel-cockpit-surface-key-help-text #'mevedel-evil--key-help)
  (advice-remove 'mevedel-view--display-fragment-keymap #'mevedel-evil--fragment-maps)
  (advice-remove 'mevedel--create-instruction #'mevedel-evil--selection)
  (advice-remove 'mevedel-delete-instructions #'mevedel-evil--selection)
  (dolist (command '(evil-change evil-enter-replace-state))
    (advice-remove command #'mevedel-evil--replacement))
  (remove-hook 'evil-insert-state-exit-hook #'mevedel-evil--replacement-end)
  (remove-hook 'evil-insert-state-entry-hook #'mevedel-evil--insert-composer)
  (remove-hook 'evil-replace-state-entry-hook #'mevedel-evil--insert-composer)
  (remove-hook 'evil-replace-state-exit-hook #'mevedel-evil--replacement-end)
  (remove-hook 'evil-local-mode-hook #'mevedel-evil--local-mode)
  (clrhash mevedel-evil--overlay-maps)
  (setq mevedel-evil--installed nil
        mevedel-evil-test--snapshot nil))

(mevedel-deftest mevedel-evil-test--fixture-restores-command-active
  (:vars ((mevedel--instruction-command-active :outer))
   :quiet t)
  ,test
  (test)
  :doc "restores pre-existing command scope after an aborted keyboard command"
  (unwind-protect
      (progn
        (mevedel-evil-test--setup)
        ;; A failed keyboard macro can leave the command scope at depth zero.
        (setq mevedel--instruction-command-active 0)
        (mevedel-evil-test--teardown)
        (should (eq mevedel--instruction-command-active :outer)))
    (when mevedel-evil-test--snapshot
      (mevedel-evil-test--teardown))))

(defmacro mevedel-evil-test--buffer (&rest body)
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
                                        (mevedel--instruction-alist))))
             (mevedel--delete-instruction overlay (current-buffer))))))))

(defun mevedel-evil-test--keys (keys)
  "Execute real KEYS through Emacs's keyboard-macro command loop."
  (execute-kbd-macro (kbd keys)))

(defmacro mevedel-evil-test--file-buffer (text &rest body)
  "Run BODY in a real visited temporary source file containing TEXT."
  (declare (indent 1) (debug t))
  `(let ((file (make-temp-file "mevedel-evil-source-" nil ".txt" ,text)))
     (unwind-protect
         (mevedel-evil-test--buffer
           (insert ,text)
           (setq-local buffer-file-name file
                       mevedel--workspace
                       (mevedel-workspace--create
                        :id file :root (file-name-directory file) :type 'file))
           ,@body)
       (unlock-file file)
       (delete-file file))))

(defun mevedel-evil-test--partial-change (span edge command)
  "Check real COMMAND at EDGE of a multiline reference of shape SPAN.
Compute expected text and geometry from the aggregate text replacement,
independently of the product's primitive change hooks."
  (ert-info ((format "span=%S operator=%S line=%S" span command edge))
    (mevedel-evil-test--buffer
      (let* ((original "before\ntwo\nthree\nfour\nafter\n")
             (start (if (eq span 'line) 8 9))
             (finish (if (eq span 'line) 23 21)))
        (insert original)
        (let ((overlay (mevedel--create-reference-in
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
                                      (overlay-get overlay 'mevedel-span))
                                 (list from to span)))
                  (should (equal (buffer-substring-no-properties
                                  (overlay-start overlay) (overlay-end overlay))
                                 (substring text (1- from) (1- to))))))
              (assert-state original start finish)
              (setq buffer-undo-list nil)
              (mevedel-evil-test--keys (concat command " N E W <escape>"))
              (assert-state expected expected-start expected-end)
              (mevedel-evil-test--keys "u")
              (assert-state original start finish)
              (mevedel-evil-test--keys "C-r")
              (assert-state expected expected-start expected-end))))))))

(mevedel-deftest mevedel-evil-setup
  (:before-each (mevedel-evil-test--setup)
   :after-each (mevedel-evil-test--teardown)
   :vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global))
   :quiet t)
  ,test
  (test)
  :doc "setup is idempotent and does not enable global Evil"
  (let ((global evil-mode)
        (hook (copy-sequence evil-insert-state-exit-hook)))
    (mevedel-evil-setup)
    (should (eq global evil-mode))
    (should (equal hook evil-insert-state-exit-hook))
    (should (eq (evil-initial-state 'mevedel-surface-mode) 'insert))
    (should (eq (evil-initial-state 'mevedel-view-mode) 'normal))
    (should (eq (evil-initial-state 'mevedel-worktree-setup-mode) 'normal)))
  :doc "all nine cockpits expose actions while hjkl gg and G remain motions"
  (dolist (spec '((mevedel-artifacts-list-mode . mevedel-artifacts-list--surface)
                  (mevedel-background-list-mode . mevedel-background-list--surface)
                  (mevedel-memory-list-mode . mevedel-memory-list--surface)
                  (mevedel-pending-inputs-mode . mevedel-pending-inputs--surface)
                  (mevedel-permissions-list-mode . mevedel-permissions-list--surface)
                  (mevedel-plugins-list-mode . mevedel-plugins-list--surface)
                  (mevedel-skills-list-mode . mevedel-skills-list--surface)
                  (mevedel-tools-list-mode . mevedel-tools-list--surface)
                  (mevedel-worktree-list-mode . mevedel-worktree-list--surface)))
    (mevedel-evil-test--buffer
      (funcall (car spec))
      (evil-normal-state)
      (let ((surface (symbol-value (cdr spec))))
        (dolist (entry (plist-get surface :keys))
          (let* ((key (mevedel-evil--binding-key (car entry)))
                 (expected (mevedel-cockpit--key-command entry)))
            (should (eq (key-binding (kbd key)) expected)))))
      (dolist (key '("h" "j" "k" "l" "gg" "G"))
        (should (string-prefix-p "evil-" (symbol-name (key-binding (kbd key))))))
      (let ((inhibit-read-only t)) (insert "one\ntwo\nthree\n"))
      (goto-char 1)
      (mevedel-evil-test--keys "j")
      (should (= 2 (line-number-at-pos)))
      (mevedel-evil-test--keys "G")
      (should (= 4 (line-number-at-pos)))
      (mevedel-evil-test--keys "g g")
      (should (= 1 (point)))))
  :doc "visual v and V supply inclusive character and explicit line spans"
  (dolist (spec '(("v l" char 2 4) ("V" line 1 5)
                  ("0 v $" char 1 5)))
    (mevedel-evil-test--buffer
      (insert "abc\ndef\n")
      (goto-char 2)
      (local-set-key (kbd "C-c r") #'mevedel-create-reference)
      (mevedel-evil-test--keys (concat (car spec) " C-c r RET"))
      (let ((overlay (car (mevedel--instructions-in 1 (point-max)))))
        (should (eq (overlay-get overlay 'mevedel-span) (nth 1 spec)))
        (should (= (overlay-start overlay) (nth 2 spec)))
        (should (= (overlay-end overlay) (nth 3 spec))))
      (should (eq evil-state 'normal))
      (should-not (region-active-p))))
  :doc "generated cockpit help names normal-state remapped actions"
  (mevedel-evil-test--buffer
    (let ((help (mevedel-cockpit-surface-key-help-text
                 mevedel-memory-list--surface)))
      (should (string-match-p "^gr +Refresh" help))
      (should (string-match-p "^J +Inspect pending journal" help))
      (should (string-match-p "^K +Kill running" help)))
    (evil-local-mode -1)
    (should (string-match-p "^g +Refresh"
                            (mevedel-cockpit-surface-key-help-text
                             mevedel-memory-list--surface))))
  :doc "block creation fails without leaving a stale visual selection"
  (mevedel-evil-test--buffer
    (insert "abc\ndef\n")
    (goto-char 1)
    (mevedel-evil-test--keys "C-v j")
    (should-error (call-interactively #'mevedel-create-reference) :type 'user-error)
    (should (eq evil-state 'normal))
    (should-not (region-active-p)))
  :doc "visual delete uses the explicit range and leaves source text unchanged"
  (mevedel-evil-test--buffer
    (insert "abc\ndef\n")
    (let ((overlay (mevedel--create-reference-in (current-buffer) 1 5)))
      (goto-char 2)
      (local-set-key (kbd "C-c d") #'mevedel-delete-instructions)
      (mevedel-evil-test--keys "V C-c d")
      (should-not (overlay-buffer overlay))
      (should (equal (buffer-string) "abc\ndef\n"))
      (should (eq evil-state 'normal))))
  :doc "view composer preserves C-c controls and literal insert typing"
  (mevedel-evil-test--buffer
    (with-temp-buffer
      (org-mode)
      (mevedel-view--setup (window-buffer) (current-buffer)))
    (evil-normal-state)
    (goto-char (point-max))
    (dolist (state '(normal insert))
      (evil-change-state state)
      (dolist (entry '(("C-c C-w" . mevedel-view-retract-pending)
                       ("C-c C-r" . mevedel-view-resume-pending)
                       ("C-c RET" . mevedel-view-send)
                       ("C-c TAB" . mevedel-view-send-follow-up)))
        (should (eq (key-binding (kbd (car entry))) (cdr entry))))
      (let (calls)
        (cl-letf (((symbol-function 'mevedel-view-retract-pending)
                   (lambda () (interactive) (push 'retract calls)))
                  ((symbol-function 'mevedel-view-resume-pending)
                   (lambda () (interactive) (push 'resume calls))))
          (mevedel-evil-test--keys "C-c C-w C-c C-r")
          (should (equal (list state (nreverse calls))
                         (list state '(retract resume)))))))
    (let ((inhibit-read-only t))
      (mevedel-evil-test--keys "h j k l g G <escape>"))
    (should (string-suffix-p "hjklgG" (buffer-string))))
  :doc "source RET dispatch is normal-only and insert RET remains newline"
  (mevedel-evil-test--buffer
    (insert "abc\ndef\n")
    (mevedel--create-reference-in (current-buffer) 1 4 'char)
    (goto-char 2)
    (let ((calls 0))
      (cl-letf (((symbol-function 'mevedel--ov-actions-dispatch)
                 (lambda () (interactive) (cl-incf calls))))
        (mevedel-evil-test--keys "RET")
        (should (= calls 1))
        (mevedel-evil-test--keys "i RET <escape>")
        (should (= calls 1))
        (should (equal (buffer-string) "a\nbc\ndef\n")))))
  :doc "empty side composer keeps abort and close controls in both states"
  (mevedel-evil-test--buffer
    (with-temp-buffer
      (org-mode)
      (mevedel-view--setup (window-buffer) (current-buffer)
                           '(:side-conversation-p t)))
    (goto-char (point-max))
    (dolist (state '(normal insert))
      (evil-change-state state)
      (let (calls)
        (cl-letf (((symbol-function 'mevedel-view-abort)
                   (lambda () (interactive) (push 'abort calls)))
                  ((symbol-function 'mevedel-side-conversation-close)
                   (lambda () (interactive) (push 'close calls))))
          (mevedel-evil-test--keys "C-c C-k C-c C-z")
          (should (equal (nreverse calls) '(abort close)))))))
  :doc "i and R enter an empty root or side composer without editing the prompt"
  (dolist (side '(nil t))
    (dolist (command '("i" "R"))
      (mevedel-evil-test--buffer
        (with-temp-buffer
          (org-mode)
          (mevedel-view--setup (window-buffer) (current-buffer)
                               (list :side-conversation-p side)))
        (evil-normal-state)
        (let* ((start (mevedel-view--input-start))
               (prompt (buffer-substring-no-properties 1 start)))
          (goto-char (1- start))
          (mevedel-evil-test--keys (concat command " X <escape>"))
          (should (equal (buffer-substring-no-properties 1 start) prompt))
          (should (equal (mevedel-view--input-text) "X"))))))
  :doc "pending summary text-property keys execute instead of Evil delete"
  (mevedel-evil-test--buffer
    (mevedel-view-mode)
    (insert (propertize "pending entry\n" 'keymap mevedel-view--pending-inputs-map))
    (goto-char 1)
    (let (calls)
      (cl-letf (((symbol-function 'mevedel-pending-inputs-drop-at-point)
                 (lambda () (interactive) (push 'drop calls)))
                ((symbol-function 'mevedel-pending-inputs-preview-activate)
                 (lambda () (interactive) (push 'preview calls))))
        (mevedel-evil-test--keys "d DEL RET")
        (should (equal (nreverse calls) '(drop drop preview)))
        (should (equal (buffer-string) "pending entry\n")))))
  :doc "dynamic interaction fragment actions preserve gg G and hjkl"
  (mevedel-evil-test--buffer
    (mevedel-view-mode)
    (evil-normal-state)
    (let* ((calls 0)
           (action (lambda () (interactive) (cl-incf calls)))
           (map (mevedel-view--display-fragment-keymap
                 (define-keymap "g" action "k" action))))
      (insert (propertize "card\nsecond\n" 'keymap map))
      (goto-char 1)
      (mevedel-evil-test--keys "g r K")
      (should (= calls 2))
      (mevedel-evil-test--keys "j g g")
      (should (= (point) 1))
      (should (= calls 2))
      (evil-local-mode -1)
      (mevedel-evil-test--keys "g k")
      (should (= calls 4))))
  :doc "pending cockpit d u x and gr execute the existing action commands"
  (mevedel-evil-test--buffer
    (mevedel-pending-inputs-mode)
    (evil-normal-state)
    (let (calls)
      (cl-letf (((symbol-function 'mevedel-pending-inputs-mark-delete)
                 (lambda () (interactive) (push 'mark calls)))
                ((symbol-function 'mevedel-pending-inputs-unmark)
                 (lambda () (interactive) (push 'unmark calls)))
                ((symbol-function 'mevedel-pending-inputs-execute-deletions)
                 (lambda () (interactive) (push 'execute calls)))
                ((symbol-function 'mevedel-pending-inputs-refresh)
                 (lambda () (interactive) (push 'refresh calls))))
        (mevedel-evil-test--keys "d u x g r")
        (should (equal (nreverse calls) '(mark unmark execute refresh))))))
  :doc "repeated control-transfer composition keeps remapped and vanilla actions"
  (mevedel-evil-test--buffer
    (mevedel-view-mode)
    (evil-normal-state)
    (insert "card\nsecond\n")
    (let (calls)
      (cl-letf (((symbol-function 'mevedel-view-control-transfer-grant)
                 (lambda () (interactive) (push 'grant calls)))
                ((symbol-function 'mevedel-view-control-transfer-keep)
                 (lambda () (interactive) (push 'keep calls))))
        (dotimes (_ 2)
          (add-text-properties
           1 (point-max)
           (list 'keymap (mevedel-view--display-fragment-keymap
                          mevedel-view--control-transfer-map)))
          (goto-char 1)
          (mevedel-evil-test--keys "g r K j g g")
          (should (= (point) 1)))
        (should (equal (nreverse calls) '(grant keep grant keep)))
        (setq calls nil)
        (evil-local-mode -1)
        (mevedel-evil-test--keys "g k")
        (should (equal (nreverse calls) '(grant keep))))))
  :doc "directive inspector keeps inherited q and its own actions"
  (mevedel-evil-test--buffer
    (mevedel-directive-activity-mode)
    (evil-normal-state)
    (let (calls)
      (cl-letf (((symbol-function 'mevedel-directive-activity-refresh)
                 (lambda () (interactive) (push 'refresh calls)))
                ((symbol-function 'quit-window)
                 (lambda (&rest _) (interactive) (push 'quit calls))))
        (mevedel-evil-test--keys "g r q")
        (should (equal (nreverse calls) '(refresh quit))))))
  :doc "directive frame prefixed controls work in normal and insert state"
  (mevedel-evil-test--buffer
    (mevedel-directive-frame-mode 1)
    (dolist (state '(normal insert))
      (evil-change-state state)
      (let (calls)
        (cl-letf (((symbol-function 'mevedel-directive-frame-toggle-filter)
                   (lambda () (interactive) (push 'filter calls)))
                  ((symbol-function 'mevedel-directive-frame-close)
                   (lambda () (interactive) (push 'close calls))))
          (mevedel-evil-test--keys "C-c C-f C-c C-z")
          (should (equal (nreverse calls) '(filter close)))))))
  :doc "collaboration share local map actions beat normal operators"
  (mevedel-evil-test--buffer
    (use-local-map mevedel-collaboration--share-map)
    (let (calls)
      (cl-letf (((symbol-function 'mevedel-collaboration-share-toggle)
                 (lambda () (interactive) (push 'toggle calls)))
                ((symbol-function 'mevedel-collaboration-share-copy-view)
                 (lambda () (interactive) (push 'view calls)))
                ((symbol-function 'mevedel-collaboration-share-copy-full)
                 (lambda () (interactive) (push 'full calls)))
                ((symbol-function 'mevedel-collaboration-share-copy-owner)
                 (lambda () (interactive) (push 'owner calls)))
                ((symbol-function 'mevedel-collaboration-share-quit)
                 (lambda () (interactive) (push 'quit calls))))
        (mevedel-evil-test--keys "TAB c f o q")
        (should (equal (nreverse calls) '(toggle view full owner quit))))))
  :doc "reports and journal help descendants use gr RET and q without motion conflicts"
  (dolist (mode '(mevedel-report-mode mevedel-journal-jobs-mode
                  mevedel-subscription-usage-mode))
    (mevedel-evil-test--buffer
      (funcall mode)
      (evil-normal-state)
      (let (calls)
        (cl-letf (((symbol-function 'mevedel-report-refresh)
                   (lambda () (interactive) (push 'refresh calls)))
                  ((symbol-function 'mevedel-journal-jobs)
                   (lambda () (interactive) (push 'refresh calls)))
                  ((symbol-function 'mevedel-journal-inspect)
                   (lambda () (interactive) (push 'activate calls)))
                  ((symbol-function 'mevedel-subscription-usage-refresh)
                   (lambda () (interactive) (push 'refresh calls)))
                  ((symbol-function 'push-button)
                   (lambda (&rest _) (interactive) (push 'activate calls)))
                  ((symbol-function 'mevedel-report-quit)
                   (lambda () (interactive) (push 'quit calls))))
          (mevedel-evil-test--keys "g r RET q")
          (should (equal (nreverse calls) '(refresh activate quit)))))))
  :doc "directive visual creation edits its real minibuffer prompt and exits cleanly"
  (dolist (spec '(("v l" char 2 4) ("V" line 1 5)))
    (let ((file (make-temp-file "mevedel-evil-" nil ".txt" "abc\ndef\n")))
      (unwind-protect
          (mevedel-evil-test--buffer
            (insert "abc\ndef\n")
            (setq-local buffer-file-name file
                        mevedel--workspace
                        (mevedel-workspace--create
                         :id file :root (file-name-directory file) :type 'file))
            (goto-char 2)
            (local-set-key (kbd "C-c d") #'mevedel-create-directive)
            (mevedel-evil-test--keys (concat (car spec) " C-c d D o n e RET"))
            (let ((overlay (car (mevedel--instructions-in 1 (point-max)))))
              (should (eq (overlay-get overlay 'mevedel-span) (nth 1 spec)))
              (should (= (overlay-start overlay) (nth 2 spec)))
              (should (= (overlay-end overlay) (nth 3 spec)))
              (should (equal (mevedel--directive-text overlay) "Done")))
            (should (eq evil-state 'normal))
            (should-not (region-active-p)))
        (unlock-file file)
        (delete-file file)))))

(mevedel-deftest mevedel-evil--replacement
  (:before-each (mevedel-evil-test--setup)
   :after-each (mevedel-evil-test--teardown)
   :vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global))
   :quiet t)
  ,test
  (test)
  :doc "cc retains the first line of a real V-j-created two-line reference"
  (mevedel-evil-test--buffer
    (insert "one\ntwo\nthree\nfour\n")
    (goto-char 5)
    (local-set-key (kbd "C-c r") #'mevedel-create-reference)
    (mevedel-evil-test--keys "V j C-c r RET")
    (let ((overlay (car (mevedel--instructions-in 1 (point-max)))))
      (should (equal (buffer-substring-no-properties
                      (overlay-start overlay) (overlay-end overlay))
                     "two\nthree\n"))
      (goto-char 5)
      (setq buffer-undo-list nil)
      (mevedel-evil-test--keys "c c T W O <escape>")
      (should (equal (buffer-string) "one\nTWO\nthree\nfour\n"))
      (should (equal (list (overlay-start overlay) (overlay-end overlay)
                           (overlay-get overlay 'mevedel-span))
                     '(5 15 line)))
      (should (equal (buffer-substring-no-properties
                      (overlay-start overlay) (overlay-end overlay))
                     "TWO\nthree\n"))
      (mevedel-evil-test--keys "u")
      (should (equal (buffer-string) "one\ntwo\nthree\nfour\n"))
      (should (equal (list (overlay-start overlay) (overlay-end overlay)
                           (overlay-get overlay 'mevedel-span))
                     '(5 15 line)))
      (mevedel-evil-test--keys "C-r")
      (should (equal (buffer-string) "one\nTWO\nthree\nfour\n"))
      (should (equal (list (overlay-start overlay) (overlay-end overlay)
                           (overlay-get overlay 'mevedel-span))
                     '(5 15 line)))))
  :doc "c cc s S C and R preserve complete replacements in both span shapes"
  (dolist (span '(char line))
    (dolist (command '("c l" "c c" "s" "S" "C" "R" "v c"))
      (mevedel-evil-test--buffer
        (insert "before\nx\nafter\n")
        (let ((overlay (mevedel--create-reference-in
                        (current-buffer) 8 (if (eq span 'line) 10 9) span)))
          (goto-char 8)
          (setq buffer-undo-list nil)
          (mevedel-evil-test--keys (concat command " N E W <escape>"))
          (should (overlay-buffer overlay))
          (should (eq (overlay-get overlay 'mevedel-span) span))
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (or (eq span 'line) (member command '("c c" "S")))
                             "NEW\n" "NEW")))
          (should (eq evil-state 'normal))
          (mevedel-evil-test--keys "u")
          (should (equal (buffer-string) "before\nx\nafter\n"))
          (should (= (overlay-start overlay) 8))
          (should (= (overlay-end overlay) (if (eq span 'line) 10 9)))
          (mevedel-evil-test--keys "C-r")
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (or (eq span 'line) (member command '("c c" "S")))
                             "NEW\n" "NEW")))))))
  :doc "dd fully deletes a reference and u C-r restore deletion geometry"
  (dolist (span '(char line))
    (mevedel-evil-test--buffer
      (insert "before\nx\nafter\n")
      (let ((overlay (mevedel--create-reference-in
                      (current-buffer) 8 (if (eq span 'line) 10 9) span)))
        (goto-char 8)
        (setq buffer-undo-list nil)
        (mevedel-evil-test--keys "d d")
        (should-not (overlay-buffer overlay))
        (mevedel-evil-test--keys "u")
        (should (= (overlay-start overlay) 8))
        (should (= (overlay-end overlay) (if (eq span 'line) 10 9)))
        (mevedel-evil-test--keys "C-r")
        (should-not (overlay-buffer overlay)))))
  :doc "o O add adjacent lines without absorbing them into either span"
  (dolist (span '(char line))
    (dolist (command '("o" "O"))
      (mevedel-evil-test--buffer
        (insert "before\nx\nafter\n")
        (let ((overlay (mevedel--create-reference-in
                        (current-buffer) 8 (if (eq span 'line) 10 9) span)))
          (goto-char 8)
          (mevedel-evil-test--keys (concat command " N E W <escape>"))
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (eq span 'line) "x\n" "x")))))))
  :doc "J joins the next line into a line span but not a character span"
  (dolist (span '(char line))
    (mevedel-evil-test--buffer
      (insert "before\nx\nafter\n")
      (let ((overlay (mevedel--create-reference-in
                      (current-buffer) 8 (if (eq span 'line) 10 9) span)))
        (goto-char 8)
        (mevedel-evil-test--keys "J")
        (should (equal (buffer-substring-no-properties
                        (overlay-start overlay) (overlay-end overlay))
                       (if (eq span 'line) "x after" "x"))))))
  :doc "linewise p P add adjacent lines without absorbing them"
  (dolist (span '(char line))
    (dolist (command '("p" "P"))
      (mevedel-evil-test--buffer
        (insert "NEW\nx\nafter\n")
        (let ((overlay (mevedel--create-reference-in
                        (current-buffer) 5 (if (eq span 'line) 7 6) span)))
          (goto-char 1)
          (mevedel-evil-test--keys (concat "y y j " command))
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (eq span 'line) "x\n" "x")))))))
  :doc "dot repeats a real change session on a second reference"
  (dolist (span '(char line))
    (mevedel-evil-test--buffer
      (insert "before\nx\nx\nafter\n")
      (let ((first (mevedel--create-reference-in
                    (current-buffer) 8 (if (eq span 'line) 10 9) span))
            (second (mevedel--create-reference-in
                     (current-buffer) 10 (if (eq span 'line) 12 11) span)))
        (goto-char 8)
        (mevedel-evil-test--keys "c l N E W <escape>")
        (goto-char (overlay-start second))
        (mevedel-evil-test--keys ".")
        (dolist (overlay (list first second))
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (eq span 'line) "NEW\n" "NEW")))))))
  :doc "disabling local Evil finalizes a held empty character change"
  (mevedel-evil-test--buffer
    (insert "before\nx\nafter\n")
    (let ((overlay (mevedel--create-reference-in (current-buffer) 8 9 'char)))
      (goto-char 8)
      (mevedel-evil-test--keys "c l")
      (should mevedel--instruction-replacement-held)
      (evil-local-mode -1)
      (should-not mevedel--instruction-replacement-held)
      (should-not (overlay-buffer overlay))))
  :doc "an aborted real change command releases its replacement scope"
  (mevedel-evil-test--buffer
    (insert "before\nx\nafter\n")
    (mevedel--create-reference-in (current-buffer) 8 9 'char)
    (goto-char 8)
    (let ((buffer-read-only t))
      (should-error (mevedel-evil-test--keys "c l")))
    (should-not mevedel--instruction-replacement-held))
  :doc "directive real change keys retain authored records and undo anchor geometry"
  (dolist (span '(char line))
    (dolist (command '("c l" "c c" "s" "R"))
      (mevedel-evil-test--file-buffer "before\nx\nafter\n"
        (let* ((overlay (mevedel--create-directive-in
                         (current-buffer) 8 (if (eq span 'line) 10 9)
                         nil "Keep the request" span))
               (record (mevedel--directive-record overlay)))
          (goto-char 8)
          (setq buffer-undo-list nil)
          (mevedel-evil-test--keys (concat command " N E W <escape>"))
          (should (eq record (mevedel--directive-record overlay)))
          (should (eq (plist-get (mevedel-directive-anchor record) :state) 'attached))
          (should (equal (mevedel--directive-text overlay) "Keep the request"))
          (should (equal (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))
                         (if (or (eq span 'line) (equal command "c c"))
                             "NEW\n" "NEW")))
          (mevedel-evil-test--keys "u")
          (should (equal (buffer-string) "before\nx\nafter\n"))
          (should (= (overlay-start overlay) 8))
          (should (= (overlay-end overlay) (if (eq span 'line) 10 9)))
          (mevedel-evil-test--keys "C-r")
          (should (eq record (mevedel--directive-record overlay)))
          (should (eq (plist-get (mevedel-directive-anchor record) :state) 'attached))))))
  :doc "fine undo follows Evil grouping and eventually restores both span shapes"
  (dolist (span '(char line))
    (mevedel-evil-test--buffer
      (let ((evil-want-fine-undo t))
        (insert "before\nx\nafter\n")
        (let ((overlay (mevedel--create-reference-in
                        (current-buffer) 8 (if (eq span 'line) 10 9) span)))
          (goto-char 8)
          (setq buffer-undo-list nil)
          (mevedel-evil-test--keys "c l N E W <escape>")
          (let ((steps 0))
            (while (and (not (equal (buffer-string) "before\nx\nafter\n"))
                        (< steps 10))
              (mevedel-evil-test--keys "u")
              (cl-incf steps))
            (should (equal (buffer-string) "before\nx\nafter\n"))
            (should (= (overlay-start overlay) 8))
            (should (= (overlay-end overlay) (if (eq span 'line) 10 9)))
            (dotimes (_ steps) (mevedel-evil-test--keys "C-r"))
            (should (equal (buffer-substring-no-properties
                            (overlay-start overlay) (overlay-end overlay))
                           (if (eq span 'line) "NEW\n" "NEW")))))))))

(mevedel-deftest mevedel-evil--partial-replacement
  (:before-each (mevedel-evil-test--setup)
   :after-each (mevedel-evil-test--teardown)
   :vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global))
   :quiet t
   :doc "multiline partial replacement keeps exact text and span through undo/redo")
  (mevedel-evil-test--partial-change ,span ,edge ,command)
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

(mevedel-deftest mevedel-evil--cold-loading ()
  ,test
  (test)
  :doc "entrypoint and optional library cold-load without Evil or side effects"
  (with-temp-buffer
    (let ((form `(progn
                   (setq load-path ',load-path)
                   (require 'mevedel)
                   (unless (autoloadp (symbol-function 'mevedel-evil-setup))
                     (error "Missing Evil setup autoload"))
                   (require 'mevedel-evil)
                   (when (or (featurep 'evil) mevedel-evil--installed)
                     (error "Loading activated Evil")))))
      (should (= 0 (call-process invocation-name nil t nil
                                 "-Q" "--batch" "--eval"
                                 (prin1-to-string form))))))
  :doc "missing Evil fails clearly and leaves setup inactive"
  (with-temp-buffer
    (let ((form `(progn
                   (setq load-path ',load-path)
                   (require 'mevedel-evil)
                   (let ((original (symbol-function 'require)) raised)
                     (cl-letf (((symbol-function 'require)
                                (lambda (feature &rest args)
                                  (unless (eq feature 'evil)
                                    (apply original feature args)))))
                       (condition-case err (mevedel-evil-setup)
                         (user-error
                          (setq raised t)
                          (unless (string-match-p "Evil is not installed"
                                                  (error-message-string err))
                            (signal (car err) (cdr err))))))
                     (unless raised (error "Missing dependency error was not raised")))
                   (when mevedel-evil--installed (error "Partial setup")))))
      (should (= 0 (call-process invocation-name nil t nil
                                 "-Q" "--batch" "--eval"
                                 (prin1-to-string form)))))))

(provide 'test-mevedel-evil)
;;; test-mevedel-evil.el ends here
