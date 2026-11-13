;;; test-mevedel-notes-list.el -- Workspace note list tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise note commands and their list in real workspace files.

;;; Code:

(require 'mevedel)
(require 'mevedel-notes)
(require 'mevedel-mentions)
(require 'mevedel-note-mentions)
(require 'mevedel-persistence)
(require 'mevedel-permission-rules)
(require 'mevedel-permissions)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-notes-list-test--no-source-probes (operation &rest args)
  "Reject source observations through OPERATION; allow pure path work on ARGS."
  (when (memq operation '(file-exists-p file-readable-p file-attributes
                         insert-file-contents insert-file-contents-literally))
    (error "Unexpected source probe: %s" operation))
  (let ((file-name-handler-alist nil))
    (apply operation args)))

(defmacro mevedel-notes-list-test--workspace (&rest body)
  "Run BODY with two source files in a private workspace."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "notes-list-" t))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel-workspace--registry (make-hash-table :test #'equal))
          (workspace (mevedel-workspace-get-or-create
                      'file directory directory "list"))
          (first (file-name-concat directory "first.el"))
          (second (file-name-concat directory "second.el"))
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          list-buffer)
     (save-window-excursion
       (unwind-protect
           (progn
             (make-directory (file-name-concat directory ".mevedel"))
             (write-region "first code\n" nil first nil 'silent)
             (write-region "before\nsecond code\n" nil second nil 'silent)
             (with-current-buffer (find-file-noselect first)
               (setq-local mevedel--workspace workspace)
               (goto-char (point-min))
               (mevedel-note-create "First thought"))
             (with-current-buffer (find-file-noselect second)
               (setq-local mevedel--workspace workspace)
               (goto-char (point-min))
               (forward-line 1)
               (mevedel-note-create "Second thought"))
             ;; Automatic visits and explicit actions share one store owner.
             (dolist (file (list first second))
               (should (eq workspace (buffer-local-value
                                      'mevedel--workspace (find-buffer-visiting file)))))
             ,@body)
         (when (buffer-live-p list-buffer) (kill-buffer list-buffer))
         (dolist (file (list first second))
           (when-let* ((buffer (find-buffer-visiting file)))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))
         (setq mevedel-instruction-store--dirty nil)
         (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
         (delete-directory directory t)))))

(mevedel-deftest mevedel-notes-list
  (:quiet t)
  ,test
  (test)
  :doc "folding and refresh keep point on the same note or group heading"
  (mevedel-notes-list-test--workspace
    (with-current-buffer (find-buffer-visiting first)
      (goto-char (point-min))
      (mevedel-note-done))
    (setq list-buffer (mevedel-notes-list workspace))
    (with-current-buffer list-buffer
      (goto-char (point-min))
      (search-forward "Recently closed")
      (dolist (key '("TAB" "g" "TAB"))
        (call-interactively (key-binding (kbd key)))
        (should (string-match-p "Recently closed"
                                (buffer-substring (line-beginning-position) (line-end-position)))))
      (goto-char (point-min))
      (search-forward "Second thought")
      (let ((id (tabulated-list-get-id)))
        (dolist (key '("TAB" "g"))
          (call-interactively (key-binding (kbd key)))
          (should (equal id (tabulated-list-get-id)))))
      (goto-char (point-min))
      (search-forward "Unsent")
      (call-interactively (key-binding (kbd "TAB")))
      (should (string-match-p "Unsent"
                              (buffer-substring (line-beginning-position) (line-end-position))))))
  :doc "done and delete retain the note when visible or the nearest row when hidden"
  (dolist (key '("x" "d"))
    (dolist (expanded '(nil t))
      (mevedel-notes-list-test--workspace
        (setq list-buffer (mevedel-notes-list workspace))
        (with-current-buffer list-buffer
          (when expanded (call-interactively (key-binding (kbd "TAB"))))
          (goto-char (point-min))
          (search-forward "Second thought")
          (let ((id (tabulated-list-get-id)))
            (call-interactively (key-binding key))
            (if expanded
                (should (equal id (tabulated-list-get-id)))
              (should (string-match-p
                       "Recently closed"
                       (buffer-substring (line-beginning-position) (line-end-position))))))))))
  :doc "lists notes from unopened files after reload and visits with one key"
  (mevedel-notes-list-test--workspace
    (kill-buffer (find-buffer-visiting second))
    (mevedel--clear-instruction-state workspace)
    (setq list-buffer (mevedel-notes-list workspace))
    (should-not (find-buffer-visiting second))
    (with-current-buffer list-buffer
      (should (string-match-p "Unsent" (buffer-string)))
      (should (string-match-p "first.el:1.*You.*First thought.*unsent"
                              (buffer-string)))
      (should (string-match-p "second.el:2.*You.*Second thought.*unsent"
                              (buffer-string)))
      (goto-char (point-min))
      (search-forward "Second thought")
      (call-interactively (key-binding "v")))
    (should (equal second (buffer-file-name (window-buffer (selected-window)))))
    (with-current-buffer (window-buffer (selected-window))
      (should (= 2 (line-number-at-pos (window-point (selected-window)) t)))))
  :doc "list edits and deletes use the selected UUID and preserve source"
  (mevedel-notes-list-test--workspace
    (setq list-buffer (mevedel-notes-list workspace))
    (with-current-buffer list-buffer
      (goto-char (point-min))
      (search-forward "Second thought")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Edited in list")))
        (call-interactively (key-binding "e")))
      (should (string-match-p "Edited in list" (buffer-string)))
      (goto-char (point-min))
      (search-forward "Edited in list")
      (call-interactively (key-binding "d"))
      (should-not (string-match-p "Edited in list" (buffer-string)))
      (should (string-match-p "First thought" (buffer-string))))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (with-current-buffer list-buffer
      (mevedel-notes-list-refresh)
      (should-not (string-match-p "Edited in list" (buffer-string))))
    (with-current-buffer (find-buffer-visiting second)
      (should (equal "before\nsecond code\n" (buffer-string)))))
  :doc "closed notes fold, show reasons, and restore by list key after reload"
  (mevedel-notes-list-test--workspace
    (setq list-buffer (mevedel-notes-list workspace))
    (with-current-buffer list-buffer
      (goto-char (point-min))
      (search-forward "First thought")
      (call-interactively (key-binding "x"))
      (goto-char (point-min))
      (search-forward "Second thought")
      (call-interactively (key-binding "d")))
    (mevedel-instruction-store-flush t)
    (kill-buffer (find-buffer-visiting second))
    (mevedel--clear-instruction-state workspace)
    (with-current-buffer list-buffer
      (mevedel-notes-list-refresh)
      (should (string-match-p "Recently closed (2)" (buffer-string)))
      (should-not (string-match-p "First thought\\|Second thought" (buffer-string)))
      ;; Folding is display-only, including for a remote workspace store.
      (let ((file-name-handler-alist
             '(("." . (lambda (&rest _) (error "Unexpected file I/O"))))))
        (call-interactively (key-binding (kbd "TAB")))
        (should (string-match-p "First thought.*done" (buffer-string)))
        (should (string-match-p "Second thought.*deleted" (buffer-string)))
        (call-interactively (key-binding (kbd "TAB")))
        (should-not (string-match-p "Second thought" (buffer-string)))
        (call-interactively (key-binding (kbd "TAB"))))
      (should-not (find-buffer-visiting second))
      (goto-char (point-min))
      (search-forward "Second thought")
      (call-interactively (key-binding "r"))
      (should (string-match-p "second.el:2.*Second thought.*unsent" (buffer-string)))
      (should (eq (current-buffer) (window-buffer (selected-window)))))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (with-current-buffer list-buffer
      (mevedel-notes-list-refresh)
      (should (string-match-p "second.el:2.*Second thought.*unsent" (buffer-string))))
    (with-current-buffer (find-buffer-visiting second)
      (should (equal "before\nsecond code\n" (buffer-string)))))
  :doc "closed imports without a saved position show the first line"
  (mevedel-notes-list-test--workspace
    (with-current-buffer (find-buffer-visiting first)
      (goto-char (point-min))
      (mevedel-note-done)
      (let ((export (file-name-concat directory "notes.el")))
        (mevedel-save-instructions export)
        (let* ((data (with-temp-buffer
                       (insert-file-contents export)
                       (read (current-buffer))))
               (instruction (car (plist-get (cdr (assoc "first.el" (plist-get data :files)))
                                            :instructions))))
          (cl-remf instruction :closed-position)
          (setf (plist-get (cdr (assoc "first.el" (plist-get data :files))) :instructions)
                (list instruction))
          (write-region (prin1-to-string data) nil export nil 'silent))
        (mevedel-load-instructions export)))
    (setq list-buffer (mevedel-notes-list workspace))
    (with-current-buffer list-buffer
      (call-interactively (key-binding (kbd "TAB")))
      (should (string-match-p "first.el:1.*First thought.*done" (buffer-string)))
      (should-not (string-match-p "first.el:nil" (buffer-string)))))
  :doc "missing source uses cached live and stashed flags without row probes"
  (mevedel-notes-list-test--workspace
    (setq list-buffer (mevedel-notes-list workspace))
    (delete-file second)
    ;; No source observation yet: list refresh must not invent a new one.
    (with-current-buffer list-buffer
      (let ((file-name-handler-alist
             '(("second\\.el" . mevedel-notes-list-test--no-source-probes))))
        (mevedel-notes-list-refresh))
      (should-not (string-match-p "source missing" (buffer-string))))
    (with-current-buffer (find-buffer-visiting second)
      (run-hooks 'post-command-hook))
    (mevedel-instruction-store-flush t)
    (with-current-buffer list-buffer
      (mevedel-notes-list-refresh)
      (should (string-match-p "Second thought.*unsent; source missing" (buffer-string))))
    (kill-buffer (find-buffer-visiting second))
    (mevedel--clear-instruction-state workspace)
    ;; The unvisited stash retains its last observation even if the file returns.
    (write-region "before\nsecond code\n" nil second nil 'silent)
    ;; Cold store installation has its own source-restoration boundary.
    (mevedel-instruction-store-observe workspace)
    (with-current-buffer list-buffer
      (let ((file-name-handler-alist
             '(("second\\.el" . mevedel-notes-list-test--no-source-probes))))
        (mevedel-notes-list-refresh))
      (should (string-match-p "Second thought.*unsent; source missing" (buffer-string)))
      (goto-char (point-min))
      (search-forward "Second thought")
      (call-interactively (key-binding "v")))
    (with-current-buffer list-buffer
      (mevedel-notes-list-refresh)
      (should-not (string-match-p "source missing" (buffer-string))))))

(mevedel-deftest mevedel-note-set-tags
  (:quiet t)
  ,test
  (test)
  :doc "tag edits persist, appear in the list, and can remove every tag"
  (mevedel-notes-list-test--workspace
    (with-current-buffer (find-buffer-visiting first)
      (mevedel-note-set-tags '(review parser)))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (setq list-buffer (mevedel-notes-list workspace))
    (with-current-buffer list-buffer
      (should (string-match-p "review, parser" (buffer-string)))
      (goto-char (point-min))
      (search-forward "First thought")
      ;; Supply minibuffer input; the list action and persistence are real.
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) nil)))
        (call-interactively (key-binding "t"))))
    (mevedel-instruction-store-flush t)
    (mevedel--clear-instruction-state workspace)
    (with-current-buffer list-buffer
      (mevedel-notes-list-refresh)
      (should-not (string-match-p "review\\|parser" (buffer-string))))))

(mevedel-deftest mevedel-note-set-tags/queryable
  (:quiet t)
  ,test
  (test)
  :doc "unqueryable tag symbols are refused without replacing existing tags"
  (mevedel-notes-list-test--workspace
    (with-current-buffer (find-buffer-visiting first)
      (mevedel-note-set-tags '(review))
      (dolist (name '("123" "needs review" "and" "or" "not" "nil" "unfinished("))
        (should-error (mevedel-note-set-tags (list (intern name))) :type 'user-error)
        (should (equal '(review) (overlay-get (mevedel-notes--at-point) 'mevedel-note-tags))))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) '("123"))))
        (should-error (call-interactively #'mevedel-note-set-tags) :type 'user-error))))
  :doc "the list tag prompt rejects the same unqueryable tags as the source command"
  (mevedel-notes-list-test--workspace
    (setq list-buffer (mevedel-notes-list workspace))
    (with-current-buffer list-buffer
      (goto-char (point-min))
      (search-forward "First thought")
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) '("needs review"))))
        (should-error (call-interactively (key-binding "t")) :type 'user-error)))
    (with-current-buffer (find-buffer-visiting first)
      (should-not (overlay-get (mevedel-notes--at-point) 'mevedel-note-tags)))))

(mevedel-deftest mevedel-note-capf/unopened
  (:quiet t)
  ,test
  (test)
  :doc "ordinary completion does not observe an unreadable note store"
  (mevedel-notes-list-test--workspace
    (with-temp-file (mevedel-instruction-store-path workspace) (insert "invalid"))
    (with-temp-buffer
      (setq-local mevedel--session (mevedel-session-create "plain-completion" workspace))
      (insert "ordinary word")
      (should-not (mevedel-note-capf))))
  :doc "note and ref completion bind unopened notes without visiting source"
  (mevedel-notes-list-test--workspace
    (kill-buffer (find-buffer-visiting second))
    (mevedel--clear-instruction-state workspace)
    (mevedel-instruction-store-observe workspace)
    (with-temp-buffer
      (setq-local mevedel--session (mevedel-session-create "completion" workspace))
      (dolist (kind '(note ref))
        (erase-buffer)
        (insert (format "@%s:" kind))
        (let* ((completion (mevedel-note-capf))
               (candidates (all-completions "" (nth 2 completion)))
               (exit (plist-get (nthcdr 3 completion) :exit-function)))
          (should (member "2" candidates))
          (should-not (find-buffer-visiting second))
          (insert "2")
          (funcall exit "2" 'finished)
          (let ((binding (get-text-property 0 'mevedel-mention-binding (buffer-string))))
            (should (eq kind (plist-get binding :kind)))
            (should (stringp (plist-get binding (if (eq kind 'note) :note-uuid :reference-uuid)))))))))
  :doc "note completion uses loaded metadata even when the store becomes unreadable"
  (mevedel-notes-list-test--workspace
    (with-temp-file (mevedel-instruction-store-path workspace) (insert "invalid"))
    (with-temp-buffer
      (setq-local mevedel--session (mevedel-session-create "cached-completion" workspace))
      (insert "@note:")
      (let ((completion (mevedel-note-capf)))
        (should (member "2" (all-completions "" (nth 2 completion))))))))

(mevedel-deftest mevedel-mentions-install
  (:quiet t)
  ,test
  (test)
  :doc "note and ref tokens fontify without reference resolvers or store observations"
  (mevedel-notes-list-test--workspace
    (with-temp-buffer
      (rename-buffer "note mention fontification" t)
      (fundamental-mode)
      (setq-local mevedel--session (mevedel-session-create "fontification" workspace)
                  font-lock-defaults '(nil t))
      (insert "@ref:2 @note:2 @ref:{review} @note:{review}")
      (mevedel-mentions-install)
      (font-lock-mode 1)
      (font-lock-ensure)
      (goto-char (point-min))
      (dolist (token '("@ref:2" "@note:2" "@ref:{review}" "@note:{review}"))
        (search-forward token)
        (should (get-text-property (match-beginning 0) 'face))))))

(mevedel-deftest mevedel-mentions-expand-user-input-notes
  (:quiet t)
  ,test
  (test)
  :doc "ref tag mentions select notes explicitly and empty queries attach nothing"
  (mevedel-notes-list-test--workspace
    (with-current-buffer (find-buffer-visiting second)
      (mevedel-note-set-tags '(review)))
    (let* ((session (mevedel-session-create "ref-notes" workspace))
           (expansion (mevedel-mentions-expand-user-input "Explain @ref:{review}" session))
           (body (mapconcat (lambda (item) (plist-get item :body))
                           (plist-get expansion :reminder-items) "\n")))
      (should (string-match-p "second code" body))
      (should-not (string-match-p "first code" body))
      (setq expansion (mevedel-mentions-expand-user-input "Explain @ref:{}" session))
      (should (string-match-p "no matches" (plist-get expansion :text)))
      (should-not (string-match-p "first code\\|second code"
                                  (mapconcat (lambda (item) (plist-get item :body))
                                             (plist-get expansion :reminder-items) "\n")))))
  :doc "number mentions attach only selected code from unopened files"
  (mevedel-notes-list-test--workspace
    (kill-buffer (find-buffer-visiting second))
    (mevedel--clear-instruction-state workspace)
    (let* ((session (mevedel-session-create "mentions" workspace))
           (prepared (mevedel-mentions-prepare-user-input "Explain @note:2" session))
           (expansion (mevedel-mentions-expand-user-input prepared session))
           (body (mapconcat (lambda (item) (plist-get item :body))
                            (plist-get expansion :reminder-items) "\n")))
      (should (string-match-p "contents attached" (plist-get expansion :text)))
      (should (string-match-p "second code" body))
      (should (string-match-p "Second thought" body))
      (should (string-match-p "second.el" body))
      (should-not (string-match-p "first code" body))))
  :doc "tag queries attach only matching notes and reevaluate current tags"
  (mevedel-notes-list-test--workspace
    (with-current-buffer (find-buffer-visiting first)
      (mevedel-note-set-tags '(parser review)))
    (with-current-buffer (find-buffer-visiting second)
      (mevedel-note-set-tags '(review)))
    (let* ((session (mevedel-session-create "tags" workspace))
           (expansion (mevedel-mentions-expand-user-input
                       "Explain @note:{review and not parser}" session))
           (body (mapconcat (lambda (item) (plist-get item :body))
                            (plist-get expansion :reminder-items) "\n")))
      (should (string-match-p "second code" body))
      (should-not (string-match-p "first code" body))
      (with-current-buffer (find-buffer-visiting first)
        (mevedel-note-set-tags '(review)))
      (setq expansion (mevedel-mentions-expand-user-input
                       "Explain @note:{review and not parser}" session))
      (should (string-match-p "first code"
                              (plist-get (car (plist-get expansion :reminder-items)) :body)))))
  :doc "plain prompts and empty or missing mentions never attach every note"
  (mevedel-notes-list-test--workspace
    (let ((session (mevedel-session-create "empty" workspace)))
      (let ((plain (mevedel-mentions-expand-user-input "Hello" session)))
        (should (equal "Hello" (plist-get plain :text)))
        (should-not (plist-get plain :reminder-items)))
      (dolist (text '("@note:{}" "@note:{absent}" "@note:999"))
        (let* ((expansion (mevedel-mentions-expand-user-input text session))
               (body (mapconcat (lambda (item) (plist-get item :body))
                                (plist-get expansion :reminder-items) "\n")))
          (should (string-match-p "no matches\\|unavailable" (plist-get expansion :text)))
          (should-not (string-match-p "first code\\|second code" body))))))
  :doc "a bound number never retargets a deleted note or another workspace"
  (mevedel-notes-list-test--workspace
    (let* ((session (mevedel-session-create "identity" workspace))
           (bound (mevedel-mentions-prepare-user-input "@note:1" session)))
      (with-current-buffer (find-buffer-visiting first)
        (mevedel-note-delete)
        (mevedel-note-create "Replacement"))
      (let ((expansion (mevedel-mentions-expand-user-input bound session)))
        (should (string-match-p "unavailable" (plist-get expansion :text))))
      (let* ((other (mevedel-workspace--create
                     :type 'file :id "other" :root directory :name "other"))
             (other-session (mevedel-session-create "other" other)))
        ;; No store/observation for the unrelated workspace fixture.
        (setf (mevedel-workspace-root other) (file-name-concat directory "other"))
        (let ((expansion (mevedel-mentions-expand-user-input bound other-session)))
          (should (string-match-p "unavailable" (plist-get expansion :text)))))))
  :doc "literal generated context is preserved rather than expanding its tokens"
  (mevedel-notes-list-test--workspace
    (let* ((session (mevedel-session-create "literal" workspace))
           (literal (propertize "Code mentioning @note:1 and @file:missing.el"
                                'mevedel-mention-literal t))
           (expansion (mevedel-mentions-expand-user-input literal session)))
      (should (equal literal (plist-get expansion :text)))
      (should-not (plist-get expansion :reminder-items))))
  :doc "Read-denied notes attach no code, including through tag queries"
  (mevedel-notes-list-test--workspace
    (with-current-buffer (find-buffer-visiting second)
      (mevedel-note-set-tags '(review)))
    (kill-buffer (find-buffer-visiting second))
    (let ((session (mevedel-session-create "denied" workspace))
          (mevedel-permission-rules nil))
      (mevedel-permission--add-session-rule session (list "Read" :path second :action 'deny))
      (dolist (text '("@note:2" "@note:{review}"))
        (let* ((prepared (mevedel-mentions-prepare-user-input text session))
               (expansion (mevedel-mentions-expand-user-input prepared session))
               (body (mapconcat (lambda (item) (plist-get item :body))
                                (plist-get expansion :reminder-items) "\n")))
          (should (string-match-p "permission denied" body))
          (should-not (string-match-p "second code" body))
          (should-not (find-buffer-visiting second))))))
  :doc "unreadable note sources report locally without blocking other tag matches"
  (mevedel-notes-list-test--workspace
    (dolist (file (list first second))
      (with-current-buffer (find-buffer-visiting file)
        (mevedel-note-set-tags '(review))))
    (mevedel-instruction-store-flush t)
    (kill-buffer (find-buffer-visiting second))
    (set-file-modes second #o000)
    (let* ((session (mevedel-session-create "unreadable" workspace))
           (prepared (mevedel-mentions-prepare-user-input "@note:2" session))
           (single (mevedel-mentions-expand-user-input prepared session))
           (query (mevedel-mentions-expand-user-input "@note:{review}" session))
           (body (mapconcat (lambda (item) (plist-get item :body))
                            (plist-get query :reminder-items) "\n")))
      (should (string-match-p "unreadable" (plist-get single :text)))
      (should (string-match-p "first code" body))
      (should (string-match-p "unreadable" body))
      (should-not (string-match-p "second code" body)))))

(mevedel-deftest mevedel-mentions-note-send-cost
  (:quiet t)
  ,test
  (test)
  :doc "binding and expanding several note mentions observes the store once"
  (mevedel-notes-list-test--workspace
    (let* ((session (mevedel-session-create "mentions" workspace))
           (observations 0)
           (counter (lambda (&rest _) (cl-incf observations))))
      (advice-add 'mevedel-instruction-store-observe :before counter)
      (unwind-protect
          (with-temp-buffer
            (setq-local mevedel--session session mevedel--workspace workspace)
            (let* ((prepared (mevedel-mentions-prepare-user-input "@note:1 @note:2 @ref:1" session))
                   (expansion (mevedel-mentions-expand-user-input prepared session)))
              (should (string-match-p "contents attached" (plist-get expansion :text)))
              (should (= 1 observations))))
        (advice-remove 'mevedel-instruction-store-observe counter))))
  :doc "tag attachments read unopened current sources without visiting their files"
  (mevedel-notes-list-test--workspace
    (dolist (file (list first second))
      (with-current-buffer (find-buffer-visiting file)
        (mevedel-note-set-tags '(review))))
    (mevedel-instruction-store-flush t)
    (dolist (file (list first second)) (kill-buffer (find-buffer-visiting file)))
    (with-temp-file second (insert "new heading\nbefore\nsecond code\n"))
    (let* ((session (mevedel-session-create "unopened" workspace))
           (prepared (mevedel-mentions-prepare-user-input "@note:{review}" session))
           (expansion (mevedel-mentions-expand-user-input prepared session))
           (body (mapconcat (lambda (item) (plist-get item :body))
                            (plist-get expansion :reminder-items) "\n")))
      (should (string-match-p "first code" body))
      (should (string-match-p "second code" body))
      (should (string-match-p "second.el.*, line 3" body))
      (should-not (find-buffer-visiting first))
      (should-not (find-buffer-visiting second)))))

(mevedel-deftest mevedel-mentions-note-remap
  (:quiet t)
  ,test
  (test)
  :doc "externally rewritten stashed code falls back to source remapping"
  (mevedel-notes-list-test--workspace
    (with-current-buffer (find-buffer-visiting second)
      (mevedel-note-set-tags '(review)))
    (mevedel-instruction-store-flush t)
    (kill-buffer (find-buffer-visiting second))
    (with-temp-file second (insert "new heading\nbefore\nsecond revised code\n"))
    (let* ((session (mevedel-session-create "remap" workspace))
           (prepared (mevedel-mentions-prepare-user-input "@note:{review}" session))
           (expansion (mevedel-mentions-expand-user-input prepared session))
           (body (mapconcat (lambda (item) (plist-get item :body))
                            (plist-get expansion :reminder-items) "\n")))
      (should (string-match-p "second revised code" body))
      (should (string-match-p "second.el.*, line 3" body))
      (should (find-buffer-visiting second)))))

(mevedel-deftest mevedel-notes-mode-line
  (:quiet t)
  ,test
  (test)
  :doc "summarizes attention across live and unopened notes without file I/O"
  (mevedel-notes-list-test--workspace
    ;; Fixture future workflow states; observe them only at the UI seam.
    (with-current-buffer (find-buffer-visiting first)
      (overlay-put (mevedel-notes--at-point) 'mevedel-note-state 'ready))
    (with-current-buffer (find-buffer-visiting second)
      (overlay-put (mevedel-notes--at-point) 'mevedel-note-state 'working))
    (kill-buffer (find-buffer-visiting second))
    (with-current-buffer (find-buffer-visiting first)
      (let ((file-name-handler-alist
             '(("." . (lambda (&rest _) (error "Unexpected file I/O"))))))
        (should (equal " Notes: 1 working, 1 ready"
                       (mevedel-notes-mode-line))))))
  :doc "workflow attention providers project unopened notes without loading sessions"
  (mevedel-notes-list-test--workspace
    (kill-buffer (find-buffer-visiting second))
    (let ((mevedel-notes-attention-state-functions
           (list (lambda (properties)
                   (when (= (if (overlayp properties)
                                (overlay-get properties 'mevedel-id)
                              (plist-get properties 'mevedel-id)) 2)
                     'working)))))
      (with-current-buffer (find-buffer-visiting first)
        (should (equal " Notes: 1 working" (mevedel-notes-mode-line))))
      (setq list-buffer (mevedel-notes-list workspace))
      (with-current-buffer list-buffer
        (should (string-match-p "Working" (buffer-string))))))
  :doc "source attention uses its cached instruction ownership without rediscovery"
  (mevedel-notes-list-test--workspace
    (with-current-buffer (find-buffer-visiting first)
      (overlay-put (mevedel-notes--at-point) 'mevedel-note-attention-state 'ready)
      ;; Ordinary workspace discovery need not cache a workspace struct.
      ;; Creating the note already cached this source's instruction-state key.
      (unwind-protect
          (progn
            (kill-local-variable 'mevedel--workspace)
            (let ((file-name-handler-alist
                   '(("." . (lambda (&rest _) (error "Unexpected file I/O"))))))
              (should (equal " Notes: 1 ready" (mevedel-notes-mode-line)))))
        (setq-local mevedel--workspace workspace)))))

(provide 'test-mevedel-notes-list)
;;; test-mevedel-notes-list.el ends here
