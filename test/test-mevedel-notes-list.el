;;; test-mevedel-notes-list.el -- Workspace note list tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise note commands and their list in real workspace files.

;;; Code:

(require 'mevedel)
(require 'mevedel-notes)
(require 'mevedel-mentions)
(require 'mevedel-persistence)
(require 'mevedel-permission-rules)
(require 'mevedel-permissions)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

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
  :doc "lists notes from unopened files after reload and visits with one key"
  (mevedel-notes-list-test--workspace
    (kill-buffer (find-buffer-visiting second))
    (mevedel--clear-instruction-state workspace)
    (setq list-buffer (mevedel-notes-list workspace))
    (should-not (find-buffer-visiting second))
    (with-current-buffer list-buffer
      (should (string-match-p "Unsent" (buffer-string)))
      (should (string-match-p "first.el:1.*user.*First thought.*unsent"
                              (buffer-string)))
      (should (string-match-p "second.el:2.*user.*Second thought.*unsent"
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
      (should (equal "before\nsecond code\n" (buffer-string))))))

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

(mevedel-deftest mevedel-mentions-expand-user-input-notes
  (:quiet t)
  ,test
  (test)
  :doc "number mentions attach only selected code from unopened files"
  (mevedel-notes-list-test--workspace
    (kill-buffer (find-buffer-visiting second))
    (mevedel--clear-instruction-state workspace)
    (let* ((session (mevedel-session-create "mentions" workspace))
           (expansion (mevedel-mentions-expand-user-input "Explain @note:2" session))
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
                   (when (= (plist-get properties 'mevedel-id) 2) 'working)))))
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
