;;; test-mevedel-memory-notes-citations.el --- Fresh note deletion holds -*- lexical-binding: t -*-

;;; Commentary:

;; Complete current citation observations and same-byte age changes are checked
;; with real files.  Injected interleavings exercise the production helpers.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-notes)
(require 'mevedel-memory-scope)
(require 'mevedel-session-control-fs)
(require 'mevedel-system)
(require 'mevedel-workspace-identity)

(mevedel-deftest mevedel-memory-notes--citation-instructions ()
  (let* ((root (file-name-as-directory (make-temp-file "citation-instructions-" t)))
         (workspace (mevedel-workspace--create :root root)))
    (unwind-protect
        (let ((paths (mevedel-memory-notes--citation-instructions workspace nil)))
          (should (equal (list (file-name-concat root "AGENTS.local.md")
                               (file-name-concat root "AGENTS.md")) paths))
          ;; Unresolved old root IDs are validated against the fresh scope by
          ;; the public helper, not guessed while collecting instruction paths.
          (should (equal paths (mevedel-memory-notes--citation-instructions
                                workspace '(:omissions ((:root "unknown")))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-memory-notes--citation-unsaved-p ()
  (let* ((root (file-name-as-directory (make-temp-file "citation-buffers-" t)))
         (path (file-name-concat root "new.MD"))
         (buffer (find-file-noselect path)))
    (unwind-protect
        (progn
          (should-not (mevedel-memory-notes--citation-unsaved-p (list path) nil))
          (with-current-buffer buffer (insert "Draft"))
          (should (mevedel-memory-notes--citation-unsaved-p (list path) nil))
          (should (mevedel-memory-notes--citation-unsaved-p nil (list (list :dir root))))
          (should-not (mevedel-memory-notes--citation-unsaved-p nil (list (list :dir (file-name-concat root "other"))))))
      (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-memory-notes--note-hold ()
  (let* ((directory (make-temp-file "note-hold-missing-" t))
         (workspace (mevedel-workspace--create :root directory)))
    (unwind-protect
        (should (equal "Captured note metadata is unavailable"
                       (mevedel-memory-notes--note-hold workspace (list :dir directory) "absent.md")))
      (delete-directory directory t))))

(mevedel-deftest mevedel-memory-notes-citation-hold
    (:vars* ((root (file-name-as-directory (make-temp-file "note-citation-" t)))
             (workspace (mevedel-workspace--create :root root))
             (memory (file-name-concat root ".mevedel/memory"))
             (index (file-name-concat memory "MEMORY.md"))
             (topic (file-name-concat memory "topic.md"))
             (local-instructions (file-name-concat root "AGENTS.local.md"))
             (mevedel-memory-dirs '(".mevedel/memory")))
     :before-each ((mevedel-workspace-identity-ensure root)
                   (make-directory memory t)
                   (write-region "# Memory\n" nil index nil 'silent))
     :after-each ((dolist (buffer (buffer-list))
                    (when-let* ((path (buffer-local-value 'buffer-file-name buffer))
                                ((string-prefix-p root path)))
                      (with-current-buffer buffer (set-buffer-modified-p nil))
                      (kill-buffer buffer)))
                  (delete-directory root t)))
  ,test
  (test)
  :doc "complete current absence includes empty known-absent instructions without changing source mtimes"
  (let ((mtime (file-attribute-modification-time (file-attributes memory)))
        (scope (mevedel-memory-scope-capture workspace)))
    (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))
    (should (equal mtime (file-attribute-modification-time (file-attributes memory))))
    (should-not (file-exists-p local-instructions)))
  :doc "memory index citations and containing-folder citations hold until repaired"
  (progn
    (write-region "See work://shared/lab/.\n" nil index nil 'silent)
    (should (string-search "still reference" (mevedel-memory-notes-citation-hold workspace "lab/note.md")))
    (write-region "No working-note dependency.\n" nil index nil 'silent)
    (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md")))
  :doc "a generic retained-root working-material policy is not a dangling note citation"
  (progn
    (write-region "Put temporary notes in work://shared/ (.mevedel/shared/).\n" nil
                  (file-name-concat root "AGENTS.md") nil 'silent)
    (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md")))
  :doc "new topics and newly configured roots are checked after the original capture"
  (let ((scope (mevedel-memory-scope-capture workspace)))
    (write-region "See .mevedel/shared/lab/note.md\n" nil topic nil 'silent)
    (should (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))
    (write-region "No citation\n" nil topic nil 'silent)
    (let* ((other (file-name-concat root ".mevedel/other-memory"))
           (mevedel-memory-dirs '(".mevedel/memory" ".mevedel/other-memory")))
      (make-directory other t)
      (write-region "work://shared/lab/note.md\n" nil (file-name-concat other "MEMORY.md") nil 'silent)
      (should (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))
      (write-region "No citation\n" nil (file-name-concat other "MEMORY.md") nil 'silent)
      (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))))
  :doc "frozen nested instruction paths and absent companion files survive root-default lookup"
  (let* ((nested (file-name-concat root "nested"))
         (agents (file-name-concat nested "AGENTS.md"))
         (local (file-name-concat nested "AGENTS.local.md")))
    (make-directory nested t)
    (write-region "Unrelated instructions.\n" nil agents nil 'silent)
    (let ((scope (mevedel-memory-scope-capture
                  workspace nil (list :roots (mevedel-system--memory-roots workspace)
                                      :instructions (list agents)))))
      (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))
      (write-region "Read work://shared/lab/note.md\n" nil local nil 'silent)
      (delete-file agents)
      (should (string-search "still reference" (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope)))
      (delete-file local)
      (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))))
  :doc "previously omitted instruction candidates can be repaired but never silently disappear"
  (let* ((nested (file-name-concat root "nested"))
         (agents (file-name-concat nested "AGENTS.md")))
    (make-directory nested t)
    (write-region (make-string (* 33 1024) ?x) nil agents nil 'silent)
    (let ((scope (mevedel-memory-scope-capture
                  workspace nil (list :roots (mevedel-system--memory-roots workspace)
                                      :instructions (list agents)))))
      (should (plist-get scope :omissions))
      (write-region "work://shared/lab/note.md\n" nil agents nil 'silent)
      (should (string-search "still reference" (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope)))
      (write-region "Repaired complete instructions.\n" nil agents nil 'silent)
      (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))))
  :doc "oversized, omitted, unsupported Markdown and incomplete inventories cannot prove absence"
  (progn
    (write-region (make-string (* 33 1024) ?x) nil topic nil 'silent)
    (should (string-search "incomplete" (mevedel-memory-notes-citation-hold workspace "lab/note.md")))
    (delete-file topic)
    (write-region "work://shared/lab/note.md" nil (file-name-concat memory "upper.MD") nil 'silent)
    (should (string-search "unavailable" (mevedel-memory-notes-citation-hold workspace "lab/note.md")))
    (delete-file (file-name-concat memory "upper.MD"))
    (write-region "No citation" nil topic nil 'silent)
    (let ((mevedel-memory-scope--max-entries 1))
      (should (string-search "incomplete" (mevedel-memory-notes-citation-hold workspace "lab/note.md")))))
  :doc "a recovered excluded index releases its exact old root but missing or removed roots stay unknown"
  (progn
    (write-region (make-string (* 33 1024) ?x) nil index nil 'silent)
    (let ((scope (mevedel-memory-scope-capture workspace)))
      (should (plist-get scope :omissions))
      (should (= 1 (length (plist-get scope :roots))))
      (should (string-search "incomplete" (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope)))
      (write-region "Recovered complete index.\n" nil index nil 'silent)
      (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))
      (let ((mevedel-memory-dirs nil))
        (should (string-search "unavailable" (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))))
      (should (string-search "unavailable"
                             (mevedel-memory-notes-citation-hold
                              workspace "lab/note.md" '(:omissions ((:root "unknown"))))))))
  :doc "a body changed after its pinned read invalidates the absence proof"
  (let ((snapshot (symbol-function 'mevedel-memory-scope--snapshot)) changed)
    (cl-letf (((symbol-function 'mevedel-memory-scope--snapshot)
               (lambda (path &rest args)
                 (prog1 (apply snapshot path args)
                   (when (and (equal path index) (not changed))
                     (setq changed t)
                     (write-region "work://shared/lab/note.md\n" nil index nil 'silent))))))
      (should (string-search "changed during" (mevedel-memory-notes-citation-hold workspace "lab/note.md"))))
    (should changed)
    (should (string-search "still reference" (mevedel-memory-notes-citation-hold workspace "lab/note.md"))))
  :doc "a file arriving during snapshot verification invalidates the complete inventory"
  (let ((check (symbol-function 'mevedel-memory-scope-check)) changed)
    (cl-letf (((symbol-function 'mevedel-memory-scope-check)
               (lambda (&rest args)
                 (prog1 (apply check args)
                   (unless changed
                     (setq changed t)
                     (write-region "work://shared/lab/note.md\n" nil topic nil 'silent))))))
      (should (string-search "inventory changed" (mevedel-memory-notes-citation-hold workspace "lab/note.md"))))
    (should changed))
  :doc "a configuration change during verification cannot hide a newly applicable memory root"
  (let ((check (symbol-function 'mevedel-memory-scope-check)) changed)
    (cl-letf (((symbol-function 'mevedel-memory-scope-check)
               (lambda (&rest args)
                 (prog1 (apply check args)
                   (unless changed
                     (setq changed t mevedel-memory-dirs '(".mevedel/memory" ".mevedel/new-memory")))))))
      (should (string-search "configuration changed" (mevedel-memory-notes-citation-hold workspace "lab/note.md"))))
    (should changed))
  :doc "modified existing memory and new unsaved Markdown topics hold without reading drafts"
  (dolist (path (list index (file-name-concat memory "new-topic.MD")))
    (let ((buffer (find-file-noselect path)))
      (with-current-buffer buffer (goto-char (point-max)) (insert "draft citation"))
      (should (string-search "unsaved" (mevedel-memory-notes-citation-hold workspace "lab/note.md")))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md"))))
  :doc "an unsaved disk-absent local instruction draft is unknown, not proof of absence"
  (let ((scope (mevedel-memory-scope-capture workspace))
        (buffer (find-file-noselect local-instructions)))
    (with-current-buffer buffer (insert "Draft citation"))
    (should-not (file-exists-p local-instructions))
    (should (string-search "unsaved" (mevedel-memory-notes-citation-hold workspace "lab/note.md" scope))))
  :doc "a draft beginning after disk reads is caught by the final local-buffer check"
  (let ((check (symbol-function 'mevedel-memory-scope-check)) changed)
    (cl-letf (((symbol-function 'mevedel-memory-scope-check)
               (lambda (&rest args)
                 (prog1 (apply check args)
                   (unless changed
                     (setq changed t)
                     (with-current-buffer (find-file-noselect (file-name-concat memory "new.md"))
                       (setq-local create-lockfiles nil)
                       (insert "Unsaved draft citation")))))))
      (should (string-search "unsaved" (mevedel-memory-notes-citation-hold workspace "lab/note.md"))))
    (should changed))
  :doc "unrelated unsaved buffers do not turn a complete citation scope into a hold"
  (let ((buffer (find-file-noselect (file-name-concat root "unrelated.md"))))
    (with-current-buffer buffer (insert "Draft"))
    (should-not (mevedel-memory-notes-citation-hold workspace "lab/note.md"))))

(mevedel-deftest mevedel-memory-notes-mutation-hold
    (:vars* ((root (file-name-as-directory (make-temp-file "note-mutation-hold-" t)))
             (workspace (mevedel-workspace--create :root root))
             (shared (mevedel-resource-work-shared-directory workspace))
             (path (file-name-concat shared "note.md"))
             (mevedel-memory-dirs nil)
             (now 1800000000)
             (body (mevedel-working-note-normalize "# Note\n" "owner" "2000-01-01T00:00:00Z"))
             snapshot note-root)
     :before-each ((mevedel-workspace-identity-ensure root)
                   (make-directory shared t)
                   (write-region body nil path nil 'silent)
                   (set-file-times path (seconds-to-time (- now (* 7 86400))))
                   (setq snapshot (mevedel-memory-scope--snapshot path (* 32 1024))
                         note-root (list :dir shared :before (list (cons "note.md" snapshot)))))
     :after-each ((dolist (buffer (buffer-list))
                    (when-let* ((file (buffer-local-value 'buffer-file-name buffer))
                                ((string-prefix-p root file)))
                      (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer)))
                  (delete-directory root t)))
  (cl-letf (((symbol-function 'mevedel-session-control-fs-observation-time) (lambda (_) now)))
    ,test)
  (test)
  :doc "an exact unchanged age-ready note with no citations is not held"
  (should-not (mevedel-memory-notes-mutation-hold workspace note-root "note.md"))
  :doc "a same-byte touch after capture resets age without weakening the byte proof"
  (progn
    (set-file-times path (seconds-to-time now))
    (should (mevedel-memory-notes-current-p snapshot))
    (should (string-search "not age-ready" (mevedel-memory-notes-mutation-hold workspace note-root "note.md")))
    (set-file-times path (seconds-to-time (- now (* 7 86400))))
    (should-not (mevedel-memory-notes-mutation-hold workspace note-root "note.md")))
  :doc "future mtimes and changed bytes remain held"
  (progn
    (set-file-times path (seconds-to-time (1+ now)))
    (should (string-search "future-mtime" (mevedel-memory-notes-mutation-hold workspace note-root "note.md")))
    (write-region "changed" nil path nil 'silent)
    (should (string-search "changed since capture" (mevedel-memory-notes-mutation-hold workspace note-root "note.md"))))
  :doc "same-byte touches during citation preparation are caught by the second note guard"
  (let ((citation (symbol-function 'mevedel-memory-notes-citation-hold)) touched)
    (cl-letf (((symbol-function 'mevedel-memory-notes-citation-hold)
               (lambda (&rest args)
                 (prog1 (apply citation args)
                   (setq touched t)
                   (set-file-times path (seconds-to-time now))))))
      (should (string-search "not age-ready" (mevedel-memory-notes-mutation-hold workspace note-root "note.md"))))
    (should touched))
  :doc "unsaved note buffers hold even when captured disk bytes and age match"
  (progn
    (with-current-buffer (find-file-noselect path) (goto-char (point-max)) (insert "Draft"))
    (should (string-search "unsaved buffer" (mevedel-memory-notes-mutation-hold workspace note-root "note.md"))))
  :doc "fresh citations hold an otherwise age-ready unchanged note"
  (progn
    (write-region "work://shared/note.md\n" nil (file-name-concat root "AGENTS.local.md") nil 'silent)
    (should (string-search "still reference" (mevedel-memory-notes-mutation-hold workspace note-root "note.md")))))

(provide 'test-mevedel-memory-notes-citations)
;;; test-mevedel-memory-notes-citations.el ends here
