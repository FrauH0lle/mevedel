;;; test-mevedel-memory-write.el -- Retained memory snapshots -*- lexical-binding: t -*-

;;; Commentary:

;; Check exact reverse changes over real created, deleted, and mode-changed files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-write)
(require 'mevedel-session-control-fs)
(require 'mevedel-tool-patch)

(mevedel-deftest mevedel-memory-write-control-directory
  (:doc "shares target-local state across callers without aliasing memory and instruction ownership")
  (progn
    (dolist (prefix '("" "/ssh:host:"))
      (let ((workspace (concat prefix "/w")))
        (should (equal (concat workspace "/.mevedel/state/memory-write")
                       (mevedel-memory-write-control-directory
                        (concat workspace "/.mevedel/memory/"))))
        (should (equal (concat workspace "/.mevedel/state/memory-write/root")
                       (mevedel-memory-write-control-directory workspace)))
        (should (equal (concat workspace "/custom/.mevedel/state/memory-write/root")
                       (mevedel-memory-write-control-directory (concat workspace "/custom/"))))
        ;; Shared-note coordination lives outside the folder it cleans.
        (should (equal (concat workspace "/.mevedel/state/shared-notes")
                       (mevedel-memory-write-control-directory (concat workspace "/.mevedel/shared/"))))
        (should (equal (concat workspace "/notes/shared/.mevedel/state/memory-write/root")
                       (mevedel-memory-write-control-directory (concat workspace "/notes/shared"))))))))

(mevedel-deftest mevedel-memory-write--target
    (:vars* ((root (make-temp-file "mevedel-memory-state-" t))
             (memory (file-name-concat root "owner" ".mevedel" "memory"))
             (alias (file-name-concat root "other" ".mevedel" "memory"))
             (owner (mevedel-workspace--create :root (file-name-concat root "owner")))
             (other (mevedel-workspace--create :root (file-name-concat root "other")))
             (control (file-name-concat root "owner" ".mevedel" "state" "memory-write")))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "standard memory and its alias share one physical lock across workspaces"
  (progn
    (make-directory memory t)
    (make-directory (file-name-directory alias) t)
    (make-symbolic-link memory alias)
    (mevedel-workspace-identity-ensure (mevedel-workspace-root owner))
    (mevedel-workspace-identity-ensure (mevedel-workspace-root other))
    (let* ((scope (let ((mevedel-memory-dirs (list memory)))
                    (mevedel-memory-scope-capture owner)))
           (second (let ((mevedel-memory-dirs (list alias)))
                     (mevedel-memory-scope-capture other)))
           (id (caar (plist-get scope :roots)))
           (second-id (caar (plist-get second :roots))))
      (let ((target (mevedel-memory-write--target scope id))
            (contender (mevedel-memory-write--target second second-id)))
        (should (equal control target))
        (should (equal target contender))
        (make-directory target t)
        (let ((owner (mevedel-session-control-fs-lock-session-open target)))
          (unwind-protect (should-error (mevedel-session-control-fs-lock-session-open contender .5) :type 'file-error)
            (mevedel-session-control-fs-lock-session-close owner)))
        (mevedel-session-control-fs-lock-session-close (mevedel-session-control-fs-lock-session-open contender .5))
        (should-not (file-exists-p (file-name-concat memory ".mevedel")))))))

(mevedel-deftest mevedel-memory-write--changes
    (:vars* ((root (make-temp-file "mevedel-memory-reverse-" t))
             (removed (file-name-concat root "removed.md"))
             (created (file-name-concat root "created.md"))
             (changed (file-name-concat root "changed.md")))
     :after-each ((delete-directory root t)))
  ,test
  (test)
  :doc "restores deleted content, original modes and line endings, and expected absence"
  (progn
    (write-region "Deleted lesson.\n" nil removed nil 'silent)
    (write-region "Original\r\n" nil changed nil 'silent)
    (set-file-modes removed #o640)
    (set-file-modes changed #o640)
    (let* ((paths (list removed created changed))
           (before (mapcar (lambda (path) (cons (file-name-nondirectory path) (mevedel-memory-scope--snapshot path 32768))) paths)))
      (delete-file removed)
      (write-region "New lesson.\n" nil created nil 'silent)
      (write-region "Replaced.\n" nil changed nil 'silent)
      (set-file-modes changed #o600)
      (let* ((after (mapcar (lambda (path) (cons (file-name-nondirectory path) (mevedel-memory-scope--snapshot path 32768))) paths))
             (changes (mevedel-memory-write--changes after before)))
        (should (= 3 (length changes)))
        (mevedel-tool-patch-commit changes)
        (dolist (row before)
          (should (equal (cdr row) (mevedel-memory-scope--snapshot (plist-get (cdr row) :path) 32768))))
        (should-not (mevedel-memory-write--changes before before)))))
  :doc "byte-less snapshots differ by hash and carry their exact after-state"
  (let* ((before (list (cons "a.png" (list :path (file-name-concat root "a.png") :exists t
                                           :hash (make-string 64 ?a) :size 3 :mode #o644))))
         (after (list (cons "a.png" (list :path (file-name-concat root "a.png") :exists t
                                          :hash (make-string 64 ?b) :size 3 :mode #o644))))
         (change (car (mevedel-memory-write--changes before after))))
    (should (equal (cdar after) (plist-get change :after)))
    (should (eq 'write (plist-get change :action)))
    (should-not (plist-member change :bytes))
    (should (equal after (mevedel-memory-write--after before (list change))))
    (should-not (mevedel-memory-write--changes before before))))

(provide 'test-mevedel-memory-write)
;;; test-mevedel-memory-write.el ends here
