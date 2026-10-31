;;; test-mevedel-memory-write--abandon.el --- Checked pending write retirement -*- lexical-binding: t -*-

;;; Commentary:
;; Native control IO proves cancellation can retire an unchanged intent only.

;;; Code:

(require 'memory-history-fixtures
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "memory-history-fixtures"))
(require 'mevedel-memory-write)
(require 'mevedel-memory-decision)
(require 'mevedel-journal-claim)
(require 'mevedel-session-control-fs)
(require 'mevedel-workspace-identity)

(defvar mevedel-test-abandon-memory nil
  "Non-nil selects a two-file curated-memory intent instead of one note.")

(defvar mevedel-test-abandon-directory nil
  "Non-nil selects a real quiet empty-folder housekeeping intent.")

(defun mevedel-test-abandon-memory-accepted (workspace)
  "Accept a real two-file promotion for WORKSPACE without applying it."
  (let* ((scope (mevedel-memory-scope-capture workspace))
         (root (car (seq-find (lambda (row) (eq 'memory (plist-get (cdr row) :kind))) (plist-get scope :roots))))
         (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
    (unwind-protect
        (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
               (accepted
                (mevedel-memory-store-accept
                 workspace prepared
                 (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"new.md\"\ntype: \"project\"\n"
                                 "title: \"New\"\nhook: \"Guidance\"\nreason: \"Retain\"\nevidence: []\n---\nNew guidance.\n```\n"
                                 "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none") root)
                 nil "test:model" nil)))
          (mevedel-memory-store-publish workspace (plist-get prepared :id))
          accepted)
      (mevedel-journal-claim-settle claim 'cancelled ""))))

(defun mevedel-test-abandon-directory-accepted (workspace)
  "Accept real native empty-directory housekeeping for WORKSPACE without applying."
  (let* ((capture (mevedel-memory-notes-capture workspace nil :mode 'auto))
         (scope (mevedel-memory-scope-capture workspace))
         (scope (plist-put scope :roots (append (plist-get scope :roots) (list (plist-get capture :root)))))
         (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
    (unwind-protect
        (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
               (accepted (mevedel-memory-store-accept workspace prepared nil nil "housekeeping" nil)))
          (mevedel-memory-store-publish workspace (plist-get prepared :id))
          accepted)
      (mevedel-journal-claim-settle claim 'cancelled ""))))

(defmacro mevedel-test-abandon-fixture (&rest body)
  "Run BODY with authentic unchanged INTENT, coordinator CLAIM and TARGET."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-abandon-" t))
          (workspace (mevedel-workspace--create :root root))
          (mevedel-memory-dirs (when mevedel-test-abandon-memory (list (file-name-concat root "memory"))))
          (mevedel-memory-consolidation-mode 'auto)
          (mevedel-journal-cleanup--inhibit-scheduling t)
          (_identity (mevedel-workspace-identity-ensure root))
          (_directory (when mevedel-test-abandon-memory (make-directory (car mevedel-memory-dirs) t)))
          (_folder (when mevedel-test-abandon-directory
                     (let ((path (file-name-concat root ".mevedel" "shared" "empty")))
                       (make-directory path t)
                       (set-file-times path (seconds-to-time (- (float-time) (* 15 86400)))))))
          (accepted (cond (mevedel-test-abandon-memory (mevedel-test-abandon-memory-accepted workspace))
                          (mevedel-test-abandon-directory (mevedel-test-abandon-directory-accepted workspace))
                          (t (car (mevedel-test-memory-history-create workspace 1 1 100 t)))))
          (proposal (car (plist-get accepted :proposals)))
          (path (plist-get (cdr (car (plist-get proposal :before))) :path))
          (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
     (unwind-protect
         (mevedel-memory-write--call
          (plist-get (plist-get accepted :prepared) :scope) (plist-get proposal :root)
          (lambda (target)
            (let* ((intent (mevedel-memory-write--prepare workspace claim target accepted proposal))
                   (pin (mevedel-memory-write--pin intent)))
              ,@body)))
       (mevedel-journal-claim-settle claim 'cancelled "")
       (when-let* ((buffer (find-buffer-visiting path)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-directory root t))))

(mevedel-deftest mevedel-memory-write--abandon ()
  ,test
  (test)
  :doc "unchanged native intent is unmarked only after exact fence and creates no public decision"
  (mevedel-test-abandon-fixture
    (should (file-exists-p pin))
    (mevedel-memory-write--abandon workspace claim target intent)
    (should (equal (make-string 100 ?x) (mevedel-session-control-fs-read-file path)))
    (should-not (file-exists-p pin))
    (should (equal intent (mevedel-memory-write--read workspace (plist-get intent :id) (plist-get intent :hash))))
    (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))
    (let ((outcome (mevedel-journal-claim-outcome claim)))
      (should (eq 'cancelled (plist-get outcome :status)))
      (should (equal "" (plist-get outcome :payload))))
    (mevedel-memory-store--assert-owned target)
    (should-not (plist-get (car (mevedel-memory-write--list workspace)) :marked)))

  :doc "wrong intent hash target token directory or originating client cannot abandon"
  (dolist (change '(hash token directory client))
    (mevedel-test-abandon-fixture
      (let ((bad-intent (copy-tree intent)) (bad-target (copy-sequence target)))
        (pcase change
          ('hash (plist-put bad-intent :hash (make-string 64 ?a)))
          ('token (plist-put bad-target :owner (make-string 64 ?a)))
          ('directory (plist-put bad-target :directory (file-name-concat root "other-claims"))))
        (if (eq change 'client)
            (cl-letf (((symbol-function 'mevedel-workspace-identity-client) (lambda () "other-client")))
              (should-error (mevedel-memory-write--abandon workspace claim bad-target bad-intent)))
          (should-error (mevedel-memory-write--abandon workspace claim bad-target bad-intent)))
        (should (file-exists-p pin))
        (should (file-exists-p path))
        (should-not (mevedel-journal-claim-outcome claim)))))

  :doc "after or intervening states cannot abandon or settle coordinator"
  (dolist (change '(after intervening))
    (mevedel-test-abandon-fixture
      (if (eq change 'after) (delete-file path)
        (write-region "Intervening edit" nil path nil 'silent))
      (should-error (mevedel-memory-write--abandon workspace claim target intent))
      (should (file-exists-p pin))
      (should-not (mevedel-journal-claim-outcome claim))))

  :doc "mixed two-file memory state cannot abandon or release its marker"
  (let ((mevedel-test-abandon-memory t))
    (mevedel-test-abandon-fixture
      (let* ((change (car (plist-get intent :changes)))
             (before (plist-get change :before))
             (after (cdr (seq-find (lambda (row) (equal (plist-get before :path) (plist-get (cdr row) :path)))
                                   (plist-get intent :after)))))
        (should (= 2 (length (plist-get intent :changes))))
        (mevedel-memory-write--mutate target before after)
        (should (eq 'mixed (mevedel-memory-write--state intent)))
        (should-error (mevedel-memory-write--abandon workspace claim target intent))
        (should (file-exists-p pin))
        (should-not (mevedel-journal-claim-outcome claim)))))

  :doc "foreign coordinator outcome status payload or token cannot authorize marker deletion"
  (dolist (change '(failed completed payload owner))
    (mevedel-test-abandon-fixture
      (let ((settle (symbol-function 'mevedel-journal-claim-settle)) injected)
        (cl-letf (((symbol-function 'mevedel-journal-claim-settle)
                   (lambda (token status payload)
                     (if (equal (plist-get token :owner) (plist-get claim :owner))
                         (progn
                           (setq injected t)
                           (funcall settle token (if (memq change '(failed completed)) change status)
                                    (if (eq change 'payload) "foreign" payload))
                           (when (eq change 'owner)
                             (write-region
                              (json-serialize (append (plist-put (mevedel-journal-claim--record token) :owner (make-string 64 ?a))
                                                      (list :status "cancelled" :payload "")))
                              nil (mevedel-journal-claim--path token t) nil 'silent)))
                       (funcall settle token status payload)))))
          (should-error (mevedel-memory-write--abandon workspace claim target intent)))
        (should injected)
        (should (file-exists-p pin))
        (should (file-exists-p path)))))

  :doc "late changed marker disk mode or target settlement rejects guarded delete and retains evidence"
  (dolist (change '(marker bytes mode target))
    (mevedel-test-abandon-fixture
      (let ((run (symbol-function 'mevedel-session-control-fs-run-program)) injected)
        (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                   (lambda (program &optional directory)
                     (when (and (not injected)
                                (seq-some (lambda (op) (and (eq (plist-get op :op) 'delete-file)
                                                           (equal pin (plist-get op :path)))) program))
                       (setq injected t)
                       (pcase change
                         ('marker (write-region (make-string 64 ?a) nil pin nil 'silent))
                         ('bytes (write-region "Intervening edit" nil path nil 'silent))
                         ('mode (set-file-modes path #o400))
                         ('target (mevedel-journal-claim-settle target 'cancelled ""))))
                     (funcall run program directory))))
          (should-error (mevedel-memory-write--abandon workspace claim target intent)))
        (should injected)
        (should (file-exists-p pin))
        (should (file-exists-p path))
        (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome claim) :status))))))

  :doc "original target expiry after outside assertion rejects native marker deletion"
  (let ((acquire (symbol-function 'mevedel-journal-claim-acquire)))
    (cl-letf (((symbol-function 'mevedel-journal-claim-acquire)
               (lambda (directory seconds &optional deadline)
                 (funcall acquire directory (if (string-suffix-p "/state/shared-notes/claims" directory) 5 seconds) deadline))))
      (mevedel-test-abandon-fixture
        (let ((run (symbol-function 'mevedel-session-control-fs-run-program))
              injected owned-before-wait deadline-crossed)
          (should (<= (- (plist-get target :expires-at)
                         (mevedel-session-control-fs-target-time (plist-get target :directory))) 5))
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (program &optional directory)
                       (when (and (not injected)
                                  (seq-some (lambda (op) (and (eq (plist-get op :op) 'delete-file)
                                                             (equal pin (plist-get op :path)))) program))
                         (setq injected t)
                         (setq owned-before-wait
                               (mevedel-journal-claim-owned-p target))
                         (let ((deadline (+ (float-time) 10)))
                           (while (and (< (float-time) deadline)
                                       (< (mevedel-session-control-fs-target-time (plist-get target :directory))
                                          (plist-get target :expires-at)))
                             (sleep-for .05)))
                         (setq deadline-crossed
                               (>= (mevedel-session-control-fs-target-time
                                    (plist-get target :directory))
                                   (plist-get target :expires-at))))
                       (funcall run program directory))))
            (should-error (mevedel-memory-write--abandon workspace claim target intent)))
          (should injected)
          (should owned-before-wait)
          (should deadline-crossed)
          (should (file-exists-p pin))
          (should (equal (make-string 100 ?x) (mevedel-session-control-fs-read-file path)))
          (should-not (mevedel-journal-claim-outcome target))
          (let ((outcome (mevedel-journal-claim-outcome claim)))
            (should (eq 'cancelled (plist-get outcome :status)))
            (should (equal "" (plist-get outcome :payload))))
          (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id)))))))

  :doc "late child in authentic empty-directory intent fails native listing proof and preserves pending evidence"
  (let ((mevedel-test-abandon-directory t))
    (mevedel-test-abandon-fixture
      (should (eq 'note-prune (plist-get proposal :action)))
      (should (plist-get (cdar (plist-get intent :before)) :empty))
      (mevedel-memory-write--abandon workspace claim target intent)
      (should-not (file-exists-p pin))
      (should (file-directory-p path))
      (should-not (directory-files path nil directory-files-no-dot-files-regexp))
      (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id))))
    (mevedel-test-abandon-fixture
      (should (eq 'note-prune (plist-get proposal :action)))
      (should (plist-get (cdar (plist-get intent :before)) :directory))
      (should (plist-get (cdar (plist-get intent :before)) :empty))
      (should (eq 'before (mevedel-memory-write--state intent)))
      (let ((run (symbol-function 'mevedel-session-control-fs-run-program))
            (child (file-name-concat path "arrived.txt")) injected)
        (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                   (lambda (program &optional directory)
                     (when (and (not injected)
                                (seq-some (lambda (op) (and (eq (plist-get op :op) 'delete-file)
                                                           (equal pin (plist-get op :path)))) program))
                       (setq injected t)
                       (write-region "New child" nil child nil 'silent))
                     (funcall run program directory))))
          (should-error (mevedel-memory-write--abandon workspace claim target intent)))
        (should injected)
        (should (file-exists-p pin))
        (should (equal "New child" (mevedel-session-control-fs-read-file child)))
        (let ((outcome (mevedel-journal-claim-outcome claim)))
          (should (eq 'cancelled (plist-get outcome :status)))
          (should (equal "" (plist-get outcome :payload))))
        (should-not (mevedel-memory-decision-status workspace (plist-get proposal :id))))))

  :doc "unsaved buffer arriving across coordinator settlement blocks retirement without replacing its draft"
  (mevedel-test-abandon-fixture
    (let ((settle (symbol-function 'mevedel-journal-claim-settle)) buffer injected)
      (cl-letf (((symbol-function 'mevedel-journal-claim-settle)
                 (lambda (token status payload)
                   (prog1 (funcall settle token status payload)
                     (when (equal (plist-get token :owner) (plist-get claim :owner))
                       (setq injected t buffer (find-file-noselect path))
                       (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved draft")))))))
        (should-error (mevedel-memory-write--abandon workspace claim target intent)))
      (should injected)
      (should (file-exists-p pin))
      (should (equal (make-string 100 ?x) (mevedel-session-control-fs-read-file path)))
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should (string-suffix-p "unsaved draft" (buffer-string))))))

  :doc "IO failure or quit after coordinator fencing retains marker and propagates retirement failure"
  (dolist (failure '(io quit))
    (mevedel-test-abandon-fixture
      (let ((run (symbol-function 'mevedel-session-control-fs-run-program)) injected interrupted)
        (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                   (lambda (program &optional directory)
                     (when (seq-some (lambda (op) (and (eq (plist-get op :op) 'delete-file)
                                                       (equal pin (plist-get op :path)))) program)
                       (setq injected t)
                       (if (eq failure 'quit) (signal 'quit nil) (error "Injected abandonment IO failure")))
                     (funcall run program directory))))
          (if (eq failure 'quit)
              (condition-case nil
                  (progn (mevedel-memory-write--abandon workspace claim target intent)
                         (ert-fail "Quit was swallowed"))
                (quit (setq interrupted t)))
            (should (string-match-p "Injected abandonment IO failure"
                                    (error-message-string (should-error
                                                           (mevedel-memory-write--abandon workspace claim target intent)))))))
        (should injected)
        (when (eq failure 'quit) (should interrupted))
        (should (file-exists-p pin))
        (should (equal (make-string 100 ?x) (mevedel-session-control-fs-read-file path)))
        (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome claim) :status))))))

  :doc "instrumented separate-authority dispatch never sends coordinator paths to target deletion executor"
  (mevedel-test-abandon-fixture
    (let ((run (symbol-function 'mevedel-session-control-fs-run-program)) deletion)
      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                 (lambda (program &optional directory)
                   (when (seq-some (lambda (op) (and (eq (plist-get op :op) 'delete-file)
                                                     (equal pin (plist-get op :path)))) program)
                     (setq deletion t)
                     (should (equal directory (plist-get target :directory)))
                     (dolist (op program)
                       (should-not (string-prefix-p (plist-get claim :directory) (plist-get op :path)))
                       (should (memq (plist-get op :op)
                                     '(verify absent before-time verify-hash verify-mode directory-p verify-list delete-file)))
                       (when (eq (plist-get op :op) 'delete-file) (should (equal pin (plist-get op :path))))))
                   (funcall run program directory))))
        (mevedel-memory-write--abandon workspace claim target intent))
      (should deletion)
      (should-not (file-exists-p pin)))))

(provide 'test-mevedel-memory-write-abandon)
;;; test-mevedel-memory-write-abandon.el ends here
