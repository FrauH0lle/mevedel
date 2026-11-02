;;; test-mevedel-overlays-delete.el -- Bulk instruction deletion tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise workspace scope and durable cleanup through interactive commands.

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-directive-source)
(require 'mevedel-directive)
(require 'mevedel-instruction-registry)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-instruction-test-support"))

(mevedel-deftest mevedel--delete-all-instructions-of-type
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "commands isolate types and workspaces, including nested directives"
  (dolist (command '(mevedel-delete-all-references
                     mevedel-delete-all-directives
                     mevedel-delete-all-instructions))
    (let* ((mevedel--instruction-states (make-hash-table :test #'equal))
           (workspace-a (mevedel-workspace--create :type 'file :id "a"))
           (workspace-b (mevedel-workspace--create :type 'file :id "b"))
           (reference-a (mevedel-instruction-test--make-reference "reference\n" workspace-a))
           (directive-a (mevedel-instruction-test--make-directive "parent detail\n" "Parent" workspace-a))
           (reference-b (mevedel-instruction-test--make-reference "other\n" workspace-b))
           (directive-b (mevedel-instruction-test--make-directive "other\n" "Other" workspace-b))
           (record (with-current-buffer (car directive-a)
                     (mevedel--directive-record (cdr directive-a))))
           child prompts messages)
      (unwind-protect
          (progn
            (with-current-buffer (car directive-a)
              (setq child (mevedel--create-directive-in
                           (current-buffer) 8 14 nil "Detail")))
            (with-current-buffer (car reference-a)
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (prompt) (push prompt prompts) t)))
                (mevedel-test--with-captured-messages messages
                  (call-interactively command))))
            (should (= 1 (length prompts)))
            (should (string-match-p
                     (if (eq command 'mevedel-delete-all-instructions)
                         "3 instructions in 2 buffers"
                       (if (eq command 'mevedel-delete-all-references)
                           "1 reference in 1 buffer"
                         "2 directives in 1 buffer"))
                     (car prompts)))
            (should (string-search
                     (concat "Deleted "
                             (cond
                              ((eq command 'mevedel-delete-all-instructions)
                               "3 instructions in 2 buffers")
                              ((eq command 'mevedel-delete-all-references)
                               "1 reference in 1 buffer")
                              (t "2 directives in 1 buffer")))
                     messages))
            (should (eq (not (eq command 'mevedel-delete-all-directives))
                        (null (overlay-buffer (cdr reference-a)))))
            (should (eq (eq command 'mevedel-delete-all-references)
                        (and (overlay-buffer (cdr directive-a)) t)))
            (should (eq (eq command 'mevedel-delete-all-references)
                        (and (overlay-buffer child) t)))
            (should (eq (eq command 'mevedel-delete-all-references)
                        (and (memq record (mevedel-workspace-directives workspace-a)) t)))
            (should (overlay-buffer (cdr reference-b)))
            (should (overlay-buffer (cdr directive-b)))
            (should (= 1 (length (mevedel-workspace-directives workspace-b)))))
        (mapc #'mevedel-instruction-test--discard
              (list reference-a directive-a reference-b directive-b)))))

  :doc "all no-workspace buffers share the global bucket, not workspace buckets"
  (let* ((workspace (mevedel-workspace--create :type 'file :id "project"))
         (project (mevedel-instruction-test--make-reference "project\n" workspace))
         (first (generate-new-buffer " *global-first*"))
         (second (generate-new-buffer " *global-second*"))
         overlays)
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (insert "global")
              (push (mevedel--create-reference-in buffer 1 7) overlays)))
          (with-current-buffer first
            (mevedel-test--with-captured-messages nil
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
                (call-interactively #'mevedel-delete-all-references))))
          (should-not (seq-some #'overlay-buffer overlays))
          (should (overlay-buffer (cdr project))))
      (kill-buffer first)
      (kill-buffer second)
      (mevedel-instruction-test--discard project)))

  :doc "closed source restoration retains the registry workspace owner"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (file (nth 2 fixture))
         (reference (mevedel-instruction-test--make-reference "reference\n" workspace)))
    (unwind-protect
        (progn
          (kill-buffer (nth 3 fixture))
          (with-current-buffer (car reference)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
              (mevedel-test--with-captured-messages nil
                (call-interactively #'mevedel-delete-all-directives))))
          (should-not (mevedel-workspace-directives workspace))
          (should (overlay-buffer (cdr reference))))
      (when-let* ((buffer (find-buffer-visiting file)))
        (setf (nth 3 fixture) buffer))
      (mevedel-instruction-test--discard reference)
      (mevedel-instruction-test--discard-source fixture))))

(mevedel-deftest mevedel-delete-all-directives
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "Source-missing inclusion is optional, defaults no, and is workspace-scoped"
  (dolist (selection '((mevedel-delete-all-directives nil)
                       (mevedel-delete-all-directives t)
                       (mevedel-delete-all-instructions nil)
                       (mevedel-delete-all-instructions t)))
    (let* ((fixture (mevedel-instruction-test--source-fixture))
           (command (car selection))
           (include (cadr selection))
           (workspace (nth 1 fixture))
           (buffer (nth 3 fixture))
           (record (nth 5 fixture))
           (visible (mevedel-instruction-test--make-directive "visible\n" "Visible" workspace))
           (other-workspace (mevedel-workspace--create :type 'file :id "other"))
           (other (mevedel-directive--create :id "other" :anchor '(:state source-missing)))
           (archived (mevedel-directive--create :id "archive" :anchor '(:state archived)))
           prompts)
      (unwind-protect
          (progn
            (mevedel-workspace-add-directive other-workspace other)
            (mevedel-workspace-add-directive workspace archived)
            (delete-file (nth 2 fixture))
            (mevedel--mark-buffer-source-missing buffer)
            (with-current-buffer (car visible)
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (prompt)
                           (push prompt prompts)
                           (if (string-match-p "Source-missing" prompt)
                               (progn
                                 (should (eq 'skip (lookup-key query-replace-map (kbd "RET"))))
                                 include)
                             t))))
                (mevedel-test--with-captured-messages nil
                  (call-interactively command))))
            (should (= 2 (length prompts)))
            (should (string-match-p "1 Source-missing" (cadr prompts)))
            (should (eq (not include) (and (memq record (mevedel-workspace-directives workspace)) t)))
            (should (memq archived (mevedel-workspace-directives workspace)))
            (should (memq other (mevedel-workspace-directives other-workspace)))
            (should-not (overlay-buffer (cdr visible))))
        (mevedel-instruction-test--discard visible)
        (mevedel-instruction-test--discard-source fixture))))

  :doc "Source-missing-only deletion counts records without inventing buffers"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         (id (overlay-get (nth 4 fixture) 'mevedel-id))
         child-id messages)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq child-id
                  (mevedel--instruction-id
                   (mevedel--create-directive-in buffer 9 12 nil "Detail"))))
          (delete-file (nth 2 fixture))
          (mevedel--mark-buffer-source-missing buffer)
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
              (mevedel-test--with-captured-messages messages
                (call-interactively #'mevedel-delete-all-directives))))
          (should (string-search "Deleted 1 directive in 0 buffers" messages))
          (should-not (mevedel-workspace-directives workspace))
          (should-not
           (gethash id (plist-get
                        (mevedel--instruction-state
                         (mevedel--instruction-workspace-key workspace))
                        :id-usage-map)))
          (should-not
           (gethash child-id (plist-get
                              (mevedel--instruction-state
                               (mevedel--instruction-workspace-key workspace))
                              :id-usage-map))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "activity blocks all deletion before references or directives are removed"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (record (nth 5 fixture))
         (reference (mevedel-instruction-test--make-reference "reference\n" workspace)))
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record)
                (list (mevedel-instruction-test--attempt)))
          (with-current-buffer (car reference)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
              (should-error (call-interactively #'mevedel-delete-all-instructions)
                            :type 'user-error)))
          (should (overlay-buffer (cdr reference)))
          (should (overlay-buffer (nth 4 fixture)))
          (should (memq record (mevedel-workspace-directives workspace))))
      (mevedel-instruction-test--discard reference)
      (mevedel-instruction-test--discard-source fixture))))

(mevedel-deftest mevedel-delete-all-references
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "declining the confirmation leaves references and ID state intact"
  (let* ((workspace (mevedel-workspace--create :type 'file :id "cancel"))
         (cell (mevedel-instruction-test--make-reference "reference\n" workspace)))
    (unwind-protect
        (with-current-buffer (car cell)
          (let ((state (mevedel--instruction-state
                        (mevedel--instruction-workspace-key workspace))))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
              (should-error (call-interactively #'mevedel-delete-all-references)
                            :type 'user-error))
            (should (overlay-buffer (cdr cell)))
            (should (eq state (mevedel--instruction-state
                               (mevedel--instruction-workspace-key workspace))))))
      (mevedel-instruction-test--discard cell)))

  :doc "reference deletion preserves Source-missing and archived records without asking about them"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (record (nth 5 fixture))
         (archived (mevedel-directive--create :id "archive" :anchor '(:state archived)))
         (reference (mevedel-instruction-test--make-reference "reference\n" workspace))
         prompts)
    (unwind-protect
        (progn
          (mevedel-workspace-add-directive workspace archived)
          (delete-file (nth 2 fixture))
          (mevedel--mark-buffer-source-missing (nth 3 fixture))
          (with-current-buffer (car reference)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt) (push prompt prompts) t)))
              (mevedel-test--with-captured-messages nil
                (call-interactively #'mevedel-delete-all-references))))
          (should (= 1 (length prompts)))
          (should (memq record (mevedel-workspace-directives workspace)))
          (should (memq archived (mevedel-workspace-directives workspace)))
          (should-not (overlay-buffer (cdr reference))))
      (mevedel-instruction-test--discard reference)
      (mevedel-instruction-test--discard-source fixture))))

(mevedel-deftest mevedel--delete-source-missing-directive ()
  ,test
  (test)
  :doc "source-owner deletion enforces missing-source and activity guards"
  (let* ((record (mevedel-directive--create :id "guard" :anchor '(:state attached)))
         (workspace (mevedel-workspace--create :type 'file :id "guard"
                                              :directives (list record))))
    (should-error (mevedel--delete-source-missing-directive record workspace)
                  :type 'user-error)
    (setf (mevedel-directive-anchor record) '(:state source-missing)
          (mevedel-directive-attempts record) (list (mevedel-instruction-test--attempt)))
    (should-error (mevedel--delete-source-missing-directive record workspace)
                  :type 'user-error)
    (should (memq record (mevedel-workspace-directives workspace)))
    (setf (mevedel-directive-attempts record) nil)
    (mevedel--delete-source-missing-directive record workspace)
    (should-not (mevedel-workspace-directives workspace))))

(mevedel-deftest mevedel-delete-all-instructions
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "deleting a detached parent also retires hidden subdirective identities"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         child-id)
    (unwind-protect
        (with-current-buffer buffer
          (setq child-id
                (mevedel--instruction-id
                 (mevedel--create-directive-in buffer 9 12 nil "Detail")))
          (delete-region 8 14)
          (mevedel-test--with-captured-messages nil
            (mevedel-delete-all-instructions))
          (should-not (mevedel-workspace-directives workspace))
          (should-not
           (gethash child-id (plist-get
                              (mevedel--instruction-state
                               (mevedel--instruction-workspace-key workspace))
                              :id-usage-map))))
      (mevedel-instruction-test--discard-source fixture))))

(mevedel-deftest mevedel--retire-instruction-identity
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "deleting visible or Source-missing directives removes dangling reference links"
  (dolist (missing '(nil t))
    (let* ((fixture (mevedel-instruction-test--source-fixture))
           (workspace (nth 1 fixture))
           (buffer (nth 3 fixture))
           (directive (nth 4 fixture))
           (reference (mevedel-instruction-test--make-reference "reference\n" workspace))
           (id (mevedel--instruction-id directive)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (mevedel-link-instructions
               (list id) (list (mevedel--instruction-id (cdr reference))))
              (mevedel-link-instructions
               (list (mevedel--instruction-id (cdr reference))) (list id)))
            (should (memq id (mevedel--instruction-inlinks (cdr reference))))
            (when missing
              (delete-file (nth 2 fixture))
              (mevedel--mark-buffer-source-missing buffer))
            (with-current-buffer (car reference)
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
                (mevedel-test--with-captured-messages nil
                  (call-interactively #'mevedel-delete-all-directives))))
            (should (overlay-buffer (cdr reference)))
            (should-not (mevedel--instruction-inlinks (cdr reference)))
            (should-not (mevedel--instruction-outlinks (cdr reference))))
        (mevedel-instruction-test--discard reference)
        (mevedel-instruction-test--discard-source fixture))))

  :doc "preserved Source-missing links cannot silently target reused reference IDs"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         (directive (nth 4 fixture))
         (record (nth 5 fixture))
         (reference (mevedel-instruction-test--make-reference "reference\n" workspace))
         (id (mevedel--instruction-id (cdr reference)))
         replacement)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (mevedel-link-instructions
             (list (mevedel--instruction-id directive)) (list id)))
          (delete-file (nth 2 fixture))
          (mevedel--mark-buffer-source-missing buffer)
          (with-current-buffer (car reference)
            (mevedel-test--with-captured-messages nil
              (mevedel-delete-all-references)))
          (setq replacement (mevedel-instruction-test--make-reference "replacement\n" workspace))
          (should (= id (mevedel--instruction-id (cdr replacement))))
          (with-temp-file (nth 2 fixture) (insert "before target after\n"))
          (with-current-buffer buffer
            (set-visited-file-modtime)
            (let ((restored (mevedel--reattach-directive record workspace buffer 8 14)))
              (should-not (mevedel--instruction-outlinks restored)))))
      (when replacement (mevedel-instruction-test--discard replacement))
      (mevedel-instruction-test--discard reference)
      (mevedel-instruction-test--discard-source fixture))))

(provide 'test-mevedel-overlays-delete)
;;; test-mevedel-overlays-delete.el ends here
