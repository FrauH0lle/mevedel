;;; test-niwa-instruction-registry.el -- Instruction registry tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for workspace instruction state and identity lookup.

;;; Code:

(require 'niwa-notes)

(require 'niwa-instruction-registry)
(require 'niwa-note-source)
(require 'niwa-overlays)
(require 'niwa-structs)
(require 'niwa-workspace)

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

(niwa-deftest niwa--instruction-alist ()
  ,test
  (test)
  :doc "expands to fresh structure the compiler cannot posify in place"
  (dolist (place '(niwa--instruction-alist
                   niwa--instruction-id-counter
                   niwa--instruction-id-usage-map
                   niwa--instruction-retired-ids))
    (should-not (eq (macroexpand (list place))
                    (macroexpand (list place))))))


(niwa-deftest niwa--instruction-state-rollback ()
  ,test
  (test)
  :doc "restores exact in-memory overlays after replacement state is installed"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "rollback" :root "/tmp" :name "rollback"))
         original incoming rollback uuid)
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--workspace workspace)
          (insert "original incoming")
          (setq original (niwa-instruction-test--create-note-in (current-buffer) 1 9)
                uuid (overlay-get original 'niwa-uuid)
                niwa--highlighted-instruction original
                rollback (niwa--instruction-state-rollback workspace))
          (niwa--clear-instruction-state workspace)
          (setq incoming
                (niwa-instruction-test--create-note-in (current-buffer) 10 18))
          (funcall rollback)
          (should-not (overlay-buffer incoming))
          (should (eq original
                      (niwa--instruction-with-uuid uuid workspace)))
          (should (eq original niwa--highlighted-instruction))
          (should (equal "original"
                         (buffer-substring-no-properties
                          (overlay-start original) (overlay-end original)))))
      (niwa--clear-instruction-state workspace)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa--instruction-find-unique-live
  (:vars
   ((niwa--instruction-states (make-hash-table :test #'equal))
    (niwa--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "returns a unique live match and rejects ambiguous matches"
  (let* ((workspace-a (niwa-workspace--create
                       :type 'file :id "find-a" :root "/tmp" :name "find-a"))
         (workspace-b (niwa-workspace--create
                       :type 'file :id "find-b" :root "/tmp" :name "find-b"))
         (first (niwa-instruction-test--make-note "first\n" workspace-a))
         (second (niwa-instruction-test--make-note "second\n" workspace-b))
         (first-reference (cdr first)))
    (unwind-protect
        (progn
          (should (eq first-reference
                      (niwa--instruction-find-unique-live
                       (lambda (instruction)
                         (eq instruction first-reference)))))
          (should-not
           (niwa--instruction-find-unique-live
            (lambda (instruction)
              (eq 'note (overlay-get instruction 'niwa-instruction-type))))))
      (niwa-instruction-test--discard first)
      (niwa-instruction-test--discard second))))

(niwa-deftest niwa--instruction-with-id
  (:vars
   ((niwa--instruction-states (make-hash-table :test #'equal))
    (niwa--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "scopes explicit lookup and permits only unambiguous fallback"
  (let* ((workspace-a (niwa-workspace--create
                       :type 'file :id "id-a" :root "/tmp" :name "id-a"))
         (workspace-b (niwa-workspace--create
                       :type 'file :id "id-b" :root "/tmp" :name "id-b"))
         (first (niwa-instruction-test--make-note "first\n" workspace-a))
         (second (niwa-instruction-test--make-note "second\n" workspace-b))
         (first-reference (cdr first))
         (second-reference (cdr second))
         (id (niwa--instruction-id first-reference)))
    (unwind-protect
        (progn
          (should (= id (niwa--instruction-id second-reference)))
          (should (eq first-reference
                      (niwa--instruction-with-id id workspace-a)))
          (should (eq second-reference
                      (niwa--instruction-with-id id workspace-b)))
          (with-temp-buffer
            (should-not (niwa--instruction-with-id id)))
          (niwa--delete-instruction first-reference (car first))
          (with-temp-buffer
            (should (eq second-reference
                        (niwa--instruction-with-id id)))))
      (niwa-instruction-test--discard first)
      (niwa-instruction-test--discard second))))

(niwa-deftest niwa--instruction-with-uuid
  (:vars
   ((niwa--instruction-states (make-hash-table :test #'equal))
    (niwa--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "restores a stashed instruction before resolving its UUID"
  (let* ((workspace (niwa-workspace--create
                     :type 'file :id "restore" :root "/tmp" :name "restore"))
         (cell (niwa-instruction-test--make-note
                "restored body\n" workspace))
         (buffer (car cell))
         (reference (cdr cell))
         (uuid (overlay-get reference 'niwa-uuid))
         (file (buffer-file-name buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (let ((restored (niwa--instruction-with-uuid uuid workspace)))
            (should (overlayp restored))
            (should (buffer-live-p (overlay-buffer restored)))
            (with-current-buffer (overlay-buffer restored)
              (should (equal "restored body"
                             (buffer-substring-no-properties
                              (overlay-start restored)
                              (overlay-end restored)))))))
      (when-let* ((restored (niwa--instruction-with-uuid uuid workspace)))
        (niwa-instruction-test--discard
         (cons (overlay-buffer restored) restored)))
      (when (file-exists-p file) (delete-file file))))

  :doc "known workspace never selects the same UUID from another workspace"
  (let* ((workspace-a (niwa-workspace--create
                       :type 'file :id "uuid-a" :root "/tmp" :name "uuid-a"))
         (workspace-b (niwa-workspace--create
                       :type 'file :id "uuid-b" :root "/tmp" :name "uuid-b"))
         (first (niwa-instruction-test--make-note
                 "first body\n" workspace-a))
         (second (niwa-instruction-test--make-note
                  "second body\n" workspace-b))
         (first-reference (cdr first))
         (second-reference (cdr second))
         (uuid (overlay-get first-reference 'niwa-uuid)))
    (unwind-protect
        (progn
          (overlay-put second-reference 'niwa-uuid uuid)
          (should (eq first-reference
                      (niwa--instruction-with-uuid uuid workspace-a)))
          (should-not (niwa--instruction-with-uuid uuid))
          (niwa--delete-instruction first-reference (car first))
          (should-not (niwa--instruction-with-uuid uuid workspace-a))
          (should (eq second-reference
                      (niwa--instruction-with-uuid uuid))))
      (niwa-instruction-test--discard first)
      (niwa-instruction-test--discard second))))

(provide 'test-niwa-instruction-registry)
;;; test-niwa-instruction-registry.el ends here
