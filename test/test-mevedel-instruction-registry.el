;;; test-mevedel-instruction-registry.el -- Instruction registry tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for workspace instruction state and identity lookup.

;;; Code:

(require 'mevedel-instruction-registry)
(require 'mevedel-directive-source)
(require 'mevedel-overlays)
(require 'mevedel-structs)
(require 'mevedel-workspace)

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

(mevedel-deftest mevedel--instruction-alist ()
  ,test
  (test)
  :doc "expands to fresh structure the compiler cannot posify in place"
  (dolist (place '(mevedel--instruction-alist
                   mevedel--instruction-id-counter
                   mevedel--instruction-id-usage-map
                   mevedel--instruction-retired-ids))
    (should-not (eq (macroexpand (list place))
                    (macroexpand (list place))))))


(mevedel-deftest mevedel--instruction-state-rollback ()
  ,test
  (test)
  :doc "restores exact in-memory overlays after replacement state is installed"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "rollback" :root "/tmp" :name "rollback"))
         original incoming rollback uuid)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--workspace workspace)
          (insert "original incoming")
          (setq original (mevedel--create-reference-in (current-buffer) 1 9)
                uuid (overlay-get original 'mevedel-uuid)
                mevedel--highlighted-instruction original
                rollback (mevedel--instruction-state-rollback workspace))
          (mevedel--clear-instruction-state workspace)
          (setq incoming
                (mevedel--create-reference-in (current-buffer) 10 18))
          (funcall rollback)
          (should-not (overlay-buffer incoming))
          (should (eq original
                      (mevedel--instruction-with-uuid uuid workspace)))
          (should (eq original mevedel--highlighted-instruction))
          (should (equal "original"
                         (buffer-substring-no-properties
                          (overlay-start original) (overlay-end original)))))
      (mevedel--clear-instruction-state workspace)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel--instruction-find-unique-live
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "returns a unique live match and rejects ambiguous matches"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'file :id "find-a" :root "/tmp" :name "find-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'file :id "find-b" :root "/tmp" :name "find-b"))
         (first (mevedel-instruction-test--make-reference "first\n" workspace-a))
         (second (mevedel-instruction-test--make-reference "second\n" workspace-b))
         (first-reference (cdr first)))
    (unwind-protect
        (progn
          (should (eq first-reference
                      (mevedel--instruction-find-unique-live
                       (lambda (instruction)
                         (eq instruction first-reference)))))
          (should-not
           (mevedel--instruction-find-unique-live
            (lambda (instruction)
              (mevedel--referencep instruction)))))
      (mevedel-instruction-test--discard first)
      (mevedel-instruction-test--discard second))))

(mevedel-deftest mevedel--instruction-with-id
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "scopes explicit lookup and permits only unambiguous fallback"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'file :id "id-a" :root "/tmp" :name "id-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'file :id "id-b" :root "/tmp" :name "id-b"))
         (first (mevedel-instruction-test--make-reference "first\n" workspace-a))
         (second (mevedel-instruction-test--make-reference "second\n" workspace-b))
         (first-reference (cdr first))
         (second-reference (cdr second))
         (id (mevedel--instruction-id first-reference)))
    (unwind-protect
        (progn
          (should (= id (mevedel--instruction-id second-reference)))
          (should (eq first-reference
                      (mevedel--instruction-with-id id workspace-a)))
          (should (eq second-reference
                      (mevedel--instruction-with-id id workspace-b)))
          (with-temp-buffer
            (should-not (mevedel--instruction-with-id id)))
          (mevedel--delete-instruction first-reference (car first))
          (with-temp-buffer
            (should (eq second-reference
                        (mevedel--instruction-with-id id)))))
      (mevedel-instruction-test--discard first)
      (mevedel-instruction-test--discard second))))

(mevedel-deftest mevedel--instruction-with-uuid
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "restores a stashed instruction before resolving its UUID"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "restore" :root "/tmp" :name "restore"))
         (cell (mevedel-instruction-test--make-reference
                "restored body\n" workspace))
         (buffer (car cell))
         (reference (cdr cell))
         (uuid (overlay-get reference 'mevedel-uuid))
         (file (buffer-file-name buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (let ((restored (mevedel--instruction-with-uuid uuid workspace)))
            (should (overlayp restored))
            (should (buffer-live-p (overlay-buffer restored)))
            (with-current-buffer (overlay-buffer restored)
              (should (equal "restored body"
                             (buffer-substring-no-properties
                              (overlay-start restored)
                              (overlay-end restored)))))))
      (when-let* ((restored (mevedel--instruction-with-uuid uuid workspace)))
        (mevedel-instruction-test--discard
         (cons (overlay-buffer restored) restored)))
      (when (file-exists-p file) (delete-file file))))

  :doc "known workspace never selects the same UUID from another workspace"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'file :id "uuid-a" :root "/tmp" :name "uuid-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'file :id "uuid-b" :root "/tmp" :name "uuid-b"))
         (first (mevedel-instruction-test--make-reference
                 "first body\n" workspace-a))
         (second (mevedel-instruction-test--make-reference
                  "second body\n" workspace-b))
         (first-reference (cdr first))
         (second-reference (cdr second))
         (uuid (overlay-get first-reference 'mevedel-uuid)))
    (unwind-protect
        (progn
          (overlay-put second-reference 'mevedel-uuid uuid)
          (should (eq first-reference
                      (mevedel--instruction-with-uuid uuid workspace-a)))
          (should-not (mevedel--instruction-with-uuid uuid))
          (mevedel--delete-instruction first-reference (car first))
          (should-not (mevedel--instruction-with-uuid uuid workspace-a))
          (should (eq second-reference
                      (mevedel--instruction-with-uuid uuid))))
      (mevedel-instruction-test--discard first)
      (mevedel-instruction-test--discard second))))

(provide 'test-mevedel-instruction-registry)
;;; test-mevedel-instruction-registry.el ends here
