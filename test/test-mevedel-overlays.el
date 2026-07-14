;;; test-mevedel-overlays.el --- Tests for mevedel-overlays.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-persistence)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun mevedel-overlays-test--make-reference (content workspace)
  "Create a file buffer containing CONTENT and one reference in WORKSPACE."
  (let* ((file (make-temp-file "mevedel-overlay-" nil ".txt" content))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (fundamental-mode)
      (setq-local mevedel--workspace workspace)
      (set-buffer-modified-p nil)
      (cons buffer
            (mevedel--create-reference-in
             buffer (point-min) (1- (point-max)))))))

(defun mevedel-overlays-test--discard-reference (cell)
  "Kill and delete the file belonging to reference CELL."
  (when-let* ((buffer (car cell))
              ((buffer-live-p buffer)))
    (let ((file (buffer-file-name buffer)))
      (with-current-buffer buffer
        (setq-local kill-buffer-hook nil)
        (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (when (file-exists-p file)
        (delete-file file)))))


;;
;;; Lookup

(mevedel-deftest mevedel--filter-references
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "filters only references belonging to the explicit workspace"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'test :id "filter-a" :root "/tmp" :name "filter-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'test :id "filter-b" :root "/tmp" :name "filter-b"))
         (first (mevedel-overlays-test--make-reference "first\n" workspace-a))
         (second (mevedel-overlays-test--make-reference "second\n" workspace-b)))
    (overlay-put (cdr first) 'mevedel-reference-tags '(shared))
    (overlay-put (cdr second) 'mevedel-reference-tags '(shared))
    (unwind-protect
        (progn
          (should (equal (list (cdr first))
                         (mevedel--filter-references 'shared workspace-a)))
          (should (equal (list (cdr second))
                         (mevedel--filter-references 'shared workspace-b))))
      (mevedel-overlays-test--discard-reference first)
      (mevedel-overlays-test--discard-reference second))))

(mevedel-deftest mevedel--instruction-find-unique-live
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "returns a unique live match and rejects ambiguous matches"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'test :id "find-a" :root "/tmp" :name "find-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'test :id "find-b" :root "/tmp" :name "find-b"))
         (first (mevedel-overlays-test--make-reference "first\n" workspace-a))
         (second (mevedel-overlays-test--make-reference "second\n" workspace-b))
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
      (mevedel-overlays-test--discard-reference first)
      (mevedel-overlays-test--discard-reference second))))

(mevedel-deftest mevedel--instruction-with-id
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "scopes explicit lookup and permits only unambiguous fallback"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'test :id "id-a" :root "/tmp" :name "id-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'test :id "id-b" :root "/tmp" :name "id-b"))
         (first (mevedel-overlays-test--make-reference "first\n" workspace-a))
         (second (mevedel-overlays-test--make-reference "second\n" workspace-b))
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
      (mevedel-overlays-test--discard-reference first)
      (mevedel-overlays-test--discard-reference second))))

(mevedel-deftest mevedel--instruction-with-uuid
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "restores a stashed instruction before resolving its UUID"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "restore" :root "/tmp" :name "restore"))
         (cell (mevedel-overlays-test--make-reference
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
        (mevedel-overlays-test--discard-reference
         (cons (overlay-buffer restored) restored)))
      (when (file-exists-p file) (delete-file file))))

  :doc "known workspace never selects the same UUID from another workspace"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'test :id "uuid-a" :root "/tmp" :name "uuid-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'test :id "uuid-b" :root "/tmp" :name "uuid-b"))
         (first (mevedel-overlays-test--make-reference
                 "first body\n" workspace-a))
         (second (mevedel-overlays-test--make-reference
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
      (mevedel-overlays-test--discard-reference first)
      (mevedel-overlays-test--discard-reference second))))

(provide 'test-mevedel-overlays)
;;; test-mevedel-overlays.el ends here
