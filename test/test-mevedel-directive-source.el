;;; test-mevedel-directive-source.el -- Durable directive source tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for source-missing, reattachment, and archive behavior.

;;; Code:

(require 'mevedel-directive-persistence)
(require 'mevedel-overlays)
(require 'mevedel-instruction-registry)
(require 'mevedel-directive-source)
(require 'mevedel-overlay-ui)
(require 'mevedel-persistence)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-directive)
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

;;
;;; Source loss and reattachment

(mevedel-deftest mevedel--mark-buffer-source-missing
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "preserves directive identity and activity when its file disappears"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (file (nth 2 fixture))
         (buffer (nth 3 fixture))
         (overlay (nth 4 fixture))
         (record (nth 5 fixture))
         (id (mevedel-directive-id record)))
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record)
                (list (mevedel-instruction-test--attempt)))
          (delete-file file)
          (with-current-buffer buffer
            (run-hooks 'post-command-hook))
          (should-not (overlay-buffer overlay))
          (should (eq 'source-missing
                      (plist-get (mevedel-directive-anchor record) :state)))
          (should (equal file
                         (plist-get (mevedel-directive-anchor record) :file)))
          (should (equal id (mevedel-directive-id record)))
          (should (= 1 (length (mevedel-directive-attempts record))))
          (should (memq record (mevedel-workspace-directives workspace))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "the missing-file kill hook preserves the workspace record"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (file (nth 2 fixture))
         (buffer (nth 3 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (progn
          (delete-file file)
          (kill-buffer buffer)
          (should (memq record (mevedel-workspace-directives workspace)))
          (should (eq 'source-missing
                      (plist-get (mevedel-directive-anchor record) :state))))
      (mevedel-instruction-test--discard-source fixture))))

(mevedel-deftest mevedel--restore-source-missing-directives
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "reattaches one exact source match with the same directive identity"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (file (nth 2 fixture))
         (buffer (nth 3 fixture))
         (record (nth 5 fixture))
         (id (mevedel-directive-id record)))
    (unwind-protect
        (progn
          (delete-file file)
          (with-current-buffer buffer
            (mevedel--mark-buffer-source-missing buffer))
          (with-temp-file file
            (insert "before target after\n"))
          (with-current-buffer buffer
            (set-visited-file-modtime)
            (erase-buffer)
            (insert "before target after\n")
            (set-buffer-modified-p nil)
            (setq-local mevedel--workspace workspace)
            (should (= 1 (mevedel--restore-source-missing-directives buffer))))
          (let ((restored (mevedel--instruction-with-uuid id workspace)))
            (should (overlayp restored))
            (should (equal "target"
                           (with-current-buffer buffer
                             (buffer-substring-no-properties
                              (overlay-start restored) (overlay-end restored)))))
            (should (eq 'attached
                        (plist-get (mevedel-directive-anchor record) :state)))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "leaves an ambiguous returning source missing"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (file (nth 2 fixture))
         (buffer (nth 3 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (progn
          (delete-file file)
          (with-current-buffer buffer
            (mevedel--mark-buffer-source-missing buffer))
          (with-temp-file file
            (insert "target and target\n"))
          (with-current-buffer buffer
            (set-visited-file-modtime)
            (erase-buffer)
            (insert "target and target\n")
            (set-buffer-modified-p nil)
            (setq-local mevedel--workspace workspace)
            (should (= 0 (mevedel--restore-source-missing-directives buffer))))
          (should (eq 'source-missing
                      (plist-get (mevedel-directive-anchor record) :state))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "reattaches parent-owned nested presentations after the source returns"
  (let* ((directory (make-temp-file "mevedel-nested-source-" t))
         (file (file-name-concat directory "source.el"))
         (workspace (mevedel-workspace--create
                     :type 'file :id directory :root directory
                     :name "nested-source"))
         buffer parent child parent-id child-id)
    (unwind-protect
        (progn
          (with-temp-file file (insert "before target after\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (setq-local mevedel--workspace workspace)
            (setq parent
                  (mevedel--create-directive-in
                   buffer (point-min) (1- (point-max)) nil "Parent"))
            (goto-char (point-min))
            (search-forward "target")
            (setq child
                  (mevedel--create-directive-in
                   buffer (match-beginning 0) (match-end 0) nil "Detail"))
            (setq parent-id (overlay-get parent 'mevedel-uuid)
                  child-id (overlay-get child 'mevedel-uuid)))
          (delete-file file)
          (with-current-buffer buffer
            (mevedel--mark-buffer-source-missing buffer))
          (with-temp-file file (insert "before target after\n"))
          (with-current-buffer buffer
            (let ((mevedel--inhibit-source-missing-restore t))
              (revert-buffer t t))
            (setq-local mevedel--workspace workspace)
            (should (= 2 (mevedel--restore-source-missing-directives buffer))))
          (setq parent (mevedel--instruction-with-uuid parent-id workspace)
                child (mevedel--instruction-with-uuid child-id workspace))
          (should (overlayp parent))
          (should (overlayp child))
          (should (eq parent
                      (mevedel--topmost-instruction child 'directive))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory directory t))))

(mevedel-deftest mevedel--reattach-directive
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "explicitly reattaches at chosen bounds without changing identity"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (file (nth 2 fixture))
         (buffer (nth 3 fixture))
         (record (nth 5 fixture))
         (id (mevedel-directive-id record)))
    (unwind-protect
        (progn
          (delete-file file)
          (with-current-buffer buffer
            (mevedel--mark-buffer-source-missing buffer))
          (with-temp-file file
            (insert "replacement\n"))
          (with-current-buffer buffer
            (revert-buffer t t)
            (let ((overlay
                   (mevedel--reattach-directive
                    record workspace buffer (point-min) (1- (point-max)))))
              (should (equal id (overlay-get overlay 'mevedel-uuid)))
              (should (eq 'attached
                          (plist-get (mevedel-directive-anchor record) :state)))
              (should (string-match-p
                       "DIRECTIVE"
                       (substring-no-properties
                        (overlay-get overlay 'before-string)))))))
      (mevedel-instruction-test--discard-source fixture))))


;;
;;; Archive and persistence


(mevedel-deftest mevedel-archive-directive
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "archives activity, hides its source, and blocks permanent removal"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (overlay (nth 4 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record)
                (list (mevedel-instruction-test--attempt)))
          (mevedel-archive-directive record workspace)
          (should-not (overlay-buffer overlay))
          (should (eq 'archived
                      (plist-get (mevedel-directive-anchor record) :state)))
          (should (memq record (mevedel-workspace-directives workspace)))
          (should (= 1
                     (length
                      (mevedel--deserialize-directives
                       (mevedel--serialize-directives workspace
                                                       (car fixture))
                       (car fixture))))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "archiving a detached directive still round-trips persistence"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (directory (car fixture))
         (workspace (nth 1 fixture))
         (file (nth 2 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record)
                (list (mevedel-instruction-test--attempt)))
          (mevedel-directive-set-anchor
           record
           (list :state 'detached :file file :position 1
                 :source-order (list 1 2) :evidence nil))
          (mevedel-archive-directive record workspace)
          (let ((anchor (mevedel-directive-anchor record)))
            (should (eq 'archived (plist-get anchor :state)))
            (should (natnump (plist-get anchor :start)))
            (should (natnump (plist-get anchor :end))))
          (should (= 1
                     (length
                      (mevedel--deserialize-directives
                       (mevedel--serialize-directives workspace directory)
                       directory)))))
      (mevedel-instruction-test--discard-source fixture)))

  :doc "deletion removes an activity-free record and blocks durable activity"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (overlay (nth 4 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record)
                (list (mevedel-instruction-test--attempt)))
          (should-error (mevedel--delete-instruction overlay)
                        :type 'user-error)
          (setf (mevedel-directive-attempts record) nil)
          (mevedel--delete-instruction overlay)
          (should-not (memq record (mevedel-workspace-directives workspace))))
      (mevedel-instruction-test--discard-source fixture))))


(mevedel-deftest mevedel--create-directive-in
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "creates one workspace-owned record resolved by the source overlay"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "create-directive" :root "/tmp"
                     :name "create-directive"))
         (cell (mevedel-instruction-test--make-directive
                "directive body\n" "Initial request" workspace))
         (directive (cdr cell))
         (record (mevedel--directive-record directive)))
    (unwind-protect
        (progn
          (should (eq record (car (mevedel-workspace-directives workspace))))
          (should (equal (overlay-get directive 'mevedel-uuid)
                         (mevedel-directive-id record)))
          (should (equal "Initial request"
                         (mevedel-directive-request record)))
          (should (equal 'attached
                         (plist-get (mevedel-directive-anchor record)
                                    :state)))
          (should-not (overlay-get directive 'mevedel-directive)))
      (mevedel-instruction-test--discard cell)))

  :doc "stores nested directives as ordered details on the top-level record"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "nested" :root "/tmp" :name "nested"))
         (cell (mevedel-instruction-test--make-directive
                "outer child tail\n" "Parent request" workspace))
         (buffer (car cell))
         (parent (cdr cell))
         child)
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "child")
          (setq child
                (mevedel--create-directive-in
                 buffer (match-beginning 0) (match-end 0) nil "Child detail"))
          (let* ((records (mevedel-workspace-directives workspace))
                 (record (car records))
                 (subdirectives (mevedel-directive-subdirectives record)))
            (should (= 1 (length records)))
            (should (eq record (mevedel--directive-record parent)))
            (should-not (mevedel--directive-record child))
            (should (= 1 (length subdirectives)))
            (should (equal (overlay-get child 'mevedel-uuid)
                           (mevedel-subdirective-id (car subdirectives))))
            (should (equal "Child detail"
                           (mevedel-subdirective-request
                            (car subdirectives))))
            (should (equal "Child detail" (mevedel--directive-text child)))))
      (mevedel-instruction-test--discard cell)))

  :doc "rejected parent deletion keeps every presentation attached"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "reject-detach" :root "/tmp"
                     :name "reject-detach"))
         (cell (mevedel-instruction-test--make-directive
                "outer child tail\n" "Parent request" workspace))
         (buffer (car cell))
         (parent (cdr cell))
         child)
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "child")
          (setq child
                (mevedel--create-directive-in
                 buffer (match-beginning 0) (match-end 0) nil
                 "Child detail"))
          (add-hook 'before-change-functions
                    (lambda (&rest _) (error "Reject edit")) t t)
          (should-error
           (delete-region (overlay-start parent) (overlay-end parent))
           :type 'error)
          (should (overlay-buffer parent))
          (should (overlay-buffer child))
          (should (equal "outer child tail\n"
                         (buffer-substring-no-properties
                          (point-min) (point-max))))
          (should (= 1 (length
                        (mevedel-directive-subdirectives
                         (mevedel--directive-record parent))))))
      (mevedel-instruction-test--discard cell)))

  :doc "detaches Ready and attempted directives after real full-region edits"
  (dolist (state '(nil implemented))
    (let* ((request (concat "A request " (make-string 140 ?x)))
           (workspace (mevedel-workspace--create
                       :type 'file :id (format "detach-%s" state)
                       :root "/tmp" :name "detach"))
           (cell (mevedel-instruction-test--make-directive
                  "before\ndirective body\nafter\n"
                  request workspace))
           (directive (cdr cell))
           (record (mevedel--directive-record directive))
           (id (mevedel-directive-id record))
           (start (overlay-start directive))
           (end (overlay-end directive)))
      (unwind-protect
          (with-current-buffer (car cell)
            (setf (mevedel-directive-state record) state)
            (when state
              (setf (mevedel-directive-attempts record)
                    (list (mevedel-directive-attempt--create
                           :directive-request (mevedel-directive-request record)
                           :request "Exact" :result "Done" :outcome 'success
                           :patch "" :capture 'complete
                           :captured-at "2026-08-02T01:00:00+0200"
                           :checkpoint '(:session-id "s" :turn 1)))))
            (delete-region start end)
            (let ((detached (mevedel--instruction-with-uuid id workspace))
                  (anchor (mevedel-directive-anchor record)))
              (should (overlayp detached))
              (should (overlay-buffer detached))
              (should (= (overlay-start detached) (overlay-end detached)))
              (should-not (overlay-get detached 'evaporate))
              (should-not (overlay-get detached
                                       'mevedel-instruction-collapse-p))
              (should (eq 'detached (plist-get anchor :state)))
              (should (= start (plist-get anchor :position)))
              (should (equal (list start end)
                             (plist-get anchor :source-order)))
              (should (eq record (car (mevedel-workspace-directives workspace))))
              (should (string-match-p
                       (if state "DETACHED.*IMPLEMENTED" "DETACHED.*READY")
                       (substring-no-properties
                        (overlay-get detached 'before-string))))
              (should-not (string-search
                           request
                           (substring-no-properties
                            (overlay-get detached 'before-string))))
              (should (keymapp (overlay-get detached 'keymap)))))
        (mevedel-instruction-test--discard cell))))

  :doc "detached parents retain nested prompt and submission details"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "detach-nested" :root "/tmp"
                     :name "detach-nested"))
         (cell (mevedel-instruction-test--make-directive
                "outer child tail\n" "" workspace))
         (buffer (car cell))
         (parent (cdr cell))
         child)
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "child")
          (setq child
                (mevedel--create-directive-in
                 buffer (match-beginning 0) (match-end 0) nil
                 "Child detail"))
          (delete-region (overlay-start parent) (overlay-end parent))
          (setq parent
                (mevedel--instruction-with-uuid
                 (mevedel-directive-id
                  (car (mevedel-workspace-directives workspace)))
                 workspace))
          (should-not (overlay-buffer child))
          (should (= 1 (length
                        (mevedel-directive-subdirectives
                         (mevedel--directive-record parent)))))
          (should (string-match-p "Child detail"
                                  (mevedel--directive-llm-prompt parent)))
          (let ((submitted (mevedel--submitted-subdirectives parent)))
            (should (= 1 (length submitted)))
            (should (equal "Child detail"
                           (mevedel-subdirective-request
                            (car submitted))))))
      (mevedel-instruction-test--discard cell)))

  :doc "deleting only a nested range still removes its durable detail"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "delete-nested" :root "/tmp"
                     :name "delete-nested"))
         (cell (mevedel-instruction-test--make-directive
                "outer child tail\n" "Parent request" workspace))
         (buffer (car cell))
         (parent (cdr cell))
         child)
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "child")
          (setq child
                (mevedel--create-directive-in
                 buffer (match-beginning 0) (match-end 0) nil
                 "Child detail"))
          (delete-region (overlay-start child) (overlay-end child))
          (mevedel--all-instructions)
          (should-not
           (mevedel-directive-subdirectives
            (mevedel--directive-record parent)))
          (should (eq 'attached
                      (plist-get
                       (mevedel-directive-anchor
                        (mevedel--directive-record parent))
                       :state))))
      (mevedel-instruction-test--discard cell)))

  :doc "keeps partial edits attached through ordinary overlay resizing"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "detach-partial" :root "/tmp"
                     :name "detach-partial"))
         (cell (mevedel-instruction-test--make-directive
                "directive body\n" "Keep attached" workspace))
         (directive (cdr cell))
         (record (mevedel--directive-record directive)))
    (unwind-protect
        (with-current-buffer (car cell)
          (delete-region (1+ (overlay-start directive))
                         (1- (overlay-end directive)))
          (should (overlay-buffer directive))
          (should (< (overlay-start directive) (overlay-end directive)))
          (should (eq 'attached
                      (plist-get (mevedel-directive-anchor record) :state))))
      (mevedel-instruction-test--discard cell)))

  :doc "renders co-located detached directives in former source order"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "detach-many" :root "/tmp"
                     :name "detach-many"))
         (file (make-temp-file "mevedel-detach-many-" nil ".txt"
                               "first\nmiddle\nsecond\n"))
         (buffer (find-file-noselect file))
         first second first-id second-id)
    (unwind-protect
        (with-current-buffer buffer
          (fundamental-mode)
          (setq-local mevedel--workspace workspace)
          (setq first (mevedel--create-directive-in
                       buffer 1 6 nil "First request"))
          (setq second (mevedel--create-directive-in
                        buffer 14 20 nil "Second request"))
          (setq first-id (overlay-get first 'mevedel-uuid)
                second-id (overlay-get second 'mevedel-uuid))
          (delete-region 1 20)
          (let ((detached-first
                 (mevedel--instruction-with-uuid first-id workspace))
                (detached-second
                 (mevedel--instruction-with-uuid second-id workspace)))
            (should (= (overlay-start detached-first)
                       (overlay-start detached-second)))
            (should (> (overlay-get detached-first 'priority)
                       (overlay-get detached-second 'priority)))
            (should (string-match-p
                     "First request"
                     (substring-no-properties
                      (overlay-get detached-first 'before-string))))
            (should (string-match-p
                     "Second request"
                     (substring-no-properties
                      (overlay-get detached-second 'before-string))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(mevedel-deftest mevedel--create-reference-in
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "keeps references source-bound and outside durable directive records"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "create-reference" :root "/tmp"
                     :name "create-reference"))
         (cell (mevedel-instruction-test--make-reference
                "reference body\n" workspace)))
    (unwind-protect
        (progn
          (should (overlay-get (cdr cell) 'evaporate))
          (should-not (mevedel-workspace-directives workspace))
          (with-current-buffer (car cell)
            (delete-region (overlay-start (cdr cell))
                           (overlay-end (cdr cell)))
            (should-not (overlay-buffer (cdr cell)))
            (should-not (mevedel--all-instructions))))
      (mevedel-instruction-test--discard cell))))

(mevedel-deftest mevedel--set-directive-request
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "updates the durable request without replacing directive identity"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "edit-directive" :root "/tmp"
                     :name "edit-directive"))
         (cell (mevedel-instruction-test--make-directive
                "directive body\n" "Initial request" workspace))
         (directive (cdr cell))
         (record (mevedel--directive-record directive)))
    (unwind-protect
        (progn
          (setf (mevedel-directive-plan record)
                '(:status proposed :proposal "# Stale"))
          (mevedel--set-directive-request directive "Changed request")
          (should (eq record (mevedel--directive-record directive)))
          (should (equal "Changed request"
                         (mevedel-directive-request record)))
          (should (eq 'draft
                      (plist-get (mevedel-directive-plan record) :status)))
          (should (plist-get (mevedel-directive-plan record) :invalidated)))
      (mevedel-instruction-test--discard cell))))

(mevedel-deftest mevedel--delete-instruction
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "removes a Ready directive record with its presentation overlay"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "delete-directive" :root "/tmp"
                     :name "delete-directive"))
         (cell (mevedel-instruction-test--make-directive
                "directive body\n" "Unused request" workspace))
         (directive (cdr cell)))
    (unwind-protect
        (progn
          (should (mevedel-workspace-directives workspace))
          (mevedel--delete-instruction directive)
          (should-not (mevedel-workspace-directives workspace)))
      (mevedel-instruction-test--discard cell))))

(provide 'test-mevedel-directive-source)
;;; test-mevedel-directive-source.el ends here
