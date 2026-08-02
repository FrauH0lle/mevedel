;;; test-mevedel-overlays-source.el --- Durable directive source tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for source-missing, reattachment, and archive behavior.

;;; Code:

(require 'mevedel-directive-activity)
(require 'mevedel-overlays)
(require 'mevedel-persistence)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-overlays-source-test--fixture ()
  "Return `(DIRECTORY WORKSPACE FILE BUFFER OVERLAY RECORD)' for a source file."
  (let* ((directory (make-temp-file "mevedel-source-" t))
         (file (file-name-concat directory "source.el"))
         (workspace (mevedel-workspace--create
                     :type 'test :id directory :root directory
                     :name "source")))
    (with-temp-file file
      (insert "before target after\n"))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (setq-local mevedel--workspace workspace)
        (let* ((start (progn (goto-char (point-min))
                             (search-forward "target")
                             (match-beginning 0)))
               (end (match-end 0))
               (overlay (mevedel--create-directive-in
                         buffer start end nil "Preserve this"))
               (record (mevedel--directive-record overlay)))
          (list directory workspace file buffer overlay record))))))

(defun mevedel-overlays-source-test--attempt ()
  "Return a complete directive attempt suitable for persistence tests."
  (mevedel-directive-attempt--create
   :directive-request "Preserve this"
   :request "prompt" :result "result" :outcome 'success
   :patch "patch" :capture 'complete :covered-files nil :gaps nil
   :captured-at "2026-08-02T00:00:00+0200"
   :checkpoint '(:session-id "session" :turn 1)))

(defun mevedel-overlays-source-test--discard (fixture)
  "Discard files and buffers owned by FIXTURE."
  (when-let* ((buffer (nth 3 fixture))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq-local kill-buffer-hook nil))
    (kill-buffer buffer))
  (when-let* ((directory (car fixture))
              ((file-directory-p directory)))
    (delete-directory directory t)))


;;
;;; Source loss and reattachment

(mevedel-deftest mevedel--mark-buffer-source-missing
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "preserves directive identity and activity when its file disappears"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
         (workspace (nth 1 fixture))
         (file (nth 2 fixture))
         (buffer (nth 3 fixture))
         (overlay (nth 4 fixture))
         (record (nth 5 fixture))
         (id (mevedel-directive-id record)))
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record)
                (list (mevedel-overlays-source-test--attempt)))
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
      (mevedel-overlays-source-test--discard fixture)))

  :doc "the missing-file kill hook preserves the workspace record"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
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
      (mevedel-overlays-source-test--discard fixture))))

(mevedel-deftest mevedel--restore-source-missing-directives
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "reattaches one exact source match with the same directive identity"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
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
      (mevedel-overlays-source-test--discard fixture)))

  :doc "leaves an ambiguous returning source missing"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
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
      (mevedel-overlays-source-test--discard fixture))))

(mevedel-deftest mevedel--reattach-directive
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "explicitly reattaches at chosen bounds without changing identity"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
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
      (mevedel-overlays-source-test--discard fixture))))


;;
;;; Archive and persistence

(mevedel-deftest mevedel-directive-has-activity-p
  (:vars ())
  ,test
  (test)
  :doc "distinguishes removable authored directives from durable activity"
  (let ((directive (mevedel-directive--create
                    :id "id" :request "request" :anchor nil
                    :state nil :attempts nil :discussion nil)))
    (should-not (mevedel-directive-has-activity-p directive))
    (setf (mevedel-directive-attempts directive)
          (list (mevedel-overlays-source-test--attempt)))
    (should (mevedel-directive-has-activity-p directive))))

(mevedel-deftest mevedel-archive-directive
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "archives activity, hides its source, and blocks permanent removal"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
         (workspace (nth 1 fixture))
         (overlay (nth 4 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record)
                (list (mevedel-overlays-source-test--attempt)))
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
                       (car fixture)))))
          (should-error (mevedel-remove-directive record workspace)
                        :type 'user-error))
      (mevedel-overlays-source-test--discard fixture)))

  :doc "ordinary removal still deletes a directive without activity"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
         (workspace (nth 1 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (progn
          (mevedel-remove-directive record workspace)
          (should-not (memq record (mevedel-workspace-directives workspace))))
      (mevedel-overlays-source-test--discard fixture))))

(mevedel-deftest mevedel--deserialize-directives/source-states
  (:vars ())
  ,test
  (test)
  :doc "round trips source-missing and archived records without source buffers"
  (let* ((directory (make-temp-file "mevedel-source-persist-" t))
         (workspace (mevedel-workspace--create
                     :type 'test :id directory :root directory
                     :name "persist"))
         (missing (mevedel-directive--create
                   :id "missing" :request "request"
                   :anchor (list :state 'source-missing
                                 :file (file-name-concat directory "gone.el")
                                 :start 1 :end 7
                                 :evidence '(:schema 1 :bodyless nil
                                             :text "target")
                                 :properties
                                 '(mevedel-instruction t
                                   mevedel-uuid "missing"
                                   mevedel-instruction-type directive))
                   :state nil :session-id nil :attempts nil :discussion nil))
         (archived (mevedel-directive--create
                    :id "archived" :request "history"
                    :anchor (list :state 'archived
                                  :file (file-name-concat directory "old.el")
                                  :start 2 :end 2
                                  :evidence '(:schema 1 :bodyless t)
                                  :properties
                                  '(mevedel-instruction t
                                    mevedel-uuid "archived"
                                    mevedel-instruction-type directive))
                    :state 'implemented :session-id nil
                    :attempts (list (mevedel-overlays-source-test--attempt))
                    :discussion nil)))
    (unwind-protect
        (progn
          (mevedel-workspace-set-directives workspace (list missing archived))
          (let* ((serialized (mevedel--serialize-directives workspace directory))
                 (restored (mevedel--deserialize-directives serialized directory)))
            (should (equal '(source-missing archived)
                           (mapcar (lambda (record)
                                     (plist-get (mevedel-directive-anchor record)
                                                :state))
                                   restored)))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-list-directives/archived
  (:vars ())
  ,test
  (test)
  :doc "keeps source-missing directives active while hiding archived records"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "list" :root "/tmp" :name "list"))
         (missing (mevedel-directive--create
                   :id "missing" :request "missing"
                   :anchor '(:state source-missing) :state nil))
         (archived (mevedel-directive--create
                    :id "archived" :request "archived"
                    :anchor '(:state archived) :state nil))
         choices opened)
    (mevedel-workspace-set-directives workspace (list missing archived))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq choices collection)
                 (caar collection)))
              ((symbol-function 'mevedel-open-directive-activity)
               (lambda (directive owner)
                 (setq opened (list directive owner)))))
      (mevedel-list-directives workspace))
    (should (= 1 (length choices)))
    (should (string-match-p "missing" (caar choices)))
    (should (equal (list missing workspace) opened))))

(mevedel-deftest mevedel-list-archived-directives
  (:vars ())
  ,test
  (test)
  :doc "keeps archived activity inspectable outside the active list"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "archive-list" :root "/tmp"
                     :name "archive-list"))
         (active (mevedel-directive--create
                  :id "active" :request "active"
                  :anchor '(:state source-missing) :state nil))
         (archived (mevedel-directive--create
                    :id "archived" :request "history"
                    :anchor '(:state archived) :state 'implemented))
         choices opened)
    (mevedel-workspace-set-directives workspace (list active archived))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq choices collection)
                 (caar collection)))
              ((symbol-function 'mevedel-open-directive-activity)
               (lambda (directive owner)
                 (setq opened (list directive owner)))))
      (mevedel-list-archived-directives workspace))
    (should (= 1 (length choices)))
    (should (string-match-p "archived" (caar choices)))
    (should (equal (list archived workspace) opened))))

(mevedel-deftest mevedel--directive-action-context/source-missing
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "uses real prompt construction for attached eligibility"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
         (workspace (nth 1 fixture))
         (overlay (nth 4 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (let ((context (mevedel--directive-action-context record workspace)))
          (should (eq overlay (plist-get context :directive)))
          (should (string-match-p "Preserve this"
                                  (plist-get context :prompt))))
      (mevedel-overlays-source-test--discard fixture)))

  :doc "rejects source-missing submission with one shared validation error"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (mevedel--mark-buffer-source-missing buffer))
          (should-error
           (mevedel--directive-action-context record workspace)
           :type 'user-error))
      (mevedel-overlays-source-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-reattach
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "reattaches the activity record through the explicit activity action"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
         (workspace (nth 1 fixture))
         (file (nth 2 fixture))
         (source (nth 3 fixture))
         (record (nth 5 fixture))
         (activity (generate-new-buffer " *mevedel-reattach-activity*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (mevedel--mark-buffer-source-missing source))
          (with-current-buffer activity
            (mevedel-directive-activity-mode)
            (setq-local mevedel-directive-activity--workspace workspace
                        mevedel-directive-activity--directive record)
            (mevedel-directive-activity-reattach file 1 7))
          (should (eq 'attached
                      (plist-get (mevedel-directive-anchor record) :state))))
      (when (buffer-live-p activity) (kill-buffer activity))
      (mevedel-overlays-source-test--discard fixture))))

(mevedel-deftest mevedel-directive-activity-archive
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "archives the current record through the activity action"
  (let* ((fixture (mevedel-overlays-source-test--fixture))
         (workspace (nth 1 fixture))
         (record (nth 5 fixture))
         (activity (generate-new-buffer " *mevedel-archive-activity*")))
    (unwind-protect
        (progn
          (setf (mevedel-directive-attempts record)
                (list (mevedel-overlays-source-test--attempt)))
          (with-current-buffer activity
            (mevedel-directive-activity-mode)
            (setq-local mevedel-directive-activity--workspace workspace
                        mevedel-directive-activity--directive record)
            (mevedel-directive-activity-archive))
          (should (eq 'archived
                      (plist-get (mevedel-directive-anchor record) :state))))
      (when (buffer-live-p activity) (kill-buffer activity))
      (mevedel-overlays-source-test--discard fixture))))

(provide 'test-mevedel-overlays-source)
;;; test-mevedel-overlays-source.el ends here
