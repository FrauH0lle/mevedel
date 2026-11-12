;;; test-mevedel-notes-migration.el --- Old workspace note migration -*- lexical-binding: t -*-

;;; Commentary:

;; Open real old-format workspaces through registration and source visits.

;;; Code:

(require 'mevedel)
(require 'mevedel-notes)
(require 'mevedel-notes-list)
(require 'mevedel-mentions)
(require 'mevedel-persistence)
(require 'mevedel-directive-activity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-notes-migration-test--workspace (record &rest body)
  "Open old store RECORD in a private workspace, then run BODY."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "notes-migration-" t))
          (file (file-name-concat directory "source.el"))
          (store (file-name-concat directory ".mevedel/instructions.el"))
          (backup (concat store ".format-1.bak"))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel-workspace--registry (make-hash-table :test #'equal))
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          workspace source list-buffer old-bytes)
     (save-window-excursion
       (unwind-protect
           (progn
             (make-directory (file-name-directory store) t)
             (write-region "first code\nsecond code\n" nil file nil 'silent)
             (setq old-bytes (concat (prin1-to-string ,record) "\n"))
             (write-region old-bytes nil store nil 'silent)
             (setq workspace (mevedel-workspace-get-or-create
                              'file directory directory "migration")
                   source (find-file-noselect file)
                   list-buffer (mevedel-notes-list workspace))
             ,@body)
         (when (buffer-live-p list-buffer) (kill-buffer list-buffer))
         (when (buffer-live-p source) (kill-buffer source))
         (setq mevedel-instruction-store--dirty nil)
         (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
         (delete-directory directory t)))))

(defun mevedel-notes-migration-test--reference-store ()
  "Return a literal format-1 reference store."
  '(:format 1 :ids (:id-counter 7 :used-ids (7) :retired-ids (2))
    :directives nil :rewinds nil
    :files (("source.el" :original-content "first code\nsecond code\n"
             :instructions
             ((:overlay-start 1 :overlay-end 11
               :anchor (:uuid "old-reference" :text "first code")
               :properties (mevedel-instruction t mevedel-id 7
                            mevedel-uuid "old-reference"
                            mevedel-instruction-type reference
                            mevedel-commentary "Keep this explanation"
                            mevedel-reference-tags (important reviewed))))))))

(defun mevedel-notes-migration-test--directive-store ()
  "Return a format-1 directive store without activity."
  (let* ((data (copy-tree (mevedel-notes-migration-test--reference-store)))
         (instruction (car (plist-get (cdar (plist-get data :files)) :instructions))))
    (setf (plist-get instruction :anchor)
          '(:uuid "old-directive" :text "first code")
          (plist-get instruction :properties)
          '(mevedel-instruction t mevedel-id 7 mevedel-uuid "old-directive"
            mevedel-instruction-type directive)
          (plist-get data :directives)
          '((:id "old-directive" :request "Improve this function"
             :anchor (:state attached :file "source.el" :start 1 :end 11
                      :evidence (:uuid "old-directive" :text "first code"))
             :subdirectives nil :session-id nil :planning-enabled nil
             :skills nil :plan nil :planning nil :attempts nil :discussion nil)))
    (copy-tree data)))

(mevedel-deftest mevedel-notes-file-visit-migration
  (:quiet t)
  ,test
  (test)
  :doc "old references become numbered notes with text, tags and a byte backup"
  (mevedel-notes-migration-test--workspace
      (mevedel-notes-migration-test--reference-store)
    (with-current-buffer list-buffer
      (should (string-match-p "Keep this explanation.*important.*reviewed"
                              (buffer-string))))
    (with-current-buffer source
      (let ((note (cl-find-if
                   (lambda (overlay)
                     (eq (overlay-get overlay 'mevedel-instruction-type) 'note))
                   (overlays-in (point-min) (point-max)))))
        (should note)
        (should (= 7 (overlay-get note 'mevedel-id)))
        (should (equal "old-reference" (overlay-get note 'mevedel-uuid)))
        (should (equal "first code" (buffer-substring-no-properties
                                    (overlay-start note) (overlay-end note)))))
      (should (equal "first code\nsecond code\n" (buffer-string))))
    (should (equal old-bytes
                   (with-temp-buffer (insert-file-contents backup) (buffer-string)))))
  :doc "a directive without activity becomes an unsent note, not a session"
  (mevedel-notes-migration-test--workspace
      (mevedel-notes-migration-test--directive-store)
    (with-current-buffer list-buffer
      (should (string-match-p "Improve this function.*unsent" (buffer-string))))
    (should-not (mevedel-workspace-directives workspace))
    (should-not (file-exists-p (file-name-concat directory ".mevedel/sessions"))))
  :doc "subdirectives append to their parent text, without separate notes"
  (let* ((data (mevedel-notes-migration-test--directive-store))
         (record (car (plist-get data :directives)))
         (entry (cdar (plist-get data :files))))
    (setf (plist-get record :subdirectives)
          '((:id "old-child" :request "Preserve the edge case"
             :anchor (:state attached :file "source.el" :start 2 :end 6
                      :evidence (:uuid "old-child" :parent-uuid "old-directive"
                                 :text "irst")
                      :properties (mevedel-id 9))))
          (plist-get entry :instructions)
          (append (plist-get entry :instructions)
                  '((:overlay-start 2 :overlay-end 6
                     :anchor (:uuid "old-child" :parent-uuid "old-directive"
                              :text "irst")
                     :properties (mevedel-instruction t mevedel-id 9
                                  mevedel-uuid "old-child"
                                  mevedel-instruction-type subdirective)))))
    (mevedel-notes-migration-test--workspace data
      (with-current-buffer source
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_prompt initial &rest _)
                     (should (equal "Improve this function\n\nPreserve the edge case" initial))
                     initial)))
          (call-interactively #'mevedel-note-edit)))
      (with-current-buffer list-buffer
        (should (= 1 (length (cl-remove-if-not #'car tabulated-list-entries)))))))
  :doc "activity opens in the existing inspector as frozen history after reload"
  (let* ((data (mevedel-notes-migration-test--directive-store))
         (record (car (plist-get data :directives))))
    (setf (plist-get record :session-id) "historical-session"
          (plist-get record :discussion)
          '((:sequence 1 :directive-request "Improve this function"
             :message "Explain the old approach" :request "Explain the old approach"
             :result "Historical explanation" :outcome success
             :attempt-index nil :checkpoint (:session-id "historical-session" :turn 1))))
    (mevedel-notes-migration-test--workspace data
      (mevedel-instruction-store-sync workspace)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (with-current-buffer source
        (goto-char (point-min))
        (let ((history
               (cl-letf (((symbol-function 'read-multiple-choice)
                          (lambda (_prompt choices &rest _)
                            (should (assq ?h choices)) '(?h "history"))))
                 (mevedel-note-menu))))
          (unwind-protect
              (with-current-buffer history
                (should (derived-mode-p 'mevedel-directive-activity-mode))
                (should buffer-read-only)
                (should (string-match-p "Historical explanation" (buffer-string)))
                (dolist (command '(mevedel-directive-activity-discuss
                                   mevedel-directive-activity-request-changes
                                   mevedel-directive-activity-retry
                                   mevedel-directive-activity-implement-this
                                   mevedel-directive-activity-settings))
                  (should-not (fboundp command)))
                (dolist (key '("d" "c" "x" "i" "s"))
                  (should (memq (key-binding (kbd key)) '(nil undefined))))
                (dolist (command '(mevedel-directive-activity-rewind
                                   mevedel-directive-activity-archive))
                  (should-error (funcall command) :type 'user-error))
                (should-error (mevedel-directive-activity-reattach file 1 11)
                              :type 'user-error))
            (kill-buffer history))))
      (should-not (mevedel-workspace-directives workspace))))
  :doc "archived directives without presentations stay closed beyond every expiry limit"
  (let* ((data (mevedel-notes-migration-test--directive-store))
         (record (car (plist-get data :directives)))
         (anchor (plist-get record :anchor))
         (mevedel-notes-closed-max-count 0)
         (mevedel-notes-closed-max-age-days 0))
    (setf (plist-get data :files) nil
          (plist-get anchor :state) 'archived
          (plist-get anchor :properties)
          '(mevedel-id 7 mevedel-uuid "old-directive" mevedel-instruction t
            mevedel-instruction-type directive))
    (setf (plist-get record :anchor) anchor)
    (mevedel-notes-migration-test--workspace data
      (ert-info ((format "Migrated store: %S"
                         (with-temp-buffer
                           (insert-file-contents store) (buffer-string))))
        (should (= 1 (length (mevedel-notes-closed workspace)))))
      (mevedel-instruction-store-sync workspace)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (should (= 1 (length (mevedel-notes-closed workspace))))
      (let ((restored (mevedel-note-restore 7 workspace)))
        (should (equal "Improve this function" (overlay-get restored 'mevedel-note-text)))
        (should (equal "old-directive" (overlay-get restored 'mevedel-uuid))))))
  :doc "detached records allocate a number above all used and retired numbers"
  (let* ((data (mevedel-notes-migration-test--directive-store))
         (record (car (plist-get data :directives))))
    (setf (plist-get data :files) nil
          (plist-get data :ids) '(:id-counter 7 :used-ids (7) :retired-ids (20))
          (plist-get record :anchor)
          '(:state detached :file "source.el" :position 1 :source-order (1 0)))
    (mevedel-notes-migration-test--workspace data
      (with-current-buffer source
        (goto-char (point-min))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_prompt initial &rest _)
                     (should (equal "Improve this function" initial)) initial)))
          (call-interactively #'mevedel-note-edit))
        (goto-char (point-max))
        (let ((new-note (mevedel-note-create "New note")))
          (should (= 22 (overlay-get new-note 'mevedel-id)))))
      (with-current-buffer list-buffer
        (should (string-match-p "21.*source.el.*Improve this function"
                                (buffer-string)))))))

(mevedel-deftest mevedel-notes-list-migrated-descendants
  (:quiet t)
  ,test
  (test)
  :doc "references nested inside folded subdirectives survive anchor restore and sync"
  (let* ((data (mevedel-notes-migration-test--directive-store))
         (record (car (plist-get data :directives)))
         (file-entry (cdar (plist-get data :files)))
         (mevedel-patch-outdated-instructions nil))
    (setf (plist-get record :subdirectives)
          '((:id "old-child" :request "Nested request"
             :anchor (:state attached :file "source.el" :start 2 :end 6
                      :evidence (:uuid "old-child" :parent-uuid "old-directive"
                                 :text "irst") :properties (mevedel-id 9))))
          (plist-get file-entry :instructions)
          (append (plist-get file-entry :instructions)
                  '((:overlay-start 2 :overlay-end 6
                     :anchor (:uuid "old-child" :parent-uuid "old-directive" :text "irst")
                     :properties (mevedel-instruction t mevedel-id 9
                                  mevedel-uuid "old-child" mevedel-instruction-type subdirective))
                    (:overlay-start 3 :overlay-end 5
                     :anchor (:uuid "nested-reference" :parent-uuid "old-child" :text "rs")
                     :properties (mevedel-instruction t mevedel-id 10
                                  mevedel-uuid "nested-reference" mevedel-instruction-type reference
                                  mevedel-commentary "Nested observation")))))
    (mevedel-notes-migration-test--workspace data
      (mevedel-instruction-store-sync workspace)
      (mevedel--clear-instruction-state workspace)
      (with-current-buffer list-buffer
        (mevedel-notes-list-refresh)
        (should (string-match-p "Nested observation" (buffer-string)))))))

(mevedel-deftest mevedel-note-source-edit-migrated-history
  (:quiet t)
  ,test
  (test)
  :doc "edge typing does not copy legacy history into source geometry undo"
  (let* ((data (mevedel-notes-migration-test--directive-store))
         (record (car (plist-get data :directives))))
    (setf (plist-get record :discussion)
          (list (list :sequence 1 :directive-request "Improve this function"
                      :message "Old discussion" :request "Old discussion"
                      :result (make-string 200000 ?x) :outcome 'success
                      :attempt-index nil :checkpoint '(:session-id "old" :turn 1))))
    (mevedel-notes-migration-test--workspace data
      (with-current-buffer source
        (goto-char 11)
        (let ((before (nth 4 (memory-use-counts))))
          (insert "!")
          ;; A geometry edit may allocate labels and anchors, but not even one
          ;; copy of the 200,000-character historical reply.
          (should (< (- (nth 4 (memory-use-counts)) before) 100000)))))))

(mevedel-deftest mevedel-workspace-get-or-create-migration
  (:quiet t)
  ,test
  (test)
  :doc "a second opening preserves edited notes and never recreates the backup"
  (mevedel-notes-migration-test--workspace
      (mevedel-notes-migration-test--reference-store)
    (with-current-buffer source
      (goto-char (point-min))
      (mevedel-note-edit "Edited after migration"))
    (mevedel-instruction-store-flush t)
    (let ((store-bytes (with-temp-buffer (insert-file-contents store) (buffer-string))))
      ;; A current store must load without needing its old migration input.
      (delete-file backup)
      (kill-buffer source)
      (setq source nil)
      (mevedel--clear-instruction-state workspace)
      (clrhash mevedel-workspace--registry)
      (setq workspace (mevedel-workspace-get-or-create
                       'file directory directory "migration")
            source (find-file-noselect file))
      (with-current-buffer list-buffer
        (mevedel-notes-list-refresh)
        (should (string-match-p "Edited after migration" (buffer-string))))
      (should-not (file-exists-p backup))
      (should (equal store-bytes
                     (with-temp-buffer (insert-file-contents store) (buffer-string)))))))

(defmacro mevedel-notes-migration-test--backup (before &rest body)
  "Run BEFORE registration and BODY afterwards in a store-only workspace."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "notes-migration-backup-" t))
          (store (file-name-concat directory ".mevedel/instructions.el"))
          (backup (concat store ".format-1.bak"))
          (session-file (file-name-concat directory ".mevedel/sessions/old/transcript.org"))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel-workspace--registry (make-hash-table :test #'equal))
          (old-bytes (concat (prin1-to-string
                              (mevedel-notes-migration-test--reference-store)) "\n"))
          workspace list-buffer)
     (save-window-excursion
       (unwind-protect
           (progn
             (make-directory (file-name-directory session-file) t)
             (write-region "Historical session bytes\n" nil session-file nil 'silent)
             (write-region old-bytes nil store nil 'silent)
             ,before
             (setq workspace (mevedel-workspace-get-or-create
                              'file directory directory "backup"))
             ,@body
             (should (equal "Historical session bytes\n"
                            (with-temp-buffer
                              (insert-file-contents session-file) (buffer-string)))))
         (when (buffer-live-p list-buffer) (kill-buffer list-buffer))
         (delete-directory directory t)))))

(mevedel-deftest mevedel-instruction-store-observe-migration-backup
  (:quiet t)
  ,test
  (test)
  :doc "a matching existing backup permits recovery without changing sessions"
  (mevedel-notes-migration-test--backup
      (write-region old-bytes nil backup nil 'silent)
    (setq list-buffer (mevedel-notes-list workspace))
    (with-current-buffer list-buffer
      (should (string-match-p "Keep this explanation" (buffer-string))))
    (should (equal old-bytes
                   (with-temp-buffer (insert-file-contents backup) (buffer-string)))))
  :doc "a conflicting backup refuses conversion and preserves both files"
  (mevedel-notes-migration-test--backup
      (write-region "Different backup\n" nil backup nil 'silent)
    (let ((failure (should-error (mevedel-instruction-store-observe workspace))))
      (should (string-match-p "move the backup aside and retry"
                              (error-message-string failure)))
      (should (string-match-p (regexp-quote backup) (error-message-string failure))))
    (should (equal old-bytes
                   (with-temp-buffer (insert-file-contents store) (buffer-string))))
    (should (equal "Different backup\n"
                   (with-temp-buffer (insert-file-contents backup) (buffer-string)))))
  :doc "unsupported workspace formats are refused without a backup or replacement"
  (mevedel-notes-migration-test--backup
      (progn
        (setq old-bytes "(:format 99 :ids nil :files nil :directives nil)\n")
        (write-region old-bytes nil store nil 'silent))
    (should-error (mevedel-instruction-store-observe workspace))
    (should-not (file-exists-p backup))
    (should (equal old-bytes
                   (with-temp-buffer (insert-file-contents store) (buffer-string))))))

(mevedel-deftest mevedel-mentions-expand-user-input-migrated
  (:quiet t)
  ,test
  (test)
  :doc "old numeric ref mentions attach the migrated note's code and text"
  (mevedel-notes-migration-test--workspace
      (mevedel-notes-migration-test--reference-store)
    (let* ((session (mevedel-session-create "mentions" workspace))
           (prepared (mevedel-mentions-prepare-user-input "See @ref:7" session))
           (result (mevedel-mentions-expand-user-input prepared session))
           (body (mapconcat (lambda (item) (plist-get item :body))
                            (plist-get result :reminder-items) "\n")))
      (should (string-match-p "contents attached" (plist-get result :text)))
      (should (string-match-p "first code" body))
      (should (string-match-p "Keep this explanation" body))))
  :doc "old UUID-bound ref mentions keep their target even with another display number"
  (mevedel-notes-migration-test--workspace
      (mevedel-notes-migration-test--reference-store)
    (let* ((session (mevedel-session-create "bound-mentions" workspace))
           (text (copy-sequence "@ref:99")))
      (mevedel-mention-bindings-set
       0 (length text)
       '(:kind ref :token "@ref:99" :reference-uuid "old-reference") text)
      (let* ((result (mevedel-mentions-expand-user-input text session))
             (body (mapconcat (lambda (item) (plist-get item :body))
                              (plist-get result :reminder-items) "\n")))
        (should (string-match-p "Keep this explanation" body))
        (should (string-match-p "first code" body))))))

(provide 'test-mevedel-notes-migration)
;;; test-mevedel-notes-migration.el ends here
