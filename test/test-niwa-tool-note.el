;;; test-niwa-tool-note.el -- Model note writes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the model tool with real source files and workspace sessions.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-pipeline)
(require 'niwa-tool-registry)
(require 'niwa-tool-note)
(require 'niwa-note-write)
(require 'niwa-session-artifacts)
(require 'niwa-view)
(require 'niwa-tool-fs-read)

(defun niwa-tool-note-test--read (data address)
  "Read note ADDRESS in DATA through the model pipeline."
  (let (outcome)
    (with-current-buffer data
      (niwa-pipeline-run-tool-outcome
       (niwa-tool-ensure "Read") (lambda (value) (setq outcome value))
       (list :file_path address)))
    (with-timeout (10 (ert-fail "Read did not settle"))
      (while (not outcome) (accept-process-output nil 0.01)))
    (should (eq 'success (plist-get outcome :status)))
    (plist-get outcome :result)))

(defun niwa-tool-note-test--create (data file &rest fields)
  "Create one note in FILE from DATA with FIELDS and return its UUID."
  (let ((outcome (niwa-tool-note-test--write
                  data (vector (append (list :action "create" :file_path file
                                             :start_line 2 :text "Finding") fields)))))
    (ert-info ((format "%S" outcome))
      (should (eq 'success (plist-get outcome :status))))
    (should (string-match "notes://\\([a-z0-9-]+\\)\\.md" (plist-get outcome :result)))
    (match-string 1 (plist-get outcome :result))))

(defmacro niwa-tool-note-test--workspace (&rest body)
  "Run BODY with a real source file and an isolated session."
  (declare (indent 0))
  `(niwa-view-test--with-buffers
     (let* ((root (make-temp-file "note-write-" t))
            (file (file-name-concat root "source.txt"))
            (niwa-sessions-directory (file-name-concat root "sessions"))
            (niwa--instruction-states (make-hash-table :test #'equal))
            (niwa-workspace--registry (make-hash-table :test #'equal))
            (niwa-instruction-store--dirty nil)
            (niwa-instruction-store--failed nil)
            (niwa-note-labels 'expanded)
            (post-command-hook (copy-sequence post-command-hook))
            (workspace (niwa-workspace-get-or-create 'file root root "notes"))
            (session (niwa-session-create "notes" workspace root))
            (old-buffers (buffer-list)))
       (unwind-protect
           (progn
             (make-directory (file-name-concat root ".niwa"))
             (write-region "before\nold\nafter\n" nil file nil 'silent)
             (with-current-buffer data-buf
               (setq-local default-directory (file-name-as-directory root)
                           niwa--workspace workspace niwa--session session))
             (niwa-session-artifacts-save session data-buf nil t)
             (save-window-excursion ,@body))
         (dolist (buffer (cl-set-difference (buffer-list) old-buffers))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil)
               (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
         (niwa-workspace-clear-registry)
         (delete-directory root t)))))

(defun niwa-tool-note-test--write (data operations)
  "Call NoteWrite with OPERATIONS in DATA through the model pipeline."
  (let ((tool (niwa-tool-ensure "NoteWrite")) result)
    (should tool)
    (with-current-buffer data
      (niwa-pipeline-run-tool-outcome
       tool (lambda (value) (setq result value)) (list :operations operations)))
    (with-timeout (10 (ert-fail "NoteWrite did not settle"))
      (while (not result) (accept-process-output nil 0.01)))
    result))

(niwa-deftest niwa-tool-note-register ()
  ,test
  (test)
  :doc "the model contract reserves close, send and done for the user's request"
  (let ((prompt (niwa-tool-prompt (niwa-tool-ensure "NoteWrite"))))
    (should (string-match-p
             "Do not close notes, send them or mark them done unless the user asks" prompt)))
  :doc "the maintained NoteWrite contract reserves close for the user's request"
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat (file-name-directory (locate-library "niwa-tool-note"))
                       "docs/tools.md"))
    (should (string-match-p "Closing, sending and marking[\n ]+done require the user's request"
                            (buffer-string)))))

(niwa-deftest niwa-tool-note-write/invalid-tags (:quiet t)
  ,test
  (test)
  :doc "invalid create tags leave no note, prepared change or store mutation"
  (dolist (tags '(["needs review"] ["123"] ["and"]))
    (niwa-tool-note-test--workspace
      (niwa-tool-note-test--create data-buf file)
      (let* ((before (niwa-tool-note-test--read data-buf "notes://"))
             (store (niwa-test--read-file (niwa-instruction-store-path workspace)))
             (outcome (niwa-tool-note-test--write
                       data-buf
                       (vector (list :action "create" :file_path file :start_line 2
                                     :text "Invalid suggestion" :tags tags :prepared_change
                                     "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))))
        (should (eq 'error (plist-get outcome :status)))
        (should (string-search "cannot be used in a note query" (plist-get outcome :result)))
        (should (equal before (niwa-tool-note-test--read data-buf "notes://")))
        (should (equal store (niwa-test--read-file (niwa-instruction-store-path workspace))))
        (should (equal "before\nold\nafter\n" (niwa-test--read-file file))))))
  :doc "invalid edit tags preserve the note's existing text, tags and store"
  (niwa-tool-note-test--workspace
    (let* ((id (niwa-tool-note-test--create data-buf file :tags ["review"]))
           (address (concat "notes://" id ".md"))
           (before (niwa-tool-note-test--read data-buf address))
           (store (niwa-test--read-file (niwa-instruction-store-path workspace)))
           (outcome (niwa-tool-note-test--write
                     data-buf (vector (list :action "edit" :id id
                                            :text "Should not replace Finding"
                                            :tags ["needs review"])))))
      (should (eq 'error (plist-get outcome :status)))
      (should (string-search "cannot be used in a note query" (plist-get outcome :result)))
      (should (equal before (niwa-tool-note-test--read data-buf address)))
      (should (equal store (niwa-test--read-file (niwa-instruction-store-path workspace)))))))

(niwa-deftest niwa-tool-note-write (:quiet t)
  ,test
  (test)
  :doc "creates agent-authored tagged notes durably with one sync per batch"
  (niwa-tool-note-test--workspace
    (let* ((syncs 0)
          (counter (lambda (&rest _) (cl-incf syncs))))
      (advice-add 'niwa-instruction-store-sync :before counter)
      (unwind-protect
          (let ((outcome (niwa-tool-note-test--write
                          data-buf
                          (vector (list :action "create" :file_path file :start_line 2
                                        :text "Review this" :tags ["review"])
                                  (list :action "create" :file_path file :start_line 3
                                        :text "Second finding")))))
            (ert-info ((format "%S" outcome))
              (should (eq 'success (plist-get outcome :status))))
            (should (string-search "notes://" (plist-get outcome :result)))
            (should (= 1 syncs)))
        (advice-remove 'niwa-instruction-store-sync counter)))
    (kill-buffer (find-buffer-visiting file))
    (niwa--clear-instruction-state workspace)
    (niwa-instruction-store-observe workspace)
    (let* ((listing (niwa-tool-note-test--read data-buf "notes://"))
           (document (progn
                       (should (string-match "notes://[a-z0-9-]+\\.md" listing))
                       (niwa-tool-note-test--read data-buf (match-string 0 listing)))))
      (should (string-search "#2" listing))
      (dolist (expected '("Review this" "review" "agent" "unsent"))
        (should (string-search expected document)))))
  :doc "edits text and replaces tags, then retains done and deleted notes after reload"
  (niwa-tool-note-test--workspace
    (let* ((id (niwa-tool-note-test--create data-buf file))
           (address (concat "notes://" id ".md")))
      (should (eq 'success (plist-get
                           (niwa-tool-note-test--write
                            data-buf (vector (list :action "edit" :id id
                                                   :text "Updated finding" :tags ["fix"]))) :status)))
      (let ((text (niwa-tool-note-test--read data-buf address)))
        (should (string-search "Updated finding" text))
        (should (string-search "fix" text)))
      (dolist (action '("done" "delete" "close"))
        (should (eq 'success (plist-get
                             (niwa-tool-note-test--write
                              data-buf (vector (list :action action :id id))) :status)))
        (kill-buffer (find-buffer-visiting file))
        (niwa--clear-instruction-state workspace)
        (let ((text (niwa-tool-note-test--read data-buf address)))
          (should (string-search "closed" text))
          (should (string-search (if (equal action "delete") "deleted" "done") text)))
        (niwa-note-restore id workspace))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))))
  :doc "read-only owners may create findings but cannot edit, send or close existing notes"
  (niwa-tool-note-test--workspace
    (dolist (mode '(plan request invocation explorer))
      (with-current-buffer data-buf
        (setf (niwa-session-plan-mode session) (eq mode 'plan)
              (niwa-session-preset-name session) 'niwa-implement)
        (setq-local niwa--current-request
                    (and (eq mode 'request) (niwa-request--create :plan-read-only t))
                    niwa--agent-invocation
                    (cond ((eq mode 'invocation) (niwa-agent-invocation--create
                                                 :plan-read-only t :path "/root/review" :parent-session session))
                          ((eq mode 'explorer) (niwa-agent-invocation--create
                                               :path "/root/review" :parent-session session
                                               :agent (niwa-agent-get 'explorer))))))
      (let* ((id (niwa-tool-note-test--create data-buf file))
             (address (concat "notes://" id ".md")))
        (dolist (action '("edit" "done" "delete" "close" "send"))
          (let ((outcome (niwa-tool-note-test--write
                          data-buf (vector (list :action action :id id :text "Forbidden")))))
            (should (eq 'error (plist-get outcome :status)))
            (should (string-match-p "[Rr]ead-only" (plist-get outcome :result)))))
        (should (string-search "Finding" (niwa-tool-note-test--read data-buf address)))
        (should (string-search "unsent" (niwa-tool-note-test--read data-buf address))))))
  :doc "a created note carries a durable prepared change that only the user applies"
  (niwa-tool-note-test--workspace
    (setf (niwa-session-plan-mode session) t)
    (let* ((id (niwa-tool-note-test--create
                data-buf file :prepared_change
                "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))
      (kill-buffer (find-buffer-visiting file))
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (let ((note (niwa-note-resolve id workspace)))
        (should (string-search "change ready"
                               (overlay-get (overlay-get note 'niwa-label-overlay) 'after-string)))
        (niwa-note-apply-change note)
        (should (equal "before\nnew\nafter\n" (niwa-test--read-file file)))
        (should (string-search "State: closed"
                               (niwa-tool-note-test--read data-buf (concat "notes://" id ".md")))))))
  :doc "creation refuses to steal a source buffer from a different workspace"
  (niwa-tool-note-test--workspace
    (let* ((nested (file-name-concat root "nested"))
           (other (niwa-workspace-get-or-create 'file nested nested "other")))
      (with-current-buffer (find-file-noselect file)
        (setq-local niwa--workspace other))
      (let ((outcome (niwa-tool-note-test--write
                      data-buf (vector (list :action "create" :file_path file :start_line 2)))))
        (should (eq 'error (plist-get outcome :status)))
        (should (string-search "different workspace" (plist-get outcome :result))))
      (with-current-buffer (find-buffer-visiting file)
        (should (eq other niwa--workspace)))))
  :doc "prepared suggestions respect ApplyPatch hard-deny path rules even in Plan"
  (niwa-tool-note-test--workspace
    (setf (niwa-session-plan-mode session) t
          (niwa-session-permission-rules session)
          (list (list "ApplyPatch" :path file :action 'deny)))
    (let ((outcome (niwa-tool-note-test--write
                    data-buf
                    (vector (list :action "create" :file_path file :start_line 2
                                  :prepared_change
                                  "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))))
      (should (eq 'error (plist-get outcome :status)))
      (should (string-search "Prepared change is denied" (plist-get outcome :result)))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))))
  :doc "partial failure reports already-created UUIDs and persists those notes once"
  (niwa-tool-note-test--workspace
    (let ((outcome (niwa-tool-note-test--write
                    data-buf
                    (vector (list :action "create" :file_path file :start_line 2 :text "Kept finding")
                            (list :action "create" :file_path file :start_line 999)))))
      (should (eq 'error (plist-get outcome :status)))
      (should (string-search "Completed" (plist-get outcome :result)))
      (should (string-match "notes://[a-z0-9-]+\\.md" (plist-get outcome :result)))
      (let ((address (match-string 0 (plist-get outcome :result))))
        (kill-buffer (find-buffer-visiting file))
        (niwa--clear-instruction-state workspace)
        (should (string-search "Kept finding" (niwa-tool-note-test--read data-buf address)))))))

(niwa-deftest niwa-tool-note-write/guards (:quiet t)
  ,test
  (test)
  :doc "create refuses workspace escapes and absolute external paths without changes"
  (niwa-tool-note-test--workspace
    (let ((outside (make-temp-file "note-outside-" nil ".txt" "outside\n")))
      (unwind-protect
          (dolist (path (list (file-relative-name outside root) outside "/etc/hosts"))
            (let ((before (niwa-test--read-file (expand-file-name path root)))
                  (outcome (niwa-tool-note-test--write
                            data-buf (vector (list :action "create" :file_path path
                                                   :start_line 1 :text "Forbidden")))))
              (should (eq 'error (plist-get outcome :status)))
              (should (equal before (niwa-test--read-file (expand-file-name path root))))
              (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))))
        (delete-file outside))))
  :doc "prepared changes refuse protected source paths without writing them"
  (niwa-tool-note-test--workspace
    (let* ((niwa-protected-paths (list (cons file 'read-only)))
           (outcome (niwa-tool-note-test--write
                     data-buf (vector (list :action "create" :file_path file :start_line 2
                                            :prepared_change
                                            "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")))))
      (should (eq 'error (plist-get outcome :status)))
      (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))))
  :doc "prepared resource writes and moves refuse without changing source"
  (niwa-tool-note-test--workspace
    (dolist (patch '("*** Begin Patch\n*** Add File: work://shared/refused.md\n+forbidden\n*** End Patch"
                     "*** Begin Patch\n*** Update File: source.txt\n*** Move to: work://shared/refused.md\n@@\n-old\n+new\n*** End Patch"))
      (let ((outcome (niwa-tool-note-test--write
                      data-buf (vector (list :action "create" :file_path file
                                             :start_line 2 :prepared_change patch)))))
        (should (eq 'error (plist-get outcome :status)))
        (should (string-search "not resources" (plist-get outcome :result)))
        (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))))))

(niwa-deftest niwa-tool-note-write/line-bounds (:quiet t)
  ,test
  (test)
  :doc "a trailing newline does not create an addressable fourth source line"
  (niwa-tool-note-test--workspace
    (dolist (range '((4 4) (3 4)))
      (let ((outcome (niwa-tool-note-test--write
                      data-buf (vector (list :action "create" :file_path file
                                             :start_line (car range) :end_line (cadr range))))))
        (should (eq 'error (plist-get outcome :status)))
        (should (string-search "beyond source" (plist-get outcome :result)))
        (should (equal "before\nold\nafter\n" (niwa-test--read-file file)))))))

(niwa-deftest niwa-tool-note-write/match
  (:quiet t)
  ,test
  (test)
  :doc "occurrence 1 can select a unique match and gets a region tint and label"
  (niwa-tool-note-test--workspace
    (write-region "before\n(use value)\nafter\n" nil file nil 'silent)
    (let* ((id (niwa-tool-note-test--create data-buf file :match "value" :occurrence 1))
           (note (niwa-note-resolve id workspace))
           (label (overlay-get note 'niwa-label-overlay)))
      (with-current-buffer (overlay-buffer note)
        (should (equal "value" (buffer-substring-no-properties (overlay-start note) (overlay-end note))))
        (should (eq 'char (overlay-get note 'niwa-span)))
        (should (eq 'niwa-note-agent (get-char-property (overlay-start note) 'face)))
        (should-not (eq 'niwa-note-agent (get-char-property (overlay-end note) 'face)))
        (should (= (overlay-end label) (1+ (overlay-end note))))
        (should-not (get-text-property 0 'face (overlay-get label 'after-string)))
        (should (string-match-p "\n.*Note .* · AI  Finding" (overlay-get label 'after-string))))))
  :doc "a match can cross lines without expanding to the whole search range"
  (niwa-tool-note-test--workspace
    (write-region "before\n(use value\n  again)\nafter\n" nil file nil 'silent)
    (let* ((text "value\n  again")
           (id (niwa-tool-note-test--create data-buf file :match text :end_line 3))
           (note (niwa-note-resolve id workspace)))
      (with-current-buffer (overlay-buffer note)
        (should (equal text (buffer-substring-no-properties (overlay-start note) (overlay-end note)))))))
  :doc "refuses missing, ambiguous and invalid occurrence requests without writing a note"
  (niwa-tool-note-test--workspace
    (write-region "value\nvalue Value value\nother\n" nil file nil 'silent)
    (dolist (case '(((:match "value") "ambiguous: 2 matches")
                    ((:match "VALUE") "not found in lines 2–2")
                    ((:match "value\nother") "not found in lines 2–2")
                    ((:match "value" :occurrence 3) "out of range")
                    ((:match "Value" :occurrence 2) "out of range")
                    ((:occurrence 1) "requires match")))
      (let ((outcome (niwa-tool-note-test--write
                      data-buf (vector (append (list :action "create" :file_path file :start_line 2)
                                               (car case))))))
        (should (eq 'error (plist-get outcome :status)))
        (should (string-search (cadr case) (plist-get outcome :result)))))
    (should-not (niwa-notes-open workspace))
    (let* ((id (niwa-tool-note-test--create data-buf file :match "value" :occurrence 2))
           (note (niwa-note-resolve id workspace)))
      (with-current-buffer (overlay-buffer note)
        (should (= (overlay-end note) (save-excursion (goto-char (overlay-start note))
                                                     (line-end-position))))))))

(niwa-deftest niwa-tool-note-write/tint (:quiet t)
  ,test
  (test)
  :doc "eol-ending line notes tint the newline without moving their code or label"
  (niwa-tool-note-test--workspace
    (let* ((niwa-note-labels 'collapsed)
           (id (niwa-tool-note-test--create data-buf file :end_line 3))
           (note (niwa-note-resolve id workspace)))
      (with-current-buffer (overlay-buffer note)
        (should (= 8 (overlay-start note)))
        (should (= 17 (overlay-end note)))
        (should (= 17 (overlay-end (overlay-get note 'niwa-label-overlay))))
        (should (eq 'default (get-char-property 17 'face)))
        (should (eq 'niwa-note-agent
                    (get-text-property 0 'face
                                       (overlay-get (overlay-get note 'niwa-label-overlay)
                                                    'after-string))))
        (should-not (cl-some (lambda (overlay) (overlay-get overlay 'face))
                             (overlays-at 18))))))
  :doc "newline tint follows edits and undo in both label modes and is removed on close"
  (niwa-tool-note-test--workspace
    (let* ((id (niwa-tool-note-test--create data-buf file))
           (note (niwa-note-resolve id workspace)))
      (with-current-buffer (overlay-buffer note)
        (buffer-enable-undo)
        (dolist (display '(collapsed expanded))
          (niwa-notes-toggle-labels)
          (should (eq display (overlay-get note 'niwa-note-labels)))
          (setq buffer-undo-list nil)
          (goto-char 9)
          (insert "new\n")
          (undo-boundary)
          (should (= 15 (overlay-end note)))
          (should (= 15 (overlay-end (overlay-get note 'niwa-label-overlay))))
          (should (eq 'default (get-char-property 15 'face)))
          (should (eq 'niwa-note-agent
                      (get-text-property 0 'face
                                         (overlay-get (overlay-get note 'niwa-label-overlay)
                                                      'after-string))))
          (let (last-command this-command) (undo-only 1))
          (should (equal "before\nold\nafter\n" (buffer-string)))
          (should (= 11 (overlay-end note)))
          (should (= 11 (overlay-end (overlay-get note 'niwa-label-overlay))))
          (should (eq 'default (get-char-property 11 'face))))
        (niwa-note-done note)
        (should-not (overlays-in (point-min) (point-max))))))
  :doc "a final line without a newline gains and loses the extra tint with source undo"
  (niwa-tool-note-test--workspace
    (write-region "before\nold\nafter" nil file nil 'silent)
    (let* ((id (niwa-tool-note-test--create data-buf file :end_line 3))
           (note (niwa-note-resolve id workspace)))
      (with-current-buffer (overlay-buffer note)
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (goto-char (point-max))
        (should-not (cl-some (lambda (overlay) (overlay-get overlay 'face))
                             (overlays-at (point))))
        (insert "\n")
        (undo-boundary)
        (should (= 17 (overlay-end note)))
        (should (eq 'default (get-char-property 17 'face)))
        (undo-only 1)
        (should-not (cl-some (lambda (overlay) (overlay-get overlay 'face))
                             (overlays-at (point-max))))
        (niwa--clear-instruction-state workspace)
        (should-not (overlays-in (point-min) (point-max)))))))

(niwa-deftest niwa-tool-note-write/create-cost (:quiet t)
  ,test
  (test)
  :doc "creating text notes renders twice each without redundant edits, including tags"
  (niwa-tool-note-test--workspace
    (let* ((renders 0) (edits 0)
           (count-render (lambda (&rest _) (cl-incf renders)))
           (count-edit (lambda (&rest _) (cl-incf edits))))
      (advice-add 'niwa-notes--render :before count-render)
      (advice-add 'niwa-note-edit :before count-edit)
      (unwind-protect
          (let ((outcome (niwa-tool-note-test--write
                          data-buf (vector (list :action "create" :file_path file
                                                 :start_line 1 :text "Plain")
                                           (list :action "create" :file_path file
                                                 :start_line 2 :text "Tagged" :tags ["review"])))))
            (should (eq 'success (plist-get outcome :status)))
            (should (= 0 edits))
            (should (= 4 renders)))
        (advice-remove 'niwa-notes--render count-render)
        (advice-remove 'niwa-note-edit count-edit)))
    (with-current-buffer (find-buffer-visiting file)
      (dolist (label (cl-remove-if-not #'stringp
                                     (mapcar (lambda (overlay) (overlay-get overlay 'after-string))
                                             (overlays-in (point-min) (point-max)))))
        (should (string-match-p "AI" label))
        (should-not (string-match-p "You" label))))))

(niwa-deftest niwa-note-dismiss-agent-notes (:quiet t)
  ,test
  (test)
  :doc "single and workspace-wide dismissal preserve user notes and sync a batch once"
  (niwa-tool-note-test--workspace
    (let ((first (niwa-tool-note-test--create data-buf file))
          (second (niwa-tool-note-test--create data-buf file)))
      (with-current-buffer (find-buffer-visiting file)
        (goto-char (point-min))
        (niwa-note-create "Human note"))
      (niwa-note-dismiss (niwa-note-resolve first workspace))
      (should (string-search "dismissed" (niwa-tool-note-test--read
                                          data-buf (concat "notes://" first ".md"))))
      (niwa-note-restore first workspace)
      (niwa-instruction-store-flush t)
      (kill-buffer (find-buffer-visiting file))
      (niwa--clear-instruction-state workspace)
      (let* ((syncs 0) (counter (lambda (&rest _) (cl-incf syncs))))
        (advice-add 'niwa-instruction-store-sync :before counter)
        (unwind-protect
            (progn
              (should (= 2 (niwa-note-dismiss-agent-notes workspace)))
              (should (= 1 syncs)))
          (advice-remove 'niwa-instruction-store-sync counter)))
      (dolist (id (list first second))
        (should (string-search "dismissed" (niwa-tool-note-test--read
                                            data-buf (concat "notes://" id ".md")))))
      (let ((listing (niwa-tool-note-test--read data-buf "notes://")))
        (should (string-match-p "user unsent" listing)))))
  :doc "missing sources refuse dismiss-all before it can dismiss a different note at point"
  (niwa-tool-note-test--workspace
    (niwa-tool-note-test--create data-buf file)
    (let* ((other (file-name-concat root "other.txt"))
           (_ (write-region "before\nold\nafter\n" nil other nil 'silent))
           (id (niwa-tool-note-test--create data-buf other)))
      (kill-buffer (find-buffer-visiting file))
      (delete-file file)
      (with-current-buffer (find-buffer-visiting other)
        (goto-char (point-min))
        (forward-line 1)
        (should-error (niwa-note-dismiss-agent-notes workspace) :type 'user-error))
      (should (string-search "unsent" (niwa-tool-note-test--read
                                       data-buf (concat "notes://" id ".md")))))))

(niwa-deftest niwa-note-dismiss-agent-notes/lookup-cost (:quiet t)
  ,test
  (test)
  :doc "bulk dismissal collects entries once and restores each source once"
  (niwa-tool-note-test--workspace
    (niwa-tool-note-test--create data-buf file)
    (niwa-tool-note-test--create data-buf file)
    (kill-buffer (find-buffer-visiting file))
    (let* ((builds 0) (restores 0)
           (count-build (lambda (&rest _) (cl-incf builds)))
           (count-restore (lambda (&rest _) (cl-incf restores))))
      (advice-add 'niwa-notes-open :before count-build)
      (advice-add 'niwa--restore-file-instructions :before count-restore)
      (unwind-protect
          (progn
            (should (= 2 (niwa-note-dismiss-agent-notes workspace)))
            (should (= 1 builds))
            (should (= 1 restores)))
        (advice-remove 'niwa-notes-open count-build)
        (advice-remove 'niwa--restore-file-instructions count-restore)))))

(provide 'test-niwa-tool-note)
;;; test-niwa-tool-note.el ends here
