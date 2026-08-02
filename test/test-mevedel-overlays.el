;;; test-mevedel-overlays.el --- Tests for mevedel-overlays.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-persistence)
(require 'mevedel-chat)
(require 'mevedel-menu)
(require 'mevedel-models)
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

(defun mevedel-overlays-test--make-directive (content request workspace)
  "Create a file buffer containing CONTENT and one directive in WORKSPACE."
  (let* ((file (make-temp-file "mevedel-directive-" nil ".txt" content))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (fundamental-mode)
      (setq-local mevedel--workspace workspace)
      (set-buffer-modified-p nil)
      (cons buffer
            (mevedel--create-directive-in
             buffer (point-min) (1- (point-max)) nil request)))))

(defun mevedel-overlays-test--discard-directive (cell)
  "Kill and delete the file belonging to directive CELL."
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
;;; Instruction presentation

(mevedel-deftest mevedel--instruction-directive-color ()
  ,test
  (test)
  :doc "inherits implementing and failure colors from the top directive"
  (with-temp-buffer
    (insert "directive")
    (let ((parent (make-overlay 1 5))
          (child (make-overlay 2 4))
          (parent-status 'implementing)
          (mevedel-directive-processing-color "processing")
          (mevedel-directive-fail-color "failed")
          (mevedel-directive-success-color "succeeded"))
      (cl-letf (((symbol-function 'mevedel--topmost-instruction)
                 (lambda (&rest _) parent))
                ((symbol-function 'mevedel--directive-status)
                 (lambda (instruction)
                   (if (eq instruction parent)
                       parent-status
                     'implemented))))
        (should (equal "processing"
                       (mevedel--instruction-directive-color child)))
        (setq parent-status 'failed)
        (should (equal "failed"
                       (mevedel--instruction-directive-color child)))))))

(mevedel-deftest mevedel--instruction-action-setup ()
  ,test
  (test)
  :doc "selects status-specific directive interactions"
  (with-temp-buffer
    (insert "directive")
    (let ((instruction (make-overlay 1 5)))
      (cl-letf (((symbol-function 'mevedel--directive-status)
                 (lambda (_) 'failed)))
        (mevedel--instruction-action-setup instruction 'directive))
      (should (eq (overlay-get instruction 'keymap)
                  mevedel-directive-failed-actions-map))
      (should (string-match-p
               "Request failed"
               (overlay-get instruction 'help-echo))))))

(mevedel-deftest mevedel--instruction-directive-typename ()
  ,test
  (test)
  :doc "derives child directive names without mutating overlays"
  (with-temp-buffer
    (insert "abc")
    (let ((parent (make-overlay 1 3))
          (child (make-overlay 1 2)))
      (overlay-put parent 'mevedel-instruction-type 'directive)
      (cl-letf (((symbol-function 'mevedel--directive-status)
                 (lambda (_) 'implemented)))
        (should (equal "CORRECTION"
                       (mevedel--instruction-directive-typename
                        child parent))))
      (should-not (overlay-get child 'mevedel-subdirective-typename)))))

(mevedel-deftest mevedel--instruction-label ()
  ,test
  (test)
  :doc "labels same-type reference links as reference links"
  (with-temp-buffer
    (insert "reference")
    (let ((reference (make-overlay 1 5))
          (target (make-overlay 2 4))
          (mevedel-reference-color "reference-color"))
      (overlay-put reference 'mevedel-id 1)
      (overlay-put reference 'mevedel-links '(:to (2)))
      (overlay-put target 'mevedel-id 2)
      (overlay-put target 'mevedel-instruction-type 'reference)
      (cl-letf (((symbol-function 'mevedel--instruction-with-id)
                 (lambda (id) (and (= id 2) target)))
                ((symbol-function 'mevedel--parent-instruction)
                 (lambda (&rest _) nil)))
        (let ((presentation
               (mevedel--instruction-label
                (list :instruction reference :type 'reference
                      :padding ""))))
          (should (equal (plist-get presentation :color)
                         "reference-color"))
          (should (string-match-p
                   "REFERENCE LINKS: TO: #2"
                   (substring-no-properties
                    (plist-get presentation :label)))))))))

(mevedel-deftest mevedel--instruction-style ()
  ,test
  (test)
  :doc "stores presentation and uses the supplied parent for indentation"
  (with-temp-buffer
    (insert "abc")
    (let ((instruction (make-overlay 1 2))
          (parent (make-overlay 1 3)))
      (overlay-put parent 'mevedel-label-color "parent-label")
      (overlay-put parent 'mevedel-bg-color "parent-bg")
      (cl-letf (((symbol-function 'face-foreground)
                 (lambda (&rest _) "foreground"))
                ((symbol-function 'face-background)
                 (lambda (&rest _) "background"))
                ((symbol-function 'mevedel--tint)
                 (lambda (_source tint &optional _intensity) tint))
                ((symbol-function 'mevedel--parent-instruction)
                 (lambda (&rest _)
                   (ert-fail "Style recomputed the parent"))))
        (mevedel--instruction-style
         (list :instruction instruction
               :type 'reference
               :label "REFERENCE #1\n child"
               :color "reference"
               :padding " "
               :priority mevedel--default-instruction-priority
               :parent parent)))
      (should (= (overlay-get instruction 'priority)
                 mevedel--default-instruction-priority))
      (should (string-match-p
               "REFERENCE #1"
               (substring-no-properties
                (overlay-get instruction 'before-string))))
      (should (overlay-get instruction 'mevedel-bg-color))
      (should (overlay-get instruction 'face)))))

(mevedel-deftest mevedel--update-instruction-overlay-tree ()
  ,test
  (test)
  :doc "renders priorities and clears stale names under mixed-type parents"
  (with-temp-buffer
    (insert "abcdef")
    (let ((parent (make-overlay 1 6))
          (child (make-overlay 2 4))
          rendered)
      (overlay-put parent 'mevedel-instruction-type 'reference)
      (overlay-put child 'mevedel-instruction-type 'directive)
      (overlay-put child 'mevedel-subdirective-typename "STALE")
      (cl-letf (((symbol-function 'mevedel--instruction-bufferlevel-p)
                 (lambda (_instruction) nil))
                ((symbol-function 'mevedel--instruction-label)
                 (lambda (presentation)
                   (append presentation
                           '(:label "label" :color "color"))))
                ((symbol-function 'mevedel--instruction-action-setup)
                 #'ignore)
                ((symbol-function 'mevedel--instruction-style)
                 (lambda (presentation)
                   (push
                    (cons (plist-get presentation :instruction)
                          (plist-get presentation :priority))
                    rendered)))
                ((symbol-function 'mevedel--child-instructions)
                 (lambda (instruction)
                   (and (eq instruction parent) (list child)))))
        (mevedel--update-instruction-overlay-tree
         parent t mevedel--default-instruction-priority nil))
      (should
       (equal (mapcar #'cdr (nreverse rendered))
              (list mevedel--default-instruction-priority
                    (1+ mevedel--default-instruction-priority))))
      (should-not
       (overlay-get child 'mevedel-subdirective-typename)))))

(mevedel-deftest mevedel--update-instruction-overlay ()
  ,test
  (test)
  :doc "deletes a congruent conflicting instruction"
  (with-temp-buffer
    (insert "abc")
    (let ((instruction (make-overlay 1 2))
          (other (make-overlay 1 2))
          deleted)
      (cl-letf (((symbol-function 'mevedel--instructions-at)
                 (lambda (_point) (list instruction other)))
                ((symbol-function 'mevedel--instructions-congruent-p)
                 (lambda (&rest _) t))
                ((symbol-function 'mevedel--delete-instruction)
                 (lambda (value &rest _) (setq deleted value))))
        (mevedel--update-instruction-overlay instruction))
      (should (eq deleted instruction))))
  :doc "renders a non-conflicting root at the default priority"
  (with-temp-buffer
    (insert "abc")
    (let ((instruction (make-overlay 1 2))
          rendered)
      (cl-letf (((symbol-function 'mevedel--instructions-at)
                 (lambda (_point) (list instruction)))
                ((symbol-function 'mevedel--parent-instruction)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel--update-instruction-overlay-tree)
                 (lambda (value children priority parent)
                   (setq rendered
                         (list value children priority parent)))))
        (mevedel--update-instruction-overlay instruction t))
      (should
       (equal rendered
              (list instruction t
                    mevedel--default-instruction-priority nil))))))


;;
;;; Directive model selection

(mevedel-deftest mevedel--directive-model-values ()
  ,test
  (test)

  :doc "returns a pinned directive model without consulting the session"
  (with-temp-buffer
    (insert "directive")
    (let ((directive (make-overlay (point-min) (point-max))))
      (overlay-put directive
                   'mevedel-directive-model-provider "Pinned:model")
      (overlay-put directive
                   'mevedel-directive-reasoning-effort 'high)
      (should
       (equal '("Pinned:model" high nil)
              (mevedel--directive-model-values directive)))))

  :doc "returns the effective inherited session model"
  (let ((session-buffer (generate-new-buffer " *directive-values-session*")))
    (unwind-protect
        (progn
          (with-current-buffer session-buffer
            (setq-local gptel-reasoning-effort 'medium))
          (with-temp-buffer
            (insert "directive")
            (let ((directive (make-overlay (point-min) (point-max))))
              (cl-letf (((symbol-function 'mevedel--chat-buffer)
                         (lambda (&rest _) session-buffer))
                        ((symbol-function 'mevedel-workspace)
                         (lambda (&optional _) 'workspace))
                        ((symbol-function
                          'mevedel-model-current-provider-label)
                         (lambda (&optional _) "Session:model")))
                (should
                 (equal '("Session:model" medium t)
                        (mevedel--directive-model-values directive)))))))
      (kill-buffer session-buffer))))

(mevedel-deftest mevedel--ov-actions-model
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "pins and resets the request-owning top-level directive"
  (let ((session-buffer (generate-new-buffer " *directive-model-session*"))
        (workspace (mevedel-workspace--create
                    :type 'test :id "model-owner" :root "/tmp"
                    :name "model-owner"))
        options)
    (unwind-protect
        (progn
          (with-current-buffer session-buffer
            (setq-local gptel-reasoning-effort 'medium))
          (with-temp-buffer
            (insert "abcdef")
            (setq-local mevedel--workspace workspace)
            (let ((parent (mevedel--create-directive-in
                           (current-buffer) 1 7 nil "Parent"))
                  child)
              (setq child
                    (mevedel--create-directive-in
                     (current-buffer) 2 4 nil "Child"))
              (cl-letf (((symbol-function 'mevedel--chat-buffer)
                         (lambda (&rest _) session-buffer))
                        ((symbol-function 'mevedel-workspace)
                         (lambda (&optional _) workspace))
                        ((symbol-function
                          'mevedel-model-current-provider-label)
                         (lambda (&optional _) "Session:session-model"))
                        ((symbol-function
                          'mevedel-menu-open-model-selection)
                         (lambda (&rest args) (setq options args))))
                (mevedel--ov-actions-model child))
              (should (equal "Session:session-model"
                             (plist-get options :provider)))
              (should (eq 'medium (plist-get options :effort)))
              (should (eq t (plist-get options :inherited)))
              (funcall (plist-get options :update)
                       "Fast:fast-model" 'high)
              (should (equal "Fast:fast-model"
                             (overlay-get
                              parent 'mevedel-directive-model-provider)))
              (should (eq 'high
                          (overlay-get
                           parent 'mevedel-directive-reasoning-effort)))
              (should-not
               (overlay-get child 'mevedel-directive-model-provider))
              (should (string-match-p
                       "MODEL: Fast:fast-model · effort high"
                       (substring-no-properties
                        (overlay-get parent 'before-string))))
              (let ((properties
                     (mevedel--instruction-persisted-properties parent)))
                (should
                 (equal "Fast:fast-model"
                        (plist-get
                         properties 'mevedel-directive-model-provider)))
                (should
                 (eq 'high
                     (plist-get
                      properties
                      'mevedel-directive-reasoning-effort))))
              (should
               (equal
                '("Session:session-model" medium)
                (cl-letf (((symbol-function 'mevedel--chat-buffer)
                           (lambda (&rest _) session-buffer))
                          ((symbol-function 'mevedel-workspace)
                           (lambda (&optional _) workspace))
                          ((symbol-function
                            'mevedel-model-current-provider-label)
                           (lambda (&optional _)
                             "Session:session-model")))
                  (funcall (plist-get options :reset)))))
              (should-not
               (overlay-get parent 'mevedel-directive-model-provider))
              (should-not
               (overlay-get parent 'mevedel-directive-reasoning-effort)))))
      (kill-buffer session-buffer)))

  :doc "shows the effective pair except while the directive is implementing"
  (let ((session-buffer (generate-new-buffer " *directive-actions-session*"))
        (workspace (mevedel-workspace--create
                    :type 'test :id "action-owner" :root "/tmp"
                    :name "action-owner"))
        choices action-row target)
    (unwind-protect
        (with-temp-buffer
          (insert "directive")
          (setq-local mevedel--workspace workspace)
          (let* ((directive
                  (mevedel--create-directive-in
                   (current-buffer) (point-min) (point-max)
                   nil "Test"))
                 (child
                  (mevedel--create-directive-in
                   (current-buffer) 2 5 nil "Child")))
            (setq target directive)
            (cl-letf (((symbol-function 'mevedel--chat-buffer)
                       (lambda (&rest _) session-buffer))
                      ((symbol-function 'mevedel-workspace)
                       (lambda (&optional _) workspace))
                      ((symbol-function
                        'mevedel-model-current-provider-label)
                       (lambda (&optional _) "Session:model"))
                      ((symbol-function 'read-multiple-choice)
                       (lambda (_prompt options)
                         (setq choices options
                               action-row
                               (substring-no-properties
                                (overlay-get target 'before-string)))
                         (throw 'captured nil))))
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?M "model") choices))
              (should (member '(?o "activity") choices))
              (should-not (member '(?r "revise") choices))
              (should
               (string-match-p
                "Session:model · effort default · session"
                action-row))
              (mevedel--set-directive-status directive 'implementing)
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should-not (assoc ?M choices))
              (should (member '(?o "activity") choices))
              (should (member '(?a "abort") choices))
              (mevedel--set-directive-status directive 'discussed)
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?d "continue-discussion") choices))
              (should (member '(?i "implement-this") choices))
              (mevedel--set-directive-status directive 'implemented)
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?c "request-changes") choices))
              (should-not (member '(?i "implement") choices))
              (should-not (member '(?r "revise") choices))
              (mevedel--set-directive-status directive 'failed)
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?r "retry") choices))
              (should-not (member '(?i "implement") choices))
              (mevedel--set-directive-status directive 'aborted)
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?r "retry") choices))
              (mevedel--set-directive-status directive 'discussing)
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should-not (assoc ?M choices))
              (should (member '(?a "abort") choices))
              (setq target child)
              (catch 'captured
                (mevedel--ov-actions-dispatch child))
              (should-not (assoc ?M choices))
              (should-error
               (mevedel--ov-actions-model child)
               :type 'user-error))))
      (kill-buffer session-buffer))))

(mevedel-deftest mevedel--instruction-label/request-changed
  (:doc "shows Ready with a request-changed qualifier after an authored edit")
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "edited" :root "/tmp" :name "edited"))
           (record
            (mevedel-directive--create
             :id "directive" :request "Edited request"
             :anchor '(:state attached) :state nil
             :attempts
             (list
              (mevedel-directive-attempt--create
               :directive-request "Original request" :request "Exact"
               :result "Done" :outcome 'success :patch ""
               :capture 'complete :captured-at "2026-07-01T10:00:00+0200"))))
           (directive (make-overlay (point-min) (point-max))))
      (mevedel-workspace-add-directive workspace record)
      (setq-local mevedel--workspace workspace)
      (overlay-put directive 'mevedel-uuid "directive")
      (overlay-put directive 'mevedel-id 1)
      (overlay-put directive 'mevedel-instruction-type 'directive)
      (let ((presentation
             (mevedel--instruction-label
              (list :instruction directive :type 'directive :padding ""))))
        (should
         (string-match-p
          "READY.*REQUEST CHANGED"
          (substring-no-properties (plist-get presentation :label))))))))


;;
;;; Lookup

(mevedel-deftest mevedel-get-directive-patch ()
  ,test
  (test)
  :doc "reads patch history from the latest workspace-owned attempt"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "patch" :root "/tmp" :name "patch"))
         (record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :anchor '(:state attached) :state 'implemented
                  :attempts
                  (list
                   (mevedel-directive-attempt--create :patch "first")
                   (mevedel-directive-attempt--create :patch "latest"))))
         (buffer (generate-new-buffer " *directive-patch-owner*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "source")
          (setq-local mevedel--workspace workspace)
          (mevedel-workspace-add-directive workspace record)
          (let ((directive (make-overlay (point-min) (point-max))))
            (overlay-put directive 'mevedel-uuid "directive")
            (overlay-put directive 'mevedel-instruction-type 'directive)
            (should (equal "latest"
                           (mevedel-get-directive-patch directive)))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel--create-directive-in
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "creates one workspace-owned record resolved by the source overlay"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "create-directive" :root "/tmp"
                     :name "create-directive"))
         (cell (mevedel-overlays-test--make-directive
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
      (mevedel-overlays-test--discard-directive cell))))

(mevedel-deftest mevedel--create-reference-in
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "keeps references source-bound and outside durable directive records"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "create-reference" :root "/tmp"
                     :name "create-reference"))
         (cell (mevedel-overlays-test--make-reference
                "reference body\n" workspace)))
    (unwind-protect
        (progn
          (should (overlay-get (cdr cell) 'evaporate))
          (should-not (mevedel-workspace-directives workspace)))
      (mevedel-overlays-test--discard-reference cell))))

(mevedel-deftest mevedel--set-directive-request
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "updates the durable request without replacing directive identity"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "edit-directive" :root "/tmp"
                     :name "edit-directive"))
         (cell (mevedel-overlays-test--make-directive
                "directive body\n" "Initial request" workspace))
         (directive (cdr cell))
         (record (mevedel--directive-record directive)))
    (unwind-protect
        (progn
          (mevedel--set-directive-request directive "Changed request")
          (should (eq record (mevedel--directive-record directive)))
          (should (equal "Changed request"
                         (mevedel-directive-request record))))
      (mevedel-overlays-test--discard-directive cell))))

(mevedel-deftest mevedel--delete-instruction
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "removes a Ready directive record with its presentation overlay"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "delete-directive" :root "/tmp"
                     :name "delete-directive"))
         (cell (mevedel-overlays-test--make-directive
                "directive body\n" "Unused request" workspace))
         (directive (cdr cell)))
    (unwind-protect
        (progn
          (should (mevedel-workspace-directives workspace))
          (mevedel--delete-instruction directive)
          (should-not (mevedel-workspace-directives workspace)))
      (mevedel-overlays-test--discard-directive cell))))

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
