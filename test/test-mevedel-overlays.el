;;; test-mevedel-overlays.el --- Tests for mevedel-overlays.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-persistence)
(require 'mevedel-chat)
(require 'mevedel-menu)
(require 'mevedel-models)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'mevedel-directive)
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
                    (plist-get presentation :label))))))))

  :doc "shows Ready with a request-changed qualifier after an authored edit"
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace (mevedel-workspace--create
                       :type 'file :id "edited" :root "/tmp" :name "edited"))
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
                    :type 'file :id "model-owner" :root "/tmp"
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
                    :type 'file :id "action-owner" :root "/tmp"
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
              (should (member '(?s "settings") choices))
              (should-not (member '(?o "activity") choices))
              (should-not (member '(?r "revise") choices))
              (should
               (string-match-p
                "Session:model · effort default · session"
                action-row))
              (setf (mevedel-directive-plan
                     (mevedel--directive-record directive))
                    '(:status draft))
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?P "continue-plan") choices))
              (setf (mevedel-directive-plan
                     (mevedel--directive-record directive))
                    nil)
              (mevedel--set-directive-status directive 'implementing)
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should-not (assoc ?s choices))
              (should (member '(?o "activity") choices))
              (should-not (member '(?k "clear") choices))
              (should-not (member '(?A "archive") choices))
              (should (member '(?a "abort") choices))
              (mevedel--set-directive-status directive 'discussed)
              (setf
               (mevedel-directive-discussion
                (mevedel--directive-record directive))
               (list (mevedel-directive-discussion-turn--create
                      :sequence 1 :directive-request "Test"
                      :outcome 'success)))
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?o "activity") choices))
              (should (member '(?d "continue-discussion") choices))
              (should (member '(?i "implement-this") choices))
              (setf
               (mevedel-directive-attempts
                (mevedel--directive-record directive))
               (list (mevedel-directive-attempt--create
                      :directive-request "Test" :outcome 'success
                      :patch "")))
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?c "request-changes") choices))
              (should-not (assoc ?v choices))
              (should-not (member '(?i "implement") choices))
              (should-not (member '(?r "revise") choices))
              (setf
               (mevedel-directive-attempt-patch
                (car (mevedel-directive-attempts
                      (mevedel--directive-record directive))))
               "patch")
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?v "view-changes") choices))
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?A "archive") choices))
              (should-not (member '(?k "clear") choices))
              (setf
               (mevedel-directive-attempts
                (mevedel--directive-record directive))
               (list (mevedel-directive-attempt--create
                      :directive-request "Test" :outcome 'failure)))
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?r "retry") choices))
              (should-not (member '(?i "implement") choices))
              (setf
               (mevedel-directive-attempts
                (mevedel--directive-record directive))
               (list (mevedel-directive-attempt--create
                      :directive-request "Test" :outcome 'aborted)))
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should (member '(?r "retry") choices))
              (mevedel--set-directive-status directive 'discussing)
              (catch 'captured
                (mevedel--ov-actions-dispatch directive))
              (should-not (assoc ?s choices))
              (should (member '(?a "abort") choices))
              (setq target child)
              (catch 'captured
                (mevedel--ov-actions-dispatch child))
              (should-not (assoc ?s choices))
              (should-error
               (mevedel--ov-actions-model child)
               :type 'user-error))))
      (kill-buffer session-buffer))))

(mevedel-deftest mevedel--ov-actions-settings
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)

  :doc "toggles Plan before implementation on the request owner"
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace
            (mevedel-workspace--create
             :type 'file :id "settings" :root "/tmp" :name "settings"))
           (mevedel--workspace workspace)
           (directive
            (mevedel--create-directive-in
             (current-buffer) (point-min) (point-max) nil "Test"))
           (record (mevedel--directive-record directive)))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) '(?p "plan before implementation: off"))))
        (mevedel--ov-actions-settings directive))
      (should (mevedel-directive-planning-enabled record))
      (should (string-search "PLAN: ON"
                             (substring-no-properties
                              (overlay-get directive 'before-string))))))

  :doc "labels model selection as planning-specific only when Plan is on"
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace
            (mevedel-workspace--create
             :type 'file :id "settings-label" :root "/tmp"
             :name "settings-label"))
           (mevedel--workspace workspace)
           (directive
            (mevedel--create-directive-in
             (current-buffer) (point-min) (point-max) nil "Test"))
           (record (mevedel--directive-record directive))
           labels model-opened)
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (_prompt choices)
                   (setq labels choices)
                   '(?m "model/effort")))
                ((symbol-function 'mevedel--ov-actions-model)
                 (lambda (_) (setq model-opened t))))
        (mevedel--ov-actions-settings directive)
        (should (member '(?m "model/effort") labels))
        (should model-opened)
        (setf (mevedel-directive-planning-enabled record) t)
        (mevedel--ov-actions-settings directive)
        (should (member '(?m "planning model/effort") labels)))))

  :doc "toggles a skill, labels it on the overlay, and backs out to actions"
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace
            (mevedel-workspace--create
             :type 'file :id "settings-skills" :root "/tmp"
             :name "settings-skills"))
           (mevedel--workspace workspace)
           (directive
            (mevedel--create-directive-in
             (current-buffer) (point-min) (point-max) nil "Test"))
           (record (mevedel--directive-record directive))
           (skill (mevedel-skill--create
                   :name "alpha" :source-file "/tmp/alpha/SKILL.md"
                   :user-invocable-p t :active-p t))
           (session (mevedel-session--create :name "main"
                                             :skills (list skill)))
           labels dispatched)
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (_prompt choices)
                   (setq labels choices)
                   '(?s "skills: none")))
                ((symbol-function 'mevedel--directive-skills-session)
                 (lambda (&rest _) session))
                ((symbol-function 'mevedel-skills--user-visible-skills)
                 (lambda (_) (list skill)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "alpha")))
        (mevedel--ov-actions-settings directive))
      (should (member '(?s "skills: none") labels))
      (should (member '(?b "back") labels))
      (should (equal '((:name "alpha" :source-file "/tmp/alpha/SKILL.md"))
                     (mevedel-directive-skills record)))
      (should (string-search
               "SKILLS: alpha"
               (substring-no-properties
                (overlay-get directive 'before-string))))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (&rest _) '(?b "back")))
                ((symbol-function 'mevedel--ov-actions-dispatch)
                 (lambda (&rest args) (setq dispatched args))))
        (mevedel--ov-actions-settings directive))
      (should dispatched))))

(mevedel-deftest mevedel-preview-directive-prompt ()
  ,test
  (test)
  :doc "previews the planning request when Plan before implementation is on"
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace
            (mevedel-workspace--create
             :type 'file :id "preview-plan" :root "/tmp"
             :name "preview-plan"))
           (mevedel--workspace workspace)
           (directive
            (mevedel--create-directive-in
             (current-buffer) (point-min) (point-max) nil "Test"))
           (record (mevedel--directive-record directive)))
      (setf (mevedel-directive-planning-enabled record) t)
      (goto-char (point-min))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "implement")))
              (mevedel-preview-directive-prompt))
            (with-current-buffer "*mevedel-directive-preview*"
              (should (string-search "Create a concrete implementation plan"
                                     (buffer-string)))
              (should (string-search "## TASK: Implement"
                                     (buffer-string)))))
        (when-let* ((buffer (get-buffer "*mevedel-directive-preview*")))
          (kill-buffer buffer)))))

  :doc "previews the read-only discussion request without a planning wrap"
  (with-temp-buffer
    (insert "directive")
    (let* ((workspace
            (mevedel-workspace--create
             :type 'file :id "preview-discuss" :root "/tmp"
             :name "preview-discuss"))
           (mevedel--workspace workspace)
           (directive
            (mevedel--create-directive-in
             (current-buffer) (point-min) (point-max) nil "Test"))
           (record (mevedel--directive-record directive)))
      (setf (mevedel-directive-planning-enabled record) t)
      (goto-char (point-min))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "discuss")))
              (mevedel-preview-directive-prompt))
            (with-current-buffer "*mevedel-directive-preview*"
              (should (string-search "## TASK: Answer the following request."
                                     (buffer-string)))
              (should-not
               (string-search "Create a concrete implementation plan"
                              (buffer-string)))))
        (when-let* ((buffer (get-buffer "*mevedel-directive-preview*")))
          (kill-buffer buffer))))))


;;
;;; Lookup

(mevedel-deftest mevedel-get-directive-patch ()
  ,test
  (test)
  :doc "reads patch history from the latest workspace-owned attempt"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "patch" :root "/tmp" :name "patch"))
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
                           (mevedel-get-directive-patch directive)))
            (setf (mevedel-directive-attempt-patch
                   (car (last (mevedel-directive-attempts record))))
                  "")
            (should-not (mevedel-get-directive-patch directive))))
      (kill-buffer buffer))))

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
      (mevedel-overlays-test--discard-directive cell)))

  :doc "stores nested directives as ordered details on the top-level record"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "nested" :root "/tmp" :name "nested"))
         (cell (mevedel-overlays-test--make-directive
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
      (mevedel-overlays-test--discard-directive cell)))

  :doc "detaches Ready and attempted directives after real full-region edits"
  (dolist (state '(nil implemented))
    (let* ((request (concat "A request " (make-string 140 ?x)))
           (workspace (mevedel-workspace--create
                       :type 'file :id (format "detach-%s" state)
                       :root "/tmp" :name "detach"))
           (cell (mevedel-overlays-test--make-directive
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
        (mevedel-overlays-test--discard-directive cell))))

  :doc "keeps partial edits attached through ordinary overlay resizing"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "detach-partial" :root "/tmp"
                     :name "detach-partial"))
         (cell (mevedel-overlays-test--make-directive
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
      (mevedel-overlays-test--discard-directive cell)))

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
         (cell (mevedel-overlays-test--make-reference
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
      (mevedel-overlays-test--discard-reference cell))))

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
         (cell (mevedel-overlays-test--make-directive
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
      (mevedel-overlays-test--discard-directive cell))))

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
                       :type 'file :id "filter-a" :root "/tmp" :name "filter-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'file :id "filter-b" :root "/tmp" :name "filter-b"))
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
                       :type 'file :id "find-a" :root "/tmp" :name "find-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'file :id "find-b" :root "/tmp" :name "find-b"))
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
                       :type 'file :id "id-a" :root "/tmp" :name "id-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'file :id "id-b" :root "/tmp" :name "id-b"))
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
                     :type 'file :id "restore" :root "/tmp" :name "restore"))
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
                       :type 'file :id "uuid-a" :root "/tmp" :name "uuid-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'file :id "uuid-b" :root "/tmp" :name "uuid-b"))
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
