;;; test-mevedel-overlay-ui.el -- Instruction overlay UI tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for instruction labels, actions, styling, and redraw.

;;; Code:

(require 'mevedel-overlay-ui)
(require 'mevedel-directive-source)
(require 'mevedel-overlays)
(require 'mevedel-instruction-registry)
(require 'mevedel-chat)
(require 'mevedel-menu)
(require 'mevedel-models)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'mevedel-directive)
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

(mevedel-deftest mevedel-overlay-ui-directive-action-label ()
  ,test
  (test)
  :doc "labels known actions and normalizes an unknown action"
  (should (equal "Request changes"
                 (mevedel-overlay-ui-directive-action-label
                  'request-changes)))
  (should (equal "Custom Action"
                 (mevedel-overlay-ui-directive-action-label
                  "custom_action"))))

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

(provide 'test-mevedel-overlay-ui)
;;; test-mevedel-overlay-ui.el ends here
