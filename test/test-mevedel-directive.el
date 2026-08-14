;;; test-mevedel-directive.el --- Tests for mevedel-directive.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-directive)
(require 'mevedel-plan-mode)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-directive-request-changed-p
  (:doc "compares the authored request with the latest attempt snapshot")
  (let ((directive
         (mevedel-directive--create
          :id "directive" :request "Current" :anchor '(:state attached)
          :attempts
          (list
           (mevedel-directive-attempt--create
            :directive-request "Older")
           (mevedel-directive-attempt--create
            :directive-request "Current")))))
    (should-not (mevedel-directive-request-changed-p directive))
    (mevedel-directive-set-request directive "Edited")
    (should (mevedel-directive-request-changed-p directive))
    (should-not (mevedel-directive-state directive))))

(mevedel-deftest mevedel-directive-invalidate-plan
  ()
  ,test
  (test)
  :doc "demotes unstarted authority and dismisses its approval"
  (let* ((chat-buffer (generate-new-buffer " *directive-invalidation*"))
         (session
          (mevedel-session--create
           :pending-plan-approval '(:directive-id "directive")))
         (directive
          (mevedel-directive--create
           :id "directive"
           :plan (list :status 'proposed :chat-buffer chat-buffer)))
         aborted)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-plan-approval-abort)
                     (lambda (owner outcome)
                       (setq aborted (list owner outcome)))))
            (should (mevedel-directive-invalidate-plan directive)))
          (should (eq 'draft
                      (plist-get (mevedel-directive-plan directive) :status)))
          (should (plist-get (mevedel-directive-plan directive) :invalidated))
          (should (equal (list session 'invalidated) aborted)))
      (kill-buffer chat-buffer)))

  :doc "leaves started attempts immutable"
  (let ((directive
         (mevedel-directive--create :plan '(:status implementing))))
    (should-not (mevedel-directive-invalidate-plan directive))
    (should (eq 'implementing
                (plist-get (mevedel-directive-plan directive) :status))))

  :doc "does not dismiss another workflow's approval"
  (let* ((chat-buffer (generate-new-buffer " *other-approval*"))
         (session
          (mevedel-session--create
           :pending-plan-approval '(:directive-id "other")))
         (directive
          (mevedel-directive--create
           :id "edited"
           :plan (list :status 'proposed :chat-buffer chat-buffer)))
         aborted)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-plan-approval-abort)
                     (lambda (&rest _) (setq aborted t))))
            (mevedel-directive-invalidate-plan directive))
          (should-not aborted))
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-directive-actions
  (:doc "derives the state-correct directive action set")
  (let ((directive
         (mevedel-directive--create :request "Current")))
    (should (equal '(discuss implement)
                   (mevedel-directive-actions directive)))
    (setf (mevedel-directive-discussion directive)
          (list (mevedel-directive-discussion-turn--create
                 :directive-request "Current" :outcome 'success)))
    (should (equal '(continue-discussion implement-this)
                   (mevedel-directive-actions directive)))
    (setf (mevedel-directive-attempts directive)
          (list (mevedel-directive-attempt--create
                 :directive-request "Current" :outcome 'success)))
    (should (equal '(discuss-result request-changes)
                   (mevedel-directive-actions directive)))
    (setf (mevedel-directive-attempt-outcome
           (car (mevedel-directive-attempts directive)))
          'error)
    (should (equal '(discuss-result retry)
                   (mevedel-directive-actions directive)))
    (setf (mevedel-directive-state directive) 'implementing)
    (should (equal '(abort) (mevedel-directive-actions directive)))
    (setf (mevedel-directive-state directive) 'planning)
    (should (equal '(abort) (mevedel-directive-actions directive)))
    (mevedel-directive-set-request directive "Edited")
    (should (equal '(discuss implement)
                   (mevedel-directive-actions directive)))))

(mevedel-deftest mevedel-directive-recompute-state
  (:doc "derives lifecycle state from the latest surviving model activity")
  (let* ((success
         (mevedel-directive-attempt--create
           :sequence 1 :directive-request "Current" :outcome 'success
           :checkpoint '(:session-id "session" :turn 1)))
         (failure
         (mevedel-directive-attempt--create
           :sequence 3 :directive-request "Current" :outcome 'error
           :checkpoint '(:session-id "session" :turn 3)))
         (discussion
         (mevedel-directive-discussion-turn--create
           :sequence 2 :directive-request "Current" :outcome 'success
           :checkpoint '(:session-id "session" :turn 2)))
         (directive
          (mevedel-directive--create
           :id "directive" :request "Current" :anchor '(:state attached))))
    (should-not (mevedel-directive-recompute-state directive))
    (setf (mevedel-directive-attempts directive) (list success))
    (should (eq 'implemented
                (mevedel-directive-recompute-state directive)))
    (setf (mevedel-directive-discussion directive) (list discussion))
    (should (eq 'implemented
                (mevedel-directive-recompute-state directive)))
    (setf (mevedel-directive-attempts directive) (list success failure))
    (should (eq 'failed
                (mevedel-directive-recompute-state directive)))
    (setf (mevedel-directive-request directive) "Edited")
    (should-not (mevedel-directive-recompute-state directive))
    (setf (mevedel-directive-discussion directive)
          (append
           (mevedel-directive-discussion directive)
           (list
            (mevedel-directive-discussion-turn--create
             :sequence 4 :directive-request "Edited" :outcome 'success
             :checkpoint '(:session-id "session" :turn 4)))))
    (should (eq 'discussed
                (mevedel-directive-recompute-state directive)))))

(mevedel-deftest mevedel-directive-next-activity-sequence
  (:doc "allocates after the greatest surviving activity sequence")
  (let ((directive
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :attempts
          (list (mevedel-directive-attempt--create :sequence 2))
          :discussion
          (list (mevedel-directive-discussion-turn--create :sequence 5))
          :planning (list '(:sequence 7)))))
    (should (= 8 (mevedel-directive-next-activity-sequence directive)))))

(mevedel-deftest mevedel-workspace-rewind-directives
  ()
  ,test
  (test)
  :doc "prunes one execution-session suffix while retaining authored records"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "rewind" :root "/tmp" :name "rewind"))
         (earlier
          (mevedel-directive--create
           :id "earlier" :request "Earlier" :anchor '(:state attached)
           :attempts
           (list
            (mevedel-directive-attempt--create
             :sequence 1 :directive-request "Earlier" :outcome 'success
             :checkpoint '(:session-id "session" :turn 1))
            (mevedel-directive-attempt--create
             :sequence 3 :directive-request "Earlier" :outcome 'error
             :checkpoint '(:session-id "session" :turn 4)))
           :discussion
           (list
            (mevedel-directive-discussion-turn--create
             :sequence 2 :outcome 'success
             :checkpoint '(:session-id "session" :turn 2)))))
         (edited
          (mevedel-directive--create
           :id "edited" :request "Edited request" :anchor '(:state attached)
           :attempts
           (list
            (mevedel-directive-attempt--create
             :sequence 1 :directive-request "Original request" :outcome 'success
             :checkpoint '(:session-id "session" :turn 2)))
           :discussion
           (list
            (mevedel-directive-discussion-turn--create
             :sequence 2 :directive-request "Edited request" :outcome 'success
             :checkpoint '(:session-id "session" :turn 3)))))
         (later
          (mevedel-directive--create
           :id "later" :request "Later" :anchor '(:state source-missing)
           :attempts
           (list
            (mevedel-directive-attempt--create
             :directive-request "Later" :outcome 'aborted
             :checkpoint '(:session-id "session" :turn 5))))))
    (mevedel-workspace-set-directives workspace (list earlier edited later))
    (mevedel-workspace-rewind-directives workspace "session" 4)
    (should (equal (list earlier edited later)
                   (mevedel-workspace-directives workspace)))
    (should (= 1 (length (mevedel-directive-attempts earlier))))
    (should (eq 'implemented (mevedel-directive-state earlier)))
    (should (= 1 (length (mevedel-directive-attempts edited))))
    (should (eq 'discussed (mevedel-directive-state edited)))
    (should-not (mevedel-directive-attempts later))
    (should-not (mevedel-directive-state later)))

  :doc "restores consumed children while retaining later authored children"
  (let* ((consumed
          (mevedel-subdirective--create
           :id "consumed" :request "Original detail"
           :anchor '(:state attached :file "/tmp/source" :start 3 :end 7)))
         (later
          (mevedel-subdirective--create
           :id "later" :request "Later correction"
           :anchor '(:state attached :file "/tmp/source" :start 9 :end 12)))
         (directive
          (mevedel-directive--create
           :id "parent" :request "Parent" :anchor '(:state attached)
           :state 'implemented :subdirectives (list later)
           :attempts
           (list
            (mevedel-directive-attempt--create
             :directive-request "Parent" :outcome 'success
             :consumed-subdirectives (list consumed)
             :checkpoint '(:session-id "session" :turn 2)))))
         (workspace
          (mevedel-workspace--create
           :type 'file :id "children" :root "/tmp" :name "children"
           :directives (list directive))))
    (mevedel-workspace-rewind-directives workspace "session" 2)
    (should-not (mevedel-directive-attempts directive))
    (should-not (mevedel-directive-state directive))
    (should
     (equal '("consumed" "later")
            (mapcar #'mevedel-subdirective-id
                    (mevedel-directive-subdirectives directive)))))

  :doc "restores accepted-plan authority before a rewound planned attempt"
  (let* ((directive
          (mevedel-directive--create
           :id "planned" :request "Plan it" :anchor '(:state attached)
           :plan '(:status settled :action implement :proposal "# Plan")
           :planning
           (list '(:sequence 1 :checkpoint (:session-id "session" :turn 1))
                 '(:sequence 3 :checkpoint (:session-id "session" :turn 3)))
           :attempts
           (list
            (mevedel-directive-attempt--create
             :sequence 2 :action 'implement :directive-request "Plan it"
             :request "Exact accepted implementation prompt"
             :outcome 'success :plan "# Plan"
             :plan-context '(:request "Plan it" :subdirectives nil)
             :plan-selection '(:mode edits :model-provider "Test:model")
             :checkpoint '(:session-id "session" :turn 2)))))
         (workspace
          (mevedel-workspace--create
           :type 'file :id "planned" :root "/tmp" :name "planned"
           :directives (list directive))))
    (mevedel-workspace-rewind-directives workspace "session" 2)
    (should-not (mevedel-directive-attempts directive))
    (should (= 1 (length (mevedel-directive-planning directive))))
    (should (eq 'accepted
                (plist-get (mevedel-directive-plan directive) :status)))
    (should (equal "Exact accepted implementation prompt"
                   (plist-get (mevedel-directive-plan directive)
                              :accepted-prompt))))

  :doc "restores the preceding proposal when rewinding planning turns"
  (let* ((directive
          (mevedel-directive--create
           :id "planning" :request "Plan it" :anchor '(:state attached)
           :plan '(:status proposed :action implement
                   :implementation-prompt "Implement current"
                   :proposal "# Current" :selection (:mode ask))
           :planning
           (list '(:sequence 1 :action implement
                   :implementation-prompt "Implement prior"
                   :proposal "# Prior"
                   :checkpoint (:session-id "session" :turn 1))
                 '(:sequence 2 :action implement
                   :implementation-prompt "Implement current"
                   :proposal "# Current"
                   :checkpoint (:session-id "session" :turn 2)))))
         (workspace
          (mevedel-workspace--create
           :type 'file :id "planning" :root "/tmp" :name "planning"
           :directives (list directive))))
    (mevedel-workspace-rewind-directives workspace "session" 2)
    (should (= 1 (length (mevedel-directive-planning directive))))
    (should (equal '(:status proposed :action implement
                     :implementation-prompt "Implement prior"
                     :proposal "# Prior" :selection (:mode ask))
                   (mevedel-directive-plan directive))))

  :doc "does not restore accepted authority after authored context changes"
  (let* ((directive
          (mevedel-directive--create
           :id "stale" :request "Edited" :anchor '(:state attached)
           :plan '(:status settled :action implement :proposal "# Plan")
           :attempts
           (list
            (mevedel-directive-attempt--create
             :sequence 1 :action 'implement :directive-request "Original"
             :request "Accepted prompt" :outcome 'success :plan "# Plan"
             :plan-context '(:request "Original" :subdirectives nil)
             :checkpoint '(:session-id "session" :turn 1)))))
         (workspace
          (mevedel-workspace--create
           :type 'file :id "stale" :root "/tmp" :name "stale"
           :directives (list directive))))
    (mevedel-workspace-rewind-directives workspace "session" 1)
    (should-not (mevedel-directive-plan directive))))

(provide 'test-mevedel-directive)
;;; test-mevedel-directive.el ends here
