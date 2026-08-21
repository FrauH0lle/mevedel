;;; test-mevedel-directive-persistence.el -- Directive codec tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for the workspace directive record codec.

;;; Code:

(require 'mevedel-directive-persistence)
(require 'mevedel-structs)
(require 'mevedel-utilities)
(require 'mevedel-directive)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-directive-persistence-test--attempt ()
  "Return a complete attempt for directive codec tests."
  (mevedel-directive-attempt--create
   :sequence 1 :action 'retry :directive-request "Preserve this"
   :request "prompt" :result "result" :outcome 'success
   :patch "patch" :capture 'complete :covered-files nil :gaps nil
   :untracked-effects nil
   :captured-at "2026-08-02T00:00:00+0200"
   :checkpoint '(:session-id "session" :turn 1)
   :plan "# Accepted plan"
   :plan-context '(:request "Preserve this" :subdirectives nil)
   :plan-selection '(:mode edits :model-provider "Test:model")))

(mevedel-deftest mevedel--deserialize-directives
  (:doc "`mevedel--deserialize-directives' validates and restores records"
   :vars ())
  ,test
  (test)
  :doc "round trips source-missing and archived records without source buffers"
  (let* ((directory (make-temp-file "mevedel-source-persist-" t))
         (workspace (mevedel-workspace--create
                     :type 'file :id directory :root directory
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
                    :id "archived" :request "Preserve this"
                    :anchor (list :state 'archived
                                  :file (file-name-concat directory "old.el")
                                  :start 2 :end 2
                                  :evidence '(:schema 1 :bodyless t)
                                  :properties
                                  '(mevedel-instruction t
                                    mevedel-uuid "archived"
                                    mevedel-instruction-type directive))
                    :state 'failed :session-id nil
                    :planning-enabled t
                    :skills '((:name "alpha"
                               :source-file "/tmp/alpha/SKILL.md"))
                    :plan '(:status proposed :action retry
                            :cancelled nil
                            :invalidated nil
                            :implementation-prompt "Retry prompt"
                            :accepted-prompt nil
                            :proposal "# Replacement plan"
                            :selection (:mode edits
                                        :model-provider "Test:model"))
                    :planning
                    (list
                     '(:sequence 3 :action retry
                       :directive-request "Preserve this"
                       :message "Cover recovery"
                       :implementation-prompt "Retry prompt"
                       :proposal "# Replacement plan"
                       :request "planning prompt"
                       :result "# Replacement plan" :outcome success
                       :checkpoint (:session-id "session" :turn 3)))
                    :attempts (list (mevedel-directive-persistence-test--attempt))
                    :discussion
                    (list
                     (mevedel-directive-discussion-turn--create
                      :sequence 2 :directive-request "Preserve this"
                      :message "Why?" :request "discussion prompt"
                      :result "Because" :outcome 'success :attempt-index 1
                      :checkpoint '(:session-id "session" :turn 2))))))
    (unwind-protect
        (progn
          (mevedel-workspace-set-directives workspace (list missing archived))
          (let* ((serialized (mevedel--serialize-directives workspace directory))
                 (restored (mevedel--deserialize-directives serialized directory)))
            (should-not (plist-member (cadr serialized) :state))
            (should (equal '(source-missing archived)
                           (mapcar (lambda (record)
                                     (plist-get (mevedel-directive-anchor record)
                                                :state))
                                   restored)))
            (should (eq 'retry
                        (mevedel-directive-attempt-action
                         (car (mevedel-directive-attempts
                               (cadr restored))))))
            (should (mevedel-directive-planning-enabled (cadr restored)))
            (should (equal '((:name "alpha"
                              :source-file "/tmp/alpha/SKILL.md"))
                           (mevedel-directive-skills (cadr restored))))
            (should (equal "# Replacement plan"
                           (plist-get (mevedel-directive-plan (cadr restored))
                                      :proposal)))
            (should (equal "Cover recovery"
                           (plist-get
                            (car (mevedel-directive-planning (cadr restored)))
                            :message)))
            (should (equal "# Accepted plan"
                           (mevedel-directive-attempt-plan
                            (car (mevedel-directive-attempts
                                  (cadr restored))))))
            (should
             (equal "Because"
                    (mevedel-directive-discussion-turn-result
                     (car (mevedel-directive-discussion
                           (cadr restored))))))
            (should (eq 'implemented
                        (mevedel-directive-state (cadr restored))))))
      (delete-directory directory t)))

  :doc "validates the source region a top-level attached anchor stores"
  (let* ((directory (make-temp-file "mevedel-attached-anchor-" t))
         (workspace (mevedel-workspace--create
                     :type 'file :id directory :root directory
                     :name "persist"))
         (file (file-name-concat directory "a.el"))
         (directive (mevedel-directive--create
                     :id "attached" :request "Explain this"
                     :anchor (list :state 'attached :file file
                                   :start 1 :end 7
                                   :evidence '(:schema 1 :bodyless nil
                                               :text "target")))))
    (unwind-protect
        (progn
          (mevedel-workspace-set-directives workspace (list directive))
          (let ((serialized
                 (mevedel--serialize-directives workspace directory)))
            (should (= 1 (length (mevedel--deserialize-directives
                                  serialized directory))))
            (dolist (anchor
                     '((:state attached :start 1 :end 7 :evidence nil)
                       (:state attached :file 7 :start 1 :end 7
                               :evidence nil)
                       (:state attached :file "a.el" :start 7 :end 1
                               :evidence nil)
                       (:state attached :file "a.el" :start 1 :end 7
                               :evidence "target")))
              (let ((input (copy-tree serialized)))
                (plist-put (car input) :anchor (copy-sequence anchor))
                (should-error
                 (mevedel--deserialize-directives input directory)
                 :type 'user-error)))))
      (delete-directory directory t)))

  :doc "rejects one activity sequence shared across two collections"
  ;; A sequence is allocated as a max+1 over all three collections, and the
  ;; inspector resolves a rendered row back to a durable entry with it.
  (let* ((directory (make-temp-file "mevedel-activity-sequence-" t))
         (workspace (mevedel-workspace--create
                     :type 'file :id directory :root directory
                     :name "persist"))
         (directive
          (mevedel-directive--create
           :id "shared" :request "Preserve this"
           :anchor (list :state 'archived
                         :file (file-name-concat directory "old.el")
                         :start 1 :end 2 :evidence nil :properties nil)
           :attempts (list (mevedel-directive-persistence-test--attempt))
           :discussion
           (list (mevedel-directive-discussion-turn--create
                  :sequence 1 :directive-request "Preserve this"
                  :message "Why?" :request "discussion prompt"
                  :result "Because" :outcome 'success
                  :checkpoint '(:session-id "session" :turn 1))))))
    (unwind-protect
        (progn
          (mevedel-workspace-set-directives workspace (list directive))
          (should-error
           (mevedel--deserialize-directives
            (mevedel--serialize-directives workspace directory)
            directory)
           :type 'user-error))
      (delete-directory directory t)))

  :doc "rejects top-level and nested current directive ID collisions"
  (let* ((directory (make-temp-file "mevedel-directive-ids-" t))
         (workspace (mevedel-workspace--create
                     :type 'file :id directory :root directory
                     :name "directive-ids"))
         (child-a
          (mevedel-subdirective--create
           :id "child-a" :request "Child A"
           :anchor (list :state 'attached
                         :file (file-name-concat directory "a.el")
                         :start 1 :end 2 :evidence nil :properties nil)))
         (child-b
          (mevedel-subdirective--create
           :id "child-b" :request "Child B"
           :anchor (list :state 'attached
                         :file (file-name-concat directory "b.el")
                         :start 1 :end 2 :evidence nil :properties nil)))
         (attempt
          (let ((attempt (mevedel-directive-persistence-test--attempt)))
            (setf (mevedel-directive-attempt-consumed-subdirectives attempt)
                  (list (mevedel-subdirective-copy child-a)))
            attempt))
         (parent-a
          (mevedel-directive--create
           :id "parent-a" :request "Parent A"
           :anchor (list :state 'source-missing
                         :file (file-name-concat directory "a.el")
                         :start 1 :end 2 :evidence nil :properties nil)
           :subdirectives (list child-a)
           :attempts (list attempt)))
         (parent-b
          (mevedel-directive--create
           :id "parent-b" :request "Parent B"
           :anchor (list :state 'source-missing
                         :file (file-name-concat directory "b.el")
                         :start 1 :end 2 :evidence nil :properties nil)
           :subdirectives (list child-b))))
    (unwind-protect
        (progn
          (mevedel-workspace-set-directives workspace
                                            (list parent-a parent-b))
          (let ((serialized
                 (mevedel--serialize-directives workspace directory)))
            (should (= 2 (length
                          (mevedel--deserialize-directives
                           serialized directory))))
            (dolist (collision '(top-level nested cross-level))
              (let ((input (copy-tree serialized)))
                (pcase collision
                  ('top-level
                   (plist-put (cadr input) :id "parent-a"))
                  ('nested
                   (plist-put
                    (car (plist-get (cadr input) :subdirectives))
                    :id "child-a"))
                  ('cross-level
                   (plist-put
                    (car (plist-get (car input) :subdirectives))
                    :id "parent-b")))
                (should-error
                 (mevedel--deserialize-directives input directory)
                 :type 'user-error)))))
      (delete-directory directory t))))


(provide 'test-mevedel-directive-persistence)
;;; test-mevedel-directive-persistence.el ends here
