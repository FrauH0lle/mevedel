;;; test-mevedel-directive-persistence.el -- Directive codec tests -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for the workspace directive record codec.

;;; Code:

(require 'mevedel-directive-persistence)
(require 'mevedel-structs)
(require 'mevedel-utilities)
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
      (delete-directory directory t))))


(provide 'test-mevedel-directive-persistence)
;;; test-mevedel-directive-persistence.el ends here
