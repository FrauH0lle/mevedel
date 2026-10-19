;;; test-mevedel-collaboration-goal.el --- Goal projection tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only Goal facts, publication, and removal.

;;; Code:
(require 'helpers (file-name-concat
                  (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-goal)
(require 'mevedel-structs)

(mevedel-deftest mevedel-collaboration--goal-frame
  ()
  ,test
  (test)
  :doc "projects durable accounting without inventing complete usage"
  (let* ((goal (mevedel-goal--create :id "goal-1" :objective "Finish the review"
                                   :status 'budget-limited :reason "Usage incomplete"
                                   :turns-run 3 :token-budget 1000 :tokens-used 400
                                   :tokens-incomplete-p t :time-used-seconds 25))
         (session (mevedel-session--create :goal goal))
         (frame (mevedel-collaboration--goal-frame (list :session session)))
         (facts (plist-get frame :goal)))
    (should (equal "goal" (plist-get frame :t)))
    (should (equal "Finish the review" (plist-get facts :objective)))
    (should (equal "budget-limited" (plist-get facts :status)))
    (should (= 3 (plist-get facts :turns)))
    (should (= 1000 (plist-get facts :tokenBudget)))
    (should (= 400 (plist-get facts :tokensUsed)))
    (should (eq t (plist-get facts :incomplete)))
    (should (= 25 (plist-get facts :seconds)))
    (setf (mevedel-goal-token-budget goal) nil)
    (should-not
     (alist-get 'tokenBudget
                (alist-get 'goal (json-read-from-string
                                 (json-encode (mevedel-collaboration--goal-frame
                                               (list :session session)))))))
    (setf (mevedel-session-goal session) nil)
    (should-not (alist-get 'goal (json-read-from-string
                                 (json-encode (mevedel-collaboration--goal-frame
                                               (list :session session)))))))

  :doc "bounds arbitrary objective and reason text without hiding truncation"
  (let* ((goal (mevedel-goal--create :id "goal-1" :status 'paused
                                   :objective (make-string 100000 ?\n)
                                   :reason (make-string 100000 ?\n)))
         (frame (mevedel-collaboration--goal-frame
                 (list :session (mevedel-session--create :goal goal)))))
    (should (< (string-bytes (json-encode frame)) (* 64 1024)))
    (should (eq t (plist-get (plist-get frame :goal) :truncated)))))

(mevedel-deftest mevedel-collaboration--publish-goal
  (:doc "sends changes and clearing to every ready tier, not unchanged facts")
  (let* ((guests (make-hash-table :test #'eql))
         (session (mevedel-session--create :goal
                   (mevedel-goal--create :id "goal-1" :objective "Review" :status 'paused
                                        :turns-run 0 :tokens-used 0 :time-used-seconds 0)))
         (room (list :session session :guests guests :transport 'transport)) sent)
    (puthash 1 (list :ready t :writable nil) guests)
    (puthash 2 (list :ready t :writable t) guests)
    (puthash 3 (list :ready t :owner t) guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame) (push (cons peer frame) sent))))
      (mevedel-collaboration--publish-goal room)
      (should (= 1 (length sent)))
      (should (= 0 (caar sent))) ; Relay broadcast, not one privileged peer.
      (setq sent nil)
      (mevedel-collaboration--publish-goal room)
      (should-not sent)
      (setf (mevedel-session-goal session) nil)
      (mevedel-collaboration--publish-goal room)
      (should (= 1 (length sent)))
      (should (= 0 (caar sent)))
      (should-not (alist-get 'goal (json-read-from-string (json-encode (cdar sent))))))))

(provide 'test-mevedel-collaboration-goal)
;;; test-mevedel-collaboration-goal.el ends here
