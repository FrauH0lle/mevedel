;;; test-niwa-collaboration-goal.el --- Goal projection tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only Goal facts, publication, and removal.

;;; Code:
(require 'helpers (file-name-concat
                  (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-collaboration)
(require 'niwa-collaboration-goal)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-task)
(require 'niwa-collaboration-transport)
(require 'niwa-goal)
(require 'niwa-session-artifacts)
(require 'niwa-session-codec)
(require 'niwa-structs)
(require 'niwa-tool-goal)

(niwa-deftest niwa-collaboration--goal-frame
  ()
  ,test
  (test)
  :doc "projects durable accounting without inventing complete usage"
  (let* ((goal (niwa-goal--create :id "goal-1" :objective "Finish the review"
                                   :status 'budget-limited :reason "Usage incomplete"
                                   :turns-run 3 :token-budget 1000 :tokens-used 400
                                   :tokens-incomplete-p t :time-used-seconds 25))
         (session (niwa-session--create :goal goal))
         (frame (niwa-collaboration--goal-frame (list :session session)))
         (facts (plist-get frame :goal)))
    (should (equal "goal" (plist-get frame :t)))
    (should (equal "Finish the review" (plist-get facts :objective)))
    (should (equal "budget-limited" (plist-get facts :status)))
    (should (= 3 (plist-get facts :turns)))
    (should (= 1000 (plist-get facts :tokenBudget)))
    (should (= 400 (plist-get facts :tokensUsed)))
    (should (eq t (plist-get facts :incomplete)))
    (should (= 25 (plist-get facts :seconds)))
    (setf (niwa-goal-token-budget goal) nil)
    (should-not
     (alist-get 'tokenBudget
                (alist-get 'goal (json-read-from-string
                                 (json-encode (niwa-collaboration--goal-frame
                                               (list :session session)))))))
    (setf (niwa-session-goal session) nil)
    (should-not (alist-get 'goal (json-read-from-string
                                 (json-encode (niwa-collaboration--goal-frame
                                               (list :session session)))))))

  :doc "bounds arbitrary objective and reason text without hiding truncation"
  (let* ((goal (niwa-goal--create :id "goal-1" :status 'paused
                                   :objective (make-string 100000 ?\n)
                                   :reason (make-string 100000 ?\n)))
         (frame (niwa-collaboration--goal-frame
                 (list :session (niwa-session--create :goal goal)))))
    (should (< (string-bytes (json-encode frame)) (* 64 1024)))
    (should (eq t (plist-get (plist-get frame :goal) :truncated)))))

(niwa-deftest niwa-collaboration--publish-goal
  (:doc "sends changes and clearing to every ready tier, not unchanged facts")
  (let* ((guests (make-hash-table :test #'eql))
         (session (niwa-session--create :goal
                   (niwa-goal--create :id "goal-1" :objective "Review" :status 'paused
                                        :turns-run 0 :tokens-used 0 :time-used-seconds 0)))
         (room (list :session session :guests guests :transport 'transport)) sent)
    (puthash 1 (list :ready t :writable nil) guests)
    (puthash 2 (list :ready t :writable t) guests)
    (puthash 3 (list :ready t :owner t) guests)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport peer frame) (push (cons peer frame) sent))))
      (niwa-collaboration--publish-goal room)
      (should (= 1 (length sent)))
      (should (= 0 (caar sent))) ; Relay broadcast, not one privileged peer.
      (setq sent nil)
      (niwa-collaboration--publish-goal room)
      (should-not sent)
      (setf (niwa-session-goal session) nil)
      (niwa-collaboration--publish-goal room)
      (should (= 1 (length sent)))
      (should (= 0 (caar sent)))
      (should-not (alist-get 'goal (json-read-from-string (json-encode (cdar sent))))))))

(defun niwa-test--goal-failure-reasons (failure)
  "Check that FAILURE never publishes host diagnostics to any link tier."
    (with-temp-buffer
      (let* ((diagnostic (if (eq failure 'settlement)
                             "Authentication failed at /private/provider/socket: secret-token"
                           "Install runtime at /private/provider/socket: secret-token"))
             (public (if (eq failure 'settlement)
                         "Provider sign-in is required"
                       "The provider runtime needs attention"))
             (goal (niwa-goal--create :id "g" :objective "Review" :status 'active
                                        :turns-run 0 :tokens-used 0 :time-used-seconds 0
                                        :created-at "now" :updated-at "now"))
             (session (niwa-session--create :name "main" :goal goal))
             (guests (make-hash-table :test #'eql))
             (token (niwa-collaboration--random-bytes 16))
             (owner-token (niwa-collaboration--random-bytes 16))
             (room (list :session session :transport 'transport :guests guests
                         :write-token token :owner-token owner-token :records nil
                         :ui-requests (make-hash-table :test #'eql)))
             sent)
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                  ((symbol-function 'niwa-collaboration--store-frame)
                   (lambda (_room) '(:t "store-artifacts" :artifacts [])))
                  ((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame) (push (cons peer frame) sent) t)))
          (if (eq failure 'settlement)
              (niwa-goal-settle-failure
               (gptel-make-fsm :info (list :buffer (current-buffer)
                                         :niwa-goal-id "g" :niwa-goal-accounting-id "g"
                                         :tokens-full '(:input 1 :output 1)
                                         :error diagnostic)) 'error)
            (niwa-goal-pause-runtime-failure (current-buffer) diagnostic))
          (should (eq 'paused (niwa-goal-status goal)))
          ;; Retain the real diagnosis for the native host; project only safe text.
          (should (equal diagnostic (niwa-goal-reason goal)))
          (should (niwa-goal-reason-diagnostic goal))
          ;; Provenance must survive persistence before any guest can rejoin.
          (setf (niwa-session-goal session)
                (niwa-session-codec--goal-from-plist
                 (niwa-session-codec--goal-to-plist goal)))
          (dolist (peer '(1 2 3))
            (niwa-collaboration--handle-hello
             room peer (list :proto niwa-collaboration--protocol-version :name "Reader"
                             :writeToken (and (> peer 1)
                                              (niwa-collaboration--base64url token))
                             :ownerToken (and (= peer 3)
                                              (niwa-collaboration--base64url owner-token)))))
          (should-not (plist-get (gethash 1 guests) :writable))
          (should (plist-get (gethash 2 guests) :writable))
          (should (plist-get (gethash 3 guests) :owner))
          (niwa-collaboration--publish-goal room)
          (let ((frames (cl-remove-if-not
                         (lambda (entry) (equal "goal" (plist-get (cdr entry) :t))) sent)))
            (should (equal '(0 1 2 3) (sort (mapcar #'car frames) #'<)))
            (dolist (entry frames)
              ;; Assert the consumer-visible JSON, for both broadcast and joins.
              (let* ((json (json-encode (cdr entry)))
                     (facts (alist-get 'goal (json-read-from-string json))))
                (should-not (string-match-p "private/provider\\|secret-token" json))
                (should (equal public (alist-get 'reason facts))))))))))

(ert-deftest niwa-collaboration-goal-settlement-reason/all-tiers ()
  (niwa-test--goal-failure-reasons 'settlement))

(ert-deftest niwa-collaboration-goal-runtime-reason/all-tiers ()
  (niwa-test--goal-failure-reasons 'runtime))

(ert-deftest niwa-collaboration-goal-public-reasons ()
  "Public content remains visible even when it resembles failure text."
  (with-temp-buffer
    (let* ((goal (niwa-goal--create :id "g" :objective "Review" :status 'active
                                       :reason-diagnostic t))
           (session (niwa-session--create :goal goal))
           (room (list :session session))
           (summary "Blocked: authentication tests need a fixture in /private/provider"))
      (setq-local niwa--session session)
      (niwa-tool-goal-update 'blocked summary session "g")
      (should-not (niwa-goal-reason-diagnostic goal))
      (should (equal summary (plist-get (plist-get (niwa-collaboration--goal-frame room)
                                                 :goal) :reason)))
      (setf (niwa-goal-status goal) 'active
            (niwa-goal-reason-diagnostic goal) t)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore))
        (niwa-goal-pause))
      (should-not (niwa-goal-reason-diagnostic goal))
      (should (equal "paused by user"
                     (plist-get (plist-get (niwa-collaboration--goal-frame room)
                                           :goal) :reason))))))

(provide 'test-niwa-collaboration-goal)
;;; test-niwa-collaboration-goal.el ends here
