;;; test-mevedel-collaboration-goal.el --- Goal projection tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only Goal facts, publication, and removal.

;;; Code:
(require 'helpers (file-name-concat
                  (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-goal)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-task)
(require 'mevedel-collaboration-transport)
(require 'mevedel-goal)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-codec)
(require 'mevedel-structs)
(require 'mevedel-tool-goal)

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

(defun mevedel-test--goal-failure-reasons (failure)
  "Check that FAILURE never publishes host diagnostics to any link tier."
    (with-temp-buffer
      (let* ((diagnostic (if (eq failure 'settlement)
                             "Authentication failed at /private/provider/socket: secret-token"
                           "Install runtime at /private/provider/socket: secret-token"))
             (public (if (eq failure 'settlement)
                         "Provider sign-in is required"
                       "The provider runtime needs attention"))
             (goal (mevedel-goal--create :id "g" :objective "Review" :status 'active
                                        :turns-run 0 :tokens-used 0 :time-used-seconds 0
                                        :created-at "now" :updated-at "now"))
             (session (mevedel-session--create :name "main" :goal goal))
             (guests (make-hash-table :test #'eql))
             (token (mevedel-collaboration--random-bytes 16))
             (owner-token (mevedel-collaboration--random-bytes 16))
             (room (list :session session :transport 'transport :guests guests
                         :write-token token :owner-token owner-token :records nil
                         :ui-requests (make-hash-table :test #'eql)))
             sent)
        (setq-local mevedel--session session)
        (cl-letf (((symbol-function 'mevedel-session-artifacts-save) #'ignore)
                  ((symbol-function 'mevedel-collaboration--store-frame)
                   (lambda (_room) '(:t "store-artifacts" :artifacts [])))
                  ((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame) (push (cons peer frame) sent) t)))
          (if (eq failure 'settlement)
              (mevedel-goal-settle-failure
               (gptel-make-fsm :info (list :buffer (current-buffer)
                                         :mevedel-goal-id "g" :mevedel-goal-accounting-id "g"
                                         :tokens-full '(:input 1 :output 1)
                                         :error diagnostic)) 'error)
            (mevedel-goal-pause-runtime-failure (current-buffer) diagnostic))
          (should (eq 'paused (mevedel-goal-status goal)))
          ;; Retain the real diagnosis for the native host; project only safe text.
          (should (equal diagnostic (mevedel-goal-reason goal)))
          (should (mevedel-goal-reason-diagnostic goal))
          ;; Provenance must survive persistence before any guest can rejoin.
          (setf (mevedel-session-goal session)
                (mevedel-session-codec--goal-from-plist
                 (mevedel-session-codec--goal-to-plist goal)))
          (dolist (peer '(1 2 3))
            (mevedel-collaboration--handle-hello
             room peer (list :proto mevedel-collaboration--protocol-version :name "Reader"
                             :writeToken (and (> peer 1)
                                              (mevedel-collaboration--base64url token))
                             :ownerToken (and (= peer 3)
                                              (mevedel-collaboration--base64url owner-token)))))
          (should-not (plist-get (gethash 1 guests) :writable))
          (should (plist-get (gethash 2 guests) :writable))
          (should (plist-get (gethash 3 guests) :owner))
          (mevedel-collaboration--publish-goal room)
          (let ((frames (cl-remove-if-not
                         (lambda (entry) (equal "goal" (plist-get (cdr entry) :t))) sent)))
            (should (equal '(0 1 2 3) (sort (mapcar #'car frames) #'<)))
            (dolist (entry frames)
              ;; Assert the consumer-visible JSON, for both broadcast and joins.
              (let* ((json (json-encode (cdr entry)))
                     (facts (alist-get 'goal (json-read-from-string json))))
                (should-not (string-match-p "private/provider\\|secret-token" json))
                (should (equal public (alist-get 'reason facts))))))))))

(ert-deftest mevedel-collaboration-goal-settlement-reason/all-tiers ()
  (mevedel-test--goal-failure-reasons 'settlement))

(ert-deftest mevedel-collaboration-goal-runtime-reason/all-tiers ()
  (mevedel-test--goal-failure-reasons 'runtime))

(ert-deftest mevedel-collaboration-goal-public-reasons ()
  "Public content remains visible even when it resembles failure text."
  (with-temp-buffer
    (let* ((goal (mevedel-goal--create :id "g" :objective "Review" :status 'active
                                       :reason-diagnostic t))
           (session (mevedel-session--create :goal goal))
           (room (list :session session))
           (summary "Blocked: authentication tests need a fixture in /private/provider"))
      (setq-local mevedel--session session)
      (mevedel-tool-goal-update 'blocked summary session "g")
      (should-not (mevedel-goal-reason-diagnostic goal))
      (should (equal summary (plist-get (plist-get (mevedel-collaboration--goal-frame room)
                                                 :goal) :reason)))
      (setf (mevedel-goal-status goal) 'active
            (mevedel-goal-reason-diagnostic goal) t)
      (cl-letf (((symbol-function 'mevedel-session-artifacts-save) #'ignore))
        (mevedel-goal-pause))
      (should-not (mevedel-goal-reason-diagnostic goal))
      (should (equal "paused by user"
                     (plist-get (plist-get (mevedel-collaboration--goal-frame room)
                                           :goal) :reason))))))

(provide 'test-mevedel-collaboration-goal)
;;; test-mevedel-collaboration-goal.el ends here
