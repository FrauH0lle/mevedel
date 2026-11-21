;;; test-niwa-tool-notify.el --- Deliberate root notices -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise Notify through the tool pipeline and its transcript projection.

;;; Code:

(require 'niwa)
(require 'niwa-notify)
(require 'niwa-tools)
(require 'niwa-pipeline)
(require 'niwa-presets)
(require 'niwa-schedule)
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-lobby)
(require 'niwa-collaboration-transport)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro niwa-tool-notify-test--with-request (&rest body)
  "Run BODY with a registered tool and an admitted root conversation."
  (declare (indent 0))
  `(with-temp-buffer
     (let* ((session (niwa-skills-test--make-session "notice" default-directory))
            (fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                              :position (point-marker)
                                              :callback #'gptel--insert-response)))
            notices
            (niwa-notify-function (lambda (kind details)
                                       (push (list kind (plist-get details :text)) notices))))
       (setq-local niwa--session session
                   niwa--current-request (niwa-request--create :session session :fsm fsm :origin "/root"))
       (setf (niwa-session-save-path session) default-directory
             (niwa-session-root-buffer session) (current-buffer))
       (niwa-tools-register)
       (cl-flet ((call (text)
                   (let (answer)
                     (niwa-pipeline-run-tool-outcome
                      (niwa-tool-for-call "Notify") (lambda (value) (setq answer value))
                      (list :text text) (list :engine fsm :projection 'provider))
                     (niwa-test--await 3 "Notify did not answer" answer)
                     answer)))
         ,@body))))

(niwa-deftest niwa-tool-notify/delivery ()
  ,test
  (test)
  :doc "records the delivered line in the ordinary tool transcript"
  (niwa-tool-notify-test--with-request
    (let ((niwa-collaboration--rooms (make-hash-table :test #'eq))
          (subscriptions (make-hash-table :test #'equal))
          (gptel-include-tool-results t) controls)
      (puthash "phone" '(:writable t) subscriptions)
      (puthash (current-buffer) (list :data-buffer (current-buffer) :session session
                                      :push-guests subscriptions :guests (make-hash-table)
                                      :transport 'relay :status nil) niwa-collaboration--rooms)
      (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
                 (lambda (_transport frame) (push frame controls))))
        (let ((answer (call "CI is red")))
          (should (equal notices '((notify "CI is red"))))
          (gptel--display-tool-results
           (list (list (gptel-get-tool '("niwa" "Notify")) '(:text "CI is red")
                       (plist-get answer :result))) (gptel-fsm-info fsm)))
        (should (string-match-p "^Notified: CI is red$" (buffer-string)))
        (should (equal controls '((:t "push" :guestIds ["phone"]))))
        (niwa-request-end)
        (should (equal controls '((:t "push") (:t "push" :guestIds ["phone"]))))))))

(niwa-deftest niwa-tool-notify/lobby ()
  ,test
  (test)
  :doc "one browser subscribed through two distinct Push endpoints wakes once"
  (niwa-tool-notify-test--with-request
    (let* ((niwa-collaboration--rooms (make-hash-table :test #'eq))
           (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
           (lobby-subs (make-hash-table :test #'equal))
           (room-subs (make-hash-table :test #'equal))
           (lobby-guests (make-hash-table)) controls frames room-down)
      (puthash "phone" '(:endpoint "https://push/lobby" :writable t) lobby-subs)
      (puthash "tablet" '(:endpoint "https://push/tablet" :writable t) lobby-subs)
      (puthash 1 '(:ready t :guest-id "tablet" :writable t) lobby-guests)
      (puthash 2 '(:ready t :guest-id "reader") lobby-guests)
      (puthash default-directory (list :push-guests lobby-subs :transport 'lobby
                                       :guests lobby-guests) niwa-collaboration-lobby--lobbies)
      (puthash "phone" '(:endpoint "https://push/room" :writable t) room-subs)
      (puthash (current-buffer) (list :session session :guests (make-hash-table)
                                      :transport 'room :push-guests room-subs)
               niwa-collaboration--rooms)
      (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
                 (lambda (transport frame)
                   (unless (and room-down (eq transport 'room))
                     (push (cons transport frame) controls))))
                ((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (transport peer frame) (push (list transport peer frame) frames))))
        (call "shared session")
        (should (member '(room :t "push" :guestIds ["phone"]) controls))
        (should (member '(lobby :t "push" :guestIds ["tablet"]) controls))
        (should (equal frames '((lobby 1 (:t "notice" :kind "notify")))))
        ;; The lobby still wakes the phone when the room socket cannot write.
        (setq room-down t controls nil)
        (call "room reconnecting")
        (should (equal '("phone" "tablet")
                       (sort (append (plist-get (cdar controls) :guestIds) nil) #'string<)))
        (clrhash niwa-collaboration--rooms)
        (setq controls nil)
        (call "unshared session")
        (should (= (length controls) 1))
        (should (eq 'lobby (caar controls)))
        (should (equal '("phone" "tablet")
                       (sort (append (plist-get (cdar controls) :guestIds) nil) #'string<)))))))

(niwa-deftest niwa-notify/workspace-start-failure ()
  ,test
  (test)
  :doc "a run failing before a session exists reaches the local notifier and its workspace lobby"
  (let* ((workspace (niwa-workspace--create :type 'file :root default-directory))
         (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (subs (make-hash-table :test #'equal)) local pushed
         (niwa-notify-function (lambda (kind details) (setq local (cons kind details)))))
    (puthash "phone" '(:writable t) subs)
    (puthash default-directory (list :push-guests subs :guests (make-hash-table)
                                     :transport 'lobby) niwa-collaboration-lobby--lobbies)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
               (lambda (transport frame) (push (cons transport frame) pushed))))
      (niwa-notify workspace 'failure "Release check failed: model unavailable"))
    (should (eq 'failure (car local)))
    (should (eq workspace (plist-get (cdr local) :workspace)))
    (should-not (plist-get (cdr local) :session))
    (should (equal "Release check failed: model unavailable" (plist-get (cdr local) :text)))
    (should (equal pushed '((lobby :t "push" :guestIds ["phone"]))))))

(niwa-deftest niwa-tool-notify/authority ()
  ,test
  (test)
  :doc "the standard root offers Notify without widening custom or child rosters"
  (niwa-tool-notify-test--with-request
    (let ((gptel-tools nil)
          (niwa-preset--registry nil))
      (niwa-define-preset notice-test :tools ((:tool "Read")))
      (niwa-preset--post 'notice-test nil)
      (should (equal '("Read") (mapcar #'niwa-tool-name (niwa-tools-native-roster fsm))))
      (niwa--define-presets)
      (niwa-preset--post 'niwa-implement nil)
      (should (member "Notify" (mapcar #'niwa-tool-name (niwa-tools-native-roster fsm))))
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create :parent-session session :path "/root/child"))
      (should-not (member "Notify" (mapcar #'niwa-tool-name (niwa-tools-native-roster fsm))))
      (should-not (member "Notify" (niwa-tool-ptc--roster)))
      (should (eq 'error (plist-get (call "Child bypass") :status)))
      (setq-local niwa--agent-invocation nil)
      (setf (niwa-request-origin niwa--current-request) "/root/child")
      (should (eq 'error (plist-get (call "Indirect child") :status)))
      (should-not notices))))

(niwa-deftest niwa-tool-notify/validation ()
  ,test
  (test)
  :doc "invalid text never reaches the user"
  (niwa-tool-notify-test--with-request
    (dolist (text (list "" "   " "one\ntwo" "one\rtwo" "one\u2028two" (make-string 241 ?x)))
      (should (eq 'error (plist-get (call text) :status))))
    (should-not notices)
    (call (make-string 240 ?é))
    (should (= 1 (length notices)))))

(niwa-deftest niwa-tool-notify/duplicate-window ()
  ,test
  (test)
  :doc "reopening preserves suppression; other text, owners and the 24h boundary pass"
  (niwa-tool-notify-test--with-request
    (let* ((now 1000000)
           (niwa-schedule-clock (lambda () now)))
      (call "CI red")
      (call "Login expired")
      (setq-local niwa--session
                  (niwa-session-create "renamed" (niwa-session-workspace session)
                                          nil (niwa-session-session-id session)))
      (setf (niwa-request-session niwa--current-request) niwa--session
            (niwa-session-save-path niwa--session) (niwa-session-save-path session))
      (cl-incf now 86399)
      (call "CI red")
      (should (= 2 (length notices)))
      (cl-incf now)
      (call "CI red")
      (should (= 3 (length notices)))
      (setq-local niwa--session (niwa-skills-test--make-session "other" default-directory))
      (make-directory (file-name-concat default-directory "other"))
      (setf (niwa-request-session niwa--current-request) niwa--session
            (niwa-session-save-path niwa--session) (file-name-concat default-directory "other"))
      (call "CI red")
      (should (= 4 (length notices))))))

(provide 'test-niwa-tool-notify)
;;; test-niwa-tool-notify.el ends here
