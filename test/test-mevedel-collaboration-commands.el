;;; test-mevedel-collaboration-commands.el --- Collaboration command boundary tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests collaboration observers, status, stopping, and public commands.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-pending-inputs)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-chat)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-input-files)
(require 'mevedel-view-render)
(require 'mevedel-workspace)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-ui)


;;
;;; Observer and command boundaries

(mevedel-deftest mevedel-collaboration-request-lifecycle
  (:doc "publishes busy after admission and idle after teardown without another response")
  (with-temp-buffer
    (let* ((session (mevedel-session--create :name "status"))
           (guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer (current-buffer)
                       :guests guests :transport 'transport))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           sent)
      (setq-local mevedel--session session)
      (puthash 1 (list :ready t) guests)
      (cl-letf (((symbol-function 'mevedel-request-assert-target-ready) #'ignore)
                ((symbol-function 'mevedel-session-artifacts-assert-mutation-authority) #'ignore)
                ((symbol-function 'mevedel-telemetry-record) #'ignore)
                ((symbol-function 'mevedel-collaboration--transport-control) #'ignore)
                ((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport _peer frame) (push frame sent) t)))
        (mevedel-collaboration--publish-status room)
        (dotimes (_ 2)
          (mevedel-request-begin session)
          (should (eq t (plist-get (car sent) :busy)))
          (mevedel-request-end)
          (should (eq :json-false (plist-get (car sent) :busy))))))))

(mevedel-deftest mevedel-collaboration--safe-post-response
  (:doc "installed response hooks coalesce updates and isolate publication faults")
  (mevedel-view-test--with-buffers
    (let* ((room (list :data-buffer data-buf))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           (draft "> first line\nsecond line\n> third line")
           scheduled stopped warnings)
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft 4))
      (with-current-buffer data-buf
        (setq-local gptel-post-stream-hook nil
                    gptel-post-response-functions nil)
        (mevedel-chat-install-request-hooks)
        ;; Other hook owners have their own render tests; exercise the real
        ;; collaboration observer and scheduler through both gptel seams.
        (cl-letf (((symbol-function 'mevedel-view-stream-schedule) #'ignore)
                  ((symbol-function 'mevedel-view-stream-render-response) #'ignore)
                  ((symbol-function 'mevedel-tool-repair-clear-ledger) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (_delay _repeat callback &rest args)
                     (push (cons callback args) scheduled)
                     'publication-timer)))
          (run-hooks 'gptel-post-stream-hook)
          (run-hook-with-args 'gptel-post-response-functions 1 1)
          (should (equal (list (list #'mevedel-collaboration--publish-timer data-buf))
                         scheduled))
          (should (eq 'publication-timer (plist-get room :publish-timer)))
          (cl-letf (((symbol-function 'mevedel-collaboration--schedule-publish)
                     (lambda (_room) (error "Observer failure")))
                    ((symbol-function 'mevedel-collaboration--stop-internal)
                     (lambda (failed-room reason)
                       (push (cons failed-room reason) stopped)))
                    ((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings))))
            (run-hooks 'gptel-post-stream-hook)
            (run-hook-with-args 'gptel-post-response-functions 1 1)
            (should (equal (make-list 2 (cons room 'observer-failure)) stopped))
            (should (= 2 (length warnings))))))
      (with-current-buffer view-buf
        (should (equal draft (mevedel-view--input-text)))
        (should (= 4 (- (point) (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-collaboration-notify-history-changed
  (:doc "coalesces committed history changes and isolates observer failures")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           scheduled failures)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat callback &rest args)
                   (push (cons callback args) scheduled) 'timer)))
        (mevedel-collaboration-notify-history-changed (current-buffer))
        (mevedel-collaboration-notify-history-changed (current-buffer))
        (should (= 1 (length scheduled))))
      (cl-letf (((symbol-function 'mevedel-collaboration--schedule-publish)
                 (lambda (_) (error "observer")))
                ((symbol-function 'mevedel-collaboration--observer-failure)
                 (lambda (failed) (push failed failures))))
        (mevedel-collaboration-notify-history-changed (current-buffer))
        (should (equal (list room) failures))))))

(mevedel-deftest mevedel-collaboration-status
  (:doc "reports safe active and inactive status without exposing secrets")
  (let* ((messages nil)
         (guests (make-hash-table :test #'eql))
         (room (list :session-label "share"
                     :transport 'transport
                     :key "secret-key-bytes"
                     :write-token "secret-token"
                     :link-full "http://example/#room.full-secret"
                     :link-view "http://example/#room.view-secret"
                     :guests guests))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room)))
    (puthash 1 (list :name "Phone" :writable t :ready t) guests)
    (puthash 2 (list :name "Laptop" :writable nil :ready t) guests)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages)))
              ((symbol-function 'mevedel-collaboration--transport-open-p)
               (lambda (_) t)))
      (mevedel-collaboration-status)
      (should (string-match-p "share" (car messages)))
      (should (string-match-p "connected" (car messages)))
      (should (string-match-p "Phone" (car messages)))
      (should (string-match-p "Laptop (view)" (car messages)))
      (should-not (string-match-p "secret" (car messages)))
      (clrhash mevedel-collaboration--rooms)
      (mevedel-collaboration-status)
      (should (string-match-p "inactive" (car messages)))
      ;; A running lobby is collaboration too.
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby--status)
                 (lambda () "Lobby: proj: relay connected; 0 guests")))
        (mevedel-collaboration-status)
        (should (string-match-p "active for Lobby: proj" (car messages)))))))

(mevedel-deftest mevedel-collaboration-status--preserves-composer
  (:doc "preserves a multiline composer draft beginning with >")
  (with-temp-buffer
    (insert "> first line\nsecond line\n> third line")
    (let* ((before (buffer-string))
           (room (list :session-label "draft" :transport nil
                       :guests (make-hash-table :test #'eql)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room)))
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (mevedel-collaboration-status))
      (should (equal before (buffer-string))))))

(mevedel-deftest mevedel-collaboration-stop
  (:doc "stops the current or only room and never another session's share")
  (let* ((stopped nil)
         (messages nil)
         (room (list :transport 'transport :session-label "share"))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room)))
    (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
               (lambda (stop-room reason) (push (cons stop-room reason)
                                                stopped)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      ;; From a session that is not shared, another session's share must
      ;; survive: report instead of tearing it down.
      (with-temp-buffer
        (cl-letf (((symbol-function
                    'mevedel-collaboration--current-data-buffer)
                   (lambda () (current-buffer))))
          (mevedel-collaboration-stop)))
      (should-not stopped)
      (should (string-match-p "no active share" (car messages)))
      ;; Outside any session context, stop falls back to every share.
      (cl-letf (((symbol-function
                  'mevedel-collaboration--current-data-buffer)
                 (lambda () nil)))
        (mevedel-collaboration-stop))
      (should (equal (list (cons room 'user-stop)) stopped))
      (should (string-match-p "stopped" (car messages)))
      (clrhash mevedel-collaboration--rooms)
      (mevedel-collaboration-stop)
      (should (string-match-p "not active" (car messages))))))

(mevedel-deftest mevedel-collaboration--room-for-overlay
  (:doc "resolves side-conversation interaction overlays to the parent session's room")
  (let* ((parent-data (generate-new-buffer " *collab-overlay-parent*"))
         (side-data (generate-new-buffer " *collab-overlay-side*"))
         (side-view (generate-new-buffer " *collab-overlay-view*"))
         (room (list :data-buffer parent-data))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room)))
    (unwind-protect
        (progn
          (require 'mevedel-side-conversation)
          (with-current-buffer side-data
            (setq-local mevedel-side-conversation--parent-buffer
                        parent-data))
          (with-current-buffer side-view
            (setq-local mevedel--data-buffer side-data)
            (insert "prompt")
            ;; A /btw permission prompt renders in the side view; its
            ;; authority surface is the parent session's room.
            (should (eq room (mevedel-collaboration--room-for-overlay
                              (make-overlay 1 2))))))
      (kill-buffer side-view)
      (kill-buffer side-data)
      (kill-buffer parent-data))))

(mevedel-deftest mevedel-collaboration-view
  (:doc "discloses secrets and bearer-link scope before starting")
  (let ((session (mevedel-session--create :name "share"))
        (data-buffer (generate-new-buffer " *collaboration-disclosure*"))
        prompts)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function
                      'mevedel-collaboration--current-data-buffer)
                     (lambda () data-buffer))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (push prompt prompts)
                       nil)))
            (should-error (mevedel-collaboration-view) :type 'user-error))
          (should (string-match-p "credentials or secrets" (car prompts)))
          (should (string-match-p "bearer" (car prompts))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-cmd--collab
  (:doc "does not return a bearer URL to slash dispatch")
  (cl-letf (((symbol-function 'mevedel-collaboration-view)
             (lambda () "http://127.0.0.1:1/#room.secret")))
    (should-not (mevedel-cmd--collab "view"))
    (should-not (mevedel-cmd--collab ""))
    (let (called)
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby)
                 (lambda () (push 'lobby called) "http://x/#room.secret"))
                ((symbol-function 'mevedel-collaboration-lobby-stop)
                 (lambda () (push 'stop called)))
                ((symbol-function 'mevedel-collaboration-lobby-rotate)
                 (lambda () (push 'rotate called))))
        (should-not (mevedel-cmd--collab "lobby"))
        (should-not (mevedel-cmd--collab "lobby stop"))
        (should-not (mevedel-cmd--collab " lobby rotate "))
        (should (equal '(rotate stop lobby) called))))))

(mevedel-deftest mevedel-skills--dispatch-slash-command
  (:doc "dispatches /collab without copying its bearer URL into messages")
  (with-temp-buffer
    (let ((gptel-prompt-prefix-alist '((fundamental-mode . "### ")))
          (messages nil))
      (insert "### /collab view")
      (cl-letf (((symbol-function 'mevedel-collaboration-view)
                 (lambda () "http://127.0.0.1:1/#room.secret"))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should-not (seq-some (lambda (message)
                                (string-match-p "room\\.secret" message))
                              messages))))))

(mevedel-deftest mevedel-skills-local-command-active-request-p
  (:doc "allows collaboration safety commands while a request is active")
  (progn
    (should (mevedel-skills-local-command-active-request-p "collab" "status"))
    (should (mevedel-skills-local-command-active-request-p "collab" "stop"))))

(provide 'test-mevedel-collaboration-commands)
;;; test-mevedel-collaboration-commands.el ends here
