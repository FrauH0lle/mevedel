;;; test-niwa-collaboration-commands.el --- Collaboration command boundary tests -*- lexical-binding: t; -*-

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
(require 'niwa-collaboration-projection)
(require 'niwa-collaboration-transport)
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-lobby)
(require 'niwa-pending-inputs)
(require 'niwa-prompt-submission)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-transcript)
(require 'niwa-transcript-audit)
(require 'niwa-chat)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-input-files)
(require 'niwa-view-render)
(require 'niwa-workspace)
(require 'niwa-skills-invoke)
(require 'niwa-skills-ui)


;;
;;; Observer and command boundaries

(niwa-deftest niwa-collaboration-request-lifecycle
  (:doc "publishes busy after admission and idle after teardown without another response")
  (with-temp-buffer
    (let* ((session (niwa-session--create :name "status"))
           (guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer (current-buffer)
                       :guests guests :transport 'transport))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           sent)
      (setq-local niwa--session session)
      (puthash 1 (list :ready t) guests)
      (cl-letf (((symbol-function 'niwa-request-assert-target-ready) #'ignore)
                ((symbol-function 'niwa-session-artifacts-assert-mutation-authority) #'ignore)
                ((symbol-function 'niwa-telemetry-record) #'ignore)
                ((symbol-function 'niwa-collaboration--transport-control) #'ignore)
                ((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (_transport _peer frame) (push frame sent) t)))
        (niwa-collaboration--publish-status room)
        (dotimes (_ 2)
          (niwa-request-begin session)
          (should (eq t (plist-get (car sent) :busy)))
          (niwa-request-end)
          (should (eq :json-false (plist-get (car sent) :busy))))))))

(niwa-deftest niwa-collaboration--safe-post-response
  (:doc "installed response hooks coalesce updates and isolate publication faults")
  (niwa-view-test--with-buffers
    (let* ((room (list :data-buffer data-buf))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           (draft "> first line\nsecond line\n> third line")
           scheduled stopped warnings publication-timer)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft draft 4))
      (with-current-buffer data-buf
        (setq-local gptel-post-stream-hook nil
                    gptel-post-response-functions nil)
        (niwa-chat-install-request-hooks)
        ;; Other hook owners have their own render tests; exercise the real
        ;; collaboration observer and scheduler through both gptel seams.
        (cl-letf (((symbol-function 'niwa-view-stream-schedule) #'ignore)
                  ((symbol-function 'niwa-view-stream-render-response) #'ignore)
                  ((symbol-function 'niwa-tool-repair-clear-ledger) #'ignore)
                  ((symbol-function 'niwa-transport-run-at-time)
                   (lambda (_delay callback &rest args)
                     (push (cons callback args) scheduled)
                     (setq publication-timer (run-at-time 60 nil #'ignore)))))
          (run-hooks 'gptel-post-stream-hook)
          (run-hook-with-args 'gptel-post-response-functions 1 1)
          (should (equal (list (list #'niwa-collaboration--publish-timer data-buf))
                         scheduled))
          (should (eq publication-timer (plist-get room :publish-timer)))
          (cancel-timer publication-timer)
          (cl-letf (((symbol-function 'niwa-collaboration--schedule-publish)
                     (lambda (_room) (error "Observer failure")))
                    ((symbol-function 'niwa-collaboration--stop-internal)
                     (lambda (failed-room reason)
                       (push (cons failed-room reason) stopped)))
                    ((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings))))
            (run-hooks 'gptel-post-stream-hook)
            (run-hook-with-args 'gptel-post-response-functions 1 1)
            (should-not stopped)
            ;; Both seams fail; the host is warned once, with the error.
            (should (= 1 (length warnings)))
            (should (string-search "Observer failure" (cadr (car warnings)))))))
      (with-current-buffer view-buf
        (should (equal draft (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-test--composer-point-offset)))))))

(niwa-deftest niwa-collaboration--schedule-publish
  (:doc "a publish timer lost to a TRAMP timer suspension is re-armed")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           (publishes 0))
      (require 'tramp)
      (cl-letf (((symbol-function 'niwa-collaboration--publish)
                 (lambda (_) (cl-incf publishes))))
        (with-tramp-suspended-timers
          (niwa-collaboration--schedule-publish room))
        (niwa-collaboration--schedule-publish room)
        ;; Rearming/one delivery is the contract, not one-second publication.
        (with-timeout (5 (ert-fail "Publication never ran"))
          (while (zerop publishes) (accept-process-output nil 0.02)))
        (should (= 1 publishes))))))

(niwa-deftest niwa-collaboration--schedule-publish/cost
  (:doc "paces coalesced publication by the last publish's measured cost, up to a second")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :guests (make-hash-table :test #'eql)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           delays)
      (niwa-collaboration--publish room)
      (should (numberp (plist-get room :publish-cost)))
      (cl-letf (((symbol-function 'niwa-transport-run-at-time)
                 (lambda (delay &rest _) (push delay delays) nil)))
        (plist-put room :publish-cost 0.001)
        (niwa-collaboration--schedule-publish room)
        (plist-put room :publish-cost 0.2)
        (niwa-collaboration--schedule-publish room)
        ;; A pause inside one publish does not stall the stream for long.
        (plist-put room :publish-cost 3.0)
        (niwa-collaboration--schedule-publish room))
      (should (equal (list 1.0
                           (* niwa-collaboration--publish-cost-factor 0.2)
                           niwa-collaboration--publish-delay)
                     delays)))))

(niwa-deftest niwa-collaboration-notify-history-changed
  (:doc "coalesces committed history changes and isolates observer failures")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           scheduled failures)
      (cl-letf (((symbol-function 'niwa-transport-run-at-time)
                 (lambda (_delay callback &rest args)
                   (push (cons callback args) scheduled)
                   (run-at-time 60 nil #'ignore))))
        (niwa-collaboration-notify-history-changed (current-buffer))
        (niwa-collaboration-notify-history-changed (current-buffer))
        (should (= 1 (length scheduled)))
        (cancel-timer (plist-get room :publish-timer)))
      (cl-letf (((symbol-function 'niwa-collaboration--schedule-publish)
                 (lambda (_) (error "observer")))
                ((symbol-function 'niwa-collaboration--observer-failure)
                 (lambda (failed &rest _) (push failed failures))))
        (niwa-collaboration-notify-history-changed (current-buffer))
        (should (equal (list room) failures))))))

(niwa-deftest niwa-collaboration-status
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
         (niwa-collaboration--rooms (niwa-test-room-registry room)))
    (puthash 1 (list :name "Phone" :writable t :ready t) guests)
    (puthash 2 (list :name "Laptop" :writable nil :ready t) guests)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages)))
              ((symbol-function 'niwa-collaboration--transport-open-p)
               (lambda (_) t)))
      (niwa-collaboration-status)
      (should (string-match-p "share" (car messages)))
      (should (string-match-p "connected" (car messages)))
      (should (string-match-p "Phone" (car messages)))
      (should (string-match-p "Laptop (view)" (car messages)))
      (should-not (string-match-p "secret" (car messages)))
      (clrhash niwa-collaboration--rooms)
      (niwa-collaboration-status)
      (should (string-match-p "inactive" (car messages)))
      ;; A running lobby is collaboration too.
      (cl-letf (((symbol-function 'niwa-collaboration-lobby--status)
                 (lambda () "Lobby: proj: relay connected; 0 guests")))
        (niwa-collaboration-status)
        (should (string-match-p "active for Lobby: proj" (car messages)))))))

(niwa-deftest niwa-collaboration-status--preserves-composer
  (:doc "preserves a multiline composer draft beginning with >")
  (with-temp-buffer
    (insert "> first line\nsecond line\n> third line")
    (let* ((before (buffer-string))
           (room (list :session-label "draft" :transport nil
                       :guests (make-hash-table :test #'eql)))
           (niwa-collaboration--rooms (niwa-test-room-registry room)))
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (niwa-collaboration-status))
      (should (equal before (buffer-string))))))

(niwa-deftest niwa-collaboration-stop
  (:doc "stops the current or only room and never another session's share")
  (let* ((stopped nil)
         (messages nil)
         (room (list :transport 'transport :session-label "share"))
         (niwa-collaboration--rooms (niwa-test-room-registry room)))
    (cl-letf (((symbol-function 'niwa-collaboration--stop-internal)
               (lambda (stop-room reason) (push (cons stop-room reason)
                                                stopped)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      ;; From a session that is not shared, another session's share must
      ;; survive: report instead of tearing it down.
      (with-temp-buffer
        (cl-letf (((symbol-function
                    'niwa-collaboration--current-data-buffer)
                   (lambda () (current-buffer))))
          (niwa-collaboration-stop)))
      (should-not stopped)
      (should (string-match-p "no active share" (car messages)))
      ;; Outside any session context, stop falls back to every share.
      (cl-letf (((symbol-function
                  'niwa-collaboration--current-data-buffer)
                 (lambda () nil)))
        (niwa-collaboration-stop))
      (should (equal (list (cons room 'user-stop)) stopped))
      (should (string-match-p "stopped" (car messages)))
      (clrhash niwa-collaboration--rooms)
      (niwa-collaboration-stop)
      (should (string-match-p "not active" (car messages))))))

(niwa-deftest niwa-collaboration-stop-session
  (:doc "stops only the given session's room, whatever other rooms are live")
  (let* ((mine (generate-new-buffer " *stop-mine*"))
         (other (generate-new-buffer " *stop-other*"))
         (room (list :data-buffer other :session-label "other"))
         (niwa-collaboration--rooms (niwa-test-room-registry room))
         stopped messages)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration--stop-internal)
                   (lambda (stop-room reason) (push (cons stop-room reason) stopped)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (should-not (niwa-collaboration-stop-session mine))
          (should-not (niwa-collaboration-stop-session nil))
          (should-not stopped)
          (should (string-match-p "not shared" (car messages)))
          (should (niwa-collaboration-stop-session other))
          (should (equal (list (cons room 'user-stop)) stopped))
          (should (string-match-p "stopped for other" (car messages))))
      (kill-buffer mine)
      (kill-buffer other))))

(niwa-deftest niwa-collaboration-shares
  (:doc "lists rooms then lobbies from memory, counting distinct guests, without secrets")
  (let* ((data (generate-new-buffer " *shares*"))
         (guests (make-hash-table :test #'eql))
         (room (list :data-buffer data :guests guests :transport 'transport
                     :session-label "Review"
                     :session (niwa-session--create
                               :workspace (niwa-workspace--create :root "/origin/project/"))
                     :link-full "http://example/#room.full-secret"))
         (niwa-collaboration--rooms (niwa-test-room-registry)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration--transport-open-p)
                   (lambda (_) t))
                  ((symbol-function 'niwa-collaboration-lobby-shares)
                   (lambda () (list (list :kind 'lobby :label "proj")))))
          (should (equal (list (list :kind 'lobby :label "proj"))
                         (niwa-collaboration-shares)))
          (puthash data room niwa-collaboration--rooms)
          (puthash 1 (list :guest-id "a") guests)
          (puthash 2 (list :guest-id "a") guests)
          (let ((shares (niwa-collaboration-shares)))
            (should (equal (list :kind 'room :label "Review" :guests 1
                                 :connected t :data-buffer data :diagnostic-scope "/origin/project/")
                           (car shares)))
            (should (eq 'lobby (plist-get (cadr shares) :kind)))
            (should-not (string-search "secret" (format "%S" shares)))))
      (kill-buffer data))))

(niwa-deftest niwa-collaboration-show-share
  (:doc "shows a live room's links, refuses a stopped one, and routes lobbies")
  (let* ((data (generate-new-buffer " *show-share*"))
         (room (list :data-buffer data :session-label "Review"))
         (niwa-collaboration--rooms (niwa-test-room-registry room))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration-share-present)
                   (lambda (shown) (push (list 'present shown) calls)))
                  ((symbol-function 'niwa-collaboration-lobby-show-directory)
                   (lambda (root restart) (push (list 'lobby root restart) calls))))
          (niwa-collaboration-show-share
           (list :kind 'room :label "Review" :data-buffer data))
          (niwa-collaboration-show-share
           (list :kind 'lobby :directory "/root/"))
          (niwa-collaboration-show-share
           (list :kind 'recorded-lobby :directory "/srv/"))
          (should (equal (list (list 'present room)
                               (list 'lobby "/root/" nil)
                               (list 'lobby "/srv/" t))
                         (nreverse calls)))
          (clrhash niwa-collaboration--rooms)
          (kill-buffer data)
          (should (equal (list 'user-error "niwa [/origin/project/]: Review is no longer shared")
                         (should-error
                          (niwa-collaboration-show-share
                           (list :kind 'room :label "Review" :data-buffer data
                                 :diagnostic-scope "/origin/project/"))
                          :type 'user-error))))
      (kill-buffer data))))

(niwa-deftest niwa-collaboration-stop-share
  (:doc "stops exactly the chosen room or lobby")
  (let* ((data (generate-new-buffer " *stop-share*"))
         (room (list :data-buffer data :session-label "Review"))
         (niwa-collaboration--rooms (niwa-test-room-registry room))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration-stop-session)
                   (lambda (buffer) (push (list 'room buffer) calls)))
                  ((symbol-function 'niwa-collaboration-lobby-stop-directory)
                   (lambda (root) (push (list 'lobby root) calls))))
          (niwa-collaboration-stop-share
           (list :kind 'room :label "Review" :data-buffer data))
          (niwa-collaboration-stop-share
           (list :kind 'recorded-lobby :directory "/srv/"))
          (should (equal (list (list 'room data) (list 'lobby "/srv/"))
                         (nreverse calls)))
          (clrhash niwa-collaboration--rooms)
          (should-error (niwa-collaboration-stop-share
                         (list :kind 'room :label "Review" :data-buffer data))
                        :type 'user-error))
      (kill-buffer data))))

(niwa-deftest niwa-collaboration-stop-all
  (:doc "stops every room, including other sessions', and every lobby")
  (let* ((first (list :data-buffer (generate-new-buffer " *all-1*")))
         (second (list :data-buffer (generate-new-buffer " *all-2*")))
         (niwa-collaboration--rooms (niwa-test-room-registry first second))
         (lobbies 2)
         stopped messages)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-collaboration--stop-internal)
                   (lambda (room reason)
                     (push (cons room reason) stopped)
                     (remhash (plist-get room :data-buffer)
                              niwa-collaboration--rooms)))
                  ((symbol-function 'niwa-collaboration-lobby-stop-every)
                   (lambda () (prog1 lobbies (setq lobbies 0)))))
          (niwa-test--with-captured-messages messages
            ;; From inside one session, the others stop too.
            (with-current-buffer (plist-get first :data-buffer)
              (niwa-collaboration-stop-all)))
          (should (equal (list (cons first 'user-stop) (cons second 'user-stop))
                         (nreverse stopped)))
          (should (string-match-p "stopped 2 shares and 2 lobbies" messages))
          (niwa-test--with-captured-messages messages
            (niwa-collaboration-stop-all))
          (should (string-match-p "not active" messages)))
      (kill-buffer (plist-get first :data-buffer))
      (kill-buffer (plist-get second :data-buffer)))))

(niwa-deftest niwa-collaboration-room-summary
  (:doc "summarizes a session's room from memory, without its secrets")
  (let* ((data (generate-new-buffer " *summary*"))
         (guests (make-hash-table :test #'eql))
         (room (list :data-buffer data :guests guests
                     :link-full "http://example/#room.full-secret"))
         (niwa-collaboration--rooms (niwa-test-room-registry room)))
    (unwind-protect
        (progn
          (should-not (niwa-collaboration-room-summary nil))
          (should (equal '(:guests 0) (niwa-collaboration-room-summary data)))
          (puthash 1 (list :name "Phone" :guest-id "a") guests)
          (puthash 2 (list :name "Phone" :guest-id "a") guests)
          (puthash 3 (list :name "Laptop" :guest-id "b") guests)
          ;; Tabs of one browser are one guest.
          (should (equal '(:guests 2) (niwa-collaboration-room-summary data)))
          (clrhash niwa-collaboration--rooms)
          (should-not (niwa-collaboration-room-summary data)))
      (kill-buffer data))))

(niwa-deftest niwa-collaboration--room-for-overlay
  (:doc "resolves side-conversation interaction overlays to the parent session's room")
  (let* ((parent-data (generate-new-buffer " *collab-overlay-parent*"))
         (side-data (generate-new-buffer " *collab-overlay-side*"))
         (side-view (generate-new-buffer " *collab-overlay-view*"))
         (room (list :data-buffer parent-data))
         (niwa-collaboration--rooms (niwa-test-room-registry room)))
    (unwind-protect
        (progn
          (require 'niwa-side-conversation)
          (with-current-buffer side-data
            (setq-local niwa-side-conversation--parent-buffer
                        parent-data))
          (with-current-buffer side-view
            (setq-local niwa--data-buffer side-data)
            (insert "prompt")
            ;; A /btw permission prompt renders in the side view; its
            ;; authority surface is the parent session's room.
            (should (eq room (niwa-collaboration--room-for-overlay
                              (make-overlay 1 2))))))
      (kill-buffer side-view)
      (kill-buffer side-data)
      (kill-buffer parent-data))))

(niwa-deftest niwa-collaboration--room-or-start
  (:doc "attributes a command refusal without changing the internal session condition")
  (with-temp-buffer
    (setq-local niwa--workspace (niwa-workspace--create :root "/origin/project/"))
    (should (equal '(user-error "No active niwa session in this buffer")
                   (should-error (niwa-collaboration--data-buffer-session (current-buffer))
                                 :type 'user-error)))
    (should (equal (list 'user-error "niwa [/origin/project/]: No active niwa session in this buffer")
                   (should-error (niwa-collaboration--room-or-start (current-buffer))
                                 :type 'user-error)))))

(niwa-deftest niwa-collaboration-view
  (:doc "discloses secrets and bearer-link scope before starting")
  (let ((session (niwa-session--create :name "share"))
        (data-buffer (generate-new-buffer " *collaboration-disclosure*"))
        prompts)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function
                      'niwa-collaboration--current-data-buffer)
                     (lambda () data-buffer))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (push prompt prompts)
                       nil)))
            (should-error (niwa-collaboration-view) :type 'user-error))
          (should (string-match-p "credentials or secrets" (car prompts)))
          (should (string-match-p "bearer" (car prompts))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(niwa-deftest niwa-collaboration-open-shared-item
  (:doc "opens the item in the session's room, starting it only after consent")
  (let ((session (niwa-session--create :name "share"))
        (data-buffer (generate-new-buffer " *collaboration-open-item*"))
        (room (list :session-label "share"
                    :link-full "https://relay.example/#room123456.secret"))
        live consent started opened)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-collaboration--room-for-buffer)
                     (lambda (_) live))
                    ((symbol-function 'yes-or-no-p) (lambda (_) consent))
                    ((symbol-function 'niwa-collaboration--start)
                     (lambda (seen buffer)
                       (should (eq session seen))
                       (should (eq data-buffer buffer))
                       (setq started t)
                       room))
                    ((symbol-function 'browse-url)
                     (lambda (url &rest _) (setq opened url)))
                    ((symbol-function 'message) #'ignore))
            ;; Declining leaves the session unshared and opens nothing.
            (should-error (niwa-collaboration-open-shared-item data-buffer "ab12")
                          :type 'user-error)
            (should-not (or started opened))
            (setq consent t)
            (niwa-collaboration-open-shared-item data-buffer "ab12")
            (should started)
            (should (equal "https://relay.example/?shared=ab12#room123456.secret" opened))
            ;; A live room opens directly, without asking again.
            (setq live room started nil consent nil opened nil)
            (niwa-collaboration-open-shared-item data-buffer "cd34")
            (should-not started)
            (should (equal "https://relay.example/?shared=cd34#room123456.secret" opened))))
      (kill-buffer data-buffer))))

(niwa-deftest niwa-collaboration-item-opener
  (:doc "asks to share while preparing, then opens later without asking again")
  (let ((session (niwa-session--create :name "share"))
        (data-buffer (generate-new-buffer " *collaboration-opener*"))
        (room (list :session-label "share"
                    :link-full "https://relay.example/#room123456.secret"))
        live consent asked started opened)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-collaboration--room-for-buffer)
                     (lambda (_) live))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (_) (setq asked (1+ (or asked 0))) consent))
                    ((symbol-function 'niwa-collaboration--start)
                     (lambda (seen buffer)
                       (should (eq session seen))
                       (should (eq data-buffer buffer))
                       (setq started t live room)
                       room))
                    ((symbol-function 'browse-url)
                     (lambda (url &rest _) (setq opened url)))
                    ((symbol-function 'message) #'ignore))
            ;; Declining while preparing leaves nothing to open.
            (should-error (niwa-collaboration-item-opener data-buffer)
                          :type 'user-error)
            (should (= 1 asked))
            (should-not (or started opened))
            ;; Consent is asked once, up front; the opener itself never asks.
            (setq consent t asked nil)
            (let ((opener (niwa-collaboration-item-opener)))
              (should (= 1 asked))
              (setq consent nil asked nil)
              (funcall opener data-buffer "ab12")
              (should-not asked)
              (should started)
              (should (equal "https://relay.example/?shared=ab12#room123456.secret"
                             opened)))
            ;; A session shared already opens without asking; if its room
            ;; ends meanwhile, the opener refuses rather than reshare unasked.
            (setq started nil opened nil)
            (let ((opener (niwa-collaboration-item-opener data-buffer)))
              (should-not asked)
              (setq live nil)
              (should-error (funcall opener data-buffer "cd34") :type 'user-error)
              (should-not (or started opened)))))
      (kill-buffer data-buffer))))

(niwa-deftest niwa-cmd--collab
  (:doc "does not return a bearer URL to slash dispatch")
  (cl-letf (((symbol-function 'niwa-collaboration-view)
             (lambda () "http://127.0.0.1:1/#room.secret")))
    (should-not (niwa-cmd--collab "view"))
    (should-not (niwa-cmd--collab ""))
    (let (called)
      (cl-letf (((symbol-function 'niwa-collaboration-lobby)
                 (lambda () (push 'lobby called) "http://x/#room.secret"))
                ((symbol-function 'niwa-collaboration-lobby-stop)
                 (lambda () (push 'stop called)))
                ((symbol-function 'niwa-collaboration-lobby-rotate)
                 (lambda () (push 'rotate called)))
                ((symbol-function 'niwa-collaboration-stop-all)
                 (lambda () (push 'stop-all called))))
        (should-not (niwa-cmd--collab "lobby"))
        (should-not (niwa-cmd--collab "lobby stop"))
        (should-not (niwa-cmd--collab " lobby rotate "))
        (should-not (niwa-cmd--collab "stop all"))
        (should (equal '(stop-all rotate stop lobby) called))))))

(niwa-deftest niwa-skills--dispatch-slash-command
  (:doc "dispatches /collab without copying its bearer URL into messages")
  (with-temp-buffer
    (let ((messages nil))
      (insert "/collab view")
      (cl-letf (((symbol-function 'niwa-collaboration-view)
                 (lambda () "http://127.0.0.1:1/#room.secret"))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (should (eq 'local (niwa-skills--dispatch-slash-command)))
        (should-not (seq-some (lambda (message)
                                (string-match-p "room\\.secret" message))
                              messages))))))

(niwa-deftest niwa-skills-local-command-active-request-p
  (:doc "allows collaboration safety commands while a request is active")
  (progn
    (should (niwa-skills-local-command-active-request-p "collab" "status"))
    (should (niwa-skills-local-command-active-request-p "collab" "stop"))))

(provide 'test-niwa-collaboration-commands)
;;; test-niwa-collaboration-commands.el ends here
