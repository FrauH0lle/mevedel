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

(mevedel-deftest mevedel-collaboration--safe-post-stream
  (:doc "contains observer failures without signaling into the request")
  (with-temp-buffer
    (let* ((stopped nil)
           (room (list :data-buffer (current-buffer)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room)))
      (cl-letf (((symbol-function 'mevedel-collaboration--post-stream)
                 (lambda () (error "Observer failure")))
                ((symbol-function 'mevedel-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason)))
                ((symbol-function 'display-warning) (lambda (&rest _) nil)))
        (should-not (mevedel-collaboration--safe-post-stream))
        (should (eq 'observer-failure stopped))))))

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
      (should (string-match-p "inactive" (car messages))))))

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
    (should-not (mevedel-cmd--collab ""))))

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
