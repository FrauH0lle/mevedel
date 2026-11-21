;;; test-niwa-view-control-transfer.el -- View transfer UI tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests control-transfer polling, presentation, commands, and lifecycle.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution)
(require 'niwa-session-artifacts)
(require 'niwa-session-codec)
(require 'niwa-session-control-transfer)
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-view-control-transfer)
(require 'niwa-view-render)

(defmacro test-niwa-view-control-transfer--with-pair
    (session read-only &rest body)
  "Run BODY in a view paired to data holding SESSION and READ-ONLY mode."
  (declare (indent 2))
  `(let ((data (generate-new-buffer " *niwa-transfer-data*"))
         (view (generate-new-buffer " *niwa-transfer-view*")))
     (unwind-protect
         (progn
           (with-current-buffer data
             (setq-local niwa--session ,session)
             (setq-local niwa-session--read-only-mode ,read-only))
           (with-current-buffer view
             (setq-local niwa--data-buffer data)
             (setq-local niwa-view--agent-transcript-p nil)
             (setq-local niwa-view--control-transfer-rebuild-function
                         #'ignore)
             ,@body))
       (when (buffer-live-p view)
         (with-current-buffer view
           (niwa-view-control-transfer-teardown)))
       (dolist (buffer (list data view))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))))))

(niwa-deftest niwa-view--control-transfer-body ()
  ,test
  (test)
  :doc "frames transfer decisions with their advertised key actions"
  (let* ((descriptor
          '(:title "Laptop requests control"
                   :detail "Session must drain first"
                   :attention t
                   :keys (("g" . "grant") ("k" . "keep"))))
         (body (niwa-view--control-transfer-body descriptor)))
    (should (string-match-p "Laptop requests control" body))
    (should (string-match-p "Session must drain first" body))
    (should
     (cl-loop
      for index from 0 below (length body)
      thereis (let ((face (get-text-property index 'font-lock-face body)))
                (member 'help-key-binding
                        (if (proper-list-p face) face (list face))))))))

(niwa-deftest niwa-view--control-transfer-refresh ()
  ,test
  (test)
  :doc "rebuilds the interaction zone only when the transfer descriptor changes"
  (let ((session (niwa-session--create :name "main"))
        (rebuilds 0))
    (test-niwa-view-control-transfer--with-pair session nil
      (setq-local niwa-view--control-transfer-rebuild-function
                  (lambda () (cl-incf rebuilds)))
      (dotimes (_ 3) (niwa-view--control-transfer-refresh view))
      (should (= 1 rebuilds))
      (niwa-session-set-control-transfer
       session (list :state 'requested
                     :request (list :requester-label "laptop")))
      (niwa-view--control-transfer-refresh view)
      (should (= 2 rebuilds))
      (niwa-view--control-transfer-refresh view)
      (should (= 2 rebuilds)))))

(niwa-deftest niwa-view--control-transfer-poll-seconds ()
  ,test
  (test)
  :doc "uses local, remote, and active transfer cadences"
  (let ((niwa-view-control-transfer-poll-seconds 5)
        (niwa-view-control-transfer-remote-poll-seconds 30)
        (niwa-view-control-transfer-active-poll-seconds 2)
        (local (niwa-session--create
                :name "local" :working-directory "/srv/project/"))
        (remote (niwa-session--create
                 :name "remote"
                 :working-directory "/ssh:user@host:/srv/project/")))
    (should (= 5 (niwa-view--control-transfer-poll-seconds nil)))
    (should (= 5 (niwa-view--control-transfer-poll-seconds local)))
    (should (= 30 (niwa-view--control-transfer-poll-seconds remote)))
    (dolist (state '(requested quiescing))
      (setf (niwa-session-control-transfer remote) (list :state state))
      (should (= 2 (niwa-view--control-transfer-poll-seconds remote))))
    (setf (niwa-session-control-transfer remote) '(:state acquired))
    (should (= 30 (niwa-view--control-transfer-poll-seconds remote)))))

(niwa-deftest niwa-view-control-transfer-current-descriptor ()
  ,test
  (test)
  :doc "reads authority from data and returns the complete interaction row"
  (let ((session (niwa-session--create :name "control"))
        read-only-args)
    (cl-letf (((symbol-function
                'niwa-session-control-transfer-descriptor)
               (lambda (_session read-only-p)
                 (push read-only-p read-only-args)
                 '(:kind request :action grant :title "Transfer"))))
      (test-niwa-view-control-transfer--with-pair session t
        (let ((descriptor
               (niwa-view-control-transfer-current-descriptor)))
          (should (eq 'control-transfer (plist-get descriptor :id)))
          (should (eq niwa-view--control-transfer-map
                      (plist-get descriptor :keymap)))
          (should (string-match-p "Transfer" (plist-get descriptor :body)))))
      (test-niwa-view-control-transfer--with-pair session nil
        (niwa-view-control-transfer-current-descriptor)))
    (should (equal '(nil t) read-only-args)))

  :doc "binds only the actions displayed for each transfer side"
  (progn
    (should (eq (lookup-key niwa-view--control-transfer-map (kbd "g"))
                #'niwa-view-control-transfer-grant))
    (should (eq (lookup-key niwa-view--control-transfer-map (kbd "k"))
                #'niwa-view-control-transfer-keep))
    (should
     (eq (lookup-key niwa-view--control-transfer-request-map (kbd "r"))
         #'niwa-take-control))
    (dolist (map (list niwa-view--control-transfer-map
                       niwa-view--control-transfer-request-map
                       niwa-view--control-transfer-status-map))
      (dolist (key '("<mouse-1>" "<mouse-2>" "<mouse-3>" "RET" "<return>"))
        (should-not (lookup-key map (kbd key)))))
    (should-not (lookup-key niwa-view--control-transfer-map (kbd "r")))
    (should-not
     (lookup-key niwa-view--control-transfer-status-map (kbd "g")))
    (should-not
     (lookup-key niwa-view--control-transfer-status-map (kbd "r")))))

(niwa-deftest niwa-view-control-transfer-initialize ()
  ,test
  (test)
  :doc "registers the root, presentation, observer, drain, and poll timer"
  (dolist (symbol '(niwa-view--control-transfer-refresh
                    niwa-view--control-transfer-body
                    niwa-take-control))
    (should (equal "niwa-view-control-transfer"
                   (file-name-base (or (symbol-file symbol 'defun) "")))))
  (let ((session (niwa-session--create
                  :name "control" :session-id "control"
                  :authority-mode 'portable)))
    (test-niwa-view-control-transfer--with-pair session nil
      (let ((rebuild #'ignore)
            (drain (lambda () t)))
        (niwa-view-control-transfer-initialize rebuild drain)
        (should (eq rebuild
                    niwa-view--control-transfer-rebuild-function))
        (should (eq data
                    (niwa-session-control-transfer-root-buffer session)))
        (should (memq drain
                      (niwa-session-control-transfer-drains session)))
        (should (gethash session
                         niwa-session-control-transfer--observers))
        (should (timerp niwa-view--control-transfer-timer))))))

(niwa-deftest niwa-view-control-transfer-stop-polling ()
  ,test
  (test)
  :doc "retains registrations but refuses rearming until initialization"
  (let ((session (niwa-session--create :name "stop-polling"
                                          :authority-mode 'portable)))
    (test-niwa-view-control-transfer--with-pair session nil
      (let ((drain (lambda () t)))
        (niwa-view-control-transfer-initialize #'ignore drain)
        (let ((timer niwa-view--control-transfer-timer))
          (niwa-view-control-transfer-stop-polling)
          (niwa-view-control-transfer-stop-polling)
          (should-not (memq timer timer-list)))
        (should (eq data
                    (niwa-session-control-transfer-root-buffer session)))
        (should (memq drain (niwa-session-control-transfer-drains session)))
        (should (gethash session niwa-session-control-transfer--observers))
        (niwa-view--control-transfer-schedule view)
        (should-not niwa-view--control-transfer-timer)
        (niwa-view-control-transfer-initialize #'ignore drain)
        (should (memq niwa-view--control-transfer-timer timer-list))))))

(niwa-deftest niwa-view--control-transfer-schedule ()
  ,test
  (test)
  :doc "does not poll a session that cannot transfer control"
  (let ((session (niwa-session--create :name "local")))
    (test-niwa-view-control-transfer--with-pair session nil
      (niwa-view--control-transfer-schedule view)
      (should-not niwa-view--control-transfer-timer))))

(niwa-deftest niwa-view-control-transfer-teardown ()
  ,test
  (test)
  :doc "removes every registration and cancels the owned timer"
  (let ((session (niwa-session--create
                  :name "control" :session-id "control"
                  :authority-mode 'portable)))
    (test-niwa-view-control-transfer--with-pair session nil
      (let ((drain (lambda () t)))
        (niwa-view-control-transfer-initialize #'ignore drain)
        (niwa-view-control-transfer-teardown)
        (should-not niwa-view--control-transfer-rebuild-function)
        (should-not niwa-view--control-transfer-timer)
        (should-not
         (niwa-session-control-transfer-root-buffer session))
        (should-not
         (niwa-session-control-transfer-drains session))
        (should-not
         (gethash session niwa-session-control-transfer--observers))))))

(niwa-deftest niwa-take-control ()
  ,test
  (test)
  :doc "acquires free authority, requests held authority, and rejects owners"
  (let ((session (niwa-session--create :name "control"))
        (state 'available)
        (acquired 0)
        (requested 0)
        (scheduled 0))
    (setf (niwa-session-save-path session) "/session/")
    (cl-letf (((symbol-function 'niwa-session-durability-lease-state)
               (lambda (&rest _) state))
              ((symbol-function 'niwa-session-control-transfer-acquire)
               (lambda (&rest _) (cl-incf acquired)))
              ((symbol-function 'niwa-view-control-transfer-request)
               (lambda (&rest _) (cl-incf requested)))
              ((symbol-function 'niwa-view--full-rerender)
               (lambda (&rest _) (error "Taking control renders synchronously")))
              ((symbol-function 'niwa-view-rerender)
               (lambda (&rest _) (cl-incf scheduled))))
      (test-niwa-view-control-transfer--with-pair session t
        (call-interactively
         (lookup-key niwa-view--control-transfer-request-map (kbd "r")))
        (setq state 'expired)
        (call-interactively
         (lookup-key niwa-view--control-transfer-request-map (kbd "r")))
        (setq state 'foreign)
        (niwa-take-control))
      (should (= 2 acquired))
      (should (= 1 requested))
      ;; Each acquisition schedules the view's rebuild rather than blocking.
      (should (= 2 scheduled))
      (test-niwa-view-control-transfer--with-pair session nil
        (should-error (niwa-take-control) :type 'user-error)))))

(niwa-deftest niwa-release-control ()
  ,test
  (test)
  :doc "publishes before release and refuses while work remains"
  (let ((session (niwa-session--create :name "control"))
        (blocker "a live execution")
        (saved 0)
        (released 0)
        (stopped 0)
        reason)
    (setf (niwa-session-save-path session) "/session/")
    (cl-letf (((symbol-function
                'niwa-session-control-transfer-drain-blocker)
               (lambda (&rest _) blocker))
              ((symbol-function 'niwa-session-artifacts-save)
               (lambda (&rest _) (cl-incf saved)))
              ((symbol-function 'niwa-execution-stop-services)
               (lambda (owner)
                 (should (eq owner session))
                 (should (= 1 saved))
                 (should (= 0 released))
                 (cl-incf stopped)))
              ((symbol-function 'niwa-session-durability-lease-release)
               (lambda (&rest _) (cl-incf released)))
              ((symbol-function
                'niwa-session-persistence-apply-read-only-mode)
               (lambda (_buffer &optional text) (setq reason text))))
      (test-niwa-view-control-transfer--with-pair session nil
        (should-error (niwa-release-control) :type 'user-error)
        (should (= 0 stopped))
        (setq blocker nil)
        (niwa-test--with-captured-messages nil
                                              (niwa-release-control)))
      (should (= 1 saved))
      (should (= 1 stopped))
      (should (= 1 released))
      (should (stringp reason))
      (should-not (string-match-p "another host" reason))
      (test-niwa-view-control-transfer--with-pair session t
        (should-error (niwa-release-control) :type 'user-error)))))

(provide 'test-niwa-view-control-transfer)
;;; test-niwa-view-control-transfer.el ends here
