;;; test-mevedel-view-control-transfer.el -- View transfer UI tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests control-transfer polling, presentation, commands, and lifecycle.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-session-artifacts)
(require 'mevedel-session-codec)
(require 'mevedel-session-control-transfer)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-view-control-transfer)
(require 'mevedel-view-render)

(defmacro test-mevedel-view-control-transfer--with-pair
    (session read-only &rest body)
  "Run BODY in a view paired to data holding SESSION and READ-ONLY mode."
  (declare (indent 2))
  `(let ((data (generate-new-buffer " *mevedel-transfer-data*"))
         (view (generate-new-buffer " *mevedel-transfer-view*")))
     (unwind-protect
         (progn
           (with-current-buffer data
             (setq-local mevedel--session ,session)
             (setq-local mevedel-session--read-only-mode ,read-only))
           (with-current-buffer view
             (setq-local mevedel--data-buffer data)
             (setq-local mevedel-view--agent-transcript-p nil)
             (setq-local mevedel-view--control-transfer-rebuild-function
                         #'ignore)
             ,@body))
       (when (buffer-live-p view)
         (with-current-buffer view
           (mevedel-view-control-transfer-teardown)))
       (dolist (buffer (list data view))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))))))

(mevedel-deftest mevedel-view--control-transfer-body ()
  ,test
  (test)
  :doc "frames transfer decisions with their advertised key actions"
  (let* ((descriptor
          '(:title "Laptop requests control"
                   :detail "Session must drain first"
                   :attention t
                   :keys (("g" . "grant") ("k" . "keep"))))
         (body (mevedel-view--control-transfer-body descriptor)))
    (should (string-match-p "Laptop requests control" body))
    (should (string-match-p "Session must drain first" body))
    (should
     (cl-loop
      for index from 0 below (length body)
      thereis (let ((face (get-text-property index 'font-lock-face body)))
                (member 'help-key-binding
                        (if (proper-list-p face) face (list face))))))))

(mevedel-deftest mevedel-view--control-transfer-poll-seconds ()
  ,test
  (test)
  :doc "uses local, remote, and active transfer cadences"
  (let ((mevedel-view-control-transfer-poll-seconds 5)
        (mevedel-view-control-transfer-remote-poll-seconds 30)
        (mevedel-view-control-transfer-active-poll-seconds 2)
        (local (mevedel-session--create
                :name "local" :working-directory "/srv/project/"))
        (remote (mevedel-session--create
                 :name "remote"
                 :working-directory "/ssh:user@host:/srv/project/")))
    (should (= 5 (mevedel-view--control-transfer-poll-seconds nil)))
    (should (= 5 (mevedel-view--control-transfer-poll-seconds local)))
    (should (= 30 (mevedel-view--control-transfer-poll-seconds remote)))
    (dolist (state '(requested quiescing))
      (setf (mevedel-session-control-transfer remote) (list :state state))
      (should (= 2 (mevedel-view--control-transfer-poll-seconds remote))))
    (setf (mevedel-session-control-transfer remote) '(:state acquired))
    (should (= 30 (mevedel-view--control-transfer-poll-seconds remote)))))

(mevedel-deftest mevedel-view-control-transfer-current-descriptor ()
  ,test
  (test)
  :doc "reads authority from data and returns the complete interaction row"
  (let ((session (mevedel-session--create :name "control"))
        read-only-args)
    (cl-letf (((symbol-function
                'mevedel-session-control-transfer-descriptor)
               (lambda (_session read-only-p)
                 (push read-only-p read-only-args)
                 '(:kind request :action grant :title "Transfer"))))
      (test-mevedel-view-control-transfer--with-pair session t
        (let ((descriptor
               (mevedel-view-control-transfer-current-descriptor)))
          (should (eq 'control-transfer (plist-get descriptor :id)))
          (should (eq mevedel-view--control-transfer-map
                      (plist-get descriptor :keymap)))
          (should (string-match-p "Transfer" (plist-get descriptor :body)))))
      (test-mevedel-view-control-transfer--with-pair session nil
        (mevedel-view-control-transfer-current-descriptor)))
    (should (equal '(nil t) read-only-args)))

  :doc "binds only the actions displayed for each transfer side"
  (progn
    (should (eq (lookup-key mevedel-view--control-transfer-map (kbd "g"))
                #'mevedel-view-control-transfer-grant))
    (should (eq (lookup-key mevedel-view--control-transfer-map (kbd "k"))
                #'mevedel-view-control-transfer-keep))
    (should
     (eq (lookup-key mevedel-view--control-transfer-request-map (kbd "r"))
         #'mevedel-view-control-transfer-request))
    (dolist (map (list mevedel-view--control-transfer-map
                       mevedel-view--control-transfer-request-map
                       mevedel-view--control-transfer-status-map))
      (dolist (key '("<mouse-1>" "<mouse-2>" "<mouse-3>" "RET" "<return>"))
        (should-not (lookup-key map (kbd key)))))
    (should-not (lookup-key mevedel-view--control-transfer-map (kbd "r")))
    (should-not
     (lookup-key mevedel-view--control-transfer-status-map (kbd "g")))
    (should-not
     (lookup-key mevedel-view--control-transfer-status-map (kbd "r")))))

(mevedel-deftest mevedel-view-control-transfer-initialize ()
  ,test
  (test)
  :doc "registers the root, presentation, observer, drain, and poll timer"
  (dolist (symbol '(mevedel-view--control-transfer-refresh
                    mevedel-view--control-transfer-body
                    mevedel-take-control))
    (should (equal "mevedel-view-control-transfer"
                   (file-name-base (or (symbol-file symbol 'defun) "")))))
  (let ((session (mevedel-session--create
                  :name "control" :session-id "control")))
    (test-mevedel-view-control-transfer--with-pair session nil
      (let ((rebuild #'ignore)
            (drain (lambda () t)))
        (mevedel-view-control-transfer-initialize rebuild drain)
        (should (eq rebuild
                    mevedel-view--control-transfer-rebuild-function))
        (should (eq data
                    (mevedel-session-control-transfer-root-buffer session)))
        (should (eq view
                    (mevedel-session-control-transfer-presentation-buffer
                     session)))
        (should (memq drain
                      (mevedel-session-control-transfer-drains session)))
        (should (gethash session
                         mevedel-session-control-transfer--observers))
        (should (timerp mevedel-view--control-transfer-timer))))))

(mevedel-deftest mevedel-view-control-transfer-teardown ()
  ,test
  (test)
  :doc "removes every registration and cancels the owned timer"
  (let ((session (mevedel-session--create
                  :name "control" :session-id "control")))
    (test-mevedel-view-control-transfer--with-pair session nil
      (let ((drain (lambda () t)))
        (mevedel-view-control-transfer-initialize #'ignore drain)
        (mevedel-view-control-transfer-teardown)
        (should-not mevedel-view--control-transfer-rebuild-function)
        (should-not mevedel-view--control-transfer-timer)
        (should-not
         (mevedel-session-control-transfer-root-buffer session))
        (should-not
         (mevedel-session-control-transfer-presentation-buffer session))
        (should-not
         (mevedel-session-control-transfer-drains session))
        (should-not
         (gethash session mevedel-session-control-transfer--observers))))))

(mevedel-deftest mevedel-take-control ()
  ,test
  (test)
  :doc "acquires free authority, requests held authority, and rejects owners"
  (let ((session (mevedel-session--create :name "control"))
        (state 'available)
        (acquired 0)
        (requested 0))
    (setf (mevedel-session-save-path session) "/session/")
    (cl-letf (((symbol-function 'mevedel-session-durability-lease-state)
               (lambda (&rest _) state))
              ((symbol-function 'mevedel-session-control-transfer-acquire)
               (lambda (&rest _) (cl-incf acquired)))
              ((symbol-function 'mevedel-view-control-transfer-request)
               (lambda (&rest _) (cl-incf requested)))
              ((symbol-function 'mevedel-view--full-rerender) #'ignore))
      (test-mevedel-view-control-transfer--with-pair session t
        (mevedel-take-control)
        (setq state 'expired)
        (mevedel-take-control)
        (setq state 'foreign)
        (mevedel-take-control))
      (should (= 2 acquired))
      (should (= 1 requested))
      (test-mevedel-view-control-transfer--with-pair session nil
        (should-error (mevedel-take-control) :type 'user-error)))))

(mevedel-deftest mevedel-release-control ()
  ,test
  (test)
  :doc "publishes before release and refuses while work remains"
  (let ((session (mevedel-session--create :name "control"))
        (blocker "a live execution")
        (saved 0)
        (released 0)
        reason)
    (setf (mevedel-session-save-path session) "/session/")
    (cl-letf (((symbol-function
                'mevedel-session-control-transfer-drain-blocker)
               (lambda (&rest _) blocker))
              ((symbol-function 'mevedel-session-artifacts-save)
               (lambda (&rest _) (cl-incf saved)))
              ((symbol-function 'mevedel-session-durability-lease-release)
               (lambda (&rest _) (cl-incf released)))
              ((symbol-function
                'mevedel-session-persistence-apply-read-only-mode)
               (lambda (_buffer &optional text) (setq reason text))))
      (test-mevedel-view-control-transfer--with-pair session nil
        (should-error (mevedel-release-control) :type 'user-error)
        (setq blocker nil)
        (mevedel-test--with-captured-messages nil
                                              (mevedel-release-control)))
      (should (= 1 saved))
      (should (= 1 released))
      (should (stringp reason))
      (should-not (string-match-p "another host" reason))
      (test-mevedel-view-control-transfer--with-pair session t
        (should-error (mevedel-release-control) :type 'user-error)))))

(provide 'test-mevedel-view-control-transfer)
;;; test-mevedel-view-control-transfer.el ends here
