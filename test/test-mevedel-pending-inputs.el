;;; test-mevedel-pending-inputs.el --- Pending Inputs cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-cockpit)
(require 'mevedel-pending-inputs)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-workspace)
(require 'gptel-request)
(require 'tabulated-list)

(defvar mevedel-plugin-extra-roots)

(defmacro mevedel-pending-inputs-test--with-session (&rest body)
  "Run BODY with a paired view, data buffer, and pending-input session."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((workspace
             (mevedel-workspace--create
              :type 'test :id "pending-inputs"
              :root "/tmp/pending-inputs"
              :name "pending-inputs"))
            (session (mevedel-session-create "main" workspace)))
       (with-current-buffer data-buf
         (setq-local mevedel--session session
                     mevedel--workspace workspace
                     mevedel--view-buffer view-buf))
       (with-current-buffer view-buf
         (setq-local mevedel--session session))
       (unwind-protect
           (progn ,@body)
         (when-let* ((cockpit
                      (get-buffer mevedel-pending-inputs-buffer-name)))
           (kill-buffer cockpit))))))

(defun mevedel-pending-inputs-test--replace-composer (view text)
  "Replace VIEW's editable composer with TEXT."
  (with-current-buffer view
    (let ((inhibit-read-only t))
      (delete-region (mevedel-view--input-start) (point-max))
      (goto-char (mevedel-view--input-start))
      (insert text))))

(mevedel-deftest mevedel-pending-inputs-open ()
  ,test
  (test)

  :doc "opens paused with both categories projected in category FIFO order"
  (mevedel-pending-inputs-test--with-session
    (let ((steering
           (mevedel-session-enqueue-pending-input
            session 'steering '(:input "steer now")))
          (follow-up
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "later turn"))))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (should (mevedel-session-pending-input-paused session))
          (with-current-buffer cockpit
            (should (eq major-mode 'mevedel-pending-inputs-mode))
            (should (= 2 (length tabulated-list-entries)))
            (should
             (equal
              (list (plist-get steering :id) (plist-get follow-up :id))
              (mapcar #'car tabulated-list-entries)))
            (should (string-match-p
                     "delivery paused"
                     (mevedel-pending-inputs--header
                      (mevedel-cockpit-surface-items)
                      mevedel-cockpit--context)))
            (should (eq (lookup-key (current-local-map) (kbd "RET"))
                        #'mevedel-pending-inputs-edit))
            (should (eq (lookup-key (current-local-map) (kbd "e"))
                        #'mevedel-pending-inputs-edit)))
          (with-current-buffer view-buf
            (should
             (string-match-p
              "Pending-input delivery paused"
              (buffer-substring-no-properties
               (point-min) (mevedel-view--input-start)))))))))

  :doc "refresh reads live session state without a shadow Apply step"
  (mevedel-pending-inputs-test--with-session
    (let ((first
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "first"))))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (mevedel-session-enqueue-pending-input
           session 'follow-up '(:input "second"))
          (with-current-buffer cockpit
            (mevedel-cockpit-surface-refresh
             (plist-get first :id))
            (should (= 2 (length tabulated-list-entries)))
            (should (equal (plist-get first :id)
                           (tabulated-list-get-id))))))))

  :doc "rejects opening an empty Pending Inputs cockpit"
  (mevedel-pending-inputs-test--with-session
    (with-current-buffer view-buf
      (should-error (mevedel-pending-inputs-open) :type 'user-error))))

(mevedel-deftest mevedel-pending-inputs-edit ()
  ,test
  (test)
  :doc "suspends an exact draft and loads the selected entry in the composer"
  (mevedel-pending-inputs-test--with-session
    (let* ((grant "/tmp/draft-grant")
           (entry
            (mevedel-session-enqueue-pending-input
             session 'follow-up
             '(:input "queued\nmessage"
               :dropped-file-grants ("/tmp/entry-grant"))))
           draft point-offset cockpit)
      (setf (mevedel-session-dropped-file-grants session) (list grant))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert (propertize "> draft\nsecond" 'draft-property 'kept))
        (goto-char (+ (mevedel-view--input-start) 3))
        (setq draft
              (buffer-substring
               (mevedel-view--input-start) (point-max))
              point-offset (- (point) (mevedel-view--input-start))))
      (save-window-excursion
        (setq cockpit
              (with-current-buffer view-buf
                (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get entry :id))
          (mevedel-pending-inputs-edit))
        (with-current-buffer view-buf
          (should (equal "queued\nmessage"
                         (mevedel-view--input-text)))
          (should (string-match-p
                   "Editing follow-up 1"
                   (mevedel-view--input-prompt-string)))
          (should-error (mevedel-view-send) :type 'user-error)
          (should-error (mevedel-view-send-follow-up) :type 'user-error)
          (mevedel-pending-inputs-cancel-edit))
        (with-current-buffer view-buf
          (should
           (equal-including-properties
            draft
            (buffer-substring
             (mevedel-view--input-start) (point-max))))
          (should (= point-offset
                     (- (point) (mevedel-view--input-start))))
          (should (eq 'kept
                      (get-text-property
                       (mevedel-view--input-start) 'draft-property)))))
      (should (equal (list grant)
                     (mevedel-session-dropped-file-grants session)))
      (should (eq entry
                  (car (mevedel-session-pending-follow-ups session)))))))

(mevedel-deftest mevedel-pending-inputs-save-edit ()
  ,test
  (test)

  :doc "replaces a follow-up in place and restores the suspended draft"
  (mevedel-pending-inputs-test--with-session
    (let* ((first
            (mevedel-session-enqueue-pending-input
             session 'follow-up '(:input "first")))
           (second
            (mevedel-session-enqueue-pending-input
             session 'follow-up '(:input "second")))
           cockpit)
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert "> retained\nmultiline"))
      (save-window-excursion
        (setq cockpit
              (with-current-buffer view-buf
                (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get first :id))
          (mevedel-pending-inputs-edit))
        (mevedel-pending-inputs-test--replace-composer
         view-buf "edited first")
        (with-current-buffer view-buf
          (mevedel-pending-inputs-save-edit))
        (with-current-buffer view-buf
          (should (equal "> retained\nmultiline"
                         (mevedel-view--input-text))))
        (let ((entries (mevedel-session-pending-follow-ups session)))
          (should
           (equal '("edited first" "second")
                  (mapcar (lambda (entry) (plist-get entry :input))
                          entries)))
          (should (equal (list (plist-get first :id)
                               (plist-get second :id))
                         (mapcar
                          (lambda (entry) (plist-get entry :id))
                          entries))))
        (should (eq cockpit (window-buffer (selected-window)))))))

  :doc "invalid follow-up text leaves the original and edit available"
  (mevedel-pending-inputs-test--with-session
    (let* ((entry
            (mevedel-session-enqueue-pending-input
             session 'follow-up '(:input "original")))
           cockpit)
      (save-window-excursion
        (setq cockpit
              (with-current-buffer view-buf
                (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get entry :id))
          (mevedel-pending-inputs-edit))
        (mevedel-pending-inputs-test--replace-composer view-buf "/review")
        (with-current-buffer view-buf
          (mevedel-pending-inputs-save-edit)
          (should mevedel-view--pending-input-edit)
          (should (equal "/review" (mevedel-view--input-text))))
        (should (eq entry
                    (car
                     (mevedel-session-pending-follow-ups session)))))))

  :doc "valid steering is fully prepared and replaces the original identity"
  (mevedel-pending-inputs-test--with-session
    (let* ((fsm (gptel-make-fsm :state 'TOOL))
           (request
            (mevedel-request--create
             :id "edit-steering" :session session :fsm fsm))
           (entry
            (mevedel-session-enqueue-pending-input
             session 'steering
             '(:input "old steering" :model-input "old steering"
               :request-id "edit-steering")))
           cockpit)
      (with-current-buffer data-buf
        (setq-local mevedel--current-request request))
      (save-window-excursion
        (setq cockpit
              (with-current-buffer view-buf
                (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get entry :id))
          (mevedel-pending-inputs-edit))
        (mevedel-pending-inputs-test--replace-composer
         view-buf "new steering")
        (with-current-buffer view-buf
          (cl-letf (((symbol-function
                      'mevedel-agent-control-root-waiting-p)
                     (lambda (_session) nil)))
            (mevedel-pending-inputs-save-edit)))
        (let ((replacement
               (car (mevedel-session-pending-steering session))))
          (should (equal (plist-get entry :id)
                         (plist-get replacement :id)))
          (should (equal "new steering"
                         (plist-get replacement :input)))
          (should (plist-get replacement :submission))))))

  :doc "a stale steering turn leaves the original and editing text"
  (mevedel-pending-inputs-test--with-session
    (let* ((entry
            (mevedel-session-enqueue-pending-input
             session 'steering
             '(:input "old steering" :request-id "dead-request")))
           cockpit)
      (save-window-excursion
        (setq cockpit
              (with-current-buffer view-buf
                (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get entry :id))
          (mevedel-pending-inputs-edit))
        (mevedel-pending-inputs-test--replace-composer
         view-buf "still editing")
        (with-current-buffer view-buf
          (mevedel-pending-inputs-save-edit)
          (should mevedel-view--pending-input-edit)
          (should (equal "still editing" (mevedel-view--input-text))))
        (should (eq entry
                    (car
                     (mevedel-session-pending-steering session))))))))

(mevedel-deftest mevedel-pending-inputs-cancel-edit ()
  ,test
  (test)
  :doc "cancel keeps the original entry and returns to the cockpit"
  (mevedel-pending-inputs-test--with-session
    (let* ((entry
            (mevedel-session-enqueue-pending-input
             session 'follow-up '(:input "unchanged")))
           cockpit)
      (save-window-excursion
        (setq cockpit
              (with-current-buffer view-buf
                (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get entry :id))
          (mevedel-pending-inputs-edit))
        (mevedel-pending-inputs-test--replace-composer
         view-buf "discard me")
        (with-current-buffer view-buf
          (mevedel-pending-inputs-cancel-edit)
          (should-not mevedel-view--pending-input-edit))
        (should (eq entry
                    (car
                     (mevedel-session-pending-follow-ups session))))
        (should (eq cockpit (window-buffer (selected-window))))))))

(mevedel-deftest mevedel-pending-inputs-quit ()
  ,test
  (test)
  :doc "normal close resumes delivery and schedules eligible follow-ups"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "later"))
    (let (quit scheduled)
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (cl-letf
              (((symbol-function 'mevedel-cockpit-quit)
                (lambda (&rest _) (setq quit t)))
               ((symbol-function
                 'mevedel-view--schedule-late-follow-up-drain)
                (lambda () (setq scheduled t))))
            (with-current-buffer cockpit
              (mevedel-pending-inputs-quit)))))
      (should quit)
      (should scheduled)
      (should-not (mevedel-session-pending-input-paused session))))

  :doc "closing resumes a WAIT held at the pending-input boundary"
  (mevedel-pending-inputs-test--with-session
    (let* ((fsm
            (gptel-make-fsm
             :state 'WAIT
             :info '(:mevedel-pending-input-hold t)))
           (request
            (mevedel-request--create
             :id "held-request" :session session :fsm fsm))
           transition)
      (mevedel-session-enqueue-pending-input
       session 'steering
       '(:input "held" :request-id "held-request"))
      (with-current-buffer data-buf
        (setq-local mevedel--current-request request))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (cl-letf
              (((symbol-function 'gptel--fsm-transition)
                (lambda (_fsm state) (setq transition state)))
               ((symbol-function 'mevedel-cockpit-quit) #'ignore)
               ((symbol-function
                 'mevedel-view--schedule-late-follow-up-drain)
                #'ignore))
            (with-current-buffer cockpit
              (mevedel-pending-inputs-quit)))))
      (should (eq transition 'WAIT))
      (should-not (mevedel-session-pending-input-paused session)))))

(provide 'test-mevedel-pending-inputs)
;;; test-mevedel-pending-inputs.el ends here
