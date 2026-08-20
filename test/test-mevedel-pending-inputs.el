;;; test-mevedel-pending-inputs.el --- Pending Inputs cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-resource)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-cockpit)
(require 'mevedel-file-state)
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
              :type 'file :id "pending-inputs"
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

  :doc "rejects opening an empty Pending Inputs cockpit"
  (mevedel-pending-inputs-test--with-session
    (with-current-buffer view-buf
      (should-error (mevedel-pending-inputs-open) :type 'user-error))))

(mevedel-deftest mevedel-pending-inputs-refresh ()
  ,test
  (test)
  :doc "reads live session state without a shadow Apply step"
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
            (mevedel-cockpit-goto-id (plist-get first :id))
            (mevedel-pending-inputs-refresh)
            (should (= 2 (length tabulated-list-entries)))
            (should (equal (plist-get first :id)
                           (tabulated-list-get-id)))))))))

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
          (mevedel-test--with-captured-messages nil
            (mevedel-pending-inputs-cancel-edit)))
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
          (mevedel-test--with-captured-messages nil
            (mevedel-pending-inputs-save-edit)))
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
          (mevedel-test--with-captured-messages nil
            (mevedel-pending-inputs-save-edit))
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
            (mevedel-test--with-captured-messages nil
              (mevedel-pending-inputs-save-edit))))
        (let ((replacement
               (car (mevedel-session-pending-steering session))))
          (should (equal (plist-get entry :id)
                         (plist-get replacement :id)))
          (should (equal "new steering"
                         (plist-get replacement :input)))
          (should (plist-get replacement :submission))))))

  :doc "failed-turn steering remains review-only after editing"
  (mevedel-pending-inputs-test--with-session
    (let* ((submission
            (mevedel-prompt-submission-create
             :input "old" :display-text "old" :session session
             :state 'reserved))
           (entry
            (mevedel-session-enqueue-pending-input
             session 'steering
             (list :input "old" :model-input "prepared"
                   :request-id "dead" :state 'failed-turn
                   :submission submission)))
           cockpit)
      (mevedel-session-set-pending-input-failure-paused session t)
      (save-window-excursion
        (setq cockpit
              (with-current-buffer view-buf
                (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get entry :id))
          (mevedel-pending-inputs-edit))
        (mevedel-pending-inputs-test--replace-composer
         view-buf "edited for review")
        (with-current-buffer view-buf
          (mevedel-test--with-captured-messages nil
            (mevedel-pending-inputs-save-edit))))
      (let ((replacement
             (car (mevedel-session-pending-steering session))))
        (should (equal "edited for review"
                       (plist-get replacement :input)))
        (should (eq 'failed-turn (plist-get replacement :state)))
        (should-not (plist-member replacement :model-input))
        (should-not (plist-member replacement :submission)))
      (should
       (mevedel-session-pending-input-failure-paused session))))

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
          (mevedel-test--with-captured-messages nil
            (mevedel-pending-inputs-save-edit))
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
          (mevedel-test--with-captured-messages nil
            (mevedel-pending-inputs-cancel-edit))
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
      (should-not (mevedel-session-pending-input-paused session))))

  :doc "closing cannot resume delivery during unresolved failure recovery"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'steering
     '(:input "review" :request-id "dead" :state failed-turn))
    (mevedel-session-set-pending-input-failure-paused session t)
    (let (scheduled)
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (cl-letf
              (((symbol-function 'mevedel-cockpit-quit) #'ignore)
               ((symbol-function
                 'mevedel-view--schedule-late-follow-up-drain)
                (lambda () (setq scheduled t))))
            (with-current-buffer cockpit
              (mevedel-pending-inputs-quit)))))
      (should-not scheduled)
      (should
       (mevedel-session-pending-input-failure-paused session))
      (should-not (mevedel-session-pending-input-paused session)))))

(mevedel-deftest mevedel-pending-inputs-move-up ()
  ,test
  (test)
  :doc "moves only inside the selected category and preserves the draft"
  (mevedel-pending-inputs-test--with-session
    (let* ((first
            (mevedel-session-enqueue-pending-input
             session 'steering '(:input "first")))
           (second
            (mevedel-session-enqueue-pending-input
             session 'steering '(:input "second")))
           (follow-up
            (mevedel-session-enqueue-pending-input
             session 'follow-up '(:input "later"))))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert "> draft"))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get second :id))
            (mevedel-pending-inputs-move-up)
            (should (equal (plist-get second :id)
                           (tabulated-list-get-id))))))
      (should (equal (list second first)
                     (mevedel-session-pending-steering session)))
      (should (equal (list follow-up)
                     (mevedel-session-pending-follow-ups session)))
      (with-current-buffer view-buf
        (should (equal "> draft" (mevedel-view--input-text)))))))

(mevedel-deftest mevedel-pending-inputs-move-down ()
  ,test
  (test)
  :doc "moves down within one category and rejects its lower boundary"
  (mevedel-pending-inputs-test--with-session
    (let ((first
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "first")))
          (second
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "second"))))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get first :id))
            (mevedel-pending-inputs-move-down)
            (should-error (mevedel-pending-inputs-move-down)
                          :type 'user-error))))
      (should (equal (list second first)
                     (mevedel-session-pending-follow-ups session))))))

(mevedel-deftest mevedel-pending-inputs-make-follow-up ()
  ,test
  (test)
  :doc "drops steering preparation and appends a delayed follow-up"
  (mevedel-pending-inputs-test--with-session
    (let* ((context-entries '((:event SessionStart :body "restore me")))
           (submission
            (mevedel-prompt-submission-create
             :input "steer" :display-text "steer" :session session
             :context-entries context-entries :state 'reserved))
           (steering
            (mevedel-session-enqueue-pending-input
             session 'steering
             (list :input "steer" :model-input "prepared"
                   :request-id "request" :submission submission
                   :state 'failed-turn)))
           (older
            (mevedel-session-enqueue-pending-input
             session 'follow-up '(:input "older"))))
      (mevedel-session-set-pending-input-failure-paused session t)
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get steering :id))
            (mevedel-test--with-captured-messages nil
              (mevedel-pending-inputs-make-follow-up)))))
      (should-not (mevedel-session-pending-steering session))
      (let ((converted
             (cadr (mevedel-session-pending-follow-ups session))))
        (should (eq older
                    (car (mevedel-session-pending-follow-ups session))))
        (should (equal (plist-get steering :id)
                       (plist-get converted :id)))
        (should (eq (plist-get converted :category) 'follow-up))
        (should (eq (plist-get converted :state) 'pending))
        (should-not (plist-member converted :request-id))
        (should-not (plist-member converted :model-input))
        (should-not (plist-member converted :submission)))
      (should (equal context-entries
                     (mevedel-session-hook-context-pending session)))
      (should
       (mevedel-session-pending-input-failure-paused session))))

  :doc "quiescing authority leaves steering and its submission reserved"
  (mevedel-pending-inputs-test--with-session
    (let* ((submission
            (mevedel-prompt-submission-create
             :input "steer" :display-text "steer" :session session
             :state 'reserved))
           (steering
            (mevedel-session-enqueue-pending-input
             session 'steering
             (list :input "steer" :submission submission
                   :state 'failed-turn)))
           (before (copy-tree (mevedel-session-pending-steering session))))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (setf (mevedel-session-control-transfer session)
                '(:state quiescing))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get steering :id))
            (should-error (mevedel-pending-inputs-make-follow-up)
                          :type 'user-error))))
      (should (equal before
                     (mevedel-session-pending-steering session)))
      (should-not (mevedel-session-pending-follow-ups session))
      (should (eq 'reserved
                  (mevedel-prompt-submission-state submission))))))

(mevedel-deftest mevedel-pending-inputs-make-steering ()
  ,test
  (test)

  :doc "fully prepares a follow-up and appends it to live steering"
  (mevedel-pending-inputs-test--with-session
    (let* ((fsm (gptel-make-fsm :state 'TOOL))
           (request
            (mevedel-request--create
             :id "convert-live" :session session :fsm fsm))
           (older
            (mevedel-session-enqueue-pending-input
             session 'steering
             '(:input "older steering" :request-id "convert-live")))
           (follow-up
            (mevedel-session-enqueue-pending-input
             session 'follow-up '(:input "convert me"))))
      (with-current-buffer data-buf
        (setq-local mevedel--current-request request))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get follow-up :id))
            (mevedel-pending-inputs-make-steering))))
      (should-not (mevedel-session-pending-follow-ups session))
      (let ((converted (cadr (mevedel-session-pending-steering session))))
        (should (eq older (car (mevedel-session-pending-steering session))))
        (should (equal (plist-get follow-up :id)
                       (plist-get converted :id)))
        (should (eq (plist-get converted :category) 'steering))
        (should (equal "convert-live"
                       (plist-get converted :request-id)))
        (should (plist-get converted :submission)))))

  :doc "retains queued file grants and restores the existing draft grants"
  (mevedel-pending-inputs-test--with-session
    (let* ((fsm (gptel-make-fsm :state 'TOOL))
           (request
            (mevedel-request--create
             :id "convert-grant" :session session :fsm fsm))
           (file (make-temp-file "mevedel-convert-grant-"))
           (draft-file (make-temp-file "mevedel-draft-grant-"))
           (token (format "@file:%s" file))
           (input
            (propertize
             token
             'mevedel-mention-binding
             (list :kind 'file :token token :path file)))
           (follow-up
            (mevedel-session-enqueue-pending-input
             session 'follow-up
             (list :input input :dropped-file-grants (list file)))))
      (unwind-protect
          (progn
            (setf (mevedel-session-dropped-file-grants session)
                  (list draft-file))
            (with-current-buffer data-buf
              (setq-local mevedel--current-request request))
            (save-window-excursion
              (let ((cockpit
                     (with-current-buffer view-buf
                       (mevedel-pending-inputs-open))))
                (with-current-buffer cockpit
                  (mevedel-cockpit-goto-id (plist-get follow-up :id))
                  (mevedel-pending-inputs-make-steering))))
            (let ((converted
                   (car (mevedel-session-pending-steering session))))
              (should (equal (list file)
                             (plist-get converted :dropped-file-grants))))
            (should
             (equal (list draft-file)
                    (mevedel-session-dropped-file-grants session))))
        (delete-file file)
        (delete-file draft-file))))

  :doc "a turn-ending preparation race leaves the follow-up unchanged"
  (mevedel-pending-inputs-test--with-session
    (let* ((fsm (gptel-make-fsm :state 'TOOL))
           (request
            (mevedel-request--create
             :id "convert-race" :session session :fsm fsm))
           (follow-up
            (mevedel-session-enqueue-pending-input
             session 'follow-up '(:input "stay follow-up")))
           dispatch)
      (with-current-buffer data-buf
        (setq-local mevedel--current-request request))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (cl-letf
              (((symbol-function 'mevedel-view--submit-planned-input)
                (lambda (_input _before _blocked callback &rest _)
                  (setq dispatch callback))))
            (with-current-buffer cockpit
              (mevedel-cockpit-goto-id (plist-get follow-up :id))
              (mevedel-pending-inputs-make-steering)))
          (with-current-buffer data-buf
            (setq mevedel--current-request nil))
          (with-current-buffer view-buf
            ;; The race is reported to the user; the assertions below own
            ;; the durable outcome it echoes.
            (mevedel-test--with-captured-messages nil
              (funcall
               dispatch
               (mevedel-prompt-submission-create
                :input "stay follow-up" :display-text "stay follow-up"
                :session session
                :outcome
                '(:model-input "stay follow-up"
                  :transcript-input "stay follow-up")))))))
      (should (equal (list follow-up)
                     (mevedel-session-pending-follow-ups session)))
      (should-not (mevedel-session-pending-steering session)))))

(mevedel-deftest mevedel-pending-inputs-mark-delete ()
  ,test
  (test)
  :doc "marks the selected live row and advances"
  (mevedel-pending-inputs-test--with-session
    (let ((entry
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "delete me"))))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get entry :id))
            (mevedel-pending-inputs-mark-delete)
            (should (member (plist-get entry :id)
                            mevedel-pending-inputs--marked-ids))))))))

(mevedel-deftest mevedel-pending-inputs-unmark ()
  ,test
  (test)
  :doc "removes the selected row's deletion mark"
  (mevedel-pending-inputs-test--with-session
    (let ((entry
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "keep me"))))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get entry :id))
            (mevedel-pending-inputs-mark-delete)
            (mevedel-cockpit-goto-id (plist-get entry :id))
            (mevedel-pending-inputs-unmark)
            (should-not mevedel-pending-inputs--marked-ids)))))))

(mevedel-deftest mevedel-pending-inputs-execute-deletions ()
  ,test
  (test)
  :doc "confirms category counts and deletes only marked entries"
  (mevedel-pending-inputs-test--with-session
    (let ((steering
           (mevedel-session-enqueue-pending-input
            session 'steering '(:input "delete steering")))
          (keep
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "keep follow-up")))
          (follow-up
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "delete follow-up")))
          prompt)
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (setq mevedel-pending-inputs--marked-ids
                  (list (plist-get steering :id)
                        (plist-get follow-up :id)))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (text) (setq prompt text) t)))
              (mevedel-pending-inputs-execute-deletions)))))
      (should (string-match-p "1 steering and 1 follow-up" prompt))
      (should-not (mevedel-session-pending-steering session))
      (should (equal (list keep)
                     (mevedel-session-pending-follow-ups session)))))

  :doc "quiescing authority preserves marked entries and submissions"
  (mevedel-pending-inputs-test--with-session
    (let* ((submission
            (mevedel-prompt-submission-create
             :input "delete" :display-text "delete" :session session
             :state 'reserved))
           (entry
            (mevedel-session-enqueue-pending-input
             session 'steering
             (list :input "delete" :submission submission)))
           (before (copy-tree (mevedel-session-pending-steering session))))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (setq mevedel-pending-inputs--marked-ids
                  (list (plist-get entry :id)))
            (setf (mevedel-session-control-transfer session)
                  '(:state quiescing))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t)))
              (should-error
               (mevedel-pending-inputs-execute-deletions)
               :type 'user-error)))))
      (should (equal before
                     (mevedel-session-pending-steering session)))
      (should (eq 'reserved
                  (mevedel-prompt-submission-state submission))))))

(mevedel-deftest mevedel-pending-inputs-resume-after-failure ()
  ,test
  (test)

  :doc "rejects unresolved steering and accepts explicit conversion recovery"
  (mevedel-pending-inputs-test--with-session
    (let ((failed
           (mevedel-session-enqueue-pending-input
            session 'steering
            '(:input "review me" :request-id "dead"
              :state failed-turn)))
          (later
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "later"))))
      (mevedel-session-set-pending-input-failure-paused session t)
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer view-buf
            (should
             (string-match-p
              "delivery stopped after turn failure"
              (buffer-string))))
          (with-current-buffer cockpit
            (should (string-match-p
                     "failure recovery required"
                     (mevedel-pending-inputs--header
                      (mevedel-cockpit-surface-items)
                      mevedel-cockpit--context)))
            (mevedel-cockpit-goto-id (plist-get failed :id))
            (should-error
             (mevedel-test--with-captured-messages nil
               (mevedel-pending-inputs-resume-after-failure))
             :type 'user-error)
            (mevedel-test--with-captured-messages nil
              (mevedel-pending-inputs-make-follow-up))
            (mevedel-test--with-captured-messages nil
              (mevedel-pending-inputs-resume-after-failure)))))
      (should-not
       (mevedel-session-pending-input-failure-paused session))
      (let ((entries (mevedel-session-pending-follow-ups session)))
        (should (eq later (car entries)))
        (should (equal (plist-get failed :id)
                       (plist-get (cadr entries) :id))))))

  :doc "deletion recovery can resume from an otherwise empty cockpit"
  (mevedel-pending-inputs-test--with-session
    (let ((failed
           (mevedel-session-enqueue-pending-input
            session 'steering
            '(:input "delete me" :request-id "dead"
              :state failed-turn))))
      (mevedel-session-set-pending-input-failure-paused session t)
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (setq mevedel-pending-inputs--marked-ids
                  (list (plist-get failed :id)))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t)))
              (mevedel-pending-inputs-execute-deletions))
            (should-not (mevedel-cockpit-surface-items))
            (mevedel-test--with-captured-messages nil
              (mevedel-pending-inputs-resume-after-failure)))))
      (should-not
       (mevedel-session-pending-input-failure-paused session))))

  :doc "quiescing authority keeps the failure pause latched"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-set-pending-input-failure-paused session t)
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "later"))
    (save-window-excursion
      (let ((cockpit
             (with-current-buffer view-buf
               (mevedel-pending-inputs-open))))
        (setf (mevedel-session-control-transfer session)
              '(:state quiescing))
        (with-current-buffer cockpit
          (should-error
           (mevedel-test--with-captured-messages nil
             (mevedel-pending-inputs-resume-after-failure))
           :type 'user-error))))
    (should (mevedel-session-pending-input-failure-paused session))))

(mevedel-deftest mevedel-pending-inputs-clear ()
  ,test
  (test)
  :doc "confirms separate counts and clears both live categories"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'steering '(:input "steering"))
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "follow-up"))
    (let (prompt)
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (text) (setq prompt text) t)))
          (mevedel-test--with-captured-messages nil
            (mevedel-pending-inputs-clear))))
      (should (string-match-p "1 steering and 1 follow-up" prompt))
      (should-not (mevedel-session-pending-steering session))
      (should-not (mevedel-session-pending-follow-ups session))))

  :doc "quiescing authority preserves both queues"
  (mevedel-pending-inputs-test--with-session
    (let ((steering
           (mevedel-session-enqueue-pending-input
            session 'steering '(:input "steer")))
          (follow-up
           (mevedel-session-enqueue-pending-input
            session 'follow-up '(:input "later"))))
      (setf (mevedel-session-control-transfer session) '(:state quiescing))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) t)))
          (should-error
           (mevedel-test--with-captured-messages nil
             (mevedel-pending-inputs-clear))
           :type 'user-error)))
      (should (equal (list steering)
                     (mevedel-session-pending-steering session)))
      (should (equal (list follow-up)
                     (mevedel-session-pending-follow-ups session))))))


;;
;;; Queue and delivery helpers

(mevedel-deftest mevedel-view--queue-follow-up
  (:quiet t)
  ,test
  (test)
  :doc "stores directive scope on queued follow-ups"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'file :id "queue-scope" :root "/tmp"
                       :name "queue-scope"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session
                    mevedel-view--composer-scope
                    '(:directive-id "directive-1" :action discuss
                      :attempt-index 2))
        (cl-letf (((symbol-function 'mevedel-view--interaction-rebuild)
                   #'ignore)
                  ((symbol-function
                    'mevedel-view--schedule-late-follow-up-drain)
                   #'ignore))
          (mevedel-view--queue-follow-up "follow up"))
        (let ((scope
               (plist-get (car (mevedel-session-pending-follow-ups session))
                          :scope)))
          (should (equal "directive-1" (plist-get scope :directive-id)))
          (should (eq 'discuss (plist-get scope :action)))
          (should (= 2 (plist-get scope :attempt-index))))))))

(mevedel-deftest mevedel-view--pending-input-text ()
  ,test
  (test)
  :doc "returns queued input and defaults a missing value to empty text"
  (should (equal "queued" (mevedel-view--pending-input-text
                            '(:input "queued"))))
  (should (equal "" (mevedel-view--pending-input-text nil))))

(mevedel-deftest mevedel-view--pending-input-category-body ()
  ,test
  (test)
  :doc "shows three compact previews and a remaining count"
  (let ((body
         (mevedel-view--pending-input-category-body
          "Steering"
          (mapcar (lambda (n)
                    (list :input
                          (format "message %d\nwith extra whitespace" n)))
                  '(1 2 3 4 5)))))
    (dolist (n '(1 2 3))
      (should (string-match-p (format "message %d with extra" n) body)))
    (should-not (string-match-p "message 4" body))
    (should (string-match-p "2 more" body))))

(mevedel-deftest mevedel-view--steering-request-context-supported-p ()
  ,test
  (test)
  :doc "allows bookkeeping-only skill context and rejects request policy"
  (should
   (mevedel-view--steering-request-context-supported-p
    '(:permission-rules nil :hook-rules nil :invoked-skills (alpha))))
  (dolist (context '((:permission-rules (rule))
                     (:hook-rules (rule))
                     (:model model)
                     (:effort high)))
    (should-not
     (mevedel-view--steering-request-context-supported-p context)))
  (should-not
   (mevedel-view--steering-request-context-supported-p
    '(:future-policy nil))))

(mevedel-deftest mevedel-view--follow-up-auto-drain-blocked-p ()
  ,test
  (test)
  :doc "blocks fallback drainage for approval and Goal handoff ownership"
  (let ((session (mevedel-session--create
                  :authority-mode 'pid-lock
                  :name "main" :pending-plan-approval 'plan)))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session))
    (setf (mevedel-session-pending-plan-approval session) nil)
    (should-not
     (mevedel-view--follow-up-auto-drain-blocked-p session)))
  (let ((here
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "here"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "here-goal"
             :selection (:location here :execution goal)))))
        (source
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "source"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "target-goal"
             :selection (:location worktree :execution goal)))))
        (target
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "target"
          :plan-metadata '(:implementation-goal-id "target-goal"))))
    (should (mevedel-view--follow-up-auto-drain-blocked-p here))
    (should (mevedel-view--follow-up-auto-drain-blocked-p source))
    (should (mevedel-view--follow-up-auto-drain-blocked-p target)))
  (let* ((goal (mevedel-goal--create :id "goal" :status 'paused))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "paused" :goal goal
           :pending-follow-ups
           '((:input "held" :queued-at-goal-id "goal")))))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session))
    (setf (mevedel-goal-status goal) 'active)
    (should-not
     (mevedel-view--follow-up-auto-drain-blocked-p session))
    (dolist (status '(blocked budget-limited))
      (setf (mevedel-goal-status goal) status)
      (should (mevedel-view--follow-up-auto-drain-blocked-p session))))
  (let ((session (mevedel-session--create
                  :authority-mode 'pid-lock
                  :name "failed" :pending-input-failure-paused t)))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session)))
  :doc "holds ordinary input but permits the owning directive Plan follow-up"
  (let ((session
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "directive-plan"
          :directive-planning '(:directive-id "d1" :phase approval)
          :pending-follow-ups '((:input "ordinary")))))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session))
    (setf (mevedel-session-pending-follow-ups session)
          '((:input "ordinary")
            (:input "revise" :scope (:directive-id "d1" :action plan))))
    (should-not (mevedel-view--follow-up-auto-drain-blocked-p session))))

(provide 'test-mevedel-pending-inputs)
;;; test-mevedel-pending-inputs.el ends here
