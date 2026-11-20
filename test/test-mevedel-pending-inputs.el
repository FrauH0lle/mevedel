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
(require 'mevedel-agent-control)
(require 'mevedel-chat)
(require 'mevedel-cockpit)
(require 'mevedel-file-state)
(require 'mevedel-pending-inputs)
(require 'mevedel-prompt-submission)
(require 'mevedel-recovery)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-telemetry)
(require 'mevedel-tools)
(require 'mevedel-transport)
(require 'mevedel-turn)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-workspace)
(require 'gptel-request)
(require 'tabulated-list)


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
          (cl-letf (((symbol-function 'mevedel-recovery-save) #'ignore)
                    ((symbol-function 'mevedel-session-artifacts-assert-mutation-authority)
                     #'ignore)
                    ((symbol-function 'mevedel-session-durability-lease-owned-p)
                     (lambda (&rest _) t)))
            ,@body)
         (when-let* ((cockpit
                      (get-buffer mevedel-pending-inputs-buffer-name)))
           ;; Killing the cockpit resumes delivery, and a real drain
           ;; would leave a timer behind after the case has ended.
           (cl-letf (((symbol-function
                       'mevedel-pending-inputs-schedule-held-input-delivery)
                     #'ignore)
                    ((symbol-function 'mevedel-recovery-save) #'ignore))
             (kill-buffer cockpit)))))))

(defun mevedel-pending-inputs-test--replace-composer (view text)
  "Replace VIEW's editable composer with TEXT."
  (let ((inhibit-read-only t))
    (mevedel-view-composer-replace text view)))

(mevedel-deftest mevedel-pending-inputs--set-queues ()
  ,test
  (test)
  :doc "replaces both queues before publishing one guest-visible change"
  (mevedel-pending-inputs-test--with-session
    (let ((notifications 0))
      (cl-letf (((symbol-function
                  'mevedel-collaboration-notify-queue-changed)
                 (lambda (changed-session)
                   (should (eq session changed-session))
                   (setq notifications (1+ notifications)))))
        (mevedel-pending-inputs--set-queues
         session 'steering '((:input "now"))
         'follow-up '((:input "later")))
        (should (equal '((:input "now"))
                       (mevedel-session-pending-steering session)))
        (should (equal '((:input "later"))
                       (mevedel-session-pending-follow-ups session)))
        (should (= 1 notifications)))))
  :doc "saves and notifies only when a queue changed"
  (mevedel-pending-inputs-test--with-session
    (let ((notifications 0) (saves 0))
      (cl-letf (((symbol-function 'mevedel-collaboration-notify-queue-changed)
                 (lambda (_) (cl-incf notifications)))
                ((symbol-function 'mevedel-recovery-save)
                 (lambda (_) (cl-incf saves))))
        (mevedel-pending-inputs--set-queues
         session 'follow-up (mevedel-session-pending-follow-ups session))
        (should (= 0 (+ notifications saves)))
        ;; Steering is not guest-visible, but it is durable.
        (mevedel-pending-inputs--set-queues session 'steering '((:input "now")))
        (should (= 0 notifications))
        (should (= 1 saves))))))

(mevedel-deftest mevedel-view--pending-inputs-render ()
  ,test
  (test)
  :doc "failure-paused steering with a file mention renders input once when expanded"
  (mevedel-pending-inputs-test--with-session
    (let* ((input "@file:/home/roland/Bilder/Bildschirmfotos/Bildschirmfoto_20261024_160228.png Can we add an elapsed time beside the background jobs?")
           (entry (mevedel-session-enqueue-pending-input
                   session 'steering (list :input input)))
           (draft "> active draft\nsecond line"))
      (mevedel-session-set-pending-input-failure-paused session t)
      (with-current-buffer view-buf
        (mevedel-pending-inputs-test--replace-composer view-buf draft)
        (with-current-buffer (mevedel-view-composer-buffer)
          (goto-char (+ (mevedel-view-composer-draft-start) 5)))
        (mevedel-view--interaction-rebuild)
        (should (equal draft (mevedel-view-composer-draft-text nil t)))
        (should (= 5 (mevedel-view-test--composer-point-offset)))
        (should (string-search "Steering\n  1. [+] @file:" (buffer-string)))
        (mevedel-pending-inputs-test--preview-command "1. [+] @file:")
        (dotimes (_ 2)
          ;; Rebuilding must preserve the selected entry without appending
          ;; another projection or restoring the abbreviated input header.
          (mevedel-view--interaction-rebuild)
          (let ((rendered (buffer-string)))
            (should (string-search
                     "Pending-input delivery stopped after turn failure; review required"
                     rendered))
            (should (string-search "Steering\n  1. [-]\n    @file:" rendered))
            (should (= 1 (how-many "@file:" (point-min) (point-max))))
            (should (string-search
                     input (replace-regexp-in-string "\n    " "" rendered)))))
        (with-current-buffer (mevedel-view-composer-buffer)
          (goto-char (+ (mevedel-view-composer-draft-start) 5)))
        (mevedel-view--interaction-rebuild)
        (should (equal draft (mevedel-view-composer-draft-text nil t)))
        (should (= 5 (mevedel-view-test--composer-point-offset)))
        (mevedel-pending-inputs-test--preview-command "1. [-]")
        (should (string-search "Steering\n  1. [+] @file:" (buffer-string)))
        (should (= 1 (how-many "@file:" (point-min) (point-max))))
        (should (eq entry (car (mevedel-session-pending-steering session))))
        (should (mevedel-session-pending-input-failure-paused session)))))
  :doc "shows no review banner once nothing remains to review"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-set-pending-input-failure-paused session t)
    (with-current-buffer view-buf
      (mevedel-view--interaction-rebuild)
      (should-not (string-search "delivery stopped" (buffer-string))))
    (mevedel-session-enqueue-pending-input session 'follow-up '(:input "later"))
    (with-current-buffer view-buf
      (mevedel-view--interaction-rebuild)
      (should (string-search "delivery stopped" (buffer-string))))))

(defun mevedel-pending-inputs-test--preview-command (text &optional mouse)
  "Activate rendered TEXT with its real RET or MOUSE binding."
  (goto-char (point-min))
  (search-forward text)
  (backward-char (length text))
  (let* ((event (and mouse (list 'mouse-1
                                (list (selected-window) (point) '(0 . 0) 0))))
         (last-nonmenu-event event)
         (command (key-binding (if mouse [mouse-1] (kbd "RET")))))
    (should (eq command #'mevedel-pending-inputs-preview-activate))
    (call-interactively command)))

(mevedel-deftest mevedel-pending-inputs-preview-activate ()
  ,test
  (test)
  :doc "RET and mouse inspect one entry without pausing, saving, consuming or approving"
  (mevedel-pending-inputs-test--with-session
    (let ((first (mevedel-session-enqueue-pending-input
                  session 'steering '(:input "first\ncomplete first text")))
          (second (mevedel-session-enqueue-pending-input
                   session 'follow-up '(:input "second\ncomplete second text")))
          (saves 0))
      (save-window-excursion
        (switch-to-buffer view-buf)
        (mevedel-view--interaction-rebuild)
        (cl-letf (((symbol-function 'mevedel-recovery-save)
                   (lambda (_) (cl-incf saves)))
                  ((symbol-function 'mevedel-pending-inputs-open)
                   (lambda (&rest _) (ert-fail "Preview opened management"))))
          (mevedel-pending-inputs-test--preview-command "1. [+] first")
          (should (string-search "    first\n    complete first text" (buffer-string)))
          (mevedel-pending-inputs-test--preview-command "1. [+] second" t)
          (should (string-search "    second\n    complete second text" (buffer-string)))
          (should-not (string-search "    complete first text" (buffer-string)))
          (mevedel-pending-inputs-test--preview-command "1. [-]")
          (should-not (string-search "    complete second text" (buffer-string)))
          (should-not (mevedel-session-pending-input-paused session))
          (should-not (mevedel-view-interaction-blocking-p))
          (should (= saves 0))
          (should (eq first (car (mevedel-session-pending-steering session))))
          (should (eq second (car (mevedel-session-pending-follow-ups session))))))))

  :doc "only the explicit management link pauses delivery; whitespace and detail stay inert"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "queued\n\nplain body"))
    (save-window-excursion
      (switch-to-buffer view-buf)
      (mevedel-view--interaction-rebuild)
      (mevedel-pending-inputs-test--preview-command "1. [+] queued")
      (dolist (text '("  1." "    plain body" "\n\nManage" " (pauses delivery"))
        (goto-char (point-min))
        (search-forward text)
        (backward-char (length text))
        (should-not (get-text-property (point) 'mevedel-pending-input-target))
        (let ((last-nonmenu-event nil))
          (call-interactively (key-binding (kbd "RET"))))
        (should-not (mevedel-session-pending-input-paused session)))
      (mevedel-pending-inputs-test--preview-command "Manage queue…" t)
      (should (derived-mode-p 'mevedel-pending-inputs-mode))
      (should (mevedel-session-pending-input-paused session))))

  :doc "reordering and conversion retain identity; removal or another session closes preview"
  (mevedel-pending-inputs-test--with-session
    (let ((first (mevedel-session-enqueue-pending-input
                  session 'follow-up '(:input "first\nfirst details")))
          (second (mevedel-session-enqueue-pending-input
                   session 'follow-up '(:input "second\nsecond details"))))
      (with-current-buffer view-buf
        (mevedel-view--interaction-rebuild)
        (mevedel-pending-inputs-test--preview-command "2. [+] second")
        (mevedel-session-set-pending-inputs session 'follow-up (list second first))
        (mevedel-view--interaction-rebuild)
        (should (string-search "1. [-]\n    second" (buffer-string)))
        (should (string-search "    second details" (buffer-string)))
        (let ((third (mevedel-session-enqueue-pending-input
                      session 'follow-up '(:input "third")))
              (fourth (mevedel-session-enqueue-pending-input
                       session 'follow-up '(:input "fourth"))))
          (mevedel-session-set-pending-inputs
           session 'follow-up (list first third fourth second))
          (mevedel-view--interaction-rebuild)
          (should-not (string-search "    second details" (buffer-string)))
          (should (string-search "1 more" (buffer-string)))
          (mevedel-session-set-pending-inputs session 'follow-up (list second first))
          (mevedel-view--interaction-rebuild)
          (should (string-search "1. [+] second" (buffer-string)))
          (mevedel-pending-inputs-test--preview-command "1. [+] second"))
        (mevedel-session-set-pending-inputs session 'follow-up (list first))
        (mevedel-session-set-pending-inputs session 'steering (list second))
        (mevedel-view--interaction-rebuild)
        (should (string-search "Steering\n  1. [-]\n    second" (buffer-string)))
        ;; Delivery removes the selected identity, not merely its old index.
        (mevedel-session-set-pending-inputs session 'steering nil)
        (mevedel-view--interaction-rebuild)
        (should-not (string-search "[-]" (buffer-string)))
        (mevedel-pending-inputs-test--preview-command "1. [+] first")
        (let ((other (mevedel-session-create "other" workspace)))
          (mevedel-session-enqueue-pending-input
           other 'follow-up '(:input "other\nother details"))
          (with-current-buffer data-buf (setq-local mevedel--session other))
          (setq-local mevedel--session other)
          (mevedel-view--interaction-rebuild)
          (should (string-search "1. [+] other" (buffer-string)))
          (should-not (string-search "    other details" (buffer-string)))))))

  :doc "queue rerenders preserve a multiline greater-than draft, mark and window position"
  (mevedel-pending-inputs-test--with-session
    (let ((entry (mevedel-session-enqueue-pending-input
                  session 'follow-up '(:input "queued\nfull queued text"))))
      (save-window-excursion
        (switch-to-buffer view-buf)
        (mevedel-view--interaction-rebuild)
        (mevedel-pending-inputs-test--preview-command "1. [+] queued")
        (mevedel-pending-inputs-test--replace-composer
         view-buf (propertize "> draft\nsecond line\n> third" 'draft-test 'kept))
        (with-current-buffer (mevedel-view-composer-buffer)
          (goto-char (+ (mevedel-view-composer-draft-start) 12)))
        (with-current-buffer (mevedel-view-composer-buffer)
          (set-mark (+ (mevedel-view-composer-draft-start) 2)))
        (setq mark-active t)
        (set-window-start (selected-window) (point-min))
        ;; Native viewport measurement fontifies like redisplay.  Snapshot
        ;; displayed draft properties, not an unfinished fontification state.
        (with-current-buffer (mevedel-view-composer-buffer)
          (jit-lock-fontify-now (mevedel-view-composer-draft-start) (mevedel-view-composer-draft-end)))
        (let ((draft (mevedel-view-composer-draft-text))
              (start (window-start)))
          (mevedel-session-enqueue-pending-input
           session 'steering '(:input "newly queued"))
          (mevedel-view--interaction-rebuild)
          (should (equal-including-properties
                   draft (mevedel-view-composer-draft-text)))
          (should (= 12 (mevedel-view-test--composer-point-offset)))
          (should (= 2 (with-current-buffer (mevedel-view-composer-buffer)
                         (- (mark) (mevedel-view-composer-draft-start)))))
          (should mark-active)
          (should (= start (window-start)))
          (should (string-search "    full queued text" (buffer-string)))
          (mevedel-session-set-pending-inputs session 'follow-up nil)
          (mevedel-view--interaction-rebuild)
          (should-not (string-search "full queued text" (buffer-string)))
          (should (= 12 (mevedel-view-test--composer-point-offset)))
          ;; A fresh queue must not inherit the old overlay's disclosure.
          (mevedel-session-set-pending-inputs session 'steering nil)
          (mevedel-view--interaction-rebuild)
          (mevedel-session-set-pending-inputs session 'follow-up (list entry))
          (mevedel-view--interaction-rebuild)
          (should (string-search "1. [+] queued" (buffer-string)))))))

  :doc "oversized plain input is bounded and labels omitted content honestly"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'steering '(:input "The highlighting is flickering"))
    (with-current-buffer view-buf
      (mevedel-view--interaction-rebuild)
      (ert-info ("a fully shown one-line input has no disclosure and cannot repeat itself")
        (should (string-search "1.     The highlighting is flickering\n" (buffer-string)))
        (should-not (string-search "[+]" (buffer-string)))
        (goto-char (point-min))
        (search-forward "The highlighting")
        (should-not (get-text-property (point) 'mevedel-pending-input-target)))))

  :doc "oversized plain input is bounded and labels omitted content honestly"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'follow-up (list :input (concat "large\n" (make-string 10000 ?x) "TAIL")))
    (with-current-buffer view-buf
      (mevedel-view--interaction-rebuild)
      (mevedel-pending-inputs-test--preview-command "1. [+] large")
      (should (string-search "Partial preview: 10 lines maximum" (buffer-string)))
      (should-not (string-search "TAIL" (buffer-string)))
      (should (< (- (mevedel-view-transcript-tail) (point-min)) 1500))
      (should (string-search "Manage queue… (pauses delivery; C-c C-e)" (buffer-string))))))

(mevedel-deftest mevedel-view--pending-input-preview ()
  ,test
  (test)
  :doc "summary uses display columns and strips authored properties"
  (let ((preview (mevedel-view--pending-input-preview
                  (propertize (make-string 100 ?界) 'display "hidden"))))
    (should (<= (string-width preview) 96))
    (should (string-suffix-p "..." preview))
    (should-not (text-properties-at 0 preview)))
  :doc "ordinary whitespace folds without changing short input"
  (should (equal "one two three"
                 (mevedel-view--pending-input-preview " \tone\ntwo\r three  ")))
  :doc "queue summaries normalize only bounded prefixes of very large input"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'follow-up (list :input (make-string 1000000 ?界)))
    (with-current-buffer view-buf
      (let ((normalize (symbol-function 'mevedel--normalize-message-text))
            (largest 0))
        (cl-letf (((symbol-function 'mevedel--normalize-message-text)
                   (lambda (text)
                     (setq largest (max largest (length text)))
                     (funcall normalize text))))
          (mevedel-view--pending-inputs-body session))
        (should (> largest 0))
        (should (<= largest 1024))))))

(mevedel-deftest mevedel-pending-inputs--preview-detail ()
  ,test
  (test)
  :doc "expanded input normalizes bounded text and neutralizes tab and CR layout"
  (with-temp-buffer
    (let ((normalize (symbol-function 'mevedel--normalize-message-text))
          (largest 0)
          (text (concat "heading\tvalue\r\n" (make-string 1000000 ?界)))
          detail)
      (cl-letf (((symbol-function 'mevedel--normalize-message-text)
                 (lambda (input)
                   (setq largest (max largest (length input)))
                   (funcall normalize input))))
        (setq detail (mevedel-pending-inputs--preview-detail (list :input text))))
      (should-not (string-match-p "[\t\r]" detail))
      (should (string-search "Partial preview" detail))
      (should (<= largest 1601))
      (dolist (line (butlast (split-string detail "\n") 2))
        (should (<= (string-width line) 84))))))

(mevedel-deftest mevedel-pending-inputs-follow-up-changed ()
  ,test
  (test)
  :doc "delegates one follow-up queue change to collaboration"
  (let ((session (mevedel-session--create :name "queue"))
        notified)
    (cl-letf (((symbol-function
                'mevedel-collaboration-notify-queue-changed)
               (lambda (changed-session) (setq notified changed-session))))
      (mevedel-pending-inputs-follow-up-changed session)
      (should (eq session notified)))))

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
               (point-min) (mevedel-view-transcript-tail)))))))))

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
        (mevedel-view-test--insert-composer-draft (propertize "> draft\nsecond" 'draft-property 'kept))
        (with-current-buffer (mevedel-view-composer-buffer)
          (goto-char (+ (mevedel-view-composer-draft-start) 3)))
        (setq draft
              (mevedel-view-composer-draft-text)
              point-offset (mevedel-view-test--composer-point-offset)))
      (save-window-excursion
        (setq cockpit
              (with-current-buffer view-buf
                (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get entry :id))
          (mevedel-pending-inputs-edit))
        (with-current-buffer view-buf
          (should (equal "queued\nmessage"
                         (mevedel-view-composer-draft-text nil t)))
          (let ((rail (mevedel-view-zone-region 'progress)))
            (should (overlayp rail))
            (should (string-match-p
                     "Editing follow-up 1"
                     (buffer-substring-no-properties
                      (overlay-start rail) (overlay-end rail)))))
          (should-error (with-current-buffer (mevedel-view-composer-buffer)
                          (mevedel-view-send)) :type 'user-error)
          (should-error (with-current-buffer (mevedel-view-composer-buffer)
                          (mevedel-view-send-follow-up)) :type 'user-error)
          (mevedel-test--with-captured-messages nil
            (mevedel-pending-inputs-cancel-edit)))
        (with-current-buffer view-buf
          (should
           (equal-including-properties
            draft
            (mevedel-view-composer-draft-text)))
          (should (= point-offset
                     (mevedel-view-test--composer-point-offset)))
          (should (eq 'kept
                      (with-current-buffer (mevedel-view-composer-buffer)
                        (get-text-property
                         (mevedel-view-composer-draft-start) 'draft-property))))))
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
        (mevedel-view-test--insert-composer-draft "> retained\nmultiline"))
      (save-window-excursion
        (setq cockpit
              (with-current-buffer view-buf
                (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get first :id))
          (mevedel-pending-inputs-edit))
        (mevedel-pending-inputs-test--replace-composer
         view-buf "edited first")
        (should (eq (window-buffer (selected-window))
                    (mevedel-view-composer-buffer view-buf)))
        (with-current-buffer (window-buffer (selected-window))
          (mevedel-test--with-captured-messages nil
            (call-interactively #'mevedel-pending-inputs-save-edit)))
        (with-current-buffer view-buf
          (should (equal "> retained\nmultiline"
                         (mevedel-view-composer-draft-text nil t))))
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

  :doc "editing a shared question retains retry identity without claiming the old context"
  (mevedel-pending-inputs-test--with-session
    (let* ((entry (mevedel-session-enqueue-pending-input
                   session 'follow-up
                   '(:input "original snapshot" :shared-question
                     (:questionId "q" :itemId "doc" :fingerprint "receipt"
                      :text "original" :quote "old passage" :revision 4 :scope "selection"))))
           cockpit)
      (save-window-excursion
        (setq cockpit (with-current-buffer view-buf (mevedel-pending-inputs-open)))
        (with-current-buffer cockpit
          (mevedel-cockpit-goto-id (plist-get entry :id))
          (mevedel-pending-inputs-edit))
        (mevedel-pending-inputs-test--replace-composer view-buf "Revised on host")
        (with-current-buffer view-buf (mevedel-pending-inputs-save-edit))
        (let ((shared (plist-get (car (mevedel-session-pending-follow-ups session)) :shared-question)))
          (should (plist-get shared :edited))
          (should (equal (plist-get shared :text) "Revised on host"))
          (should (equal (plist-get shared :questionId) "q"))
          (should (equal (plist-get shared :fingerprint) "receipt"))
          (should-not (plist-get shared :quote))
          (should-not (plist-get shared :revision))))))

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
          (should (equal "/review" (mevedel-view-composer-draft-text nil t))))
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
          (should (equal "still editing" (mevedel-view-composer-draft-text nil t))))
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
        (with-current-buffer (mevedel-view-composer-buffer view-buf)
          (mevedel-test--with-captured-messages nil
            (mevedel-pending-inputs-cancel-edit)))
        (should-not (buffer-local-value 'mevedel-view--pending-input-edit view-buf))
        (should (eq entry
                    (car
                     (mevedel-session-pending-follow-ups session))))
        (should (eq cockpit (window-buffer (selected-window))))))))

(mevedel-deftest mevedel-pending-inputs--on-kill-buffer ()
  ,test
  (test)
  :doc "killing the cockpit resumes delivery instead of stranding it"
  ;; The pause lives on the session while the cockpit buffer owns it, so
  ;; `C-x k' instead of `q' left delivery paused on a live session with
  ;; no visible way back: reopening refuses on an empty queue.
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "later"))
    (let (scheduled)
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (cl-letf (((symbol-function
                      'mevedel-pending-inputs-schedule-held-input-delivery)
                     (lambda () (setq scheduled t))))
            (kill-buffer cockpit))))
      (should scheduled)
      (should-not (mevedel-session-pending-input-paused session))))

  :doc "killing the cockpit releases a WAIT held at the boundary"
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
          (cl-letf (((symbol-function 'gptel--fsm-transition)
                     (lambda (_fsm state) (setq transition state)))
                    ((symbol-function
                      'mevedel-pending-inputs-schedule-held-input-delivery)
                     #'ignore))
            (kill-buffer cockpit))))
      (should (eq transition 'WAIT))
      (should-not (mevedel-session-pending-input-paused session))))

  :doc "killing during an entry edit restores the draft it suspended"
  ;; The resume would be dead on arrival otherwise: the entry's text sits
  ;; in the composer, and the drain refuses to deliver while it is there.
  (mevedel-pending-inputs-test--with-session
    (let ((entry (mevedel-session-enqueue-pending-input
                  session 'follow-up '(:input "queued")))
          scheduled)
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft "> draft"))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get entry :id))
            (mevedel-pending-inputs-edit))
          (should (buffer-local-value 'mevedel-view--pending-input-edit
                                      view-buf))
          (cl-letf (((symbol-function
                      'mevedel-pending-inputs-schedule-held-input-delivery)
                     (lambda () (setq scheduled t))))
            (kill-buffer cockpit))))
      (should scheduled)
      (should-not (buffer-local-value 'mevedel-view--pending-input-edit
                                      view-buf))
      (with-current-buffer view-buf
        (should (equal "> draft" (mevedel-view-composer-draft-text nil t))))
      (should-not (mevedel-session-pending-input-paused session))))

  :doc "a failure pause survives the kill and delivers nothing"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "later"))
    (mevedel-session-set-pending-input-failure-paused session t)
    (let (scheduled)
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (cl-letf (((symbol-function
                      'mevedel-pending-inputs-schedule-held-input-delivery)
                     (lambda () (setq scheduled t))))
            (kill-buffer cockpit))))
      (should-not scheduled)
      (should (mevedel-session-pending-input-failure-paused session))
      (should-not (mevedel-session-pending-input-paused session))))

  :doc "a failure latch with no queued input re-offers background mail"
  (mevedel-pending-inputs-test--with-session
    (let ((failed (mevedel-session-enqueue-pending-input
                   session 'steering
                   '(:input "drop" :request-id "dead" :state failed-turn)))
          scheduled)
      (mevedel-session-set-pending-input-failure-paused session t)
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          ;; Resolving the failed input by deletion leaves the latch set.
          (mevedel-pending-inputs--set-queues
           session 'steering
           (remq failed (mevedel-session-pending-steering session)))
          (cl-letf (((symbol-function
                      'mevedel-pending-inputs-schedule-held-input-delivery)
                     (lambda () (setq scheduled t))))
            (kill-buffer cockpit))))
      (should scheduled)
      (should (mevedel-session-pending-input-failure-paused session))))

  :doc "reopening for the same session keeps its pause and delivers nothing"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "later"))
    (let (scheduled)
      (save-window-excursion
        (cl-letf (((symbol-function
                    'mevedel-pending-inputs-schedule-held-input-delivery)
                   (lambda () (setq scheduled t))))
          (with-current-buffer view-buf
            (mevedel-pending-inputs-open))
          ;; Reopening is an inspection, not a release.
          (with-current-buffer view-buf
            (mevedel-pending-inputs-open))))
      (should-not scheduled)
      (should (mevedel-session-pending-input-paused session))))

  :doc "opening the cockpit for another session releases the one it held"
  ;; The buffer name is a constant, so the second session used to take
  ;; the first session's cockpit and its pause with it.
  (mevedel-pending-inputs-test--with-session
    (let ((first-session session))
      (mevedel-session-enqueue-pending-input
       first-session 'follow-up '(:input "later"))
      (cl-letf (((symbol-function
                  'mevedel-pending-inputs-schedule-held-input-delivery)
                 #'ignore))
        (save-window-excursion
          (with-current-buffer view-buf
            (mevedel-pending-inputs-open))
          (should (mevedel-session-pending-input-paused first-session))
          (mevedel-pending-inputs-test--with-session
            (mevedel-session-enqueue-pending-input
             session 'follow-up '(:input "other"))
            (with-current-buffer view-buf
              (mevedel-pending-inputs-open))
            (should (mevedel-session-pending-input-paused session))
            (should-not
             (mevedel-session-pending-input-paused first-session)))))))

  :doc "a normal close leaves nothing for the kill hook to resume"
  (mevedel-pending-inputs-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'follow-up '(:input "later"))
    (let ((drains 0))
      (save-window-excursion
        (let ((cockpit
               (with-current-buffer view-buf
                 (mevedel-pending-inputs-open))))
          (cl-letf (((symbol-function
                      'mevedel-pending-inputs-schedule-held-input-delivery)
                     (lambda () (cl-incf drains)))
                    ((symbol-function 'mevedel-cockpit-quit) #'ignore))
            (with-current-buffer cockpit
              (mevedel-pending-inputs-quit))
            (when (buffer-live-p cockpit)
              (kill-buffer cockpit)))))
      ;; Once, for the quit.  The hook must not fire again on the kill.
      (should (= 1 drains)))))

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
                 'mevedel-pending-inputs-schedule-held-input-delivery)
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
                 'mevedel-pending-inputs-schedule-held-input-delivery)
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
                 'mevedel-pending-inputs-schedule-held-input-delivery)
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
        (mevedel-view-test--insert-composer-draft "> draft"))
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
        (should (equal "> draft" (mevedel-view-composer-draft-text nil t)))))))

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
  :doc "requeues failed follow-up at the tail without losing guest metadata"
  (mevedel-pending-inputs-test--with-session
    (let* ((failed (mevedel-session-enqueue-pending-input
                    session 'follow-up
                    '(:input "retry me" :state failed-turn :blocked t
                      :request-id "lost" :guest-id "guest" :guest-name "Guest"
                      :guest-role full :guest-paths ("/tmp/attachment")
                      :inert-skills t :guest-skills ("review"))))
           (later (mevedel-session-enqueue-pending-input
                   session 'follow-up '(:input "later"))))
      (mevedel-session-set-pending-input-failure-paused session t)
      (save-window-excursion
        (let ((cockpit (with-current-buffer view-buf
                         (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get failed :id))
            (mevedel-pending-inputs-make-follow-up))))
      (let ((replacement (cadr (mevedel-session-pending-follow-ups session))))
        (should (eq later (car (mevedel-session-pending-follow-ups session))))
        (should (= 2 (length (mevedel-session-pending-follow-ups session))))
        (should (eq 'pending (plist-get replacement :state)))
        (should-not (plist-member replacement :blocked))
        (should-not (plist-member replacement :request-id))
        (dolist (key '(:id :input :guest-id :guest-name :guest-role :guest-paths
                        :inert-skills :guest-skills))
          (should (equal (plist-get failed key) (plist-get replacement key)))))
      (should (mevedel-session-pending-input-failure-paused session))))

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

(mevedel-deftest mevedel-pending-inputs-requeue-steering ()
  ,test
  (test)
  :doc "a settled turn's steering leads the follow-ups in order, its submission restored"
  (mevedel-pending-inputs-test--with-session
    (let* ((context-entries '((:event SessionStart :body "restore me")))
           (submission
            (mevedel-prompt-submission-create
             :input "first" :display-text "first" :session session
             :context-entries context-entries :state 'reserved))
           (first (mevedel-session-enqueue-pending-input
                   session 'steering
                   (list :input "first" :model-input "prepared" :request-id "settled"
                         :submission submission)))
           (other (mevedel-session-enqueue-pending-input
                   session 'steering '(:input "other turn" :request-id "live")))
           (second (mevedel-session-enqueue-pending-input
                    session 'steering '(:input "second" :request-id "settled")))
           (older (mevedel-session-enqueue-pending-input
                   session 'follow-up '(:input "older"))))
      (should (equal (list first second)
                     (mevedel-pending-inputs-requeue-steering session "settled")))
      (should (equal (list other) (mevedel-session-pending-steering session)))
      (let ((queue (mevedel-session-pending-follow-ups session)))
        (should (equal '("first" "second" "older")
                       (mapcar (lambda (entry) (plist-get entry :input)) queue)))
        (should (equal (plist-get first :id) (plist-get (car queue) :id)))
        (should (eq 'follow-up (plist-get (car queue) :category)))
        (should-not (plist-member (car queue) :request-id))
        (should-not (plist-member (car queue) :submission))
        (should (eq older (nth 2 queue))))
      (should (equal context-entries (mevedel-session-hook-context-pending session)))))

  :doc "a turn without leftover steering leaves both queues untouched"
  (mevedel-pending-inputs-test--with-session
    (let ((steering (mevedel-session-pending-steering session))
          (follow-ups (mevedel-session-pending-follow-ups session)))
      (should-not (mevedel-pending-inputs-requeue-steering session "settled"))
      (should (eq steering (mevedel-session-pending-steering session)))
      (should (eq follow-ups (mevedel-session-pending-follow-ups session))))))

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

  :doc "failed follow-ups require explicit requeue before resuming"
  (mevedel-pending-inputs-test--with-session
    (let ((failed (mevedel-session-enqueue-pending-input
                   session 'follow-up '(:input "review me" :state failed-turn))))
      (mevedel-session-set-pending-input-failure-paused session t)
      (save-window-excursion
        (let ((cockpit (with-current-buffer view-buf
                         (mevedel-pending-inputs-open))))
          (with-current-buffer cockpit
            (mevedel-cockpit-goto-id (plist-get failed :id))
            (should-error (mevedel-pending-inputs-resume-after-failure)
                          :type 'user-error)
            (should (mevedel-session-pending-input-failure-paused session))
            (mevedel-pending-inputs-make-follow-up)
            (mevedel-test--with-captured-messages nil
              (mevedel-pending-inputs-resume-after-failure)))))
      (should-not (mevedel-session-pending-input-failure-paused session))
      (should (eq 'pending (plist-get (car (mevedel-session-pending-follow-ups session))
                                     :state)))))

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

(defmacro test-pending-inputs--with-held-session (&rest body)
  "Run BODY in a view whose session waits for a user message after expiry."
  (declare (indent 0))
  `(mevedel-view-test--with-buffers
     (let* ((mevedel-permission-queue-timeout 3600)
            (workspace (mevedel-workspace--create
                        :type 'file :id "absence-clear"
                        :root (mevedel-test--placeholder-root)
                        :name "absence-clear"))
            (session (mevedel-session-create "main" workspace)))
       (with-current-buffer data-buf
         (setq-local mevedel--session session))
       (setf (mevedel-session-root-buffer session) data-buf)
       (setf (mevedel-session-permission-absence session)
             (list :expired-at (float-time) :count 2))
       (with-current-buffer view-buf
         (setq-local mevedel--session session)
         (cl-letf (((symbol-function 'mevedel-view--interaction-rebuild)
                    #'ignore)
                   ((symbol-function
                     'mevedel-view--schedule-late-follow-up-drain)
                    #'ignore)
                   ((symbol-function 'mevedel-view--forward-input) #'ignore))
           ,@body)))))

(mevedel-deftest mevedel-view--absence-clearing-sites
  (:quiet t :doc "a submitted user message ends the wait after expiry")
  ,test
  (test)

  :doc "queueing a follow-up from the composer clears the hold"
  (test-pending-inputs--with-held-session
    (should (mevedel-permission-absence-held-p session))
    (mevedel-view--queue-follow-up "later")
    (should-not (mevedel-permission-absence-held-p session)))

  :doc "a recorded prompt clears the hold once it is inserted"
  (test-pending-inputs--with-held-session
    (let (caller-ran)
      (cl-letf (((symbol-function 'mevedel-view--forward-input)
                 (lambda (_input &rest keys)
                   (should (mevedel-permission-absence-held-p session))
                   (funcall (plist-get keys :after-insert)))))
        (mevedel-view--submit-planned-input
         "hello" nil nil nil (lambda () (setq caller-ran t)) t))
      (should caller-ran)
      (should-not (mevedel-permission-absence-held-p session))))

  :doc "a prompt rejected before dispatch keeps the hold"
  (test-pending-inputs--with-held-session
    (cl-letf (((symbol-function 'mevedel-view--forward-input)
               (lambda (_input &rest keys)
                 (funcall (plist-get keys :on-block)))))
      (mevedel-view--submit-planned-input "hello" nil #'ignore nil nil t))
    (should (mevedel-permission-absence-held-p session)))

  :doc "steering clears the hold only once it is queued"
  (test-pending-inputs--with-held-session
    (let ((submission (mevedel-prompt-submission-create :input "steer")))
      ;; A request that can no longer be steered queues nothing.
      (cl-letf (((symbol-function 'mevedel-view--prepare-steering-entry)
                 #'ignore))
        (should-not (mevedel-view--queue-prepared-steering submission 'request)))
      (should (mevedel-permission-absence-held-p session))
      (cl-letf (((symbol-function 'mevedel-view--prepare-steering-entry)
                 (lambda (&rest _) (list :input "steer")))
                ((symbol-function 'mevedel-view-history-add) #'ignore)
                ((symbol-function 'mevedel-agent-control-root-waiting-p)
                 #'ignore))
        (should (mevedel-view--queue-prepared-steering submission 'request)))
      (should-not (mevedel-permission-absence-held-p session))))

  :doc "a dispatch callback, not submission, decides whether the hold clears"
  (test-pending-inputs--with-held-session
    (cl-letf (((symbol-function 'mevedel-view--run-prompt-submit-hook)
               (lambda (input _display callback &rest _)
                 (funcall callback
                          (mevedel-prompt-submission-create :input input))))
              ((symbol-function 'mevedel-view--dispatch-prepared-outcome)
               (lambda (sub _buffer &rest keys)
                 (when-let* ((before (plist-get keys :before-send)))
                   (funcall before))
                 (funcall (plist-get keys :dispatch) sub))))
      (mevedel-view--submit-planned-input "steer" nil nil #'ignore nil t))
    (should (mevedel-permission-absence-held-p session)))

  :doc "saving an edit to queued steering is not a new message and keeps the hold"
  (test-pending-inputs--with-held-session
    (let ((state (list :saving nil)) finished)
      (setq-local mevedel-view--pending-input-edit state)
      (cl-letf (((symbol-function 'mevedel-view--run-prompt-submit-hook)
                 (lambda (input _display callback &rest _)
                   (funcall callback
                            (mevedel-prompt-submission-create :input input))))
                ((symbol-function 'mevedel-view--dispatch-prepared-outcome)
                 (lambda (sub _buffer &rest keys)
                   (when-let* ((before (plist-get keys :before-send)))
                     (funcall before))
                   (funcall (plist-get keys :dispatch) sub)))
                ((symbol-function 'mevedel-view--prepare-steering-entry)
                 (lambda (&rest _) (list :input "edited")))
                ((symbol-function 'mevedel-pending-inputs--finish-replacement)
                 (lambda (&rest _) (setq finished t))))
        (mevedel-pending-inputs--save-steering
         state "edited" (mevedel-request--create :session session)))
      (should finished)
      (should (mevedel-permission-absence-held-p session))))

  :doc "a /goal blocked by its submit hook keeps the hold"
  (test-pending-inputs--with-held-session
    (let (started)
      (cl-letf (((symbol-function 'mevedel-view--run-prompt-submit-hook)
                 #'ignore)
                ((symbol-function 'mevedel-goal-start)
                 (lambda (&rest _) (setq started t))))
        (mevedel-view--send-local-goal "/goal finish" "finish"))
      (should-not started)
      (should (mevedel-permission-absence-held-p session))))

  :doc "an accepted /goal clears the hold"
  (test-pending-inputs--with-held-session
    (cl-letf (((symbol-function 'mevedel-view--run-prompt-submit-hook)
               (lambda (input _display callback &rest _)
                 (funcall callback
                          (mevedel-prompt-submission-create :input input))))
              ((symbol-function 'mevedel-goal-start) #'ignore)
              ((symbol-function 'mevedel-view-history-add) #'ignore)
              ((symbol-function 'mevedel-view--clear-submitted-input) #'ignore))
      (mevedel-view--send-local-goal "/goal finish" "finish"))
    (should-not (mevedel-permission-absence-held-p session)))

  :doc "a guest's queued message clears the hold"
  (test-pending-inputs--with-held-session
    (mevedel-view-enqueue-external-follow-up data-buf "from a guest"
                                             :guest-name "Guest")
    (should-not (mevedel-permission-absence-held-p session)))

  :doc "a background-event wake waits while held and runs once a message clears it"
  (test-pending-inputs--with-held-session
    (cl-letf (((symbol-function 'mevedel-agent-control-wake-pending-p) #'always)
              ((symbol-function 'mevedel-turn-busy-p) #'ignore)
              ((symbol-function 'mevedel-recovery-blocker) #'ignore)
              ((symbol-function 'mevedel-view--reserved-goal-handoff-id)
               #'ignore))
      (should-not (mevedel-pending-inputs--event-wake-view data-buf))
      (mevedel-permission-absence-clear session)
      (should (eq view-buf (mevedel-pending-inputs--event-wake-view data-buf)))))

  :doc "the hold stops queued follow-ups from draining until it clears"
  (test-pending-inputs--with-held-session
    (should (mevedel-view--follow-up-auto-drain-blocked-p session))
    (mevedel-permission-absence-clear session)
    (should-not (mevedel-view--follow-up-auto-drain-blocked-p session))))

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
    (should (string-match-p "2 more" body)))
  :doc "expanded steering and follow-ups show input lines without repeating them"
  (dolist (label '("Steering" "Follow-ups"))
    (let* ((session (mevedel-session--create :name "preview"))
           (entry (list :id 1 :input "first line\nsecond line"))
           (body (mevedel-view--pending-input-category-body
                  label (list entry) session 1)))
      (should (equal (concat "\n" label "\n  1. [-]"
                             "\n    first line\n    second line\n")
                     (substring-no-properties body)))
      (should (equal (cons session 1)
                     (get-text-property (string-search "[-]" body)
                                        'mevedel-pending-input-target body))))))

(mevedel-deftest mevedel-view--steering-request-context-supported-p ()
  ,test
  (test)
  :doc "allows bookkeeping-only context and unrestricted ToolCall policy"
  (should
   (mevedel-view--steering-request-context-supported-p
    '(:permission-rules nil :hook-rules nil
      :ptc-primitives :unrestricted :invoked-skills (alpha))))
  (dolist (context '((:permission-rules (rule))
                     (:hook-rules (rule))
                     (:model model)
                     (:effort high)
                     (:ptc-primitives ("Read"))))
    (should-not
     (mevedel-view--steering-request-context-supported-p context)))
  (should-not
   (mevedel-view--steering-request-context-supported-p
    '(:future-policy nil))))

(mevedel-deftest mevedel-view--next-follow-up ()
  ,test
  (test)
  :doc "uses FIFO without a workflow and returns the original entry"
  (let* ((first (list :input "first"))
         (session (mevedel-session--create
                   :pending-follow-ups (list first '(:input "second")))))
    (should (eq first (mevedel-view--next-follow-up session)))
    (setf (mevedel-session-pending-follow-ups session) nil)
    (should-not (mevedel-view--next-follow-up session))))

(mevedel-deftest mevedel-view--follow-up-auto-drain-blocked-p ()
  ,test
  (test)
  :doc "blocks fallback drainage for approval and Goal handoff ownership"
  (let ((session (mevedel-session--create
                  :name "main" :pending-plan-approval 'plan)))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session))
    (setf (mevedel-session-pending-plan-approval session) nil)
    (should-not
     (mevedel-view--follow-up-auto-drain-blocked-p session)))
  (let ((here
         (mevedel-session--create
          :name "here"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "here-goal"
             :selection (:location here :execution goal)))))
        (source
         (mevedel-session--create
          :name "source"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "target-goal"
             :selection (:location worktree :execution goal))))))
    (should (mevedel-view--follow-up-auto-drain-blocked-p here))
    (should (mevedel-view--follow-up-auto-drain-blocked-p source)))
  (let* ((goal (mevedel-goal--create :id "goal" :status 'paused))
         (session
          (mevedel-session--create
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
                  :name "failed" :pending-input-failure-paused t)))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session))))

(mevedel-deftest mevedel-pending-inputs--idle-work ()
  ,test
  (test)
  :doc "names queued follow-ups before an active Goal and requires an idle root"
  (mevedel-pending-inputs-test--with-session
    (should-not (mevedel-pending-inputs--idle-work data-buf))
    (let ((goal (mevedel-goal--create :id "g" :status 'active)))
      (setf (mevedel-session-goal session) goal)
      (should (eq 'goal (mevedel-pending-inputs--idle-work data-buf)))
      (mevedel-session-set-pending-inputs
       session 'follow-up (list (list :input "queued")))
      (should (eq 'follow-up (mevedel-pending-inputs--idle-work data-buf)))
      (mevedel-session-set-pending-inputs session 'follow-up nil)
      (setf (mevedel-goal-status goal) 'paused)
      (should-not (mevedel-pending-inputs--idle-work data-buf))
      (setf (mevedel-goal-status goal) 'active)
      (with-current-buffer data-buf
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (should-not (mevedel-pending-inputs--idle-work data-buf))
      (with-current-buffer data-buf
        (setq-local mevedel--current-request nil
                    mevedel--view-buffer nil))
      (should-not (mevedel-pending-inputs--idle-work data-buf))))
  :doc "names background events after follow-ups and before an active Goal"
  (mevedel-pending-inputs-test--with-session
    (setf (mevedel-session-goal session)
          (mevedel-goal--create :id "g" :status 'active))
    (mevedel-agent-control-enqueue-execution-result
     session "/root" "done" "exec-1" t)
    (should (eq 'event (mevedel-pending-inputs--idle-work data-buf)))
    (mevedel-session-set-pending-inputs
     session 'follow-up (list (list :input "queued")))
    (should (eq 'follow-up (mevedel-pending-inputs--idle-work data-buf)))
    (mevedel-session-set-pending-inputs session 'follow-up nil)
    ;; Mail that does not wake leaves the Goal next.
    (mevedel-agent-control-clear-wake session "/root" "exec-1")
    (should (eq 'goal (mevedel-pending-inputs--idle-work data-buf)))))

(mevedel-deftest mevedel-pending-inputs-offer-root-event ()
  ,test
  (test)
  :doc "uses the session root or a valid unsaved root, never a child's buffer"
  (mevedel-pending-inputs-test--with-session
    (let ((child (generate-new-buffer " *event-child*")) offered)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-idle-work)
                     (lambda (buffer) (push buffer offered))))
            (with-current-buffer child
              (setq-local mevedel--session session
                          mevedel--agent-invocation
                          (mevedel-agent-invocation--create :path "/root/child")))
            (setf (mevedel-session-root-buffer session) data-buf)
            (mevedel-pending-inputs-offer-root-event session)
            (mevedel-pending-inputs-offer-root-event session child)
            (setf (mevedel-session-root-buffer session) nil)
            (mevedel-pending-inputs-offer-root-event session data-buf)
            (mevedel-pending-inputs-offer-root-event session child)
            (should (equal (list data-buf data-buf data-buf) offered)))
        (kill-buffer child))))
  :doc "an offer failure leaves committed waking mail available"
  (mevedel-pending-inputs-test--with-session
    (setf (mevedel-session-root-buffer session) data-buf)
    (mevedel-agent-control-enqueue-execution-result
     session "/root" "done" "exec-1" t)
    (let (warning)
      (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-idle-work)
                 (lambda (_) (error "Offer failed")))
                ((symbol-function 'mevedel--warn-once)
                 (lambda (_scope _key format &rest args)
                   (setq warning (apply #'format format args)))))
        (mevedel-pending-inputs-offer-root-event session))
      (should (string-search "Offer failed" warning))
      (should (mevedel-agent-control-wake-pending-p session))
      (should (equal "done" (plist-get (car (mevedel-session-messages session))
                                      :payload))))))

(mevedel-deftest mevedel-pending-inputs-offer-idle-work ()
  ,test
  (test)
  :doc "defers the decision and starts the follow-up drain before the Goal"
  (mevedel-pending-inputs-test--with-session
    (let ((goal (mevedel-goal--create :id "g" :status 'active))
          timers drained continued)
      (setf (mevedel-session-goal session) goal)
      (mevedel-session-set-pending-inputs
       session 'follow-up (list (list :input "queued")))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (push (cons function args) timers)))
                ((symbol-function 'mevedel-view--run-follow-up-drain)
                 (lambda (buffer) (push buffer drained)))
                ((symbol-function 'mevedel-goal-schedule-continuation)
                 (lambda (&rest args) (push args continued))))
        (mevedel-pending-inputs-offer-idle-work data-buf)
        (should-not drained)
        (should (equal `((mevedel-pending-inputs--offer-idle-work-now
                          ,data-buf))
                       timers))
        (apply (caar timers) (cdar timers))
        (should (equal (list data-buf) drained))
        (should-not continued)
        (mevedel-session-set-pending-inputs session 'follow-up nil)
        (mevedel-pending-inputs--offer-idle-work-now data-buf)
        (should (equal `((,session ,data-buf)) continued)))))

  :doc "dispatches a pending background event"
  (mevedel-pending-inputs-test--with-session
    (let (woken)
      (mevedel-agent-control-enqueue-execution-result
       session "/root" "done" "exec-1" t)
      (cl-letf (((symbol-function 'mevedel-pending-inputs--run-event-wake)
                 (lambda (buffer) (push buffer woken))))
        (mevedel-pending-inputs--offer-idle-work-now data-buf))
      (should (equal (list data-buf) woken))))

  :doc "schedules nothing when the session holds no idle work"
  (mevedel-pending-inputs-test--with-session
    (let (timers)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest args) (push args timers))))
        (mevedel-pending-inputs-offer-idle-work data-buf))
      (should-not timers))))

(mevedel-deftest mevedel-view--offer-follow-up-after-draft-change ()
  ,test
  (test)
  :doc "a follow-up held by a draft is delivered once the draft is deleted"
  (mevedel-pending-inputs-test--with-session
    (let ((entry (list :input "queued follow-up"))
          timers delivered)
      (mevedel-session-set-pending-inputs session 'follow-up (list entry))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (push (cons function args) timers)))
                ((symbol-function 'mevedel-transport-run-when-idle)
                 (lambda (_key _path thunk &rest _) (funcall thunk) t))
                ((symbol-function 'mevedel-view--dispatch-follow-up-entry)
                 (lambda (_kind entry &rest _) (push entry delivered))))
        (with-current-buffer view-buf
          (mevedel-view-test--insert-composer-draft "unsent draft")
          (should-not timers)
          (mevedel-view--drain-follow-up data-buf)
          (should-not delivered)
          ;; A partial deletion leaves a draft to protect.
          (with-current-buffer (mevedel-view-composer-buffer)
            (delete-region (- (mevedel-view-composer-draft-end) 6)
                           (mevedel-view-composer-draft-end)))
          (should-not timers)
          (with-current-buffer (mevedel-view-composer-buffer)
            (delete-region (mevedel-view-composer-draft-start) (mevedel-view-composer-draft-end)))
          (should (equal `((mevedel-pending-inputs--offer-idle-work-now ,data-buf t))
                         timers))
          (apply (caar timers) (cdar timers))
          (should (equal (list entry) delivered))))))

  :doc "clearing the composer schedules nothing without queued follow-ups"
  (mevedel-pending-inputs-test--with-session
    (let (timers)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest args) (push args timers))))
        (with-current-buffer view-buf
          (mevedel-view-test--insert-composer-draft "draft")
          (with-current-buffer (mevedel-view-composer-buffer)
            (delete-region (mevedel-view-composer-draft-start) (mevedel-view-composer-draft-end)))))
      (should-not timers)))

  :doc "a background event held by the native draft is offered once it is cleared"
  (mevedel-pending-inputs-test--with-session
    (let (timers woken continued)
      (setf (mevedel-session-goal session)
            (mevedel-goal--create :id "g" :status 'active))
      (mevedel-agent-control-enqueue-execution-result
       session "/root" "done" "exec-1" t)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (push (cons function args) timers)))
                ((symbol-function 'mevedel-pending-inputs--run-event-wake)
                 (lambda (buffer) (push buffer woken)))
                ((symbol-function 'mevedel-goal-schedule-continuation)
                 (lambda (&rest args) (push args continued))))
        (with-current-buffer (mevedel-view-composer-buffer view-buf)
          (mevedel-view-composer-replace "draft")
          (should-not (mevedel-pending-inputs--event-wake-view data-buf))
          (should-not timers)
          (mevedel-view-composer-replace " \n ")
          (should (= 1 (length timers)))
          (apply (caar timers) (cdar timers))
          (should (equal (list data-buf) woken))
          ;; A wake consumed before the timer fires leaves the Goal alone.
          (setq woken nil)
          (mevedel-agent-control-clear-context-mailbox session)
          (apply (caar timers) (cdar timers))
          (should-not woken)
          (should-not continued))))))

(defmacro mevedel-pending-inputs-test--with-event (&rest body)
  "Run BODY in a session holding one waking result, capturing submissions.
BODY sees SUBMITTED, a list of (PROMPT DISPLAY ATTRIBUTION) per turn."
  (declare (indent 0) (debug t))
  `(mevedel-pending-inputs-test--with-session
     (let (submitted)
       (mevedel-agent-control-enqueue-execution-result
        session "/root" "done" "exec-1" t)
       (cl-letf (((symbol-function 'mevedel-transport-run-when-idle)
                  (lambda (_key _path thunk &rest _) (funcall thunk) t))
                 ((symbol-function 'mevedel--submit-generated-turn)
                  (lambda (prompt &optional display &rest _)
                    (push (list prompt display
                                mevedel-view--pending-guest-attribution)
                          submitted))))
         ,@body))))

(mevedel-deftest mevedel-pending-inputs-schedule-held-input-delivery ()
  ,test
  (test)
  :doc "hold release wakes background mail without a queued follow-up"
  (mevedel-pending-inputs-test--with-event
    (let (timers)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (push (cons function args) timers))))
        (with-current-buffer view-buf
          (mevedel-pending-inputs-schedule-held-input-delivery)))
      (dolist (timer timers) (apply (car timer) (cdr timer)))
      (should (= 1 (length submitted)))))
  :doc "hold release offers follow-ups before background mail"
  (mevedel-pending-inputs-test--with-event
    (let (timers drained)
      (mevedel-session-set-pending-inputs
       session 'follow-up (list (list :input "queued")))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (push (cons function args) timers)))
                ((symbol-function 'mevedel-view--run-follow-up-drain)
                 (lambda (buffer) (push buffer drained))))
        (mevedel-pending-inputs-schedule-held-input-delivery data-buf)
        (dolist (timer timers) (apply (car timer) (cdr timer))))
      (should (equal (list data-buf) drained))
      (should-not submitted)))
  :doc "consumed mail never turns a hold-release offer into Goal continuation"
  (mevedel-pending-inputs-test--with-event
    (let (timers continued)
      (setf (mevedel-session-goal session)
            (mevedel-goal--create :id "g" :status 'active))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (push (cons function args) timers)))
                ((symbol-function 'mevedel-goal-schedule-continuation)
                 (lambda (&rest args) (push args continued))))
        (mevedel-pending-inputs-schedule-held-input-delivery data-buf)
        (should (= 1 (length timers)))
        (mevedel-agent-control-clear-wake session "/root" "exec-1")
        (dolist (timer timers) (apply (car timer) (cdr timer)))
        (setq timers nil)
        (mevedel-pending-inputs-schedule-held-input-delivery data-buf)
        (should-not timers))
      (should-not continued)
      (should-not submitted))))

(mevedel-deftest mevedel-pending-inputs--run-event-wake ()
  ,test
  (test)
  :doc "agent mail preserves a multiline draft and wakes when the draft is cleared"
  (mevedel-pending-inputs-test--with-session
    (let ((draft "> unsent draft\nsecond line") callbacks submitted)
      (setf (mevedel-session-root-buffer session) data-buf
            (mevedel-session-agent-registry session)
            (list (cons "/root/worker" (mevedel-agent-record--create :path "/root/worker"))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (push (cons function args) callbacks)))
                ((symbol-function 'mevedel-transport-run-when-idle)
                 (lambda (_key _path thunk &rest _) (funcall thunk) t))
                ((symbol-function 'mevedel--submit-generated-turn)
                 (lambda (&rest args) (push args submitted))))
        (with-current-buffer view-buf
          (mevedel-view-test--insert-composer-draft draft))
        (let ((mevedel--agent-invocation
               (mevedel-agent-invocation--create
                :path "/root/worker" :parent-session session)))
          (mevedel-agent-control-send-message session "/root" "report"))
        (while callbacks
          (let ((callback (pop callbacks))) (apply (car callback) (cdr callback))))
        (should-not submitted)
        (with-current-buffer view-buf
          (should (equal draft (mevedel-view-composer-draft-text nil t)))
          (with-current-buffer (mevedel-view-composer-buffer)
            (goto-char (mevedel-view-composer-draft-end))
            (insert " more")
            (delete-char -1)
            (should-not callbacks)
            (delete-region (mevedel-view-composer-draft-start)
                           (mevedel-view-composer-draft-end))
            (insert " ")
            (delete-char -1)))
        (should (= 1 (length callbacks)))
        (while callbacks
          (let ((callback (pop callbacks))) (apply (car callback) (cdr callback))))
        (should (= 1 (length submitted)))
        (should (mevedel-agent-control-wake-pending-p session)))))

  :doc "sending the blocking draft delivers the mail once in the user turn"
  (mevedel-pending-inputs-test--with-session
    (let ((root (make-temp-file "mevedel-draft-send-" t))
          timers prompts background-turns)
      (unwind-protect
          (progn
            (setf (mevedel-workspace-root workspace) root
                  (mevedel-session-root-buffer session) data-buf)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_time _repeat function &rest args)
                         (when (eq function #'mevedel-pending-inputs--offer-idle-work-now)
                           (push (cons function args) timers))))
                      ((symbol-function 'mevedel--submit-generated-turn)
                       (lambda (&rest args) (push args background-turns)))
                      ((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (let* ((data (list :messages []))
                                (fsm (gptel-make-fsm :info (list :buffer data-buf :data data))))
                           (dotimes (_ 2) (mevedel-tools--handle-message-inject fsm))
                           (setq prompts (append (plist-get data :messages) nil))))))
              (mevedel-agent-control-enqueue-execution-result session "/root" "done" "exec-1" t)
              (with-current-buffer view-buf
                (mevedel-view-test--insert-composer-draft "User question")
                (should-not (mevedel-pending-inputs--event-wake-view data-buf))
                (with-current-buffer (mevedel-view-composer-buffer)
                  (mevedel-view-send))
                (should (string-empty-p (mevedel-view-composer-draft-text nil t))))
              (dolist (timer timers) (apply (car timer) (cdr timer)))
              (should-not background-turns)
              (should (= 1 (length prompts)))
              (should (string-search "done" (plist-get (car prompts) :content)))
              (should-not (mevedel-agent-control-context-mailbox session))))
        (delete-directory root t))))

  :doc "starts one fixed, attributed turn and leaves the mail for the request"
  (mevedel-pending-inputs-test--with-event
    (mevedel-pending-inputs--run-event-wake data-buf)
    (should (equal `((,mevedel-pending-inputs--event-trigger
                      ,mevedel-pending-inputs--event-trigger
                      (:type background-event)))
                   submitted))
    (should (string-match-p "not a message from the user"
                            mevedel-pending-inputs--event-trigger))
    (with-current-buffer data-buf
      (should-not mevedel-view--pending-guest-attribution))
    ;; The request's inbox, not the prompt, delivers the result.
    (should (= 1 (length (mevedel-agent-control-context-mailbox session)))))
  :doc "two completions while idle start one turn"
  (mevedel-pending-inputs-test--with-event
    (mevedel-agent-control-enqueue-execution-result
     session "/root" "two" "exec-2" t)
    (cl-letf* ((submit (symbol-function 'mevedel--submit-generated-turn))
               ((symbol-function 'mevedel--submit-generated-turn)
                (lambda (&rest args)
                  (apply submit args)
                  ;; A started turn occupies the session.
                  (with-current-buffer data-buf
                    (setq-local mevedel--current-request
                                (mevedel-request--create :session session))))))
      (mevedel-pending-inputs--run-event-wake data-buf)
      (mevedel-pending-inputs--run-event-wake data-buf))
    (should (= 1 (length submitted))))
  :doc "each hold defers the wake"
  (dolist (hold
           (list
            (cons "draft"
                  (lambda (_session _data view)
                    (with-current-buffer view
                      (mevedel-view-test--insert-composer-draft "draft"))))
            (cons "follow-up"
                  (lambda (session _data _view)
                    (mevedel-session-set-pending-inputs
                     session 'follow-up (list (list :input "first")))))
            (cons "paused input"
                  (lambda (session _data _view)
                    (mevedel-session-set-pending-input-paused session t)))
            (cons "permission"
                  (lambda (session _data _view)
                    (setf (mevedel-session-permission-queue session)
                          (list (list :id "p")))))
            (cons "plan approval"
                  (lambda (session _data _view)
                    (setf (mevedel-session-pending-plan-approval session)
                          (list :id "plan"))))
            (cons "compaction"
                  (lambda (_session data _view)
                    (with-current-buffer data
                      (setq-local mevedel-compact-run-in-flight t))))
            (cons "busy turn"
                  (lambda (session data _view)
                    (with-current-buffer data
                      (setq-local mevedel--current-request
                                  (mevedel-request--create :session session)))))
            (cons "recovery blocker"
                  (lambda (session _data _view)
                    (setf (mevedel-session-recovery-issues session)
                          (list (list :blocking t :message "blocked")))))))
    (mevedel-pending-inputs-test--with-event
      (funcall (cdr hold) session data-buf view-buf)
      (cl-letf (((symbol-function 'mevedel-view-interaction-pending-p)
                 #'ignore))
        (mevedel-pending-inputs--run-event-wake data-buf))
      (ert-info ((car hold))
        (should-not submitted)
        (should (mevedel-agent-control-wake-pending-p session)))))
  :doc "admission rechecks holds after mutation authority yields"
  (dolist (hold '(draft busy follow-up consumed))
    (mevedel-pending-inputs-test--with-event
      (cl-letf (((symbol-function 'mevedel-session-artifacts-assert-new-mutation-authority)
                 (lambda (_session)
                   (pcase hold
                     ('draft
                      (with-current-buffer view-buf
                        (mevedel-view-test--insert-composer-draft "> draft\nsecond line")))
                     ('busy
                      (with-current-buffer data-buf
                        (setq-local mevedel--current-request
                                    (mevedel-request--create :session session))))
                     ('follow-up
                      (mevedel-session-set-pending-inputs
                       session 'follow-up (list (list :input "first"))))
                     ('consumed
                      (mevedel-agent-control-clear-wake session "/root" "exec-1"))))))
        (mevedel-pending-inputs--run-event-wake data-buf))
      (ert-info ((symbol-name hold))
        (should-not submitted))))
  :doc "a pending view interaction holds the wake"
  (mevedel-pending-inputs-test--with-event
    (cl-letf (((symbol-function 'mevedel-view-interaction-pending-p)
               (lambda (&rest _) t)))
      (mevedel-pending-inputs--run-event-wake data-buf))
    (should-not submitted))
  :doc "a failure latch with no queued input does not strand background mail"
  ;; Regression: a failed turn latched delivery; once its failed input was
  ;; resolved the latch remained until `R' or a user message, and child
  ;; results never woke the idle root in between.
  (mevedel-pending-inputs-test--with-event
    (mevedel-session-set-pending-input-failure-paused session t)
    (with-current-buffer view-buf
      (mevedel-view--interaction-rebuild))
    (mevedel-pending-inputs--run-event-wake data-buf)
    (should (= 1 (length submitted))))
  :doc "the failure latch still holds the wake while failed input awaits review"
  (dolist (category '(steering follow-up))
    (mevedel-pending-inputs-test--with-event
      (mevedel-session-enqueue-pending-input
       session category '(:input "review me" :request-id "dead"
                          :state failed-turn))
      (mevedel-session-set-pending-input-failure-paused session t)
      (with-current-buffer view-buf
        (mevedel-view--interaction-rebuild))
      (mevedel-pending-inputs--run-event-wake data-buf)
      (ert-info ((symbol-name category))
        (should-not submitted)
        (should (mevedel-agent-control-wake-pending-p session)))))
  :doc "no live view means no wake"
  (mevedel-pending-inputs-test--with-event
    (let (recorded)
      (cl-letf (((symbol-function 'mevedel-telemetry-record)
                 (lambda (_session event &rest props)
                   (push (cons event (plist-get props :reason)) recorded))))
        (with-current-buffer view-buf
          (mevedel-view-test--insert-composer-draft "draft"))
        ;; Repeated offers under one reason record it once.
        (mevedel-pending-inputs--run-event-wake data-buf)
        (mevedel-pending-inputs--run-event-wake data-buf)
        (mevedel-session-set-pending-inputs
         session 'follow-up (list (list :input "first")))
        (mevedel-pending-inputs--run-event-wake data-buf))
      (should-not submitted)
      (should (equal '((event-wake-refused . composer-draft)
                       (event-wake-refused . drain-blocked-queued))
                     (reverse recorded)))))
  :doc "a stale failure latch with queued steering reports the latch"
  (mevedel-pending-inputs-test--with-event
    (let (reasons)
      (mevedel-session-enqueue-pending-input
       session 'steering '(:input "held" :request-id "live"))
      (mevedel-session-set-pending-input-failure-paused session t)
      (cl-letf (((symbol-function 'mevedel-telemetry-record)
                 (lambda (_session _event &rest props)
                   (push (plist-get props :reason) reasons))))
        (mevedel-pending-inputs--run-event-wake data-buf))
      (should (equal '(drain-blocked-latch) reasons))))
  :doc "no live view means no wake"
  (mevedel-pending-inputs-test--with-event
    (with-current-buffer data-buf
      (setq-local mevedel--view-buffer nil))
    (mevedel-pending-inputs--run-event-wake data-buf)
    (should-not submitted))
  :doc "a failed start leaves the waking mail and clears the attribution"
  (mevedel-pending-inputs-test--with-event
    (let (reported)
      (cl-letf (((symbol-function 'mevedel--submit-generated-turn)
                 (lambda (&rest _) (error "Backend not ready"))))
        (mevedel-test--with-captured-diagnostics reported
          (mevedel-pending-inputs--run-event-wake data-buf)))
      (should (string-match-p "Backend not ready" reported))
      (should (mevedel-agent-control-wake-pending-p session))
      (with-current-buffer data-buf
        (should-not mevedel-view--pending-guest-attribution))))
  :doc "deferred event failure retains origin after buffer teardown"
  (mevedel-pending-inputs-test--with-session
    (let (deferred reported)
      (cl-letf (((symbol-function 'mevedel-transport-run-when-idle)
                 (lambda (_key _directory callback) (setq deferred callback)))
                ((symbol-function 'mevedel-pending-inputs--wake-for-event)
                 (lambda (_) (error "Backend not ready"))))
        (mevedel-pending-inputs--run-event-wake data-buf)
        (kill-buffer data-buf)
        (with-temp-buffer
          (setq-local mevedel--workspace
                      (mevedel-workspace--create :root "/tmp/other/pending-inputs"))
          (mevedel-test--with-captured-diagnostics reported
            (funcall deferred))))
      (should (string-prefix-p
               "mevedel [/tmp/pending-inputs]: " reported)))))

(mevedel-deftest mevedel-pending-inputs-offer-event-after-turn ()
  ,test
  (test)
  :doc "offers idle work only while a wake is pending"
  (mevedel-pending-inputs-test--with-session
    (let ((fsm (gptel-make-fsm :info (list :buffer data-buf)))
          offered)
      (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-idle-work)
                 (lambda (buffer) (push buffer offered))))
        (mevedel-pending-inputs-offer-event-after-turn fsm)
        (should-not offered)
        (mevedel-agent-control-enqueue-execution-result
         session "/root" "done" "exec-1" t)
        (mevedel-pending-inputs-offer-event-after-turn fsm)
        (should (equal (list data-buf) offered))))))

(provide 'test-mevedel-pending-inputs)
;;; test-mevedel-pending-inputs.el ends here
