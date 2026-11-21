;;; test-niwa-patch-review-remote.el --- Remote patch review tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused coverage for the ApplyPatch review's remote surface: the
;; whole-call Apply and revision-request closures, and the plan
;; approval's remote feedback outcome.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-interaction-prompt)
(require 'niwa-tool-patch)
(require 'niwa-patch-review)
(require 'niwa-pending-inputs)
(require 'niwa-plan-mode)
(require 'niwa-view)

;; `niwa-collaboration'
(defvar niwa-collaboration-remote-guest)

(defconst niwa-test--remote-patch
  "*** Begin Patch\n*** Update File: lib/parser.el\n@@\n-old line\n+new line\n*** End Patch\n"
  "Minimal authored patch for remote review fixtures.")

(defun niwa-test--remote-review-overlay ()
  "Stage a review for the fixture patch and return (OVERLAY . PROPOSAL)."
  (let ((proposal (niwa-tool-patch-parse niwa-test--remote-patch))
        overlay)
    (cl-letf (((symbol-function 'niwa-tool-patch-annotate-line-numbers)
               #'ignore)
              ((symbol-function 'niwa-view--interaction-target-buffer)
               (lambda (&optional _) (current-buffer)))
              ((symbol-function 'niwa-patch-review--render)
               (lambda (_proposal)
                 (setq overlay (make-overlay (point-min) (point-min)
                                             (current-buffer) nil t))))
              ((symbol-function 'niwa--prompt--register-canceller)
               #'ignore))
      (setq-local niwa--prompt-overlays nil)
      (niwa-patch-review-start proposal #'ignore (current-buffer)))
    (cons overlay proposal)))

(niwa-deftest niwa-patch-review--deselect-all
  (:doc "deselects every operation and hunk")
  (let* ((proposal (niwa-tool-patch-parse niwa-test--remote-patch))
         (operation (car (plist-get proposal :operations))))
    (plist-put operation :selected t)
    (dolist (hunk (plist-get operation :hunks))
      (plist-put hunk :selected t))
    (niwa-patch-review--deselect-all proposal)
    (should-not (plist-get operation :selected))
    (should-not (cl-some (lambda (hunk) (plist-get hunk :selected))
                         (plist-get operation :hunks)))))

(niwa-deftest niwa-patch-review-start/remote
  (:doc "declares the remote surface: diff body, Apply, and revision feedback")
  (with-temp-buffer
    (pcase-let ((`(,overlay . ,proposal)
                 (niwa-test--remote-review-overlay)))
      (let ((remote (overlay-get overlay 'niwa--remote))
            submitted)
        (should (equal "diff" (plist-get remote :body-kind)))
        (should (string-match-p "\\+new line" (plist-get remote :body)))
        (should (equal '("Apply 1 change in 1 file")
                       (mapcar #'cdr (plist-get remote :options))))
        (cl-letf (((symbol-function 'niwa-patch-review--submit)
                   (lambda (p) (setq submitted p))))
          ;; Apply submits the staged selection unchanged.
          (funcall (car (car (plist-get remote :options))))
          (should (eq proposal submitted))
          ;; Feedback becomes a whole-patch revision request: the text
          ;; lands on the proposal and nothing stays selected.
          (setq submitted nil)
          (dolist (hunk (plist-get (car (plist-get proposal :operations))
                                   :hunks))
            (plist-put hunk :selected t))
          (funcall (plist-get remote :feedback) "keep the old guard")
          (should (eq proposal submitted))
          (should (equal "keep the old guard"
                         (plist-get proposal :feedback)))
          (should-not (cl-some (lambda (hunk) (plist-get hunk :selected))
                               (plist-get (car (plist-get proposal
                                                          :operations))
                                          :hunks))))))))

(niwa-deftest niwa-patch-review--refresh-remote
  (:doc "publishes current selections and edits without republishing unchanged folds")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((proposal (niwa-tool-patch-parse
                      (concat "*** Begin Patch\n*** Add File: one.txt\n+first\n"
                              "*** Add File: two.txt\n+second\n*** End Patch\n")))
           (first (car (plist-get proposal :operations)))
           (second (cadr (plist-get proposal :operations)))
           (announcements 0))
      (cl-letf (((symbol-function 'niwa--prompt-announce)
                 (lambda (_) (cl-incf announcements))))
        (niwa-patch-review-start proposal #'ignore data-buf)
        (let* ((overlay (plist-get proposal :overlay))
               (original (overlay-get overlay 'niwa--remote)))
          (with-current-buffer view-buf
            (niwa-view-test--insert-composer-draft "> draft\nsecond line"))
          (should (= 1 announcements))
          (should (string-search "+first" (plist-get original :body)))
          (plist-put first :selected nil)
          (plist-put first :feedback "Keep first unchanged")
          (plist-put second :content "host edited\n")
          (plist-put second :modified t)
          (niwa-patch-review--render proposal)
          (let ((remote (overlay-get overlay 'niwa--remote)))
            (should-not (string-search "+first" (plist-get remote :body)))
            (should-not (string-search "+second" (plist-get remote :body)))
            (should (string-search "+host edited" (plist-get remote :body)))
            (should (string-search "Keep first unchanged" (plist-get remote :notice)))
            (should (string-search "host selection" (plist-get remote :notice)))
            (should (string-search "send 1 comment" (cdar (plist-get remote :options))))
            (should (eq (caar (plist-get original :options))
                        (caar (plist-get remote :options)))))
          (should (= 2 announcements))
          (plist-put second :expanded t)
          (niwa-patch-review--render proposal)
          (should (= 2 announcements))
          (should (eq overlay (plist-get proposal :overlay)))
          (with-current-buffer view-buf
            (should (equal "> draft\nsecond line"
                           (niwa-view-composer-draft-text nil t))))
          (niwa-patch-review--deselect-all proposal)
          (niwa-patch-review--render proposal)
          (let ((remote (overlay-get overlay 'niwa--remote)))
            (should (string-empty-p (plist-get remote :body)))
            (should (equal "Request revision · 1 comment"
                           (cdar (plist-get remote :options)))))))))

  :doc "publishes failure and active-parent warnings on the existing request"
  (niwa-view-test--with-buffers
    (let ((proposal (niwa-tool-patch-parse
                     "*** Begin Patch\n*** Add File: one.txt\n+first\n*** End Patch\n"))
          attempted)
      (cl-letf (((symbol-function 'niwa-side-conversation-mutation-warning)
                 (lambda (&rest _) "Parent conversation is active.\n")))
        (niwa-patch-review-start proposal #'ignore data-buf)
        (let ((overlay (plist-get proposal :overlay)))
          (should (string-search "Parent conversation is active"
                                 (plist-get (overlay-get overlay 'niwa--remote)
                                            :notice)))
          (cl-letf (((symbol-function 'niwa-tool-patch-assert-baseline)
                     (lambda (_)
                       (setq attempted t)
                       (signal 'niwa-tool-patch-partial-rollback
                               '("Could not restore one.txt")))))
            (niwa-patch-review--submit proposal))
          (should attempted)
          (should-not (plist-get proposal :state))
          (let ((remote (overlay-get overlay 'niwa--remote)))
            (should (string-search "Conflict: Could not apply the selected changes"
                                   (plist-get remote :notice)))
            (should-not (string-search "Could not restore one.txt"
                                       (plist-get remote :notice)))
            (should (string-search "Could not restore one.txt"
                                   (plist-get proposal :conflict)))
            (should (string-search "Rollback was incomplete"
                                   (plist-get remote :notice)))
            (should (plist-get remote :options)))))))

  :doc "bounds escaped multibyte presentation while retaining leading warnings"
  (with-temp-buffer
    (pcase-let ((`(,overlay . ,proposal)
                 (niwa-test--remote-review-overlay)))
      (let* ((operation (car (plist-get proposal :operations)))
             (hunk (car (plist-get operation :hunks)))
             (large (apply #'concat (make-list 14000 "漢\t\"\001"))))
        (plist-put hunk :diff-lines (list "-old line" (concat "+" large)))
        (plist-put proposal :feedback large)
        (plist-put proposal :conflict "Stale file")
        (niwa-patch-review--refresh-remote proposal)
        (let ((remote (overlay-get overlay 'niwa--remote)))
          (should (<= (string-bytes
                       (json-serialize
                        (list :body (plist-get remote :body)
                              :notice (plist-get remote :notice))))
                      58000))
          (should (string-search "Conflict: Could not apply the selected changes"
                                 (plist-get remote :notice)))
          (should (string-search "[truncated" (plist-get remote :notice)))
          (should (string-search "[truncated" (plist-get remote :body))))))))

(niwa-deftest niwa-patch-review--remote-conflict-privacy
  (:doc "A full guest's stale-buffer refusal retains native detail without sending host paths" :quiet t)
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "private-host-project-" t)))
           (path (file-name-concat root "relative.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "privacy"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "privacy" workspace root))
           (guests (make-hash-table :test #'eql))
           (room (list :transport 'transport :session session :guests guests
                       :data-buffer data-buf :ui-requests (make-hash-table :test #'eql)
                       :push-guests (make-hash-table :test #'equal)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           (niwa-collaboration-remote-interactions t)
           (niwa-interaction-prompt-created-hook
            '(niwa-collaboration--on-prompt-created))
           sent visited result)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent) t))
                    ((symbol-function 'niwa-collaboration--transport-control)
                     #'ignore))
            (puthash 1 '(:name "Full guest" :writable t :ready t) guests)
            (with-temp-file path (insert "old line\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch (concat "*** Begin Patch\n*** Update File: relative.txt\n"
                                    "@@\n-old line\n+new line\n*** End Patch\n"))))
            (let* ((initial (cdr (car sent)))
                   (req-id (plist-get initial :reqId))
                   (overlay (gethash req-id (plist-get room :ui-requests))))
              (should (equal "ui-request" (plist-get initial :t)))
              (setq visited (find-file-noselect path))
              (with-current-buffer visited
                (goto-char (point-max)) (insert "unsaved change\n"))
              (setq sent nil)
              (niwa-collaboration--dispatch-frame
               room 1 (list :t "ui-response" :reqId req-id :option 0))
              (should-not result)
              (should (eq overlay (gethash req-id (plist-get room :ui-requests))))
              (with-current-buffer view-buf
                (should (string-search path (buffer-string))))
              (let ((update (cdr (seq-find
                                  (lambda (entry)
                                    (equal "ui-request" (plist-get (cdr entry) :t)))
                                  sent))))
                (should update)
                (should (string-search "Conflict:" (plist-get update :notice)))
                (should (string-search "relative.txt" (plist-get update :body)))
                (should-not (string-search root (json-encode update)))))
            (with-current-buffer visited
              (should (buffer-modified-p))
              (should (string-search "unsaved change" (buffer-string))))
            (should (equal "old line\n"
                           (with-temp-buffer (insert-file-contents path) (buffer-string)))))
        (when (buffer-live-p visited)
          (with-current-buffer visited (set-buffer-modified-p nil))
          (kill-buffer visited))
        (delete-directory root t)))))

(niwa-deftest niwa-plan-mode--remote-feedback
  (:doc "demotes the proposal and queues the templated revision request with attribution")
  (with-temp-buffer
    (let ((chat-buffer (current-buffer))
          (niwa-collaboration-remote-guest "Herr Boing")
          demoted queued)
      (setq-local niwa-plan--relative-current-path "plans/current.md")
      (cl-letf (((symbol-function 'niwa-plan-mode--demote-proposal)
                 (lambda (_session _discard) (setq demoted t)))
                ((symbol-function 'niwa-plan-resource-address)
                 (lambda (path) (format "plan://%s" path)))
                ((symbol-function 'niwa-view-enqueue-external-follow-up)
                 (lambda (buffer text &rest keys)
                   (setq queued (list buffer text
                                      (plist-get keys :guest-name))))))
        (niwa-plan-mode--approval-callback
         "plan md" chat-buffer 'session
         '(:remote-feedback "split the second step")))
      (should demoted)
      (should (eq chat-buffer (nth 0 queued)))
      (should (string-match-p "Plan feedback:\n\nsplit the second step"
                              (nth 1 queued)))
      (should (string-match-p "plan://plans/current.md" (nth 1 queued)))
      (should (equal "Herr Boing" (nth 2 queued))))))

;;; test-niwa-patch-review-remote.el ends here
