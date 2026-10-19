;;; test-mevedel-patch-review-remote.el --- Remote patch review tests -*- lexical-binding: t; -*-

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
(require 'mevedel-interaction-prompt)
(require 'mevedel-tool-patch)
(require 'mevedel-patch-review)
(require 'mevedel-pending-inputs)
(require 'mevedel-plan-mode)
(require 'mevedel-view)

;; `mevedel-collaboration'
(defvar mevedel-collaboration-remote-guest)

(defconst mevedel-test--remote-patch
  "*** Begin Patch\n*** Update File: lib/parser.el\n@@\n-old line\n+new line\n*** End Patch\n"
  "Minimal authored patch for remote review fixtures.")

(defun mevedel-test--remote-review-overlay ()
  "Stage a review for the fixture patch and return (OVERLAY . PROPOSAL)."
  (let ((proposal (mevedel-tool-patch-parse mevedel-test--remote-patch))
        overlay)
    (cl-letf (((symbol-function 'mevedel-tool-patch-annotate-line-numbers)
               #'ignore)
              ((symbol-function 'mevedel-view--interaction-target-buffer)
               (lambda (&optional _) (current-buffer)))
              ((symbol-function 'mevedel-patch-review--render)
               (lambda (_proposal)
                 (setq overlay (make-overlay (point-min) (point-min)
                                             (current-buffer) nil t))))
              ((symbol-function 'mevedel--prompt--register-canceller)
               #'ignore))
      (setq-local mevedel--prompt-overlays nil)
      (mevedel-patch-review-start proposal #'ignore (current-buffer)))
    (cons overlay proposal)))

(mevedel-deftest mevedel-patch-review--deselect-all
  (:doc "deselects every operation and hunk")
  (let* ((proposal (mevedel-tool-patch-parse mevedel-test--remote-patch))
         (operation (car (plist-get proposal :operations))))
    (plist-put operation :selected t)
    (dolist (hunk (plist-get operation :hunks))
      (plist-put hunk :selected t))
    (mevedel-patch-review--deselect-all proposal)
    (should-not (plist-get operation :selected))
    (should-not (cl-some (lambda (hunk) (plist-get hunk :selected))
                         (plist-get operation :hunks)))))

(mevedel-deftest mevedel-patch-review-start/remote
  (:doc "declares the remote surface: diff body, Apply, and revision feedback")
  (with-temp-buffer
    (pcase-let ((`(,overlay . ,proposal)
                 (mevedel-test--remote-review-overlay)))
      (let ((remote (overlay-get overlay 'mevedel--remote))
            submitted)
        (should (equal "diff" (plist-get remote :body-kind)))
        (should (string-match-p "\\+new line" (plist-get remote :body)))
        (should (equal '("Apply 1 change in 1 file")
                       (mapcar #'cdr (plist-get remote :options))))
        (cl-letf (((symbol-function 'mevedel-patch-review--submit)
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

(mevedel-deftest mevedel-patch-review--refresh-remote
  (:doc "publishes current selections and edits without republishing unchanged folds")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((proposal (mevedel-tool-patch-parse
                      (concat "*** Begin Patch\n*** Add File: one.txt\n+first\n"
                              "*** Add File: two.txt\n+second\n*** End Patch\n")))
           (first (car (plist-get proposal :operations)))
           (second (cadr (plist-get proposal :operations)))
           (announcements 0))
      (cl-letf (((symbol-function 'mevedel--prompt-announce)
                 (lambda (_) (cl-incf announcements))))
        (mevedel-patch-review-start proposal #'ignore data-buf)
        (let* ((overlay (plist-get proposal :overlay))
               (original (overlay-get overlay 'mevedel--remote)))
          (with-current-buffer view-buf
            (goto-char (point-max))
            (insert "> draft\nsecond line"))
          (should (= 1 announcements))
          (should (string-search "+first" (plist-get original :body)))
          (plist-put first :selected nil)
          (plist-put first :feedback "Keep first unchanged")
          (plist-put second :content "host edited\n")
          (plist-put second :modified t)
          (mevedel-patch-review--render proposal)
          (let ((remote (overlay-get overlay 'mevedel--remote)))
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
          (mevedel-patch-review--render proposal)
          (should (= 2 announcements))
          (should (eq overlay (plist-get proposal :overlay)))
          (with-current-buffer view-buf
            (should (equal "> draft\nsecond line"
                           (mevedel-view--input-text))))
          (mevedel-patch-review--deselect-all proposal)
          (mevedel-patch-review--render proposal)
          (let ((remote (overlay-get overlay 'mevedel--remote)))
            (should (string-empty-p (plist-get remote :body)))
            (should (equal "Request revision · 1 comment"
                           (cdar (plist-get remote :options)))))))))

  :doc "publishes failure and active-parent warnings on the existing request"
  (mevedel-view-test--with-buffers
    (let ((proposal (mevedel-tool-patch-parse
                     "*** Begin Patch\n*** Add File: one.txt\n+first\n*** End Patch\n"))
          attempted)
      (cl-letf (((symbol-function 'mevedel-side-conversation-mutation-warning)
                 (lambda (&rest _) "Parent conversation is active.\n")))
        (mevedel-patch-review-start proposal #'ignore data-buf)
        (let ((overlay (plist-get proposal :overlay)))
          (should (string-search "Parent conversation is active"
                                 (plist-get (overlay-get overlay 'mevedel--remote)
                                            :notice)))
          (cl-letf (((symbol-function 'mevedel-tool-patch-assert-baseline)
                     (lambda (_)
                       (setq attempted t)
                       (signal 'mevedel-tool-patch-partial-rollback
                               '("Could not restore one.txt")))))
            (mevedel-patch-review--submit proposal))
          (should attempted)
          (should-not (plist-get proposal :state))
          (let ((remote (overlay-get overlay 'mevedel--remote)))
            (should (string-search "Conflict: Patch rollback incomplete"
                                   (plist-get remote :notice)))
            (should (string-search "Could not restore one.txt"
                                   (plist-get remote :notice)))
            (should (string-search "Rollback was incomplete"
                                   (plist-get remote :notice)))
            (should (plist-get remote :options)))))))

  :doc "bounds escaped multibyte presentation while retaining leading warnings"
  (with-temp-buffer
    (pcase-let ((`(,overlay . ,proposal)
                 (mevedel-test--remote-review-overlay)))
      (let* ((operation (car (plist-get proposal :operations)))
             (hunk (car (plist-get operation :hunks)))
             (large (apply #'concat (make-list 14000 "漢\t\"\001"))))
        (plist-put hunk :diff-lines (list "-old line" (concat "+" large)))
        (plist-put proposal :feedback large)
        (plist-put proposal :conflict "Stale file")
        (mevedel-patch-review--refresh-remote proposal)
        (let ((remote (overlay-get overlay 'mevedel--remote)))
          (should (<= (string-bytes
                       (json-serialize
                        (list :body (plist-get remote :body)
                              :notice (plist-get remote :notice))))
                      58000))
          (should (string-search "Conflict: Stale file" (plist-get remote :notice)))
          (should (string-search "[truncated" (plist-get remote :notice)))
          (should (string-search "[truncated" (plist-get remote :body))))))))

(mevedel-deftest mevedel-plan-mode--remote-feedback
  (:doc "demotes the proposal and queues the templated revision request with attribution")
  (with-temp-buffer
    (let ((chat-buffer (current-buffer))
          (mevedel-collaboration-remote-guest "Herr Boing")
          demoted queued)
      (setq-local mevedel-plan--relative-current-path "plans/current.md")
      (cl-letf (((symbol-function 'mevedel-plan-mode--demote-proposal)
                 (lambda (_session _discard) (setq demoted t)))
                ((symbol-function 'mevedel-plan-resource-address)
                 (lambda (path) (format "plan://%s" path)))
                ((symbol-function 'mevedel-view-enqueue-external-follow-up)
                 (lambda (buffer text &rest keys)
                   (setq queued (list buffer text
                                      (plist-get keys :guest-name))))))
        (mevedel-plan-mode--approval-callback
         "plan md" chat-buffer 'session
         '(:remote-feedback "split the second step")))
      (should demoted)
      (should (eq chat-buffer (nth 0 queued)))
      (should (string-match-p "Plan feedback:\n\nsplit the second step"
                              (nth 1 queued)))
      (should (string-match-p "plan://plans/current.md" (nth 1 queued)))
      (should (equal "Herr Boing" (nth 2 queued))))))

;;; test-mevedel-patch-review-remote.el ends here
