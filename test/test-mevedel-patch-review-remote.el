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
(require 'mevedel-plan-mode)

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
        (should (equal '("Apply patch")
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
