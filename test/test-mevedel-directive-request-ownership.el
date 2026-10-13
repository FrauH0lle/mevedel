;;; test-mevedel-directive-request-ownership.el --- Admission races -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the real directive entry point across yielding startup seams.

;;; Code:

(require 'mevedel-directive-request)
(require 'mevedel-chat)
(require 'mevedel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-test--directive-startup-race (seam outcome &optional replace plan)
  "Exercise SEAM with OUTCOME, optionally REPLACE the reserved PLAN request."
  (let* ((root (file-name-as-directory (make-temp-file "directive-race-" t)))
         (file (file-name-concat root "source.txt"))
         (source (find-file-noselect file))
         chat first second inner-request inner-fsm inner-text inner-planning injected
         outer-request)
    (unwind-protect
        (with-current-buffer source
          (insert "first\nsecond\n")
          (write-region (point-min) (point-max) file nil 'silent)
          (set-buffer-modified-p nil)
          (setq first (mevedel--create-directive-in source 1 6 nil "First task")
                second (if (memq replace '(same settled)) first
                         (mevedel--create-directive-in source 7 13 nil "Second task"))
                chat (mevedel--chat-buffer nil t (mevedel-workspace)))
          (overlay-put first 'mevedel-directive-action 'implement)
          (overlay-put second 'mevedel-directive-action 'implement)
          (when plan (overlay-put first 'mevedel-directive-action 'plan))
          (cl-labels
              ((inject ()
                 (unless injected
                   (setq injected t
                         outer-request
                         (buffer-local-value 'mevedel--current-request chat))
                   (if replace
                       (progn
                         (with-current-buffer chat
                           (mevedel-request-end t))
                         (unless (eq replace 'abort)
                           (overlay-put second 'mevedel-directive-action 'implement)
                           (setq inner-fsm
                                 (mevedel--process-directive
                                  second '(:system "test") #'identity nil
                                  '(:permission-mode edits)))
                           (with-current-buffer chat
                             (setq inner-request mevedel--current-request)
                             (when (eq replace 'settled)
                               (goto-char (plist-get (gptel-fsm-info inner-fsm) :position))
                               (insert (propertize "Done.\n" 'gptel 'response))
                               (funcall (plist-get (gptel-fsm-info inner-fsm)
                                                   :mevedel-request-callback)
                                        nil inner-fsm)
                               (mevedel--turn-commit inner-fsm)
                               (mevedel-request-end))
                             (setq inner-text (buffer-string)
                                   inner-planning (list :directive-id "newer" :phase 'planning))
                             (setf (mevedel-session-directive-planning mevedel--session)
                                   inner-planning))))
                     (should-error
                      (mevedel--process-directive
                       second '(:system "test") #'identity nil
                       '(:permission-mode edits))
                      :type 'user-error))
                   (pcase outcome
                     ('error (error "Injected outer startup failure"))
                     ('quit (signal 'quit nil))))))
            (cl-letf (((symbol-function 'display-buffer) #'ignore)
                      ((symbol-function 'gptel--apply-preset) #'ignore)
                      ((symbol-function 'save-some-buffers)
                       (lambda (&rest _) (when (eq seam 'save) (inject))))
                      ((symbol-function 'gptel-request)
                       (lambda (_prompt &rest args)
                         (when (eq seam 'dispatch) (inject))
                         (let ((fsm (plist-get args :fsm)))
                           (setf (gptel-fsm-info fsm)
                                 (list :buffer chat :position (plist-get args :position)
                                       :callback #'ignore))
                           fsm))))
              (mevedel-test--with-captured-diagnostics nil
                (let (failure)
                  (condition-case err
                      (mevedel--process-directive
                       first '(:system "test") #'identity nil
                       '(:permission-mode full-auto))
                    (t (setq failure err)))
                  (if (or replace outcome)
                      (should failure)
                    (should-not failure))
                  (when (eq outcome 'quit)
                    (should (eq (car failure) 'quit)))))))
          (should injected)
          (should (buffer-live-p source))
          (should (overlay-buffer second))
          (with-current-buffer chat
            (cond
             ((and replace (not (eq replace 'abort)))
              (if (eq replace 'settled)
                  (progn
                    (should-not mevedel--current-request)
                    (should-not mevedel--current-directive-uuid))
                (should (eq inner-request mevedel--current-request))
                (should-not (mevedel-request-cancelled-p inner-request))
                (should (equal mevedel--current-directive-uuid
                               (overlay-get second 'mevedel-uuid))))
              (should (eq 'edits (mevedel-request-permission-mode-override inner-request)))
              (should (equal inner-text (buffer-string)))
              (should (eq inner-planning
                          (mevedel-session-directive-planning mevedel--session)))
              (should (eq (if (eq replace 'settled) 'implemented 'implementing)
                          (mevedel-directive-state (mevedel--directive-record second)))))
             ((or outcome (eq replace 'abort))
              (should-not mevedel--current-request)
              (should-not mevedel--current-directive-uuid)
              (should-not mevedel--directive-read-only-request-p)
              (should-not mevedel--pending-permission-mode)
              (should-not (mevedel-session-directive-planning mevedel--session))
              (should-not (mevedel--directive-status first))
              (should-not (mevedel-transcript-audit-records
                           (buffer-string) 'directive-turn-boundary)))
             (t
              (should (eq outer-request mevedel--current-request))
              (should (eq 'full-auto
                          (mevedel-request-permission-mode-override outer-request)))
              (should (equal mevedel--current-directive-uuid
                             (overlay-get first 'mevedel-uuid)))))))
      (when (buffer-live-p chat)
        (with-current-buffer chat
          (mevedel-test--with-captured-diagnostics nil
            (when mevedel--current-request (mevedel-request-end t)))
          (when (buffer-live-p mevedel--view-buffer) (kill-buffer mevedel--view-buffer)))
        (kill-buffer chat))
      (when (buffer-live-p source) (kill-buffer source))
      (delete-directory root t))))

(mevedel-deftest mevedel--process-directive-ownership
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "reservation prevents nested save-prompt admission and preserves the selected mode"
  (mevedel-test--directive-startup-race 'save nil)
  :doc "save failure rolls back its reservation after refusing nested admission"
  (mevedel-test--directive-startup-race 'save 'error)
  :doc "save resumption cannot dispatch after cancellation and replacement"
  (mevedel-test--directive-startup-race 'save nil t)
  :doc "save failure leaves replacement context and planning metadata intact"
  (mevedel-test--directive-startup-race 'save 'error t)
  :doc "planning startup failure preserves the newer workflow reservation"
  (mevedel-test--directive-startup-race 'save 'error t t)
  :doc "abort during the save prompt retires startup context without a replacement"
  (mevedel-test--directive-startup-race 'save nil 'abort t)
  :doc "abort during dispatch retires the provisional transcript without a replacement"
  (mevedel-test--directive-startup-race 'dispatch nil 'abort)
  :doc "late startup error preserves replacement transcript and same-directive status"
  (mevedel-test--directive-startup-race 'dispatch 'error 'same)
  :doc "late startup quit preserves replacement transcript and same-directive status"
  (mevedel-test--directive-startup-race 'dispatch 'quit 'same)
  :doc "late startup failure cannot roll back a replacement that already settled"
  (mevedel-test--directive-startup-race 'dispatch 'error 'settled))

(provide 'test-mevedel-directive-request-ownership)
;;; test-mevedel-directive-request-ownership.el ends here
