;;; test-niwa-pending-inputs-dispatch.el --- Pending input dispatch tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests queued follow-up classification and delivery dispatch.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-pending-inputs)
(require 'niwa-structs)

(niwa-deftest niwa-view--pending-follow-up-kind ()
  ,test
  (test)
  :doc "uses the established dispatch precedence when capabilities overlap"
  (let ((submission (list :submission 'prepared)))
    (should
     (eq 'invocation
         (niwa-view--pending-follow-up-kind
          (append '(:guest-invoke "plan") submission))))
    (should (eq 'skills (niwa-view--pending-follow-up-kind
                          '(:guest-skills ("alpha" "beta")))))
    (should
     (eq 'prepared
         (niwa-view--pending-follow-up-kind submission)))
    (should
     (eq 'prompt
         (niwa-view--pending-follow-up-kind '(:input "plain"))))))

(niwa-deftest niwa-view--dispatch-follow-up-entry ()
  ,test
  (test)
  :doc "routes each explicit kind through its focused delivery function"
  (let ((entry '(:guest-invoke "plan" :guest-skills ("alpha" "beta") :submission prepared
                 :inert-skills t))
        (before #'ignore)
        (after #'ignore)
        (release #'ignore)
        calls)
    (cl-letf (((symbol-function 'niwa-view-run-invocation)
               (lambda (&rest args) (push (cons 'invocation args) calls)))
              ((symbol-function 'niwa-view--dispatch-prepared-outcome)
               (lambda (&rest args) (push (cons 'prepared args) calls)))
              ((symbol-function 'niwa-view--submit-planned-input)
               (lambda (&rest args) (push (cons 'prompt args) calls))))
      (dolist (kind '(invocation skills prepared prompt))
        (niwa-view--dispatch-follow-up-entry
         kind entry "input" 'data-buffer before after release)))
    (should
     (equal
      `((prompt "input" ,before ,release nil ,after t)
        (prepared prepared data-buffer
                  :before-send ,before :after-insert ,after :on-block ,release)
        (prompt "input" ,before ,release nil ,after nil ("alpha" "beta"))
        (invocation "plan" "input" :on-quiet ,after :on-sent ,after))
      calls))))

(provide 'test-niwa-pending-inputs-dispatch)
;;; test-niwa-pending-inputs-dispatch.el ends here
