;;; test-mevedel-pending-inputs-dispatch.el --- Pending input dispatch tests -*- lexical-binding: t -*-

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
(require 'mevedel-pending-inputs)
(require 'mevedel-structs)

(mevedel-deftest mevedel-view--pending-follow-up-kind ()
  ,test
  (test)
  :doc "uses the established dispatch precedence when capabilities overlap"
  (let ((submission (list :submission 'prepared)))
    (should
     (eq 'invocation
         (mevedel-view--pending-follow-up-kind
          (append '(:guest-invoke "plan") submission))))
    (should (eq 'skills (mevedel-view--pending-follow-up-kind
                          '(:guest-skills ("alpha" "beta")))))
    (should
     (eq 'prepared
         (mevedel-view--pending-follow-up-kind submission)))
    (should
     (eq 'prompt
         (mevedel-view--pending-follow-up-kind '(:input "plain"))))))

(mevedel-deftest mevedel-view--dispatch-follow-up-entry ()
  ,test
  (test)
  :doc "routes each explicit kind through its focused delivery function"
  (let ((entry '(:guest-invoke "plan" :guest-skills ("alpha" "beta") :submission prepared
                 :inert-skills t))
        (before #'ignore)
        (after #'ignore)
        (release #'ignore)
        calls)
    (cl-letf (((symbol-function 'mevedel-view-run-invocation)
               (lambda (&rest args) (push (cons 'invocation args) calls)))
              ((symbol-function 'mevedel-view--dispatch-prepared-outcome)
               (lambda (&rest args) (push (cons 'prepared args) calls)))
              ((symbol-function 'mevedel-view--submit-planned-input)
               (lambda (&rest args) (push (cons 'prompt args) calls))))
      (dolist (kind '(invocation skills prepared prompt))
        (mevedel-view--dispatch-follow-up-entry
         kind entry "input" 'data-buffer before after release)))
    (should
     (equal
      `((prompt "input" ,before ,release nil ,after t)
        (prepared prepared data-buffer
                  :before-send ,before :after-insert ,after :on-block ,release)
        (prompt "input" ,before ,release nil ,after nil ("alpha" "beta"))
        (invocation "plan" "input" :on-quiet ,after :on-sent ,after))
      calls))))

(provide 'test-mevedel-pending-inputs-dispatch)
;;; test-mevedel-pending-inputs-dispatch.el ends here
