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
     (eq 'directive
         (mevedel-view--pending-follow-up-kind
          (append '(:scope (:directive-id "d1") :guest-invoke "plan")
                  submission))))
    (should
     (eq 'invocation
         (mevedel-view--pending-follow-up-kind
          (append '(:guest-invoke "plan") submission))))
    (should
     (eq 'prepared
         (mevedel-view--pending-follow-up-kind submission)))
    (should
     (eq 'prompt
         (mevedel-view--pending-follow-up-kind '(:input "plain"))))))

(mevedel-deftest mevedel-view--dispatch-directive-follow-up ()
  ,test
  (test)

  :doc "activates authority and commits a successful directive turn"
  (let* ((session (mevedel-session--create :name "directive"))
         (entry '(:scope (:directive-id "d1" :action discuss)))
         calls)
    (cl-letf (((symbol-function 'mevedel-view--dispatch-directive-input)
               (lambda (scope input)
                 (push (list 'dispatch scope input) calls))))
      (mevedel-view--dispatch-directive-follow-up
       entry "question" session
       (lambda () (push 'before calls))
       (lambda () (push 'after calls))
       (lambda () (push 'release calls))))
    (should
     (equal '(after
              (dispatch (:directive-id "d1" :action discuss) "question")
              before)
            calls)))

  :doc "drops a permanently stale directive entry and schedules its successor"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "stale" :root "/tmp/stale"
                     :name "stale"))
         (session (mevedel-session-create "main" workspace))
         (entry '(:input "question"
                  :scope (:directive-id "missing" :action discuss)))
         released scheduled)
    (mevedel-session-set-pending-inputs session 'follow-up (list entry))
    (cl-letf (((symbol-function 'mevedel-view--dispatch-directive-input)
               (lambda (&rest _) (error "Missing directive")))
              ((symbol-function 'mevedel-view--interaction-rebuild) #'ignore)
              ((symbol-function 'mevedel-view--schedule-late-follow-up-drain)
               (lambda () (setq scheduled t))))
      (mevedel-test--with-captured-messages nil
        (mevedel-view--dispatch-directive-follow-up
         entry "question" session #'ignore #'ignore
         (lambda () (setq released t)))))
    (should released)
    (should scheduled)
    (should-not (mevedel-session-pending-follow-ups session)))

  :doc "retains a transiently failing directive entry for a later drain"
  (let* ((directive (mevedel-directive--create :id "d1" :request "Question"))
         (workspace (mevedel-workspace--create
                     :type 'file :id "retry" :root "/tmp/retry"
                     :name "retry" :directives (list directive)))
         (session (mevedel-session-create "main" workspace))
         (entry '(:input "question"
                  :scope (:directive-id "d1" :action discuss)))
         released scheduled)
    (mevedel-session-set-pending-inputs session 'follow-up (list entry))
    (cl-letf (((symbol-function 'mevedel-view--dispatch-directive-input)
               (lambda (&rest _) (error "Try later")))
              ((symbol-function 'mevedel-view--interaction-rebuild) #'ignore)
              ((symbol-function 'mevedel-view--schedule-late-follow-up-drain)
               (lambda () (setq scheduled t))))
      (mevedel-test--with-captured-messages nil
        (mevedel-view--dispatch-directive-follow-up
         entry "question" session #'ignore #'ignore
         (lambda () (setq released t)))))
    (should released)
    (should-not scheduled)
    (should (equal (list entry)
                   (mevedel-session-pending-follow-ups session)))))

(mevedel-deftest mevedel-view--dispatch-follow-up-entry ()
  ,test
  (test)
  :doc "routes each explicit kind through its focused delivery function"
  (let ((entry '(:scope (:directive-id "d1")
                 :guest-invoke "plan" :submission prepared
                 :inert-skills t))
        (session (mevedel-session--create :name "dispatch"))
        (before #'ignore)
        (after #'ignore)
        (release #'ignore)
        calls)
    (cl-letf (((symbol-function 'mevedel-view--dispatch-directive-follow-up)
               (lambda (&rest args) (push (cons 'directive args) calls)))
              ((symbol-function 'mevedel-view-run-invocation)
               (lambda (&rest args) (push (cons 'invocation args) calls)))
              ((symbol-function 'mevedel-view--dispatch-prepared-outcome)
               (lambda (&rest args) (push (cons 'prepared args) calls)))
              ((symbol-function 'mevedel-view--submit-planned-input)
               (lambda (&rest args) (push (cons 'prompt args) calls))))
      (dolist (kind '(directive invocation prepared prompt))
        (mevedel-view--dispatch-follow-up-entry
         kind entry "input" session 'data-buffer before after release)))
    (should
     (equal
      `((prompt "input" ,before ,release nil ,after t)
        (prepared prepared data-buffer
                  :before-send ,before :after-insert ,after :on-block ,release)
        (invocation "plan" "input" :on-quiet ,after :on-sent ,after)
        (directive ,entry "input" ,session ,before ,after ,release))
      calls))))

(provide 'test-mevedel-pending-inputs-dispatch)
;;; test-mevedel-pending-inputs-dispatch.el ends here
