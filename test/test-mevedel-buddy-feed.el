;;; test-mevedel-buddy-feed.el --- Tests for /buddy -*- lexical-binding: t -*-

;;; Commentary:

;; `/buddy' runs through the ordinary slash-command dispatcher in a chat
;; buffer holding a real session, plan artifact and transcript.

;;; Code:

(require 'mevedel-buddy)
(require 'mevedel-buddy-feed)
(require 'mevedel-plan)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-buddy-feed-test--dispatch (session directory transcript command)
  "Dispatch COMMAND after TRANSCRIPT in a chat buffer for SESSION.
DIRECTORY is the buffer's `default-directory'.  TRANSCRIPT is inserted
as-is, so it may carry gptel response properties.  Return the echoed
messages."
  (let (messages)
    (with-temp-buffer
      (setq-local mevedel--session session
                  default-directory (file-name-as-directory directory))
      (insert transcript command)
      (goto-char (point-max))
      (mevedel-test--with-captured-messages messages
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))))
    messages))

(defun mevedel-buddy-feed-test--brief (directory)
  "Return the (SOURCE . TEXT) brief stored for DIRECTORY's project."
  (gethash (mevedel-buddy-directory-scope-key directory)
           mevedel-buddy--briefs))

(mevedel-deftest mevedel-buddy-feed-slash-command
  (:vars* ((root (make-temp-file "mevedel-buddy-feed-" t))
           (project (let ((dir (file-name-concat root "project")))
                      (make-directory (file-name-concat dir ".git") t)
                      dir))
           (save-dir (let ((dir (file-name-concat root "session")))
                       (make-directory dir t)
                       dir))
           (session (mevedel-session--create
                     :authority-mode 'pid-lock :name "test"
                     :save-path save-dir))
           (exchange (concat "Add retries to the fetcher\n"
                             (propertize "Use exponential backoff.\n"
                                         'gptel 'response))))
   :after-each ((mevedel-buddy-global-mode -1)
                (clrhash mevedel-buddy--briefs)
                (delete-directory root t)))
  ,test
  (test)

  :doc "in Plan mode the current plan becomes the brief and Plan is untouched"
  (progn
    (with-temp-buffer
      (mevedel-plan-write-current "# Retry plan\n\n- back off" session
                                  (current-buffer)))
    (setf (mevedel-session-plan-mode session) t)
    (let ((metadata (copy-tree (mevedel-session-plan-metadata session)))
          (messages (mevedel-buddy-feed-test--dispatch
                     session project exchange "/buddy")))
      (should (equal '("the current plan" . "# Retry plan\n\n- back off")
                     (mevedel-buddy-feed-test--brief project)))
      (should mevedel-buddy-global-mode)
      (should (string-match-p "Buddy is watching .* with the current plan"
                              messages))
      (should (mevedel-session-plan-mode session))
      (should (equal metadata (mevedel-session-plan-metadata session)))))

  :doc "an accepted plan is stale; an unfinished Goal's objective is used"
  (progn
    (with-temp-buffer
      (mevedel-plan-write-current "# Old plan" session (current-buffer)))
    (setf (mevedel-session-plan-mode session) t
          (mevedel-session-plan-metadata session)
          (plist-put (mevedel-session-plan-metadata session)
                     :status 'accepted)
          (mevedel-session-goal session)
          (mevedel-goal--create :objective "Ship retries" :status 'paused))
    (mevedel-buddy-feed-test--dispatch session project exchange "/buddy")
    (should (equal '("the session's Goal" . "Ship retries")
                   (mevedel-buddy-feed-test--brief project))))

  :doc "without plan or unfinished Goal the latest exchange is used"
  (progn
    (setf (mevedel-session-goal session)
          (mevedel-goal--create :objective "Done" :status 'complete))
    (mevedel-buddy-feed-test--dispatch
     session project (concat "Earlier question\n"
                             (propertize "Earlier answer\n" 'gptel 'response)
                             exchange)
     "/buddy")
    (should (equal (cons "the latest exchange"
                         (concat "Latest request:\nAdd retries to the fetcher"
                                 "\n\nLatest response:\nUse exponential backoff."))
                   (mevedel-buddy-feed-test--brief project))))

  :doc "explicit text outranks the plan, and a second /buddy replaces the brief"
  (progn
    (with-temp-buffer
      (mevedel-plan-write-current "# Plan" session (current-buffer)))
    (setf (mevedel-session-plan-mode session) t)
    (mevedel-buddy-feed-test--dispatch session project "" "/buddy")
    (mevedel-buddy-feed-test--dispatch session project ""
                                       "/buddy parse the config first")
    (should (equal '("the /buddy request" . "parse the config first")
                   (mevedel-buddy-feed-test--brief project))))

  :doc "/buddy off forgets the brief and turns the global mode off"
  (progn
    (mevedel-buddy-feed-test--dispatch session project "" "/buddy write it")
    (should mevedel-buddy-global-mode)
    (should (string-match-p "Buddy off"
                            (mevedel-buddy-feed-test--dispatch
                             session project "" "/buddy off")))
    (should-not (mevedel-buddy-feed-test--brief project))
    (should-not mevedel-buddy-global-mode))

  :doc "an empty session has nothing to feed and changes nothing"
  (progn
    (should-error (mevedel-buddy-feed-test--dispatch session project "" "/buddy")
                  :type 'user-error)
    (should-not (mevedel-buddy-feed-test--brief project))
    (should-not mevedel-buddy-global-mode))

  :doc "a session outside a project is refused, but can still turn Buddy off"
  (progn
    (should-error (mevedel-buddy-feed-test--dispatch session save-dir ""
                                                     "/buddy write it")
                  :type 'user-error)
    (should-not mevedel-buddy-global-mode)
    (mevedel-buddy-global-mode 1)
    (mevedel-buddy-feed-test--dispatch session save-dir "" "/buddy off")
    (should-not mevedel-buddy-global-mode)))

(provide 'test-mevedel-buddy-feed)
;;; test-mevedel-buddy-feed.el ends here
