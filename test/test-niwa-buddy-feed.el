;;; test-niwa-buddy-feed.el --- Tests for /buddy -*- lexical-binding: t -*-

;;; Commentary:

;; `/buddy' runs through the ordinary slash-command dispatcher in a chat
;; buffer holding a real session, plan artifact and transcript.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))

(require 'niwa-buddy)
(require 'niwa-buddy-feed)
(require 'niwa-plan)
(require 'niwa-session-persistence)
(require 'niwa-skills-ui)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun niwa-buddy-feed-test--dispatch (session directory transcript command)
  "Dispatch COMMAND after TRANSCRIPT in a chat buffer for SESSION.
DIRECTORY is the buffer's `default-directory'.  TRANSCRIPT is inserted
as-is, so it may carry gptel response properties.  Return the echoed
messages."
  (let (messages)
    (with-temp-buffer
      (setq-local niwa--session session
                  default-directory (file-name-as-directory directory))
      (insert transcript command)
      (goto-char (point-max))
      (niwa-test--with-captured-messages messages
        (should (eq 'local (niwa-skills--dispatch-slash-command)))))
    messages))

(defun niwa-buddy-feed-test--brief (directory)
  "Return the (SOURCE . TEXT) brief stored for DIRECTORY's project."
  (gethash (niwa-buddy-directory-scope-key directory)
           niwa-buddy--briefs))

(niwa-deftest niwa-buddy-feed-slash-command
  (:vars* ((root (make-temp-file "niwa-buddy-feed-" t))
           (project (let ((dir (file-name-concat root "project")))
                      (make-directory (file-name-concat dir ".git") t)
                      dir))
           (save-dir (let ((dir (file-name-concat root "session")))
                       (make-directory dir t)
                       dir))
           (session (test-niwa-session-persistence--leased-session save-dir
                     :name "test"
                     ))
           (exchange (concat "Add retries to the fetcher\n"
                             (propertize "Use exponential backoff.\n"
                                         'gptel 'response))))
   :after-each ((niwa-buddy-global-mode -1)
                (clrhash niwa-buddy--briefs)
                (niwa-session-persistence-lock-release save-dir session)
                (niwa-workspace-clear-registry)
                (delete-directory root t)))
  ,test
  (test)

  :doc "in Plan mode the current plan becomes the brief and Plan is untouched"
  (progn
    (with-temp-buffer
      (niwa-plan-write-current "# Retry plan\n\n- back off" session
                                  (current-buffer)))
    (setf (niwa-session-plan-mode session) t)
    (let ((metadata (copy-tree (niwa-session-plan-metadata session)))
          (messages (niwa-buddy-feed-test--dispatch
                     session project exchange "/buddy")))
      (should (equal '("the current plan" . "# Retry plan\n\n- back off")
                     (niwa-buddy-feed-test--brief project)))
      (should niwa-buddy-global-mode)
      (should (string-match-p "Buddy is watching .* with the current plan"
                              messages))
      (should (niwa-session-plan-mode session))
      (should (equal metadata (niwa-session-plan-metadata session)))))

  :doc "an accepted plan is stale; an unfinished Goal's objective is used"
  (progn
    (with-temp-buffer
      (niwa-plan-write-current "# Old plan" session (current-buffer)))
    (setf (niwa-session-plan-mode session) t
          (niwa-session-plan-metadata session)
          (plist-put (niwa-session-plan-metadata session)
                     :status 'accepted)
          (niwa-session-goal session)
          (niwa-goal--create :objective "Ship retries" :status 'paused))
    (niwa-buddy-feed-test--dispatch session project exchange "/buddy")
    (should (equal '("the session's Goal" . "Ship retries")
                   (niwa-buddy-feed-test--brief project))))

  :doc "without plan or unfinished Goal the latest exchange is used"
  (progn
    (setf (niwa-session-goal session)
          (niwa-goal--create :objective "Done" :status 'complete))
    (niwa-buddy-feed-test--dispatch
     session project (concat "Earlier question\n"
                             (propertize "Earlier answer\n" 'gptel 'response)
                             exchange)
     "/buddy")
    (should (equal (cons "the latest exchange"
                         (concat "Latest request:\nAdd retries to the fetcher"
                                 "\n\nLatest response:\nUse exponential backoff."))
                   (niwa-buddy-feed-test--brief project))))

  :doc "explicit text outranks the plan, and a second /buddy replaces the brief"
  (progn
    (with-temp-buffer
      (niwa-plan-write-current "# Plan" session (current-buffer)))
    (setf (niwa-session-plan-mode session) t)
    (niwa-buddy-feed-test--dispatch session project "" "/buddy")
    (niwa-buddy-feed-test--dispatch session project ""
                                       "/buddy parse the config first")
    (should (equal '("the /buddy request" . "parse the config first")
                   (niwa-buddy-feed-test--brief project))))

  :doc "/buddy off forgets the brief and turns the global mode off"
  (progn
    (niwa-buddy-feed-test--dispatch session project "" "/buddy write it")
    (should niwa-buddy-global-mode)
    (should (string-match-p "Buddy off"
                            (niwa-buddy-feed-test--dispatch
                             session project "" "/buddy off")))
    (should-not (niwa-buddy-feed-test--brief project))
    (should-not niwa-buddy-global-mode))

  :doc "an empty session has nothing to feed and changes nothing"
  (progn
    (should-error (niwa-buddy-feed-test--dispatch session project "" "/buddy")
                  :type 'user-error)
    (should-not (niwa-buddy-feed-test--brief project))
    (should-not niwa-buddy-global-mode))

  :doc "a session outside a project is refused, but can still turn Buddy off"
  (progn
    (should-error (niwa-buddy-feed-test--dispatch session save-dir ""
                                                     "/buddy write it")
                  :type 'user-error)
    (should-not niwa-buddy-global-mode)
    (niwa-buddy-global-mode 1)
    (niwa-buddy-feed-test--dispatch session save-dir "" "/buddy off")
    (should-not niwa-buddy-global-mode)))

(provide 'test-niwa-buddy-feed)
;;; test-niwa-buddy-feed.el ends here
