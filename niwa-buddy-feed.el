;;; niwa-buddy-feed.el -- Feed a session's context to Buddy -*- lexical-binding: t -*-

;;; Commentary:

;; `/buddy' hands what a session knows about the work to Buddy, so the
;; user can implement it themselves while Buddy reviews and guides with
;; that intent in view.  The brief is the explicit command text, the
;; current plan in Plan mode, an unfinished Goal's objective, or the
;; latest exchange, in that order.  It is stored in memory for the
;; session's project scope; the session, its Plan mode, and any pending
;; proposal are left exactly as they were.

;;; Code:

(require 'niwa-utilities)

(eval-when-compile (require 'cl-lib))
(require 'niwa-buddy)
(require 'niwa-plan)
(require 'niwa-structs)
(require 'niwa-transcript)

;; `niwa-chat'
(defvar niwa--session)


;;
;;; Brief sources

(defun niwa-buddy-feed--plan (session)
  "Return SESSION's current plan when Plan mode has one not yet accepted."
  (when (and (niwa-session-plan-mode session)
             (not (eq 'accepted
                      (plist-get (niwa-session-plan-metadata session)
                                 :status))))
    (niwa-plan-current-body session)))

(defun niwa-buddy-feed--goal (session)
  "Return the objective of SESSION's unfinished Goal, or nil."
  (when-let* ((goal (niwa-session-goal session))
              ((not (eq 'complete (niwa-goal-status goal)))))
    (niwa-goal-objective goal)))

(defun niwa-buddy-feed--latest-exchange ()
  "Return the current transcript's latest request and its response, or nil."
  (let (user responses)
    (cl-flet ((text (segment)
                (string-trim (buffer-substring-no-properties
                              (nth 1 segment) (nth 2 segment)))))
      (dolist (segment (niwa-transcript-segments (point-min) (point-max)))
        (pcase (car segment)
          ('user (unless (string-empty-p (text segment))
                   (setq user (text segment) responses nil)))
          ('response (push (text segment) responses))))
      (when user
        (concat "Latest request:\n" user
                (when responses
                  (concat "\n\nLatest response:\n"
                          (string-join (nreverse responses) "\n"))))))))

(defun niwa-buddy-feed--brief (session args)
  "Return (SOURCE . TEXT) describing SESSION's work, or nil.
Nonblank ARGS outrank everything the session itself knows."
  (cond
   ((not (string-blank-p args)) (cons "the /buddy request" args))
   ((when-let* ((plan (niwa-buddy-feed--plan session)))
      (cons "the current plan" plan)))
   ((when-let* ((objective (niwa-buddy-feed--goal session)))
      (cons "the session's Goal" objective)))
   ((when-let* ((exchange (niwa-buddy-feed--latest-exchange)))
      (cons "the latest exchange" exchange)))))


;;
;;; Command

(defun niwa-buddy-feed-slash-command (args)
  "Run `/buddy' with ARGS: a brief, `off', or nothing.
Without ARGS the brief comes from the session.  `off' forgets this
project's brief and turns `niwa-buddy-global-mode' off."
  (let ((session (or (bound-and-true-p niwa--session)
                     (niwa-diagnostic-user-error 'unavailable "No niwa session in this buffer")))
        (args (string-trim (or args "")))
        (scope-key (niwa-buddy-directory-scope-key default-directory)))
    (if (equal args "off")
        (progn
          (when scope-key
            (niwa-buddy-set-brief scope-key nil nil))
          (niwa-buddy-global-mode -1)
          (message "niwa: Buddy off"))
      (unless scope-key
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Buddy needs a project to watch; %s"
                    "this session is not in one"))
      (let ((brief (or (niwa-buddy-feed--brief session args)
                       (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Nothing to feed Buddy yet; %s"
                                   "use /buddy WHAT YOU ARE BUILDING"))))
        (niwa-buddy-set-brief scope-key (car brief) (cdr brief))
        (niwa-buddy-global-mode 1)
        (message "niwa: Buddy is watching %s with %s; /buddy off stops it"
                 (abbreviate-file-name scope-key)
                 (car brief))))
    nil))

(provide 'niwa-buddy-feed)
;;; niwa-buddy-feed.el ends here
