;;; mevedel-buddy-feed.el -- Feed a session's context to Buddy -*- lexical-binding: t -*-

;;; Commentary:

;; `/buddy' hands what a session knows about the work to Buddy, so the
;; user can implement it themselves while Buddy reviews and guides with
;; that intent in view.  The brief is the explicit command text, the
;; current plan in Plan mode, an unfinished Goal's objective, or the
;; latest exchange, in that order.  It is stored in memory for the
;; session's project scope; the session, its Plan mode, and any pending
;; proposal are left exactly as they were.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-buddy)
(require 'mevedel-plan)
(require 'mevedel-structs)
(require 'mevedel-transcript)

;; `mevedel-chat'
(defvar mevedel--session)


;;
;;; Brief sources

(defun mevedel-buddy-feed--plan (session)
  "Return SESSION's current plan when Plan mode has one not yet accepted."
  (when (and (mevedel-session-plan-mode session)
             (not (eq 'accepted
                      (plist-get (mevedel-session-plan-metadata session)
                                 :status))))
    (mevedel-plan-current-body session)))

(defun mevedel-buddy-feed--goal (session)
  "Return the objective of SESSION's unfinished Goal, or nil."
  (when-let* ((goal (mevedel-session-goal session))
              ((not (eq 'complete (mevedel-goal-status goal)))))
    (mevedel-goal-objective goal)))

(defun mevedel-buddy-feed--latest-exchange ()
  "Return the current transcript's latest request and its response, or nil."
  (let (user responses)
    (cl-flet ((text (segment)
                (string-trim (buffer-substring-no-properties
                              (nth 1 segment) (nth 2 segment)))))
      (dolist (segment (mevedel-transcript-segments (point-min) (point-max)))
        (pcase (car segment)
          ('user (unless (string-empty-p (text segment))
                   (setq user (text segment) responses nil)))
          ('response (push (text segment) responses))))
      (when user
        (concat "Latest request:\n" user
                (when responses
                  (concat "\n\nLatest response:\n"
                          (string-join (nreverse responses) "\n"))))))))

(defun mevedel-buddy-feed--brief (session args)
  "Return (SOURCE . TEXT) describing SESSION's work, or nil.
Nonblank ARGS outrank everything the session itself knows."
  (cond
   ((not (string-blank-p args)) (cons "the /buddy request" args))
   ((when-let* ((plan (mevedel-buddy-feed--plan session)))
      (cons "the current plan" plan)))
   ((when-let* ((objective (mevedel-buddy-feed--goal session)))
      (cons "the session's Goal" objective)))
   ((when-let* ((exchange (mevedel-buddy-feed--latest-exchange)))
      (cons "the latest exchange" exchange)))))


;;
;;; Command

(defun mevedel-buddy-feed-slash-command (args)
  "Run `/buddy' with ARGS: a brief, `off', or nothing.
Without ARGS the brief comes from the session.  `off' forgets this
project's brief and turns `mevedel-buddy-global-mode' off."
  (let ((session (or (bound-and-true-p mevedel--session)
                     (user-error "No mevedel session in this buffer")))
        (args (string-trim (or args "")))
        (scope-key (mevedel-buddy-directory-scope-key default-directory)))
    (if (equal args "off")
        (progn
          (when scope-key
            (mevedel-buddy-set-brief scope-key nil nil))
          (mevedel-buddy-global-mode -1)
          (message "mevedel: Buddy off"))
      (unless scope-key
        (user-error "Buddy needs a project to watch; %s"
                    "this session is not in one"))
      (let ((brief (or (mevedel-buddy-feed--brief session args)
                       (user-error "Nothing to feed Buddy yet; %s"
                                   "use /buddy WHAT YOU ARE BUILDING"))))
        (mevedel-buddy-set-brief scope-key (car brief) (cdr brief))
        (mevedel-buddy-global-mode 1)
        (message "mevedel: Buddy is watching %s with %s; /buddy off stops it"
                 (abbreviate-file-name scope-key)
                 (car brief))))
    nil))

(provide 'mevedel-buddy-feed)
;;; mevedel-buddy-feed.el ends here
