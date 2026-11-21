;;; niwa-notify.el --- Local and remote attention notices -*- lexical-binding: t; -*-

;;; Commentary:

;; A local notifier receives the details of a notice.  Remote devices receive
;; only a wake signal; permission cards retain their own audience selection.

;;; Code:

(require 'niwa-utilities)

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration-push-notice "niwa-collaboration-guest" (owner kind))

(defcustom niwa-notify-function nil
  "Function called with a notice kind and its details plist, or nil.
For `permission' the details are the admitted card, without its internal
`:callback'.  There is one notice per card, with no rate limiting.
For `notify', `goal' and `failure', `:text' holds the notice.  All kinds
include `:session', nil with `:workspace' if a run failed before opening
its session.  Return values are ignored and errors become warnings.
See the desktop notification examples in docs/permissions.md."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'niwa)

(defun niwa-notify (owner kind details)
  "Deliver KIND of notice for session or workspace OWNER with DETAILS.
DETAILS is the sanitized card plist for `permission', otherwise text.
No text is sent to a push service.  Return the lobby wake receipt, if any."
  (when niwa-notify-function
    (condition-case err
        (funcall niwa-notify-function kind
                 (cond ((eq kind 'permission) details)
                       ((niwa-workspace-p owner)
                        (list :session nil :workspace owner :text details))
                       (t (list :session owner :text details))))
      (error (niwa-diagnostic-warning
              (niwa-diagnostic-scope owner) 'niwa
              (format "%s notify failed: %S" (capitalize (symbol-name kind)) err)
              :warning))))
  (when (fboundp 'niwa-collaboration-push-notice)
    (condition-case err
        (niwa-collaboration-push-notice owner kind)
      (error (niwa-diagnostic-warning
              (niwa-diagnostic-scope owner) 'niwa
              (format "Remote notice failed: %S" err) :warning)
             nil))))

(provide 'niwa-notify)
;;; niwa-notify.el ends here
