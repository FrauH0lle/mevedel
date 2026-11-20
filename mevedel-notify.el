;;; mevedel-notify.el --- Local and remote attention notices -*- lexical-binding: t; -*-

;;; Commentary:

;; A local notifier receives the details of a notice.  Remote devices receive
;; only a wake signal; permission cards retain their own audience selection.

;;; Code:

(require 'mevedel-utilities)

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration-push-notice "mevedel-collaboration-guest" (session kind))

(defcustom mevedel-notify-function nil
  "Function called with a notice kind and its details plist, or nil.
For `permission' the details are the admitted card, without its internal
`:callback'.  There is one notice per card, with no rate limiting.
For `notify' and `goal', `:text' holds the notice.  All kinds include
`:session'.  Return values are ignored and errors become warnings.
See the desktop notification examples in docs/permissions.md."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'mevedel)

(defun mevedel-notify (session kind details)
  "Deliver KIND of notice for SESSION with DETAILS.
DETAILS is the sanitized card plist for `permission', otherwise text.
No text is sent to a push service."
  (when mevedel-notify-function
    (condition-case err
        (funcall mevedel-notify-function kind
                 (if (eq kind 'permission) details
                   (list :session session :text details)))
      (error (mevedel-diagnostic-warning
              (mevedel-diagnostic-scope session) 'mevedel
              (format "%s notify failed: %S" (capitalize (symbol-name kind)) err)
              :warning))))
  (when (and (not (eq kind 'permission))
             (fboundp 'mevedel-collaboration-push-notice))
    (condition-case err
        (mevedel-collaboration-push-notice session kind)
      (error (mevedel-diagnostic-warning
              (mevedel-diagnostic-scope session) 'mevedel
              (format "Remote notice failed: %S" err) :warning)))))

(provide 'mevedel-notify)
;;; mevedel-notify.el ends here
