;;; mevedel-tool-web.el -- Web tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Web-related tool wrappings: WebSearch, WebFetch, YouTube.
;; These wrap the upstream gptel-agent tools so every call flows
;; through the mevedel pipeline (permissions, result persistence,
;; display) while the underlying implementation stays in gptel-agent.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry))

(require 'gptel-agent-tools)

;; `gptel-request'
(declare-function gptel-get-tool "ext:gptel-request" (path))

;; `mevedel-tool-registry'
(declare-function mevedel-tool--register-wrap
                  "mevedel-tool-registry" (&rest keys))


;;
;;; Tool registration

;;;###autoload
(defun mevedel-tool-web--register ()
  "Wrap gptel-agent's web tools for mevedel."

  (mevedel-define-tool
    :wrap (gptel-get-tool '("gptel-agent" "WebSearch"))
    :prompt-file "tools/websearch.md"
    :groups (web)
    :read-only-p t)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("gptel-agent" "WebFetch"))
    :prompt-file "tools/webfetch.md"
    :groups (web)
    :read-only-p t
    :max-result-size 50000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("gptel-agent" "YouTube"))
    :prompt-file "tools/youtube.md"
    :groups (web)
    :read-only-p t
    :max-result-size 50000))

(provide 'mevedel-tool-web)
;;; mevedel-tool-web.el ends here
