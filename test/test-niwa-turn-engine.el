;;; test-niwa-turn-engine.el --- Request-owned terminal transactions -*- lexical-binding: t -*-

;;; Commentary:
;; External turns must publish and settle without a synthetic provider FSM.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))

(niwa-deftest niwa--complete-turn/request (:quiet t)
  (niwa-engine-test--with-session
    (insert "External assistant response.\n")
    (niwa--complete-turn request)
    (should (niwa-turn-busy-p buffer))
    (niwa-test--await 5 "Request settlement did not release admission"
      (not (niwa-turn-busy-p buffer)))
    (should (= 1 (niwa-session-turn-count session)))
    (should-not niwa--current-request)
    (should (eq 'idle (niwa-session-agent-root-activity session)))
    (should (niwa-session-artifacts-artifact-present-p
             session (format "segment-%04d.chat.org"
                             (niwa-session-current-segment session)) t))
    (niwa--complete-turn request)
    (should (= 1 (niwa-session-turn-count session)))
    (should-not (niwa-turn-busy-p buffer))))

(provide 'test-niwa-turn-engine)
;;; test-niwa-turn-engine.el ends here
