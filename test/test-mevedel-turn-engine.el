;;; test-mevedel-turn-engine.el --- Request-owned terminal transactions -*- lexical-binding: t -*-

;;; Commentary:
;; External turns must publish and settle without a synthetic provider FSM.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))

(mevedel-deftest mevedel--complete-turn/request (:quiet t)
  (mevedel-engine-test--with-session
    (insert "External assistant response.\n")
    (mevedel--complete-turn request)
    (should (mevedel-turn-busy-p buffer))
    (with-timeout (5 (ert-fail "Request settlement did not release admission"))
      (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
    (should (= 1 (mevedel-session-turn-count session)))
    (should-not mevedel--current-request)
    (should (eq 'idle (mevedel-session-agent-root-activity session)))
    (should (mevedel-session-artifacts-artifact-present-p
             session (format "segment-%04d.chat.org"
                             (mevedel-session-current-segment session)) t))
    (mevedel--complete-turn request)
    (should (= 1 (mevedel-session-turn-count session)))
    (should-not (mevedel-turn-busy-p buffer))))

(provide 'test-mevedel-turn-engine)
;;; test-mevedel-turn-engine.el ends here
