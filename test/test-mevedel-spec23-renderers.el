;;; test-mevedel-spec23-renderers.el -- Renderer layout tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the pure renderer + helper functions used by the view layout:
;; - mevedel-tool-ui--display-label-from-canonical
;; - mevedel-tool-ui--handle-badge
;; - mevedel-tool-plan--render-present
;; - mevedel-view--zone-separator

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-tool-ui)
(require 'mevedel-tool-plan)
(require 'mevedel-view)
(require 'mevedel-structs)


;;
;;; Display label derivation

(mevedel-deftest mevedel-tool-ui--display-label-from-canonical
  (:doc "derives <type>--<idshort> from <type>--<32-char-md5>")
  ,test
  (test)

  :doc "extracts first 8 chars of suffix after the `--' separator"
  (should (equal "explorer--abc7f3d2"
                 (mevedel-tool-ui--display-label-from-canonical
                  "explorer--abc7f3d2deadbeefcafe1234567890ab")))

  :doc "preserves type prefix verbatim"
  (should (equal "planner--12345678"
                 (mevedel-tool-ui--display-label-from-canonical
                  "planner--1234567890abcdefdeadbeefcafefeed")))

  :doc "returns input unchanged when no `--' separator present"
  (should (equal "main"
                 (mevedel-tool-ui--display-label-from-canonical "main")))

  :doc "returns nil for nil input"
  (should-not (mevedel-tool-ui--display-label-from-canonical nil))

  :doc "handles short suffixes without crashing"
  (should (equal "x--abc"
                 (mevedel-tool-ui--display-label-from-canonical "x--abc"))))


;;
;;; Handle badge renderer

(mevedel-deftest mevedel-tool-ui--handle-badge
  (:doc "maps :status + :calls/:elapsed/:reason to a state badge string")
  ,test
  (test)

  :doc "running with N calls renders [running · N calls]"
  (should (string-match-p
           "running.*3 calls"
           (mevedel-tool-ui--handle-badge
            '(:status running :calls 3))))

  :doc "running with zero calls suppresses the count suffix"
  (let ((badge (mevedel-tool-ui--handle-badge
                '(:status running :calls 0))))
    (should (string-match-p "running" badge))
    (should-not (string-match-p "calls" badge)))

  :doc "blocked reason overrides running badge"
  (let ((badge (mevedel-tool-ui--handle-badge
                '(:status running :calls 2 :blocked-reason "permission"))))
    (should (string-match-p "blocked" badge))
    (should (string-match-p "permission" badge))
    (should-not (string-match-p "running" badge)))

  :doc "completed renders ✓ done with elapsed and calls"
  (let ((badge (mevedel-tool-ui--handle-badge
                '(:status completed :calls 5 :elapsed 2.3))))
    (should (string-match-p "done" badge))
    (should (string-match-p "2\\.3s" badge))
    (should (string-match-p "5 calls" badge)))

  :doc "completed without elapsed/calls renders just ✓ done"
  (should (string-match-p
           "✓ done"
           (mevedel-tool-ui--handle-badge '(:status completed))))

  :doc "error renders ✗ error · REASON"
  (should (string-match-p
           "error.*max-turns"
           (mevedel-tool-ui--handle-badge
            '(:status error :reason "max-turns"))))

  :doc "aborted renders ✗ aborted"
  (should (string-match-p
           "✗ aborted"
           (mevedel-tool-ui--handle-badge '(:status aborted))))

  :doc "incomplete renders ○ incomplete"
  (should (string-match-p
           "○ incomplete"
           (mevedel-tool-ui--handle-badge '(:status incomplete))))

  :doc "unknown status returns empty string"
  (should (equal ""
                 (mevedel-tool-ui--handle-badge '(:status banana)))))


(mevedel-deftest mevedel-tool-ui--render-agent
  (:doc "renders effective agent handle state from render-data and queues")
  ,test
  (test)

  :doc "permission queue entry for running agent renders blocked badge"
  (let* ((agent-id "explore--abc12345deadbeefcafefeed")
         (mevedel--session
          (mevedel-session--create
           :permission-queue (list (list :origin agent-id)))))
    (let ((rendering
           (mevedel-tool-ui--render-agent
            "Agent"
            '(:subagent_type "explore" :description "check")
            "launch status"
            (list :kind 'agent-transcript
                  :agent-id agent-id
                  :status 'running
                  :calls 2))))
      (should (string-match-p "blocked" (plist-get rendering :header)))
      (should (string-match-p "permission" (plist-get rendering :header)))
      (should-not (string-match-p "\\[running" (plist-get rendering :header)))))

  :doc "plan queue entry for running agent renders blocked plan badge"
  (let* ((agent-id "planner--abc12345deadbeefcafefeed")
         (mevedel--session
          (mevedel-session--create
           :plan-queue (list (list :origin agent-id)))))
    (let ((rendering
           (mevedel-tool-ui--render-agent
            "Agent"
            '(:subagent_type "planner" :description "plan")
            "launch status"
            (list :kind 'agent-transcript
                  :agent-id agent-id
                  :status 'running
                  :calls 1))))
      (should (string-match-p "blocked" (plist-get rendering :header)))
      (should (string-match-p "plan" (plist-get rendering :header))))))


;;
;;; Plan summary renderer

(mevedel-deftest mevedel-tool-plan--render-present
  (:doc "consumes :kind plan-summary render-data and produces collapsible card")
  ,test
  (test)

  :doc "implement outcome produces 'implemented at TIMESTAMP' header"
  (let ((result
         (mevedel-tool-plan--render-present
          "PresentPlan" nil "User accepted..."
          '(:kind plan-summary
                  :body "## Refactor plan\n1. Step\n"
                  :origin "planner--abc12345"
                  :outcome implement
                  :timestamp "2026-04-29T14-35-22"))))
    (should (plist-get result :header))
    (should (string-match-p "Plan from" (plist-get result :header)))
    (should (string-match-p "implemented at" (plist-get result :header)))
    (should (string-match-p "2026-04-29T14-35-22" (plist-get result :header)))
    (should (equal "## Refactor plan\n1. Step\n" (plist-get result :body)))
    (should (eq 'markdown-mode (plist-get result :body-mode)))
    (should (plist-get result :initially-collapsed-p)))

  :doc "implement-clear outcome produces 'cleared' header variant"
  (let ((result
         (mevedel-tool-plan--render-present
          "PresentPlan" nil "User accepted..."
          '(:kind plan-summary
                  :body "x"
                  :origin "main"
                  :outcome implement-clear
                  :timestamp "2026-04-29T14-35-22"))))
    (should (string-match-p "cleared" (plist-get result :header))))

  :doc "non-plan-summary render-data falls through (returns nil)"
  (should-not
   (mevedel-tool-plan--render-present
    "PresentPlan" nil "result"
    '(:kind agent-transcript :agent-id "x")))

  :doc "nil render-data falls through"
  (should-not
   (mevedel-tool-plan--render-present
    "PresentPlan" nil "result" nil))

  :doc "display label derived from canonical agent-id in :origin"
  (let ((result
         (mevedel-tool-plan--render-present
          "PresentPlan" nil "result"
          '(:kind plan-summary
                  :body "x"
                  :origin "planner--abc12345deadbeefcafefeed"
                  :outcome implement
                  :timestamp "T"))))
    ;; Header carries the short label, not the full canonical id.
    (should (string-match-p "planner--abc12345" (plist-get result :header)))
    (should-not (string-match-p "deadbeef" (plist-get result :header)))))


;;
;;; Zone separator

(mevedel-deftest mevedel-view--zone-separator
  (:doc "renders ` ─── LABEL ─── ─── ' with width clamp")
  ,test
  (test)

  :doc "contains the label between leading and trailing dash runs"
  (let ((sep (mevedel-view--zone-separator "tasks")))
    (should (string-match-p "─── tasks ───" sep))
    (should (string-match-p "\n\\'" sep)))

  :doc "narrow window clamps to minimum width without negative tail"
  (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 6)))
    (let ((sep (mevedel-view--zone-separator "x")))
      ;; No error, no infinite repeat
      (should (stringp sep))
      (should (string-match-p "─── x ───" sep))))

  :doc "wide window clamps tail length to <= 60 chars"
  (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 200))
            ((symbol-function 'get-buffer-window-list)
             (lambda (&rest _) nil)))
    (let ((sep (mevedel-view--zone-separator "tasks")))
      ;; The composed string should not stretch indefinitely.
      (should (< (length sep) 80)))))


(provide 'test-mevedel-spec23-renderers)
;;; test-mevedel-spec23-renderers.el ends here
