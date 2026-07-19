;;; test-mevedel-view-layout-renderers.el -- Renderer layout tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the pure renderer + helper functions used by the view layout:
;; - mevedel-agent-runtime-display-label
;; - mevedel-tool-ui--handle-badge
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
(require 'mevedel-goal)
(require 'mevedel-view)
(require 'mevedel-structs)


;;
;;; Display label derivation




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
  (:doc "renders a canonical asynchronous Agent start event")
  ,test
  (test)

  :doc "uses the canonical path supplied by render-data"
  (let ((rendering
         (mevedel-tool-ui--render-agent
          "Agent"
          '(:task_name "explore")
          "launch status"
          '(:kind collaboration-event
            :event started
            :path "/root/explore"
            :status running))))
    (should (equal "Started /root/explore"
                   (plist-get rendering :header)))
    (should (equal "/root/explore"
                   (plist-get rendering :agent-path)))))


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


(provide 'test-mevedel-view-layout-renderers)
;;; test-mevedel-view-layout-renderers.el ends here
