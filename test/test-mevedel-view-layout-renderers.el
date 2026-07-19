;;; test-mevedel-view-layout-renderers.el -- Renderer layout tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the pure renderer + helper functions used by the view layout:
;; - mevedel-agent-runtime-display-label
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
