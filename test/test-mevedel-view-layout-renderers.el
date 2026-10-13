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
(require 'mevedel-view)


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
  (:doc "renders equal-width window-based rules with bounded labels")
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

  :doc "task and agent rules occupy the same columns in wide and narrow windows"
  (with-temp-buffer
    (dolist (width '(10 20 80 120 200))
      (cl-letf (((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _) nil))
                ((symbol-function 'window-width)
                 (lambda (&optional _) width)))
        (dolist (label '("tasks · 1 running · 1 blocked · 1 done · RET to collapse"
                         "1 agent: 1 running [-]"
                         "界界界界界界界界界界界界界界界界界界界界界界界界界界界界界界界界"))
          (let ((rule (mevedel-view--zone-separator label)))
            (should (= (max 10 (- width 4))
                       (string-width (string-trim-right rule))))
            (should (string-suffix-p "\n" rule))
            (when (>= width 120)
              (should (string-search label rule))))))))

  :doc "multiple displayed windows use the widest window's rule width"
  (with-temp-buffer
    (cl-letf (((symbol-function 'get-buffer-window-list)
               (lambda (&rest _) '(narrow wide)))
              ((symbol-function 'window-width)
               (lambda (window) (if (eq window 'wide) 120 40))))
      (should (= 116 (string-width
                      (string-trim-right
                       (mevedel-view--zone-separator "tasks"))))))))


(provide 'test-mevedel-view-layout-renderers)
;;; test-mevedel-view-layout-renderers.el ends here
