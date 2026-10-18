;;; test-mevedel-view-execution.el --- Inline execution tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the status fragment through managed-zone rendering and its real
;; keyboard/mouse activation path, with public execution snapshots as inputs.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-execution)
(require 'mevedel-view-audit)
(require 'mevedel-execution)
(require 'mevedel-agents)
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))

(defmacro mevedel-view-execution-test--with-view (&rest body)
  "Run BODY in a real view with SESSION and mutable public LIVE snapshots."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (let ((session (mevedel-session--create :name "execution-ui" :root-buffer data-buf))
           (live (list (list :execution-id "e1" :owner "/root" :tool-use-id "tool1"
                             :yielded t :state 'running :command "same command"
                             :output-tail "" :output-bytes 0))))
       (with-current-buffer data-buf (setq-local mevedel--session session))
       (with-current-buffer view-buf
         (cl-letf (((symbol-function 'mevedel-execution-count-user)
                    (lambda (owner) (should (eq owner session)) (length live)))
                   ((symbol-function 'mevedel-execution-list-user)
                    (lambda (owner) (should (eq owner session)) (copy-tree live))))
           ,@body)))))

(defun mevedel-view-execution-test--at (text)
  "Move to the beginning of TEXT in the current rendered view."
  (goto-char (point-min))
  (search-forward text)
  (backward-char (length text)))

(defun mevedel-view-execution-test--activate (text)
  "Activate rendered TEXT with its actual RET binding."
  (mevedel-view-execution-test--at text)
  (call-interactively (key-binding (kbd "RET"))))

(mevedel-deftest mevedel-view-execution-fragment ()
  ,test
  (test)
  :doc "zero work avoids listing, resources, buffers and timers"
  (with-temp-buffer
    (cl-letf (((symbol-function 'mevedel-execution-count-user) (lambda (_) 0))
              ((symbol-function 'mevedel-execution-list-user) (lambda (_) (ert-fail "listed")))
              ((symbol-function 'buffer-list) (lambda (&rest _) (ert-fail "scanned")))
              ((symbol-function 'run-at-time) (lambda (&rest _) (ert-fail "timer")))
              ((symbol-function 'mevedel-resource-visit-path) (lambda (&rest _) (ert-fail "resource"))))
      (should-not (mevedel-view-execution-fragment (mevedel-session--create)))))
  :doc "limits live rows, excludes foreground, preserves selected hidden identity"
  (mevedel-view-execution-test--with-view
    (setq live (cl-loop for n from 1 to 8
                        collect (list :execution-id (format "e%d" n) :owner "/root"
                                      :yielded (/= n 8) :state 'running :command "same command")))
    (let* ((body (plist-get (mevedel-view-execution-fragment session) :body)))
      (should (= 5 (mevedel-view-test--count-substring "▸ Running" body)))
      (should (string-search "[Show all 7]  [Manage]" body)))
    (setq mevedel-view-execution--selected (plist-put (copy-sequence (nth 6 live)) :session session))
    (let ((body (plist-get (mevedel-view-execution-fragment session) :body)))
      (should (= 4 (mevedel-view-test--count-substring "▸ Running" body)))
      (should (= 1 (mevedel-view-test--count-substring "▾ Running" body))))
    (setq mevedel-view-execution--selected nil
          mevedel-view-execution--show-all t)
    (let ((body (plist-get (mevedel-view-execution-fragment session) :body)))
      (should (= 7 (mevedel-view-test--count-substring "▸ Running" body)))
      (should (string-search "[Show fewer]  [Manage]" body))))
  :doc "few rows offer only Manage; collapse leaves the header with its counts"
  (mevedel-view-execution-test--with-view
    (let ((body (plist-get (mevedel-view-execution-fragment session) :body)))
      (should (string-search "\n  [Manage]\n" body))
      (should-not (string-search "[Show" body)))
    (mevedel-view-zone-set-collapse-state mevedel-view-execution--collapse-key t)
    (let ((fragment (mevedel-view-execution-fragment session)))
      (should (plist-get fragment :collapsed))
      (should (string-match-p "\\` ─── background work · 1 command \\[\\+\\] ─+\n\\'"
                              (substring-no-properties (plist-get fragment :body))))))
  :doc "rows are newest first; a blocking agent keeps its place, children follow parents"
  (mevedel-view-execution-test--with-view
    (setq live (list (list :execution-id "m1" :owner "/root" :yielded t :state 'running
                           :kind 'monitor :description "test ticker" :event-count 1
                           :started-at 20.0)
                     (list :execution-id "e1" :owner "/root/counter" :yielded t
                           :state 'running :command "make test" :description "Run tests"
                           :started-at 30.0)))
    (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
               (lambda ()
                 (list (list :path "/root/counter" :status 'running :depth 0
                             :model-metadata '(:model "opus" :effort "medium")
                             :started-at (seconds-to-time 10))
                       (list :path "/root/counter/sub" :status 'running :depth 1
                             :started-at (seconds-to-time 50))
                       (list :path "/root/reviewer" :status 'blocked :depth 0
                             :activity 'permission-blocked
                             :started-at (seconds-to-time 5))))))
      (let ((text (substring-no-properties
                   (plist-get (mevedel-view-execution-fragment session) :body))))
        (should (string-prefix-p
                 " ─── background work · 1 blocked · 3 agents · 1 command · 1 monitor [-] ─"
                 text))
        (should (string-search
                 (concat "  ▸ Running   bash     Run tests · /root/counter\n"
                         "  ▸ Watching  monitor  test ticker · 1 event\n"
                         "  ▸ Running   agent    /root/counter · opus:medium\n"
                         "    ▸ Running   agent    /root/counter/sub\n"
                         "  ▸ Blocked   agent    /root/reviewer · waiting for permission\n")
                 text)))))
  :doc "a finished row lingers while point is on the panel and the grace period restarts on leaving"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (let* ((clock 1000.0)
             (float-time-orig (symbol-function 'float-time)))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time) (if time (funcall float-time-orig time) clock))))
          (setf (plist-get (car live) :started-at) 1.0
                (plist-get (car live) :description) "Run tests")
          (setq live (append live (list (list :execution-id "e2" :owner "/root" :yielded t
                                              :state 'running :command "serve"
                                              :started-at 2.0))))
          (mevedel-view--render-status)
          (mevedel-view-execution-test--at "  ▸ Running   bash     serve")
          ;; e1 finishes with exit 0 while point is on the panel.
          (mevedel-view-execution--event
           (list :session session :data-buffer data-buf :owner "/root" :type 'terminal
                 :facts '(:execution-id "e1" :state completed :termination exited :exit-code 0)))
          (setq live (list (cadr live)))
          (mevedel-view--render-status)
          (unwind-protect
              (progn
                (should (string-search "  ✓ Completed bash     Run tests · exit 0\n" (buffer-string)))
                (mevedel-view-execution-test--at "  ✓ Completed")
                (should-not (get-text-property (point) 'mevedel-view-zone-activate))
                (should (timerp mevedel-view-execution--release-timer))
                ;; Long past the grace period, point is still on the panel.
                (setq clock 1100.0)
                (mevedel-view-execution--check-release view-buf)
                (mevedel-view--render-status)
                (should (string-search "✓ Completed" (buffer-string)))
                ;; Leaving restarts the full grace period.
                (goto-char (point-max))
                (mevedel-view-execution--check-release view-buf)
                (setq clock 1109.0)
                (mevedel-view-execution--check-release view-buf)
                (should (string-search "✓ Completed" (buffer-string)))
                (setq clock 1110.5)
                (mevedel-view-execution--check-release view-buf)
                (should-not (string-search "✓ Completed" (buffer-string)))
                (mevedel-view-execution--check-release view-buf)
                (should-not mevedel-view-execution--release-timer)
                (should (string-search "▸ Running   bash     serve" (buffer-string))))
            (mevedel-view-execution--cancel-release))))))
  :doc "a hidden overflow row that finishes is not held"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setq live (cl-loop for n from 1 to 7
                          collect (list :execution-id (format "e%d" n) :owner "/root"
                                        :yielded t :state 'running
                                        :command (format "cmd %d" n)
                                        :started-at (float n))))
      (mevedel-view--render-status)
      ;; Newest first: e7..e3 shown, e2 and e1 hidden behind [Show all 7].
      (mevedel-view-execution-test--at "  ▸ Running   bash     cmd 7")
      (setq live (cl-remove "e1" live :key (lambda (item) (plist-get item :execution-id))
                            :test #'equal))
      (unwind-protect
          (progn
            (mevedel-view--render-status)
            (ert-info ("a finished row that was hidden is not brought back as held")
              (should-not mevedel-view-execution--held)
              (should-not (string-search "✓ Completed" (buffer-string)))))
        (mevedel-view-execution--cancel-release))))
  :doc "at capacity under point, shown rows stay put and new work is only counted"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setq live (cl-loop for n from 1 to 5
                          collect (list :execution-id (format "e%d" n) :owner "/root"
                                        :yielded t :state 'running
                                        :command (format "cmd %d" n)
                                        :started-at (float n))))
      (mevedel-view--render-status)
      (mevedel-view-execution-test--at "  ▸ Running   bash     cmd 1")
      (let ((line-of (lambda (text)
                       (save-excursion
                         (goto-char (point-min))
                         (search-forward text)
                         (line-number-at-pos)))))
        (let ((line (funcall line-of "cmd 3")))
          (unwind-protect
              (progn
                ;; At capacity, e1 finishes under point, then e6 starts.
                (setq live (cdr live))
                (mevedel-view--render-status)
                (setq live (append live (list (list :execution-id "e6" :owner "/root"
                                                    :yielded t :state 'running
                                                    :command "cmd 6" :started-at 6.0))))
                (mevedel-view--render-status)
                (ert-info ("the finished row stays and no shown row moves")
                  (should (string-search "✓ Completed bash     cmd 1" (buffer-string)))
                  (should-not (string-search "cmd 6" (buffer-string)))
                  (should (= line (funcall line-of "cmd 3")))
                  (should (string-search "[Show all 6]" (buffer-string))))
                ;; Leaving shows new work and starts the held row's grace.
                (goto-char (point-max))
                (mevedel-view-execution--check-release view-buf)
                (should (string-search "▸ Running   bash     cmd 6" (buffer-string)))
                (should (string-search "✓ Completed" (buffer-string))))
            (mevedel-view-execution--cancel-release))))))
  :doc "a row finishing while point is elsewhere lingers for the grace period"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (let* ((clock 1000.0)
             (float-time-orig (symbol-function 'float-time)))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time) (if time (funcall float-time-orig time) clock))))
          (mevedel-view--render-status)
          (goto-char (point-max))
          (setq live nil)
          (unwind-protect
              (progn
                (mevedel-view--render-status)
                (should (string-search "✓ Completed bash     same command" (buffer-string)))
                (setq clock (+ 1000.0 (1- mevedel-view-execution-grace-seconds)))
                (mevedel-view-execution--check-release view-buf)
                (should (string-search "✓ Completed" (buffer-string)))
                (setq clock (+ 1000.0 mevedel-view-execution-grace-seconds 0.5))
                (mevedel-view-execution--check-release view-buf)
                (should-not (mevedel-view-execution-fragment session))
                (should-not mevedel-view-execution--held))
            (mevedel-view-execution--cancel-release))))))
  :doc "changing sessions drops retained selection even with equal IDs"
  (mevedel-view-execution-test--with-view
    (setq mevedel-view-execution--selected (plist-put (copy-sequence (car live)) :session session))
    (setq session (mevedel-session--create) live nil)
    (should-not (mevedel-view-execution-fragment session))
    (should-not mevedel-view-execution--selected))
  :doc "hidden wall time does not invalidate unchanged rendered properties"
  (mevedel-view-execution-test--with-view
    (let ((before (plist-get (mevedel-view-execution-fragment session) :body)))
      (setf (plist-get (car live) :wall-time-seconds) 20)
      (should (equal-including-properties
               before (plist-get (mevedel-view-execution-fragment session) :body))))))

(mevedel-deftest mevedel-view-execution--line ()
  ,test
  (test)
  :doc "whole row including leading spaces and newline toggles with RET"
  (mevedel-view-execution-test--with-view
    (mevedel-view--render-status)
    (dolist (where '(leading chevron label newline))
      (mevedel-view-execution-test--at "  ▸ Running")
      (pcase where
        ('chevron (forward-char 2))
        ('label (search-forward "same"))
        ('newline (end-of-line)))
      (should (get-text-property (point) 'read-only))
      (should (functionp (get-text-property (point) 'mevedel-view-zone-activate)))
      (call-interactively (key-binding (kbd "RET")))
      (should (equal "e1" (plist-get mevedel-view-execution--selected :execution-id)))
      (should (string-search "Preview (not full output)" (buffer-string)))
      (mevedel-view-execution-test--activate "  ▾ Running")
      (should-not mevedel-view-execution--selected)))
  :doc "real mouse binding toggles clicked row, not point's old location"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (mevedel-view--render-status)
      (mevedel-view-execution-test--at "  ▸ Running")
      (let* ((position (point))
             (event (list 'mouse-1 (list (selected-window) position '(0 . 0) 0)))
             (command (lookup-key (get-text-property position 'keymap) [mouse-1])))
        (goto-char (point-max))
        (funcall command event)
        (should mevedel-view-execution--selected))))
  :doc "TAB toggles only its row, including preview controls, and preserves the draft"
  (mevedel-view-execution-test--with-view
    (setq live (append live (list (list :execution-id "e2" :owner "/root"
                                      :yielded t :state 'running :kind 'monitor
                                      :description "Watch errors"))))
    (mevedel-view--render-status)
    (goto-char (point-max))
    (insert "> draft\nsecond line")
    (cl-letf (((symbol-function 'mevedel-execution-stop-user)
               (lambda (&rest _) (ert-fail "TAB stopped an execution"))))
      (dolist (text '("same command" "Watch errors"))
        (mevedel-view-execution-test--at text)
        (call-interactively (key-binding (kbd "TAB")))
        (should mevedel-view-execution--selected)
        (should-not (mevedel-view-zone-collapse-state
                     mevedel-view-execution--collapse-key))
        (should (string-search "Preview (not full output)" (buffer-string)))
        (mevedel-view-execution-test--at "[Stop]")
        (call-interactively (key-binding (kbd "TAB")))
        (should-not mevedel-view-execution--selected)
        (should (string-search "same command" (buffer-string)))
        (should (string-search "Watch errors" (buffer-string)))))
    (dolist (offset '(0 -2))
      (mevedel-view-execution-test--at "[Manage]")
      (forward-char offset)
      (call-interactively (key-binding (kbd "TAB")))
      (should-not (mevedel-view-zone-collapse-state
                   mevedel-view-execution--collapse-key)))
    (should (equal "> draft\nsecond line" (mevedel-view--input-text))))
  :doc "blank suffix and header margins are inert, Manage is explicit"
  (mevedel-view-execution-test--with-view
    (mevedel-view--render-status)
    (mevedel-view-execution-test--at " ─── background work")
    (should-not (get-text-property (point) 'mevedel-view-zone-activate))
    (mevedel-view-execution-test--at "  ▸ Running")
    (forward-line 2)
    (should (eq (char-after) ?\n))
    (should-not (get-text-property (point) 'mevedel-view-zone-activate))
    (let (opened)
      (cl-letf (((symbol-function 'mevedel-background-list-open) (lambda (&optional _) (setq opened t))))
        (mevedel-view-execution-test--activate "[Manage]")
        (should opened))))
  :doc "hover highlight covers one row, not its neighbours"
  (mevedel-view-execution-test--with-view
    (setq live (append live (list (plist-put (copy-sequence (car live)) :execution-id "e2"))))
    (mevedel-view--render-status)
    (mevedel-view-execution-test--at "  ▸ Running")
    (let ((start (point)))
      (should (eq 'highlight (get-text-property start 'mouse-face)))
      (end-of-line)
      ;; The newline still activates the row but ends its highlight run.
      (should-not (get-text-property (point) 'mouse-face))
      (should (functionp (get-text-property (point) 'mevedel-view-zone-activate)))
      (should (= (point) (next-single-property-change start 'mouse-face)))))
  :doc "same-command IDs have independent selection and one expanded item"
  (mevedel-view-execution-test--with-view
    (setq live (append live (list (plist-put (copy-sequence (car live)) :execution-id "e2"))))
    (mevedel-view--render-status)
    (mevedel-view-execution-test--activate "  ▸ Running")
    (mevedel-view-execution-test--activate "  ▸ Running")
    (should (equal "e2" (plist-get mevedel-view-execution--selected :execution-id)))
    (should (= 1 (mevedel-view-test--count-substring "Preview (not full output)" (buffer-string))))))

(mevedel-deftest mevedel-view-execution--header ()
  ,test
  (test)
  :doc "rule counts commands and monitors and ends with the collapse toggle"
  (mevedel-view-execution-test--with-view
    (let* ((header (mevedel-view-execution--header
                    session nil '((:kind bash) (:kind monitor) (:kind monitor)) nil))
           (toggle (string-search "[-]" header)))
      (should (string-prefix-p " ─── background work · 1 command · 2 monitors [-] ─"
                               header))
      (should (eq 'mevedel-view-zone-separator (get-text-property 1 'font-lock-face header)))
      (should (eq 'link (get-text-property toggle 'font-lock-face header)))
      (should (functionp (get-text-property toggle 'mevedel-view-zone-activate header)))))
  :doc "blocked agents are counted first in a warning face"
  (mevedel-view-execution-test--with-view
    (let* ((header (mevedel-view-execution--header
                    session '((:status blocked) (:status running)) nil t))
           (blocked (string-search "1 blocked" header)))
      (should (string-prefix-p " ─── background work · 1 blocked · 2 agents [+] ─" header))
      (should (eq 'mevedel-view-tool-warning
                  (get-text-property blocked 'font-lock-face header)))))
  :doc "a clipped narrow rule keeps the toggle reachable"
  (mevedel-view-execution-test--with-view
    (cl-letf (((symbol-function 'mevedel-view--zone-separator)
               (lambda (_) " ─── background w… ─\n")))
      (let ((header (mevedel-view-execution--header session nil nil nil)))
        (should (equal " [-] background w… ─\n" (substring-no-properties header)))
        (should (get-text-property 1 'mevedel-view-zone-activate header))))))

(mevedel-deftest mevedel-view-execution--mouse-press ()
  ,test
  (test)
  :doc "first focus click only selects; selected-window clicks toggle the row or header"
  (dolist (same-buffer '(nil t))
    (dolist (target '(row header))
      (mevedel-view-execution-test--with-view
        (save-window-excursion
          (switch-to-buffer view-buf)
          (let* ((view-window (selected-window))
                 (other-window (split-window-right))
                 (mouse-drag-copy-region nil)
                 commands
                 (pre-command-hook
                  (cons (lambda () (push this-command commands)) pre-command-hook)))
            (set-window-buffer other-window (if same-buffer view-buf data-buf))
            (mevedel-view--render-status)
            (mevedel-view-execution-test--at (if (eq target 'row) "  ▸ Running" "[-]"))
            (let* ((position (point))
                   ;; Command lookup uses text-pos (slot 5), not just point.
                   (location (list view-window position '(10 . 10) 0 nil position))
                   (click (vector (list 'down-mouse-1 location)
                                  (list 'mouse-1 location))))
              (select-window other-window)
              (execute-kbd-macro click)
              (should (eq (selected-window) view-window))
              (should (= (point) position))
              (should-not mevedel-view-execution--selected)
              (should-not (mevedel-view-zone-collapse-state
                           mevedel-view-execution--collapse-key))
              (execute-kbd-macro click)
              (ert-info ((format "same-buffer=%S target=%S commands=%S"
                                 same-buffer target (reverse commands)))
                (if (eq target 'row)
                    (should mevedel-view-execution--selected)
                  (should (mevedel-view-zone-collapse-state
                           mevedel-view-execution--collapse-key))))))))))
  :doc "a focus click on Stop cannot stop work; the subsequent selected-window click can"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (let ((view-window (selected-window))
            (other-window (split-window-right))
            (mouse-drag-copy-region nil)
            stopped)
        (set-window-buffer other-window data-buf)
        (mevedel-view--render-status)
        (mevedel-view-execution-test--activate "  ▸ Running")
        (mevedel-view-execution-test--at "[Stop]")
        (let* ((position (point))
               (location (list view-window position '(10 . 10) 0 nil position))
               (click (vector (list 'down-mouse-1 location) (list 'mouse-1 location))))
          (cl-letf (((symbol-function 'mevedel-execution-stop-user)
                     (lambda (owner id) (push (list owner id) stopped))))
            (select-window other-window)
            (execute-kbd-macro click)
            (should-not stopped)
            (execute-kbd-macro click)
            (should (equal (list (list session "e1")) stopped)))))))
  :doc "selected-window dragging selects text without toggling and leaves composer bindings alone"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (let ((mouse-drag-copy-region nil)
            (mouse-drag-and-drop-region nil))
        (mevedel-view--render-status)
        (mevedel-view-execution-test--at "  ▸ Running")
        (let* ((start (point))
               (end (+ start 12))
               (from (list (selected-window) start '(10 . 10) 0 nil start))
               (to (list (selected-window) end '(100 . 10) 0 nil end)))
          (execute-kbd-macro (vector (list 'down-mouse-1 from)
                                     (list 'drag-mouse-1 from to)))
          (should-not mevedel-view-execution--selected)
          (should (= start (region-beginning)))
          (should (= end (region-end))))
        (goto-char (mevedel-view--input-start))
        (should (eq (key-binding [down-mouse-1]) 'mouse-drag-region))))))

(mevedel-deftest mevedel-view-execution--pointer-on-panel-p ()
  ,test
  (test)
  :doc "a pointer above or left of the frame is off the panel instead of an error; point still counts"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (mevedel-view--render-status)
      (goto-char (mevedel-view--input-start))
      (dolist (coordinates '((0 . -27) (-4 . 3) (-1 . -1)))
        (cl-letf (((symbol-function 'mouse-pixel-position)
                   (lambda () (cons (selected-frame) coordinates))))
          (should-not (mevedel-view-execution--pointer-on-panel-p))))
      (mevedel-view-execution-test--at "  ▸ Running")
      (cl-letf (((symbol-function 'mouse-pixel-position)
                 (lambda () (cons (selected-frame) '(0 . -27)))))
        (should (mevedel-view-execution--pointer-on-panel-p))))))

(mevedel-deftest mevedel-view-execution--retain-selected ()
  ,test
  (test)
  :doc "completed inspectors start a fresh grace period on collapse, blocked until pointers leave"
  (dolist (hover-at-collapse '(nil t))
    (mevedel-view-execution-test--with-view
      (let* ((clock 1000.0)
             (hover hover-at-collapse)
             (float-time-orig (symbol-function 'float-time)))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time) (if time (funcall float-time-orig time) clock)))
                  ((symbol-function 'mevedel-view-execution--pointer-on-panel-p)
                   (lambda () hover)))
          (unwind-protect
              (progn
                (mevedel-view--render-status)
                (goto-char (point-max))
                (insert "> draft\nsecond line")
                (mevedel-view-execution-test--activate "  ▸ Running")
                (setq live nil)
                (mevedel-view-execution--event
                 (list :session session :data-buffer data-buf :owner "/root" :type 'terminal
                       :facts '(:execution-id "e1" :state completed :termination exited :exit-code 0)
                       :whole-output "retained output"))
                (setq clock 1200.0)
                (mevedel-view-execution--check-release view-buf)
                (should (string-search "retained output" (buffer-string)))
                (should-not mevedel-view-execution--release-timer)
                (mevedel-view-execution-test--at "  ▾ Completed")
                (call-interactively (key-binding (kbd "TAB")))
                (should-not mevedel-view-execution--selected)
                (should (string-search "✓ Completed bash" (buffer-string)))
                (should (timerp mevedel-view-execution--release-timer))
                (let ((held (nth 2 (car mevedel-view-execution--held))))
                  (should (= 1210.0 (plist-get held :release-at)))
                  (should-not (plist-get held :whole-output))
                  (should-not (plist-get held :output-tail)))
                (when hover-at-collapse
                  (setq clock 2000.0)
                  (mevedel-view-execution--check-release view-buf)
                  (should (string-search "✓ Completed bash" (buffer-string)))
                  (setq hover nil)
                  (mevedel-view-execution--check-release view-buf)
                  (should (= 2010.0 (plist-get (nth 2 (car mevedel-view-execution--held))
                                              :release-at))))
                (setq clock (if hover-at-collapse 2009.0 1209.0))
                (mevedel-view-execution--check-release view-buf)
                (should (string-search "✓ Completed bash" (buffer-string)))
                (setq clock (+ clock 1.5))
                (mevedel-view-execution--check-release view-buf)
                (should-not (string-search "✓ Completed bash" (buffer-string)))
                (should-not mevedel-view-execution--release-timer)
                (should (equal "> draft\nsecond line" (mevedel-view--input-text))))
            (mevedel-view-execution--cancel-release))))))
  :doc "a completed inspector beyond five newer rows stays visible until its grace expires"
  (mevedel-view-execution-test--with-view
    (let* ((clock 1000.0)
           (hover t)
           (float-time-orig (symbol-function 'float-time)))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time) (if time (funcall float-time-orig time) clock)))
                ((symbol-function 'mevedel-view-execution--pointer-on-panel-p)
                 (lambda () hover)))
        (unwind-protect
            (progn
              (setq mevedel-view-execution--selected
                    (list :session session :execution-id "old" :owner "/root"
                          :state 'completed :terminal-p t :termination 'exited :exit-code 0
                          :description "Old inspector" :started-at 1.0 :whole-output "old output")
                    live (cl-loop for n from 2 to 6
                                  collect (list :execution-id (format "e%d" n)
                                                :owner "/root" :yielded t :state 'running
                                                :started-at (float n) :description (format "New %d" n))))
              (mevedel-view--render-status)
              (mevedel-view-execution-test--at "  ▾ Completed")
              (call-interactively (key-binding (kbd "TAB")))
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (setq clock 1001.0 hover nil)
              (mevedel-view-execution--check-release view-buf)
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (setq clock 1010.0)
              (mevedel-view-execution--check-release view-buf)
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (setq clock 1011.5)
              (mevedel-view-execution--check-release view-buf)
              (should-not (string-search "Old inspector" (buffer-string)))
              (should (= 5 (mevedel-view-test--count-substring "▸ Running" (buffer-string)))))
          (mevedel-view-execution--cancel-release)))))
  :doc "pinning an older live inspector does not evict a held completed row"
  (mevedel-view-execution-test--with-view
    (let* ((clock 1000.0)
           (hover t)
           (float-time-orig (symbol-function 'float-time)))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time) (if time (funcall float-time-orig time) clock)))
                ((symbol-function 'mevedel-view-execution--pointer-on-panel-p)
                 (lambda () hover)))
        (unwind-protect
            (progn
              (setq mevedel-view-execution--selected
                    (list :session session :execution-id "old" :owner "/root"
                          :state 'completed :terminal-p t :description "Old inspector"
                          :started-at 1.0)
                    live (cl-loop for n from 2 to 8
                                  collect (list :execution-id (format "e%d" n)
                                                :owner "/root" :yielded t :state 'running
                                                :started-at (float n) :description (format "New %d" n))))
              (mevedel-view--render-status)
              (mevedel-view-execution-test--activate "  ▾ Completed")
              (mevedel-view-execution-test--activate "[Show all 8]")
              (mevedel-view-execution-test--activate "New 2")
              (mevedel-view-execution-test--activate "[Show fewer]")
              (setq clock 1001.0 hover nil)
              (mevedel-view-execution--check-release view-buf)
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (should (string-search "▾ Running   bash     New 2" (buffer-string)))
              (should (= 4 (mevedel-view-test--count-substring "▸ Running" (buffer-string)))))
          (mevedel-view-execution--cancel-release)))))
  :doc "selecting another execution or agent also retains a completed inspector summary"
  (dolist (kind '(execution agent))
    (mevedel-view-execution-test--with-view
      (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                 (lambda () '((:path "/root/new" :status running :depth 0)))))
        (unwind-protect
            (progn
              (setq mevedel-view-execution--selected
                    (list :session session :execution-id "old" :owner "/root"
                          :state 'completed :terminal-p t :description "Old inspector"
                          :whole-output "discarded output"))
              (mevedel-view--render-status)
              (mevedel-view-execution-test--activate
               (if (eq kind 'agent) "  ▸ Running   agent" "  ▸ Running   bash"))
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (should-not (plist-get (nth 2 (car mevedel-view-execution--held)) :whole-output)))
          (mevedel-view-execution--cancel-release))))))

(mevedel-deftest mevedel-view-execution-toggle-panel ()
  ,test
  (test)
  :doc "the header toggle hides and restores rows, preserving an active draft"
  (mevedel-view-execution-test--with-view
    (mevedel-view--render-status)
    (goto-char (point-max))
    (insert "> draft\nsecond line")
    (mevedel-view-execution-test--activate "[-]")
    (should-not (string-search "▸ Running" (buffer-string)))
    (should (string-search "background work · 1 command [+]" (buffer-string)))
    (mevedel-view-execution-test--activate "[+]")
    (should (string-search "▸ Running" (buffer-string)))
    (should (equal "> draft\nsecond line" (mevedel-view--input-text))))
  :doc "TAB on the header, not the work rows, toggles the complete panel"
  (mevedel-view-execution-test--with-view
    (mevedel-view--render-status)
    (dolist (collapsed '(t nil))
      (mevedel-view-execution-test--at " ─── background work")
      (call-interactively (key-binding (kbd "TAB")))
      (should (eq collapsed (mevedel-view-zone-collapse-state
                             mevedel-view-execution--collapse-key)))
      (should (eq (not collapsed)
                  (and (string-search "  ▸ Running" (buffer-string)) t))))
    (should (string-search "  ▸ Running" (buffer-string)))
    (should-not (mevedel-view-zone-collapse-state
                 mevedel-view-execution--collapse-key))))

(mevedel-deftest mevedel-view-execution-reset ()
  ,test
  (test)
  :doc "clears expansion, overflow, collapse and held rows, stopping the release timer"
  (mevedel-view-execution-test--with-view
    (setq mevedel-view-execution--selected-agent "/root/a"
          mevedel-view-execution--show-all t
          mevedel-view-execution--held (list (list '(agent "/root/a") 'agent '(:path "/root/a")))
          mevedel-view-execution--release-timer (run-with-timer 3600 nil #'ignore))
    (mevedel-view-zone-set-collapse-state mevedel-view-execution--collapse-key t)
    (mevedel-view-execution-reset)
    (should-not mevedel-view-execution--selected-agent)
    (should-not mevedel-view-execution--show-all)
    (should-not mevedel-view-execution--held)
    (should-not mevedel-view-execution--release-timer)
    (should-not (mevedel-view-zone-collapse-state mevedel-view-execution--collapse-key))
    (should (string-search "▸ Running" (buffer-string)))))

(mevedel-deftest mevedel-view-execution--ordered ()
  ,test
  (test)
  :doc "newest unit first; an agent tree is one unit ordered by its top-level agent"
  (let ((entry (lambda (kind data) (list (mevedel-view-execution--key kind data) kind data))))
    (should (equal '((agent "/root/new") (execution "/root" "e") (agent "/root/old")
                     (agent "/root/old/kid"))
                   (mapcar #'car
                           (mevedel-view-execution--ordered
                            (list (funcall entry 'agent '(:path "/root/old/kid" :started-at 90.0))
                                  (funcall entry 'agent '(:path "/root/old" :started-at 1.0))
                                  (funcall entry 'execution '(:owner "/root" :execution-id "e"
                                                              :started-at 5.0))
                                  (funcall entry 'agent '(:path "/root/new" :started-at 9.0))))))))
  :doc "an agent tree keeps its place after its top-level agent finishes"
  (let ((entry (lambda (kind data) (list (mevedel-view-execution--key kind data) kind data)))
        (starts (make-hash-table :test #'equal)))
    (mevedel-view-execution--ordered
     (list (funcall entry 'agent '(:path "/root/old" :started-at 1.0))
           (funcall entry 'agent '(:path "/root/old/kid" :started-at 90.0))
           (funcall entry 'agent '(:path "/root/new" :started-at 9.0)))
     starts)
    (should (equal '((agent "/root/new") (agent "/root/old/kid"))
                   (mapcar #'car
                           (mevedel-view-execution--ordered
                            (list (funcall entry 'agent '(:path "/root/old/kid" :started-at 90.0))
                                  (funcall entry 'agent '(:path "/root/new" :started-at 9.0)))
                            starts))))
    ;; Units that are gone are forgotten.
    (mevedel-view-execution--ordered
     (list (funcall entry 'agent '(:path "/root/new" :started-at 9.0))) starts)
    (should (equal '("/root/new") (hash-table-keys starts)))))

(mevedel-deftest mevedel-view-execution--check-release ()
  ,test
  (test)
  :doc "a dead view's timer cancels only itself, never another view's"
  (let* ((dead (generate-new-buffer " *dead-view*"))
         (own (run-with-timer 3600 3600 #'ignore))
         (other (run-with-timer 3600 3600 #'mevedel-view-execution--check-release
                                (current-buffer))))
    (unwind-protect
        (progn
          (kill-buffer dead)
          (mevedel-view-execution--check-release dead own)
          (should-not (memq own timer-list))
          (should (memq other timer-list)))
      (cancel-timer own)
      (cancel-timer other))))

(mevedel-deftest mevedel-view-execution--state-face ()
  (dolist (case '(((:state running) . mevedel-view-agent-running)
                  ((:state running :kind monitor) . mevedel-view-agent-running)
                  ((:state stopping) . mevedel-view-tool-warning)
                  ((:state lost) . error)
                  ((:state completed :termination exited :exit-code 0) . success)
                  ((:state completed :termination exited :exit-code 7) . error)
                  ((:terminal-p t :termination user-stop) . mevedel-view-tool-warning)
                  ((:unavailable-p t) . mevedel-view-tool-metadata)))
    (should (eq (cdr case) (mevedel-view-execution--state-face (car case))))))

(mevedel-deftest mevedel-view-execution--line-presentation ()
  ,test
  (test)
  :doc "state carries its outcome face and a single Monitor event is singular"
  (mevedel-view-execution-test--with-view
    (let ((line (mevedel-view-execution--line
                 '(:owner "/root" :execution-id "m1" :kind monitor :state running
                   :description "watch" :event-count 1))))
      (should (equal "  ▸ Watching  monitor  watch · 1 event\n" (substring-no-properties line)))
      (should (eq 'mevedel-view-agent-running
                  (get-text-property (string-search "Watching" line) 'font-lock-face line)))
      (should (eq 'mevedel-view-tool-metadata
                  (get-text-property (string-search "▸" line) 'font-lock-face line)))))
  :doc "a Bash description replaces the command; the command is the fallback"
  (mevedel-view-execution-test--with-view
    (should (equal "  ▸ Running   bash     Run tests\n"
                   (substring-no-properties
                    (mevedel-view-execution--line
                     '(:owner "/root" :execution-id "e1" :state running
                       :command "make test" :description "Run tests")))))
    (should (equal "  ▸ Running   bash     make test\n"
                   (substring-no-properties
                    (mevedel-view-execution--line
                     '(:owner "/root" :execution-id "e1" :state running
                       :command "make test")))))))

(mevedel-deftest mevedel-view-execution--agent-line ()
  ,test
  (test)
  :doc "nested agents indent; interaction-blocked agents say they wait for input"
  (mevedel-view-execution-test--with-view
    (should (equal "    ▸ Blocked   agent    /root/a/b · waiting for input\n"
                   (substring-no-properties
                    (mevedel-view-execution--agent-line
                     session '(:path "/root/a/b" :status blocked :depth 1
                               :activity interaction-blocked)))))
    (should (equal "  ▸ Waiting   agent    /root/a\n"
                   (substring-no-properties
                    (mevedel-view-execution--agent-line
                     session '(:path "/root/a" :status waiting :depth 0)))))))

(mevedel-deftest mevedel-view-execution--agent-details ()
  ,test
  (test)
  :doc "RET expands one agent row with its actions, collapsing any execution row"
  (mevedel-view-execution-test--with-view
    (let (opened interrupted shown)
      (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                 (lambda ()
                   (list (list :path "/root/counter" :status 'running :depth 0
                               :parent-path "/root" :tool-use-id "agent-call"
                               :model-metadata '(:model "opus")))))
                ((symbol-function 'mevedel-view-agent-handle-activate)
                 (lambda (&optional path) (setq opened path)))
                ((symbol-function 'mevedel-agent-control-interrupt-user)
                 (lambda (owner path) (should (eq owner session)) (setq interrupted path)))
                ((symbol-function 'mevedel-view-audit-show-result)
                 (lambda (record) (setq shown record))))
        (mevedel-view--render-status)
        (mevedel-view-execution-test--activate "  ▸ Running   bash")
        (should mevedel-view-execution--selected)
        (mevedel-view-execution-test--activate "  ▸ Running   agent")
        (should (equal "/root/counter" mevedel-view-execution--selected-agent))
        (should-not mevedel-view-execution--selected)
        (should (string-search "    Model     opus\n" (buffer-string)))
        (should-not (string-search "Preview (not full output)" (buffer-string)))
        (mevedel-view-execution-test--activate "[Open transcript]")
        (should (equal "/root/counter" opened))
        (mevedel-view-execution-test--activate "[Show in conversation]")
        (should (equal '(:owner "/root" :tool-use-id "agent-call") shown))
        (mevedel-view-execution-test--activate "[Interrupt]")
        (should (equal "/root/counter" interrupted))
        (mevedel-view-execution-test--activate "  ▾ Running   agent")
        (should-not mevedel-view-execution--selected-agent))))
  :doc "TAB on agent rows or details toggles the agent without invoking Interrupt"
  (mevedel-view-execution-test--with-view
    (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
               (lambda () '((:path "/root/counter" :status running :depth 0))))
              ((symbol-function 'mevedel-agent-control-interrupt-user)
               (lambda (&rest _) (ert-fail "TAB interrupted an agent"))))
      (mevedel-view--render-status)
      (mevedel-view-execution-test--at "  ▸ Running   agent")
      (call-interactively (key-binding (kbd "TAB")))
      (should (equal "/root/counter" mevedel-view-execution--selected-agent))
      (mevedel-view-execution-test--at "[Interrupt]")
      (call-interactively (key-binding (kbd "TAB")))
      (should-not mevedel-view-execution--selected-agent)
      (should (string-search "  ▸ Running   bash" (buffer-string)))
      (should-not (mevedel-view-zone-collapse-state
                   mevedel-view-execution--collapse-key))))
  :doc "a finished agent drops its expansion and its stale controls refuse"
  (mevedel-view-execution-test--with-view
    (let ((agents (list (list :path "/root/counter" :status 'running :depth 0))))
      (cl-letf (((symbol-function 'mevedel-view--agent-status-collect) (lambda () agents))
                ((symbol-function 'mevedel-agent-control-interrupt-user)
                 (lambda (&rest _) (ert-fail "interrupted a finished agent"))))
        (mevedel-view--render-status)
        (mevedel-view-execution-test--activate "  ▸ Running   agent")
        (mevedel-view-execution-test--at "[Interrupt]")
        (let ((callback (get-text-property (point) 'mevedel-view-zone-activate)))
          (setq agents nil)
          (mevedel-view--render-status)
          (should-not mevedel-view-execution--selected-agent)
          (should-error (funcall callback) :type 'user-error))))))

(mevedel-deftest mevedel-view-execution--preview ()
  ,test
  (test)
  :doc "preview is bounded to three lines and 100 columns with visible warnings"
  (let* ((output (mapconcat #'identity (make-list 8 (make-string 500 ?x)) "\n"))
         (preview (mevedel-view-execution--preview
                   (list :output-tail output :termination 'output-limit))))
    (should (string-search "truncated" preview))
    (should (= 3 (length (cl-remove-if-not
                          (lambda (line) (string-prefix-p "    xxx" line))
                          (split-string preview "\n")))))
    (dolist (line (split-string preview "\n")) (should (<= (string-width line) 104))))
  :doc "distinguishes empty live, empty terminal, missing and lost output"
  (dolist (case '(((:output-tail "" :output-bytes 0) . "No output yet.")
                  ((:terminal-p t :whole-output "" :output-bytes 0) . "No output.")
                  ((:terminal-p t :state lost) . "Output unavailable.")
                  ((:output-tail "" :output-bytes 10) . "Output unavailable.")))
    (should (string-search (cdr case) (mevedel-view-execution--preview (car case))))))

(mevedel-deftest mevedel-view-execution--state-label ()
  (dolist (case '(((:state running) . "Running")
                  ((:state running :kind monitor) . "Watching")
                  ((:state stopping) . "Stopping")
                  ((:state lost) . "Lost")
                  ((:state completed :termination exited :exit-code 7) . "Completed (exit 7)")
                  ((:terminal-p t :termination user-stop) . "Completed (user-stop)")))
    (should (equal (cdr case) (mevedel-view-execution--state-label (car case))))))

(mevedel-deftest mevedel-view-execution--event ()
  ,test
  (test)
  :doc "quiet progress retains new facts without status work; changed output renders normally"
  (mevedel-view-execution-test--with-view
    (mevedel-view--render-status)
    (mevedel-view-execution-test--activate "  ▸ Running")
    (let* ((before (buffer-string))
           (facts (plist-put (copy-tree (car live)) :wall-time-seconds 99))
           (event (list :session session :data-buffer data-buf :owner "/root"
                        :type 'progress :facts facts :output-tail "")))
      (cl-letf (((symbol-function 'mevedel-view--render-status)
                 (lambda (&rest _) (ert-fail "quiet progress rendered status")))
                ((symbol-function 'mevedel-execution-count-user)
                 (lambda (_) (ert-fail "quiet progress counted executions")))
                ((symbol-function 'mevedel-execution-list-user)
                 (lambda (_) (ert-fail "quiet progress listed executions"))))
        (mevedel-view-execution--event event))
      (should (= 99 (plist-get mevedel-view-execution--selected :wall-time-seconds)))
      (should (equal-including-properties before (buffer-string)))
      ;; The facade's public snapshot advances with the event in production.
      (setf (plist-get (car live) :output-tail) "changed output")
      (mevedel-view-execution--event (plist-put event :output-tail "changed output"))
      (should (string-search "changed output" (buffer-string)))
      (should-not (string-search "No output yet." (buffer-string)))))
  :doc "progress updates only selected item; terminal survives zero-live gap and collapse"
  (mevedel-view-execution-test--with-view
    (let ((event (list :session session :data-buffer data-buf :owner "/root"
                       :type 'progress :facts (copy-tree (car live)) :output-tail "new output")))
      (cl-letf (((symbol-function 'mevedel-view--render-status) (lambda (&rest _) (ert-fail "unselected refresh"))))
        (mevedel-view-execution--event event))
      (mevedel-view--render-status)
      (mevedel-view-execution-test--activate "  ▸ Running")
      (setf (plist-get (car live) :output-tail) "new output")
      (mevedel-view-execution--event event)
      (should (string-search "new output" (buffer-string)))
      (setq live nil)
      (mevedel-view-execution--state-changed session data-buf)
      (should mevedel-view-execution--selected)
      (should-not (string-search "[Stop]" (buffer-string)))
      (setq event (plist-put event :type 'terminal)
            event (plist-put event :facts '(:execution-id "e1" :state completed :termination exited :exit-code 0))
            event (plist-put event :whole-output "final retained output"))
      (mevedel-view-execution--event event)
      (should (string-search "Completed (exit 0)" (buffer-string)))
      (should (string-search "final retained output" (buffer-string)))
      (mevedel-view-execution--event (plist-put (copy-tree event) :type 'progress))
      (should (string-search "final retained output" (buffer-string)))
      (mevedel-view-execution-test--activate "  ▾ Completed")
      (should (string-search "✓ Completed bash" (buffer-string)))
      (should-not mevedel-view-execution--selected)
      (should-not (string-search "final retained output" (buffer-string)))))
  :doc "nested agent owner routes passive completion to parent without scanning"
  (mevedel-view-execution-test--with-view
    (let ((child (generate-new-buffer " *execution-child*")))
      (unwind-protect
          (progn
            (setf (plist-get (car live) :owner) "/root/parent/child")
            (with-current-buffer child
              (setq-local mevedel--agent-invocation
                          (mevedel-agent-invocation--create :parent-data-buffer data-buf)))
            (mevedel-view--render-status)
            (mevedel-view-execution-test--activate "  ▸ Running")
            (setq live nil)
            (cl-letf (((symbol-function 'buffer-list) (lambda (&rest _) (ert-fail "scan"))))
              (mevedel-view-execution--event
               (list :session session :data-buffer child :owner "/root/parent/child"
                     :type 'terminal :whole-output "child result"
                     :facts '(:execution-id "e1" :state completed :termination exited :exit-code 0))))
            (should (string-search "child result" (buffer-string)))
            (should (string-search "/root/parent/child" (buffer-string))))
        (kill-buffer child))))
  :doc "real yielded execution events preserve an active draft without consuming output"
  (mevedel-view-test--with-buffers
    (let* ((root (make-temp-file "mevedel-view-execution-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-execution-event-functions '(mevedel-view-execution--event))
           (mevedel-execution-state-change-hook '(mevedel-view-execution--state-changed))
           (mevedel-execution-mailbox-delivery-function (lambda (&rest _) t)))
      (unwind-protect
          (progn
            (with-current-buffer data-buf (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (let* ((observation
                      (test-mevedel-execution--start-managed
                       session root
                       '("sh" "-c" "while [ ! -e go ]; do sleep 0.02; done; printf 'live output\\n'; while [ ! -e finish ]; do sleep 0.02; done; printf 'final output\\n'")
                       :owner "/root" :data-buffer data-buf :tool-use-id "actual-tool"))
                     (id (plist-get (plist-get observation :facts) :execution-id)))
                (ert-info ((format "Initial execution observation: %S" observation))
                  (should id))
                (mevedel-view-execution-test--activate "  ▸ Running")
                (goto-char (point-max))
                (insert (propertize "> active draft\nsecond line" 'draft-test 'kept))
                (backward-char 3)
                (set-mark (- (point) 4))
                (setq mark-active t)
                (let ((draft (buffer-substring (mevedel-view--input-start) (point-max)))
                      (offset (- (point) (mevedel-view--input-start)))
                      (mark-offset (- (mark) (mevedel-view--input-start))))
                  (write-region "go" nil (file-name-concat root "go") nil 'silent)
                  (test-mevedel-execution--wait
                   (lambda () (string-search "    live output" (buffer-string))))
                  (let* ((state (mevedel-session-execution-state session))
                         (record (gethash id (mevedel-execution--state-records state))))
                    (should (= 0 (mevedel-execution--record-read-offset record))))
                  (write-region "finish" nil (file-name-concat root "finish") nil 'silent)
                  (test-mevedel-execution--wait
                   (lambda () (plist-get mevedel-view-execution--selected :terminal-p)))
                  (should (string-search "Completed (exit 0)" (buffer-string)))
                  (should (string-search "    final output" (buffer-string)))
                  (should-not (string-search "[Stop]" (buffer-string)))
                  (should (equal-including-properties
                           draft (buffer-substring (mevedel-view--input-start) (point-max))))
                  (should (= offset (- (point) (mevedel-view--input-start))))
                  (should (= mark-offset (- (mark) (mevedel-view--input-start))))
                  (should mark-active)))))
        (mevedel-execution-teardown-session session)
        (delete-directory root t))))
  :doc "wrong session and equal ID with different owner cannot update selection"
  (mevedel-view-execution-test--with-view
    (mevedel-view--render-status)
    (mevedel-view-execution-test--activate "  ▸ Running")
    (let ((before (buffer-string)))
      (dolist (event (list
                     (list :session (mevedel-session--create) :data-buffer data-buf :owner "/root"
                           :type 'terminal :facts '(:execution-id "e1") :whole-output "wrong session")
                     (list :session session :data-buffer data-buf :owner "/root/other"
                           :type 'terminal :facts '(:execution-id "e1") :whole-output "wrong owner")))
        (mevedel-view-execution--event event))
      (should (equal-including-properties before (buffer-string))))))

(mevedel-deftest mevedel-view-execution--state-changed ()
  ,test
  (test)
  :doc "Monitor batches and selected refresh preserve draft, mark, point and windows"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setf (plist-get (car live) :kind) 'monitor
            (plist-get (car live) :description) "watch files"
            (plist-get (car live) :expires-at) 1800000000
            (plist-get (car live) :event-count) 2)
      (mevedel-view--render-status)
      (mevedel-view-execution-test--activate "  ▸ Watching")
      (goto-char (point-max))
      (insert "> draft\nsecond line")
      (backward-char 3)
      (set-mark (- (point) 5))
      (setq mark-active t)
      (let* ((other (split-window-below))
            (offset (- (point) (mevedel-view--input-start)))
            (mark-offset (- (mark) (mevedel-view--input-start)))
            (windows (window-list))
            (start (window-start)))
        (set-window-point other (+ (mevedel-view--input-start) 2))
        (setf (plist-get (car live) :event-count) 3
              (plist-get (car live) :output-tail) "changed")
        (mevedel-view-execution--state-changed session data-buf)
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (should (= offset (- (point) (mevedel-view--input-start))))
        (should (= mark-offset (- (mark) (mevedel-view--input-start))))
        (should mark-active)
        (should (equal windows (window-list)))
        (should (= 2 (- (window-point other) (mevedel-view--input-start))))
        (should (= start (window-start)))
        (should (string-search "until " (buffer-string)))
        (should (string-search "3 events" (buffer-string)))
        (should (string-search "changed" (buffer-string)))))))

(mevedel-deftest mevedel-view-execution--callback ()
  ,test
  (test)
  :doc "Stop uses user authority; stale terminal and session controls are guarded"
  (mevedel-view-execution-test--with-view
    (let (stopped)
      (cl-letf (((symbol-function 'mevedel-execution-stop-user)
                 (lambda (owner id) (setq stopped (list owner id)))))
        (mevedel-view--render-status)
        (mevedel-view-execution-test--activate "  ▸ Running")
        (mevedel-view-execution-test--at "[Stop]")
        (let ((callback (get-text-property (point) 'mevedel-view-zone-activate)))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal stopped (list session "e1")))
          (setq live nil stopped nil)
          (should-error (funcall callback) :type 'user-error)
          (should-not stopped)
          (with-current-buffer data-buf (setq-local mevedel--session (mevedel-session--create)))
          (should-error (funcall callback) :type 'user-error)))))
  :doc "explicit conversation action uses the current snapshot"
  (mevedel-view-execution-test--with-view
    (let (shown)
      (cl-letf (((symbol-function 'mevedel-view-audit-show-result) (lambda (record) (setq shown record))))
        (mevedel-view--render-status)
        (mevedel-view-execution-test--activate "  ▸ Running")
        (mevedel-view-execution-test--activate "[Show in conversation]")
        (should (equal "tool1" (plist-get shown :tool-use-id)))
        (should (equal "e1" (plist-get shown :execution-id)))))))

(mevedel-deftest mevedel-view-execution-show-in-conversation ()
  ,test
  (test)
  :doc "settled agent navigation prepares its scheduled projection before locating the row"
  (mevedel-view-execution-test--with-view
    (save-window-excursion
      (let* ((path "/root/settled")
             (agent-data (generate-new-buffer " *execution-settled-agent*"))
             (record (mevedel-agent-record--create
                      :id "settled-agent" :path path :parent-path "/root"
                      :role "default" :activity 'idle
                      :conversation-buffer agent-data))
             agent-view)
        (unwind-protect
            (progn
              (setf (mevedel-session-agent-registry session) (list (cons path record)))
              (with-current-buffer agent-data
                (org-mode)
                (setq-local mevedel--session session
                            mevedel--view-buffer view-buf)
                (insert (propertize
                         "(:name \"Bash\" :args (:command \"printf result\"))\n\nRESULT\n"
                         'gptel '(tool . "tool1"))))
              (switch-to-buffer view-buf)
              (goto-char (point-max))
              (insert "> draft\nsecond line")
              (backward-char 3)
              (progn
                (setf (plist-get (car live) :owner) path)
                (mevedel-view--render-status)
                (mevedel-view-execution-test--activate "  ▸ Running")
                (cl-letf (((symbol-function 'mevedel-view-audit--evidence)
                           (lambda (_) (ert-fail "Existing agent row was not revealed"))))
                  (mevedel-view-execution-test--activate "[Show in conversation]"))
                (setq agent-view (window-buffer (selected-window)))
                (with-current-buffer agent-view
                  (should mevedel-view--agent-transcript-p)
                  (should (equal "tool1" (get-text-property (point) 'mevedel-view-tool-use-id)))
                  (should-not (get-text-property (point) 'mevedel-view-collapsed)))
                (with-current-buffer view-buf
                  (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))
          (unless agent-view
            (when-let* ((window (buffer-local-value
                                'mevedel-view--agent-transcript-window view-buf))
                        ((window-live-p window)))
              (setq agent-view (window-buffer window))))
          (when (buffer-live-p agent-view) (kill-buffer agent-view))
          (when (buffer-live-p agent-data) (kill-buffer agent-data)))))))

(mevedel-deftest mevedel-view-execution--full-output ()
  ,test
  (test)
  :doc "Full output reads real spool into private read-only buffer, not visiting buffer"
  (mevedel-view-execution-test--with-view
    (let* ((path (make-temp-file "mevedel-output-" nil nil "all output\n"))
           (visiting (find-file-noselect path))
           output)
      (unwind-protect
          (progn
            (with-current-buffer visiting (fundamental-mode))
            (setf (plist-get (car live) :artifact-path) path)
            (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq output buffer))))
              (mevedel-view--render-status)
              (mevedel-view-execution-test--activate "  ▸ Running")
              (mevedel-view-execution-test--activate "[Full output]"))
            (should (eq 'fundamental-mode (buffer-local-value 'major-mode visiting)))
            (should-not (buffer-local-value 'buffer-read-only visiting))
            (with-current-buffer output
              (should buffer-read-only)
              (should-not buffer-file-name)
              (should (string-search "all output" (buffer-string)))))
        (when (buffer-live-p output) (kill-buffer output))
        (kill-buffer visiting)
        (delete-file path))))
  :doc "missing full output never presents a truncated tail as full output"
  (mevedel-view-execution-test--with-view
    (let (output)
      (unwind-protect
          (progn
            (setf (plist-get (car live) :output-tail) "only the tail"
                  (plist-get (car live) :output-preview-truncated-p) t)
            (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq output buffer))))
              (mevedel-view-execution--full-output session (car live)))
            (with-current-buffer output
              (should (string-search "Full output unavailable" (buffer-string)))
              (should (string-search "Preview (not full output) — truncated" (buffer-string)))))
        (when (buffer-live-p output) (kill-buffer output)))))
  :doc "completion uses retained canonical artifact when local spool is gone"
  (mevedel-view-execution-test--with-view
    (let* ((directory (file-name-concat mevedel-user-dir "session"))
           (path (file-name-concat directory "tool-results" "output.log"))
           output)
      (make-directory (file-name-directory path) t)
      (write-region "canonical whole output\n" nil path nil 'silent)
      (setf (mevedel-session-save-path session) directory)
      (setq mevedel-view-execution--selected
            (append (list :session session :terminal-p t :state 'completed
                          :output-path "artifact://output.log" :artifact-path "/missing/spool"
                          :whole-output "preview" :whole-output-truncated-p t)
                    (car live))
            live nil)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq output buffer))))
              (mevedel-view-execution--full-output session mevedel-view-execution--selected))
            (with-current-buffer output
              (should (string-search "canonical whole output" (buffer-string)))
              (should-not (string-search "Full output unavailable" (buffer-string)))))
        (when (buffer-live-p output) (kill-buffer output)))))
  :doc "empty spool cannot prove no output when bytes were lost or execution is lost"
  (mevedel-view-execution-test--with-view
    (let ((path (make-temp-file "mevedel-empty-output-")))
      (unwind-protect
          (dolist (facts '((:output-bytes 10) (:state lost :output-bytes 0)))
            (setq mevedel-view-execution--selected
                  (append (list :session session :terminal-p t :execution-id "e1"
                                :owner "/root" :artifact-path path) facts)
                  live nil)
            (let (output)
              (unwind-protect
                  (progn
                    (cl-letf (((symbol-function 'display-buffer)
                               (lambda (buffer &rest _) (setq output buffer))))
                      (mevedel-view-execution--full-output session mevedel-view-execution--selected))
                    (with-current-buffer output
                      (should (string-search "output unavailable" (buffer-string)))
                      (should-not (string-search "No output." (buffer-string)))))
                (when (buffer-live-p output) (kill-buffer output)))))
        (delete-file path))))
  :doc "terminal retained whole, empty, lost, missing and truncated outputs stay distinct"
  (mevedel-view-execution-test--with-view
    (dolist (case '(((:whole-output "complete output") . "complete output")
                    ((:whole-output "" :output-bytes 0) . "No output.")
                    ((:state lost) . "Output unavailable.")
                    ((:output-bytes 20 :artifact-path "/missing/spool") . "Full output unavailable.")
                    ((:whole-output "bounded" :whole-output-truncated-p t) . "Preview (not full output) — truncated")))
      (setq mevedel-view-execution--selected
            (append (list :session session :terminal-p t :execution-id "e1" :owner "/root")
                    (car case))
            live nil)
      (let (output)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq output buffer))))
                (mevedel-view-execution--full-output session mevedel-view-execution--selected))
              (with-current-buffer output (should (string-search (cdr case) (buffer-string)))))
          (when (buffer-live-p output) (kill-buffer output)))))))

(provide 'test-mevedel-view-execution)
;;; test-mevedel-view-execution.el ends here
