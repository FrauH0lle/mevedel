;;; test-niwa-view-execution.el --- Inline execution tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the status fragment through managed-zone rendering and its real
;; keyboard/mouse activation path, with public execution snapshots as inputs.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-execution)
(require 'niwa-view-audit)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-execution)
(require 'niwa-execution-spool)
(require 'niwa-agents)
(require 'niwa-agent-control)
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))

(defmacro niwa-view-execution-test--with-view (&rest body)
  "Run BODY in a real view with SESSION and mutable public LIVE snapshots.
Input event variables are rebound because `execute-kbd-macro' leaves its
mouse events in them, which later interactive calls would read."
  (declare (indent 0) (debug t))
  `(niwa-view-test--with-buffers
     (let ((session (niwa-session--create :name "execution-ui" :root-buffer data-buf))
           (live (list (list :execution-id "e1" :owner "/root" :tool-use-id "tool1"
                             :yielded t :state 'running :command "same command"
                             :output-tail "" :output-bytes 0)))
           (last-command-event last-command-event)
           (last-input-event last-input-event)
           (last-nonmenu-event last-nonmenu-event))
       (with-current-buffer data-buf (setq-local niwa--session session))
       (with-current-buffer view-buf
         (cl-letf (((symbol-function 'niwa-execution-count-user)
                    (lambda (owner) (should (eq owner session)) (length live)))
                   ((symbol-function 'niwa-execution-list-user)
                    (lambda (owner) (should (eq owner session)) (copy-tree live)))
                   ;; Window/point tests model an attended frame even in a
                   ;; graphical runner deliberately opened without focus.
                   ((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                   ;; A real desktop pointer must not add an accidental hold
                   ;; to keyboard/grace tests.  Mouse tests override this.
                   ((symbol-function 'mouse-pixel-position)
                    (lambda () (cons (selected-frame) '(-1 . -1)))))
           ,@body)))))

(defun niwa-view-execution-test--draft-offset (&optional mark)
  "Return the composer caret (or MARK) offset from the draft start."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (- (if mark (mark) (point)) (niwa-view-composer-draft-start view)))))

(defun niwa-view-execution-test--set-draft-mark (distance)
  "Activate the composer mark DISTANCE characters before its caret."
  (with-current-buffer (niwa-view-composer-buffer)
    (set-mark (- (point) distance))
    (setq mark-active t)))

(defun niwa-view-execution-test--at (text)
  "Move to the beginning of TEXT in the current rendered view."
  (goto-char (point-min))
  (search-forward text)
  (backward-char (length text)))

(defun niwa-view-execution-test--activate (text)
  "Activate rendered TEXT with its actual RET binding."
  (niwa-view-execution-test--at text)
  (call-interactively (key-binding (kbd "RET"))))

(defun niwa-view-execution-test--assert-held-row (label expected-face metadata)
  "Assert rendered LABEL has EXPECTED-FACE, dim METADATA and an inert row."
  (niwa-view-execution-test--at label)
  (let ((start (line-beginning-position))
        (end (line-end-position))
        (glyph (point)))
    (dolist (property '(face font-lock-face))
      (should (eq expected-face (get-text-property glyph property)))
      (should (eq expected-face (get-text-property (+ 2 glyph) property)))
      (dolist (text metadata)
        (save-excursion
          (search-forward text end)
          (should (eq 'niwa-view-tool-metadata
                      (get-text-property (1- (point)) property))))))
    (dolist (property '(niwa-view-zone-activate mouse-face
                       niwa-view-zone-toggle))
      (should-not (text-property-not-all start end property nil)))))

(niwa-deftest niwa-view-execution-fragment ()
  ,test
  (test)
  :doc "zero work avoids listing, resources, buffers and timers"
  ;; Installing a primitive's native trampoline can itself schedule a timer.
  (let ((native-comp-enable-subr-trampolines nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'niwa-execution-count-user) (lambda (_) 0))
                ((symbol-function 'niwa-execution-list-user) (lambda (_) (ert-fail "listed")))
                ((symbol-function 'buffer-list) (lambda (&rest _) (ert-fail "scanned")))
                ((symbol-function 'run-at-time) (lambda (&rest _) (ert-fail "timer")))
                ((symbol-function 'niwa-resource-visit-path) (lambda (&rest _) (ert-fail "resource"))))
        (should-not (niwa-view-execution-fragment (niwa-session--create))))))
  :doc "limits live rows, excludes foreground, preserves selected hidden identity"
  (niwa-view-execution-test--with-view
    (setq live (cl-loop for n from 1 to 8
                        collect (list :execution-id (format "e%d" n) :owner "/root"
                                      :yielded (/= n 8) :state 'running :command "same command")))
    (let* ((body (plist-get (niwa-view-execution-fragment session) :body)))
      (should (= 5 (niwa-view-test--count-substring "▸ Running" body)))
      (should (string-search "[Show all 7]  [Manage]" body)))
    (setq niwa-view-execution--selected (plist-put (copy-sequence (nth 6 live)) :session session))
    (let ((body (plist-get (niwa-view-execution-fragment session) :body)))
      (should (= 4 (niwa-view-test--count-substring "▸ Running" body)))
      (should (= 1 (niwa-view-test--count-substring "▾ Running" body))))
    (setq niwa-view-execution--selected nil
          niwa-view-execution--show-all t)
    (let ((body (plist-get (niwa-view-execution-fragment session) :body)))
      (should (= 7 (niwa-view-test--count-substring "▸ Running" body)))
      (should (string-search "[Show fewer]  [Manage]" body))))
  :doc "few rows offer only Manage; collapse leaves the header with its counts"
  (niwa-view-execution-test--with-view
    (let ((body (plist-get (niwa-view-execution-fragment session) :body)))
      (should (string-search "\n  [Manage]\n" body))
      (should-not (string-search "[Show" body)))
    (niwa-view-zone-set-collapse-state niwa-view-execution--collapse-key t)
    (let ((fragment (niwa-view-execution-fragment session)))
      (should (plist-get fragment :collapsed))
      (should (string-match-p "\\` ─── background work · 1 command \\[\\+\\] ─+\n\\'"
                              (substring-no-properties (plist-get fragment :body))))))
  :doc "rows are newest first; a blocking agent keeps its place, children follow parents"
  (niwa-view-execution-test--with-view
    (setq live (list (list :execution-id "m1" :owner "/root" :yielded t :state 'running
                           :kind 'monitor :description "test ticker" :event-count 1
                           :started-at 20.0)
                     (list :execution-id "e1" :owner "/root/counter" :yielded t
                           :state 'running :command "make test" :description "Run tests"
                           :started-at 30.0)))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional time) (if time (time-convert time 'integer) 60.0)))
              ((symbol-function 'niwa-view--agent-status-collect)
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
                   (plist-get (niwa-view-execution-fragment session) :body))))
        (should (string-prefix-p
                 " ─── background work · 1 blocked · 3 agents · 1 command · 1 monitor [-] ─"
                 text))
        (should (string-search
                 (concat "  ▸ Running   bash     Run tests · 30s · /root/counter\n"
                         "  ▸ Watching  monitor  test ticker · 40s · 1 event\n"
                         "  ▸ Running   agent    /root/counter · 50s · opus:medium\n"
                         "    ▸ Running   agent    /root/counter/sub · 10s\n"
                         "  ▸ Blocked   agent    /root/reviewer · 55s · waiting for permission\n")
                 text)))))
  :doc "equal starts keep a held agent and retained execution in place across completion"
  (niwa-view-execution-test--with-view
    (let ((agents (list (list :path "/root/counter" :status 'running :depth 0
                              :started-at 0.0))))
      (setf (plist-get (car live) :started-at) 0.0)
      (cl-letf (((symbol-function 'niwa-view--agent-status-collect) (lambda () agents))
                ((symbol-function 'niwa-view-execution--pointer-on-panel-p) (lambda () t)))
        (unwind-protect
            (progn
              (niwa-view--render-status)
              (niwa-view-test--insert-composer-draft "> draft\nsecond line")
              (niwa-view-execution-test--activate "  ▸ Running   bash")
              (goto-char (niwa-view-transcript-tail))
              (let ((action-line (save-excursion
                                   (niwa-view-execution-test--at "[Full output]")
                                   (line-number-at-pos))))
                (setq agents nil)
                (dotimes (_ 2)
                  (niwa-view--render-status)
                  (should (string-search "✓ Finished  agent    /root/counter" (buffer-string)))
                  (should (= action-line (save-excursion
                                           (niwa-view-execution-test--at "[Full output]")
                                           (line-number-at-pos)))))
                (setq live nil)
                (niwa-view-execution--event
                 (list :session session :data-buffer data-buf :owner "/root" :type 'terminal
                       :facts '(:execution-id "e1" :state completed :termination exited :exit-code 0)))
                (dotimes (_ 2)
                  (niwa-view--render-status)
                  (should (plist-get niwa-view-execution--selected :terminal-p))
                  (should (= action-line (save-excursion
                                           (niwa-view-execution-test--at "[Full output]")
                                           (line-number-at-pos))))
                  (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))))
          (niwa-view-execution--cancel-release)))))
  :doc "a finished row lingers while point is on the panel and the grace period restarts on leaving"
  (niwa-view-execution-test--with-view
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
          (niwa-view--render-status)
          (niwa-view-execution-test--at "  ▸ Running   bash     serve")
          ;; e1 finishes with exit 0 while point is on the panel.
          (niwa-view-execution--event
           (list :session session :data-buffer data-buf :owner "/root" :type 'terminal
                 :facts '(:execution-id "e1" :state completed :termination exited :exit-code 0)))
          (setq live (list (cadr live)))
          (niwa-view--render-status)
          (unwind-protect
              (progn
                (should (string-search "  ✓ Completed bash     Run tests · exit 0\n" (buffer-string)))
                (niwa-view-execution-test--at "  ✓ Completed")
                (should-not (get-text-property (point) 'niwa-view-zone-activate))
                (should (timerp niwa-view-execution--release-timer))
                ;; Long past the grace period, point is still on the panel.
                (setq clock 1100.0)
                (niwa-view-execution--check-release view-buf)
                (niwa-view--render-status)
                (should (string-search "✓ Completed" (buffer-string)))
                ;; Leaving restarts the full grace period.
                (goto-char (niwa-view-transcript-tail))
                (niwa-view-execution--check-release view-buf)
                (setq clock 1109.0)
                (niwa-view-execution--check-release view-buf)
                (should (string-search "✓ Completed" (buffer-string)))
                (setq clock 1110.5)
                (niwa-view-execution--check-release view-buf)
                (should-not (string-search "✓ Completed" (buffer-string)))
                (niwa-view-execution--check-release view-buf)
                (should-not niwa-view-execution--release-timer)
                (should (string-search "▸ Running   bash     serve" (buffer-string))))
            (niwa-view-execution--cancel-release))))))
  :doc "a hidden overflow row that finishes is not held"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setq live (cl-loop for n from 1 to 7
                          collect (list :execution-id (format "e%d" n) :owner "/root"
                                        :yielded t :state 'running
                                        :command (format "cmd %d" n)
                                        :started-at (float n))))
      (niwa-view--render-status)
      ;; Newest first: e7..e3 shown, e2 and e1 hidden behind [Show all 7].
      (niwa-view-execution-test--at "  ▸ Running   bash     cmd 7")
      (setq live (cl-remove "e1" live :key (lambda (item) (plist-get item :execution-id))
                            :test #'equal))
      (unwind-protect
          (progn
            (niwa-view--render-status)
            (ert-info ("a finished row that was hidden is not brought back as held")
              (should-not niwa-view-execution--held)
              (should-not (string-search "✓ Completed" (buffer-string)))))
        (niwa-view-execution--cancel-release))))
  :doc "at capacity under point, shown rows stay put and new work is only counted"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setq live (cl-loop for n from 1 to 5
                          collect (list :execution-id (format "e%d" n) :owner "/root"
                                        :yielded t :state 'running
                                        :command (format "cmd %d" n)
                                        :started-at (float n))))
      (niwa-view--render-status)
      (niwa-view-execution-test--at "  ▸ Running   bash     cmd 1")
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
                (niwa-view--render-status)
                (setq live (append live (list (list :execution-id "e6" :owner "/root"
                                                    :yielded t :state 'running
                                                    :command "cmd 6" :started-at 6.0))))
                (niwa-view--render-status)
                (ert-info ("the finished row stays and no shown row moves")
                  (should (string-search "✓ Completed bash     cmd 1" (buffer-string)))
                  (should-not (string-search "cmd 6" (buffer-string)))
                  (should (= line (funcall line-of "cmd 3")))
                  (should (string-search "[Show all 6]" (buffer-string))))
                ;; Leaving shows new work and starts the held row's grace.
                (goto-char (niwa-view-transcript-tail))
                (niwa-view-execution--check-release view-buf)
                (should (string-search "▸ Running   bash     cmd 6" (buffer-string)))
                (should (string-search "✓ Completed" (buffer-string))))
            (niwa-view-execution--cancel-release))))))
  :doc "a row finishing while point is elsewhere lingers for the grace period"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (let* ((clock 1000.0)
             (float-time-orig (symbol-function 'float-time)))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time) (if time (funcall float-time-orig time) clock))))
          (niwa-view--render-status)
          (goto-char (niwa-view-transcript-tail))
          (setq live nil)
          (unwind-protect
              (progn
                (niwa-view--render-status)
                (should (string-search "✓ Completed bash     same command" (buffer-string)))
                (setq clock (+ 1000.0 (1- niwa-view-execution-grace-seconds)))
                (niwa-view-execution--check-release view-buf)
                (should (string-search "✓ Completed" (buffer-string)))
                (setq clock (+ 1000.0 niwa-view-execution-grace-seconds 0.5))
                (niwa-view-execution--check-release view-buf)
                (should-not (niwa-view-execution-fragment session))
                (should-not niwa-view-execution--held))
            (niwa-view-execution--cancel-release))))))
  :doc "changing sessions drops retained selection even with equal IDs"
  (niwa-view-execution-test--with-view
    (setq niwa-view-execution--selected (plist-put (copy-sequence (car live)) :session session))
    (setq session (niwa-session--create) live nil)
    (should-not (niwa-view-execution-fragment session))
    (should-not niwa-view-execution--selected))
  :doc "hidden wall time does not invalidate unchanged rendered properties"
  (niwa-view-execution-test--with-view
    (let ((before (plist-get (niwa-view-execution-fragment session) :body)))
      (setf (plist-get (car live) :wall-time-seconds) 20)
      (should (equal-including-properties
               before (plist-get (niwa-view-execution-fragment session) :body)))))
  :doc "every panel row, rule and preview line is one screen row in a narrow window, live and finished"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (split-window-right 40)
      (setq live (list (list :execution-id "m1" :owner "/root/a_rather_long_agent_name"
                             :yielded t :state 'running :kind 'monitor :event-count 9
                             :expires-at (current-time)
                             :description "Watch CI for the release branch build and report"
                             :started-at 1.0 :output-tail (make-string 90 ?o))))
      (setq niwa-view-execution--selected
            (plist-put (copy-sequence (car live)) :session session))
      (let ((agents (list (list :path "/root/reviewer_with_a_long_descriptive_name"
                                :status 'running :depth 0 :started-at 4.0
                                :model-metadata '(:model "claude-opus-5-5" :effort "high")))))
        (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                   (lambda () agents)))
          (let ((panel-lines
                 (lambda ()
                   (let ((bounds (niwa-view-zone-fragment-bounds 'status 'background)))
                     (split-string (buffer-substring-no-properties
                                                  (plist-get bounds :start)
                                                  (plist-get bounds :end))
                                   "\n")))))
            (niwa-view--render-status)
            (let ((before (length (funcall panel-lines))))
              (should (< (niwa-view-zone-width) 40))
              (dolist (line (funcall panel-lines))
                (should (< (string-width line) (niwa-view-zone-width))))
              (setq agents nil)
              (unwind-protect
                  (progn
                    (niwa-view--render-status)
                    (should (string-search "✓ Finished" (buffer-string)))
                    (should (= before (length (funcall panel-lines))))
                    (dolist (line (funcall panel-lines))
                      (should (< (string-width line) (niwa-view-zone-width)))))
                (niwa-view-execution--cancel-release))))))))
  :doc "a narrower window re-renders the panel's rule and rows for its width"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (niwa-view--render-status)
      (niwa-view-test--insert-composer-draft "> draft\nsecond line")
      (let ((rule-width (lambda ()
                          (save-excursion
                            (niwa-view-execution-test--at "background work")
                            (string-width (buffer-substring-no-properties
                                           (line-beginning-position) (line-end-position)))))))
        (should (= (- (window-body-width) 4) (funcall rule-width)))
        (split-window-right 40)
        (niwa-view-zone--on-window-change (selected-window))
        (should (= (- (niwa-view-zone-width) 4) (funcall rule-width)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))))
  :doc "a finished inspected execution keeps its place among the rows"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setq live (list (list :execution-id "e1" :owner "/root" :yielded t :state 'running
                             :command "make test-oldest" :started-at 1.0)
                       (list :execution-id "e2" :owner "/root" :yielded t :state 'running
                             :command "npm run build" :started-at 2.0)
                       (list :execution-id "e3" :owner "/root" :yielded t :state 'running
                             :command "cargo check-newest" :started-at 3.0 :output-tail "ok")))
      (setq niwa-view-execution--selected
            (plist-put (copy-sequence (nth 2 live)) :session session))
      (niwa-view--render-status)
      (niwa-view-test--insert-composer-draft "> draft\nsecond line")
      (let ((order (lambda ()
                     (mapcar (lambda (text)
                               (save-excursion (niwa-view-execution-test--at text) (point)))
                             '("cargo check-newest" "npm run build" "make test-oldest")))))
        (should (apply #'< (funcall order)))
        (setq live (butlast live))
        (unwind-protect
            (progn
              (niwa-view--render-status)
              (should (string-search "Completed" (buffer-string)))
              (should (apply #'< (funcall order)))
              (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
          (niwa-view-execution--cancel-release)))))
  :doc "point stays on its row when a row above it finishes"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setf (plist-get (car live) :started-at) 1.0)
      (let ((agents (list (list :path "/root/worker" :status 'running :depth 0
                                :model-metadata '(:model "claude-opus-5-5" :effort "high")
                                :started-at 4.0))))
        (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                   (lambda () agents)))
          (niwa-view--render-status)
          (niwa-view-test--insert-composer-draft "> draft\nsecond line")
          (niwa-view-execution-test--at "same command")
          (setq agents nil)
          (unwind-protect
              (progn
                (niwa-view--render-status)
                (should (string-search "✓ Finished  agent    /root/worker" (buffer-string)))
                (should (looking-at-p "same command"))
                (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
            (niwa-view-execution--cancel-release))))))
  :doc "output arriving above point never moves point onto another execution's Stop"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setq live (list (list :execution-id "e1" :owner "/root" :yielded t :state 'running
                             :command "make test-oldest" :started-at 1.0)
                       (list :execution-id "e2" :owner "/root" :yielded t :state 'running
                             :command "npm run build" :started-at 2.0 :output-tail "x")))
      (setq niwa-view-execution--selected
            (plist-put (copy-sequence (nth 1 live)) :session session))
      (niwa-view--render-status)
      (niwa-view-execution-test--at "make test-oldest")
      (forward-char 6)
      (setf (plist-get (nth 1 live) :output-tail) (make-string 36 ?a))
      (niwa-view--render-status)
      (should (looking-at-p "est-oldest"))
      (should-not (get-text-property (point) 'help-echo))))
  :doc "pending reduction survives failed computation, but an empty fragment consumes it"
  (niwa-view-execution-test--with-view
    (setq niwa-view-execution--reflow-pending t)
    (let (entered)
      (cl-letf (((symbol-function 'niwa-view-execution--line)
                 (lambda (&rest _)
                   (setq entered t)
                   (error "Injected background row failure"))))
        (should (equal '(error "Injected background row failure")
                       (should-error (niwa-view-execution-fragment session)))))
      (should entered))
    (should niwa-view-execution--reflow-pending)
    (should (niwa-view-execution-fragment session))
    (should-not niwa-view-execution--reflow-pending)
    ;; No previous shown rows means no held completions mask the empty case.
    (setq niwa-view-execution--shown nil
          niwa-view-execution--reflow-pending t
          live nil)
    (should-not (niwa-view-execution-fragment session))
    (should-not niwa-view-execution--reflow-pending)
    (setq live (list '(:execution-id "later" :owner "/root" :yielded t
                      :state running :command "later work")))
    (niwa-view--render-status)
    (should (string-search "later work" (buffer-string)))
    (should-not niwa-view-execution--reflow-pending)))

(niwa-deftest niwa-view-execution--controls ()
  ,test
  (test)
  :doc "Show fewer applies under stationary point, mouse-only hover, and another window's point"
  (dolist (source '(point mouse other-window))
    (ert-info ((format "hover source: %S" source))
      (niwa-view-execution-test--with-view
        (save-window-excursion
          (switch-to-buffer view-buf)
          (setq live (cl-loop for n from 1 to 7
                              collect (list :execution-id (format "e%d" n) :owner "/root"
                                            :yielded t :state 'running :started-at (float n)
                                            :command (format "cmd %d" n))))
          (let ((window (selected-window))
                (other (when (eq source 'other-window) (split-window-below))))
            (cl-letf (((symbol-function 'mouse-pixel-position)
                       (lambda () (cons (selected-frame)
                                        (if (eq source 'mouse) '(1 . 1) '(-1 . -1)))))
                      ((symbol-function 'posn-at-x-y)
                       (lambda (&rest _)
                         (list window
                               (1+ (plist-get (niwa-view-zone-fragment-bounds
                                               'status 'background) :start))
                               '(1 . 1) 0))))
              (niwa-view--render-status)
              (niwa-view-execution-test--activate "[Show all 7]")
              (should (= 7 (niwa-view-test--count-substring "▸ Running" (buffer-string))))
              (niwa-view-test--insert-composer-draft "> draft\nsecond line")
              (let ((control (save-excursion
                               (niwa-view-execution-test--at "[Show fewer]")
                               (point))))
                (pcase source
                  ('point (goto-char control))
                  ('other-window (set-window-point other control)))
                (should (niwa-view-execution--pointer-on-panel-p))
                ;; Invoke the rendered control without moving composer point
                ;; in the mouse-only and other-window cases.
                (funcall (get-text-property control 'niwa-view-zone-activate))
                (dotimes (_ 3)
                  (should (= 5 (niwa-view-test--count-substring "▸ Running" (buffer-string))))
                  (should (string-search "[Show all 7]" (buffer-string)))
                  (should-not niwa-view-execution--reflow-pending)
                  (should (niwa-view-execution--pointer-on-panel-p))
                  (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
                  (niwa-view--render-status)))))))))
  :doc "queued reduction drains once, accounts for shown completion, and keeps true hover and selection"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setq live (cl-loop for n from 1 to 7
                          collect (list :execution-id (format "e%d" n) :owner "/root"
                                        :yielded t :state 'running :started-at (float n)
                                        :command (format "cmd %d" n))))
      (niwa-view--render-status)
      (niwa-view-execution-test--activate "[Show all 7]")
      (niwa-view-execution-test--activate "cmd 1")
      (niwa-view-test--insert-composer-draft "> draft\nsecond line")
      (niwa-view-execution-test--at "[Show fewer]")
      (let ((before (buffer-string)))
        (unwind-protect
            (progn
              (niwa-view-render-mutate
               'test-background-writer
               (lambda ()
                 (niwa-view-execution-test--activate "[Show fewer]")
                 (should niwa-view-execution--reflow-pending)
                 (should (equal-including-properties before (buffer-string)))
                 ;; e2 was shown before the queued action.  It must still be
                 ;; held even though reduction would normally hide its row.
                 (setq live (cl-remove "e2" live :test #'equal
                                       :key (lambda (item) (plist-get item :execution-id))))
                 (push '(:execution-id "e8" :owner "/root" :yielded t
                         :state running :started-at 8.0 :command "cmd 8") live)
                 (niwa-view--render-status)
                 (should niwa-view-execution--reflow-pending)
                 (should (= 7 (length niwa-view-execution--shown)))))
              (should-not niwa-view-render--owner)
              (should-not niwa-view-render--pending)
              (should-not niwa-view-execution--reflow-pending)
              (should niwa-view-execution--hovering)
              (should (string-search "▾ Running   bash     cmd 1" (buffer-string)))
              (should (string-search "✓ Completed bash     cmd 2" (buffer-string)))
              (should (string-search "▸ Running   bash     cmd 8" (buffer-string)))
              (should-not (string-search "bash     cmd 3" (buffer-string)))
              (should (= 4 (niwa-view-test--count-substring "▸ Running" (buffer-string))))
              ;; Time alone must not release the completion under real point.
              (setf (plist-get (nth 2 (car niwa-view-execution--held)) :release-at) 0.0)
              (niwa-view--render-status)
              (should (string-search "✓ Completed bash     cmd 2" (buffer-string)))
              (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
          (niwa-view-execution--cancel-release)))))
  :doc "Show all supersedes a queued Show fewer through the still-rendered control"
  (niwa-view-execution-test--with-view
    (setq live (cl-loop for n from 1 to 7
                        collect (list :execution-id (format "e%d" n) :owner "/root"
                                      :yielded t :state 'running :command "work")))
    (niwa-view--render-status)
    (niwa-view-execution-test--activate "[Show all 7]")
    (niwa-view-render-mutate
     'test-background-writer
     (lambda ()
       (niwa-view-execution-test--activate "[Show fewer]")
       (should niwa-view-execution--reflow-pending)
       ;; The display has not changed yet; its callback toggles current intent.
       (niwa-view-execution-test--activate "[Show fewer]")
       (should niwa-view-execution--show-all)
       (should-not niwa-view-execution--reflow-pending)))
    (should (= 7 (niwa-view-test--count-substring "▸ Running" (buffer-string))))
    (should (string-search "[Show fewer]" (buffer-string)))
    (should-not niwa-view-execution--reflow-pending)))

(niwa-deftest niwa-view-execution--line ()
  ,test
  (test)
  :doc "whole row including leading spaces and newline toggles with RET"
  (niwa-view-execution-test--with-view
    (niwa-view--render-status)
    (dolist (where '(leading chevron label newline))
      (niwa-view-execution-test--at "  ▸ Running")
      (pcase where
        ('chevron (forward-char 2))
        ('label (search-forward "same"))
        ('newline (end-of-line)))
      (should (get-text-property (point) 'read-only))
      (should (functionp (get-text-property (point) 'niwa-view-zone-activate)))
      (call-interactively (key-binding (kbd "RET")))
      (should (equal "e1" (plist-get niwa-view-execution--selected :execution-id)))
      (should (string-search "Preview (not full output)" (buffer-string)))
      (niwa-view-execution-test--activate "  ▾ Running")
      (should-not niwa-view-execution--selected)))
  :doc "real mouse binding toggles clicked row, not point's old location"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-view--render-status)
      (niwa-view-execution-test--at "  ▸ Running")
      (let* ((position (point))
             (event (list 'mouse-1 (list (selected-window) position '(0 . 0) 0)))
             (command (lookup-key (get-text-property position 'keymap) [mouse-1])))
        (goto-char (niwa-view-transcript-tail))
        (funcall command event)
        (should niwa-view-execution--selected))))
  :doc "TAB toggles only its row, including preview controls, and preserves the draft"
  (niwa-view-execution-test--with-view
    (setq live (append live (list (list :execution-id "e2" :owner "/root"
                                      :yielded t :state 'running :kind 'monitor
                                      :description "Watch errors"))))
    (niwa-view--render-status)
    (niwa-view-test--insert-composer-draft "> draft\nsecond line")
    (cl-letf (((symbol-function 'niwa-execution-stop-user)
               (lambda (&rest _) (ert-fail "TAB stopped an execution"))))
      (dolist (text '("same command" "Watch errors"))
        (niwa-view-execution-test--at text)
        (call-interactively (key-binding (kbd "TAB")))
        (should niwa-view-execution--selected)
        (should-not (niwa-view-zone-collapse-state
                     niwa-view-execution--collapse-key))
        (should (string-search "Preview (not full output)" (buffer-string)))
        (niwa-view-execution-test--at "[Stop]")
        (call-interactively (key-binding (kbd "TAB")))
        (should-not niwa-view-execution--selected)
        (should (string-search "same command" (buffer-string)))
        (should (string-search "Watch errors" (buffer-string)))))
    (dolist (offset '(0 -2))
      (niwa-view-execution-test--at "[Manage]")
      (forward-char offset)
      (call-interactively (key-binding (kbd "TAB")))
      (should-not (niwa-view-zone-collapse-state
                   niwa-view-execution--collapse-key)))
    (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
  :doc "blank suffix and header margins are inert, Manage is explicit"
  (niwa-view-execution-test--with-view
    (niwa-view--render-status)
    (niwa-view-execution-test--at " ─── background work")
    (should-not (get-text-property (point) 'niwa-view-zone-activate))
    (niwa-view-execution-test--at "  ▸ Running")
    (forward-line 2)
    (should (eq (char-after) ?\n))
    (should-not (get-text-property (point) 'niwa-view-zone-activate))
    (let (opened)
      (cl-letf (((symbol-function 'niwa-background-list-open) (lambda (&optional _) (setq opened t))))
        (niwa-view-execution-test--activate "[Manage]")
        (should opened))))
  :doc "hover highlight covers one row, not its neighbours"
  (niwa-view-execution-test--with-view
    (setq live (append live (list (plist-put (copy-sequence (car live)) :execution-id "e2"))))
    (niwa-view--render-status)
    (niwa-view-execution-test--at "  ▸ Running")
    (let ((start (point)))
      (should (eq 'highlight (get-text-property start 'mouse-face)))
      (end-of-line)
      ;; The newline still activates the row but ends its highlight run.
      (should-not (get-text-property (point) 'mouse-face))
      (should (functionp (get-text-property (point) 'niwa-view-zone-activate)))
      (should (= (point) (next-single-property-change start 'mouse-face)))))
  :doc "same-command IDs have independent selection and one expanded item"
  (niwa-view-execution-test--with-view
    (setq live (append live (list (plist-put (copy-sequence (car live)) :execution-id "e2"))))
    (niwa-view--render-status)
    (niwa-view-execution-test--activate "  ▸ Running")
    (niwa-view-execution-test--activate "  ▸ Running")
    (should (equal "e2" (plist-get niwa-view-execution--selected :execution-id)))
    (should (= 1 (niwa-view-test--count-substring "Preview (not full output)" (buffer-string))))))

(niwa-deftest niwa-view-execution--header ()
  ,test
  (test)
  :doc "rule counts commands and monitors and ends with the collapse toggle"
  (niwa-view-execution-test--with-view
    (let* ((header (niwa-view-execution--header
                    session nil '((:kind bash) (:kind monitor) (:kind monitor)) nil))
           (toggle (string-search "[-]" header)))
      (should (string-prefix-p " ─── background work · 1 command · 2 monitors [-] ─"
                               header))
      (should (eq 'niwa-view-zone-separator (get-text-property 1 'font-lock-face header)))
      (should (eq 'link (get-text-property toggle 'font-lock-face header)))
      (should (functionp (get-text-property toggle 'niwa-view-zone-activate header)))))
  :doc "blocked agents are counted first in a warning face"
  (niwa-view-execution-test--with-view
    (let* ((header (niwa-view-execution--header
                    session '((:status blocked) (:status running)) nil t))
           (blocked (string-search "1 blocked" header)))
      (should (string-prefix-p " ─── background work · 1 blocked · 2 agents [+] ─" header))
      (should (eq 'niwa-view-tool-warning
                  (get-text-property blocked 'font-lock-face header)))))
  :doc "a clipped narrow rule keeps the toggle reachable"
  (niwa-view-execution-test--with-view
    (cl-letf (((symbol-function 'niwa-view--zone-separator)
               (lambda (_) " ─── background w… ─\n")))
      (let ((header (niwa-view-execution--header session nil nil nil)))
        (should (equal " [-] background w… ─\n" (substring-no-properties header)))
        (should (get-text-property 1 'niwa-view-zone-activate header))))))

(niwa-deftest niwa-view-execution--mouse-press ()
  ,test
  (test)
  :doc "first focus click only selects; selected-window clicks toggle the row or header"
  (dolist (same-buffer '(nil t))
    (dolist (target '(row header))
      (niwa-view-execution-test--with-view
        (save-window-excursion
          (switch-to-buffer view-buf)
          (let* ((view-window (selected-window))
                 (other-window (split-window-right))
                 (mouse-drag-copy-region nil)
                 commands
                 (pre-command-hook
                  (cons (lambda () (push this-command commands)) pre-command-hook)))
            (set-window-buffer other-window (if same-buffer view-buf data-buf))
            (niwa-view--render-status)
            (niwa-view-execution-test--at (if (eq target 'row) "  ▸ Running" "[-]"))
            (let* ((position (point))
                   ;; Command lookup uses text-pos (slot 5), not just point.
                   (location (list view-window position '(10 . 10) 0 nil position))
                   (click (vector (list 'down-mouse-1 location)
                                  (list 'mouse-1 location))))
              (select-window other-window)
              (execute-kbd-macro click)
              (should (eq (selected-window) view-window))
              (should (= (point) position))
              (should-not niwa-view-execution--selected)
              (should-not (niwa-view-zone-collapse-state
                           niwa-view-execution--collapse-key))
              (execute-kbd-macro click)
              (ert-info ((format "same-buffer=%S target=%S commands=%S"
                                 same-buffer target (reverse commands)))
                (if (eq target 'row)
                    (should niwa-view-execution--selected)
                  (should (niwa-view-zone-collapse-state
                           niwa-view-execution--collapse-key))))))))))
  :doc "a focus click on Stop cannot stop work; the subsequent selected-window click can"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (let ((view-window (selected-window))
            (other-window (split-window-right))
            (mouse-drag-copy-region nil)
            stopped)
        (set-window-buffer other-window data-buf)
        (niwa-view--render-status)
        (niwa-view-execution-test--activate "  ▸ Running")
        (niwa-view-execution-test--at "[Stop]")
        (let* ((position (point))
               (location (list view-window position '(10 . 10) 0 nil position))
               (click (vector (list 'down-mouse-1 location) (list 'mouse-1 location))))
          (cl-letf (((symbol-function 'niwa-execution-stop-user)
                     (lambda (owner id) (push (list owner id) stopped))))
            (select-window other-window)
            (execute-kbd-macro click)
            (should-not stopped)
            (execute-kbd-macro click)
            (should (equal (list (list session "e1")) stopped)))))))
  :doc "selected-window dragging selects text without toggling and leaves composer bindings alone"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (let ((mouse-drag-copy-region nil)
            (mouse-drag-and-drop-region nil))
        (niwa-view--render-status)
        (niwa-view-execution-test--at "  ▸ Running")
        (let* ((start (point))
               (end (+ start 12))
               (from (list (selected-window) start '(10 . 10) 0 nil start))
               (to (list (selected-window) end '(100 . 10) 0 nil end)))
          (execute-kbd-macro (vector (list 'down-mouse-1 from)
                                     (list 'drag-mouse-1 from to)))
          (should-not niwa-view-execution--selected)
          (should (= start (region-beginning)))
          (should (= end (region-end))))
        (goto-char (niwa-view-composer-draft-start))
        (should (eq (key-binding [down-mouse-1]) 'mouse-drag-region))))))

(niwa-deftest niwa-view-execution--frame-focused-p ()
  ,test
  (test)
  :doc "only visible focused frames hold rows; terminals tolerate unknown focus"
  (dolist (graphical '(nil t))
    (dolist (focus '(nil t unknown))
      (dolist (visible '(nil t icon))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) graphical))
                  ((symbol-function 'frame-focus-state) (lambda (&optional _) focus))
                  ((symbol-function 'frame-visible-p) (lambda (&optional _) visible)))
          (should (eq (and (eq visible t)
                           (or (eq focus t) (and (not graphical) (eq focus 'unknown))))
                      (niwa-view-execution--frame-focused-p (selected-frame))))))))
  :doc "child frames use their top-level focus, not the selected frame"
  (cl-letf (((symbol-function 'frame-live-p) (lambda (frame) (memq frame '(child top))))
            ((symbol-function 'frame-parent) (lambda (frame) (and (eq frame 'child) 'top)))
            ((symbol-function 'frame-visible-p) (lambda (_) t))
            ((symbol-function 'display-graphic-p) (lambda (_) t))
            ((symbol-function 'frame-focus-state)
             (lambda (frame) (should (eq frame 'top)) t)))
    (should (niwa-view-execution--frame-focused-p 'child))
    (should-not (niwa-view-execution--frame-focused-p 'dead))))

(niwa-deftest niwa-view-execution--pointer-on-panel-p ()
  ,test
  (test)
  :doc "a pointer above or left of the frame is off the panel instead of an error; point still counts"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-view--render-status)
      (goto-char (plist-get (niwa-view-zone-fragment-bounds 'status 'background) :end))
      (dolist (coordinates '((0 . -27) (-4 . 3) (-1 . -1)))
        (cl-letf (((symbol-function 'mouse-pixel-position)
                   (lambda () (cons (selected-frame) coordinates))))
          (should-not (niwa-view-execution--pointer-on-panel-p))))
      (niwa-view-execution-test--at "  ▸ Running")
      (cl-letf (((symbol-function 'mouse-pixel-position)
                 (lambda () (cons (selected-frame) '(0 . -27)))))
        (should (niwa-view-execution--pointer-on-panel-p)))))
  :doc "mouse lease expires without movement; point holds only while its frame has focus"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-view--render-status)
      (goto-char (niwa-view-transcript-tail))
      (let ((clock 1000.0) (focus t) (inside t) (window (selected-window)))
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock))
                  ((symbol-function 'frame-focus-state) (lambda (&optional _) focus))
                  ((symbol-function 'mouse-pixel-position)
                   (lambda () (cons (selected-frame) (if inside '(1 . 1) '(-1 . -1)))))
                  ((symbol-function 'posn-at-x-y)
                   (lambda (&rest _)
                     (list window
                           (1+ (plist-get (niwa-view-zone-fragment-bounds
                                           'status 'background) :start))
                           '(1 . 1) 0))))
          (should (niwa-view-execution--pointer-on-panel-p))
          (setq clock 1059.0)
          (should (niwa-view-execution--pointer-on-panel-p))
          (setq clock 1060.0)
          (should-not (niwa-view-execution--pointer-on-panel-p))
          ;; Polling cannot reacquire an expired lease without leaving.
          (setq clock 1500.0)
          (should-not (niwa-view-execution--pointer-on-panel-p))
          (niwa-view-execution-test--at "  ▸ Running")
          (should (niwa-view-execution--pointer-on-panel-p))
          (setq focus nil)
          (should-not (niwa-view-execution--pointer-on-panel-p))
          (goto-char (niwa-view-transcript-tail))
          (setq focus t)
          (should (niwa-view-execution--pointer-on-panel-p))
          (setq clock 1560.0)
          (should-not (niwa-view-execution--pointer-on-panel-p))
          (setq inside nil)
          (should-not (niwa-view-execution--pointer-on-panel-p))
          (setq inside t)
          (should (niwa-view-execution--pointer-on-panel-p))))))
  :doc "lease expiry or frame focus loss releases completed rows through the normal grace timer"
  (dolist (release '(expiry focus-loss))
    (niwa-view-execution-test--with-view
      (save-window-excursion
        (switch-to-buffer view-buf)
        (let* ((clock 1000.0) (focus t) (window (selected-window))
               (draft-offset nil)
               (agents (list (list :path "/root/worker" :status 'running :depth 0)))
               (float-time-orig (symbol-function 'float-time)))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional time) (if time (funcall float-time-orig time) clock)))
                    ((symbol-function 'frame-focus-state) (lambda (&optional _) focus))
                    ((symbol-function 'niwa-view--agent-status-collect) (lambda () agents))
                    ((symbol-function 'mouse-pixel-position)
                     (lambda () (cons (selected-frame) '(1 . 1))))
                    ((symbol-function 'posn-at-x-y)
                     (lambda (&rest _)
                       (list window
                             (1+ (plist-get (niwa-view-zone-fragment-bounds
                                             'status 'background) :start))
                             '(1 . 1) 0))))
            (unwind-protect
                (progn
                  (niwa-view--render-status)
                  (niwa-view-test--insert-composer-draft "> draft\nsecond line")
                  (setq draft-offset (niwa-view-execution-test--draft-offset))
                  ;; Observe entry before completion, not a fresh lease when
                  ;; completed rows change the panel's geometry.
                  (niwa-view--render-status)
                  (setq clock 1059.0 live nil agents nil)
                  (niwa-view--render-status)
                  (should (string-search "✓ Completed bash" (buffer-string)))
                  (should (string-search "✓ Finished  agent" (buffer-string)))
                  (should (timerp niwa-view-execution--release-timer))
                  (let ((tick (buffer-chars-modified-tick)))
                    ;; Unchanged release polls do not mutate the view.
                    (dotimes (_ 3) (niwa-view-execution--check-release view-buf))
                    (should (= tick (buffer-chars-modified-tick))))
                  (setq clock (if (eq release 'expiry) 1060.0 1059.5))
                  (when (eq release 'focus-loss) (setq focus nil))
                  (niwa-view-execution--check-release view-buf)
                  (should-not niwa-view-execution--hovering)
                  (setq clock (+ clock 9.0))
                  (niwa-view-execution--check-release view-buf)
                  (should (string-search "✓ Completed bash" (buffer-string)))
                  (setq clock (+ clock 1.0))
                  (niwa-view-execution--check-release view-buf)
                  (should-not (string-search "✓ Completed bash" (buffer-string)))
                  (should-not (string-search "✓ Finished  agent" (buffer-string)))
                  (should-not niwa-view-execution--held)
                  (should-not niwa-view-execution--release-timer)
                  (should (= draft-offset (niwa-view-execution-test--draft-offset)))
                  (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
                  ;; Disappearance ends the observed visit, even with no
                  ;; subsequent timer available to observe mouse exit.
                  (should-not niwa-view-execution--hover-deadline)
                  (setq clock 2000.0 focus t
                        live (list '(:execution-id "new" :owner "/root" :yielded t
                                     :state running :command "new work")))
                  (niwa-view--render-status)
                  (niwa-view--render-status)
                  (should niwa-view-execution--hovering)
                  (should (= 2060.0 niwa-view-execution--hover-deadline)))
              (niwa-view-execution--cancel-release))))))))

(niwa-deftest niwa-view-execution--retain-selected ()
  ,test
  (test)
  :doc "completed inspectors start a fresh grace period on collapse, blocked until pointers leave"
  (dolist (hover-at-collapse '(nil t))
    (niwa-view-execution-test--with-view
      (let* ((clock 1000.0)
             (hover hover-at-collapse)
             (float-time-orig (symbol-function 'float-time)))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time) (if time (funcall float-time-orig time) clock)))
                  ((symbol-function 'niwa-view-execution--pointer-on-panel-p)
                   (lambda () hover)))
          (unwind-protect
              (progn
                (niwa-view--render-status)
                (niwa-view-test--insert-composer-draft "> draft\nsecond line")
                (niwa-view-execution-test--activate "  ▸ Running")
                (setq live nil)
                (niwa-view-execution--event
                 (list :session session :data-buffer data-buf :owner "/root" :type 'terminal
                       :facts '(:execution-id "e1" :state completed :termination exited :exit-code 0)
                       :whole-output "retained output"))
                (setq clock 1200.0)
                (niwa-view-execution--check-release view-buf)
                (should (string-search "retained output" (buffer-string)))
                (should-not niwa-view-execution--release-timer)
                (niwa-view-execution-test--at "  ▾ Completed")
                (call-interactively (key-binding (kbd "TAB")))
                (should-not niwa-view-execution--selected)
                (should (string-search "✓ Completed bash" (buffer-string)))
                (should (timerp niwa-view-execution--release-timer))
                (let ((held (nth 2 (car niwa-view-execution--held))))
                  (should (= 1210.0 (plist-get held :release-at)))
                  (should-not (plist-get held :whole-output))
                  (should-not (plist-get held :output-tail)))
                (when hover-at-collapse
                  (setq clock 2000.0)
                  (niwa-view-execution--check-release view-buf)
                  (should (string-search "✓ Completed bash" (buffer-string)))
                  (setq hover nil)
                  (niwa-view-execution--check-release view-buf)
                  (should (= 2010.0 (plist-get (nth 2 (car niwa-view-execution--held))
                                              :release-at))))
                (setq clock (if hover-at-collapse 2009.0 1209.0))
                (niwa-view-execution--check-release view-buf)
                (should (string-search "✓ Completed bash" (buffer-string)))
                (setq clock (+ clock 1.5))
                (niwa-view-execution--check-release view-buf)
                (should-not (string-search "✓ Completed bash" (buffer-string)))
                (should-not niwa-view-execution--release-timer)
                (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
            (niwa-view-execution--cancel-release))))))
  :doc "a completed inspector beyond five newer rows stays visible until its grace expires"
  (niwa-view-execution-test--with-view
    (let* ((clock 1000.0)
           (hover t)
           (float-time-orig (symbol-function 'float-time)))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time) (if time (funcall float-time-orig time) clock)))
                ((symbol-function 'niwa-view-execution--pointer-on-panel-p)
                 (lambda () hover)))
        (unwind-protect
            (progn
              (setq niwa-view-execution--selected
                    (list :session session :execution-id "old" :owner "/root"
                          :state 'completed :terminal-p t :termination 'exited :exit-code 0
                          :description "Old inspector" :started-at 1.0 :whole-output "old output")
                    live (cl-loop for n from 2 to 6
                                  collect (list :execution-id (format "e%d" n)
                                                :owner "/root" :yielded t :state 'running
                                                :started-at (float n) :description (format "New %d" n))))
              (niwa-view--render-status)
              (niwa-view-execution-test--at "  ▾ Completed")
              (call-interactively (key-binding (kbd "TAB")))
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (setq clock 1001.0 hover nil)
              (niwa-view-execution--check-release view-buf)
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (setq clock 1010.0)
              (niwa-view-execution--check-release view-buf)
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (setq clock 1011.5)
              (niwa-view-execution--check-release view-buf)
              (should-not (string-search "Old inspector" (buffer-string)))
              (should (= 5 (niwa-view-test--count-substring "▸ Running" (buffer-string)))))
          (niwa-view-execution--cancel-release)))))
  :doc "pinning an older live inspector does not evict a held completed row"
  (niwa-view-execution-test--with-view
    (let* ((clock 1000.0)
           (hover t)
           (float-time-orig (symbol-function 'float-time)))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time) (if time (funcall float-time-orig time) clock)))
                ((symbol-function 'niwa-view-execution--pointer-on-panel-p)
                 (lambda () hover)))
        (unwind-protect
            (progn
              (setq niwa-view-execution--selected
                    (list :session session :execution-id "old" :owner "/root"
                          :state 'completed :terminal-p t :description "Old inspector"
                          :started-at 1.0)
                    live (cl-loop for n from 2 to 8
                                  collect (list :execution-id (format "e%d" n)
                                                :owner "/root" :yielded t :state 'running
                                                :started-at (float n) :description (format "New %d" n))))
              (niwa-view--render-status)
              (niwa-view-execution-test--activate "  ▾ Completed")
              (niwa-view-execution-test--activate "[Show all 8]")
              (niwa-view-execution-test--activate "New 2")
              (niwa-view-execution-test--activate "[Show fewer]")
              (setq clock 1001.0 hover nil)
              (niwa-view-execution--check-release view-buf)
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (should (string-search "▾ Running   bash     New 2" (buffer-string)))
              (should (= 4 (niwa-view-test--count-substring "▸ Running" (buffer-string)))))
          (niwa-view-execution--cancel-release)))))
  :doc "selecting another execution or agent also retains a completed inspector summary"
  (dolist (kind '(execution agent))
    (niwa-view-execution-test--with-view
      (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                 (lambda () '((:path "/root/new" :status running :depth 0)))))
        (unwind-protect
            (progn
              (setq niwa-view-execution--selected
                    (list :session session :execution-id "old" :owner "/root"
                          :state 'completed :terminal-p t :description "Old inspector"
                          :whole-output "discarded output"))
              (niwa-view--render-status)
              (niwa-view-execution-test--activate
               (if (eq kind 'agent) "  ▸ Running   agent" "  ▸ Running   bash"))
              (should (string-search "✓ Completed bash     Old inspector" (buffer-string)))
              (should-not (plist-get (nth 2 (car niwa-view-execution--held)) :whole-output)))
          (niwa-view-execution--cancel-release))))))

(niwa-deftest niwa-view-execution-toggle-panel ()
  ,test
  (test)
  :doc "the header toggle hides and restores rows, preserving an active draft"
  (niwa-view-execution-test--with-view
    (niwa-view--render-status)
    (niwa-view-test--insert-composer-draft "> draft\nsecond line")
    (niwa-view-execution-test--activate "[-]")
    (should-not (string-search "▸ Running" (buffer-string)))
    (should (string-search "background work · 1 command [+]" (buffer-string)))
    (niwa-view-execution-test--activate "[+]")
    (should (string-search "▸ Running" (buffer-string)))
    (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
  :doc "TAB on the header, not the work rows, toggles the complete panel"
  (niwa-view-execution-test--with-view
    (niwa-view--render-status)
    (dolist (collapsed '(t nil))
      (niwa-view-execution-test--at " ─── background work")
      (call-interactively (key-binding (kbd "TAB")))
      (should (eq collapsed (niwa-view-zone-collapse-state
                             niwa-view-execution--collapse-key)))
      (should (eq (not collapsed)
                  (and (string-search "  ▸ Running" (buffer-string)) t))))
    (should (string-search "  ▸ Running" (buffer-string)))
    (should-not (niwa-view-zone-collapse-state
                 niwa-view-execution--collapse-key))))

(niwa-deftest niwa-view-execution-reset ()
  ,test
  (test)
  :doc "clears expansion, overflow, collapse and held rows, stopping the release timer"
  (niwa-view-execution-test--with-view
    (setq niwa-view-execution--selected-agent "/root/a"
          niwa-view-execution--show-all t
          niwa-view-execution--reflow-pending t
          niwa-view-execution--hover-deadline 99999.0
          niwa-view-execution--held (list (list '(agent "/root/a") 'agent '(:path "/root/a")))
          niwa-view-execution--release-timer (run-with-timer 3600 nil #'ignore))
    (niwa-view-zone-set-collapse-state niwa-view-execution--collapse-key t)
    (niwa-view-execution-reset)
    (should-not niwa-view-execution--selected-agent)
    (should-not niwa-view-execution--show-all)
    (should-not niwa-view-execution--reflow-pending)
    (should-not niwa-view-execution--held)
    (should-not niwa-view-execution--hover-deadline)
    (should-not niwa-view-execution--release-timer)
    (should-not (niwa-view-zone-collapse-state niwa-view-execution--collapse-key))
    (should (string-search "▸ Running" (buffer-string)))))

(niwa-deftest niwa-view-execution--ordered ()
  ,test
  (test)
  :doc "newest unit first; an agent tree is one unit ordered by its top-level agent"
  (let ((entry (lambda (kind data) (list (niwa-view-execution--key kind data) kind data))))
    (should (equal '((agent "/root/new") (execution "/root" "e") (agent "/root/old")
                     (agent "/root/old/kid"))
                   (mapcar #'car
                           (niwa-view-execution--ordered
                            (list (funcall entry 'agent '(:path "/root/old/kid" :started-at 90.0))
                                  (funcall entry 'agent '(:path "/root/old" :started-at 1.0))
                                  (funcall entry 'execution '(:owner "/root" :execution-id "e"
                                                              :started-at 5.0))
                                  (funcall entry 'agent '(:path "/root/new" :started-at 9.0))))))))
  :doc "equal unit starts sort by kind and identity regardless of input order"
  (let* ((entry (lambda (kind data) (list (niwa-view-execution--key kind data) kind data)))
         (entries (list (funcall entry 'execution '(:owner "/root/b" :execution-id "a"))
                        (funcall entry 'agent '(:path "/root/b" :started-at 0.0))
                        (funcall entry 'execution '(:owner "/root/a" :execution-id "b" :started-at 0))
                        (funcall entry 'agent '(:path "/root/a/z" :started-at 90.0))
                        (funcall entry 'agent '(:path "/root/a" :started-at (0 0 0 0)))
                        (funcall entry 'execution '(:owner "/root/a" :execution-id "a" :started-at 0.0))))
         (expected '((agent "/root/a") (agent "/root/a/z") (agent "/root/b")
                     (execution "/root/a" "a") (execution "/root/a" "b")
                     (execution "/root/b" "a")))
         (starts (make-hash-table :test #'equal)))
    (dotimes (offset (length entries))
      (let ((rotated (append (nthcdr offset entries) (seq-take entries offset))))
        (dolist (input (list rotated (reverse rotated)))
          (should (equal expected (mapcar #'car (niwa-view-execution--ordered input starts)))))))
    ;; Losing the root row must not change a tied tree's unit identity.
    (should (equal (cdr expected)
                   (mapcar #'car
                           (niwa-view-execution--ordered
                            (cl-remove '(agent "/root/a") entries :key #'car :test #'equal)
                            starts)))))
  :doc "an agent tree keeps its place after its top-level agent finishes"
  (let ((entry (lambda (kind data) (list (niwa-view-execution--key kind data) kind data)))
        (starts (make-hash-table :test #'equal)))
    (niwa-view-execution--ordered
     (list (funcall entry 'agent '(:path "/root/old" :started-at 1.0))
           (funcall entry 'agent '(:path "/root/old/kid" :started-at 90.0))
           (funcall entry 'agent '(:path "/root/new" :started-at 9.0)))
     starts)
    (should (equal '((agent "/root/new") (agent "/root/old/kid"))
                   (mapcar #'car
                           (niwa-view-execution--ordered
                            (list (funcall entry 'agent '(:path "/root/old/kid" :started-at 90.0))
                                  (funcall entry 'agent '(:path "/root/new" :started-at 9.0)))
                            starts))))
    ;; Units that are gone are forgotten.
    (niwa-view-execution--ordered
     (list (funcall entry 'agent '(:path "/root/new" :started-at 9.0))) starts)
    (should (equal '("/root/new") (hash-table-keys starts)))))

(niwa-deftest niwa-view-execution--check-release ()
  ,test
  (test)
  :doc "a dead view's timer cancels only itself, never another view's"
  (let* ((dead (generate-new-buffer " *dead-view*"))
         (own (run-with-timer 3600 3600 #'ignore))
         (other (run-with-timer 3600 3600 #'niwa-view-execution--check-release
                                (current-buffer))))
    (unwind-protect
        (progn
          (kill-buffer dead)
          (niwa-view-execution--check-release dead own)
          (should-not (memq own timer-list))
          (should (memq other timer-list)))
      (cancel-timer own)
      (cancel-timer other))))

(niwa-deftest niwa-view-execution--state-face ()
  (dolist (case '(((:state running) . niwa-view-agent-running)
                  ((:state running :kind monitor) . niwa-view-agent-running)
                  ((:state stopping) . niwa-view-tool-warning)
                  ((:state lost) . error)
                  ((:state failed) . error)
                  ((:state completed :outcome success) . success)
                  ((:state completed :outcome no-match :exit-code 1) . success)
                  ((:state completed :outcome different :exit-code 1) . success)
                  ((:state completed :outcome false :exit-code 1) . success)
                  ((:state completed :outcome failure) . error)
                  ((:state completed :outcome failure :termination stopped)
                   . niwa-view-tool-warning)
                  ((:state completed :outcome failure :termination owner-stopped)
                   . niwa-view-tool-warning)
                  ((:state completed :outcome failure :termination interrupted)
                   . niwa-view-tool-warning)
                  ((:state completed :termination exited :exit-code 0) . success)
                  ((:state completed :termination exited :exit-code 7) . error)
                  ((:terminal-p t :termination exited :exit-code 0) . success)
                  ((:terminal-p t :termination exited :exit-code 7) . error)
                  ((:state completed :termination exited) . niwa-view-tool-metadata)
                  ((:state completed) . niwa-view-tool-metadata)
                  ((:terminal-p t) . niwa-view-tool-metadata)
                  ((:terminal-p t :outcome unknown) . niwa-view-tool-metadata)
                  ((:terminal-p t :termination user-stop) . niwa-view-tool-warning)
                  ((:unavailable-p t) . niwa-view-tool-metadata)))
    (ert-info ((format "snapshot: %S" (car case)))
      (should (eq (cdr case) (niwa-view-execution--state-face (car case)))))))

(niwa-deftest niwa-view-execution--hold-terminal-facts ()
  ,test
  (test)
  :doc "both terminal event orders render bounded command outcomes, leaving unknown gaps dim"
  (dolist (order '(terminal-first removal-first))
    (dolist (case '(((:state completed :outcome success :termination exited :exit-code 0)
                    . success)
                   ((:state completed :outcome no-match :termination exited :exit-code 1)
                    . success)
                   ((:state completed :outcome failure :termination exited :exit-code 7)
                    . error)
                   ((:state completed :outcome failure :termination stopped)
                    . niwa-view-tool-warning)
                   ((:state completed) . niwa-view-tool-metadata)))
      (ert-info ((format "event order: %S facts: %S" order (car case)))
        (niwa-view-execution-test--with-view
          (niwa-view--render-status)
          (niwa-view-test--insert-composer-draft "> draft\nsecond line")
          (let ((event (list :session session :data-buffer data-buf :owner "/root"
                             :type 'terminal :facts (append '(:execution-id "e1") (car case)))))
            (unwind-protect
                (progn
                  (when (eq order 'terminal-first)
                    (niwa-view-execution--event event)
                    (let ((shown (nth 2 (car niwa-view-execution--shown))))
                      (should (plist-get shown :terminal-p))
                      (dolist (key '(:state :outcome :termination :exit-code))
                        (should (equal (plist-get shown key) (plist-get (car case) key))))))
                  (setq live nil)
                  (niwa-view--render-status)
                  (when (eq order 'removal-first)
                    (niwa-view-execution-test--at "✓ Completed")
                    (dolist (property '(face font-lock-face))
                      (should (eq 'niwa-view-tool-metadata
                                  (get-text-property (point) property)))
                      (should (eq 'niwa-view-tool-metadata
                                  (get-text-property (+ 2 (point)) property))))
                    (niwa-view-execution--event event))
                  (let ((held (nth 2 (car niwa-view-execution--held))))
                    (should (plist-get held :terminal-p))
                    (dolist (key '(:state :outcome :termination :exit-code))
                      (should (equal (plist-get held key) (plist-get (car case) key)))))
                  (dotimes (_ 2)
                    (niwa-view-execution-test--assert-held-row
                     "✓ Completed" (cdr case) '("bash" "same command"))
                    (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
                    (niwa-view--render-status)))
              (niwa-view-execution--cancel-release))))))))

(niwa-deftest niwa-view-execution--held-line ()
  ,test
  (test)
  :doc "held agents snapshot settled record outcomes and keep only glyph/state colored"
  (dolist (case '((completed . success) (errored . error)
                  (interrupted . niwa-view-tool-warning)
                  (nil . niwa-view-tool-metadata)))
    (ert-info ((format "settled agent outcome: %S" (car case)))
      (niwa-view-execution-test--with-view
        (let* ((agents '((:path "/root/worker" :status running :depth 0)))
               (record (niwa-agent-record--create :id "worker" :path "/root/worker"
                                                     :parent-path "/root" :activity 'running)))
          (setf (niwa-session-agent-registry session) (list (cons "/root/worker" record)))
          (cl-letf (((symbol-function 'niwa-view--agent-status-collect) (lambda () agents)))
            (niwa-view--render-status)
            (niwa-view-test--insert-composer-draft "> draft\nsecond line")
            (setf (niwa-agent-record-activity record) 'idle
                  (niwa-agent-record-settled-outcome record) (car case))
            (setq agents nil)
            (unwind-protect
                (progn
                  (niwa-view--render-status)
                  (should (eq (car case)
                              (plist-get (nth 2 (car niwa-view-execution--held)) :outcome)))
                  ;; A follow-up mutates the retained record, not the old row.
                  (setf (niwa-agent-record-settled-outcome record)
                        (if (eq (car case) 'completed) 'errored 'completed))
                  (dotimes (_ 2)
                    (niwa-view-execution-test--assert-held-row
                     "✓ Finished" (cdr case) '("agent" "/root/worker"))
                    (niwa-view--render-status))
                  (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
              (niwa-view-execution--cancel-release))))))))

(niwa-deftest niwa-view-execution--line-presentation ()
  ,test
  (test)
  :doc "state carries its outcome face and a single Monitor event is singular"
  (niwa-view-execution-test--with-view
    (let ((line (niwa-view-execution--line
                 '(:owner "/root" :execution-id "m1" :kind monitor :state running
                   :description "watch" :event-count 1))))
      (should (equal "  ▸ Watching  monitor  watch · 1 event\n" (substring-no-properties line)))
      (should (eq 'niwa-view-agent-running
                  (get-text-property (string-search "Watching" line) 'font-lock-face line)))
      (should (eq 'niwa-view-tool-metadata
                  (get-text-property (string-search "▸" line) 'font-lock-face line)))))
  :doc "a Bash description replaces the command; the command is the fallback"
  (niwa-view-execution-test--with-view
    (should (equal "  ▸ Running   bash     Run tests\n"
                   (substring-no-properties
                    (niwa-view-execution--line
                     '(:owner "/root" :execution-id "e1" :state running
                       :command "make test" :description "Run tests")))))
    (should (equal "  ▸ Running   bash     make test\n"
                   (substring-no-properties
                    (niwa-view-execution--line
                     '(:owner "/root" :execution-id "e1" :state running
                       :command "make test"))))))
  :doc "retained expanded executions use the same rendered outcome faces as held summaries"
  (dolist (case '(((:state completed :outcome success :termination exited :exit-code 0)
                  . success)
                 ((:state completed :outcome failure :termination exited :exit-code 9)
                  . error)
                 ((:state completed :outcome failure :termination stopped)
                  . niwa-view-tool-warning)
                 ((:state completed) . niwa-view-tool-metadata)))
    (niwa-view-execution-test--with-view
      (niwa-view--render-status)
      (niwa-view-execution-test--activate "  ▸ Running")
      (niwa-view-test--insert-composer-draft "> draft\nsecond line")
      (setq live nil)
      (niwa-view-execution--event
       (list :session session :data-buffer data-buf :owner "/root" :type 'terminal
             :facts (append '(:execution-id "e1") (car case)) :whole-output "final output"))
      (dotimes (_ 2)
        (niwa-view-execution-test--at "  ▾ Completed")
        (search-forward "Completed")
        (dolist (property '(face font-lock-face))
          (should (eq (cdr case) (get-text-property (1- (point)) property))))
        (should (string-search "final output" (buffer-string)))
        (should-not (string-search "[Stop]" (buffer-string)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
        (niwa-view--render-status)))))

(niwa-deftest niwa-view-execution--agent-line ()
  ,test
  (test)
  :doc "nested agents indent; interaction-blocked agents say they wait for input"
  (niwa-view-execution-test--with-view
    (should (equal "    ▸ Blocked   agent    /root/a/b · waiting for input\n"
                   (substring-no-properties
                    (niwa-view-execution--agent-line
                     session '(:path "/root/a/b" :status blocked :depth 1
                               :activity interaction-blocked)))))
    (should (equal "  ▸ Waiting   agent    /root/a\n"
                   (substring-no-properties
                    (niwa-view-execution--agent-line
                     session '(:path "/root/a" :status waiting :depth 0)))))))

(niwa-deftest niwa-view-execution--details ()
  ,test
  (test)
  :doc "a narrow execution finishing under the mouse keeps its displayed control-row budget"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (split-window-right 47)
      (visual-line-mode 1)
      (cl-letf (((symbol-function 'niwa-view-execution--pointer-on-panel-p) (lambda () t)))
        (unwind-protect
            (progn
              (niwa-view--render-status)
              (niwa-view-test--insert-composer-draft "> draft\nsecond line")
              (niwa-view-execution-test--activate "  ▸ Running   bash")
              (let* ((bounds (niwa-view-zone-fragment-bounds 'status 'background))
                     (before (count-screen-lines (plist-get bounds :start)
                                                 (plist-get bounds :end) nil (selected-window))))
                (setq live nil)
                (niwa-view--render-status)
                (setq bounds (niwa-view-zone-fragment-bounds 'status 'background))
                (should (= before (count-screen-lines (plist-get bounds :start)
                                                      (plist-get bounds :end) nil (selected-window)))))
              (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
          (niwa-view-execution--cancel-release))))))

(niwa-deftest niwa-view-execution--control-lines ()
  ,test
  (test)
  :doc "actions fit narrow rows without losing callbacks; finished rows keep the live budget"
  (dolist (width '(10 20 30 47 80))
    (let* ((niwa-view-execution--layout-width width)
           (callback (lambda () t))
           (controls (list (list "[Open transcript]" "[View]" callback "Open transcript")
                           (list "[Show in conversation]" "[Call]" callback "Show call")
                           (list "[Interrupt]" "[Stop]" callback "Interrupt")))
           (live (niwa-view-execution--control-lines controls))
           (finished (niwa-view-execution--control-lines controls t)))
      (should (= (cl-count ?\n live) (cl-count ?\n finished)))
      (dolist (line (split-string live "\n" t))
        (should (< (string-width line) width)))
      (should (= 3 (cl-count-if
                    (lambda (char) (eq char ?\[)) (string-to-list live))))
      (let ((pos 0) (actions 0))
        (while (< pos (length live))
          (when (get-text-property pos 'niwa-view-zone-activate live)
            (cl-incf actions))
          (setq pos (next-single-property-change
                     pos 'niwa-view-zone-activate live (length live))))
        (should (= 3 actions)))
      (should-not (text-property-not-all 0 (length finished)
                                         'niwa-view-zone-activate nil finished)))))

(niwa-deftest niwa-view-execution--agent-details ()
  ,test
  (test)
  :doc "RET expands one agent row with its actions, collapsing any execution row"
  (niwa-view-execution-test--with-view
    (let (opened interrupted shown)
      (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                 (lambda ()
                   (list (list :path "/root/counter" :status 'running :depth 0
                               :parent-path "/root" :tool-use-id "agent-call"
                               :model-metadata '(:model "opus")))))
                ((symbol-function 'niwa-view-agent-handle-activate)
                 (lambda (&optional path) (setq opened path)))
                ((symbol-function 'niwa-agent-control-interrupt-user)
                 (lambda (owner path) (should (eq owner session)) (setq interrupted path)))
                ((symbol-function 'niwa-view-audit-show-result)
                 (lambda (record) (setq shown record))))
        (niwa-view--render-status)
        (niwa-view-execution-test--activate "  ▸ Running   bash")
        (should niwa-view-execution--selected)
        (niwa-view-execution-test--activate "  ▸ Running   agent")
        (should (equal "/root/counter" niwa-view-execution--selected-agent))
        (should-not niwa-view-execution--selected)
        (should (string-search "    Model     opus\n" (buffer-string)))
        (should-not (string-search "Preview (not full output)" (buffer-string)))
        (niwa-view-execution-test--activate "[Open transcript]")
        (should (equal "/root/counter" opened))
        (niwa-view-execution-test--activate "[Show in conversation]")
        (should (equal '(:owner "/root" :tool-use-id "agent-call") shown))
        (niwa-view-execution-test--activate "[Interrupt]")
        (should (equal "/root/counter" interrupted))
        (niwa-view-execution-test--activate "  ▾ Running   agent")
        (should-not niwa-view-execution--selected-agent))))
  :doc "TAB on agent rows or details toggles the agent without invoking Interrupt"
  (niwa-view-execution-test--with-view
    (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
               (lambda () '((:path "/root/counter" :status running :depth 0))))
              ((symbol-function 'niwa-agent-control-interrupt-user)
               (lambda (&rest _) (ert-fail "TAB interrupted an agent"))))
      (niwa-view--render-status)
      (niwa-view-execution-test--at "  ▸ Running   agent")
      (call-interactively (key-binding (kbd "TAB")))
      (should (equal "/root/counter" niwa-view-execution--selected-agent))
      (niwa-view-execution-test--at "[Interrupt]")
      (call-interactively (key-binding (kbd "TAB")))
      (should-not niwa-view-execution--selected-agent)
      (should (string-search "  ▸ Running   bash" (buffer-string)))
      (should-not (niwa-view-zone-collapse-state
                   niwa-view-execution--collapse-key))))
  :doc "collapsing from an agent's row, Model detail or actions returns point to that agent's row"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (setq live nil)
      (cl-letf (((symbol-function 'niwa-view--agent-status-collect)
                 (lambda ()
                   '((:path "/root/other" :status running :depth 0)
                     (:path "/root/target" :status running :depth 0
                      :model-metadata (:model "opus"))))))
        (niwa-view--render-status)
        (niwa-view-test--insert-composer-draft "> draft\nsecond line")
        (let ((row '(agent "/root/target")))
          ;; nil starts and stays on the surviving row: the control case.
          (dolist (target '(nil "    Model     opus" "[Interrupt]"))
            (ert-info ((format "collapse from %S" target))
              (goto-char (car (niwa-view-zone--anchor-run row (point-min) (point-max))))
              (call-interactively (key-binding (kbd "TAB")))
              (should (equal "/root/target" niwa-view-execution--selected-agent))
              (when target
                (niwa-view-execution-test--at target)
                (should-not (equal row (get-text-property
                                        (point) 'niwa-view-zone-anchor))))
              (call-interactively (key-binding (kbd "TAB")))
              (should-not niwa-view-execution--selected-agent)
              (should-not (niwa-view-zone-collapse-state
                           niwa-view-execution--collapse-key))
              (should (= (point) (car (niwa-view-zone--anchor-run
                                       row (point-min) (point-max)))))
              (should (= (point) (window-point (selected-window))))
              (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))))))))
  :doc "narrow hovered expansions keep displayed rows on agent and execution completion"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (split-window-right 47)
      (visual-line-mode 1)
      (let ((agents (list (list :path "/root/counter" :status 'running :depth 0
                                :tool-use-id "agent-call"
                                :model-metadata
                                '(:backend "Anthropic" :model "anthropic/claude-sonnet-4.5-20250929"
                                  :effort "high"))))
            (rows (lambda ()
                    (let ((bounds (niwa-view-zone-fragment-bounds 'status 'background)))
                      (count-screen-lines (plist-get bounds :start)
                                          (plist-get bounds :end) nil (selected-window))))))
        (cl-letf (((symbol-function 'niwa-view--agent-status-collect) (lambda () agents))
                  ((symbol-function 'niwa-view-execution--pointer-on-panel-p) (lambda () t)))
          (unwind-protect
              (progn
                (niwa-view--render-status)
                (niwa-view-test--insert-composer-draft "> draft\nsecond line")
                (niwa-view-execution-test--activate "  ▸ Running   agent")
                (let ((before (funcall rows)))
                  (setq agents nil)
                  (niwa-view--render-status)
                  (should (= before (funcall rows))))
                (niwa-view-execution-test--activate "  ▸ Running   bash")
                (let ((before (funcall rows)))
                  (setq live nil)
                  (niwa-view--render-status)
                  (should (= before (funcall rows))))
                (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
            (niwa-view-execution--cancel-release))))))
  :doc "a finished agent keeps an inert expansion of the same height while held, and its stale controls refuse"
  (niwa-view-execution-test--with-view
    (let* ((agents (list (list :path "/root/counter" :status 'running :depth 0
                               :model-metadata '(:backend "Claude" :model "opus" :effort "high"))))
           (clock 1000.0)
           (float-time-orig (symbol-function 'float-time))
           (lines (lambda () (count-lines (point-min) (niwa-view-transcript-tail)))))
      (cl-letf (((symbol-function 'niwa-view--agent-status-collect) (lambda () agents))
                ((symbol-function 'float-time)
                 (lambda (&optional time) (if time (funcall float-time-orig time) clock)))
                ((symbol-function 'niwa-agent-control-interrupt-user)
                 (lambda (&rest _) (ert-fail "interrupted a finished agent"))))
        (niwa-view--render-status)
        (niwa-view-test--insert-composer-draft "> draft\nsecond line")
        (niwa-view-execution-test--activate "  ▸ Running   agent")
        (niwa-view-execution-test--at "[Interrupt]")
        (let ((callback (get-text-property (point) 'niwa-view-zone-activate))
              (expanded (funcall lines)))
          (setq agents nil)
          (unwind-protect
              (progn
                (niwa-view--render-status)
                (should (equal "/root/counter" niwa-view-execution--selected-agent))
                (should (string-search "✓ Finished  agent    /root/counter" (buffer-string)))
                (should (string-search "    Model     opus\n" (buffer-string)))
                (should (string-search "    Agent finished.\n" (buffer-string)))
                (should-not (string-search "[Interrupt]" (buffer-string)))
                (should (= expanded (funcall lines)))
                (should-error (funcall callback) :type 'user-error)
                ;; Once the held row is released, the expansion goes with it.
                (setq clock (+ clock niwa-view-execution-grace-seconds 1))
                (niwa-view-execution--check-release view-buf)
                (niwa-view--render-status)
                (should-not niwa-view-execution--selected-agent)
                (should-not (string-search "Agent finished." (buffer-string)))
                (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
            (niwa-view-execution--cancel-release)))))))

(niwa-deftest niwa-view-execution--preview ()
  ,test
  (test)
  :doc "preview is three one-row lines below a header that carries the output-limit warning"
  (let* ((output (mapconcat #'identity (make-list 8 (make-string 500 ?x)) "\n"))
         (preview (niwa-view-execution--preview
                   (list :output-tail output :termination 'output-limit))))
    (should (string-search "output was truncated" (car (split-string preview "\n"))))
    (should (= 3 (length (cl-remove-if-not
                          (lambda (line) (string-prefix-p "    xxx" line))
                          (split-string preview "\n")))))
    (should (= 4 (cl-count ?\n preview)))
    (dolist (line (split-string preview "\n"))
      (should (< (string-width line) (niwa-view-zone-width)))))
  :doc "keeps one height from no output through one line to many"
  (dolist (output '("" "one" "one\ntwo\nthree\nfour" "x\ny"))
    (ert-info ((format "output %S" output))
      (should (= 4 (cl-count ?\n (niwa-view-execution--preview
                                  (list :output-tail output :output-bytes 0)))))))
  :doc "distinguishes empty live, empty terminal, missing and lost output"
  (dolist (case '(((:output-tail "" :output-bytes 0) . "No output yet.")
                  ((:terminal-p t :whole-output "" :output-bytes 0) . "No output.")
                  ((:terminal-p t :state lost) . "Output unavailable.")
                  ((:output-tail "" :output-bytes 10) . "Output unavailable.")))
    (should (string-search (cdr case) (niwa-view-execution--preview (car case))))))

(niwa-deftest niwa-view-execution-tick ()
  ,test
  (test)
  :doc "idle-root Bash, Monitor and agent rows share one metadata timer and preserve the draft"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (let ((clock 59.0)
            (original-float-time (symbol-function 'float-time))
            (niwa-view-spinner-animate nil)
            (niwa-view-execution--layout-width 65))
        (setf (plist-get (car live) :started-at) 0.0)
        (setf (plist-get (car live) :description)
              "Run the complete enormous regression suite across every supported environment")
        (setq live (append live
                           (list (list :execution-id "m1" :owner "/root" :yielded t
                                       :state 'running :kind 'monitor :started-at 0.0
                                       :description "Watch quiet server" :event-count 0))))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional time)
                     (if time (funcall original-float-time time) clock)))
                  ((symbol-function 'window-end) (lambda (&rest _) (point-max)))
                  ((symbol-function 'niwa-view--agent-status-collect)
                   (lambda () (list '(:path "/root/worker" :status running :started-at 0.0)))))
          (niwa-view--render-status)
          (niwa-view-test--insert-composer-draft
           "> active draft\nsecond line" (1- (length "> active draft\nsecond line")))
          (let ((offset (niwa-view-execution-test--draft-offset))
                (timer niwa-view--spinner-timer))
            (should (timerp timer))
            (should (equal '((metadata . 1.0)) niwa-view--spinner-timer-plan))
            (should (= 3 (length niwa-view-execution--elapsed-targets)))
            (should (= 3 (niwa-view-test--count-substring " · 59s" (buffer-string))))
            (setq clock 60.0)
            (cl-letf (((symbol-function 'niwa-view--render-status)
                       (lambda (&rest _) (ert-fail "tick rebuilt status")))
                      ((symbol-function 'niwa-execution-list-user)
                       (lambda (_) (ert-fail "tick enumerated work")))
                      ((symbol-function 'niwa-view--full-rerender)
                       (lambda () (ert-fail "tick rebuilt transcript"))))
              (funcall (timer--function timer)))
            (should (= 3 (niwa-view-test--count-substring " · 1m 00s" (buffer-string))))
            (should (eq timer niwa-view--spinner-timer))
            (should (equal "> active draft\nsecond line" (niwa-view-composer-draft-text)))
            (should (= offset (niwa-view-execution-test--draft-offset)))
            (dolist (target niwa-view-execution--elapsed-targets)
              (should (<= (string-width (buffer-substring-no-properties
                                        (car target) (1- (cdr target)))) 64))
              (should (get-text-property (car target) 'niwa-view-zone-activate))
              (should (get-text-property (car target) 'niwa-view-zone-anchor))
              (should (get-text-property (car target) 'niwa-view-zone-region)))
            ;; The same second writes nothing, even if decorative frames tick.
            (let ((before (buffer-modified-tick)))
              (niwa-view--spinner-tick)
              (should (= before (buffer-modified-tick))))
            ;; Stopping foreground activity must not stop independent work.
            (niwa-view--stop-spinner)
            (should (timerp niwa-view--spinner-timer))
            (niwa-view-stream--release-turn)
            (should (timerp niwa-view--spinner-timer))
            ;; Background wall time advances independently of a paused root.
            (cl-letf (((symbol-function 'niwa-view--spinner-request)
                       (lambda () (niwa-request--create
                                   :active-work-pause-started-at 60.0))))
              (dolist (sample '((3599.0 . "59m 59s") (3600.0 . "1h 00m")))
                (setq clock (car sample))
                (niwa-view--spinner-tick)
                (should (= 3 (niwa-view-test--count-substring
                              (concat " · " (cdr sample)) (buffer-string))))
                (dolist (target niwa-view-execution--elapsed-targets)
                  (should (<= (string-width (buffer-substring-no-properties
                                            (car target) (1- (cdr target)))) 64))))
              (should (equal '((metadata . 1.0)) niwa-view--spinner-timer-plan)))
            ;; Completed status reconciliation still consumes the edited rows.
            (niwa-view--render-status)
            (should (= 3 (niwa-view-test--count-substring " · 1h 00m" (buffer-string)))))))))
  :doc "hidden, unfocused and collapsed rows do not keep metadata ticking; visibility rearms"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setf (plist-get (car live) :started-at) (- (float-time) 10))
      (cl-letf (((symbol-function 'window-end) (lambda (&rest _) (point-max))))
        (niwa-view--render-status)
        (should (niwa-view--background-metadata-visible-p))
        (cl-letf (((symbol-function 'window-end) (lambda (&rest _) (point-min))))
          (niwa-view--start-spinner-timer)
          (should-not niwa-view--spinner-timer)
          (let ((before (buffer-modified-tick)))
            (niwa-view--spinner-tick)
            (should (= before (buffer-modified-tick)))))
        (cl-letf (((symbol-function 'niwa-view--window-attended-p)
                   (lambda (_) nil)))
          (niwa-view--start-spinner-timer)
          (should-not niwa-view--spinner-timer))
        (niwa-view--resume-render-if-attended view-buf)
        (should (timerp niwa-view--spinner-timer))
        (niwa-view-zone-set-collapse-state niwa-view-execution--collapse-key t)
        (niwa-view--render-status)
        (should-not niwa-view-execution--elapsed-targets)
        (should-not niwa-view--spinner-timer)
        (cl-letf (((symbol-function 'niwa-view--start-spinner-timer)
                   (lambda (&rest _) (ert-fail "idle status replanned timer"))))
          (niwa-view--render-status)))))
  :doc "root response settlement keeps quiet Monitor-only and agent-only elapsed updates alive"
  (dolist (kind '(bash monitor agent))
    (niwa-view-execution-test--with-view
      (save-window-excursion
        (switch-to-buffer view-buf)
        (let ((clock 9.0)
              (original-float-time (symbol-function 'float-time)))
          (setf (plist-get (car live) :started-at) 0.0
                (plist-get (car live) :kind) kind)
          (when (eq kind 'agent) (setq live nil))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional time)
                       (if time (funcall original-float-time time) clock)))
                    ((symbol-function 'window-end) (lambda (&rest _) (point-max)))
                    ((symbol-function 'niwa-view--agent-status-collect)
                     (lambda () (when (eq kind 'agent)
                                  (list '(:path "/root/worker" :status running :started-at 0.0))))))
            (niwa-view-test--insert-data data-buf "Root completed.\n" 'response)
            (niwa-view--render-status)
            (niwa-view-test--insert-composer-draft "> keep draft\nsecond line")
            (with-current-buffer data-buf
              (niwa-view-stream-render-response (point-min) (point-max)))
            (should (timerp niwa-view--spinner-timer))
            (should (string-search " · 9s" (buffer-string)))
            (setq clock 10.0)
            (funcall (timer--function niwa-view--spinner-timer))
            (should (string-search " · 10s" (buffer-string)))
            (should (equal "> keep draft\nsecond line" (niwa-view-composer-draft-text))))))))
  :doc "a finished agent freezes its invocation duration during the grace period"
  (niwa-view-execution-test--with-view
    (let ((clock 59.0)
          (original-float-time (symbol-function 'float-time))
          (agents (list '(:path "/root/worker" :status running :started-at 0.0))))
      (setq live nil)
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time)
                   (if time (funcall original-float-time time) clock)))
                ((symbol-function 'niwa-view--agent-status-collect) (lambda () agents)))
        (unwind-protect
            (progn
              (niwa-view--render-status)
              (setq agents nil)
              (niwa-view--render-status)
              (should (string-search "/root/worker · 59s" (buffer-string)))
              (setq clock 61.0)
              (niwa-view--render-status)
              (should (string-search "/root/worker · 59s" (buffer-string)))
              (should-not niwa-view-execution--elapsed-targets))
          (niwa-view-execution--cancel-release)))))
  :doc "terminal durations freeze in held and selected rows in both event orderings"
  (dolist (terminal-first '(nil t))
    (dolist (selected '(nil t))
      (niwa-view-execution-test--with-view
        (setf (plist-get (car live) :started-at) (- (float-time) 100))
        (niwa-view--render-status)
        (when selected (niwa-view-execution-test--activate "  ▸ Running"))
        (let ((event (list :session session :data-buffer data-buf :owner "/root" :type 'terminal
                           :facts '(:execution-id "e1" :state completed :termination exited
                                    :exit-code 0 :wall-time-seconds 99.0))))
          (setq live nil)
          (when terminal-first (niwa-view-execution--event event))
          (niwa-view--render-status)
          (unless terminal-first (niwa-view-execution--event event))
          (unwind-protect
              (progn
                (should (string-search "same command · 1m 39s" (buffer-string)))
                (should-not niwa-view-execution--elapsed-targets)
                (let ((before (buffer-string)))
                  (niwa-view-execution-tick)
                  (should (equal-including-properties before (buffer-string)))))
            (niwa-view-execution--cancel-release)))))))

(niwa-deftest niwa-view-execution--state-label ()
  (dolist (case '(((:state running) . "Running")
                  ((:state running :kind monitor) . "Watching")
                  ((:state stopping) . "Stopping")
                  ((:state lost) . "Lost")
                  ((:state completed :termination exited :exit-code 7) . "Completed (exit 7)")
                  ((:terminal-p t :termination user-stop) . "Completed (user-stop)")))
    (should (equal (cdr case) (niwa-view-execution--state-label (car case))))))

(niwa-deftest niwa-view-execution--event ()
  ,test
  (test)
  :doc "quiet progress retains new facts without status work; changed output renders normally"
  (niwa-view-execution-test--with-view
    (niwa-view--render-status)
    (niwa-view-execution-test--activate "  ▸ Running")
    (let* ((before (buffer-string))
           (facts (plist-put (copy-tree (car live)) :wall-time-seconds 99))
           (event (list :session session :data-buffer data-buf :owner "/root"
                        :type 'progress :facts facts :output-tail "")))
      (cl-letf (((symbol-function 'niwa-view--render-status)
                 (lambda (&rest _) (ert-fail "quiet progress rendered status")))
                ((symbol-function 'niwa-execution-count-user)
                 (lambda (_) (ert-fail "quiet progress counted executions")))
                ((symbol-function 'niwa-execution-list-user)
                 (lambda (_) (ert-fail "quiet progress listed executions"))))
        (niwa-view-execution--event event))
      (should (= 99 (plist-get niwa-view-execution--selected :wall-time-seconds)))
      (should (equal-including-properties before (buffer-string)))
      ;; The facade's public snapshot advances with the event in production.
      (setf (plist-get (car live) :output-tail) "changed output")
      (niwa-view-execution--event (plist-put event :output-tail "changed output"))
      (should (string-search "changed output" (buffer-string)))
      (should-not (string-search "No output yet." (buffer-string)))))
  :doc "progress updates only selected item; terminal survives zero-live gap and collapse"
  (niwa-view-execution-test--with-view
    (let ((event (list :session session :data-buffer data-buf :owner "/root"
                       :type 'progress :facts (copy-tree (car live)) :output-tail "new output")))
      (cl-letf (((symbol-function 'niwa-view--render-status) (lambda (&rest _) (ert-fail "unselected refresh"))))
        (niwa-view-execution--event event))
      (niwa-view--render-status)
      (niwa-view-execution-test--activate "  ▸ Running")
      (setf (plist-get (car live) :output-tail) "new output")
      (niwa-view-execution--event event)
      (should (string-search "new output" (buffer-string)))
      (setq live nil)
      (niwa-view-execution--state-changed session data-buf)
      (should niwa-view-execution--selected)
      (should-not (string-search "[Stop]" (buffer-string)))
      (setq event (plist-put event :type 'terminal)
            event (plist-put event :facts '(:execution-id "e1" :state completed :termination exited :exit-code 0))
            event (plist-put event :whole-output "final retained output"))
      (niwa-view-execution--event event)
      (should (string-search "Completed (exit 0)" (buffer-string)))
      (should (string-search "final retained output" (buffer-string)))
      (niwa-view-execution--event (plist-put (copy-tree event) :type 'progress))
      (should (string-search "final retained output" (buffer-string)))
      (niwa-view-execution-test--activate "  ▾ Completed")
      (should (string-search "✓ Completed bash" (buffer-string)))
      (should-not niwa-view-execution--selected)
      (should-not (string-search "final retained output" (buffer-string)))))
  :doc "nested agent owner routes passive completion to parent without scanning"
  (niwa-view-execution-test--with-view
    (let ((child (generate-new-buffer " *execution-child*")))
      (unwind-protect
          (progn
            (setf (plist-get (car live) :owner) "/root/parent/child")
            (with-current-buffer child
              (setq-local niwa--agent-invocation
                          (niwa-agent-invocation--create :parent-data-buffer data-buf)))
            (niwa-view--render-status)
            (niwa-view-execution-test--activate "  ▸ Running")
            (setq live nil)
            (cl-letf (((symbol-function 'buffer-list) (lambda (&rest _) (ert-fail "scan"))))
              (niwa-view-execution--event
               (list :session session :data-buffer child :owner "/root/parent/child"
                     :type 'terminal :whole-output "child result"
                     :facts '(:execution-id "e1" :state completed :termination exited :exit-code 0))))
            (should (string-search "child result" (buffer-string)))
            (should (string-search "/root/parent/child" (buffer-string))))
        (kill-buffer child))))
  :doc "real yielded execution events preserve an active draft without consuming output"
  (niwa-view-test--with-buffers
    (let* ((root (make-temp-file "niwa-view-execution-" t))
           (session (test-niwa-execution--session root))
           (niwa-execution-event-functions '(niwa-view-execution--event))
           (niwa-execution-state-change-hook '(niwa-view-execution--state-changed))
           (niwa-execution-mailbox-delivery-function (lambda (_event _context ack) (funcall ack t nil) t)))
      (unwind-protect
          (progn
            (with-current-buffer data-buf (setq-local niwa--session session))
            (with-current-buffer view-buf
              (let* ((observation
                      (test-niwa-execution--start-managed
                       session root
                       '("sh" "-c" "while [ ! -e go ]; do sleep 0.02; done; printf 'live output\\n'; while [ ! -e finish ]; do sleep 0.02; done; printf 'final output\\n'")
                       :owner "/root" :data-buffer data-buf :tool-use-id "actual-tool"))
                     (id (plist-get (plist-get observation :facts) :execution-id)))
                (ert-info ((format "Initial execution observation: %S" observation))
                  (should id))
                (niwa-view-execution-test--activate "  ▸ Running")
                (let ((text (propertize "> active draft\nsecond line" 'draft-test 'kept)))
                  (niwa-view-test--insert-composer-draft text (- (length text) 3)))
                (niwa-view-execution-test--set-draft-mark 4)
                (let ((draft (niwa-view-composer-draft-text))
                      (offset (niwa-view-execution-test--draft-offset))
                      (mark-offset (niwa-view-execution-test--draft-offset t)))
                  (write-region "go" nil (file-name-concat root "go") nil 'silent)
                  (test-niwa-execution--wait
                   (lambda () (string-search "    live output" (buffer-string))))
                  (let* ((state (niwa-session-execution-state session))
                         (record (gethash id (niwa-execution--state-records state))))
                    (should (= 0 (niwa-execution--record-read-offset record))))
                  (write-region "finish" nil (file-name-concat root "finish") nil 'silent)
                  (test-niwa-execution--wait
                   (lambda () (plist-get niwa-view-execution--selected :terminal-p)))
                  (should (string-search "Completed (exit 0)" (buffer-string)))
                  (should (string-search "    final output" (buffer-string)))
                  (should-not (string-search "[Stop]" (buffer-string)))
                  (should (equal-including-properties
                           draft (niwa-view-composer-draft-text)))
                  (should (= offset (niwa-view-execution-test--draft-offset)))
                  (should (= mark-offset (niwa-view-execution-test--draft-offset t)))
                  (should (buffer-local-value
                           'mark-active (niwa-view-composer-buffer)))))))
        (niwa-execution-teardown-session session)
        (delete-directory root t))))
  :doc "wrong session and equal ID with different owner cannot update selection"
  (niwa-view-execution-test--with-view
    (niwa-view--render-status)
    (niwa-view-execution-test--activate "  ▸ Running")
    (let ((before (buffer-string)))
      (dolist (event (list
                     (list :session (niwa-session--create) :data-buffer data-buf :owner "/root"
                           :type 'terminal :facts '(:execution-id "e1") :whole-output "wrong session")
                     (list :session session :data-buffer data-buf :owner "/root/other"
                           :type 'terminal :facts '(:execution-id "e1") :whole-output "wrong owner")))
        (niwa-view-execution--event event))
      (should (equal-including-properties before (buffer-string))))))

(niwa-deftest niwa-view-execution--state-changed ()
  ,test
  (test)
  :doc "Monitor batches and selected refresh preserve draft, mark, point and windows"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (switch-to-buffer view-buf)
      (setf (plist-get (car live) :kind) 'monitor
            (plist-get (car live) :description) "watch files"
            (plist-get (car live) :expires-at) 1800000000
            (plist-get (car live) :event-count) 2)
      (niwa-view--render-status)
      (niwa-view-execution-test--activate "  ▸ Watching")
      (niwa-view-test--insert-composer-draft
       "> draft\nsecond line" (- (length "> draft\nsecond line") 3))
      (niwa-view-execution-test--set-draft-mark 5)
      (let* ((other (split-window-below))
            (offset (niwa-view-execution-test--draft-offset))
            (mark-offset (niwa-view-execution-test--draft-offset t))
            (windows (window-list))
            (start (window-start)))
        (set-window-point (niwa-view-composer-window other)
                           (+ (niwa-view-composer-draft-start) 2))
        (setf (plist-get (car live) :event-count) 3
              (plist-get (car live) :output-tail) "changed")
        (niwa-view-execution--state-changed session data-buf)
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
        (should (= offset (niwa-view-execution-test--draft-offset)))
        (should (= mark-offset (niwa-view-execution-test--draft-offset t)))
        (should (buffer-local-value 'mark-active (niwa-view-composer-buffer)))
        (should (equal windows (window-list)))
        (should (= 2 (- (window-point (niwa-view-composer-window other))
                           (niwa-view-composer-draft-start))))
        (should (= start (window-start)))
        (should (string-search "until " (buffer-string)))
        (should (string-search "3 events" (buffer-string)))
        (should (string-search "changed" (buffer-string)))))))

(niwa-deftest niwa-view-execution--callback ()
  ,test
  (test)
  :doc "Stop uses user authority; stale terminal and session controls are guarded"
  (niwa-view-execution-test--with-view
    (let (stopped)
      (cl-letf (((symbol-function 'niwa-execution-stop-user)
                 (lambda (owner id) (setq stopped (list owner id)))))
        (niwa-view--render-status)
        (niwa-view-execution-test--activate "  ▸ Running")
        (niwa-view-execution-test--at "[Stop]")
        (let ((callback (get-text-property (point) 'niwa-view-zone-activate)))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal stopped (list session "e1")))
          (setq live nil stopped nil)
          (should-error (funcall callback) :type 'user-error)
          (should-not stopped)
          (with-current-buffer data-buf (setq-local niwa--session (niwa-session--create)))
          (should-error (funcall callback) :type 'user-error)))))
  :doc "explicit conversation action uses the current snapshot"
  (niwa-view-execution-test--with-view
    (let (shown)
      (cl-letf (((symbol-function 'niwa-view-audit-show-result) (lambda (record) (setq shown record))))
        (niwa-view--render-status)
        (niwa-view-execution-test--activate "  ▸ Running")
        (niwa-view-execution-test--activate "[Show in conversation]")
        (should (equal "tool1" (plist-get shown :tool-use-id)))
        (should (equal "e1" (plist-get shown :execution-id)))))))

(niwa-deftest niwa-view-execution-show-in-conversation ()
  ,test
  (test)
  :doc "settled agent navigation prepares its scheduled projection before locating the row"
  (niwa-view-execution-test--with-view
    (save-window-excursion
      (let* ((path "/root/settled")
             (agent-data (generate-new-buffer " *execution-settled-agent*"))
             (record (niwa-agent-record--create
                      :id "settled-agent" :path path :parent-path "/root"
                      :role "default" :activity 'idle
                      :conversation-buffer agent-data))
             agent-view)
        (unwind-protect
            (progn
              (setf (niwa-session-agent-registry session) (list (cons path record)))
              (with-current-buffer agent-data
                (org-mode)
                (setq-local niwa--session session
                            niwa--view-buffer view-buf)
                (insert (propertize
                         "(:name \"Bash\" :args (:command \"printf result\"))\n\nRESULT\n"
                         'gptel '(tool . "tool1"))))
              (switch-to-buffer view-buf)
              (niwa-view-test--insert-composer-draft
               "> draft\nsecond line" (- (length "> draft\nsecond line") 3))
              (progn
                (setf (plist-get (car live) :owner) path)
                (niwa-view--render-status)
                (niwa-view-execution-test--activate "  ▸ Running")
                (cl-letf (((symbol-function 'niwa-view-audit--evidence)
                           (lambda (_) (ert-fail "Existing agent row was not revealed"))))
                  (niwa-view-execution-test--activate "[Show in conversation]"))
                (setq agent-view (window-buffer (selected-window)))
                (with-current-buffer agent-view
                  (should niwa-view--agent-transcript-p)
                  (should (equal "tool1" (get-text-property (point) 'niwa-view-tool-use-id)))
                  (should-not (get-text-property (point) 'niwa-view-collapsed)))
                (with-current-buffer view-buf
                  (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))))
          (unless agent-view
            (when-let* ((window (buffer-local-value
                                'niwa-view--agent-transcript-window view-buf))
                        ((window-live-p window)))
              (setq agent-view (window-buffer window))))
          (when (buffer-live-p agent-view) (kill-buffer agent-view))
          (when (buffer-live-p agent-data) (kill-buffer agent-data)))))))

(niwa-deftest niwa-view-execution--full-output ()
  ,test
  (test)
  :doc "Full output reads real spool into private read-only buffer, not visiting buffer"
  (niwa-view-execution-test--with-view
    (let* ((path (make-temp-file "niwa-output-" nil nil "all output\n"))
           (visiting (find-file-noselect path))
           output)
      (unwind-protect
          (progn
            (with-current-buffer visiting (fundamental-mode))
            (setf (plist-get (car live) :artifact-path) path)
            (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq output buffer))))
              (niwa-view--render-status)
              (niwa-view-execution-test--activate "  ▸ Running")
              (niwa-view-execution-test--activate "[Full output]"))
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
  (niwa-view-execution-test--with-view
    (let (output)
      (unwind-protect
          (progn
            (setf (plist-get (car live) :output-tail) "only the tail"
                  (plist-get (car live) :output-preview-truncated-p) t)
            (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq output buffer))))
              (niwa-view-execution--full-output session (car live)))
            (with-current-buffer output
              (should (string-search "Full output unavailable" (buffer-string)))
              (should (string-search "Preview (not full output) — truncated" (buffer-string)))))
        (when (buffer-live-p output) (kill-buffer output)))))
  :doc "compressed full output inflates asynchronously without false truncation or leaked views"
  (niwa-view-execution-test--with-view
    (let* ((directory (file-name-concat niwa-user-dir "compressed-output-session"))
           (path (file-name-concat directory "tool-results" "executions" "output.log"))
           (body (concat "first output line\n" (make-string 70000 ?x) "\nlast line\n"))
           (make-temp (symbol-function 'make-temp-file))
           output cancel views)
      (make-directory (file-name-directory path) t)
      (write-region body nil path nil 'silent)
      (setf (niwa-session-save-path session) directory)
      (setq niwa-view-execution--selected
            (append (list :session session :terminal-p t :state 'completed
                          :output-path "artifact://executions/output.log" :artifact-path path
                          :output-bytes (string-bytes body)
                          :whole-output "preview" :whole-output-truncated-p t)
                    (car live))
            live nil)
      (unwind-protect
          (progn
            (niwa-execution-spool-compress path)
            (test-niwa-execution--wait (lambda () (not (file-exists-p path))))
            (should (file-exists-p (concat path ".gz")))
            (should (< (file-attribute-size (file-attributes (concat path ".gz")))
                       (string-bytes body)))
            (cl-letf (((symbol-function 'make-temp-file)
                       (lambda (prefix &rest args)
                         (let ((temporary (apply make-temp prefix args)))
                           (when (string-prefix-p "niwa-spool-view-" prefix)
                             (push temporary views))
                           temporary)))
                      ((symbol-function 'display-buffer)
                       (lambda (buffer &rest _) (setq output buffer))))
              (setq cancel (niwa-view-execution--full-output
                            session niwa-view-execution--selected))
              (test-niwa-execution--wait (lambda () output)))
            (with-current-buffer output
              (should buffer-read-only)
              (should-not buffer-file-name)
              (should (equal (concat "Full output · e1 · Completed\n\n" body)
                             (buffer-string)))
              (should-not (string-search "Warning:" (buffer-string))))
            (should views)
            (dolist (view views) (should-not (file-exists-p view)))
            (should-not niwa-execution-spool--readers))
        (when (functionp cancel) (funcall cancel))
        (when (buffer-live-p output) (kill-buffer output)))))
  :doc "compressed full output reports decode failure visibly and cleans its view"
  (niwa-view-execution-test--with-view
    (let* ((path (make-temp-file "niwa-corrupt-output-" nil ".log.gz"))
           (make-temp (symbol-function 'make-temp-file))
           output cancel views)
      (let ((file-name-handler-alist nil))
        (write-region "not gzip" nil path nil 'silent))
      (setf (plist-get (car live) :artifact-path) path)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'make-temp-file)
                       (lambda (prefix &rest args)
                         (let ((temporary (apply make-temp prefix args)))
                           (when (string-prefix-p "niwa-spool-view-" prefix)
                             (push temporary views))
                           temporary)))
                      ((symbol-function 'display-buffer)
                       (lambda (buffer &rest _) (setq output buffer))))
              (setq cancel (niwa-view-execution--full-output session (car live)))
              (test-niwa-execution--wait (lambda () output)))
            (with-current-buffer output
              (should buffer-read-only)
              (should (string-search "niwa [workspace unavailable]: Error: Could not decode retained execution output"
                                     (buffer-string)))
              (should (string-search "Full output unavailable" (buffer-string))))
            (should views)
            (dolist (view views) (should-not (file-exists-p view)))
            (should-not niwa-execution-spool--readers))
        (when (functionp cancel) (funcall cancel))
        (when (buffer-live-p output) (kill-buffer output))
        (delete-file path))))
  :doc "completion uses retained canonical artifact when local spool is gone"
  (niwa-view-execution-test--with-view
    (let* ((directory (file-name-concat niwa-user-dir "session"))
           (path (file-name-concat directory "tool-results" "output.log"))
           output)
      (make-directory (file-name-directory path) t)
      (write-region "canonical whole output\n" nil path nil 'silent)
      (setf (niwa-session-save-path session) directory)
      (setq niwa-view-execution--selected
            (append (list :session session :terminal-p t :state 'completed
                          :output-path "artifact://output.log" :artifact-path "/missing/spool"
                          :whole-output "preview" :whole-output-truncated-p t)
                    (car live))
            live nil)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq output buffer))))
              (niwa-view-execution--full-output session niwa-view-execution--selected))
            (with-current-buffer output
              (should (string-search "canonical whole output" (buffer-string)))
              (should-not (string-search "Full output unavailable" (buffer-string)))))
        (when (buffer-live-p output) (kill-buffer output)))))
  :doc "empty spool cannot prove no output when bytes were lost or execution is lost"
  (niwa-view-execution-test--with-view
    (let ((path (make-temp-file "niwa-empty-output-")))
      (unwind-protect
          (dolist (facts '((:output-bytes 10) (:state lost :output-bytes 0)))
            (setq niwa-view-execution--selected
                  (append (list :session session :terminal-p t :execution-id "e1"
                                :owner "/root" :artifact-path path) facts)
                  live nil)
            (let (output)
              (unwind-protect
                  (progn
                    (cl-letf (((symbol-function 'display-buffer)
                               (lambda (buffer &rest _) (setq output buffer))))
                      (niwa-view-execution--full-output session niwa-view-execution--selected))
                    (with-current-buffer output
                      (should (string-search "output unavailable" (buffer-string)))
                      (should-not (string-search "No output." (buffer-string)))))
                (when (buffer-live-p output) (kill-buffer output)))))
        (delete-file path))))
  :doc "terminal retained whole, empty, lost, missing and truncated outputs stay distinct"
  (niwa-view-execution-test--with-view
    (dolist (case '(((:whole-output "complete output") . "complete output")
                    ((:whole-output "" :output-bytes 0) . "No output.")
                    ((:state lost) . "Output unavailable.")
                    ((:output-bytes 20 :artifact-path "/missing/spool") . "Full output unavailable.")
                    ((:whole-output "bounded" :whole-output-truncated-p t) . "Preview (not full output) — truncated")))
      (setq niwa-view-execution--selected
            (append (list :session session :terminal-p t :execution-id "e1" :owner "/root")
                    (car case))
            live nil)
      (let (output)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq output buffer))))
                (niwa-view-execution--full-output session niwa-view-execution--selected))
              (with-current-buffer output (should (string-search (cdr case) (buffer-string)))))
          (when (buffer-live-p output) (kill-buffer output)))))))

(provide 'test-niwa-view-execution)
;;; test-niwa-view-execution.el ends here
