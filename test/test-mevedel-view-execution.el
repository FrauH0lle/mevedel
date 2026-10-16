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
      (should (string-search "+2 more / all" body)))
    (setq mevedel-view-execution--selected (plist-put (copy-sequence (nth 6 live)) :session session))
    (let ((body (plist-get (mevedel-view-execution-fragment session) :body)))
      (should (= 4 (mevedel-view-test--count-substring "▸ Running" body)))
      (should (= 1 (mevedel-view-test--count-substring "▾ Running" body)))))
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
  :doc "blank suffix and header margins are inert, All is explicit"
  (mevedel-view-execution-test--with-view
    (mevedel-view--render-status)
    (mevedel-view-execution-test--at " ─── background work")
    (should-not (get-text-property (point) 'mevedel-view-zone-activate))
    (mevedel-view-execution-test--at "  ▸ Running")
    (forward-line 1)
    (should (eq (char-after) ?\n))
    (should-not (get-text-property (point) 'mevedel-view-zone-activate))
    (let (opened)
      (cl-letf (((symbol-function 'mevedel-executions-list-open) (lambda (&optional _) (setq opened t))))
        (mevedel-view-execution-test--activate "All…")
        (should opened))))
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
  :doc "rule counts running and watching work and links All…"
  (mevedel-view-execution-test--with-view
    (let* ((header (mevedel-view-execution--header
                    session '((:kind bash) (:kind monitor) (:kind monitor))))
           (all (string-search "All…" header)))
      (should (string-prefix-p " ─── background work · 1 running · 2 watching · All… ─"
                               header))
      (should (eq 'mevedel-view-zone-separator (get-text-property 1 'font-lock-face header)))
      (should (eq 'link (get-text-property all 'font-lock-face header)))
      (should (functionp (get-text-property all 'mevedel-view-zone-activate header)))))
  :doc "a clipped narrow rule keeps All… reachable"
  (mevedel-view-execution-test--with-view
    (cl-letf (((symbol-function 'mevedel-view--zone-separator)
               (lambda (_) " ─── background w… ─\n")))
      (let ((header (mevedel-view-execution--header session nil)))
        (should (equal " All… background w… ─\n" (substring-no-properties header)))
        (should (get-text-property 1 'mevedel-view-zone-activate header))))))

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
      (should (equal "  ▸ Watching · watch · 1 event\n" (substring-no-properties line)))
      (should (eq 'mevedel-view-agent-running
                  (get-text-property (string-search "Watching" line) 'font-lock-face line)))
      (should (eq 'mevedel-view-tool-metadata
                  (get-text-property (string-search "▸" line) 'font-lock-face line))))))

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
      (should-not (mevedel-view-execution-fragment session))))
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
        (should (string-search "expires " (buffer-string)))
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
