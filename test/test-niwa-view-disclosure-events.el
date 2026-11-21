;;; test-niwa-view-disclosure-events.el -- Event fold retention -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise transcript disclosure choices through passive background events.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-disclosure)
(require 'niwa-view-stream)
(require 'niwa-tool-exec)
(require 'niwa-tool-ptc)

(defmacro niwa-view-disclosure-test--with-attended-buffers (&rest body)
  "Display paired fixture buffers as an attended view while evaluating BODY."
  (declare (indent 0) (debug t))
  `(niwa-view-test--with-buffers
     (save-window-excursion
       (should-not (get-buffer-window view-buf))
       (unless noninteractive
         (should (niwa-view--unattended-p view-buf)))
       (switch-to-buffer view-buf)
       ;; Isolated graphical runners deliberately avoid taking host focus.
       ;; Model focus, not attendance or geometry, so real consumers refresh.
       (cl-letf (((symbol-function 'frame-focus-state)
                  (lambda (&optional _frame) t)))
         (redisplay t)
         (should (get-buffer-window view-buf))
         (should-not (niwa-view--unattended-p view-buf))
         ,@body))))

(defun niwa-view-disclosure-events-test--caret-offset ()
  "Return the composer caret offset from its draft start.
The current buffer is the owning view."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (- (point) (niwa-view-composer-draft-start view)))))

(niwa-deftest niwa-view-disclosure/background-events ()
  (let ((grouped ,grouped))
    (let ((niwa-view-tool-group-collapse-threshold (if grouped 1 0))
          (niwa-view-tool-boundary-render-delay 0)
          (niwa-view-stream-render-delay 0))
      (niwa-view-disclosure-test--with-attended-buffers
        (niwa-view-test--insert-data data-buf "Prompt\n" nil)
        (niwa-view-test--insert-data
         data-buf
         (concat "#+begin_tool\n(:name \"Bash\" :args (:command \"sleep 10\"))\n\noriginal output"
                 (niwa-tool-render-data-format
                  '(:status success :state running :live-execution-p t) "background")
                 "\n#+end_tool\n")
         '(tool . "background"))
        (niwa-view-test--insert-data data-buf "Between\n" 'response)
        (niwa-view-test--insert-data
         data-buf "#+begin_reasoning\nKEEP THINKING BODY\n#+end_reasoning\n" 'ignore)
        (dolist (id '("other" "last"))
          (niwa-view-test--insert-data
           data-buf
           (concat "#+begin_tool\n(:name \"Bash\" :args (:command \"sleep 10\"))\n\n"
                   (if (equal id "other") "KEEP TOOL BODY" "last output")
                   (niwa-tool-render-data-format
                    '(:status success :state running :live-execution-p t) id)
                   "\n#+end_tool\n")
           (cons 'tool id)))
        (niwa-view-test--insert-data data-buf "Answer\n" 'response)
        (with-current-buffer view-buf
          (setq niwa-view--data-turn-start
                (with-current-buffer data-buf (copy-marker (point-min))))
          (niwa-view--full-rerender)
          (niwa-view-stream-set-in-flight-turn-start
           (text-property-any (point-min) (niwa-view-transcript-tail)
                              'niwa-view-turn-role 'assistant))
          (should (text-property-any (point-min) (niwa-view-transcript-tail)
                                     'niwa-view-type 'thinking-summary))
          (when grouped
            (goto-char (text-property-any (point-min) (niwa-view-transcript-tail)
                                          'niwa-view-type 'tool-group))
            (niwa-view-toggle-section))
          (let ((pos (point-min)))
            (while (< pos (niwa-view-transcript-tail))
              (when (get-text-property pos 'niwa-view-collapsed)
                (goto-char pos)
                (niwa-view-toggle-section))
              (setq pos (next-single-property-change
                         pos 'niwa-view-source nil (niwa-view-transcript-tail)))))
          (should (string-search "KEEP THINKING BODY" (buffer-string)))
          (should (string-search "KEEP TOOL BODY" (buffer-string)))
          ;; Both normalized thinking paths must retain data markers.
          (goto-char (text-property-any (point-min) (niwa-view-transcript-tail)
                                        'niwa-view-type 'thinking-summary))
          (dotimes (_ 2)
            (niwa-view-toggle-section)
            (let ((source (get-text-property (point) 'niwa-view-source)))
              (should (eq data-buf (marker-buffer (car source))))
              (should (eq data-buf (marker-buffer (cdr source))))))
          (niwa-view-test--insert-composer-draft "> keep\nsecond line" 4))
        (dolist (type '(progress terminal))
          (niwa-view-stream-handle-execution-event
           (list :type type :data-buffer data-buf :owner "/root"
                 :tool-use-id "background" :tool-args '(:command "sleep 10")
                 :output-tail "new output" :whole-output "final output"
                 :facts (list :execution-id "exec-fold" :state
                              (if (eq type 'terminal) 'completed 'running)
                              :termination 'exited :exit-code 0 :outcome 'success
                              :wall-time-seconds 3)))
          (with-current-buffer view-buf
            (niwa-view--flush-scheduled-render view-buf t)
            (should (string-search (if (eq type 'terminal) "final output" "new output")
                                   (buffer-string)))
            (save-excursion
              (dolist (body '("KEEP THINKING BODY" "KEEP TOOL BODY"))
                (goto-char (point-min))
                (should (search-forward body (niwa-view-transcript-tail) t))
                (should-not (invisible-p (match-beginning 0)))
                (should-not (get-text-property
                             (match-beginning 0) 'niwa-view-collapsed))))
            (should (equal "> keep\nsecond line" (niwa-view-composer-draft-text view-buf t)))
            (should (= 4 (niwa-view-disclosure-events-test--caret-offset))))))))
  (grouped)
  :doc "background events retain expanded standalone thinking and tools" nil
  :doc "background events retain expanded thinking and grouped tool children" t)

(niwa-deftest niwa-view-disclosure/nested-background-events ()
  ,test (test)
  :doc "a later nested execution retains its expansion after earlier metadata grows"
  (niwa-view-disclosure-test--with-attended-buffers
    (niwa-tool-register
     (niwa-tool--create :name "ToolCall" :category "niwa"
                           :renderer #'niwa-tool-ptc--render))
    (niwa-tool-register
     (niwa-tool--create :name "Bash" :category "niwa"
                           :renderer #'niwa-tool-exec--render-bash))
    (niwa-view-test--insert-data
     data-buf
     (concat "#+begin_tool\n(:name \"Bash\" :args (:command \"true\"))\n\noriginal output"
             (niwa-tool-render-data-format
              '(:status success :state running :live-execution-p t) "earlier")
             "\n#+end_tool\n")
     '(tool . "earlier"))
    (niwa-view-test--insert-data data-buf "Between\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     (concat
      "#+begin_tool\n(:name \"ToolCall\" :args (:expression \"(list (Bash :command \\\"sleep 10\\\"))\"))\n\nreturned"
      (niwa-tool-render-data-format
       '(:kind ptc :outcome completed
         :calls ((:id "outer/1" :tool "Bash" :status success
                  :args (:command "sleep 10") :result "KEEP NESTED BODY"
                  :render-data (:execution-id "exec-nested" :state running))))
       "outer")
      "\n#+end_tool\n")
     '(tool . "outer"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (car (niwa-view--tool-row-region data-buf "outer")))
      (niwa-view-toggle-section)
      (goto-char (text-property-any (point-min) (niwa-view-transcript-tail)
                                    'niwa-view-type 'tool-child))
      (niwa-view-toggle-section)
      (should (search-forward "KEEP NESTED BODY" (niwa-view-transcript-tail) t))
      (niwa-view-test--insert-composer-draft "> nested\nsecond line" 4))
    (let ((start-before (with-current-buffer data-buf
                          (car (niwa-tool-render-data-segment-bounds "outer")))))
      (niwa-view-stream-handle-execution-event
       (list :type 'terminal :data-buffer data-buf :owner "/root"
             :tool-use-id "earlier" :tool-args '(:command "true")
             :whole-output (make-string 1000 ?x)
             :facts '(:execution-id "exec-earlier" :state completed
                      :termination exited :exit-code 0 :outcome success)))
      (should (< start-before
                 (with-current-buffer data-buf
                   (car (niwa-tool-render-data-segment-bounds "outer"))))))
    (dolist (type '(progress terminal))
      (niwa-view-stream-handle-execution-event
       (list :type type :data-buffer data-buf :owner "/root"
             :tool-use-id "outer/1" :tool-args '(:command "sleep 10")
             :output-tail "UPDATED NESTED BODY" :whole-output "UPDATED NESTED BODY"
             :facts (list :execution-id "exec-nested"
                          :state (if (eq type 'terminal) 'completed 'running)
                          :termination 'exited :exit-code 0 :outcome 'success
                          :wall-time-seconds 7)))
      (with-current-buffer view-buf
        (should (plist-get (niwa-tool-render-data-for-tool data-buf "outer") :calls))
        (should (string-search (if (eq type 'terminal)
                                   "Bash: sleep 10 · finished · 7s"
                                 "Bash: sleep 10 · running · 7s")
                               (buffer-string)))
        (let ((child-position
               (text-property-any (point-min) (niwa-view-transcript-tail)
                                  'niwa-view-type 'tool-child)))
          (should child-position)
          (should-not (get-text-property child-position 'niwa-view-collapsed)))
        (save-excursion
          (goto-char (point-min))
          (should (search-forward "UPDATED NESTED BODY" (niwa-view-transcript-tail) t))
          (should-not (invisible-p (match-beginning 0)))
          (should-not (get-text-property (match-beginning 0) 'niwa-view-collapsed)))
        (should (equal "> nested\nsecond line" (niwa-view-composer-draft-text view-buf t)))
        (should (= 4 (niwa-view-disclosure-events-test--caret-offset)))))))

(niwa-deftest niwa-view--tool-row-region/source-ownership ()
  (let ((mode ,mode))
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data
       data-buf
       "#+begin_tool\n(:name \"Bash\" :args (:command \"true\"))\n\noutput\n#+end_tool\n"
       '(tool . "ownership"))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let* ((row (niwa-view--tool-row-region data-buf "ownership"))
               (raw (with-current-buffer data-buf
                      (niwa-tool-render-data-segment-bounds "ownership")))
               (source (pcase mode
                         ('foreign (cons (copy-marker (point-min))
                                         (copy-marker (point-max))))
                         ('detached (cons (make-marker) (make-marker)))
                         ('short (niwa-view-disclosure-source-range
                                  data-buf (1+ (car raw)) (1- (cdr raw)))))))
          (let ((inhibit-read-only t))
            (put-text-property (car row) (cadr row) 'niwa-view-source source))
          (should (equal raw (nth 2 (niwa-view--tool-row-region
                                    data-buf "ownership"))))))))
  (mode)
  :doc "targeted refresh rejects foreign-buffer canonical source markers" 'foreign
  :doc "targeted refresh rejects detached canonical source markers" 'detached
  :doc "targeted refresh rejects a source no longer containing the tool run" 'short)

(provide 'test-niwa-view-disclosure-events)
;;; test-niwa-view-disclosure-events.el ends here
