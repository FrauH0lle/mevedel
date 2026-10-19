;;; test-mevedel-view-disclosure-events.el -- Event fold retention -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise transcript disclosure choices through passive background events.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-stream)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-ptc)

(defmacro mevedel-view-disclosure-test--with-attended-buffers (&rest body)
  "Display paired fixture buffers as an attended view while evaluating BODY."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (save-window-excursion
       (should-not (get-buffer-window view-buf))
       (unless noninteractive
         (should (mevedel-view--unattended-p view-buf)))
       (switch-to-buffer view-buf)
       ;; Isolated graphical runners deliberately avoid taking host focus.
       ;; Model focus, not attendance or geometry, so real consumers refresh.
       (cl-letf (((symbol-function 'frame-focus-state)
                  (lambda (&optional _frame) t)))
         (redisplay t)
         (should (get-buffer-window view-buf))
         (should-not (mevedel-view--unattended-p view-buf))
         ,@body))))

(mevedel-deftest mevedel-view-disclosure/background-events ()
  (let ((grouped ,grouped))
    (let ((mevedel-view-tool-group-collapse-threshold (if grouped 1 0))
          (mevedel-view-tool-boundary-render-delay 0)
          (mevedel-view-stream-render-delay 0))
      (mevedel-view-disclosure-test--with-attended-buffers
        (mevedel-view-test--insert-data data-buf "Prompt\n" nil)
        (mevedel-view-test--insert-data
         data-buf
         (concat "#+begin_tool\n(:name \"Bash\" :args (:command \"sleep 10\"))\n\noriginal output"
                 (mevedel-tool-render-data-format
                  '(:status success :state running :live-execution-p t) "background")
                 "\n#+end_tool\n")
         '(tool . "background"))
        (mevedel-view-test--insert-data data-buf "Between\n" 'response)
        (mevedel-view-test--insert-data
         data-buf "#+begin_reasoning\nKEEP THINKING BODY\n#+end_reasoning\n" 'ignore)
        (dolist (id '("other" "last"))
          (mevedel-view-test--insert-data
           data-buf
           (concat "#+begin_tool\n(:name \"Bash\" :args (:command \"sleep 10\"))\n\n"
                   (if (equal id "other") "KEEP TOOL BODY" "last output")
                   (mevedel-tool-render-data-format
                    '(:status success :state running :live-execution-p t) id)
                   "\n#+end_tool\n")
           (cons 'tool id)))
        (mevedel-view-test--insert-data data-buf "Answer\n" 'response)
        (with-current-buffer view-buf
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf (copy-marker (point-min))))
          (mevedel-view--full-rerender)
          (mevedel-view-stream-set-in-flight-turn-start
           (text-property-any (point-min) mevedel-view--input-marker
                              'mevedel-view-turn-role 'assistant))
          (should (text-property-any (point-min) mevedel-view--input-marker
                                     'mevedel-view-type 'thinking-summary))
          (when grouped
            (goto-char (text-property-any (point-min) mevedel-view--input-marker
                                          'mevedel-view-type 'tool-group))
            (mevedel-view-toggle-section))
          (let ((pos (point-min)))
            (while (< pos mevedel-view--input-marker)
              (when (get-text-property pos 'mevedel-view-collapsed)
                (goto-char pos)
                (mevedel-view-toggle-section))
              (setq pos (next-single-property-change
                         pos 'mevedel-view-source nil mevedel-view--input-marker))))
          (should (string-search "KEEP THINKING BODY" (buffer-string)))
          (should (string-search "KEEP TOOL BODY" (buffer-string)))
          ;; Both normalized thinking paths must retain data markers.
          (goto-char (text-property-any (point-min) mevedel-view--input-marker
                                        'mevedel-view-type 'thinking-summary))
          (dotimes (_ 2)
            (mevedel-view-toggle-section)
            (let ((source (get-text-property (point) 'mevedel-view-source)))
              (should (eq data-buf (marker-buffer (car source))))
              (should (eq data-buf (marker-buffer (cdr source))))))
          (mevedel-view-test--insert-composer-draft "> keep\nsecond line" 4))
        (dolist (type '(progress terminal))
          (mevedel-view-stream-handle-execution-event
           (list :type type :data-buffer data-buf :owner "/root"
                 :tool-use-id "background" :tool-args '(:command "sleep 10")
                 :output-tail "new output" :whole-output "final output"
                 :facts (list :execution-id "exec-fold" :state
                              (if (eq type 'terminal) 'completed 'running)
                              :termination 'exited :exit-code 0 :outcome 'success
                              :wall-time-seconds 3)))
          (with-current-buffer view-buf
            (mevedel-view--flush-scheduled-render view-buf t)
            (should (string-search (if (eq type 'terminal) "final output" "new output")
                                   (buffer-string)))
            (save-excursion
              (dolist (body '("KEEP THINKING BODY" "KEEP TOOL BODY"))
                (goto-char (point-min))
                (should (search-forward body mevedel-view--input-marker t))
                (should-not (invisible-p (match-beginning 0)))
                (should-not (get-text-property
                             (match-beginning 0) 'mevedel-view-collapsed))))
            (should (equal "> keep\nsecond line" (mevedel-view--input-text)))
            (should (= 4 (- (point) (mevedel-view--input-start)))))))))
  (grouped)
  :doc "background events retain expanded standalone thinking and tools" nil
  :doc "background events retain expanded thinking and grouped tool children" t)

(mevedel-deftest mevedel-view-disclosure/nested-background-events ()
  ,test (test)
  :doc "a later nested execution retains its expansion after earlier metadata grows"
  (mevedel-view-disclosure-test--with-attended-buffers
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                           :renderer #'mevedel-tool-ptc--render))
    (mevedel-tool-register
     (mevedel-tool--create :name "Bash" :category "mevedel"
                           :renderer #'mevedel-tool-exec--render-bash))
    (mevedel-view-test--insert-data
     data-buf
     (concat "#+begin_tool\n(:name \"Bash\" :args (:command \"true\"))\n\noriginal output"
             (mevedel-tool-render-data-format
              '(:status success :state running :live-execution-p t) "earlier")
             "\n#+end_tool\n")
     '(tool . "earlier"))
    (mevedel-view-test--insert-data data-buf "Between\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "#+begin_tool\n(:name \"ToolCall\" :args (:expression \"(list (Bash :command \\\"sleep 10\\\"))\"))\n\nreturned"
      (mevedel-tool-render-data-format
       '(:kind ptc :outcome completed
         :calls ((:id "outer/1" :tool "Bash" :status success
                  :args (:command "sleep 10") :result "KEEP NESTED BODY"
                  :render-data (:execution-id "exec-nested" :state running))))
       "outer")
      "\n#+end_tool\n")
     '(tool . "outer"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (car (mevedel-view--tool-row-region data-buf "outer")))
      (mevedel-view-toggle-section)
      (goto-char (text-property-any (point-min) mevedel-view--input-marker
                                    'mevedel-view-type 'tool-child))
      (mevedel-view-toggle-section)
      (should (search-forward "KEEP NESTED BODY" mevedel-view--input-marker t))
      (mevedel-view-test--insert-composer-draft "> nested\nsecond line" 4))
    (let ((start-before (with-current-buffer data-buf
                          (car (mevedel-tool-render-data-segment-bounds "outer")))))
      (mevedel-view-stream-handle-execution-event
       (list :type 'terminal :data-buffer data-buf :owner "/root"
             :tool-use-id "earlier" :tool-args '(:command "true")
             :whole-output (make-string 1000 ?x)
             :facts '(:execution-id "exec-earlier" :state completed
                      :termination exited :exit-code 0 :outcome success)))
      (should (< start-before
                 (with-current-buffer data-buf
                   (car (mevedel-tool-render-data-segment-bounds "outer"))))))
    (dolist (type '(progress terminal))
      (mevedel-view-stream-handle-execution-event
       (list :type type :data-buffer data-buf :owner "/root"
             :tool-use-id "outer/1" :tool-args '(:command "sleep 10")
             :output-tail "UPDATED NESTED BODY" :whole-output "UPDATED NESTED BODY"
             :facts (list :execution-id "exec-nested"
                          :state (if (eq type 'terminal) 'completed 'running)
                          :termination 'exited :exit-code 0 :outcome 'success
                          :wall-time-seconds 7)))
      (with-current-buffer view-buf
        (should (plist-get (mevedel-tool-render-data-for-tool data-buf "outer") :calls))
        (should (string-search (if (eq type 'terminal)
                                   "Bash: sleep 10 · finished · 7s"
                                 "Bash: sleep 10 · running · 7s")
                               (buffer-string)))
        (let ((child-position
               (text-property-any (point-min) mevedel-view--input-marker
                                  'mevedel-view-type 'tool-child)))
          (should child-position)
          (should-not (get-text-property child-position 'mevedel-view-collapsed)))
        (save-excursion
          (goto-char (point-min))
          (should (search-forward "UPDATED NESTED BODY" mevedel-view--input-marker t))
          (should-not (invisible-p (match-beginning 0)))
          (should-not (get-text-property (match-beginning 0) 'mevedel-view-collapsed)))
        (should (equal "> nested\nsecond line" (mevedel-view--input-text)))
        (should (= 4 (- (point) (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-view--tool-row-region/source-ownership ()
  (let ((mode ,mode))
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data
       data-buf
       "#+begin_tool\n(:name \"Bash\" :args (:command \"true\"))\n\noutput\n#+end_tool\n"
       '(tool . "ownership"))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let* ((row (mevedel-view--tool-row-region data-buf "ownership"))
               (raw (with-current-buffer data-buf
                      (mevedel-tool-render-data-segment-bounds "ownership")))
               (source (pcase mode
                         ('foreign (cons (copy-marker (point-min))
                                         (copy-marker (point-max))))
                         ('detached (cons (make-marker) (make-marker)))
                         ('short (mevedel-view-disclosure-source-range
                                  data-buf (1+ (car raw)) (1- (cdr raw)))))))
          (let ((inhibit-read-only t))
            (put-text-property (car row) (cadr row) 'mevedel-view-source source))
          (should (equal raw (nth 2 (mevedel-view--tool-row-region
                                    data-buf "ownership"))))))))
  (mode)
  :doc "targeted refresh rejects foreign-buffer canonical source markers" 'foreign
  :doc "targeted refresh rejects detached canonical source markers" 'detached
  :doc "targeted refresh rejects a source no longer containing the tool run" 'short)

(provide 'test-mevedel-view-disclosure-events)
;;; test-mevedel-view-disclosure-events.el ends here
