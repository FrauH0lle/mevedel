;;; test-niwa-view-responsiveness.el --- Bounded view work -*- lexical-binding: t -*-

;;; Commentary:
;; Guard the work performed during animation and header-line redisplay.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-stream)
(require 'niwa-view-render)

(defun niwa-view-responsiveness-test--draft-offset ()
  "Return the caret offset from the draft start, read in the composer buffer."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (- (point) (niwa-view-composer-draft-start view)))))

(defun niwa-view-responsiveness-test--goto-draft (offset)
  "Move the composer caret OFFSET characters into the draft."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (goto-char (+ (niwa-view-composer-draft-start view) offset)))))

(niwa-deftest niwa-view-live-tail-after-disclosure-restore
  (:doc "Restoring earlier thinking leaves the intact response tail reusable.")
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "short thought\n" 'ignore)
    (niwa-view-test--insert-data data-buf "Growing response\n" 'response)
    (with-current-buffer view-buf
      (niwa-view-stream-begin-turn
       (copy-marker (niwa-view--history-insertion-marker))
       (with-current-buffer data-buf (copy-marker (point-min))))
      (niwa-view-render-live-update data-buf)
      (goto-char (point-min))
      (search-forward "Thinking...")
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (should-not (string-search "short thought" (buffer-string)))
      (niwa-view-toggle-section)
      (niwa-view-test--insert-composer-draft "> keep this\nsecond line" 4)
      (niwa-view-render-live-update data-buf)
      (should (niwa-view--live-tail-valid-p data-buf))
      (let ((original (symbol-function 'niwa-view--flush-thinking-group))
            (redraws 0))
        (cl-letf (((symbol-function 'niwa-view--flush-thinking-group)
                   (lambda (&rest args)
                     (when (car args) (cl-incf redraws))
                     (apply original args))))
          (dotimes (_ 3)
            (niwa-view-test--insert-data data-buf "more\n" 'response)
            (niwa-view-render-live-update data-buf)
            (should (equal (niwa-view-composer-draft-text)
                           "> keep this\nsecond line"))
            (should (= 4 (niwa-view-responsiveness-test--draft-offset)))))
        (should (zerop redraws)))
      (let ((text (buffer-string)))
        (should (= 1 (niwa-view-test--count-substring "short thought" text)))
        (should (= 1 (niwa-view-test--count-substring "Growing response" text)))
        (should (= 3 (niwa-view-test--count-substring "more" text)))))))

(niwa-deftest niwa-view-live-tail-rewritten-disclosure
  (:doc "Expanded final thinking retains a complete reusable unit across chunks.")
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "short thought\n" 'ignore)
    (with-current-buffer view-buf
      (niwa-view-stream-begin-turn
       (copy-marker (niwa-view--history-insertion-marker))
       (with-current-buffer data-buf (copy-marker (point-min))))
      (niwa-view-render-live-update data-buf)
      (goto-char (point-min))
      (search-forward "Thinking...")
      (goto-char (match-beginning 0))
      (niwa-view-toggle-section)
      (should-not (string-search "short thought" (buffer-string)))
      (niwa-view-toggle-section)
      (niwa-view-test--insert-composer-draft "> keep this\nsecond line" 4)
      (dotimes (_ 3)
        (niwa-view-test--insert-data data-buf "more thought\n" 'ignore)
        (niwa-view-render-live-update data-buf)
        ;; The renderer now inserts the expanded body directly.  No later
        ;; default-state restoration splits the final unit after insertion.
        (should (niwa-view--live-tail-valid-p data-buf))
        (should (equal (niwa-view-composer-draft-text) "> keep this\nsecond line"))
        (should (= 4 (niwa-view-responsiveness-test--draft-offset))))
      (let ((text (buffer-string)))
        (should (= 1 (niwa-view-test--count-substring "short thought" text)))
        (should (= 3 (niwa-view-test--count-substring "more thought" text)))))))

(niwa-deftest niwa-view-live-tail-rewritten-tool-prefix
  (:doc "Restoring a unit's first tool never retains only its later tools.")
  (niwa-view-test--with-buffers
    (let ((niwa-view-tool-group-collapse-threshold 0))
      (dolist (name '("FirstProbe" "SecondProbe"))
        (niwa-view-test--insert-data
         data-buf (format "(:name %S :args nil)\n\n%s body\n" name name)
         `(tool . ,name)))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (niwa-view-stream-begin-turn
         (copy-marker (niwa-view--history-insertion-marker))
         (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view-render-live-update data-buf)
        (goto-char (point-min))
        (search-forward "FirstProbe")
        (niwa-view-toggle-section)
        (niwa-view-responsiveness-test--goto-draft 4)
        (dotimes (i 3)
          (when (> i 0)
            (niwa-view-test--insert-data
             data-buf (format "(:name \"LaterProbe%d\" :args nil)\n\nbody\n" i)
             `(tool . ,(format "later-%d" i))))
          (niwa-view-render-live-update data-buf)
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
          (should (= 4 (niwa-view-responsiveness-test--draft-offset)))
          (let ((text (buffer-string)))
            (should (= 1 (niwa-view-test--count-substring
                          "FirstProbe body" text)))
            (should (= 1 (niwa-view-test--count-substring
                          "SecondProbe" text)))))))))

(defconst test-niwa-view-responsiveness--permission-audit
  '(:type tool-permission :event "PreToolUse" :outcome "allow"
    :reason "policy")
  "A hook audit whose expanded body names its outcome.")

(niwa-deftest niwa-view--insert-hook-audit-block/remembered-state
  (:doc "Hook audits render in the fold state the reader last chose.")
  (with-temp-buffer
    (let ((record test-niwa-view-responsiveness--permission-audit)
          (source (cons 10 20)))
      (niwa-view--insert-hook-audit-block record source)
      (should (get-text-property (point-min) 'niwa-view-collapsed))
      (should-not (string-search "Outcome:" (buffer-string)))
      (goto-char (point-min))
      (niwa-view-audit-toggle-hook-audit)
      (should (string-search "Outcome: allow" (buffer-string)))
      (should (equal (get-text-property (point-min) 'niwa-view-source-key)
                     (car (niwa-view-disclosure-state-for-key
                           (get-text-property (point-min)
                                              'niwa-view-source-key)))))
      ;; A later projection of the same record keeps the reader's choice.
      (let ((inhibit-read-only t)) (erase-buffer))
      (niwa-view--insert-hook-audit-block record source)
      (should-not (get-text-property (point-min) 'niwa-view-collapsed))
      (should (string-search "Outcome: allow" (buffer-string)))
      (goto-char (point-min))
      (niwa-view-audit-toggle-hook-audit)
      (let ((inhibit-read-only t)) (erase-buffer))
      (niwa-view--insert-hook-audit-block record source)
      (should (get-text-property (point-min) 'niwa-view-collapsed)))))

(niwa-deftest niwa-view-live-tail-expanded-hook-audit
  (:doc "An expanded hook audit never forces whole-turn rebuilds.")
  (niwa-view-test--with-buffers
    (let ((niwa-view-tool-group-collapse-threshold 0))
      (niwa-view-test--insert-data
       data-buf "(:name \"FirstProbe\" :args nil)\n\nFirstProbe body\n"
       '(tool . "first"))
      (with-current-buffer data-buf
        (goto-char (point-max))
        (insert (niwa--format-hook-audit-record
                 test-niwa-view-responsiveness--permission-audit)))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (niwa-view-stream-begin-turn
         (copy-marker (niwa-view--history-insertion-marker))
         (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view-render-live-update data-buf)
        (goto-char (point-min))
        (search-forward "hook changed tool permission")
        (niwa-view-toggle-section)
        (niwa-view-responsiveness-test--goto-draft 4)
        (let (toggled)
          (cl-letf (((symbol-function 'niwa-view-audit-toggle-hook-audit)
                     (lambda () (setq toggled t))))
            (dotimes (i 3)
              (niwa-view-test--insert-data
               data-buf (format "(:name \"Later%d\" :args nil)\n\nbody\n" i)
               `(tool . ,(format "later-%d" i)))
              (niwa-view-render-live-update data-buf)
              (should (niwa-view--live-tail-valid-p data-buf))
              (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
              (should (= 4 (niwa-view-responsiveness-test--draft-offset)))))
          (should-not toggled))
        (let ((text (buffer-string)))
          (should (= 1 (niwa-view-test--count-substring "Outcome: allow" text)))
          (should (= 1 (niwa-view-test--count-substring "Later2" text))))))))

(niwa-deftest niwa-view--recover-in-flight-turn-start
  (:doc "Recovery finds the first in-flight source and its turn header.")
  (with-temp-buffer
    (insert (propertize "old" 'niwa-view-source (cons 1 5))
            (propertize "Assistant\n" 'niwa-view-type 'turn-header)
            (propertize "Assistant\n" 'niwa-view-type 'turn-header
                        'face 'bold)
            "gap"
            (propertize "new" 'niwa-view-source (cons 10 20))
            (propertize "newer" 'niwa-view-source (cons 20 30)))
    (should (= 4 (niwa-view--recover-in-flight-turn-start
                  10 (point-min) (point-max))))
    (should (= 4 (niwa-view--recover-in-flight-turn-start
                  6 (point-min) (point-max))))
    ;; Without a header after HISTORY-START the source itself starts the turn.
    (should (= 30 (niwa-view--recover-in-flight-turn-start
                   15 24 (point-max))))
    (should-not (niwa-view--recover-in-flight-turn-start
                 40 (point-min) (point-max)))
    (should-not (niwa-view--recover-in-flight-turn-start
                 10 (point-min) (point-min)))
    ;; A source whose marker was detached, as when its archive buffer was
    ;; killed, is skipped instead of signalling.
    (erase-buffer)
    (let ((dead (make-marker)))
      (insert (propertize "dead" 'niwa-view-source (cons dead dead))
              (propertize "new" 'niwa-view-source (cons 10 20)))
      (should (= 5 (niwa-view--recover-in-flight-turn-start
                    10 (point-min) (point-max)))))))

(niwa-deftest niwa-view--first-live-update
  (:doc "The ordinary send echo bounds first-stream recovery.")
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Old prompt\n\n" nil)
    (niwa-view-test--insert-data data-buf "Old answer\n\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-data data-buf "New prompt\n\n" nil)
      (niwa-view-stream-begin-turn
       (niwa-view--insert-user-message "New prompt")
       (with-current-buffer data-buf (copy-marker (point-max))))
      (niwa-view-test--insert-composer-draft "Next draft" 4)
      (niwa-view-test--insert-data data-buf "First words\n\n" 'response)
      (cl-letf (((symbol-function 'niwa-view--recover-in-flight-turn-start)
                 (lambda (&rest _) (ert-fail "Scanned old history"))))
        (niwa-view-render-live-update data-buf))
      (should (string-search "Old answer" (buffer-string)))
      (should (string-search "First words" (buffer-string)))
      (should (equal "Next draft" (niwa-view-composer-draft-text)))
      (should (= 4 (niwa-view-responsiveness-test--draft-offset))))))

(niwa-deftest niwa-view--settle-reader-anchors
  (:doc "Settling a turn keeps readers in place without rescanning history.")
  (save-window-excursion
    (delete-other-windows)
    (niwa-view-test--with-buffers
      (dotimes (i 12)
        (niwa-view-test--insert-data data-buf (format "Old prompt %d\n\n" i) nil)
        (niwa-view-test--insert-data
         data-buf (format "Old answer %d\nsecond line %d\n\n" i i) 'response))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (niwa-view-test--insert-data data-buf "New prompt\n\n" nil)
        (niwa-view-stream-begin-turn
         (niwa-view--insert-user-message "New prompt")
         (with-current-buffer data-buf (copy-marker (point-max))))
        (niwa-view-test--insert-data data-buf "First words\n\n" 'response)
        (niwa-view-render-live-update data-buf)
        (let* ((reader (selected-window))
               (end (split-window reader))
               (turn-start (niwa-view-stream-in-flight-turn-start-position))
               (history-end (save-excursion
                              (goto-char (point-min))
                              (search-forward "second line 10")))
               (data-start (with-current-buffer data-buf
                             (save-excursion
                               (goto-char (point-min))
                               (search-forward "First words")
                               (match-beginning 0))))
               (old-start (save-excursion
                            (goto-char (point-min))
                            (search-forward "Old answer 3")
                            (line-beginning-position)))
               (old-point (+ 2 (save-excursion
                                 (goto-char old-start)
                                 (search-forward "second line 4")
                                 (line-beginning-position))))
               scans)
          (set-window-buffer reader view-buf)
          (set-window-buffer end view-buf)
          (set-window-start reader old-start)
          (set-window-point reader old-point)
          (set-window-start end turn-start)
          (set-window-point end (point-max))
          (niwa-view-test--insert-data data-buf "More words\n\n" 'response)
          (cl-letf* (((symbol-function 'niwa-view--recover-in-flight-turn-start)
                      (lambda (&rest _) (ert-fail "Scanned history despite a live source")))
                     (run-end (symbol-function 'niwa-view--render-anchor-run-end))
                     ((symbol-function 'niwa-view--render-anchor-run-end)
                      (lambda (pos limit)
                        (push (+ 0 pos) scans)
                        (funcall run-end pos limit))))
            (with-current-buffer data-buf
              (niwa-view-stream-render-response data-start (point-max))))
          (should (string-search "More words" (buffer-string)))
          (should (string-search "Old answer 11" (buffer-string)))
          ;; The settle rewrites only the newest turns; older rows are not
          ;; counted.
          (should scans)
          (should (cl-every (lambda (pos) (> pos history-end)) scans))
          (should (= old-start (window-start reader)))
          (should (= old-point (window-point reader)))
          (should (string-prefix-p "Old answer 3"
                                   (buffer-substring (window-start reader)
                                                     (point-max))))
          (should (= (point-max) (window-point end))))))))

(niwa-deftest niwa-view--retain-last-live-render-unit
  (:doc "Only the final unit, across its property runs, is retained.")
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "response text" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "first" 'niwa-view-live-unit-source 1)
                (propertize "sec" 'niwa-view-live-unit-source 5)
                (propertize "ond" 'niwa-view-live-unit-source 5 'face 'bold)
                "tail without unit"))
      (niwa-view--retain-last-live-render-unit data-buf 1 (point-max))
      (should (niwa-view--live-tail-valid-p data-buf))
      (should (= 6 (marker-position niwa-view--live-view-tail-start)))
      (should (= 5 (marker-position niwa-view--live-data-tail-start)))
      (niwa-view--retain-last-live-render-unit data-buf 9 (point-max))
      (should (= 9 (marker-position niwa-view--live-view-tail-start)))
      (niwa-view--retain-last-live-render-unit data-buf 12 (point-max))
      (should-not (niwa-view--live-tail-valid-p data-buf)))))

(niwa-deftest niwa-view--live-tail-intact-p
  (:doc "Retained units reject split, merged and deleted source ranges.")
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "response" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (insert "Xabcdef")
        (setq niwa-view--live-data-tail-start
              (with-current-buffer data-buf (copy-marker 1))
              niwa-view--live-view-tail-start (copy-marker 2))
        (put-text-property 2 8 'niwa-view-live-unit-source 1)
        (should (niwa-view--live-tail-intact-p data-buf 3 8))
        (should-not (niwa-view--live-tail-intact-p data-buf 2 8))
        (let ((first-end (copy-marker 3 nil))
              (end (copy-marker 8 t)))
          (unwind-protect
              (progn
                (set-marker-insertion-type niwa-view--live-view-tail-start t)
                (delete-region 2 4)
                (goto-char 2)
                (insert "new prefix")
                (remove-text-properties 2 (point)
                                        '(niwa-view-live-unit-source nil))
                ;; The remaining suffix alone is homogeneous but no longer
                ;; represents the complete unit selected before restoration.
                (should-not (niwa-view--live-tail-intact-p
                             data-buf (marker-position first-end)
                             (marker-position end))))
            (set-marker first-end nil)
            (set-marker end nil)))
        (delete-region 2 (point-max))
        (goto-char 2)
        (insert "abcdef")
        (set-marker niwa-view--live-view-tail-start 2)
        (set-marker-insertion-type niwa-view--live-view-tail-start nil)
        (put-text-property 2 8 'niwa-view-live-unit-source 1)
        (put-text-property 4 5 'niwa-view-live-unit-source nil)
        (should-not (niwa-view--live-tail-intact-p data-buf 3 8))
        (put-text-property 1 8 'niwa-view-live-unit-source 1)
        (should-not (niwa-view--live-tail-intact-p data-buf 3 8))
        (should-not (niwa-view--live-tail-intact-p data-buf 3 2))
        (niwa-view-render-invalidate-live-tail)
        (should-not (niwa-view--live-tail-intact-p data-buf 3 8))))))

(niwa-deftest niwa-view--animation-span-in-window-p
  (:doc "Visibility uses overlapping buffer ranges without querying glyphs.")
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert (make-string 200 ?x))
      (let ((window (selected-window)))
        (cl-letf (((symbol-function 'window-start) (lambda (_) 50))
                  ((symbol-function 'window-end) (lambda (_ &optional _) 150))
                  ((symbol-function 'posn-at-point)
                   (lambda (&rest _) (ert-fail "Visibility queried glyphs")))
                  ((symbol-function 'posn-at-x-y)
                   (lambda (&rest _) (ert-fail "Visibility queried pixels"))))
          (dolist (display '(nil "animated label"))
            (put-text-property 1 201 'display display)
            (dolist (scroll '(0 20))
              (set-window-hscroll window scroll)
              (should (niwa-view--animation-span-in-window-p 60 80 window))
              (should (niwa-view--animation-span-in-window-p 40 60 window))
              (should (niwa-view--animation-span-in-window-p 140 160 window))
              (should-not (niwa-view--animation-span-in-window-p 20 50 window))
              (should-not (niwa-view--animation-span-in-window-p 150 180 window))
              (should-not (niwa-view--animation-span-in-window-p 60 60 window)))))))))

(niwa-deftest niwa-view--prompt-preview
  ()
  ,test
  (test)
  :doc "short previews retain filtering and whitespace normalization"
  (should (equal "hello world"
                 (niwa-view--prompt-preview "  hello\n world  " nil)))
  (should (equal "shared text"
                 (niwa-view--prompt-preview "ignored" '(:text "shared text"))))
  (should-not (niwa-view--prompt-preview " \n\t" nil))

  :doc "long previews are bounded before header-line width measurement"
  (let* ((text (concat (make-string 20000 ?x) " final"))
         (preview (niwa-view--prompt-preview text nil)))
    (should (= (length preview) 512))
    (should (string-suffix-p "…" preview))
    (should (equal (substring preview 0 511) (make-string 511 ?x)))
    (should (= (length text) 20006)))

  :doc "a mailbox before a long prompt is filtered before truncation"
  (let ((preview (niwa-view--prompt-preview
                  (concat "<agent-message sender=\"/root/worker\">\n"
                          (make-string 1000 ?a)
                          "\n</agent-message>\n" (make-string 1000 ?b)) nil)))
    (should (= (length preview) 512))
    (should (string-prefix-p (make-string 511 ?b) preview))))

(provide 'test-niwa-view-responsiveness)
;;; test-niwa-view-responsiveness.el ends here
