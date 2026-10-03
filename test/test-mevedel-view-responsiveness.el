;;; test-mevedel-view-responsiveness.el --- Bounded view work -*- lexical-binding: t -*-

;;; Commentary:
;; Guard the work performed during animation and header-line redisplay.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-stream)
(require 'mevedel-view-render)

(mevedel-deftest mevedel-view-live-tail-after-disclosure-restore
  (:doc "Restoring earlier thinking leaves the intact response tail reusable.")
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "short thought\n" 'ignore)
    (mevedel-view-test--insert-data data-buf "Growing response\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view-stream-begin-turn
       (copy-marker (mevedel-view--history-insertion-marker))
       (with-current-buffer data-buf (copy-marker (point-min))))
      (mevedel-view-render-live-update data-buf)
      (goto-char (point-min))
      (search-forward "Thinking...")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((inhibit-read-only t))
        (goto-char (mevedel-view--input-start))
        (insert "> keep this\nsecond line"))
      (goto-char (+ 4 (mevedel-view--input-start)))
      (mevedel-view-render-live-update data-buf)
      (should (mevedel-view--live-tail-valid-p data-buf))
      (let ((original (symbol-function 'mevedel-view--flush-thinking-group))
            (redraws 0))
        (cl-letf (((symbol-function 'mevedel-view--flush-thinking-group)
                   (lambda (&rest args)
                     (when (car args) (cl-incf redraws))
                     (apply original args))))
          (dotimes (_ 3)
            (mevedel-view-test--insert-data data-buf "more\n" 'response)
            (mevedel-view-render-live-update data-buf)
            (should (equal (mevedel-view--input-text)
                           "> keep this\nsecond line"))
            (should (= 4 (- (point) (mevedel-view--input-start))))))
        (should (zerop redraws)))
      (let ((text (buffer-string)))
        (should (= 1 (mevedel-view-test--count-substring "short thought" text)))
        (should (= 1 (mevedel-view-test--count-substring "Growing response" text)))
        (should (= 3 (mevedel-view-test--count-substring "more" text)))))))

(mevedel-deftest mevedel-view-live-tail-rewritten-disclosure
  (:doc "Expanded final thinking is rebuilt whole across successive chunks.")
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "short thought\n" 'ignore)
    (with-current-buffer view-buf
      (mevedel-view-stream-begin-turn
       (copy-marker (mevedel-view--history-insertion-marker))
       (with-current-buffer data-buf (copy-marker (point-min))))
      (mevedel-view-render-live-update data-buf)
      (goto-char (point-min))
      (search-forward "Thinking...")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((inhibit-read-only t))
        (goto-char (mevedel-view--input-start))
        (insert "> keep this\nsecond line"))
      (goto-char (+ 4 (mevedel-view--input-start)))
      (dotimes (_ 3)
        (mevedel-view-test--insert-data data-buf "more thought\n" 'ignore)
        (mevedel-view-render-live-update data-buf)
        (should-not (mevedel-view--live-tail-valid-p data-buf))
        (should (equal (mevedel-view--input-text) "> keep this\nsecond line"))
        (should (= 4 (- (point) (mevedel-view--input-start)))))
      (let ((text (buffer-string)))
        (should (= 1 (mevedel-view-test--count-substring "short thought" text)))
        (should (= 3 (mevedel-view-test--count-substring "more thought" text)))))))

(mevedel-deftest mevedel-view-live-tail-rewritten-tool-prefix
  (:doc "Restoring a unit's first tool never retains only its later tools.")
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-tool-group-collapse-threshold 0))
      (dolist (name '("FirstProbe" "SecondProbe"))
        (mevedel-view-test--insert-data
         data-buf (format "(:name %S :args nil)\n\n%s body\n" name name)
         `(tool . ,name)))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (mevedel-view-stream-begin-turn
         (copy-marker (mevedel-view--history-insertion-marker))
         (with-current-buffer data-buf (copy-marker (point-min))))
        (mevedel-view-render-live-update data-buf)
        (goto-char (point-min))
        (search-forward "FirstProbe")
        (mevedel-view-toggle-section)
        (goto-char (+ (mevedel-view--input-start) 4))
        (dotimes (i 3)
          (when (> i 0)
            (mevedel-view-test--insert-data
             data-buf (format "(:name \"LaterProbe%d\" :args nil)\n\nbody\n" i)
             `(tool . ,(format "later-%d" i))))
          (mevedel-view-render-live-update data-buf)
          (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
          (should (= (point) (+ (mevedel-view--input-start) 4)))
          (let ((text (buffer-string)))
            (should (= 1 (mevedel-view-test--count-substring
                          "FirstProbe body" text)))
            (should (= 1 (mevedel-view-test--count-substring
                          "SecondProbe" text)))))))))

(defconst test-mevedel-view-responsiveness--permission-audit
  '(:type tool-permission :event "PreToolUse" :outcome "allow"
    :reason "policy")
  "A hook audit whose expanded body names its outcome.")

(mevedel-deftest mevedel-view--insert-hook-audit-block/remembered-state
  (:doc "Hook audits render in the fold state the reader last chose.")
  (with-temp-buffer
    (let ((record test-mevedel-view-responsiveness--permission-audit)
          (source (cons 10 20)))
      (mevedel-view--insert-hook-audit-block record source)
      (should (get-text-property (point-min) 'mevedel-view-collapsed))
      (should-not (string-search "Outcome:" (buffer-string)))
      (goto-char (point-min))
      (mevedel-view-audit-toggle-hook-audit)
      (should (string-search "Outcome: allow" (buffer-string)))
      (should (equal (get-text-property (point-min) 'mevedel-view-source-key)
                     (car (mevedel-view-disclosure-state-for-key
                           (get-text-property (point-min)
                                              'mevedel-view-source-key)))))
      ;; A later projection of the same record keeps the reader's choice.
      (let ((inhibit-read-only t)) (erase-buffer))
      (mevedel-view--insert-hook-audit-block record source)
      (should-not (get-text-property (point-min) 'mevedel-view-collapsed))
      (should (string-search "Outcome: allow" (buffer-string)))
      (goto-char (point-min))
      (mevedel-view-audit-toggle-hook-audit)
      (let ((inhibit-read-only t)) (erase-buffer))
      (mevedel-view--insert-hook-audit-block record source)
      (should (get-text-property (point-min) 'mevedel-view-collapsed)))))

(mevedel-deftest mevedel-view-live-tail-expanded-hook-audit
  (:doc "An expanded hook audit never forces whole-turn rebuilds.")
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-tool-group-collapse-threshold 0))
      (mevedel-view-test--insert-data
       data-buf "(:name \"FirstProbe\" :args nil)\n\nFirstProbe body\n"
       '(tool . "first"))
      (with-current-buffer data-buf
        (goto-char (point-max))
        (insert (mevedel--format-hook-audit-record
                 test-mevedel-view-responsiveness--permission-audit)))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (mevedel-view-stream-begin-turn
         (copy-marker (mevedel-view--history-insertion-marker))
         (with-current-buffer data-buf (copy-marker (point-min))))
        (mevedel-view-render-live-update data-buf)
        (goto-char (point-min))
        (search-forward "hook changed tool permission")
        (mevedel-view-toggle-section)
        (goto-char (+ (mevedel-view--input-start) 4))
        (let (toggled)
          (cl-letf (((symbol-function 'mevedel-view-audit-toggle-hook-audit)
                     (lambda () (setq toggled t))))
            (dotimes (i 3)
              (mevedel-view-test--insert-data
               data-buf (format "(:name \"Later%d\" :args nil)\n\nbody\n" i)
               `(tool . ,(format "later-%d" i)))
              (mevedel-view-render-live-update data-buf)
              (should (mevedel-view--live-tail-valid-p data-buf))
              (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
              (should (= (point) (+ (mevedel-view--input-start) 4)))))
          (should-not toggled))
        (let ((text (buffer-string)))
          (should (= 1 (mevedel-view-test--count-substring "Outcome: allow" text)))
          (should (= 1 (mevedel-view-test--count-substring "Later2" text))))))))

(mevedel-deftest mevedel-view--recover-in-flight-turn-start
  (:doc "Recovery finds the first in-flight source and its turn header.")
  (with-temp-buffer
    (insert (propertize "old" 'mevedel-view-source (cons 1 5))
            (propertize "Assistant\n" 'mevedel-view-type 'turn-header)
            (propertize "Assistant\n" 'mevedel-view-type 'turn-header
                        'face 'bold)
            "gap"
            (propertize "new" 'mevedel-view-source (cons 10 20))
            (propertize "newer" 'mevedel-view-source (cons 20 30)))
    (should (= 4 (mevedel-view--recover-in-flight-turn-start
                  10 (point-min) (point-max))))
    (should (= 4 (mevedel-view--recover-in-flight-turn-start
                  6 (point-min) (point-max))))
    ;; Without a header after HISTORY-START the source itself starts the turn.
    (should (= 30 (mevedel-view--recover-in-flight-turn-start
                   15 24 (point-max))))
    (should-not (mevedel-view--recover-in-flight-turn-start
                 40 (point-min) (point-max)))
    (should-not (mevedel-view--recover-in-flight-turn-start
                 10 (point-min) (point-min)))))

(mevedel-deftest mevedel-view--retain-last-live-render-unit
  (:doc "Only the final unit, across its property runs, is retained.")
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "response text" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "first" 'mevedel-view-live-unit-source 1)
                (propertize "sec" 'mevedel-view-live-unit-source 5)
                (propertize "ond" 'mevedel-view-live-unit-source 5 'face 'bold)
                "tail without unit"))
      (mevedel-view--retain-last-live-render-unit data-buf 1 (point-max))
      (should (mevedel-view--live-tail-valid-p data-buf))
      (should (= 6 (marker-position mevedel-view--live-view-tail-start)))
      (should (= 5 (marker-position mevedel-view--live-data-tail-start)))
      (mevedel-view--retain-last-live-render-unit data-buf 9 (point-max))
      (should (= 9 (marker-position mevedel-view--live-view-tail-start)))
      (mevedel-view--retain-last-live-render-unit data-buf 12 (point-max))
      (should-not (mevedel-view--live-tail-valid-p data-buf)))))

(mevedel-deftest mevedel-view--live-tail-intact-p
  (:doc "Retained units reject split, merged and deleted source ranges.")
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "response" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (insert "Xabcdef")
        (setq mevedel-view--live-data-tail-start
              (with-current-buffer data-buf (copy-marker 1))
              mevedel-view--live-view-tail-start (copy-marker 2))
        (put-text-property 2 8 'mevedel-view-live-unit-source 1)
        (should (mevedel-view--live-tail-intact-p data-buf 3 8))
        (should-not (mevedel-view--live-tail-intact-p data-buf 2 8))
        (let ((first-end (copy-marker 3 nil))
              (end (copy-marker 8 t)))
          (unwind-protect
              (progn
                (set-marker-insertion-type mevedel-view--live-view-tail-start t)
                (delete-region 2 4)
                (goto-char 2)
                (insert "new prefix")
                (remove-text-properties 2 (point)
                                        '(mevedel-view-live-unit-source nil))
                ;; The remaining suffix alone is homogeneous but no longer
                ;; represents the complete unit selected before restoration.
                (should-not (mevedel-view--live-tail-intact-p
                             data-buf (marker-position first-end)
                             (marker-position end))))
            (set-marker first-end nil)
            (set-marker end nil)))
        (delete-region 2 (point-max))
        (goto-char 2)
        (insert "abcdef")
        (set-marker mevedel-view--live-view-tail-start 2)
        (set-marker-insertion-type mevedel-view--live-view-tail-start nil)
        (put-text-property 2 8 'mevedel-view-live-unit-source 1)
        (put-text-property 4 5 'mevedel-view-live-unit-source nil)
        (should-not (mevedel-view--live-tail-intact-p data-buf 3 8))
        (put-text-property 1 8 'mevedel-view-live-unit-source 1)
        (should-not (mevedel-view--live-tail-intact-p data-buf 3 8))
        (should-not (mevedel-view--live-tail-intact-p data-buf 3 2))
        (mevedel-view-render-invalidate-live-tail)
        (should-not (mevedel-view--live-tail-intact-p data-buf 3 8))))))

(mevedel-deftest mevedel-view--animation-span-in-window-p
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
              (should (mevedel-view--animation-span-in-window-p 60 80 window))
              (should (mevedel-view--animation-span-in-window-p 40 60 window))
              (should (mevedel-view--animation-span-in-window-p 140 160 window))
              (should-not (mevedel-view--animation-span-in-window-p 20 50 window))
              (should-not (mevedel-view--animation-span-in-window-p 150 180 window))
              (should-not (mevedel-view--animation-span-in-window-p 60 60 window)))))))))

(mevedel-deftest mevedel-view--prompt-preview
  ()
  ,test
  (test)
  :doc "short previews retain filtering and whitespace normalization"
  (should (equal "hello world"
                 (mevedel-view--prompt-preview "  hello\n world  " nil)))
  (should (equal "shared text"
                 (mevedel-view--prompt-preview "ignored" '(:text "shared text"))))
  (should-not (mevedel-view--prompt-preview " \n\t" nil))

  :doc "long previews are bounded before header-line width measurement"
  (let* ((text (concat (make-string 20000 ?x) " final"))
         (preview (mevedel-view--prompt-preview text nil)))
    (should (= (length preview) 512))
    (should (string-suffix-p "…" preview))
    (should (equal (substring preview 0 511) (make-string 511 ?x)))
    (should (= (length text) 20006)))

  :doc "a mailbox before a long prompt is filtered before truncation"
  (let ((preview (mevedel-view--prompt-preview
                  (concat "<agent-message sender=\"/root/worker\">\n"
                          (make-string 1000 ?a)
                          "\n</agent-message>\n" (make-string 1000 ?b)) nil)))
    (should (= (length preview) 512))
    (should (string-prefix-p (make-string 511 ?b) preview))))

(provide 'test-mevedel-view-responsiveness)
;;; test-mevedel-view-responsiveness.el ends here
